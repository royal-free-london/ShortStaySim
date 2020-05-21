#Project Title: Demand and capacity: Short Stay Wards
#File Title: Short stay Model
#Author: Edward Watkinson (edward.watkinson@nhs.net)
#Team: Performance Team RFL

####---- Method ----####
# every hour people go into Ward
#  - add patients to 'inWard' list based on arrivals
#  - total length of stay randomly assigned
# every hour patients in ward LoS go up by 1
# every hour patients leave ward (dependent on reaching either their total LoS as allocated, or the maximum LoS for the ward (if set))
# every hour arrivals, patient count and

####---- Libraries ----####
library(tidyverse)
library(lubridate)
library(RFLtheme)
#####


####---- Import ----####
#ED_data, and LOS_data (for AAU/MAU) should be loaded from 00_ShortStay_Import-Data_EW.R


####---- Tidy -----#####

maxLOS <- 1000 # maximum number of hours anyone can stay on the ward (overides the distribution of LoS from hospital data)
hoursArrivalToAdm <- 3 # beds are required this many hours after attendance
daysToRun <- 100 # the number of days to run the simulation for
startDate<- dmy('01/04/2019') # Data to use
l_lOSWardsToUse <- c('AAU', 'MAU')

## Define Admissions

EDMajors_adms <- ED_data %>%
  filter(`Arrival DateTime`>=startDate,
         `Stream (Discharge)`=="Majors", 
         AttendanceDisposalCode=="Admitted to hospital bed or became Lodged Patient of the same Health Care Provider"
         ) %>%
  group_by(hour = hour(`Arrival DateTime`), day = `Arrival Date`) %>%
  summarise(hourly_arrivals=n())

EDMajors_adms_dist <- EDMajors_adms %>%
  ungroup %>%
  complete(hour, day, fill = list(hourly_arrivals = 0)) %>%
  group_by(hour, hourly_arrivals) %>%
  summarise(count=n()) %>%
  mutate(freq = count / sum(count))

admissions <- function(date_time) { 
  pickHour <- ifelse(is.POSIXct(date_time), hour(date_time), date_time)
  data <- EDMajors_adms_dist %>% filter(hour==pickHour)
  return(sample(x = data$hourly_arrivals, 1, replace = T, prob = data$freq ))
}


## Define LoS

AAU_LOS_dist <- LOS_data %>%
  filter(`Admission Date Time`>=startDate) %>%
  filter(`LOS (hours)`>0,
         `LOS (hours)`<400,
         `AAU/MAU`=="AAU") %>%
  group_by(`LOS (hours)`) %>%
  summarise(count=n()) %>%
  mutate(freq = count / sum(count))

AAU_LOS_dist <- LOS_data %>%
  filter(`Admission Date Time`>=startDate) %>%
  filter(`LOS (hours)`>0,
         `LOS (hours)`<400,
         `AAU/MAU` %in% l_lOSWardsToUse) %>%
  group_by(Hospital_Provider_Spell_Number) %>%
  summarise(`LOS (hours)`=sum(`LOS (hours)`)) %>%
  group_by(`LOS (hours)`) %>%
  summarise(count=n()) %>%
  mutate(freq = count / sum(count))

#barplot(AAU_LOS_dist$freq*100, main="Current LoS AAU", names.arg = AAU_LOS_dist$`LOS (hours)`, xlab="LoS in Hours", ylab="% of patients")

lengthOfStay <- function() { return(sample(x = AAU_LOS_dist$`LOS (hours)`, 1, replace = T, prob = AAU_LOS_dist$freq ))}


####---- Initialisation -----####

run_time <- seq(Sys.time(), Sys.time() %m+% days(daysToRun), "hour") #Vector of hours to run over

patientsInWard <- data.frame(
  patient_ID = numeric(),
  current_lengh_of_stay = numeric(),
  total_length_of_stay = numeric(),
  stringsAsFactors = FALSE
)

hourlySummary <- data.frame(
  date_time = as.POSIXct(character()),
  arrivals = numeric(),
  patients_in_ward = numeric(),
  stringsAsFactors = FALSE
) 
startPatientID <- 1

####---- Simulation -----####

for (current_hour in seq_along(run_time)) { # loop through each hour
  ###add patients arriving
  arrivalsInHour <- admissions(as_datetime(run_time[[current_hour]]) + hours(hoursArrivalToAdm)) #number of patients requiring admission in that hour
  
  ##create dataframe of new patients
  if (arrivalsInHour>0) {
    patientsArriving <- as.data.frame(matrix(numeric(arrivalsInHour*3), ncol=3))
    colnames(patientsArriving) <- c("patient_ID", "current_length_of_stay", "total_length_of_stay")
    patientsArriving$patient_ID <- seq(startPatientID, startPatientID+arrivalsInHour-1)
    patientsArriving$current_length_of_stay <- c(rep(0, arrivalsInHour))
    patientsArriving$total_length_of_stay <- c(rep(lengthOfStay(), arrivalsInHour))

    ##append new patients to patientsInWard
    patientsInWard <- rbind(patientsInWard,patientsArriving)
    
    ##set patient ID to start at next time
    startPatientID <- startPatientID + arrivalsInHour
  } 

  ###advance length of stay by 1 hour
  patientsInWard$current_length_of_stay <- patientsInWard$current_length_of_stay +1
  
  ###remove patients departing
  ## remove patients past their total LoS
  patientsInWard <- patientsInWard[patientsInWard$current_length_of_stay < patientsInWard$total_length_of_stay,]
  
  ## remove patients past the max LoS
  if (maxLOS) {
    patientsInWard <- patientsInWard[patientsInWard$current_length_of_stay < maxLOS,]
    }

  ###Provide summary (count patients in ward and save in hourlySummary)
  hourlySummary <- rbind(hourlySummary, data.frame(time=run_time[[current_hour]], arrivals=arrivalsInHour, patients_in_ward=nrow(patientsInWard)))
  
}

####---- Communicate ----####

#Create 'smoothed' patients in ward number for the option of waiting + or - 1 hour (i.e. get bed 2-4 hours after arrival in ED)
hourlySummary <- hourlySummary %>%
  mutate(smoothed_patients_in_ward = (lead(patients_in_ward) + patients_in_ward + lag(patients_in_ward))/3)
         
#How many beds are needed for different levels of demand

ggplot(hourlySummary, aes(x=arrivals)) + #Arrival patten used
  geom_density(fill=RFLcolour('blue'), colour="white") #+
  #stat_smooth(geom = "area", span = 0.4, method = "loess", alpha = 0.4)

keyPIWstats <- data.frame("Percentile" = c("65%", "75%", "85%", "95%", "99%"),
  "value" = quantile(hourlySummary$smoothed_patients_in_ward,c(0.65, 0.75, 0.85, 0.95, 0.99), na.rm=TRUE)) # key points of the capacity required distribution

gPIW_modelOutput_PDF <- ggplot(hourlySummary, aes(x=patients_in_ward)) + 
  stat_density(geom="line", colour=RFLcolour('blue', 0.5), size=2.5) +
  geom_density( fill=RFLcolour('blue'), colour=NA) +
  geom_vline(data = keyPIWstats, aes(xintercept = value))  +
  geom_text(data = keyPIWstats, aes(x= value, y=0.05, angle=-90, vjust=-0.5, hjust=-0.05, label = paste(Percentile, "(", round(value), "beds )"))) +
  labs(title="(Simulated) potential Bed requirement",
       subtitle="Simulated on ED Majors admissions and current LoS.") +
  xlab("Patients in ward / Number of required beds") +
rfl_style() +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_text(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

gPIW_modelOutput_CDF <- ggplot(hourlySummary, aes(x=patients_in_ward)) + 
  stat_ecdf(geom="line", colour=RFLcolour('blue', 0.5), size=2.5) +
  geom_vline(data = keyPIWstats, aes(xintercept = value))  +
  geom_text(data = keyPIWstats, aes(x= value, y=c(0.65, 0.75, 0.85, 0.95, 0.99), angle=-90, vjust=-0.6, hjust=-0.05, label = paste(Percentile, "(", round(value), "beds )"))) +
  labs(title="(Simulated) AAU Beds used",
       subtitle="Simulated on ED Majors admissions and current LoS shows how frequently different levels of capacity is required") +
  xlab("Number of required AAU beds") +
  rfl_style()

gPIW_modelOutput_hourly <- ggplot(hourlySummary, aes(y=patients_in_ward , x=as.factor(hour(time)))) +
  geom_boxplot() +
  geom_point()  +
  geom_jitter(shape=16, position=position_jitter(0.2), colour=RFLcolour("blue", 0.6)) +
  rfl_style() +
labs(title="(Simulation) Patients in AAU by hour",
     subtitle="Simulated on ED Majors admissions and current LoS Each blue dot is the number of patients in the ward  for a particualar hour in a day")


gMetrics_ModelOutput <- ggplot(hourlySummary %>% gather("metric", "value", -time)) + #plot of collected metrics Faceted
  geom_line(aes(x=time, y=value, colour=metric)) +
  facet_wrap( ~ metric ,  ncol=1, scales="free_y") +
  labs(title="AAU occupancy across time")

modelOutput_meanLOS <- sum(hourlySummary$patients_in_ward)/(daysToRun*24)
