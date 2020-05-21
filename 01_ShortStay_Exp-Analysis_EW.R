#Project Title: Demand and capacity: Short Stay Wards
#File Title: Short stay  Exploratory analysis
#Author: Edward Watkinson (edward.watkinson@nhs.net)
#Team: Performance Team RFL
#source: Data from the Short stay dashboard (excel as built by Jeb. Yacub) and attendances from the IM ED query

# TODO
#  + Create summary stays for simple calculation of capacity
#  - look at impacts of hospital - % of patients admitted who where admitted to AAU (to check) and which stayed <24 hours (total number to compare).

####---- Libraries ----####
library(tidyverse)
library(lubridate)
library(plotly)
library(gghighlight)
library(ggridges)
library(RFLtheme)


####---- Import ----####
#ED_data, and LOS_data (for AAU/MAU) should be loaded from 00_ShortStay_Import-Data_EW.R

####---- Exploration ----####

#Admissions from Majors

#filter data and group by hour/day

ED_data %>%
  filter(`Stream (Discharge)`=="Majors", 
         AttendanceDisposalCode=="Admitted to hospital bed or became Lodged Patient of the same Health Care Provider") %>%
  group_by(day = `Arrival Date`) %>%
  summarise(arrivals=n()) %>%
  ggplot(aes(x=day, y=arrivals)) +
  geom_line()

EDMajors_adms_hourly <- ED_data %>%
  filter(`Stream (Discharge)`=="Majors", 
         AttendanceDisposalCode=="Admitted to hospital bed or became Lodged Patient of the same Health Care Provider") %>%
  group_by(hour = hour(`Arrival DateTime`), day = `Arrival Date`) %>%
  summarise(hourly_arrivals=n()) %>%
  ungroup %>%
  complete(hour, day, fill = list(hourly_arrivals = 0))

EDMajors_adms_hourly_dist <- EDMajors_adms_hourly %>% # create distribution
  group_by(hour, hourly_arrivals) %>%
  summarise(count=n()) %>%
  mutate(freq = count / sum(count))

EDMajors_adms_daily_dist <- EDMajors_adms_hourly %>% # create distribution
  group_by(hour) %>%
  summarise(count=sum(hourly_arrivals), mean=count/n()) %>%
  mutate(freq = count / sum(count))

##create average daily for 'simple' caculation
EDMajors_adms_daily <- EDMajors_adms_hourly %>%
  group_by(day) %>%
  summarise(daily_arrivals = sum(hourly_arrivals))

meanDailyMajadms <- mean(EDMajors_adms_daily$daily_arrivals)

gEDMajors_adms_days <- ggplot(EDMajors_adms_daily_dist, aes(y=mean,x=as.factor(hour))) + geom_col(fill=RFLcolour("blue")) +
  labs(title="Average admissions from ED Majors into Royal Free wards", 
       subtitle="from April 2018") +
  xlab("average arrivals per hour") +
  ylab("hours of the day") +
  rfl_style()

gEDMajors_adms_hours_bp <- ggplot(EDMajors_adms_hourly, aes(y=hourly_arrivals, x=as.factor(hour))) +
  geom_boxplot() +
  #geom_point()  +
  #geom_jitter(shape=16, position=position_jitter(0.2), colour=RFLcolour("blue", 0.6)) +
  labs(title="Spread of admissions from ED Majors into Royal Free wards",
       subtitle="from April 2018. Each blue dot is the number of arrivals for a particualar hour in a day in the last year ") +
  scale_y_continuous(breaks=1:9)+
  xlab("average arrivals per hour") +
  ylab("hours of the day") +
  stat_summary(fun.y=mean, geom="point", shape=23, size=4, fill= RFLcolour("green")) +
  rfl_style()


###density plot of admissions by hour of arrival
gEDMajors_adms_hours_rdg <- ggplot(EDMajors_adms_hourly, aes(x = hourly_arrivals, y = reorder(as.factor(hour), -hour), group = as.factor(hour))) +
  geom_density_ridges(scale = 10, size = 0.25, rel_min_height = 0.01, fill=RFLcolour("blue"), colour="white") +
  theme_ridges() +
  scale_x_continuous(limits=c(1, max(EDMajors_adms_hourly$hourly_arrivals)), expand = c(0.01, 0)) %>%
  labs(title="Hourly arrivals RFH 2018/19") + 
  xlab("Number of patients  per hour") +
  ylab("hours of the day and likelihood of arrival number") +
  rfl_style()

#AAU
AAU_arrivals_ts <- LOS_data %>%
  group_by(floor_date(`Admission Date Time`, "day")) %>%
  summarise(count=n())

##Remove outliers and other wards
LOS_data_AAU <- LOS_data %>%
  filter(LOS_data$`LOS (hours)`>0,
         LOS_data$`LOS (hours)`<400,
         `AAU/MAU`=="AAU")

#Make distribution data
AAU_LOS_dist <- LOS_data_AAU %>%
  group_by(`LOS (hours)`) %>%
  summarise(count=n()) %>%
  mutate(freq = count / sum(count))

##create average daily for 'simple' caculation
meanTotalAAULoS <- mean(LOS_data_AAU$`LOS (hours)`)

#function to calaculte percentages above and below thresholds
less_and_greater <- function(x) {
  list <- list()
  list[paste("less than", x)] <- sum(AAU_LOS_dist[AAU_LOS_dist$`LOS (hours)`<x,]$count)/sum(AAU_LOS_dist$count)
  list[paste("greater than", x)] <- sum(AAU_LOS_dist[AAU_LOS_dist$`LOS (hours)`>x,]$count)/sum(AAU_LOS_dist$count)
  return(list)
}

thresholdsNum <- c(24, 48, 72)

thresholds <- map(thresholdsNum, less_and_greater) %>% unlist

##histogram of LOS
gLOS_hist <- ggplot(AAU_LOS_dist[AAU_LOS_dist$`LOS (hours)`<100,], aes(x=`LOS (hours)`, y=count)) +
  geom_col() +
  geom_vline(xintercept=thresholdsNum) + 
  geom_text(data= as.data.frame(matrix(9, nrow=3)), aes(x=thresholdsNum, y=rep(300, 3), angle=-90, vjust=1,
                                                        label = scales::percent(thresholds[grepl("less", names(thresholds))]))) +
  labs(title="Number of patients by their length of stay in AAU (hours)",
       subtitle="From April 2018") +
  xlab("Length of stay, in hours") +
  rfl_style()
 
LOS_data_AAU_specialty <- LOS_data_AAU %>%
  group_by(Specialty) %>%
  mutate(countSpec=n()) %>%
  ungroup() %>%
  mutate(Specialty=ifelse(countSpec<nrow(LOS_data_AAU)*0.01, "Other", Specialty)) %>%
  group_by(Specialty, `LOS (hours)`) %>%
    summarise(count=n())

##histogram of LOS by specialty
gLOS_hist_spec <- ggplot(LOS_data_AAU_specialty[LOS_data_AAU_specialty$`LOS (hours)`<100,], aes(x=`LOS (hours)`, y=count, colour=Specialty)) +
  geom_smooth(se = FALSE) +
  #geom_line(size=1) +
  #facet_wrap(~ Specialty) +
  labs(title="Number of patients by their length of stay in AAU (hours)",
       subtitle="From April 2018. By specialty") +
  xlab("Length of stay, in hours") +
  rfl_style()


# ##bringing it all together
# ggplot() +
#   geom_density(data=daily_admissions, aes(x=count, fill=RFLcolour("orange"))) +
#   geom_density(data=EDMajors_adms_daily, aes(x=daily_arrivals, fill=RFLcolour("orange"))) +
#   geom_density(data=AAU_arrivals_ts %>% filter(`AAU/MAU`=="AAU"), aes(x=count, fill=RFLcolour("green"))) +
#   geom_density(data=ED_Breach_Reasons_ts %>% filter(Breach_Reason =="No bed" | Breach_Reason =="No AAU capacity"), aes(x=count, fill=Breach_Reason))+
#   geom_vline(xintercept=2.452) +
#   geom_vline(xintercept=10.18)
  
  
