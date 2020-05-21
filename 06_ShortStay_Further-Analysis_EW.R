#Project Title: Demand and capacity: Short Stay Wards
#File Title: Short stay Exploratory analysis
#Author: Edward Watkinson (edward.watkinson@nhs.net)
#Team: Performance Team RFL
#source: Data from the Short stay dashboard (excel as built by Jeb. Yacub) and attendances from the IM ED query

##answers to the feedback: 
#As they stand the data are dispiriting and show a progressive fall in % of patients discharged within 24 and 72 hours - however, this includes only AAU/MAU discharges and doesn't differentiate between discharges home and discharges to other wards.
#Clarity about whether the profile of admission duration has changed - i.e. have we managed to left shift the curve?
# In addition, there was a concern as to whether we were losing a significant number of acute admissions who went straight to a specialist/other ward.


####---- Libraries ----####
library(tidyverse)
library(lubridate)
library(plotly)
library(RFLtheme)
library(bupaR)
library(gganimate)
library(magick)

####---- Import ----####
#ED_data, and LOS_data (for AAU/MAU) should be loaded from 00_ShortStay_Import-Data_EW.R

ADM_data <- ADM_data %>%
  filter(`Admission From`=="Non-Elective (ED)")

####---- Tidy ----####

#AAU admissions as percentage of all admissions
#Has % to AAU and MAU first changed

LOS_data_RFH_Adult <- LOS_data %>% # data at spell,ward and specialty-level so will need filtering and collapsing
  filter(
    Site == "ROYAL FREE HOSPITAL",
    Specialty != "Paediatrics",
    `Admission Date Time` > as_datetime("2018-05-06")
  ) %>%
  group_by(Hospital_Provider_Spell_Number, Ward) %>%
  mutate(min_adm_dateTime = min(`Admission Date Time`), #calcualte the times patients entered and left wards (irrespective of specialty under)
         max_dchg_dateTime = max(`Discharge Date Time`),
         `Ward LOS (hours)` = as.numeric(max_dchg_dateTime-min_adm_dateTime, "hours")) %>%
  distinct( #only show one row per patient per ward - i.e. remove consultant/specialty level information
    Hospital_Provider_Spell_Number,
    Ward,
    `AAU/MAU`,
    Site,
    min_adm_dateTime,
    max_dchg_dateTime,
    `Time Band`,
    Discharge_Date,
    WeekBeginning,
    `Ward LOS (hours)`,
    MonthBeginning,
    .keep_all = TRUE
  ) %>%
  semi_join(ADM_data, by = "Hospital_Provider_Spell_Number") %>%
  group_by(Hospital_Provider_Spell_Number) %>%
  mutate(order_in_spell = rank(min_adm_dateTime, ties.method = "first"),
         discharged_to=ifelse(order_in_spell==max(order_in_spell),"Home","Other Ward")
         ) %>%
  ungroup()

####---- Explore ----####

#Check wards to ensure no AE or NULLS
gAdmPcntWrd <- LOS_data_RFH_Adult %>%
  filter(order_in_spell == 1) %>%
  group_by(Ward) %>%
  summarise(count=n()) %>%
  mutate(freq = count / sum(count)) %>%
  ggplot(aes(y=freq, x=reorder(Ward, freq))) +
  geom_col() +
  coord_flip() +
  rfl_style() +
  rfl_scale_colour() +
  labs(title="Adult Admissions from ED - % by Ward",
                          subtitle="Royal Free Hospital, 2018/19") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

####---- Visualise ----####

##graph % of admissions from ED by ward
gAdmNumWrdGrp <- LOS_data_RFH_Adult %>% 
  mutate(week = floor_date(min_adm_dateTime, "week")) %>%
  filter(order_in_spell == 1, week<max(week), week>min(week)) %>%
  group_by(week, `AAU/MAU`) %>%
  summarise(count = n()) %>%
  ggplot(aes(
    y = count,
    x = week,
    group = `AAU/MAU`,
    colour = `AAU/MAU`
  )) +
  geom_line() +
  geom_point() +
  geom_smooth() +
  labs(title="Adult Admissions from ED - % by Ward Type",
       subtitle="Royal Free Hospital, 2018/19",
       colour = "Ward Group") +
  #scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  rfl_scale_colour() +
  rfl_style()

##graph % of admissions from ED by ward
gAdmPcntWrdGrp <- LOS_data_RFH_Adult %>% 
  mutate(week = floor_date(min_adm_dateTime, "week")) %>%
  filter(order_in_spell == 1, week<max(week), week>min(week)) %>%
  group_by(week, `AAU/MAU`) %>%
  summarise(count = n()) %>%
  mutate(freq = count / sum(count)) %>%
  ggplot(aes(
    y = freq,
    x = week,
    group = `AAU/MAU`,
    colour = `AAU/MAU`
  )) +
  geom_line() +
  geom_point() +
  geom_smooth() +
  labs(title="Adult Admissions from ED - % by Ward Type",
       subtitle="Royal Free Hospital, 2018/19",
       colour = "Ward Group") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  rfl_scale_colour() +
  rfl_style()

#AAU MedianLength of stay by week
gLoSMedAAUAll <- LOS_data_RFH_Adult %>%
  group_by(Hospital_Provider_Spell_Number) %>%
  mutate(week = floor_date(max_dchg_dateTime, "week")) %>%
  filter(`AAU/MAU`=="AAU") %>%
  group_by(week) %>%
  summarise(median_Lengh_of_stay=median(`Ward LOS (hours)`)) %>%
  ggplot(aes(x = week, y=median_Lengh_of_stay)) + 
  geom_line() +
  geom_point() +
  labs(y="Percent of admissions", x="Length of Stay in days", 
       title="AAU Median Length of Stay", 
       subtitle="Royal Free Hospital",
       colour = "Ward Discharge Destination") +
  rfl_show_axis_titles() +
  rfl_scale_colour() +
  rfl_style() +
  geom_smooth()



#AAU MedianLength of stay by discharge route and week
gLoSMedAAU <- LOS_data_RFH_Adult %>%
  group_by(Hospital_Provider_Spell_Number) %>%
  mutate(week = floor_date(max_dchg_dateTime, "week")) %>%
  filter(`AAU/MAU`=="AAU") %>%
  group_by(week, discharged_to) %>%
  summarise(median_Lengh_of_stay=median(`Ward LOS (hours)`)) %>%
  ggplot(aes(x = week, y=median_Lengh_of_stay, group = discharged_to, colour=discharged_to)) + 
  geom_line() +
  geom_point() +
  labs(y="Percent of admissions", x="Length of Stay in days", 
       title="AAU Median Length of Stay by Discharge Route", 
       subtitle="Royal Free Hospital",
       colour = "Ward Discharge Destination") +
  rfl_show_axis_titles() +
  rfl_scale_colour() +
  rfl_style()

gLoSMedAAUAnim <- gLoSMedAAU + #animated version
  transition_reveal(week)

#LoS distribution chart
gLoSDist <- LOS_data_RFH_Adult %>%
  mutate(week = floor_date(max_dchg_dateTime, "week"),
         quarter = floor_date(max_dchg_dateTime, "quarter")) %>%
  filter(`AAU/MAU`=="AAU",
         order_in_spell == 1) %>%
  select(`LOS (hours)`, discharged_to, week, quarter) %>%
  group_by(discharged_to, week) %>%
  mutate(weekly_median=median(`LOS (hours)`)) %>%
  group_by(discharged_to, quarter) %>%
  mutate(quarterly_median=median(`LOS (hours)`)) %>%
  ungroup() %>%
  mutate(lab_x=ifelse(discharged_to=="Home",70,90)) %>%
ggplot(aes(x = `LOS (hours)`, fill = discharged_to)) + 
  geom_density(alpha = 0.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(y="Percent of admissions", x="Length of Stay in days") +
  rfl_show_axis_titles() +
  rfl_scale_colour() +
  rfl_scale_fill() +
  xlim(0, 100) +
  rfl_style()

gLosDistQtlyFct <- gLoSDist +
  geom_vline(aes(xintercept=quarterly_median, colour = discharged_to), size=1) + 
  geom_label(aes(label=mean(quarterly_median), y=0.061, x=lab_x, group=discharged_to)) +
  facet_wrap(~quarter) +
  labs(title="AAU Length of stay by discharge route")
  
gLoSDistWklyAnim <- gLoSDist + #animated version
  geom_vline(aes(xintercept=quarterly_median, colour = discharged_to), size=1, show.legend = FALSE) + 
  geom_label(aes(label=quarterly_median, y=0.063, x=lab_x, group=discharged_to), alpha=0.5) +
  geom_text(aes(label="Median LoS (hours)", x=60, y=0.0665), hjust = 0)+
  transition_time(week) +
  ease_aes() +
  labs(y="Percent of admissions", x="Length of Stay in days", 
       title="AAU Length of stay by discharge route", subtitle="{format(frame_time,'%B, %Y')}",
       fill = "Ward Discharge Destination") 

##Combine charts into into Gif
a_mgif <- image_read(animate(gLoSMedAAUAnim, fps=3, width = 500, height = 300))
b_mgif <- image_read(animate(gLoSDistWklyAnim, fps=3, width = 500, height = 300))
gLoSAAUAnim <- image_append(c(a_mgif[1], b_mgif[1]), stack = TRUE)
for(i in 2:100){
  combined <- image_append(c(a_mgif[i], b_mgif[i]), stack = TRUE)
  gLoSAAUAnim <- c(gLoSAAUAnim, combined)
}

#Where do the other admissions go?

eventLog <- LOS_data %>% filter(Site=="ROYAL FREE HOSPITAL", 
                                `Admission Date Time`>=as_datetime("2019-01-01 00:00:00"),
                                Specialty!="Paediatrics") %>%
  semi_join(ADM_data, by = "Hospital_Provider_Spell_Number") %>% # include only those patients who have been dishcarged from ED
  mutate(
    status = "complete",
    activity_instance = 1:nrow(.),
    resource = NA,
    date = `Admission Date Time`,
    node = `AAU/MAU`
  ) %>%
  eventlog(
    case_id = "Hospital_Provider_Spell_Number",
    activity_id = "node",
    timestamp = "date",
    activity_instance_id = "activity_instance",
    lifecycle_id = "status",
    resource_id = "resource"
  )

#eventLog %>%
#  trace_explorer(coverage = 1)

pmWrdGroupRel <- eventLog %>%
  #filter_activity_frequency(percentage = 1) %>%
  process_map(type=frequency("relative"))

##############previous analysis on admissions (fault wards) ################


#Basically we need to use the first location that makes sense.
#if the ward end code of the first episode is Null, and ther is no second episode, then delete - patient DTAed but not acutally admitted on system
#if the ward start code of the first episode is Null and there is a second episode, use the start code of the second episode
#if the ward start code is Null but the end code is not, use the end code.

# #Fix missing wardname, tidy up wards and remove first and last months of analysis (FIXME)
# ADM_data$Ward_Code_at_Start_Episode[is.na(ADM_data$Ward_Code_at_Start_Episode)] <- "Unknown"
# ADM_data <- ADM_data %>%
#   filter(floor_date(`Start_Date_(Hospital_Provider_Spell)`, "month") != as.Date("2019-04-01"),
#          floor_date(`Start_Date_(Hospital_Provider_Spell)`, "month") != as.Date("2018-03-01")) %>%
#   rename(Ward=Ward_Code_at_Start_Episode) %>%
#   mutate(Ward=ifelse(Ward=="RAL QV", "RAL AAU", Ward),
#          Ward=str_replace(Ward, "RAL ", ""))
# 
# #admissions by ward group (SS or not)
# gADM_WardType <- ADM_data %>%
#   group_by(day=floor_date(`Start_Date_(Hospital_Provider_Spell)`, "month"), `AAU/MAU`) %>%
#   summarise(count=n()) %>%
#   mutate(pcnt=count/sum(count)) %>%
#   ggplot(aes(x=day, y=pcnt, group=`AAU/MAU`, colour=`AAU/MAU`)) +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#   geom_line() +
#   geom_point() +
#   rfl_style() +
#   rfl_scale_colour() +
#   ggtitle("Non-Elective Admissions by Ward (%)", sub="Royal Free Hospital") +
#   expand_limits(y=0)   
# 
# ggplotly(gADM_WardType)
# 
# ##look by ward group and specialty
# gADM_WardSpec <- ADM_data %>%
#   mutate(  Ward=ifelse(Ward %in% c("AAU", "8 NORTH", "Unknown"),Ward,"Other"),
#            Ward=as.factor(Ward),
#            Ward=factor(Ward, levels=c("AAU", "8 NORTH", "Unknown", "Other"))
#   ) %>%
#   group_by(month=floor_date(`Start_Date_(Hospital_Provider_Spell)`, "month"), Specialty, Ward) %>%
#   summarise(count=n()) %>%
#   group_by(Ward) %>%
#   mutate(wardCount=sum(count), wardMthMean=mean(count), wardLstMthCount=sum(count[month==max(month)])) %>%
#   group_by(Specialty) %>%
#   mutate(specCount=sum(count), specMthMean=mean(count), specLstMthCount=sum(count[month==max(month)])) %>%
#   ungroup() %>%
#   mutate(pcnt=count/sum(count),
#          Specialty=ifelse(specCount>1200,Specialty,"Other"),
#          #Ward=ifelse(wardCount>100,Ward,"Other"),
#          #Ward=reorder(Ward,-wardLstMthCount),
#          Specialty=reorder(Specialty,-specLstMthCount)) %>%
#   #filter(Specialty!="Other") %>%
#   group_by(month, Ward, Specialty) %>%
#   summarise(admissions= sum(count)) %>%
#   ggplot(aes(x=month, y=admissions, colour=Specialty)) +
#   geom_line() +
#   geom_point() +
#   facet_wrap(~Ward) +
#   rfl_style() +
#   rfl_scale_colour() +
#   ggtitle("Non-Elective Admissions by Ward", sub="Royal Free Hospital") +
#   expand_limits(y=0)
# 
# #check if patients admitted to an 'NA'ward are dupicates - do they have a corrseponding admission to a ward?
# ADM_data_UnkonwnWd <- filter(ADM_data, Ward=="Unknown") %>% select(PID=Local_Patient_ID, aDate=`Start_Date_(Hospital_Provider_Spell)`, Ward )
# ADM_data_KnownWd <- filter(ADM_data, Ward!="Unknown") %>% select(PID=Local_Patient_ID, aDate=`Start_Date_(Hospital_Provider_Spell)`, Ward )
# ADM_data_dups <- inner_join(ADM_data_UnkonwnWd, ADM_data_KnownWd, by= c("PID")) %>% mutate(diff=difftime(aDate.y,aDate.x))# %>% filter(aDate.y-aDate.x<=10,aDate.y-aDate.x>=-10)
# ADM_data_UnkonwnWd_latest10 <- top_n(ADM_data_UnkonwnWd, 10, aDate)
# 
# ##is the Los different for Known and Unknown wards
# a <- filter(ADM_data, Ward=="Unknown") %>% select(LOS, Ward)
# b <- filter(ADM_data, Ward!="Unknown") %>% mutate(Ward="Known") %>% select(LOS, Ward)
# c <- bind_rows(a,b)
# ggplot(c, aes(x = LOS, fill = Ward)) + 
#   geom_density(alpha = 0.5) +
#   add_axis_titles() +
#   labs(y="Percent of admissions", x="Length of Stay in days", title="Admissions from ED where ward is not recorded", subtitle="Royal Free Hospital 2018/19") +
#   xlim(0, 50) +
#   rfl_style()

#-------------------------------------------------------------------------

