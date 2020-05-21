#Project Title: Demand and capacity: Short Stay Wards
#File Title: Short stay  Exploratory analysis - Breaches
#Author: Edward Watkinson (edward.watkinson@nhs.net)
#Team: Performance Team RFL
#source: Data from the Short stay dashboard (excel as built by Jeb. Yacub) and attendances from the IM ED query

# TODO


####---- Libraries ----####

#Breaches
ED_Breach_Reasons_dist <- ED_data %>% # create distribution
  filter(Breaches==1) %>%
  group_by(Breach_Reason) %>%
  summarise(count=n()) %>%
  arrange(desc(count)) %>%
  mutate(freq = count / sum(count))

ED_Breach_Reasons_All_dist <- ED_data %>% # create distribution breaches as percent of all
  group_by(Breach_Reason) %>%
  summarise(count=n()) %>%
  arrange(desc(count)) %>%
  mutate(freq = count / sum(count))

ED_Breach_Reasons_ts <- ED_data %>% # create time series
  filter(Breaches==1) %>%
  group_by(Breach_Reason, `Arrival Date`) %>%
  summarise(count=n()) %>%
  mutate(freq = count / sum(count))

##what's the distribution - lots of NAs, an AAU capacity specific question and double/triple accounting
ED_Breach_Reasons_dist #

### are NAs stable? - No; all after go-live (9% some days - 1% average)
ggplot(ED_Breach_Reasons_ts %>% filter(is.na(Breach_Reason)), aes(x=`Arrival Date`, y=freq)) + geom_line() +
  rfl_style()
summary(ED_Breach_Reasons_ts %>% filter(is.na(Breach_Reason), `Arrival Date`>"2018-11-01"))

### are "no AAU capacity" and "No Bed" stable? - Yes
ggplot(ED_Breach_Reasons_ts %>% filter(Breach_Reason %in% c("No AAU capacity", "No CDU capacity", "No bed")), aes(x=`Arrival Date`, y=count, colour=Breach_Reason)) + 
  geom_line() +
  rfl_style()

### How much do they make up of the toal (roughly 25-30%)
ggplot(ED_Breach_Reasons_dist %>% filter(!is.na(Breach_Reason)), aes(x=reorder(Breach_Reason, -freq), y=freq)) + geom_col() +
  gghighlight(Breach_Reason %in% c("No AAU capacity", "No CDU capacity"))  +
  rfl_style()

###Does this chhange over time -not really
ED_Breach_NoAAUC_ts <- ED_data %>% # create time series
  filter(Breaches==1) %>%
  mutate(Breach_Reason=ifelse(grepl("No AAU capacity|No CDU capacity", Breach_Reason), "No Capacity", "Other")) %>%
  group_by(`Arrival DateTime`, Breach_Reason) %>%
  summarise(count=n()) %>%
  mutate(freq = count / sum(count))

ggplot(ED_Breach_NoAAUC_ts %>% 
         group_by(`Arrival Date`=floor_date(`Arrival DateTime`,"day"), Breach_Reason) %>%
         summarise(count=sum(count)) %>%
         mutate(freq = count / sum(count))
       , aes(x=`Arrival Date`, y=freq, colour=Breach_Reason)) + 
  geom_line() +
  rfl_style()

####what about hours of the day - YES interestinglybed breaches tail off in the evening? process or actual?
ggplot(ED_Breach_NoAAUC_ts %>% 
         group_by(hour=hour(`Arrival DateTime`)+4, Breach_Reason) %>%
         summarise(count=sum(count)) %>%
         mutate(freq = count / sum(count)),
       aes(x=hour, y=count, colour=Breach_Reason)) + 
  geom_line()