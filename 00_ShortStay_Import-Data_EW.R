#Project Title: Demand and capacity: Short Stay Wards
#File Title: Short stay data for model
#Author: Edward Watkinson (edward.watkinson@nhs.net)
#Team: Performance Team RFL

####---- Libraries ----####
library(DBI)
library(odbc)
library(tidyverse)
#####

v_getNew <- FALSE # get new data (otherwise loads from data folder)
v_saveNewIfGet <- False # if getting 


####---- Import ----####

if(v_getNew) {
  con <- dbConnect(odbc(), "RFL_Performance")
  LOS_data <- dbGetQuery(con, statement = read_file('SS-LOS-from-Dashboard_2019-02-07.sql'))
  ADM_data <- dbGetQuery(con, statement = read_file('SS-ADM-from-Dashboard_2019-04-11.sql'))
  ED_data <- dbGetQuery(con, statement = read_file('IM-ED-Query-2019_2020-05-15.sql')) %>% filter(`Hospital Site`=="Royal Free")
  dbDisconnect(con)
  if(v_saveNewIfGet){
    save(LOS_data, file='data/LOS_data.RData')
    save(ADM_data, file='data/ADM_data.RData')
    save(ED_data, file='data/ED_data.RData')
  }
} else {
  load('data/LOS_data.RData')
  load('data/ADM_data.RData')
  load('data/ED_data.RData')
}