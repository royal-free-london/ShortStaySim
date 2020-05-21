#Project Title: Demand and capacity: Short Stay Wards
#File Title: Short stay  Modelling - Forecast
#Author: Edward Watkinson (edward.watkinson@nhs.net)
#Team: Performance Team RFL
#source: Data from the Short stay dashboard (excel as built by Jeb. Yacub) and attendances from the IM ED query

# TODO


####---- Libraries ----####
library(prophet)
library(MLmetrics)

#----Model---#

ds <- ED_Data %>%
  filter(site=="Royal Free",
         count>5) %>% # to get rid of patients counted from last previous day
  rename(ds=`Arrival DateTime`, y=count) %>%
  select(ds, y) 

#if you want to test the last 2 weeks
#ds <- ds %>% filter(ds < today()-days(14))

m <- prophet(seasonality.mode = 'multiplicative')
m <- add_country_holidays(m, country_name = 'UK')
m <- fit.prophet(m, ds)

future <- make_future_dataframe(m, periods = 14)

forecast <- predict(m, future)

df_combined <- right_join(ds, forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

df_twoWks <- df_combined %>% filter(ds>today()-days(14))

#------Evaluate-----#
maxDate<- max(ds$ds)
historicForecast <- forecast %>%
  filter(ds <=maxDate)

MAPE(y_pred = historicForecast$yhat,  y_true = ds$y)

ds$yhat <- historicForecast$yhat
ds$absError <- abs(ds$yhat - ds$y)
ds$absPcntError <- ds$absError/ds$y
MAPE <- (sum(ds$absPcntError)/nrow(ds))

ggplot(ds, aes(x=absPcntError*100)) + geom_histogram(binwidth=5)


#Evaulate with cross validation - https://www.datasciencecentral.com/profiles/blogs/cross-validation-concept-and-example-in-r-2
#df_cv <- cross_validation(m, initial = 730, period = 180, horizon = 365, units = 'days')
head(df.cv)


#------Communicate-----#

gPredictionPlotTotal <- plot(m, forecast)
gComponentsPlot <- prophet_plot_components(m, forecast)

ggplot(df_twoWks %>% gather("metric", "value", -ds), aes(x=ds, y=value, group=metric, colour=metric)) +
  geom_line()
