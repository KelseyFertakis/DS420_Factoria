if(!is.null(dev.list())) dev.off() # Clear Plots
rm(list=ls()) # Clear objects from Memory
cat("\014") # Clear Console

library(ggplot2)
library(forecast)
library(tseries)
library(RCurl)
library(sqldf)
library(digest)
library(dplyr)
library(anytime)
library(geosphere)
library(lubridate)
library(chron)
require(caret)
require(rattle)
require(yardstick)

# set start and end date

startDate <- '2018-05-01-0'
endDate <- '2018-06-01-0'

# pull data for Beijing
# acquire air quality data
bj_aq_url <- paste0("https://biendata.com/competition/airquality/bj/",startDate,"/",endDate,"/2k0d1d8")
bj_aq_file <- getURL(bj_aq_url, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
bj_aq_data <- read.csv(textConnection(bj_aq_file), header=TRUE)

#delete 4 columns
bj_aq_data$id <- NULL
bj_aq_data$CO_Concentration <- NULL
bj_aq_data$SO2_Concentration <- NULL
bj_aq_data$NO2_Concentration <- NULL

#format time correctly
bj_aq_data$time <- as.POSIXct(x = as.character(bj_aq_data$time),
                            format = "%Y-%m-%d %H:%M:%S")

#beijing - removing nulls
bj_aq_data_nonulls <- bj_aq_data %>% 
  group_by(station_id) %>% 
  mutate(PM2.5=zoo::na.locf(PM25_Concentration, na.rm=FALSE))

bj_aq_data_nonulls <- bj_aq_data_nonulls %>% 
  group_by(station_id) %>% 
  mutate(PM10=zoo::na.locf(PM10_Concentration, na.rm=FALSE))

bj_aq_data_nonulls <- bj_aq_data_nonulls %>% 
  group_by(station_id) %>% 
  mutate(PMO3=zoo::na.locf(O3_Concentration, na.rm=FALSE))

#removing old columns - beijing
bj_aq_data_nonulls$PM25_Concentration <- NULL
bj_aq_data_nonulls$PM10_Concentration <- NULL
bj_aq_data_nonulls$O3_Concentration <- NULL

# pull data for London
ld_aq_url <- paste0("https://biendata.com/competition/airquality/ld/",startDate,"/",endDate,"/2k0d1d8")
ld_aq_file <- getURL(ld_aq_url, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
ld_aq_data <- read.csv(textConnection(ld_aq_file), header=TRUE)

#delete 4 columns
ld_aq_data$id <- NULL
ld_aq_data$CO_Concentration <- NULL
ld_aq_data$SO2_Concentration <- NULL
ld_aq_data$NO2_Concentration <- NULL
ld_aq_data$O3_Concentration <-NULL

#get rid of london stations not needed
ld_aq_data <- subset(ld_aq_data, station_id %in% c("CD1", "BL0", "GR4", "MY7", "HV1", "GN3", "GR9", "LW2",
   "GN0", "KF1", "CD9", "ST5", "TH4"))


#format time correctly
ld_aq_data$time <- as.POSIXct(x = as.character(ld_aq_data$time),
                            format = "%Y-%m-%d %H:%M:%S")

#london - removing nulls
ld_aq_data_nonulls <- ld_aq_data %>% 
  group_by(station_id) %>% 
  mutate(PM2.5=zoo::na.locf(PM25_Concentration, na.rm=FALSE))

ld_aq_data_nonulls <- ld_aq_data_nonulls %>% 
  group_by(station_id) %>% 
  mutate(PM10=zoo::na.locf(PM10_Concentration, na.rm=FALSE))


#removing old columns - london
ld_aq_data_nonulls$PM25_Concentration <- NULL
ld_aq_data_nonulls$PM10_Concentration <- NULL

#removing old DFs
rm(ld_aq_data)
rm(bj_aq_data)

#plotting forecasts per station
#london
ggplot(data = ld_aq_data_nonulls, aes(x = time)) + geom_line(aes(y = ld_aq_data_nonulls$PM2.5
                                                         , colour = ld_aq_data_nonulls$station_id))


ggplot(data = ld_aq_data_nonulls, aes(x = time)) + geom_line(aes(y = ld_aq_data_nonulls$PM10
                                                         , colour = ld_aq_data_nonulls$station_id))



#beijing
ggplot(data = bj_aq_data_nonulls, aes(x = time)) + geom_line(aes(y = bj_aq_data_nonulls$PM2.5
                                                         , colour = bj_aq_data_nonulls$station_id))

ggplot(data = bj_aq_data_nonulls, aes(x = time)) + geom_line(aes(y = bj_aq_data_nonulls$PM10
                                                         , colour = bj_aq_data_nonulls$station_id))

ggplot(data = bj_aq_data_nonulls, aes(x = time)) + geom_line(aes(y = bj_aq_data_nonulls$PMO3
                                                         , colour = bj_aq_data_nonulls$station_id))





#breaking it down to one station, one predictor
subset.dongsti <- subset(bj_aq_data_nonulls, station_id == "dongsi_aq")

  #removing 3 columns
  subset.dongsti$station_id <-NULL
  subset.dongsti$PM10 <-NULL
  subset.dongsti$PMO3 <-NULL
    vec.seasonal = ts(subset.dongsti$PM2.5, start = 2018/05/01, freq = 24)
  plot(vec.seasonal)
  
  ts.model = function(ts, col = 'remainder', order = c(1,0,0)){
    mod = arima(ts, order = order, include.mean = FALSE)
    print(mod)
    mod
  }
  dongsti.arima = ts.model(vec.seasonal, col = 'ARIMA model for icecream production', order = c(1,0,0))
  
  require(forecast)
  fit.dongsti = auto.arima(vec.seasonal, max.p=3, max.q=3,
                           max.P=2, max.Q=2, max.order=5, max.d=2, max.D=1,
                           start.p=0, start.q=0, start.P=0, start.Q=0)
  summary(fit.dongsti)
  

  ## Make the forecast for the next year
  dongsti.forecast = forecast(fit.dongsti, h=48)
  summary(dongsti.forecast)
  plot(dongsti.forecast)
  
  #seasonality
  count_ma = ts(na.omit(subset.dongsti$PM2.5), frequency=24)
  decomp = stl(count_ma, s.window="periodic")
  deseasonal_cnt <- seasadj(decomp)
  plot(decomp)

  #results from below show that it is stationary
  adf.test(count_ma, alternative = "stationary")  

  #acf
  Acf(count_ma, main='')
  #pacf
  Pacf(count_ma, main='')
  
  count_d1 = diff(deseasonal_cnt, differences = 1)
  plot(count_d1)
  adf.test(count_d1, alternative = "stationary")

  Acf(count_d1, main='ACF for Differenced Series')
  Pacf(count_d1, main='PACF for Differenced Series')  

  auto.arima(deseasonal_cnt, seasonal=FALSE)  
  
  fit<-auto.arima(deseasonal_cnt, seasonal=FALSE)
  
  tsdisplay(residuals(fit), lag.max=45, main='(1,0,1) Model Residuals')
  
  fit2 = arima(deseasonal_cnt, order=c(1,0,7))
  
  fit2
  
  #no seasonality
tsdisplay(residuals(fit2), lag.max=20, main='Seasonal Model Residuals')

fcast <- forecast(fit2, h=48)
plot(fcast)

#seasonality
fit_w_seasonality = auto.arima(deseasonal_cnt, seasonal=TRUE)
fit_w_seasonality

seas_fcast <- forecast(fit_w_seasonality, h=48)
plot(seas_fcast)

summary(seas_fcast)
class(seas_fcast)
seas_fcast <- data.frame(seas_fcast)
seas_fcast$hour <- 0:22
str(seas_fcast)
seas_fcast$rowname

seas_fcast$testrow <- row.names(seas_fcast)

rownames(seas_fcast) <- 0:47
