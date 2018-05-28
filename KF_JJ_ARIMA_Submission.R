if(!is.null(dev.list())) dev.off() # Clear Plots
rm(list=ls()) # Clear objects from Memory
cat("\014") # Clear Console

library(data.table)
setwd("C:/Users/Kelse/Documents/GitHub/DS420_Factoria")

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


bj_aq_data_nonulls$PM2.5 <-
  ifelse((bj_aq_data_nonulls$station_id %in% 'zhiwuyuan_aq'), '0', bj_aq_data_nonulls$PM2.5)
bj_aq_data_nonulls$PM10 <-
  ifelse((bj_aq_data_nonulls$station_id %in% 'zhiwuyuan_aq'), '0', bj_aq_data_nonulls$PM10)
bj_aq_data_nonulls$PMO3 <-
  ifelse((bj_aq_data_nonulls$station_id %in% 'zhiwuyuan_aq'), '0', bj_aq_data_nonulls$PMO3)

sqldf('
          SELECT *
            FROM bj_aq_data_nonulls
            WHERE station_id = "zhiwuyuan_aq"

                  ')

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

aqi.list <- sqldf('
          SELECT station_id
            FROM bj_aq_data_nonulls
            GROUP BY 1
                  ')

aqi.list.ld <- sqldf('
          SELECT station_id
            FROM ld_aq_data_nonulls
            GROUP BY 1
                  ')

# for (i in 1:nrow(aqi.list))
datalist <- list()
datalist.ld <- list()

#### PM2.5 bj
for (i in 1:nrow(aqi.list)) {
  
aqi.data <- subset(bj_aq_data_nonulls, station_id == aqi.list[i,1]) 

aqi.data$PM10 <-NULL
aqi.data$PMO3 <-NULL

#seasonality
count_ma = ts(na.omit(aqi.data$PM2.5), frequency=24)
decomp = stl(count_ma, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)

#seasonality
fit_w_seasonality = auto.arima(deseasonal_cnt, seasonal=TRUE)

seas_fcast <- data.frame(forecast(fit_w_seasonality, h=48))

seas_fcast$station_id <- aqi.list[i,1]
rownames(seas_fcast) <- 0:47
seas_fcast$rownum <- row.names(seas_fcast)

datalist[[i]] <- seas_fcast


}

aqi.forecast.PM25 = do.call(rbind,datalist)

aqi.forecast.PM25 <- subset(aqi.forecast.PM25, select= c(rownum, station_id, Point.Forecast))
setnames(aqi.forecast.PM25, old = 'Point.Forecast', new = 'PM2.5')


#### PM10 bj
for (i in 1:nrow(aqi.list)) {
  
  aqi.data <- subset(bj_aq_data_nonulls, station_id == aqi.list[i,1]) 
  
  aqi.data$PM2.5 <-NULL
  aqi.data$PMO3 <-NULL
  
  #seasonality
  count_ma = ts(na.omit(aqi.data$PM10), frequency=24)
  decomp = stl(count_ma, s.window="periodic")
  deseasonal_cnt <- seasadj(decomp)
  
  #seasonality
  fit_w_seasonality = auto.arima(deseasonal_cnt, seasonal=TRUE)
  
  seas_fcast <- data.frame(forecast(fit_w_seasonality, h=48))
  
  seas_fcast$station_id <- aqi.list[i,1]
  rownames(seas_fcast) <- 0:47
  seas_fcast$rownum <- row.names(seas_fcast)
  
  datalist[[i]] <- seas_fcast
  
  
}

aqi.forecast.PM10 = do.call(rbind,datalist)

aqi.forecast.PM10 <- subset(aqi.forecast.PM10, select= c(rownum, station_id, Point.Forecast))
setnames(aqi.forecast.PM10, old = 'Point.Forecast', new = 'PM10')


#### PMO3 bj
for (i in 1:nrow(aqi.list)) {
  
  aqi.data <- subset(bj_aq_data_nonulls, station_id == aqi.list[i,1]) 
  
  aqi.data$PM2.5 <-NULL
  aqi.data$PM10 <-NULL
  
  #seasonality
  count_ma = ts(na.omit(aqi.data$PMO3), frequency=24)
  decomp = stl(count_ma, s.window="periodic")
  deseasonal_cnt <- seasadj(decomp)
  
  #seasonality
  fit_w_seasonality = auto.arima(deseasonal_cnt, seasonal=TRUE)
  
  seas_fcast <- data.frame(forecast(fit_w_seasonality, h=48))
  
  seas_fcast$station_id <- aqi.list[i,1]
  rownames(seas_fcast) <- 0:47
  seas_fcast$rownum <- row.names(seas_fcast)
  
  datalist[[i]] <- seas_fcast
  
}

aqi.forecast.PMO3 = do.call(rbind,datalist)

aqi.forecast.PMO3 <- subset(aqi.forecast.PMO3, select= c(rownum, station_id, Point.Forecast))
setnames(aqi.forecast.PMO3, old = 'Point.Forecast', new = 'PMO3')

a <- merge(aqi.forecast.PM25, aqi.forecast.PM10, by = c("station_id", "rownum"))
bj.aqi.forecast <- merge(a, aqi.forecast.PMO3, by = c("station_id", "rownum"))
bj.aqi.forecast

#creating the test_id for final submission file
bj.aqi.forecast$test_id <- paste(bj.aqi.forecast$station_id, "#", bj.aqi.forecast$rownum, sep = "")

getwd()
write.csv(bj.aqi.forecast[,c("test_id", "PM2.5", "PM10", "PMO3")], file = paste("arima_bj_submission", Sys.Date(),".csv"), row.names = FALSE)


#london

#### PM2.5 ld
for (i in 1:nrow(aqi.list.ld)) {
  
  aqi.data.ld <- subset(ld_aq_data_nonulls, station_id == aqi.list.ld[i,1]) 
  
  aqi.data.ld$PM10 <-NULL
  aqi.data.ld$PMO3 <-NULL
  
  #seasonality
  count_ma.ld = ts(na.omit(aqi.data.ld$PM2.5), frequency=24)
  decomp.ld = stl(count_ma.ld, s.window="periodic")
  deseasonal_cnt.ld <- seasadj(decomp.ld)
  
  #seasonality
  fit_w_seasonality.ld = auto.arima(deseasonal_cnt.ld, seasonal=TRUE)
  
  seas_fcast.ld <- data.frame(forecast(fit_w_seasonality.ld, h=48))
  
  seas_fcast.ld$station_id <- aqi.list.ld[i,1]
  rownames(seas_fcast.ld) <- 0:47
  seas_fcast.ld$rownum <- row.names(seas_fcast.ld)
  
  datalist.ld[[i]] <- seas_fcast.ld
  
  
}

aqi.forecast.ld.PM25 = do.call(rbind,datalist.ld)

aqi.forecast.ld.PM25 <- subset(aqi.forecast.ld.PM25, select= c(rownum, station_id, Point.Forecast))
setnames(aqi.forecast.ld.PM25, old = 'Point.Forecast', new = 'PM2.5')


#### PM10 ld
for (i in 1:nrow(aqi.list.ld)) {
  
  aqi.data.ld <- subset(ld_aq_data_nonulls, station_id == aqi.list.ld[i,1]) 
  
  aqi.data.ld$PM2.5 <-NULL
  aqi.data.ld$PMO3 <-NULL
  
  #seasonality
  count_ma.ld = ts(na.omit(aqi.data.ld$PM10), frequency=24)
  decomp.ld = stl(count_ma.ld, s.window="periodic")
  deseasonal_cnt.ld <- seasadj(decomp.ld)
  
  #seasonality
  fit_w_seasonality.ld = auto.arima(deseasonal_cnt.ld, seasonal=TRUE)
  
  seas_fcast.ld <- data.frame(forecast(fit_w_seasonality.ld, h=48))
  
  seas_fcast.ld$station_id <- aqi.list.ld[i,1]
  rownames(seas_fcast.ld) <- 0:47
  seas_fcast.ld$rownum <- row.names(seas_fcast.ld)
  
  datalist.ld[[i]] <- seas_fcast.ld
  
  
}

aqi.forecast.ld.PM10 = do.call(rbind,datalist.ld)

aqi.forecast.ld.PM10 <- subset(aqi.forecast.ld.PM10, select= c(rownum, station_id, Point.Forecast))
setnames(aqi.forecast.ld.PM10, old = 'Point.Forecast', new = 'PM10')


ld.aqi.forecast <- merge(aqi.forecast.ld.PM25, aqi.forecast.ld.PM10, by = c("station_id", "rownum"))
ld.aqi.forecast

#creating the test_id for final submission file
ld.aqi.forecast$test_id <- paste(ld.aqi.forecast$station_id, "#", ld.aqi.forecast$rownum, sep = "")


write.csv(ld.aqi.forecast[,c("test_id", "PM2.5", "PM10")], file = paste("arima_ld_submission", Sys.Date(),".csv"), row.names = FALSE)


