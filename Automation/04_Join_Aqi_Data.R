
# modify closest stations
station01 <- london_closest_stations
colName <- c("station1", "station2", "distance")

station02 <- subset(station01, select= colName)
oldcolName <- names(station02)
newcolName <- c("Station_ID","Grid_ID", "Distance")

setnames(station02, old = oldcolName, new = newcolName)

# rename columns
data01 <- LD_historical_aqi
oldName01 <- c('station_id', 'MeasurementDateGMT', 'PM2.5..ug.m3.', 'PM10..ug.m3.', 'NO2..ug.m3.')
newName01 <- c('Station_ID', 'DTTM', 'PM25', 'PM10', 'NO2')

data02 <- LD_historical_aqi_other
oldName02 <- c('Station_ID', 'MeasurementDateGMT', 'PM2.5..ug.m3.', 'PM10..ug.m3.', 'NO2..ug.m3.')
newName02 <- c('Station_ID', 'DTTM', 'PM25', 'PM10', 'NO2')

data03 <- ld_aq_data
oldName03 <- c('station_id', 'time', 'PM25_Concentration', 'PM10_Concentration', 'NO2_Concentration')
newName03 <- c('Station_ID', 'DTTM', 'PM25', 'PM10', 'NO2')

library(data.table)
setnames(data01, old = oldName01, new = newName01)
setnames(data02, old = oldName02, new = newName02)
setnames(data03, old = oldName03, new = newName03)

# keep relevant columns
aqi_hist01 <- subset(data01, select= newName01)
aqi_hist02 <- subset(data02, select= newName02)
aqi_hist03 <- subset(data03, select= newName03)

rm(data01)
rm(data02)
rm(data03)

# data source indicator
aqi_hist01$data_type <- 'hist'
aqi_hist02$data_type <- 'hist'
aqi_hist03$data_type <- 'api'

aqi_hist01$DTTM <- as.POSIXct(aqi_hist01$DTTM, format = "%Y/%m/%d %H:%M", tz = "GMT")
aqi_hist02$DTTM <- as.POSIXct(aqi_hist02$DTTM, format = "%Y/%m/%d %H:%M", tz = "GMT")
aqi_hist03$DTTM <- as.POSIXct(aqi_hist03$DTTM, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")

# summarize
head(aqi_hist01)
head(aqi_hist02)
head(aqi_hist03)

str(aqi_hist01)
str(aqi_hist02)
str(aqi_hist03)

# append data
aqi_hist04 <- rbind(aqi_hist01,aqi_hist02,aqi_hist03)

head(aqi_hist04)
str(aqi_hist04)

# add date fields
aqi_hist04$DayofWeek <- as.numeric(format(aqi_hist04$DTTM,"%w"))
aqi_hist04$WeekofYear <- as.numeric(strftime(aqi_hist04$DTTM,format = "%W"))
aqi_hist04$Year <- year(aqi_hist04$DTTM)
aqi_hist04$Quarter <- quarter(aqi_hist04$DTTM)
aqi_hist04$Month <- month(aqi_hist04$DTTM)
aqi_hist04$Hour <- hour(aqi_hist04$DTTM)

# summarize
head(aqi_hist04)
str(aqi_hist04)

setnames(LD_aqi_stations, old = "X", new = "Station_ID")
head(LD_aqi_stations)

# merge aqi data and station information
aqi_hist05 <- merge(aqi_hist04, LD_aqi_stations, by = "Station_ID")


head(aqi_hist05)
head(station02)

aqi_hist06 <- merge(aqi_hist05, station02, by = "Station_ID")
str(aqi_hist06)

rm(aqi_hist01)
rm(aqi_hist02)
rm(aqi_hist03)
rm(aqi_hist04)
rm(aqi_hist05)


head(aqi_hist05)
head(london_closest_stations)

str(aqi_hist05)
str(london_closest_stations)

setnames(london_closest_stations, old = oldName04, new = newName04)
aqi_hist06 <- merge(aqi_hist05, london_closest_stations01, by = "Station_ID", all.x = TRUE)
str(aqi_hist06)


aqi_meo01 <- merge(aqi_hist06, meo_hist05, by = c("Grid_ID", "DTTM"), all.x = TRUE)
str(aqi_meo01)

# setwd("C:/Users/jjakubi/Documents/R/KDD_Cup_2018/LD_Historical_Data")
# write.csv(aqi_meo01, file = "aqi_meo_hist.csv")