

#### join api and historical weather data
head(LD_historical_meo_grid)
head(ld_gm_data)

str(LD_historical_meo_grid)
str(ld_gm_data)
str(ld_grid_map)

# remove column for sensor
meo_hist01 <- subset(LD_historical_meo_grid, select= -c(longitude, latitude))
meo_hist02 <- subset(ld_gm_data, select= -c(id, weather))
# meo_hist03 <- subset(ld_wf_data, select= -c(id, weather))

# column names
oldName01 <- names(meo_hist01)
newName01 <- names(meo_hist02)

# rename colums to match
library(data.table)
setnames(meo_hist01, old = oldName01, new = newName01)
# setnames(meo_hist03, old = 'forecast_time', new = 'time')
setnames(ld_grid_map, old = 'GridID', new = 'station_id')

# data source indicator
meo_hist01$data_type <- 'hist'
meo_hist02$data_type <- 'api'
# meo_hist03$data_type <- 'fcst'

# convert time from factor to time stamp
meo_hist01$time <- as.POSIXct(meo_hist01$time, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
meo_hist02$time <- as.POSIXct(meo_hist02$time, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
# meo_hist03$time <- as.POSIXct(meo_hist03$time, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")

str(meo_hist01)
str(meo_hist02)
# str(meo_hist03)

# append data
meo_hist04 <- rbind(meo_hist01, meo_hist02)
str(meo_hist04)

# remove prior data objects
rm(LD_historical_meo_grid)
rm(ld_gm_data)

rm(meo_hist01)
rm(meo_hist02)

# add latitude and longitude
meo_hist05 <- merge(meo_hist04, ld_grid_map, by = "station_id")
str(meo_hist05)

rm(meo_hist04)


oldName02 <- names(meo_hist05)
newName02 <- c("Grid_ID", "DTTM", "meo_temperature", "meo_pressure", "meo_humidity", "meo_wind_direction", "meo_wind_speed", "meo_data_type", "meo_latitude", "meo_longitude")
setnames(meo_hist05, old = oldName02, new = newName02)

# setwd("C:/Users/jjakubi/Documents/R/KDD_Cup_2018/LD_Historical_Data")
# write.csv(meo_hist05, file = "meo_hist.csv")



#### meo forecast data
meo_hist03 <- subset(ld_wf_data, select= -c(id, weather))
setnames(meo_hist03, old = 'forecast_time', new = 'time')
meo_hist03$data_type <- 'fcst'
meo_hist03$time <- as.POSIXct(meo_hist03$time, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
str(meo_hist03)