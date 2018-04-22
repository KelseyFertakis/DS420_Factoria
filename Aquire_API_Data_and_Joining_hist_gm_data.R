
if(!is.null(dev.list())) dev.off() # Clear Plots
rm(list=ls()) # Clear objects from Memory
cat("\014") # Clear Console
# writeClipboard(as.character(x)) # copy data frame to clipboard

#install.packages("RCurl")
library(RCurl)
#install.packages("digest")
#install.packages("sqldf")
library(sqldf)
library(digest)

# set working directory
setwd("C:/Users/Kelse/Documents/UW_MachineLearning420/KDDCup")

# set city id
cityID <- data.frame(c('bj','ld'))
colnames(cityID) <- c('CityID')

# set start and end date
startDate <- '2018-03-31-0'
endDate <- '2018-06-01-0'

# read.csv Beijing historical GM
bj_gm_hist_file <- "C:\\Users\\Kelse\\Documents\\UW_MachineLearning420\\KDDCup\\Beijing_historical_meo_grid.csv"
bj_gm_hist_data <- read.csv(bj_gm_hist_file, header=TRUE, sep=",", stringsAsFactors = FALSE)

# read.csv London historical GM
ld_gm_hist_file <- "C:\\Users\\Kelse\\Documents\\UW_MachineLearning420\\KDDCup\\London_historical_meo_grid.csv"
ld_gm_hist_data <- read.csv(ld_gm_hist_file, header=TRUE, sep=",", stringsAsFactors = FALSE)


# pull data for each city id
for (i in 1:nrow(cityID)) {

  # acquire air quality data
  assign(paste0(cityID[i,1],"_aq_url"),paste0("https://biendata.com/competition/airquality/",cityID[i,1],"/",startDate,"/",endDate,"/2k0d1d8"))
  assign(paste0(cityID[i,1],"_aq_file"),getURL(eval(as.symbol(paste0(cityID[i,1],"_aq_url"))), ssl.verifyhost=FALSE, ssl.verifypeer=FALSE))
  assign(paste0(cityID[i,1],"_aq_data"),read.csv(textConnection(eval(as.symbol(paste0(cityID[i,1],"_aq_file")))), header=TRUE))
  
  # acquire API grid meteorology data
  assign(paste0(cityID[i,1],"_gm_url"),paste0("https://biendata.com/competition/meteorology/",cityID[i,1],"_grid/",startDate,"/",endDate,"/2k0d1d8"))
  assign(paste0(cityID[i,1],"_gm_file"),getURL(eval(as.symbol(paste0(cityID[i,1],"_gm_url"))), ssl.verifyhost=FALSE, ssl.verifypeer=FALSE))
  assign(paste0(cityID[i,1],"_gm_data"),read.csv(textConnection(eval(as.symbol(paste0(cityID[i,1],"_gm_file")))), header=TRUE))
}

# acquire beijing observed meteorology data
#bj_om_url <- paste0("https://biendata.com/competition/meteorology/bj/",startDate,"/",endDate,"/2k0d1d8")
#bj_om_file <- getURL(bj_om_url, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
#bj_om_data <- read.csv(textConnection(bj_om_file), header=FALSE)

# read.csv Lat long Grid Weather Station Beijing
bj_grid_loc_file <- "C:\\Users\\Kelse\\Documents\\UW_MachineLearning420\\KDDCup\\Beijing_grid_weather_station.csv"
bj_grid_loc_data <- read.csv(bj_grid_loc_file, header=FALSE, sep=",", stringsAsFactors = FALSE)

# read.csv Lat long Grid Weather Station London
ld_grid_loc_file <- "C:\\Users\\Kelse\\Documents\\UW_MachineLearning420\\KDDCup\\London_grid_weather_station.csv"
ld_grid_loc_data <- read.csv(ld_grid_loc_file, header=FALSE, sep=",", stringsAsFactors = FALSE)


#London and Beijing join hist with API data
#first clean to match columns
ld_gm_data$id <- NULL 
bj_gm_data$id <- NULL 

#rename to match
names(ld_grid_loc_data) =
  c("stationName", "latitude", "longitude")
names(bj_grid_loc_data) =
  c("stationName", "latitude", "longitude")

#rename to get ready for merging hist with API
colnames(ld_gm_data)[which(colnames(ld_gm_data) == 'station_id')] <- 'stationName'
colnames(ld_gm_data)[which(colnames(ld_gm_data) == 'time')] <- 'utc_time'
colnames(ld_gm_hist_data)[which(colnames(ld_gm_hist_data) == 'wind_speed.kph')] <- 'wind_speed'
ld_gm_hist_data$weather <- 0 
colnames(bj_gm_data)[which(colnames(bj_gm_data) == 'station_id')] <- 'stationName'
colnames(bj_gm_data)[which(colnames(bj_gm_data) == 'time')] <- 'utc_time'
colnames(bj_gm_hist_data)[which(colnames(bj_gm_hist_data) == 'wind_speed.kph')] <- 'wind_speed'
bj_gm_hist_data$weather <- 0 

#merge to get lat/long from file
ld_gm_merge <- sqldf("Select a.StationName, b.longitude, b.latitude, a.utc_time, a.weather, a.temperature, a.pressure, a.humidity, a.wind_direction, a.wind_speed 
        From ld_gm_data as a 
                     left join ld_grid_loc_data as b
                     on a.stationName = b.stationName")
bj_gm_merge <- sqldf("Select a.StationName, b.longitude, b.latitude, a.utc_time, a.weather, a.temperature, a.pressure, a.humidity, a.wind_direction, a.wind_speed 
        From bj_gm_data as a 
                     left join bj_grid_loc_data as b
                     on a.stationName = b.stationName")

#re-order hist data for joining to API
ld_gm_hist_data <- ld_gm_hist_data[c(1,2,3,4,10,5,6,7,8,9)]
bj_gm_hist_data <- bj_gm_hist_data[c(1,2,3,4,10,5,6,7,8,9)]

#append gm API merge files to hist files to get all data in one df
ld_gm_all <- rbind(ld_gm_hist_data, ld_gm_merge)
bj_gm_all <- rbind(bj_gm_hist_data, bj_gm_merge)

#remove dataframes that we will no longer use
rm(ld_gm_data, ld_gm_merge, ld_gm_hist_data, ld_grid_loc_data, bj_gm_data, bj_gm_merge, bj_gm_hist_data, bj_grid_loc_data)

# remove cityID data frame
rm("cityID")

# list of available data frames
df.list <- names(which(unlist(eapply(.GlobalEnv,is.data.frame))))

# wirte data frames to working directory
for (i in 1:length(df.list)) {
  write.table(get(df.list[i]), file = paste(df.list[i],".csv"))
}

# preview data
head(bj_aq_data)
head(ld_aq_data)
head(bj_gm_all)
head(ld_gm_all)
#head(bj_om_data)

# row count
nrow(bj_aq_data)
nrow(ld_aq_data)
nrow(bj_gm_all)
nrow(ld_gm_all)
#nrow(bj_om_data)




