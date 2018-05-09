
if(!is.null(dev.list())) dev.off() # Clear Plots
rm(list=ls()) # Clear objects from Memory
cat("\014") # Clear Console
# writeClipboard(as.character(x)) # copy data frame to clipboard

# install.packages("RCurl")
library(RCurl)

# set working directory
setwd("C:/Users/jjakubi/Documents/R/KDD_Cup_2018")

# set city id
#cityID <- data.frame(c('bj','ld'))
cityID <- data.frame(c('ld'))
colnames(cityID) <- c('CityID')

# set start and end date
startDate <- '2018-03-31-0'
endDate <- '2018-06-01-0'
today <- Sys.Date()


# pull data for each city id
for (i in 1:nrow(cityID)) {
  
# acquire air quality data
  assign(paste0(cityID[i,1],"_aq_url"),paste0("https://biendata.com/competition/airquality/",cityID[i,1],"/",startDate,"/",endDate,"/2k0d1d8"))
  assign(paste0(cityID[i,1],"_aq_file"),getURL(eval(as.symbol(paste0(cityID[i,1],"_aq_url"))), ssl.verifyhost=FALSE, ssl.verifypeer=FALSE))
  assign(paste0(cityID[i,1],"_aq_data"),read.csv(textConnection(eval(as.symbol(paste0(cityID[i,1],"_aq_file")))), header=TRUE))
  
  # acquire grid meteorology data
  assign(paste0(cityID[i,1],"_gm_url"),paste0("https://biendata.com/competition/meteorology/",cityID[i,1],"_grid/",startDate,"/",endDate,"/2k0d1d8"))
  assign(paste0(cityID[i,1],"_gm_file"),getURL(eval(as.symbol(paste0(cityID[i,1],"_gm_url"))), ssl.verifyhost=FALSE, ssl.verifypeer=FALSE))
  assign(paste0(cityID[i,1],"_gm_data"),read.csv(textConnection(eval(as.symbol(paste0(cityID[i,1],"_gm_file")))), header=TRUE))
  
  # acquire 48-hour weather forecast
  assign(paste0(cityID[i,1],"_wf_url"),paste0("http://kdd.caiyunapp.com/competition/forecast/",cityID[i,1],"/",today,"-00/2k0d1d8"))
  assign(paste0(cityID[i,1],"_wf_file"),getURL(eval(as.symbol(paste0(cityID[i,1],"_wf_url"))), ssl.verifyhost=FALSE, ssl.verifypeer=FALSE))
  assign(paste0(cityID[i,1],"_wf_data"),read.csv(textConnection(eval(as.symbol(paste0(cityID[i,1],"_wf_file")))), header=TRUE))
  
}

# acquire beijing observed meteorology data
# bj_om_url <- paste0("https://biendata.com/competition/meteorology/bj/",startDate,"/",endDate,"/2k0d1d8")
# bj_om_file <- getURL(bj_om_url, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
# bj_om_data <- read.csv(textConnection(bj_om_file), header=FALSE)

# remove cityID data frame
rm("cityID")
