##################################
# Parallelization
##################################

library(doSNOW)
library(foreach)
library(parallel)

# Setting number of cores in your machine. 
num_cores <- detectCores()
cl <- makeCluster(num_cores-1)
registerDoSNOW(cl)

# set working directory
setwd("C:/Users/jjakubi/Documents/R/KDD_Cup_2018/LD_Historical_Data")

file.list = list.files(pattern="*.csv")
rmFile <- c("LD_grid_weather_station.csv","meo_hist.csv")
file.list <- setdiff(file.list,rmFile)

list2env(
  lapply(setNames(file.list, make.names(gsub("*.csv$", "", file.list))), 
         read.csv), envir = .GlobalEnv)

ld_grid_map <- read.csv('LD_grid_weather_station.csv', header = F)
colnames(ld_grid_map) <- c('GridID','latitude','longitude')

stopCluster(cl)

