

if(!is.null(dev.list())) dev.off() # Clear Plots
rm(list=ls()) # Clear objects from Memory
cat("\014") # Clear Console
# writeClipboard(as.character(x)) # copy data frame to clipboard

# set working directory
setwd("C:/Users/jjakubi/Documents/R/KDD_Cup_2018")

file.list = list.files(pattern="*.csv")
list2env(
  lapply(setNames(file.list, make.names(gsub("*.csv$", "", file.list))), 
         read.csv), envir = .GlobalEnv)

head(Beijing_historical_meo_grid)
