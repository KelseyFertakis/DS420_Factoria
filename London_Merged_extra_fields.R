if(!is.null(dev.list())) dev.off() # Clear Plots
rm(list=ls()) # Clear objects from Memory
cat("\014") # Clear Console

# set working directory
setwd("C:/Users/Kelse/Documents/UW_MachineLearning420/KDDCup")

# read.csv 
ld_aq_gm_hist_file <- "C:\\Users\\Kelse\\Documents\\UW_MachineLearning420\\KDDCup\\Merged_ld_aq_gm_historical.csv"
ld_aq_gm_hist_data <- read.csv(ld_aq_gm_hist_file, header=TRUE, sep=",", stringsAsFactors = FALSE)

str(ld_aq_gm_hist_data)

#adding columns
#Hour of day

ld_aq_gm_hist_data$hour <- substr(ld_aq_gm_hist_data$utc_time, 12, 13)

ld_aq_gm_hist_data$month <- substr(ld_aq_gm_hist_data$utc_time, 6,7)

ld_aq_gm_hist_data$date <- substr(ld_aq_gm_hist_data$utc_time, 1,10)


install.packages("chron")
library(chron)

ld_aq_gm_hist_data$weekend = chron::is.weekend(ld_aq_gm_hist_data$date)


# list of available data frames
df.list <- names(which(unlist(eapply(.GlobalEnv,is.data.frame))))

# wirte data frames to working directory
for (i in 1:length(df.list)) {
  write.table(get(df.list[i]), file = paste(df.list[i],".csv"))
}


