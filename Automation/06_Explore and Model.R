

#### data for modeling
station_BL0 <- sqldf('
  SELECT *
     FROM aqi_meo
  ')     

setwd("C:/Users/jjakubi/Documents/R/KDD_Cup_2018/LD_Historical_Data")
write.csv(station_BL0, file = "station_BL0.csv")

#### data exploration
data <- station_BL0

head(data)
str(data)
nrows <- nrow(data)
ncols <- ncol(data)

print(paste("There are ", nrows, " rows and ", ncols, " columns.", sep=""))

na_index <- is.na(data)
print(paste("Missing data rate = ", round(sum(na_index)/(nrows*ncols)*100,2), "%", sep=""))


# get null count
data01 <- station_BL0
na_cnt <-data.frame(sapply(data01, function(y) sum(length(which(is.na(y))))))
na_cnt$row_cnt <- nrow(data01)
na_cnt

# keep relevent columns
colName02 <- c('PM25', 'DayofWeek',  'Month', 'Hour', 'meo_temperature','meo_pressure', 'meo_humidity', 'meo_wind_direction', 'meo_wind_speed')
data02 <- subset(data01, select= colName02)
str(data02)

# remove null values
row.has.na <- apply(data02, 1, function(x){any(is.na(x))})
data03 <- data02[!row.has.na,]

# variable transformation
data03$PM25 <- log(data03$PM25)

# partition for testing and training
num_obs = nrow(data03)
set.seed(1234)

index_shuffled <- sample(num_obs)
training_ratio <- 0.75

training_size <- round(num_obs * training_ratio)
training_index <- index_shuffled[1:training_size]
testing_index <- index_shuffled[(training_size+1):num_obs]
training_data <- data03[training_index,]
testing_data <- data03[testing_index,]


# build a linear model
factors <- c("DayofWeek", "Month", "Hour", "meo_temperature", "meo_pressure", "meo_humidity", "meo_wind_direction", "meo_wind_speed")
lmFactors <- as.formula(paste("PM25~", paste(factors, collapse="+")))

model01 <- lm(lmFactors, data=training_data, na.action=na.omit)  # build the model
summary(model01)
