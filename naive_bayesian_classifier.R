install.packages("naivebayes")
library(naivebayes)
require(naivebayes)
library(e1071)
require(e1071)
??e1071
#if(!is.null(dev.list())) dev.off() # Clear Plots
#rm(list=ls())
#cat("\014")

#getwd()
setwd("C:/Users/Kelse/Documents/UW_MachineLearning420/WorkingDirectoryR")
winefile <- "C:\\Users\\Kelse\\Documents\\UW_MachineLearning420\\WorkingDirectoryR\\RedWhiteWine.csv"
winedata <- read.csv(winefile, header=TRUE, sep=",", stringsAsFactors = FALSE)

#taking out the 'quality' column
winedata$quality <- NULL  

# If the target column, say column class, is numerical, you need to convert it to factors in R for classification problem
# winedata$Class <- factor(winedata$Class) or : 
winedata$Class <-
  ifelse((winedata$Class %in% 1), "Red", "White")

#checking data
head(winedata)

nrows <- nrow(winedata)
smp_size <- floor(0.7 * nrows)
## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrows), size = smp_size)

train <- winedata[train_ind, ]
test <- winedata[-train_ind, ]

wine_nbc <- naive_bayes(Class ~ ., data = train)
wine_nbc_2 <- naive_bayes(Class ~ total.sulfur.dioxide + alcohol, data = train)
plot(wine_nbc)

#testing accuracy on all sensors
wine_test_pred <- predict(wine_nbc_2, test)
accuracy <- sum(wine_test_pred==test[,'Class'])/length(wine_test_pred)
print(paste("Test accuracy =", round(accuracy*100,2), "%", sep=""))


#testing accuracy on a couple sensors
winedata_partial <- winedata[,c("volatile.acidity","total.sulfur.dioxide","Class")] 

train2 <- winedata_partial[train_ind, ]
test2 <- winedata_partial[-train_ind, ]

wine_nbc_partial <- naive_bayes(Class ~ ., data = train2)
plot(wine_nbc_partial)
wine_test_pred_partial <- predict(wine_nbc_partial, test2)
accuracy_partial <- sum(wine_test_pred_partial==test2[,'Class'])/length(wine_test_pred_partial)
print(paste("Test accuracy (partial) =", round(accuracy_partial*100,2), "%", sep=""))


# In your homework, you need to split the data into training and testing sets.
# smp_size <- floor(0.7 * nrow(data))
# set_seed(123)
# train_index <- sample(seq_len(nrow(data)), size = smp_size)
# train <- data[train_index,]
# test <- data[-train_index,]

library(naivebayes)
nbc <- naive_bayes(play ~ ., data = data)
plot(nbc)

# In your homework, you need to test your model performance on testing data
# test_pred <- predict(nbc, test)
# accuracy <- sum(test_pred == test$class)/nrow(test)*100
# print(paste("Your testing accuracy is ", round(accuracy, 2), "%", sep=""))

# Test the NBC model on a real case
test_data <- data.frame(outlook="sunny", temp="cool", humidity="high", windy="TRUE", stringsAsFactors=FALSE)
predict(nbc, test_data)
head(predict(nbc, test_data, type = "prob"))

# NBC with numerical features
iris_file <- "https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"
iris_data <- read.csv(iris_file, header=FALSE, sep=",", stringsAsFactors=FALSE)
colnames(iris_data) <- c("sepal.length", "sepal.width", "petal.length", 
                         "petal.width", "class")
str(iris_data)
head(iris_data)
nrows <- nrow(iris_data)
smp_size <- floor(0.75 * nrows)
## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrows), size = smp_size)

train <- iris_data[train_ind, ]
test <- iris_data[-train_ind, ]

iris_nbc <- naive_bayes(class ~ ., data = train)
plot(iris_nbc)

iris_test_pred <- predict(iris_nbc, test)
accuracy <- sum(iris_test_pred==test[,'class'])/length(iris_test_pred)
print(paste("Test accuracy =", round(accuracy*100,2), "%", sep=""))
