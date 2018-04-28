library(httr)

get_data_from_api <- function(base_url, api, city, start_time, end_time, token, output_file, trials=10){
  full_url <- paste(base_url, api, city, start_time, end_time, token, sep="/")
  success <- FALSE
  tried_times <- 0
  while (!success & tried_times < trials){
    response <- GET(full_url)
    if (response$status_code == 200){
      success <- TRUE
      file_conn <- file(output_file)
      response_content <- content(response)
      if (nchar(response_content) <= 100){
        print(paste("API ", full_url, " returns empty. Return as failed.", sep=""))
        return(-1)
      }
      writeLines(response_content, file_conn)
      close(file_conn)
      print(paste("Successfully write data from ", full_url, " to file ", output_file, sep=""))
      return(0)
    } else{
      tried_times <- tried_times + 1
    }
    print(paste("Have tried the API ", full_url, " for ", trials, " times. Still failing. No data is retrieved."))
    return(-1)
  }
}

team_token <- "42e2da041f15c8ff9c6cb4855b1bf01a37d1b7be90f08c1243ebc336cacffd8f"
data_token <- '2k0d1d8' # This is the ID applied to all participants to get the data. Do not change it.
start_time <- '2018-04-28-0'
end_time <- '2018-04-28-23'
base_url <- "https://biendata.com/competition"
aqi_url <- "https://biendata.com/competition/airquality"
met_url <- "https://biendata.com/competition/meteorology"
user_id <- "KJF88"
submission_url <- 'https://biendata.com/competition/kdd_2018_submit/'
###################################################################
# Main chunk of codes to download data from APIs to local csv files
###################################################################
cities = c('bj', 'ld')
api_names <- c("airquality", "meteorology")

for (city in cities){
  for (api in api_names){
    if (api == "meteorology"){
      city_1 <- paste(city, "_grid", sep="")
    } else{
      city_1 <- city
      
    }
    output_file <- paste(city_1, "_", api, "_", start_time, "_", end_time, ".csv", sep="")
    result <- get_data_from_api(base_url, api, city_1, start_time, end_time, data_token, output_file)
    # If it is Beijing, there is a third dataset available
    if (city == 'bj' & api == 'meteorology'){
      output_file <- paste(city, "_", api, "_", start_time, "_", end_time, ".csv", sep="")
      result <- get_data_from_api(base_url, api, city, start_time, end_time, data_token, output_file)
    }
  }
}

###################################################################
# Main chunk of codes to upload data to submission API for evaluation
###################################################################
forecasting_file <- "C:/Users/Kelse/Documents/UW_MachineLearning420/KDDCupTest/sample_submission.csv"
post_result <- POST(submission_url, body=list(files=upload_file(forecasting_file, type="text/csv"), 
                                              user_id=user_id, 
                                              team_token=team_token,
                                              description="sample",
                                              filename="sample_submission.csv"))
if (post_result$status_code == 200){
  print("Forecasts successfully submitted for evaluation")
}
