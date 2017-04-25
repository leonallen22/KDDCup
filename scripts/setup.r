
#Load any libraries used in the scripts I wrote
setup = function()
{
  library("ggplot2")
  library("stringr")
  library("dplyr")
  require("tidyr")
}

#Load the data files provided by the KDD hosts into dataframes
#pathToData: path to the directory containing the "testing_phase1" and "training" directories
readData = function(pathToData)
{
  trainLinks <<- read.csv(paste(pathToData,"/training/links (table 3).csv", sep=""))
  trainRoutes <<- read.csv(paste(pathToData,"/training/routes (table 4).csv", sep=""))
  trainTraj <<- read.csv(paste(pathToData,"/training/trajectories(table 5)_training.csv", sep=""))
  trainVolume <<- read.csv(paste(pathToData,"/training/volume(table 6)_training.csv", sep=""))
  trainWeather <<- read.csv(paste(pathToData,"/training/weather (table 7)_training.csv", sep=""))
  train20Travel <<- read.csv(paste(pathToData, "/training/training_20min_avg_travel_time.csv", sep=""))
  train20Volume <<- read.csv(paste(pathToData, "/training/training_20min_avg_volume.csv", sep=""))
  testTraj <<- read.csv(paste(pathToData,"/testing_phase1/trajectories(table 5)_test1.csv", sep=""))
  testVolume <<- read.csv(paste(pathToData,"/testing_phase1/volume(table 6)_test1.csv", sep=""))
  testWeather <<- read.csv(paste(pathToData,"/testing_phase1/weather (table 7)_test1.csv", sep=""))
  test20Travel <<- read.csv(paste(pathToData, "/testing_phase1/test1_20min_avg_travel_time.csv", sep=""))
  test20Volume <<- read.csv(paste(pathToData, "/testing_phase1/test1_20min_avg_volume.csv", sep=""))
}

setupTravelTime20Avg <- function(df)
{
  df$time_window_pos <- strptime(str_sub(df$time_window, start = 2, end = 20), format = "%Y-%m-%d %H:%M:%S")
  df$time_window_num <- as.numeric(df$time_window_pos)
  df$time_of_day <- (df$time_window_pos$hour * 60) + df$time_window_pos$min
  df$Route <- as.factor(paste("I", as.character(df$intersection_id), " -> T", as.character(df$tollgate_id), sep = ""))
  df <- merge(df, trainRoutes, by = c("intersection_id", "tollgate_id"))
  df <- within(df, rm(link_seq, time_window))
  df$wday <- df$time_window_pos$wday
  df$yday <- df$time_window_pos$yday
  df$time_of_week <- df$time_of_day + (df$wday * 1440)
  df$is_holiday <- ifelse(df$time_window_pos >= "2016-09-15" & df$time_window_pos < "2016-09-18", 1, 
                          ifelse(df$time_window_pos >= "2016-10-1" & df$time_window_pos < "2016-10-8", 1, 0))
  df$is_working_day <- ifelse(df$wday > 0 & df$wday < 6 & df$is_holiday == 0, 1, 0)
  df$is_working_day <- ifelse(df$time_window_pos >= "2016-09-18" & df$time_window_pos < "2016-09-19", 1, df$is_working_day)
  df$is_working_day <- ifelse(df$time_window_pos >= "2016-10-8" & df$time_window_pos < "2016-10-10", 1, df$is_working_day)
  df$tollgate_id <- as.factor(df$tollgate_id)
  df$wday <- as.factor(df$wday)
  return(df)
}

setupVolume20Avg <- function(df)
{
  df$time_window_pos <- strptime(str_sub(df$time_window, start = 2, end = 20), format = "%Y-%m-%d %H:%M:%S")
  df$time_window_num <- as.numeric(df$time_window_pos)
  df$time_of_day <- (df$time_window_pos$hour * 60) + df$time_window_pos$min
  df$exiting <- ifelse(df$direction == 1, "exit", "entry")
  df$Tollgate_And_Direction <- as.factor(paste("T", as.character(df$tollgate_id), " - ", df$exiting, sep = ""))
  df$wday <- df$time_window_pos$wday
  df$yday <- df$time_window_pos$yday
  df$time_of_week <- df$time_of_day + (df$wday * 1440)
  df$tollgate_id <- as.factor(df$tollgate_id)
  df$is_holiday <- ifelse(df$time_window_pos >= "2016-09-15" & df$time_window_pos < "2016-09-18", 1, 
                          ifelse(df$time_window_pos >= "2016-10-1" & df$time_window_pos < "2016-10-8", 1, 0))
  df$is_working_day <- ifelse(df$wday > 0 & df$wday < 6 & df$is_holiday == 0, 1, 0)
  df$is_working_day <- ifelse(df$time_window_pos >= "2016-09-18" & df$time_window_pos < "2016-09-19", 1, df$is_working_day)
  df$is_working_day <- ifelse(df$time_window_pos >= "2016-10-8" & df$time_window_pos < "2016-10-10", 1, df$is_working_day)
  df$wday <- as.factor(df$wday)
  return(df)
}

setupWeatherTimeWindows <- function(df)
{
  df$time_window_pos <- as.POSIXlt(paste(as.character(df$date), as.character(df$hour)), format = "%Y-%m-%d %H")
  n <- nrow(df)
  twenty_min <- 1200
  for (i in 1:n) {
    curr_row <- df[i,]
    for (t in 1:8) {
      curr_row$time_window_pos <- curr_row$time_window_pos + twenty_min
      df <- rbind(df, curr_row)
    }
  }
  df$time_window_num <- as.numeric(df$time_window_pos)
  return(within(df, rm(time_window_pos)))
}

mergeWeather <- function(train, weather) 
{
  return(merge(train, weather, by = c("time_window_num")))
}