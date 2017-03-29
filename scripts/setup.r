
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
  df$Route <- as.factor(paste("I", as.character(df$intersection_id), " -> T", as.character(df$tollgate_id), sep = ""))
  return(df)
}

setupVolume20Avg <- function(df)
{
  df$time_window_pos <- strptime(str_sub(df$time_window, start = 2, end = 20), format = "%Y-%m-%d %H:%M:%S")
  df$time_window_num <- as.numeric(df$time_window_pos)
  df$exiting <- ifelse(df$direction == 1, "exit", "entry")
  df$Tollgate_And_Direction <- as.factor(paste("T", as.character(df$tollgate_id), " - ", df$exiting, sep = ""))
  return(df)
}