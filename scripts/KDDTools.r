#This script is just a quick something I threw together as I was
#playing with the visualizations we went over in class.
#Its purpose is mainly to distill the functions introduced by the slides
#and their basic useage. Not meant to cover all or even most use cases
#of the covered functions.

#Randomly chooses a sample of size 'n' from a dataframe 'data'
getSample = function(data, n)
{
  data[sample(nrow(data), n),]
}

#Separate the date colunm from trainTraj into a date and time column
sepTrajDate = function(data)
{
  separate(data, starting_time, c("date", "starting time"), sep=" ")
}

#Example merge, use to merge 2 matrices together by associating
#columns of the same name from each matrix
exMerge = function(data1, data2)
{
  #This will associate the "date" columns in data1 and data2
  #I used the date from trainTraj and trainWeather to merge the
  #matrices together, then visualize the effect of precipitation
  #on travel_time
  merge(data1, data2, by=c("date"))
}

#Generate density plot of chosen variable
densPlot = function(data, v)
{
  ggplot(data) + geom_density(aes(v))
}

#Generate histogram of chosen variable
histPlot = function(data, v)
{
  ggplot(data) + geom_histogram(aes(v))
}

#Generate bar plot of chosen variable
barPlot = function(data, v)
{
  ggplot(data) + geom_bar(aes(v))
}

#Generate boxplot of chosen variables
#Note: factor() converts continuous variables into discrete variables
#This is important b/c variables like vehicle_id are treated as continuous
#by default, but are really discrete.
boxPlot = function(data, x, y)
{
  ggplot(data, mapping = aes(factor(x), y)) + geom_boxplot()
}

#Generate a plot of 2 categorical variables
countPlot = function(data, x, y)
{
  ggplot(data) + geom_count(aes(x, y))
}

#Generate a scatter plot of chosen variables
scatterPlot = function(data, x, y)
{
  ggplot(data, aes(x, y)) + geom_point()
}

#Generate a scatter plot with linear fit of chosen variables
linFit = function(data, x, y)
{
  ggplot(data, aes(x, y)) + geom_point() + stat_smooth(method="lm")
}

plotAvgTravelTime <- function(df)
{
  qplot(time_window_pos, y = avg_travel_time, data = df, color = Route, geom = "smooth", 
        xlab = "Date/Time", ylab = "Avg. Travel Time (seconds)",
        main = "Average Travel Time vs Date based on Route")
}

plotAvgTravelTimeByDay <- function(df) 
{
  qplot(wday, y = avg_travel_time, data = df, color = Route, geom = "jitter", 
        xlab = "Day of Week (starting on Sunday)", ylab = "Avg. Travel Time (seconds)",
        main = "Average Travel Time vs Date based on Route")
}

plotAvgVolume <- function(df)
{
  qplot(time_window_pos, y = volume, data = df, color = Tollgate_And_Direction, geom = "smooth", 
        xlab = "Date/Time", ylab = "Avg. Volume", main = "Average Volume vs Date based on Tollgate Entry/Exit")
}

createTravelTimeModel <- function(df)
{
  model <- lm(avg_travel_time ~ length + yday + wday + time_of_day + 
                Route + wind_speed + 
                rel_humidity + is_holiday + is_working_day, data = df)
  return(model)
}

createTravelVolumeModel <- function(df)
{
  model <- lm(volume ~ tollgate_id + direction + time_of_day + wday + yday + pressure 
              + sea_pressure + temperature + rel_humidity + precipitation + is_holiday
              + is_working_day, data = df)
  return(model)
}