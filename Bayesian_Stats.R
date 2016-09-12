#################################################################################################
## OZKAN EMRE OZDEMIR                                                                           #
## HOMEWORK 7 : Perform a linear model on the combined jittered headcount and                   #
## las vegas weather data set. (Lecture 7)                                                      #
## 05/20/16                                                                                     #
## Class:  Methods for Data Analysis                                                            #
##                                                                                              #
#################################################################################################
## Clear objects from Memory :
rm(list=ls())
##Clear Console:
cat("\014")

## Set Working Directory
setwd('~/DataAnalysis/7_TimeSeries_SpatialStats_Bayes')


##-----Load Libraries-----
library(dplyr)
library(data.table)

##-----Load Data-----
headcount = read.csv('JitteredHeadCount.csv', stringsAsFactors = FALSE)
weather = read.csv('las_vegas_hourly_weather.csv', stringsAsFactors = FALSE)


##-----Format Data----
headcount$DateFormat = as.Date(headcount$DateFormat, format="%m/%d/%Y")
names(weather) = c('time','temp','dew_pt','humidity','pressure',
                   'visibility','wind_dir','wind_speed','gust_speed',
                   'precipitation','events','conditions',
                   'wind_dir_deg','date')

weather$datetime = paste(weather$date,weather$time)
weather$datetime = strptime(weather$datetime, format="%Y-%m-%d %I:%M %p")
weather$Hour = as.numeric(format(round(weather$datetime, units="hours"), format="%H"))

##----Drop Duplicates----
weather = weather[!duplicated(weather[c("date", 'Hour')]),]


##----Merge Data-----
weather$DateFormat = weather$date
weather$date = NULL
weather$DateFormat = as.Date(weather$DateFormat, format="%Y-%m-%d")

headcount = merge(headcount, weather, all.x=TRUE, by=c("DateFormat","Hour"))

##----Imputation for NAs in weather-----
numeric_cols = c(11:15, 17:19, 22)
# Linear Interpolation:
headcount[,numeric_cols] = apply(headcount[,numeric_cols], 2, function(x) approx(x, xout=1:length(x), rule=2)$y)

##---Drop character columns----
headcount$wind_dir = NULL
headcount$time = NULL
headcount$datetime = NULL

##-----Deal with events/conditions----
headcount$events[headcount$events == ""] = "None"
headcount$events[is.na(headcount$events)] = "None"
headcount$conditions[is.na(headcount$conditions)] = "None"

##----Format Data for Time Series Exploration-----
headcount$DateFormat = as.Date(headcount$DateFormat, format="%m/%d/%Y")

#head(headcount)

headcount_data = aggregate(HeadCount ~ DateFormat, data = headcount, sum)

# Let's add the total occupided, open and closed tables
TablesOcc= aggregate(TablesOcc ~ DateFormat, data = headcount, sum)
headcount_data$TablesOcc = TablesOcc[,2]
TablesOpen = aggregate(TablesOpen ~ DateFormat, data = headcount, sum)
headcount_data$TablesOpen= TablesOpen[,2]
TablesClosed= aggregate(TablesClosed ~ DateFormat, data = headcount, sum)
headcount_data$TablesClosed= TablesClosed[,2]

# Finally, let's add the rest of the averaged weather data to our aggregated data set

avg_temp = aggregate(temp ~ DateFormat, data = headcount, mean)
headcount_data$temp = avg_temp[,2]
avg_dew_pt = aggregate(dew_pt ~ DateFormat, data = headcount, mean)
headcount_data$dew_pt= avg_dew_pt[,2]
avg_humidity = aggregate(humidity ~ DateFormat, data = headcount, mean)
headcount_data$humidity= avg_humidity[,2]
avg_pressure = aggregate(pressure ~ DateFormat, data = headcount, mean)
headcount_data$pressure = avg_pressure[,2]
avg_visibility= aggregate(visibility ~ DateFormat, data = headcount, mean)
headcount_data$visibility = avg_visibility[,2]
avg_wind_speed = aggregate(wind_speed  ~ DateFormat, data = headcount, mean)
headcount_data$wind_speed = avg_wind_speed[,2]
avg_gust_speed = aggregate(gust_speed  ~ DateFormat, data = headcount, mean)
headcount_data$gust_speed = avg_gust_speed[,2]
avg_precipitation = aggregate(precipitation ~ DateFormat, data = headcount, mean)
headcount_data$precipitation = avg_precipitation[,2]
avg_wind_dir_deg = aggregate(wind_dir_deg ~ DateFormat, data = headcount, mean)
headcount_data$wind_dir_deg = avg_wind_dir_deg[,2]

#head(headcount_data)


# Now Create many different time factors
headcount_data$day_count = as.numeric(headcount_data$DateFormat - min(headcount_data$DateFormat))
headcount_data$week_count = floor(headcount_data$day_count/7.0)
headcount_data$month_count = floor(headcount_data$day_count/30.5)

#head(headcount_data)

# Create linear models:

headcount_day_model = lm(HeadCount ~ . -DateFormat, data = headcount_data)
summary(headcount_day_model)

## Good fit, Adjusted R-squared:  0.978

# Look at plot

plot(headcount_data$day_count, headcount_data$HeadCount, type="l",
     main="Total Head Count Per Day",
     xlab="Day", ylab="Total Head Count", xaxt = "n")
axis(1, xaxp=c(0, 400, 20), las=2)
grid(20)

lines(headcount_data$day_count, headcount_day_model$fitted.values, lwd=2, lty=8, col="red")

# Closer view, Last 65 days:
plot(headcount_data$day_count, headcount_data$HeadCount, type="l",
     main="Total Head Count Per Day",
     xlab="Day", ylab="Total Head Count", xaxt = "n",
     xlim=c(300, 365))
axis(1, xaxp=c(300, 400, 20), las=2)
grid(20)

lines(headcount_data$day_count, headcount_day_model$fitted.values, lwd=2, lty=8, col="red")

## slightly lagging
## Let's check the effect of the total head count 2 days before

headcount_2_days_ago = sapply(1:nrow(headcount_data), function(x){
        if(x <= 2){
                return(headcount_data$HeadCount[1])
        }else{
                return(headcount_data$HeadCount[x-2])
        }
})

headcount_data$headcount_two_days_ago = headcount_2_days_ago 

headcount_2DayTotal = lm(HeadCount ~ . -DateFormat, data = headcount_data)
summary(headcount_2DayTotal)

## Adjusted R-squared:  0.9786. 
## Compared to previous R-squared value (0.978) it is a slight improvement

## Let's check the average temperature effect 2 days before

temp_2_days_ago = sapply(1:nrow(headcount_data), function(x){
        if(x <= 2){
                return(headcount_data$temp[1])
        }else{
                return(headcount_data$temp[x-2])
        }
})

headcount_data$temp_two_days_ago = temp_2_days_ago

headcount_2DayTemp = lm(HeadCount ~ . -DateFormat, data = headcount_data)
summary(headcount_2DayTemp)

## Adjusted R-squared value increased slighlty to  0.9789, seems like we are on the right track. 
## Two days might be too notenought to convinvce people to go to casino, let's try 1 week earlier

temp_1_week_ago = sapply(1:nrow(headcount_data), function(x){
        if(x <= 7){
                return(headcount_data$temp[1])
        }else{
                return(headcount_data$temp[x-7])
        }
})

headcount_data$temp_one_week_ago = temp_1_week_ago


headcount_1WeekTemp = lm(HeadCount ~ . -DateFormat, data = headcount_data)
summary(headcount_1WeekTemp)

##  1 week before temperature value has less importance than 2 days before
##  therefore it didn't improve the Asjusted R-Square value at all
####################################       End     ##############################################