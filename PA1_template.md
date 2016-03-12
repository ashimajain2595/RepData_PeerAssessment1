# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

unzip("activity.zip")
data <- read.csv("activity.csv")


## What is mean total number of steps taken per day?
stepsperday <- tapply(data$steps, data$date, sum, na.rm=TRUE)
hist(stepsperday, col="blue", breaks=50, xlab="Total steps per day", main="Histogram of total steps per day")

mean_spd <- mean(stepsperday, na.rm=TRUE)
median_spd <- median(stepsperday, na.rm=TRUE)


## What is the average daily activity pattern?
meandata <- aggregate(data$steps, by=list(data$interval), mean, na.rm=TRUE)
names(meandata) <- c("interval", "steps")
plot(meandata, type="l", col="red", xlab="5-minute interval", ylab="average number of steps taken")

maxsteps <- meandata[which.max(meandata$steps),]


## Imputing missing values
missing_val <- sum(is.na(data$steps))

fill <- function(steps, interval) {
  filled <- NA
  if (!is.na(steps)) 
    filled <- c(steps) 
  else 
    filled <- (meandata[meandata$interval == interval, "steps"])
  return(filled)
}
nomiss_data <- data
nomiss_data$steps <- mapply(fill, nomiss_data$steps, nomiss_data$interval)

totalstepsperday <- tapply(nomiss_data$steps, nomiss_data$date, sum, na.rm=TRUE)
hist(totalstepsperday, col="blue", breaks=50, xlab="Total steps per day", main="Histogram of total steps per day")

mean_tspd <- mean(totalstepsperday)
median_tspd <- median(totalstepsperday)



## Are there differences in activity patterns between weekdays and weekends?
days <- function(date){
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return ("weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return ("weekend")
}
nomiss_data$date <- as.Date(nomiss_data$date)
nomiss_data$day <- sapply(nomiss_data$date, days)

nomiss_mean <- aggregate(steps ~ interval + day, data = nomiss_data, mean)
library(ggplot2)
ggplot(nomiss_mean, aes(interval, steps)) + geom_line(colour="#000099") + facet_grid(day ~ .) + xlab("5-minute interval") + ylab("average number of steps taken")
