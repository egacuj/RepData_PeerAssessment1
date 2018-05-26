library(lattice)
library(dplyr)

## Preparation
# Load data and transform
if(!file.exists("activity.csv")){
    unzip(zipfile = "activity.zip")
}
dataSet <- read.csv("activity.csv")
dataSet$date <- as.Date(dataSet$date, "%Y-%m-%d")

## What is mean total number of steps taken per day?
noStepDay <- dataSet %>% group_by(date) %>% summarise(sum = sum(steps))
hist(noStepDay$sum, 15, col = "grey", main = "Histogram of total number of steps taken per day", xlab = "Total number of steps taken per day")
summary(noStepDay$sum)

## What is the average daily activity pattern?
noStepItv<- dataSet[!is.na(dataSet$steps),] %>% group_by(interval)%>% summarise(meansteps = mean(steps))

with(noStepItv, plot(interval, meansteps, type = "l", main = "Mean steps of 5-min interval") )
points(noStepItv[which.max(noStepItv$meansteps),], pch = 20, col = "red")
text1 <- paste0("Max at interval = ", as.character(noStepItv[which.max(noStepItv$meansteps),"interval"]))
text2 <- paste0("mean = ", as.character(round(noStepItv[which.max(noStepItv$meansteps),"meansteps"])))
text = paste(text1, text2, sep = " ")
legend("topright", pch = 20, legend = text, col = "red")

## Imputing missing values
missingSteps <- which(is.na(dataSet$steps))
length(missingSteps)
dataSetNew <- dataSet
# Strategy: missing value replaced by the mean value of the interval
for (ind in missingSteps){
    dataSetNew[ind, "steps"] <- noStepItv[noStepItv$interval == dataSetNew[ind,"interval"],"meansteps"]
}

noStepDayNew <- dataSetNew %>% group_by(date) %>% summarise(sum = sum(steps))
hist(noStepDayNew$sum, 15, col = "grey", main = "Histogram of total number of steps taken per day", xlab = "Total number of steps taken per day (NA ommitted)")
summary(noStepDayNew$sum)

## Are there differences in activity patterns between weekdays and weekends?

dataSetNew <- mutate(dataSetNew, wday = ifelse(as.POSIXlt(dataSetNew$date)$wday ==0 |as.POSIXlt(dataSetNew$date)$wday ==6, "weekend", "weekday"))
noStepItvWd<- dataSetNew %>% group_by(wday, interval)%>% summarise(meansteps = mean(steps))
xyplot(meansteps ~ interval | wday, data = noStepItvWd, layout = c(1, 2), type = "l", xlab = "Interval", ylab = "Number of steps")