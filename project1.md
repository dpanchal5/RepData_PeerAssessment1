---
  title: "Reproducible Research--Project1"
author: "Divya Panchal"
date: "September 14, 2016"
output: html_document
---
  
  
  ##Loading and preprocessing the data
  
  Set the working directory
```{r}
setwd("D:/DivyaDataScience/ReproducibleResearch/Project/repdata_data_activity")
```
Read the file
```{r}
activitydata<-read.csv("activity.csv")
head(activitydata)
```

##What is mean total number of steps taken per day?

Calculate the number of steps taken
```{r}
total<-aggregate(steps ~ date, data=activitydata, FUN = sum, na.rm = TRUE)
head(total)
```

Plotting histogram
```{r}
hist(total$steps, main = "Total steps per day", xlab = "Steps", col ="blue")
```

Calculating the mean and median of the total number of steps taken per day
```{r}
meanvalue<-mean(total$steps)
meanvalue
medianvalue<-median(total$steps)
medianvalue
```
##What is the average daily activity pattern?

```{r}
timeseries<-aggregate(steps ~ interval, data = activitydata, FUN = mean, na.rm = TRUE)
names(timeseries)<-c("Interval", "Mean")
head(timeseries)
```
Plotting
```{r}
plot(timeseries$Interval, timeseries$Mean, type = "l", xlab = "5-min Interval", ylab = "Average for all days", main = "Time Series Plot", col="red")
maxposition<-which.max(timeseries$Mean)
maxpositionrow<-timeseries[maxposition,]
maxpositionrow$Interval
```
##Imputing missing values

Calculate and report the number of missing values
```{r}
totalmissingvalue<-sum(is.na(activitydata))
totalmissingvalue
```
Creating a copy of dataset and assign 50 to NA
```{r}
copyactivitydata<-activitydata
copyactivitydata[is.na(copyactivitydata)]<-50
head(copyactivitydata)

newtotal<-aggregate(steps ~ copyactivitydata$date, data=copyactivitydata, FUN = sum)
hist(newtotal$steps, main = "Total steps per day with NA replaced with 50", xlab = "Steps", col ="blue")
newmeanvalue<-mean(newtotal$steps)
newmeanvalue
newmedianvalue<-median(newtotal$steps)
newmedianvalue
summary(newtotal$steps)
```

##Are there differences in activity patterns between weekdays and weekends?

```{r}
day <- weekdays(as.Date(activitydata$date))
daylevel <- vector()
for (i in 1:nrow(activitydata)) {
  if (day[i] == "Saturday") {
    daylevel[i] <- "Weekend"
  } else if (day[i] == "Sunday") {
    daylevel[i] <- "Weekend"
  } else {
    daylevel[i] <- "Weekday"
  }
}
activitydata$daylevel <- daylevel
activitydata$daylevel <- factor(activitydata$daylevel)
stepsByDay <- aggregate(steps ~ interval + daylevel, data = activitydata, mean)
names(stepsByDay) <- c("interval", "daylevel", "steps")
library(lattice)
xyplot(steps ~ interval | daylevel, stepsByDay, type = "l", layout = c(1, 2), 
       xlab = "Interval", ylab = "Number of steps")

```


