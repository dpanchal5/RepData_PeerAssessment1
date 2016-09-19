#LOADING AND PREPROCESSING THE DATA
#Show any code that is needed to
#Load the data (i.e. read.csv())
#Process/transform the data (if necessary) into a format suitable for your analysis

#Set the working directory
setwd("D:/DivyaDataScience/ReproducibleResearch/Project/repdata_data_activity")

#Read the file
activitydata<-read.csv("activity.csv")
head(activitydata)

#WHAT IS MEAN TOTAL NUMBER OF STEPS TAKEN PER DAY?
#For this part of the assignment, you can ignore the missing values in the dataset.
#Calculate the total number of steps taken per day
#If you do not understand the difference between a histogram and a barplot, 
#research the difference between them. Make a histogram of the total number of steps taken each day
#Calculate and report the mean and median of the total number of steps taken per day

#Calculate the number of steps taken
total<-aggregate(steps ~ date, data=activitydata, FUN = sum, na.rm = TRUE)
head(total)

#Plotting histogram
hist(total$steps, main = "Total steps per day", xlab = "Steps", col ="blue")

#Calculating the mean and median of the total number of steps taken per day
meanvalue<-mean(total$steps)
meanvalue
medianvalue<-median(total$steps)
medianvalue

#WHAT IS THE AVERAGE DAILY ACTIVITY PATTERN?
#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
#and the average number of steps taken, averaged across all days (y-axis)
#Which 5-minute interval, on average across all the days in the dataset, 
#contains the maximum number of steps?
timeseries<-aggregate(steps ~ interval, data = activitydata, FUN = mean, na.rm = TRUE)
names(timeseries)<-c("Interval", "Mean")
head(timeseries)

#Plotting
plot(timeseries$Interval, timeseries$Mean, type = "l", xlab = "5-min Interval", ylab = "Average for all days", main = "Time Series Plot", col="red")
maxposition<-which.max(timeseries$Mean)
maxpositionrow<-timeseries[maxposition,]
maxpositionrow$Interval

#IMPUTING MISSING VALUES
#Note that there are a number of days/intervals where there are missing values (coded as NA).
#The presence of missing days may introduce bias into some calculations or summaries of the data.
#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
#Devise a strategy for filling in all of the missing values in the dataset. 
#The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, 
#or the mean for that 5-minute interval, etc.
#Create a new dataset that is equal to the original dataset but with the missing data filled in.
#Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total 
#number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? 
#What is the impact of imputing missing data on the estimates of the total daily number of steps?

#Calculate and report the number of missing values
totalmissingvalue<-sum(is.na(activitydata))
totalmissingvalue

#Creating a copy of dataset and assign 50 to NA
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


#ARE THERE DIFFERENCES IN ACTIVITY PATTERNS BETWEEN WEEKDAYS AND WEEKENDS?
#For this part the weekdays() function may be of some help here. 
#Use the dataset with the filled-in missing values for this part.
#Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether 
#a given date is a weekday or weekend day.
#Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the 
#average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file 
#in the GitHub repository to see an example of what this plot should look like using simulated data.

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
