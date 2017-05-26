---
title: "PA1_template"
author: "Takunda"
date: "26 May 2017"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


```{r}
#Read in the activity data
activityData <- read.csv("activity.csv")
```
#What is mean total number of steps taken per day?

  For this part of the assignment, you can ignore the missing values in the dataset.

  Make a histogram of the total number of steps taken each day

  Calculate and report the mean and median total number of steps taken per day
  
```{r}
#First aggregate the number of steps by day and then plot these totals on a histogram.
total_daily_steps <- aggregate(steps~date,activityData,sum)
hist(total_daily_steps$steps,main = "Mean Total Number of Steps per Day",col = "red", xlab = "Number of Steps")
```


```{r}
#Calculate the mean and median number of steps.
meanSteps <- mean(total_daily_steps$steps)
medianSteps <- median(total_daily_steps$steps)
```

The mean number of steps is `r meanSteps` and the median number of steps is `r medianSteps`

#What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
#Use the aggregate function to compute the mean number of steps per interval taken across all days and plot the result.
interval_mean_steps <- aggregate(steps~interval,activityData,mean)
plot(interval_mean_steps$interval,interval_mean_steps$steps,type = "l",xlab = "interval",ylab = "Mean Steps",main = "Average Number of Steps per Interval")
```

```{r}
#Fetch the interval with the highest mean number of steps.
maxStep_interval <- interval_mean_steps[which.max(interval_mean_steps$steps),1]
```
The 5-minute interval, on average across all the days, containing the maximum number of steps is `r maxStep_interval`

#Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Create a new dataset that is equal to the original dataset but with the missing data filled in.

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r}
#Count the number of cases with incomplete step data.
missingValues <- sum(!complete.cases(activityData))
```
The number of missing values is `r missingValues`

The strategy to be used for replacing missing values will be that of replacing all NA values by the mean for that 5-minute interval
```{r}
#Create a new dataset by replacing NA step values with mean values of the respective intervals.
newActivityData <- transform(activityData,steps = ifelse(is.na(activityData$steps),interval_mean_steps$steps[match(activityData$interval,interval_mean_steps$interval)],activityData$steps))

#Re-compute the total number of steps for the transformed data.
new_total_daily_steps <- aggregate(steps~date,newActivityData,sum)

#Compare the total number of steps taken each day for the original data and for the transoformed data.Plot the corresponding histograms on one chart.
hist(new_total_daily_steps$steps,main = "Total Number of Steps per Day", xlab = "Number of Steps",col = "green")
hist(total_daily_steps$steps,main = "Total Number of Steps per Day", xlab = "Number of Steps", col = "blue", add = T)
legend("topright",c("NA values replaced","with NA values"), col = c("green","blue"),lwd = 10)

#compute the new Mean and Median number steps together with differences from the original values.
new_meanSteps <- mean(new_total_daily_steps$steps)
new_medianSteps <- median(new_total_daily_steps$steps)
meanDifference <- new_meanSteps - meanSteps
medianDifference <- new_medianSteps - medianSteps
```
The new mean number of steps is `r new_meanSteps`and the new median number of steps is `r new_medianSteps` showing that imputing new data had no impact on the mean number of steps.
The difference between the 2 means is `r meanDifference`and the diefference between the 2 medians is `r medianDifference` which is only a marginal increase.
Imputing missing values on the total number of daily steps data will increase the total daily number of steps.

#Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```{r}
#First add a new variable for the day of the week.
newActivityData$day <- weekdays(as.Date(newActivityData$date))

#Add a new factor variable with the 2 levels, "weekday" and "weekend".
newActivityData$DayCategory <- ifelse(newActivityData$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

#import the 2 libraries, lattice (which is necessary for the xyplot to follow) and plyr (which is necessary when the function ddply is called to summarise data by interval and Day Category)
library(lattice)
library(plyr)

#Summarise data by interval and DayCategory
summary_Data <- ddply(newActivityData,.(interval,DayCategory),summarize,Avg = mean(steps))

#Plot the data for the 2 different day categories
xyplot(Avg~interval|DayCategory,data = summary_Data, type = "l",layout = c(1,2),main = "Average Steps per Interval by Day Category", ylab = "Average Steps", xlab = "Interval")
```

Yes there are differences in activity patterns between weekdays and weekends. The average number of steps for weekdays is more evenly spread out across all intervals during weekends. For the weekdays the average number of steps tends to have a high peak around interval 900 and lowers significantly for the other intervals.  