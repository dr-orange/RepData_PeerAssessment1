---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## Loading and preprocessing the data

- Load the data


```r
unzip(zipfile = "activity.zip")
activity <- read.csv("activity.csv", colClasses = c("integer", "Date", "integer"))
```

- Process/transform the data into a format suitable for your analysis

```r
dates <- length(unique(activity$date))
sturgesBreaks <- floor(dates / (1 + log2(dates)))

# use histogram break parameter
print(sturgesBreaks)
```

```
## [1] 8
```

## What is mean total number of steps taken per day?

- Calculate the total number of steps taken per day


```r
totalSteps <- aggregate(steps ~ date, activity, sum)
head(totalSteps)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

- If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
barplot(totalSteps$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
hist(totalSteps$steps, breaks = sturgesBreaks)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-2.png)<!-- -->

- Calculate and report the mean and median of the total number of steps taken per day


```r
mean(totalSteps$steps)
```

```
## [1] 10766.19
```

```r
median(totalSteps$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
- Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
averageSteps <- aggregate(steps ~ interval, activity, mean)
plot(averageSteps$interval, averageSteps$steps, type = "l", xlab = "5-minute interval", ylab = "Average across all days", main = "Daily Activity Pattern")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
averageSteps$interval[which.max(averageSteps$steps)]
```

```
## [1] 835
```

## Imputing missing values

- Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
nrow(activity[is.na(activity$steps),])
```

```
## [1] 2304
```

- Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
mean(averageSteps$steps)
```

```
## [1] 37.3826
```

- Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
interpolatedActivity <- activity
interpolatedActivity[is.na(interpolatedActivity$steps), "steps"] <- mean(averageSteps$steps)
```

- Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
interpolatedTotalSteps <- aggregate(steps ~ date, interpolatedActivity, sum)
hist(interpolatedTotalSteps$steps, breaks = sturgesBreaks)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

## Are there differences in activity patterns between weekdays and weekends?
- Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
weekday <- ifelse(substr(weekdays(interpolatedActivity$date), 1, 1) == "S", "weekend", "weekday")
interpolatedActivity$weekday <- factor(weekday, levels = c("weekday", "weekend"))
```

- Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
library(lattice)
dayAverageSteps = aggregate(steps ~ interval + weekday, interpolatedActivity, mean)
xyplot(steps ~ interval | weekday, data = dayAverageSteps, type = "l", layout = c(1, 2),
       xlab = "Time interval", ylab ="Mean number of steps", main = "Activity Patterns between Weekdays and Weekends")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
