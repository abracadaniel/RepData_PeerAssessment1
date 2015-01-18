# Reproducible Research: Peer Assessment 1

## Load required modules

```r
library(ggplot2)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
Sys.setlocale("LC_TIME", "en_US")
```

```
## [1] "en_US"
```

## Loading and preprocessing the data

```r
if(!file.exists("activity.csv"))
    unzip("activity.zip")
activityData <- read.csv("activity.csv")
activityData$data <- as.Date(activityData$date)
activityData.complete <- na.omit(activityData)
```

## What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day

```r
stepsByDate <- aggregate(steps ~ date, data = activityData.complete, FUN = sum)
ggplot(stepsByDate, aes(x = steps)) + geom_histogram(binwidth = 1000, colour = "black", fill = "white") + labs(x = "Steps in thousands", y = "Number of days")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

2. Calculate and report the mean and median total number of steps taken per day

```r
mean(stepsByDate$steps)
```

```
## [1] 10766.19
```

```r
median(stepsByDate$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
stepsByInterval <- aggregate(steps ~ interval, data = activityData.complete, FUN = mean)
ggplot(stepsByInterval, aes(x = interval, y = steps)) + geom_line() + labs(x = "Interval", y = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
stepsByInterval$interval[which.max(stepsByInterval$steps)]
```

```
## [1] 835
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
length(which(is.na(activityData)))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
I am going to impute these missing values using average steps by interval.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activityData.new <- left_join(activityData, stepsByInterval, by = "interval")
activityData.new <- mutate(activityData.new, steps = ifelse(is.na(steps.x), steps.y, steps.x))
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
stepsByDate.new <- aggregate(steps ~ date, data = activityData.new, FUN = sum)
ggplot(stepsByDate.new, aes(x = steps)) + geom_histogram(binwidth = 1000, colour = "black", fill = "white") + labs(x = "Steps in thousands", y = "Number of days")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 


```r
mean(stepsByDate.new$steps)
```

```
## [1] 10766.19
```


```r
median(stepsByDate.new$steps)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
activityData.new <- mutate(activityData.new, day = factor(ifelse(weekdays(as.Date(activityData.new$date)) %in% c("Saturday", "Sunday"), "weekend", "weekday")))
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
stepsByInterval.new <- aggregate(steps ~ interval + day, data = activityData.new, FUN = mean)
ggplot(stepsByInterval.new, aes(x = interval, y = steps)) + facet_grid(day ~ .) + geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png) 
