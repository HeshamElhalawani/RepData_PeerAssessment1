Course Project 1
================
Hesham Elhalawani
June 9, 2017

Loading and preprocessing data
==============================

``` r
if(!file.exists('activity.csv')){
    unzip('activity.zip')
}
activityData <- read.csv('activity.csv')
```

Calculating mean total number of steps taken per day
====================================================

``` r
total.steps <- tapply(activityData$steps, activityData$date, FUN=sum, na.rm=TRUE)
hist(total.steps, xlab="total number of steps taken each day")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
mean(total.steps, na.rm=TRUE)
```

    ## [1] 9354.23

``` r
median(total.steps, na.rm=TRUE)
```

    ## [1] 10395

Calculating/Plotting Average daily activity pattern
===================================================

1. Time series plot
-------------------

``` r
library(ggplot2)
averages <- aggregate(x=list(steps=activityData$steps), by=list(interval=activityData$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=averages, aes(x=interval, y=steps)) + geom_line() + xlab("5-minute interval") +
    ylab("average number of steps taken")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-3-1.png)

2. Maximum number of steps
--------------------------

``` r
averages[which.max(averages$steps),]
```

    ##     interval    steps
    ## 104      835 206.1698

Imputing missing values
=======================

1. Calculating and reporting the total number of missing values in the dataset
------------------------------------------------------------------------------

``` r
missing <- is.na(activityData$steps)
table(missing)
```

    ## missing
    ## FALSE  TRUE 
    ## 15264  2304

2. Filling in all of the missing values in the dataset with the mean value of its corresponding 5-minute interval
-----------------------------------------------------------------------------------------------------------------

``` r
fill.value <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps))
        filled <- c(steps)
    else
        filled <- (averages[averages$interval==interval, "steps"])
    return(filled)
}
```

3. Creating a new dataset with missing data filled in
-----------------------------------------------------

``` r
filled.activityData <- activityData
filled.activityData$steps <- mapply(fill.value, filled.activityData$steps, filled.activityData$interval)
```

4. Impact of imputing missing data
----------------------------------

### a. Plotting a histogram of the total number of steps taken each day

``` r
total.steps <- tapply(filled.activityData$steps, filled.activityData$date, FUN=sum)
hist(total.steps, xlab="total number of steps taken each day")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-8-1.png)

### b. Calculating and reporting mean and median

``` r
mean(total.steps)
```

    ## [1] 10766.19

``` r
median(total.steps)
```

    ## [1] 10766.19

### c. Mean and median values differ from the estimates when missing values were not accounted for. Hence, an impact of imputing missing data on the estimates of the total daily number of steps can be concluded.

Estimating the differences in activity patterns between weekdays and weekends
=============================================================================

1. Subset the dataset into 2 levels (weekday vs weekend)
--------------------------------------------------------

``` r
weekday_vs_weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid date")
}
filled.activityData$date <- as.Date(filled.activityData$date)
filled.activityData$day <- sapply(filled.activityData$date, FUN=weekday_vs_weekend)
```

2. Creating time series plot
----------------------------

``` r
averages <- aggregate(steps ~ interval + day, data=filled.activityData, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
    xlab("5-minute interval") + ylab("Number of steps")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-11-1.png)
