# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
1. Load the data 

```r
readfile <- read.csv("activity.csv", na.strings="NA")
```
Review the data file

```r
head(readfile,5)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
```

```r
str(readfile)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
2. Remove "NA" values

```r
rd2 <- na.omit(readfile)
```

## What is mean total number of steps taken per day?

1. Sum the total number of steps taken per day (group by date)

```r
library(dplyr)
```

```r
sumDt <- rd2 %>% group_by(date) %>% summarise_each(funs(sum))
```

##### 2. Plot a histogram of the total number of steps taken each day

```r
hist(sumDt$steps, xlab="Total Steps", main="Total steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

##### 3. Mean and Median of the total number of steps taken per day

```r
mean(sumDt$steps)
```

```
## [1] 10766.19
```

```r
median(sumDt$steps)
```

```
## [1] 10765
```

  
## What is the average daily activity pattern?

Sum the total number of inteval taken per day (group by interval)

```r
avgInt <- rd2 %>% group_by(interval) %>% summarise_each(funs(mean))
```
##### 1. Time series plot of the interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
 plot(avgInt$interval, avgInt$steps, type = 'l', xlab="Time interval", ylab="Average steps across all days")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

##### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps? 
Answer: The interval 835, has the maximum number of steps which is 206 (or 206.17).

```r
avgInt[which.max(avgInt$steps), c(1,2)]
```

```
## Source: local data frame [1 x 2]
## 
##   interval    steps
## 1      835 206.1698
```

## Imputing missing values
##### 1. Calculate total number of missing values in the dataset (rows with NAs)

```r
sum(is.na(readfile$steps))
```

```
## [1] 2304
```

##### 2. Devise a strategy for filling in all of the missing values in the dataset. 
Strategy: use the mean for that 5-minute interval. This has been already calculated on the section above (avgInt) 


##### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
mdata <- merge(readfile, avgInt, by = "interval", suffixes = c("", ".y"))

NAcount <- is.na(mdata$steps)
mdata$steps[NAcount] <- mdata$steps.y[NAcount]
mdata <- mdata[, c(2,3,1)]
```
Preview the data

```r
head(arrange(mdata, date),5)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
```

##### 4. Plot a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 

```r
sumDt2<- mdata %>% group_by(date) %>% summarise_each(funs(sum))
```

Plot a histogram of the total number of steps taken each day

```r
hist(sumDt2$steps, xlab="Total Steps", main="Total steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png) 

Mean and Median of the total number of steps taken per day

```r
mean(sumDt2$steps)
```

```
## [1] 10766.19
```

```r
median(sumDt2$steps)
```

```
## [1] 10766.19
```
```
The values are almost the same as the first part of the assignment. The median of this part is slightly higher due to decimals. 
Thus, the impact of imputing missing data on the estimates is very minimum. 
```

## Are there differences in activity patterns between weekdays and weekends?

##### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
weekday <- weekdays(as.Date(mdata$date))
dweeks <- transform(mdata, day = weekday)

dweeks$daytype <- ifelse(dweeks$day %in% c("Saturday", "Sunday"), "weekend", "weekday")
```

##### 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

```r
library(lattice)
rdInterval <- dweeks %>% group_by(interval,daytype) %>% summarise_each(funs(mean))
xyplot( steps ~ interval | daytype, data=rdInterval, layout=c(1,2), type = 'l', ylab="Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-18-1.png) 

