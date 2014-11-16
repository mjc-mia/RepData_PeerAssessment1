Reproducible Results - Peer Assessment 1
========================================

## Loading and Pre-processing Data
If this is the first time the program is run, create directory for data files, download file, and unzip it.  The data frame is recreated each time the program is run.

Note that the download is not necessary as the file is included in the Github repository--I just prefer this version that I can run at any time and places data files in a separate directory.

Be careful with this code--older versions of RStudio give errors on the download.


```r
if (!file.exists("data")) {
        dir.create("data")
        url = "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
        download.file(url, destfile="./data/activity.zip")
        unzip("./data/activity.zip", exdir="./data")
        list.files("./data")

}
activity = read.csv("./data/activity.csv")
```

## What is mean total number of steps taken per day?


```r
library(data.table)
dt = data.table(activity)
daily = dt[,list(total_steps=sum(steps, na.rm=T)),by=date]
hist(daily$total_steps, main="Total Steps per Day", xlab="Steps")
```

![plot of chunk dailyAverage](figure/dailyAverage-1.png) 

```r
mean=mean(daily$total_steps)
median=median(daily$total_steps)
```

The mean number of steps is 9354.2295082 and the median is 10395.

## What is the average daily activity pattern?

Compute average steps by 5-minute interval.


```r
library(lattice)
period = dt[,list(avg_steps=mean(steps, na.rm=T)), by=interval]
xyplot(avg_steps~interval, data=period, type="l", ylab="Average Steps")
```

![plot of chunk analysisByPeriod](figure/analysisByPeriod-1.png) 

```r
max_steps = max(period$avg_steps)
max_row = which.max(period$avg_steps)
```
The maximum average steps per interval is 206.1698113, found at row 104.

## Imputing missing values


```r
na_count =  sum(is.na(activity$steps))
```

We can determine how many rows have missing values with the "is.na" function. Note that the instructions were a little fuzzy on this one.  The code below counts the missing steps field.  To get all NAs, including date and interval fields (if any), we could use "sum(is.na(activity))".


Before replacement, the data frame has 2304 NAs.

Next, we create a new data frame and replace NAs with the average steps for all periods.


```r
replaced = activity
replaced[is.na(replaced[, "steps"]), "steps"] <- mean(replaced[, "steps"],  na.rm = TRUE)
na_count = sum(is.na(replaced$steps))
```
After replacement, the new data frame has 0 NAs.  We can see how the replacement affected the distribution below:


```r
dtR = data.table(replaced)
dailyR = dtR[,list(total_steps=sum(steps, na.rm=T)),by=date]
hist(dailyR$total_steps, main="Total Steps per Day After Replacement", xlab="Steps")
```

![plot of chunk analysisAfterReplacement](figure/analysisAfterReplacement-1.png) 

```r
meanR=mean(dailyR$total_steps)
medianR=median(daily$total_steps)
meanV = meanR - mean
medianV = medianR - median
```

The new mean number of steps is 10766.19, a change of 1411.959; the new median is 10395, a change of 0.

## Are there differences in activity patterns between weekdays and weekends?

First, add a column to the data table to indicate whether the date in in the weekend or not, then plot each series:


```r
dtR$day = as.factor(ifelse( weekdays(as.Date(dtR$date)) %in% c('Saturday','Sunday'), "weekend", "weekday" ))
period = dtR[,list(avg_steps=mean(steps, na.rm=T)), by=c("day","interval")]
xyplot(avg_steps~interval | day, data=period, type="l", main="Weekend vs Weekday Activity", xlab="Interval", ylab="Average Steps", layout=c(1,2))
```

![plot of chunk analysisByWeekend](figure/analysisByWeekend-1.png) 
