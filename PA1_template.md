---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
data <- read.csv(unzip('activity.zip'))
```

## What is mean total number of steps taken per day?

```r
# Ignore missing values
data_NA_rm <- data[which(!is.na(data$steps)),]

# Calculate the total number of steps taken per day
data$date <- as.character(data$date)
steps_per_day <- data.frame(date=unique(data$date),steps=NA)
for (day in 1:nrow(steps_per_day)){
  steps_per_day[day,'steps'] <- sum(data_NA_rm[which(data_NA_rm$date == steps_per_day[day,'date']),'steps'])
}

# Histogram of the total number of steps taken each day
hist(steps_per_day$steps,breaks = seq(0,25000,1000))
```

![plot of chunk mean_total_number_of_steps](figure/mean_total_number_of_steps-1.png) 

```r
#Calculate the mean and median of the total number of steps taken per day
steps_mean <- prettyNum(round(mean(steps_per_day$steps)))
steps_median <- prettyNum(round(median(steps_per_day$steps)))
```

The mean of the total number of steps taken per day is 9354, the median is 10395.

## What is the average daily activity pattern?

```r
# Calculate average number of steps taken in each 5-minute interval, averaged across all days
data$interval <- as.character(data$interval)
steps_per_interval <- data.frame(steps=NA,interval=unique(data$interval))
steps_per_interval$interval <- as.character(steps_per_interval$interval)
for (interval_i in 1:nrow(steps_per_interval)){
  steps_per_interval[interval_i,'steps'] <- 
    mean(data_NA_rm[which(data_NA_rm$interval == steps_per_interval[interval_i,'interval']),'steps'])
}

# Time series plot of the interval and the average number of steps taken, averaged across all days
plot(x=steps_per_interval$interval,y=steps_per_interval$steps,type='l', 
     main="Average steps per interval",xlab='interval', ylab='steps')
```

![plot of chunk average_daily_activity_pattern](figure/average_daily_activity_pattern-1.png) 

```r
# Calculate interval, that contains the maximum number of steps (on average across all the days)
max_interval <- steps_per_interval$interval[steps_per_interval$steps == max(steps_per_interval$steps)]
```

The 5-minute interval, which on average across all the days contains the maximum number of steps is 835.


## Imputing missing values

```r
# Calculate and report the total number of missing values in the dataset
no_NA <- sum(is.na(data$steps))

# Create a new dataset that is equal to the original dataset but with the missing data filled in by adding the mean for that 5-minute interval.
data_filled <- data
for (i in 1: nrow(data_filled)){
  if(is.na(data_filled[i,'steps'])){
    data_filled[i,'steps'] <- steps_per_interval$steps[steps_per_interval$interval == data_filled[i,'interval']]
  }
}

# Calculate average number of steps taken in each 5-minute interval, averaged across all days
steps_per_day_filled <- data.frame(date=unique(data_filled$date),steps=NA)
for (day in 1:nrow(steps_per_day_filled)){
  steps_per_day_filled[day,'steps'] <- 
    sum(data_filled[which(data_filled$date == steps_per_day_filled[day,'date']),'steps'])
}

#Make a histogram of the total number of steps taken each day. 
hist(steps_per_day_filled$steps,breaks = seq(0,25000,1000))
```

![plot of chunk missing_values](figure/missing_values-1.png) 

```r
#Calculate the mean and median of the total number of steps taken per day
steps_mean_filled <- prettyNum(round(mean(steps_per_day_filled$steps)))
steps_median_filled <- prettyNum(round(median(steps_per_day_filled$steps)))
diff_means <- as.numeric(steps_mean_filled) - as.numeric(steps_mean)
diff_medians <- as.numeric(steps_median_filled) - as.numeric(steps_median)
```

The dataset contains 2304 missing values. The mean of the total number of steps taken per day is 10766, the median is 10766. The filled mean and median of total number of steps taken per day are higher than the original ones. The difference between the means is 1412, the difference between the medians is 371.

## Are there differences in activity patterns between weekdays and weekends?

```r
#Create a new factor variable in the dataset with two levels – “weekday” and “weekend” 
data_filled$day <- weekdays(as.Date(data$date))
data_filled$day_type <- 'weekday'
data_filled$day_type[data_filled$day=='Samstag'] <- 'weekend'
data_filled$day_type[data_filled$day=='Sonntag'] <- 'weekend'
data_filled$day_type <- as.factor(data_filled$day_type)

#Make a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days.
steps_per_interval_filled <- data.frame(steps_weekend=NA,steps_weekday=NA,interval=unique(data$interval))
steps_per_interval_filled$interval <- as.character(steps_per_interval_filled$interval)
for (interval_i in 1:nrow(steps_per_interval_filled)){   
  steps_per_interval_filled[interval_i,'steps_weekend'] <- 
    mean(data_filled[which(data_filled$interval == steps_per_interval_filled[interval_i,'interval'] &
                             data_filled$day_type == 'weekend'),'steps'])
  steps_per_interval_filled[interval_i,'steps_weekday'] <- 
    mean(data_filled[which(data_filled$interval == steps_per_interval_filled[interval_i,'interval'] &
                             data_filled$day_type == 'weekday'),'steps'])
}
# The assignment said, "feel free to use any plotting system in R", so I keep using the base system and create the panel equivalent
par(mfrow=c(2,1))
plot(x=steps_per_interval_filled$interval,y=steps_per_interval_filled$steps_weekday,type='l', 
     main="Average steps per interval (weekdays)",xlab='interval', ylab='steps')
plot(x=steps_per_interval_filled$interval,y=steps_per_interval_filled$steps_weekend,type='l', 
     main="Average steps per interval (weekend)",xlab='interval', ylab='steps')
```

![plot of chunk differences_weekdays_weekends](figure/differences_weekdays_weekends-1.png) 

