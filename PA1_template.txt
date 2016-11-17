# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(knitr)
library(ggplot2)

activity <- read.csv("./activity.csv", header = TRUE)
```

## What is mean total number of steps taken per day?

```r
steps_day <- activity %>% group_by(date) %>% summarise(steps = sum(steps, na.rm =TRUE))

p <- ggplot(steps_day, aes(steps))
p <- p + geom_histogram(fill = "red")
```

Calculating the mean and the median

```r
mean(steps_day$steps, na.rm = TRUE)
```

```
## [1] 9354.23
```

```r
median(steps_day$steps, na.rm = TRUE)
```

```
## [1] 10395
```

## What is the average daily activity pattern?
steps_interval <- activity %>% group_by(interval) %>% summarise(steps = mean(steps, na.rm =TRUE))

p2 <- ggplot(steps_interval, aes(x = interval, y = steps))
p2 <- p2 + geom_line(colour = "darkgreen")

max_interval <- steps_interval[which.max(steps_interval$steps),]$interval
 
## Imputing missing values
activity_filled <- activity


interval2steps <- function(interval) {
        steps_interval[steps_interval$interval == interval,]$steps
}

for (i in 1:nrow(activity_filled)) {
        if (is.na(activity_filled[i,]$steps)) {
                activity_filled[i,]$steps <- interval2steps(activity_filled[i,]$interval)  
        }
}


steps_filled_day <- activity_filled %>% group_by(date) %>% 
        summarise(steps = sum(steps, na.rm =TRUE))

p3 <- ggplot(steps_filled_day, aes(steps))
p3 <- p3 + geom_histogram(fill = "red")

mean_filled_steps_day <- mean(steps_filled_day$steps, na.rm = TRUE)
median_filled_steps_day <- median(steps_filled_day$steps, na.rm = TRUE)

## Are there differences in activity patterns between weekdays and weekends?
activity_filled$day <- ifelse(wday(as.Date(activity_filled$date)) %in% c(1,7), "weekend","weekday")
activity_filled$day <- factor(activity_filled$day,levels = c("weekday","weekend"))

steps_filled_interval <- activity_filled %>% group_by(interval,day) %>% summarise(steps = mean(steps, na.rm =TRUE))


p4 <- ggplot(steps_filled_interval, aes(x = interval, y = steps))
p4 <- p4 + geom_line(colour = "darkgreen")
p4 + facet_grid(facets = day ~ .)

