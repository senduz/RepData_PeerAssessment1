---
title: "Reproducible Research: Peer Assessment 1"
author: "Senduz"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data

```r
#load packages:
library(ggplot2)
library(ggthemes)
activity <- read.csv("activity.csv")
activity$date <- as.POSIXct(activity$date, "%Y%m%d")
day <- weekdays(activity$date)
activity <- cbind(activity, day)
#looking at processed data
summary(activity)
```

```
##      steps             date               interval          day           
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0   Length:17568      
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8   Class :character  
##  Median :  0.00   Median :2012-10-31   Median :1177.5   Mode  :character  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5                     
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2                     
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0                     
##  NA's   :2304
```
## What is mean total number of steps taken per day?

```r
activityTotalSteps <- with(activity, aggregate(steps, by = list(date), sum, na.rm = TRUE))
names(activityTotalSteps) <- c("Date", "Steps")
totalStepsdf <- data.frame(activityTotalSteps)
# Plot the histogram using ggplot2
g <- ggplot(totalStepsdf, aes(x = Steps)) + 
  geom_histogram(breaks = seq(0, 25000, by = 2500), fill = "#909800", col = "black") + 
  ylim(0, 30) + 
  xlab("Total Steps Taken Per Day") + 
  ylab("Frequency") + 
  ggtitle("Total Number of Steps Taken on a Day") + 
  theme_calc(base_family = "sans")
print(g)
```

![](figures meanStepsPerDay-1.png)<!-- -->

```r
mean(activityTotalSteps$Steps)
```

```
## [1] 9354.23
```

```r
median(activityTotalSteps$Steps)
```

```
## [1] 10395
```
## What is the average daily activity pattern?

```r
# Calculating the average number of steps taken, 
#averaged across all days by 5-min intervals.
averageDailyActivity <- aggregate(activity$steps, by = list(activity$interval), 
FUN = mean, na.rm = TRUE)
names(averageDailyActivity) <- c("Interval", "Mean")
averageActivitydf <- data.frame(averageDailyActivity)
# Plot using ggplot2
da <- ggplot(averageActivitydf, mapping = aes(Interval, Mean)) + 
  geom_line(col = "#E01A86") +
  xlab("Interval") + 
  ylab("Average Number of Steps") + 
  ggtitle("Average Number of Steps Per Interval") +
  theme_calc(base_family = "sans")
print(da)
```

![](figures AvgDailyActivity-1.png)<!-- -->

```r
#Which 5-minute interval, on average across all the days in the dataset, 
#contains the maximum number of steps?
averageDailyActivity[which.max(averageDailyActivity$Mean), ]$Interval
```

```
## [1] 835
```
## Imputing missing values

```r
# report the total number of rows with NAs.
sum(is.na(activity$steps))
```

```
## [1] 2304
```

```r
imputedSteps <- averageDailyActivity$Mean[match(activity$interval, averageDailyActivity$Interval)]
activityImputed <- transform(activity, steps = ifelse(is.na(activity$steps), 
yes = imputedSteps, no = activity$steps))
# Creating new dataset having imputed missing values.
totalActivityImputed <- aggregate(steps ~ date, activityImputed, sum)
names(totalActivityImputed) <- c("date", "dailySteps")
#check if dataset still has any missing values
sum(is.na(totalActivityImputed$dailySteps))
```

```
## [1] 0
```

```r
totalImputedStepsdf <- data.frame(totalActivityImputed)
# Plot the histogram using ggplot2
p <- ggplot(totalImputedStepsdf, aes(x = dailySteps)) + 
  geom_histogram(breaks = seq(0, 25000, by = 2500), fill = "#909800", col = "black") + 
  ylim(0, 30) + 
  xlab("Total Steps Taken Per Day") + 
  ylab("Frequency") + 
  ggtitle("Total Number of Steps Taken on a Day") + 
  theme_calc(base_family = "sans")
print(p)
```

![](figures ImputingNAs-1.png)<!-- -->

```r
#mean of the total number of steps taken per day is:
mean(totalActivityImputed$dailySteps)
```

```
## [1] 10766.19
```

```r
#median of the total number of steps taken per day is:
median(totalActivityImputed$dailySteps)
```

```
## [1] 10766.19
```
## Are there differences in activity patterns between weekdays and weekends?

```r
activity$date <- as.Date(strptime(activity$date, format="%Y-%m-%d"))
# Making a function which differentiates weekdays from weekends
activity$dayType <- sapply(activity$date, function(x) {
  if(weekdays(x) == "Saturday" | weekdays(x) == "Sunday")
  {y <- "Weekend"}
  else {y <- "Weekday"}
  y
})
activityByDay <-  aggregate(steps ~ interval + dayType, activity, mean, na.rm = TRUE)
# Plot using ggplot2
dayPlot <-  ggplot(activityByDay, aes(x = interval , y = steps, color = dayType)) + 
  geom_line() + ggtitle("Average daily steps by day type") + 
  xlab("Interval") + 
  ylab("Average number of steps") +
  facet_wrap(~dayType, ncol = 1, nrow=2) +
  scale_color_discrete(name = "Day type") +
  theme_calc(base_family = "sans")
print(dayPlot)
```

![](figures WeekdayWeekend-1.png)<!-- -->
