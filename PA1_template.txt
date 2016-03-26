# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
library(plyr)
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.2.3
```

```r
activity<-read.csv("./activity.csv")
```


## What is mean total number of steps taken per day?
First, I'll calculate the total number of steps taken during each day:

```r
sums<-ddply(activity, .(date), summarize, val = sum(steps))
```
Next, generate a histogram of the total number of steps taken per day:

```r
hist(sums$val, main="Steps taken Per Day", xlab="Total Steps Taken Per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)

Finally, we'll calculate the Mean and Median number of steps taken.

```r
meanSteps<-mean(sums$val, na.rm=TRUE)
medianSteps<-median(sums$val, na.rm=TRUE)
```

The Mean number of steps taken is 

```
## [1] 10766.19
```
and the Median number of steps taken is

```
## [1] 10765
```

## What is the average daily activity pattern?
Generate a time series plot of average steps by time interval across all days:

```r
#calc mean number of steps by time interval
avgSteps<-aggregate(steps ~ interval, activity, mean)

#plot results
plot(avgSteps$interval, avgSteps$steps, type='l', col=1, main="Mean Steps by Interval", xlab="5-Minute Interval Number", ylab="Mean Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)

```r
#figure out which interval has the most number of steps:
maxVal<-subset(avgSteps, avgSteps$steps==max(avgSteps$steps))
```
The interval that had the highest number of steps was:

```
## [1] 835
```

The Mean number of steps in that interval was:

```
## [1] 206.1698
```


## Imputing missing values
The total number of rows with NA values is:

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

In order to estimate NA values, I will replace the NA value with the Mean value for that time interval:

```r
#create another data set identical to Activity
impActivity<-activity
#go through and check for NA.  if NA, replace with mean for that time interval.
for(i in 1:nrow(impActivity)){
        if(is.na(impActivity$steps[i])) {
                subMean<-subset(avgSteps, avgSteps$interval==impActivity$interval[i])
                impActivity$steps[i]<-subMean$steps
        }
}
```

Recreate Histogram for total number of steps taken per day and generate new Mean and Median values:

```r
##calculate sums
impSums<-ddply(impActivity, .(date), summarize, val = sum(steps))
##generate histogram
hist(impSums$val, main="Imputed Steps taken Per Day", xlab="Total Steps Taken Per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)

```r
##calculate mean and median
meanImpSteps<-mean(impSums$val)
medianImpSteps<-median(impSums$val)
```

The Mean Imputed number of steps per day is:

```
## [1] 10766.19
```

The Median Imputed number of steps per day is:

```
## [1] 10766.19
```
Based on this method, the total number of steps per day was greater (as would be expected). By substituting mean values for each interval, the effect to the daily mean and median values was minimal, which indicates that this method of imputing the data proves to be a good approach.

## Are there differences in activity patterns between weekdays and weekends?


```r
##First, add a factor variable for weekend or weekday in the data.

weekPartFunc<-function(dateVal) {
        day<-weekdays(as.Date(dateVal), abbr=TRUE)
        if(day == 'Sat' || day == 'Sun') {
                part='Weekend'
        }
        else {
               part='Weekday'
                }       
        part
}

impActivity$weekPart<-as.factor(sapply(impActivity$date, weekPartFunc ))

weekdayActivity<-subset(impActivity, weekPart=="Weekday")
weekendActivity<-subset(impActivity, weekPart=="Weekend")

weekpartSteps<-aggregate(steps ~interval+weekPart, impActivity, mean)

#calc mean number of steps by time interval
ggplot(weekpartSteps, aes(interval, steps)) +
     geom_line(stat = "identity", aes(colour = weekPart)) +
     facet_grid(weekPart ~ ., scales="fixed", space="fixed") +
     labs(x="Interval", y=expression("Steps")) +
     ggtitle("Steps per Interval:  Weekday vs Weekend")
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)


Based on the above plot, measured activity (steps) starts earlier during the week, but activity lasts longer in the day on the weekend.  There also appears to be fewer average steps taken in the weekend mornings as compared to weekday mornings.
