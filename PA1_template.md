# Reproducible Research: Peer Assessment 1

```r
require(dplyr)
```

```
## Loading required package: dplyr
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
require(data.table)
```

```
## Loading required package: data.table
## 
## Attaching package: 'data.table'
## 
## The following objects are masked from 'package:dplyr':
## 
##     between, last
```

```r
require(ggplot2)
```

```
## Loading required package: ggplot2
```

```r
require(lattice)
```

```
## Loading required package: lattice
```

```r
require(latticeExtra)
```

```
## Loading required package: latticeExtra
## Loading required package: RColorBrewer
## 
## Attaching package: 'latticeExtra'
## 
## The following object is masked from 'package:ggplot2':
## 
##     layer
```

```r
require(knitr)
```

```
## Loading required package: knitr
```

## Loading and preprocessing the data

```r
##Load & process the data
activ_data <- read.csv("activity.csv")
activ_data$date <- as.Date(activ_data$date, "%Y-%m-%d")
```



##Calculate the total number of steps taken per day

```r
steps_day <- aggregate(steps ~ date, activ_data, sum)
steps_day
```

```
##          date steps
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## 11 2012-10-13 12426
## 12 2012-10-14 15098
## 13 2012-10-15 10139
## 14 2012-10-16 15084
## 15 2012-10-17 13452
## 16 2012-10-18 10056
## 17 2012-10-19 11829
## 18 2012-10-20 10395
## 19 2012-10-21  8821
## 20 2012-10-22 13460
## 21 2012-10-23  8918
## 22 2012-10-24  8355
## 23 2012-10-25  2492
## 24 2012-10-26  6778
## 25 2012-10-27 10119
## 26 2012-10-28 11458
## 27 2012-10-29  5018
## 28 2012-10-30  9819
## 29 2012-10-31 15414
## 30 2012-11-02 10600
## 31 2012-11-03 10571
## 32 2012-11-05 10439
## 33 2012-11-06  8334
## 34 2012-11-07 12883
## 35 2012-11-08  3219
## 36 2012-11-11 12608
## 37 2012-11-12 10765
## 38 2012-11-13  7336
## 39 2012-11-15    41
## 40 2012-11-16  5441
## 41 2012-11-17 14339
## 42 2012-11-18 15110
## 43 2012-11-19  8841
## 44 2012-11-20  4472
## 45 2012-11-21 12787
## 46 2012-11-22 20427
## 47 2012-11-23 21194
## 48 2012-11-24 14478
## 49 2012-11-25 11834
## 50 2012-11-26 11162
## 51 2012-11-27 13646
## 52 2012-11-28 10183
## 53 2012-11-29  7047
```



##Make a histogram of the total number of steps taken each day

```r
hist(steps_day$steps)
```

![](https://github.com/emanuser/RepData_PeerAssessment1/blob/master/instructions_fig/unnamed-chunk-4-1.png) 

##Summary statistics to claculate the mean and median of the total number of steps taken per day

```r
summary(steps_day$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10760   10770   13290   21190
```

## 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps

```r
dt = data.table(activ_data) ###Copy original data and convert to data table
dt[, steps.mean := mean(steps, na.rm = T), by = interval] ###Add columen with mean steps acrose all days by time interval
interval_ave_max <- filter(dt, max(steps.mean)  == steps.mean) ### Use dplyr to subset table by max mean steps
unique(interval_ave_max$interval) ### 5-minute interval corasponding to max mean
```

```
## [1] 835
```

## Calculate and report the total number of missing values in the datase

```r
sum(is.na(activ_data))
```

```
## [1] 2304
```

## Imputing missing values
## New dataset with missing values filled in with mean for that 5-minute interval

```r
no_missing_values  <- as_data_frame(dt) ###Transform table in to dataframe

for(i in 1:17568) { ### for loop to replace missing values
  if(is.na(no_missing_values$steps[i])){
    no_missing_values$steps[i] = no_missing_values$steps.mean[i]
  }else(no_missing_values$steps[i] = no_missing_values$steps[i])
}
```





```r
##Calculate the total number of steps taken per day
steps_day_2 <- aggregate(steps ~ date, no_missing_values, sum)  

##Make a histogram of the total number of steps taken each day
hist(steps_day_2$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

```r
##Compare with summary statistics after imputing missing values
summary(steps_day_2$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10770   10770   12810   21190
```

```r
summary(steps_day$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10760   10770   13290   21190
```

##  Differences in activity patterns between weekdays and weekends

```r
##Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
day_of_week <- weekdays(no_missing_values$date)
day_of_week <- as.data.frame(day_of_week) ### convert to data dataframe 
week_day_end <- gsub("Saturday|Sunday", "weekend", day_of_week[,1]) ### serch for days corisponding to weekend and replace 
week_day_end <- gsub("Monday|Tuesday|Wednesday|Thursday|Friday", "weekday", week_day_end) ### serch for days corisponding to weekday and replace 

with_Day_column <-  data.frame(no_missing_values, week_day_end) ### bind sepret data frames in to one table
#with_Day_column <- as.data.frame(with_Day_column)

with_Day_column_2 <- aggregate(steps ~ week_day_end+interval, with_Day_column, mean) ### average number of steps across all weekday days or weekend days

ggplot(with_Day_column_2, aes(x=interval , y=steps, color = week_day_end))+
  geom_line(size=1)+
  facet_grid( week_day_end~. )+
  ggtitle("Average Number of Steps in Five Minute Intervals")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

```r
xyplot(steps ~ interval | week_day_end,  data = with_Day_column_2,
       ylab = "Mean Steps", xlab = "Interval", main = "Average Number of Steps in Five Minute Intervals",
       type = "l", lty = 1, col = "black", layout = c(1, 2), lattice.options = theEconomist.opts())
```

![](PA1_template_files/figure-html/unnamed-chunk-10-2.png) 

