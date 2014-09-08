Reproducible Research - Peer Assessment 1
=====================================================

###The goal of this assignment was to read and analyze an activity monitoring data set. The entire process was to be recorded in an R Markdown file, which broke up the analysis into distinct parts.  This file shows the code that was used to do the analysis, as well as, the output at various stages of the analysis.


1. First the data was downloaded and checked to see if it needed to be processed.

- The data was originally downloaded as a zip file from the reproducible research course [website][1]. After the data was downloaded, the file was extracted to the working directory. The the file was loaded into r in a table called rawactivitydata. The first few rows of the table can be viewed below.

[1]: https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip



```r
rawactivitydata <- read.csv("activity.csv")
head(rawactivitydata)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

- Next the class of each of the three columns was determined.


```r
class(rawactivitydata$steps)
```

```
## [1] "integer"
```

```r
class(rawactivitydata$date)
```

```
## [1] "factor"
```

```r
class(rawactivitydata$interval)
```

```
## [1] "integer"
```

- The classes for the columns were all fine so the data was restored under the simplier variable name, "activitydata", which indicates that it was already processed.


```r
activitydata <- rawactivitydata
```

2. Then I attempted to answer the first question. What is the mean number of steps taken each day?

- I started to answer this question by calculating the total number of steps for each day.  I stored the value in a variable called totalsteps.


```r
totalsteps <- tapply(activitydata$steps,activitydata$date,sum)
totalsteps
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##         NA        126      11352      12116      13294      15420 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##      11015         NA      12811       9900      10304      17382 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##      12426      15098      10139      15084      13452      10056 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##      11829      10395       8821      13460       8918       8355 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##       2492       6778      10119      11458       5018       9819 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##      15414         NA      10600      10571         NA      10439 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##       8334      12883       3219         NA         NA      12608 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##      10765       7336         NA         41       5441      14339 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##      15110       8841       4472      12787      20427      21194 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##      14478      11834      11162      13646      10183       7047 
## 2012-11-30 
##         NA
```

- Next I made a histogram of the total steps taken each day. 


```r
hist(totalsteps, main = "Histogram of Total Steps by Day", xlab = "Total Steps" , col="red")
```

![plot of chunk Histogram of total steps taken each day](figure/Histogram of total steps taken each day.png) 

- We can see from this histogram that the average number of steps per day should fall between 10000 and 15000.

- Then the mean and median were calculated to see if they fall in that range.


```r
mean(totalsteps[!is.na(totalsteps)])
```

```
## [1] 10766
```

```r
median(totalsteps[!is.na(totalsteps)])
```

```
## [1] 10765
```

- As you can see from the calculations above both the mean and the median fell within that range.

3. Then I moved on to answer the question:  What is the daily activity pattern?

- I began to address this question by storing the intervals as a vector.


```r
intervals <- as.numeric(levels(factor(activitydata$interval)))
intervals
```

```
##   [1]    0    5   10   15   20   25   30   35   40   45   50   55  100  105
##  [15]  110  115  120  125  130  135  140  145  150  155  200  205  210  215
##  [29]  220  225  230  235  240  245  250  255  300  305  310  315  320  325
##  [43]  330  335  340  345  350  355  400  405  410  415  420  425  430  435
##  [57]  440  445  450  455  500  505  510  515  520  525  530  535  540  545
##  [71]  550  555  600  605  610  615  620  625  630  635  640  645  650  655
##  [85]  700  705  710  715  720  725  730  735  740  745  750  755  800  805
##  [99]  810  815  820  825  830  835  840  845  850  855  900  905  910  915
## [113]  920  925  930  935  940  945  950  955 1000 1005 1010 1015 1020 1025
## [127] 1030 1035 1040 1045 1050 1055 1100 1105 1110 1115 1120 1125 1130 1135
## [141] 1140 1145 1150 1155 1200 1205 1210 1215 1220 1225 1230 1235 1240 1245
## [155] 1250 1255 1300 1305 1310 1315 1320 1325 1330 1335 1340 1345 1350 1355
## [169] 1400 1405 1410 1415 1420 1425 1430 1435 1440 1445 1450 1455 1500 1505
## [183] 1510 1515 1520 1525 1530 1535 1540 1545 1550 1555 1600 1605 1610 1615
## [197] 1620 1625 1630 1635 1640 1645 1650 1655 1700 1705 1710 1715 1720 1725
## [211] 1730 1735 1740 1745 1750 1755 1800 1805 1810 1815 1820 1825 1830 1835
## [225] 1840 1845 1850 1855 1900 1905 1910 1915 1920 1925 1930 1935 1940 1945
## [239] 1950 1955 2000 2005 2010 2015 2020 2025 2030 2035 2040 2045 2050 2055
## [253] 2100 2105 2110 2115 2120 2125 2130 2135 2140 2145 2150 2155 2200 2205
## [267] 2210 2215 2220 2225 2230 2235 2240 2245 2250 2255 2300 2305 2310 2315
## [281] 2320 2325 2330 2335 2340 2345 2350 2355
```

- Then I calculated the mean number of steps for each time interval. I stored those means as a variable.


```r
meansteps <- tapply(activitydata[!is.na(activitydata$steps),]$steps, activitydata[!is.na(activitydata$steps),]$interval, mean)
```

- Next I plotted the mean steps against the time intervals.


```r
plot(intervals, meansteps, type = "l", ylim = c(0,250), xlab = "5 Minute Time Interval", ylab = "Mean Number of Steps", main = "Mean Number of Steps in Each 5 Minute Interval")
```

![plot of chunk plot of mean steps versus time interval](figure/plot of mean steps versus time interval.png) 

- Then I looked for the 5 minute time interval with the greatest average activity.


```r
intervals[which.max(meansteps)]
```

```
## [1] 835
```

- The intervals weren't particularilly well formated. 835 corresponds to 8:35 am.

4. Now I moved on to dealing with the missing input values.

- First I determined how many rows contained missing values.


```r
length(which(is.na(activitydata$steps)))
```

```
## [1] 2304
```

- Next I converted the missing values to the mean value for that specific time interval.


```r
activitydata2 <- activitydata
meanstepmatrix <- cbind(meansteps,intervals)
index <- which(is.na(activitydata$steps))
i <- 1
for(i in 1:length(which(is.na(activitydata$steps)))){
    row <- index[i]
    Interval <- activitydata$interval[row]
    activitydata2$steps[row] <- meanstepmatrix[meanstepmatrix[,2] == Interval,1]
}
head(activitydata2)
```

```
##     steps       date interval
## 1 1.71698 2012-10-01        0
## 2 0.33962 2012-10-01        5
## 3 0.13208 2012-10-01       10
## 4 0.15094 2012-10-01       15
## 5 0.07547 2012-10-01       20
## 6 2.09434 2012-10-01       25
```

- Then I recalculated the total number of steps for each day


```r
totalsteps2 <- tapply(activitydata2$steps,activitydata2$date,sum)
totalsteps2
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##      10766        126      11352      12116      13294      15420 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##      11015      10766      12811       9900      10304      17382 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##      12426      15098      10139      15084      13452      10056 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##      11829      10395       8821      13460       8918       8355 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##       2492       6778      10119      11458       5018       9819 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##      15414      10766      10600      10571      10766      10439 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##       8334      12883       3219      10766      10766      12608 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##      10765       7336      10766         41       5441      14339 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##      15110       8841       4472      12787      20427      21194 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##      14478      11834      11162      13646      10183       7047 
## 2012-11-30 
##      10766
```

- After that I remade the histogram of the total steps taken per day.


```r
hist(totalsteps2, main = "Histogram of Total Steps by Day (missing data included)", xlab = "Total Steps" , col="blue")
```

![plot of chunk make a histogram of the total number of steps for each day](figure/make a histogram of the total number of steps for each day.png) 

- As you can see the new histogram was petty similar to the original. There is not expected to be a large change in the mean or median for this data set.

- Then I recalculated the mean and median for the total steps taken per day.


```r
mean(totalsteps2)
```

```
## [1] 10766
```

```r
median(totalsteps2)
```

```
## [1] 10766
```

- As you can see the mean value did not change. The median and the mean were equivalent. Neither of these pieces of information are paticullarly surprising, because the missing values were replaced with the mean values for that time interval.

5.  Finally I attempeted to figure out if there were any differences between the actvity levels for the weekend and weekdays.

- First I changed the class of the date column from factor to Date.


```r
activitydata$date <- as.Date(as.character(activitydata$date))
```

- Then I made a column that declared the day of the week for each observation in the data set.


```r
activitydata <- cbind(activitydata,weekdays(activitydata$date))
names(activitydata)[4] <- "weekday"
head(activitydata)
```

```
##   steps       date interval weekday
## 1    NA 2012-10-01        0  Monday
## 2    NA 2012-10-01        5  Monday
## 3    NA 2012-10-01       10  Monday
## 4    NA 2012-10-01       15  Monday
## 5    NA 2012-10-01       20  Monday
## 6    NA 2012-10-01       25  Monday
```

- Next I made a second column that said whether the day was on the weekend or during the work week.


```r
daytype <- NULL
j <- 1
for(j in 1:length(activitydata$date)){
    if(activitydata$weekday[j] == "Saturday"|activitydata$weekday[j] == "Sunday"){
    daytypenew <- "weekend"         
    }   
    else{
    daytypenew <- "weekday"
    }
    daytype <- c(daytype,daytypenew)
}
activitydata <- cbind(activitydata,daytype)
head(activitydata)
```

```
##   steps       date interval weekday daytype
## 1    NA 2012-10-01        0  Monday weekday
## 2    NA 2012-10-01        5  Monday weekday
## 3    NA 2012-10-01       10  Monday weekday
## 4    NA 2012-10-01       15  Monday weekday
## 5    NA 2012-10-01       20  Monday weekday
## 6    NA 2012-10-01       25  Monday weekday
```

- Then I plotted the number of steps against the time interval for both the week days and the weekend.


```r
activitydatasplit <- split(activitydata, daytype)
meanstepsweekend <- tapply(activitydatasplit$weekend[!is.na(activitydatasplit$weekend$steps),]$steps, activitydatasplit$weekend[!is.na(activitydatasplit$weekend$steps),]$interval, mean)
meanstepsweekday <- tapply(activitydatasplit$weekday[!is.na(activitydatasplit$weekday$steps),]$steps, activitydatasplit$weekday[!is.na(activitydatasplit$weekday$steps),]$interval, mean)
par(mfcol= c(2,1))
par(mar = rep(4, 4))
plot(intervals, meanstepsweekend, type = "l", ylim = c(0,250), xlab = "5 Minute Time Interval", ylab = "Mean Number of Steps", main = "Weekend")
plot(intervals, meanstepsweekday, type = "l", ylim = c(0,250), xlab = "5 Minute Time Interval", ylab = "Mean Number of Steps", main = "Weekday")
```

![plot of chunk plots comparing the average number of steps per time interval for weekdays and weekends](figure/plots comparing the average number of steps per time interval for weekdays and weekends.png) 

- As can be seen from the plots above, the average number of steps during the week is definately at a maximum at about 7:00 or 8:00 am. However on the weekend the mean number of steps is more consistent throughout the day. Presumably this person is more active in mourning during the week, because they are getting ready for work (work, school or whatever occupies this particular persons day) and travelling there.  I'm assuming their weekend is more open for free activities, which is why the steps are seen to be more spread out throughout the day.
