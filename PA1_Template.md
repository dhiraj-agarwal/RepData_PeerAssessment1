# Reproducible Research: Peer Assessment 1
Dhiraj Agarwal  
October 17, 2014  

```r
options(scipen = 1, digits = 2)
if(!suppressMessages(require(ggplot2))){
    print('trying to install ggplot2')
    install.packages('ggplot2')
    if(suppressMessagesrequire(ggplot2)){
        print("ggplot2 installed and loaded")
    } else {
        stop("could not install ggplot2")
    }
}
```

##Loading and preprocessing the data
Load the data (i.e. read.csv())
Process/transform the data (if necessary) into a format suitable for your analysis


```r
activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date, "%Y-%m-%d")
head(activity)
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

##What is mean total number of steps taken per day?

```r
# Summarize the data by day
daily_activity <-
  aggregate(formula = steps~date, data = activity,
            FUN = sum, na.rm=TRUE)

# Calculate summary statistics
#mean_steps <- round(mean(daily_activity$steps), 2)  # Mean
mean_steps <- mean(daily_activity$steps) # Mean
#median_steps <- quantile(x = daily_activity$steps, probs = 0.5)  #  Median, 50%Q
median_steps <- median(daily_activity$steps) 
```

The subject took a mean of 10766.19 and a median of 10765 steps per day.

The number of steps taken daily by the subject are represented in the figure 1 below. 


```r
# Plot the total number of steps per day.
# Use ggplot2 to summarize de data, to
# find inconsistencies with the analysis.
histogram <- 
qplot(x=date, y=steps,
      data=subset(activity, complete.cases(activity)),
      stat='summary', fun.y=sum, geom='bar') +
  labs(title='Figure 1: Number of steps taken daily\n',
       y='Total steps per day', x='Date')
plot(histogram)
```

![](./PA1_Template_files/figure-html/unnamed-chunk-4-1.png) 

##What is the average daily activity pattern?

```r
interval_activity <- 
  aggregate(formula=steps~interval, data=activity,
            FUN=mean, na.rm=TRUE)

# Get the data for the interval with the most average activity across the days
max_steps <- interval_activity[which(interval_activity$steps==max(interval_activity$steps)),]
```


```r
mean_ci <- function(data){
    m <- mean(data)
    data.frame(y=m,
               ymin = m-(1.96*sd(data)/sqrt(length(data))),
               ymax = m+(1.96*sd(data)/sqrt(length(data))))
}

# Plot the average number of steps per interval.
# Use ggplot2 to summarize de data, to
# find inconsistencies with the analysis.
# Geom 'line' is equivalent to 'type="l"' in plot.
steps_per_interval <- 
qplot(x=interval, y=steps, 
      data=subset(activity, complete.cases(activity)), 
      geom='line', stat='summary', fun.data=mean_ci) +
  labs(title='Figure 2: Average no. of steps taken each interval, across the days\n',
       y='Average steps per interval', x='Interval')

steps_per_interval
```

![](./PA1_Template_files/figure-html/unnamed-chunk-6-1.png) 

##Imputing missing values


```r
total_NAs <- sum(!complete.cases(activity))
step_NAs <- sum(is.na(activity$steps))

# Calculate the number of missing dates
dates_in_range <- seq.Date(from = min(activity$date),
                           to = max(activity$date),
                           by='1 day')
date_NAs <- sum(!activity$date[complete.cases(activity)] %in% dates_in_range)
```

The number of missing steps values in the dataset is ``2304``

The number of missing date values in the dataset is ``0``

The total number of missing values in the entire dataset is ``2304``


```r
# Use previously calculated means
interval_activity$imputed_steps <- floor(interval_activity$steps)

# Merge the replacement values
imputed_activity <- merge(activity,
                          interval_activity[,c('interval', 'imputed_steps')],
                          by='interval')

# Replace the missing values
imputed_activity$steps <- ifelse(is.na(imputed_activity$steps),
                                 imputed_activity$imputed_steps,
                                 imputed_activity$steps)

# Remove unnecesary data
imputed_activity$imputed_steps <- NULL
```


```r
# Summarize the data by day
daily_imputed_activity <-
  aggregate(formula = steps~date, data = imputed_activity,
            FUN = sum, na.rm=TRUE)

# Calculate summary statistics
mean_imputed_steps <- round(mean(daily_imputed_activity$steps), 2)
median_imputed_steps <- quantile(x = daily_imputed_activity$steps, probs = 0.5)
```

The mean and median total number of steps taken per day are ``10749.77`` and ``10641`` respectively.



```r
# Replace the data in the original histogram with the imputed data
histogram %+% imputed_activity +
  labs(title='Figure 3: Number of steps taken each day,\nafter imputing missing values')
```

![](./PA1_Template_files/figure-html/unnamed-chunk-10-1.png) 

##Are there differences in activity patterns between weekdays and weekends?

```r
## weekdays.Date <-
## function (x, abbreviate = FALSE) 
## format(x, ifelse(abbreviate, "%a", "%A"))

# Label each date as weekday/weekend (1:5 are weekdays, 6:7 are weekends)
imputed_activity$week_part <- factor(
  ifelse(as.integer(format(imputed_activity$date, format = '%u')) %in% c(1:5),
         'weekday', 'weekend'))

# Plot the average steps per interval, given the week_part
steps_per_interval %+% imputed_activity + facet_grid(week_part~.) +
  labs(title='Figure 4: Average no. of steps taken each interval across the days, \n given the part of the week')
```

![](./PA1_Template_files/figure-html/unnamed-chunk-11-1.png) 
