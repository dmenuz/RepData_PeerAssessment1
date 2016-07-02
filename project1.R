#Script for analyzing activity data

#Reproducible Research Project 1 June 29, 2016

##Loading and preprocessing the data

###Loading data
setwd("C:\\Users\\diane\\Desktop\\gitRepo\\RepData_PeerAssessment1")
datfile="https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
destfile="dat.zip"
download.file(datfile, destfile)
unzip(destfile)
act=read.csv("activity.csv")

###Preprocessing data

##What is mean total number of steps taken per day?

#Data will be processed ignoring all of the missing values. First I will drop 
#all of the NA values and calculate the number of steps taken per day
act2=act[is.na(act$steps)==FALSE,]
act2$date=factor(act2$date)
dailySteps=tapply(act2$steps, list(act2$date), sum)
summary(dailySteps)

#Next I will present a histogram of the number of steps taken each day.

hist(dailySteps, main="Histogram of activity", xlab="daily steps")

#Last I will calculate the mean and median number of steps taken per day

meanStep=mean(dailySteps)
medianStep=median(dailySteps)

#There was a mean of `r meanStep` and a median of `r medianStep` steps taken per
#day.

##What is the average daily activity pattern?

#Time series plot of average steps per 5 minute intervals across all days

avgInterval=aggregate(act2$steps, list(act2$interval),mean)
plot(x~Group.1, data=avgInterval, type="l",xlab="minutes in day", 
     ylab="average number of steps")

maxSteps=max(avgInterval$x)
maxInt=avgInterval$Group.1[avgInterval$x==maxSteps]

#On average, interval `r maxInt` contains the maximum number of steps

##Imputing missing values

numNA=length(which(is.na(act$steps)==TRUE))

#There are `r nuNA` missing values in the dataset.

#I a going to fill in the missing values using the mean value for the 5-minute
#interval. I first tried the median value, but the daily step count based on that
#data was much too low. It seems like most days the idividual was very activity 
#for part of the day, but that for most of the day, the median number of steps 
#was zero or close to zero.

actmerge=merge(act, avgInterval, by.y="Group.1", by.x="interval")
actmerge$stepsupdate=ifelse(is.na(actmerge$steps)==TRUE, actmerge$x, actmerge$steps)

dailySteps2=tapply(actmerge$stepsupdate, list(actmerge$date), sum)
summary(dailySteps2)

#Next I will present a histogram of the number of steps taken each day and calculate
#the mean and median numbers of steps.

hist(dailySteps2, main="Histogram of activity", xlab="daily steps")

meanStep2=mean(dailySteps2)
medianStep2=median(dailySteps2)

#There was a mean of `r I(meanStep2)` and a median of `r medianStep2` steps taken per
#day.

median=c(medianStep, medianStep2)
mean=c(meanStep, meanStep2)
compareTable=rbind(median, mean)
colnames(compareTable)=c("NA removed", "NA imputed")
compareTable

#As shown in the table above, the mean value did not change with the imputed values
#while the median value changed by a value of 1.19. The dataset only has missing 
#values for entire days, not for just some intervals within a day. The sum of the 
#average value for each time interval is equal to the sum of the average value 
#for a day, so it makes sense that the mean value did not change.

##Are there differences in activity patterns between weekdays and weekends?

mydate <- factor("1/15/2006 0:00:00")
actmerge$date2=as.Date(actmerge$date, format = "%Y-%m-%d")
actmerge$dayOfWeek=weekdays(actmerge$date2)
actmerge$dayType=ifelse(actmerge$dayOfWeek %in% c("Saturday", "Sunday"),
        "weekend", "weekday")
ag=aggregate(actmerge$stepsupdate, by=list(actmerge$interval, actmerge$dayType), mean)
colnames(ag)=c("interval", "dayType", "meanSteps")

library(ggplot2)

qplot(interval, meanSteps, data=ag, geom="line",facets=dayType~.,
    xlab="5-minute interval", ylab="mean steps") 

#The plots show a different pattern between weekday and weekend activities.On 
#weekdays,the individual becomes active earlier in the day than on weekends and 
#tends to concentrate most of the activity shortly after waking up, followed by
#lower levels of activity the rest of the day. One the weekends, the indivual
#appears to sleep in later and go to bed later and spread the activity out across
#the entire day, though the mean peak of activity is still at a similar time.



