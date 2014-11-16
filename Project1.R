

#####  Load and Processing the Data  #####
setwd("C:/Users/rob/Desktop/Courses/01 - Data Science/05 - Reproducible Research")
activity <- read.csv("./RepData_PeerAssessment1/activity.csv")

head(activity)
summary(activity)

#Convert date from factor 
activity$date <- as.Date(activity$date, format="%Y-%m-%d")

####    What is mean total number of steps taken per day####
        #ignore missing values

        #Make a histogram of the total number of steps taken each day
        completeActivity <- activity[complete.cases(activity),]
        totalStepsByDate <- aggregate(steps ~ date, data=completeActivity, rm.na=TRUE, sum)
        hist(totalStepsByDate$steps, xlab="Steps", main="Histogram of Total Steps by Date")

        #Calculate the mean and median total number of steps taken per day
        summary(totalStepsByDate$steps)        

        
####   What is the average daily activity pattern? ######

        #average interval steps all days
        intervalAverage <- aggregate(steps ~ interval, data=activity, mean)
        #time series plot of intervals across all days
        plot(intervalAverage$steps, type="l", xlab="5 minute Interval", 
             ylab="Average Interval Steps all Days", main="Average Interval Steps All Days")

        #Which interval contains maximum number of steps
        which.max(intervalAverage$steps)
                #outputs 104th interval at 206.169 steps


#### Imputing Missing Values  #####
        
        #Calcualte total No. of missing values in dataset
        nrow(activity[!complete.cases(activity),])
        table(is.na(activity$steps))
        
        #Take interval average removing NAs
        intervalAverageComplete <- aggregate(steps ~ interval, data=activity, rm.na=TRUE, mean)
                #rename variables        
        names(intervalAverageComplete) <- sub("steps", "intAverage", names(intervalAverageComplete))
        
        #Fill in NAs with interval average from previous step
                #Merge datasets to include inverval average in activity
                activity2 <- merge(activity, intervalAverageComplete, by="interval")
                #replace missing NAs
                activity2$steps[is.na(activity2$steps)] <- activity2$intAverage[is.na(activity2$steps)]
                #drop average column
                activity2 <- activity2[,1:3]

        #4 Make histogram total number of steps each day, calculate mean median,
                #histogram
                totalStepsNewDF <- aggregate(steps ~ date, data=activity2, sum)
                hist(totalStepsNewDF$steps, xlab="Steps", main="Histogram of Total Steps by Date")
                #calculate mean and median of total No. steps take per day
                summary(totalStepsNewDF$steps)
        

##### Differences in activity patterns between weekdays and weeekends? ######

        #Create new factor variable in dataset for weekend weekday
        activity2$wend <- as.factor(ifelse(weekdays(activity2$date)%in% c("Saturday","Sunday"),"Weekend","Weekday"))  

        #calculate average No steps taken per day
                #subset weekday
                subsetWeekday <- activity2[activity2$wend=="Weekday",]
                subsetWeekend <- activity2[activity2$wend=="Weekend",]

                weekdayStepsAverage <- aggregate(steps ~ interval, data=subsetWeekday, mean)
                weekendStepsAverage <- aggregate(steps ~ interval, data=subsetWeekend, mean)

                #plot weekday interval average
                plot(weekdayStepsAverage$steps, type="l", xlab="5 minute Interval",
                        ylab="", main="heading")
                
                #Plot weekend interval average
                plot(weekendStepsAverage$steps, type="l", xlab="5 minute Interval",
                        ylab="", main="heading")

#What is the average daily activity pattern
  #calucualte mean for each day
  mean1 <- tapply(activity$steps, activity$date, mean)
  mean2 <- aggregate(steps ~ date, data=activity, mean)
  plot(tapply(activity$steps, activity$date, mean), type="l")

  #which 5 minute inverval, on average across all the days in the dataset, contains
  #the maximum number of steps
  tapply(activity$steps, activity$interval, mode)

#Inputing missing values
  #calculate total number of rows with NA
  nrow(activity) - nrow(activity[complete.cases(activity),])
  nrow(activity[!complete.cases(activity),])
    #counts number of rows with NAs

  #fill in missing values to dataset
  #use mean/medium for that day
  #use mean for that inverval
  activity$steps[activity$steps==NA]

                        # Imputing missing values
table(is.na(activity$steps))
#create mean of intervals
  intMean <- aggregate(steps ~ interval, data=activity, mean, na.rm=TRUE)
  #rename column
  names(intMean) <- sub("steps", "intAverage", names(intMean))
# merge data sets
  activity2 <- merge(activity, intMean, by="interval")

#replace NAs
activity2$steps[is.na(activity2$steps)] <- activity2$intAverage[is.na(activity2$steps)]

#historgram of total number of steps taken each day
hist(aggregate(steps ~ date, data=activity2, sum))
  #aggreate doens't work b/c leave in dataframe of 2 columns
hist(tapply(activity2$steps, activity2$date, sum))
    #tapply only outputs matrix of one column
#calculate mean # steps take each day
#calculate median # steps taken each day
  summary(tapply(activity2$steps, activity2$date, sum))

###
#Are there differences in activity patterns between weekdays and weekends
###
  #create weekend/weekday factor variable
    activity2$wend <- as.factor(ifelse(weekdays(activity2$date)%in% c("Saturday","Sunday"),"Weekend","Weekday"))  
  #make a panel plot containing a time series plot of the 5 minute interval (x axis) and
  #the average No. steps taken, averaged across all weeekday or weekend days (y-axis).
  #see readme file in the github rep to see an exmaple of what this plot should look like 
    tapply(activity2$steps, activity2$wend==Weekday, mean)

        #above not working since == weekday


###
#
###
isWeekday(activity2$date)
  
  activity2$Weekend <- factor(activity2$date)
  activity2$WDay[activity2$D] <- as.factor(
    )

  nrow(intMean)
#assign true to new data set and grab row
activity2 <- activity


#subsetting the data
activity$steps==NA
#tells me which rows have NA in steps
which(is.na(activity$steps))





hist(log10(activity$steps))

table(is.na(activity$steps))
tail(activity$steps)
table(activity$steps)
sum(activity$steps)
