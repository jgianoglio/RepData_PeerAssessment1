# Include ggplot2 library
library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")

unzip("activity.zip")

# read the data from the unzipped file into a data frame
stepData <- read.csv("activity.csv")

# Convert the interval variable from an int to a factor
# stepData$interval <- factor(stepData$interval)

# calculate total number of steps per day
dailySteps <- aggregate(stepData$steps, by=list(Date=stepData$date), FUN=sum)

# rename steps column of dailySteps
names(dailySteps)[names(dailySteps)=="x"] <- "Steps"

# generate histogram of days by step count
ggplot(dailySteps, aes(dailySteps$Steps)) + geom_histogram()

# calculate median number of steps per day
medianSteps <- median(dailySteps$Steps, na.rm=TRUE)

# calculate mean number of steps per day
meanSteps <- mean(dailySteps$Steps, na.rm=TRUE)

#gets the mean number of steps per interval
intervalAvg <- aggregate(stepData$steps, by=list(interval=stepData$interval), FUN = mean, na.rm = TRUE)

# rename calculated column of avg steps
names(intervalAvg)[names(intervalAvg)=="x"] <- "avgSteps"

# plots the intervalAvg
ggplot(intervalAvg, aes(interval, avgSteps)) + geom_line(aes(group=1))

#gets the row with the highest avg steps
highIntervalRow <- intervalAvg[which.max(intervalAvg$avgSteps), ]
highInterval <- highIntervalRow$interval

#calculate number of observatiosn with missing values
numMissingVals <- sum(is.na(stepData))

# Create a new dataset that is equal to the original dataset, but with missing
# data filled in by the interval average number of steps
stepData2 <- stepData
for (i in 1:nrow(stepData2)){
        # print(stepData[i, 1])
        if (is.na(stepData2[i,1])){
                stepData2[i,1] <- intervalAvg[intervalAvg$interval == stepData2[i,3],2]
        }
}

# Now, with the new dataset with missing values imputed, we'll calculate and
# make a histogram of the total number of steps taken each day. Then we'll
# calculate and report the mean and median total number of steps taken per day.

dailySteps2 <- aggregate(stepData2$steps, by=list(Date=stepData2$date), FUN=sum)

names(dailySteps2)[names(dailySteps2)=="x"] <- "Steps"

ggplot(dailySteps2, aes(dailySteps2$Steps)) + geom_histogram()

meanSteps2 <- mean(dailySteps2$Steps, na.rm=TRUE)

medianSteps2 <- median(dailySteps2$Steps, na.rm=TRUE)

# Create new data frame from stepData2
stepData3 <- stepData2

# Convertthe date variable from a factor to a date class
stepData3$date <- as.Date(stepData3$date)
stepData3$dayType <- "weekday"
for(j in 1:nrow(stepData3)) {
        if(weekdays(stepData3$date[j]) == "Saturday" | weekdays(stepData3$date[j]) == "Sunday") {
                stepData3$dayType[j] <- "weekend"
        }
}
stepData3$dayType <- factor(stepData3$dayType)

#gets the mean number of steps per interval grouped by dayType
avgByDayType <- aggregate(stepData3$steps, by=list(interval=stepData3$interval, dayType=stepData3$dayType), FUN = mean)

# plot the intervalAvg, grouped by weekend vs weekday
ggplot(stepData3, aes(interval, steps)) + geom_line(aes(group=1))

# rename calculated column of avg steps
names(avgByDayType)[names(avgByDayType)=="x"] <- "avgSteps"

# plots the avg steps per interval by dayType
ggplot(avgByDayType, aes(interval, avgSteps)) + geom_line(aes(group=1))+facet_wrap(~dayType, nrow=1)
