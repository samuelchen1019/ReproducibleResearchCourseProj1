library(data.table)
library(knitr)
library(tidyverse)
library(lubridate)
##Download and unzip the data file
fileName<-"repdata_data_activity.zip"
fileURL<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

if(!file.exists(fileName)){
  download.file(fileURL,destfile=fileName,method="curl")
}

if(!file.exists("activity.csv")){
  unzip(fileName)
}

ds<-fread("activity.csv")

dim(ds)
head(ds)
names(ds)
summary(ds)
str(ds)
ds$date<-ymd(ds$date)
Q1<-tapply(ds$steps,ds$date,sum,na.rm=TRUE)
Q1<-data.table(Q1)

Q1date<-unique(ds$date)
Q1$date<-Q1date

names(Q1)[[1]]<-"Total Steps"
##Histogram of the total number of steps taken each day
ggplot(Q1,aes(y=Q1$`Total Steps`,x=Q1$date))+geom_col(fill="red") + 
  ylab("Total Steps")+xlab("Date")+
  ggtitle("Total number of steps taken each day")

png("Total_Steps_1.png")
ggplot(Q1,aes(y=Q1$`Total Steps`,x=Q1$date))+geom_col(fill="red") + 
  ylab("Total Steps")+xlab("Date")+
  ggtitle("Total number of steps taken each day")
dev.off()

hist(Q1$`Total Steps`,xlab="Total Steps",ylab="Counts",ylim=c(0,30),
     main="Total Steps Historgram",col="blue",labels=TRUE)

ggplot(Q1,aes(x=`Total Steps`))+geom_histogram(bins = 61, fill="blue")+xlab("Total Steps")+
  ylim(0,10)+ggtitle("Total number of steps taken each day")

png("Total_Steps_2.png")
hist(Q1$`Total Steps`,xlab="Total Steps",ylab="Counts",
     main="Total Steps Historgram",col="blue",labels=TRUE)
dev.off()
##Mean and median number of steps taken each day
#Mean steps by date
temp<-select(ds,date,steps)
temp<-group_by(temp,date)
temp<-summarise(temp,mean(steps))
Q2<-temp
#Median steps by date
temp<-select(ds,date,steps)
temp<-group_by(temp,date)
temp<-summarise(temp,median(steps))
Q3<-temp
#Q3 is the result object;
names(Q3)<-c("Date","mediansteps")

Q3$meansteps<-Q2$`mean(steps)`
head(Q3)

##Time series plot of the average number of steps taken
ggplot(Q2,aes(y=`mean(steps)`,x=`date`))+geom_col(fill="red") + 
  ylab("Average Steps")+xlab("Date")+
  ggtitle("Average number of steps taken each day")
png("Average_Steps.png")
ggplot(Q2,aes(y=`mean(steps)`,x=`date`))+geom_col(fill="red") + 
  ylab("Average Steps")+xlab("Date")+
  ggtitle("Average number of steps taken each day")
dev.off()

##The 5-minute interval that, on average, contains the maximum number of steps
Q5<-aggregate(data=ds,steps~interval,FUN="mean")
ggplot(Q5,aes(x=`interval`,y=`steps`))+geom_line(color="red")
intrv_maxstepsintrv<-Q5[which.max(Q5$steps),]$interval
intrv_maxsteps<-ceiling(max(Q5$steps))

##Missing values
#Total number of the records with missing values in steps
sumNA<-sum(is.na(ds$steps))
Q6<-ds
Q6$missing<-is.na(Q6$steps)
Q6<-aggregate(data=Q6,missing~date+interval,FUN="sum")
Q6test<-aggregate(data=Q6,missing~date,FUN="sum")
Q6test1<-aggregate(data=Q6,missing~interval,FUN="sum")
ggplot(Q6test,aes(x=date,y=missing))+geom_point()
ggplot(Q6test1,aes(x=interval,y=missing))+geom_point()

#The NA value will be imputed with the mean of the value of this interval.
Q6avg<-ds #This is the data table for average value of the steps by intervals.
Q6avg$missing<-is.na(Q6avg$steps) #Add a variable for the missing step records.
Q6avg<-aggregate(data=Q6avg,steps~interval,FUN="mean")#Calculate the mean steps by intervals.
#NA imputing
ds_NAimputed<-ds
for(i in 1:nrow(ds_NAimputed)){
  if(is.na(ds_NAimputed[i,]$steps)){
    temp_interval<-ds_NAimputed[i,]$interval
    ds_NAimputed[i,]$steps<-Q6avg[Q6avg$interval==temp_interval,'steps']
  }
}

sumNA_imputed<-sum(is.na(ds_NAimputed$steps))
sumNA_imputed

##Histogram of the total number of steps taken each day after missing values are imputed
ds_NoNA_total<-aggregate(data=ds_NAimputed,steps~date,FUN="sum")
ggplot(ds_NoNA_total,aes(x=date,y=steps))+geom_col(fill="green")+
  xlab("Date")+ylab("Total Steps")+ggtitle("Total Number of Steps Every Day")
png("Total_Steps_Afterimputing.png")
ggplot(ds_NoNA_total,aes(x=date,y=steps))+geom_col(fill="green")+
  xlab("Date")+ylab("Total Steps")+ggtitle("Total Number of Steps Every Day")
dev.off()
hist(ds_NoNA_total$steps,col="red",labels = TRUE, 
     xlab = "Total Steps every day",
     ylim=c(0,40),
     main = "Total number of steps taken each day without missing values")
png("Hist_Total_Steps_Afterimputing.png")
hist(ds_NoNA_total$steps,col="red",labels = TRUE,
     xlab = "Total Steps every day",
     ylim=c(0,40),
     main = "Total number of steps taken each day without missing values")
dev.off()

##Mean steps after the imputing
mean_noNA<-mean(ds_NoNA_total$steps)
mean_noNA
##Median steps after the imputing
median_noNA<-median(ds_NoNA_total$steps)
median_noNA

##To compare the mean and median steps before and after the imputing.
mean_b4impu<-mean(Q1$`Total Steps`)
median_b4impu<-median(Q1$`Total Steps`)
comprs<-c(mean_b4impu,mean_noNA,median_b4impu,median_noNA)
barplot(comprs,ylim=c(0,12000),col=comprs)


##Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
#Add the weekday variable
ds_NAimputed$weekday<-wday(ds_NAimputed$date,label = TRUE)
ds_NAimputed$wdtag<-rep("",17568)
#Add tags of "weekday" and "weekend" to all the records.
for (i in 1:nrow(ds_NAimputed)) {
  if (ds_NAimputed[i,]$weekday %in% c("周六","周日")) {
    ds_NAimputed[i,]$wdtag<-"weekend"
  }
  else{
    ds_NAimputed[i,]$wdtag<-"weekday"
  }
}
#Calcuate the average steps by intervals
intravg<-aggregate(data=ds_NAimputed,steps~interval+wdtag,FUN="mean")
#plots combined.
#ggplot(intravg,aes(x=interval,y=steps,group=wdtag,colour=wdtag))+geom_line()
ggplot(intravg,aes(x=interval,y=steps))+geom_line()+facet_grid(rows=vars(wdtag))
png("comparison.png")
ggplot(intravg,aes(x=interval,y=steps))+geom_line()+facet_grid(rows=vars(wdtag))
dev.off()

