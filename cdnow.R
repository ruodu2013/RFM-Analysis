rm(list = ls())

cdnow=read.csv("cdnow_students_transaction.csv")
head(cdnow)
cdnow$DATE=as.Date(cdnow$DATE,format="%m/%d/%y")

#1 step 1
calibration=subset(cdnow,cdnow$DATE<="1997-09-30")
head(calibration)
validation=subset(cdnow,cdnow$DATE>"1997-09-30")
head(validation)

# step 2 and step 3

monetary=aggregate(DOLLARS~ID,FUN=mean,data=calibration)
frequency=aggregate(DOLLARS~ID,FUN=length,data=calibration)
last.purchase=aggregate(DATE~ID,FUN=max,data=calibration)

calibration.summary=merge(monetary,frequency,by="ID")
calibration.summary=merge(calibration.summary,last.purchase,by="ID")
head(calibration.summary)
colnames(calibration.summary)=c("ID","monetary","frequency","lastpurchase")
calibration.summary$recency=as.numeric(max(calibration.summary$lastpurchase)-calibration.summary$lastpurchase)
summary(calibration.summary)
monetary=aggregate(DOLLARS~ID,FUN=mean,data=validation)
frequency=aggregate(DOLLARS~ID,FUN=length,data=validation)
last.purchase=aggregate(DATE~ID,FUN=max,data=validation)

validation.summary=merge(monetary,frequency,by="ID")
validation.summary=merge(validation.summary,last.purchase,by="ID")
colnames(validation.summary)=c("ID","monetary","frequency","lastpurchase")
head(validation.summary)
head(calibration.summary)

# step 4, merge the two samples

mergedcdnow=merge(x=calibration.summary,y=validation.summary,by="ID",all.x=TRUE)
head(mergedcdnow,10)

# step 5


mergedcdnow$retention=ifelse(is.na(mergedcdnow$frequency.y),0,1)

#2

head(mergedcdnow)
library(ggplot2)
library(dplyr)
library(tidyr)
library(knitr)
#decile for recency

mergedcdnow$decile_recency <- ntile(mergedcdnow$recency,10)

Decile_analysis_recency<-aggregate(mergedcdnow$retention, by = list(mergedcdnow$decile_recency), FUN = sum)
colnames(Decile_analysis_recency)<-c("DecileNumber","retention_times")
Decile_analysis_recency$retention_rate=Decile_analysis_recency$retention_times/2357


ggplot(Decile_analysis_recency, aes(x = DecileNumber,y=retention_rate)) + geom_bar(stat="identity")


#decile for monetary
mergedcdnow$decile_monerary.x <- ntile(mergedcdnow$monetary.x,10)

Decile_analysis_monetary<-aggregate(mergedcdnow$retention, by = list(mergedcdnow$decile_monerary.x), FUN = sum)
colnames(Decile_analysis_monetary)<-c("DecileNumber","retention_times")
Decile_analysis_monetary$retention_rate=Decile_analysis_monetary$retention_times/2357

ggplot(Decile_analysis_monetary, aes(x = DecileNumber,y=retention_rate)) + geom_bar(stat="identity")

#3.cumulative lifts chart
head(Decile_analysis_recency)
Decile_analysis_recency<-Decile_analysis_recency[order(-Decile_analysis_recency$retention_rate),]

average=sum(Decile_analysis_recency$retention_times)/23570

Decile_analysis_recency$cum_retention_rate<-seq(1,10)
for (i in 1:10){
  Decile_analysis_recency$cum_retention_rate[i]=cumsum(Decile_analysis_recency$retention_times)[i]/(2357*i)/average
}
Decile_analysis_recency$NewNumber<-seq(1,10)

plot(x=Decile_analysis_recency$NewNumber, y=Decile_analysis_recency$cum_retention_rate, type="l",xlab="DecileNumber",ylab="Cumulative Lift")

#lift chart for monetary
head(Decile_analysis_monetary)
Decile_analysis_monetary<-Decile_analysis_monetary[order(-Decile_analysis_monetary$retention_rate),]

average_monetary=sum(Decile_analysis_monetary$retention_times)/23570

Decile_analysis_monetary$cum_retention_rate<-seq(1,10)
for (i in 1:10){
  Decile_analysis_monetary$cum_retention_rate[i]=cumsum(Decile_analysis_monetary$retention_times)[i]/(2357*i)/average_monetary
}
Decile_analysis_monetary$NewNumber<-seq(1,10)

plot(x=Decile_analysis_monetary$NewNumber, y=Decile_analysis_monetary$cum_retention_rate, type="l",xlab="DecileNumber",ylab="Cumulative Lift")


head(mergedcdnow)


#4 
# linear
model_linear=lm(retention~recency+monetary.x+frequency.x,data=mergedcdnow)
summary(model_linear)
mergedcdnow$retention_linear_predict=predict(model_linear,data=mergedcdnow)
plot(mergedcdnow$recency,mergedcdnow$retention_linear_predict)
plot(mergedcdnow$monetary.x,mergedcdnow$retention_linear_predict)
plot(mergedcdnow$frequency.x,mergedcdnow$retention_linear_predict)

#logistic
model_logic<-glm(retention~recency+monetary.x+frequency.x,data=mergedcdnow,family="binomial")
summary(model_logic)
mergedcdnow$retention_logic_predict=predict(model_logic,data=mergedcdnow)

plot(mergedcdnow$recency,mergedcdnow$retention_logistic_predict)
plot(mergedcdnow$monetary.x,mergedcdnow$retention_logistic_predict)
plot(mergedcdnow$frequency.x,mergedcdnow$retention_logistic_predict)
