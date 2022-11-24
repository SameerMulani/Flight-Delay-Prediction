Delay<-read.csv(file.choose())
Delay
View(Delay)

library(caTools)
library(e1071)

Delay$Month<-as.factor(Delay$Month)
Delay$Day<-as.factor(Delay$Day)
Delay$Airline_name<-as.factor(Delay$Airline_name)
#Delay$Arrival<-as.factor(Delay$Arrival)
Delay$DepDelay<-as.factor(Delay$DepDelay)
Delay$CRSDepTime<-as.factor(round(Delay$CRSDepTime/100))
Delay$CRSArrTime<-as.factor(round(Delay$CRSArrTime/100))
Delay$A<-as.factor(Delay$A)


str(Delay)

set.seed(1234)
split<-sample.split(Delay$DepDelay,SplitRatio=0.75)
split

train<-subset(Delay,split==TRUE)
test<-subset(Delay,split==FALSE)
train
dim(train)    
dim(test)     


#model on train data set
B1.fit<-naiveBayes(DepDelay~., data= train)
B1.fit
pre<-predict(B1.fit,train)
table(pre,train$DepDelay)

#Test model:
pre2<-predict(B1.fit,test)
table(pre2,test$DepDelay)
