library(RWeka)
library(party)
library(caret)
library(rJava)
library(FSelector)
library(class)
library(dplyr)

df=Absenteeism_at_work

df$Height<-NULL
df$Weight<-NULL
df$Body.mass.index<-NULL
df$Pet<-NULL
df$Day.of.the.week<-NULL
df$Education<-NULL
df$Seasons<-NULL
df$Month.of.absence<-NULL
df$Transportation.expense<-NULL
df$Service.time<-NULL
df$Age<-NULL
df$ID<-NULL
df$Work.load.Average.day<-NULL
df$Hit.target<-NULL
df$Son<-NULL
df$Social.drinker<-NULL
df$Social.smoker<-NULL

#Particionamiento
index=createDataPartition(df$Reason.for.absence,p=0.8,list = FALSE)
train=df[index, ]
test=df[-index, ]

boxplot(df$Weight~df$Height)

#Ganancia de informacion
information.gain(Reason.for.absence~.,data = train)

#Modelo
modelof = knn(train[-1],test[-1],train$Reason.for.absence, k=28)

#Matriz de confusion
matriz=table(modelof,test$Reason.for.absence)

#Precision
precision<-sum(diag(matriz))/sum(matriz)*100
