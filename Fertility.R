library(RWeka)
library(party)
library(caret)
library(rJava)
library(FSelector)
library(class)
library(dplyr)
df=read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/00244/fertility_Diagnosis.txt"),sep = ",", header = FALSE)
names(df)<-c("SeasonInWhichAnalysisWasPermormed","AgeAtTimeOfAnalysis","ChildishDiseases","AccidentOrTrauma","SurgicalIntervention","HighFeversInLastYear","FrecuencyOfAlcoholConsumption","SmokingHabit","NumberOfHRSittingPerDay","Diagnosis")

set.seed(1)

#Eliminar atributos
df$SeasonInWhichAnalysisWasPermormed<-NULL
df$ChildishDiseases<-NULL
df$AccidentOrTrauma<-NULL
df$SmokingHabit<-NULL
df$HighFeversInLastYear<-NULL

MaxA=max(df$AgeAtTimeOfAnalysis)
MinA=min(df$AgeAtTimeOfAnalysis)
mediaA=(df$AgeAtTimeOfAnalysis[1]-MaxA)/(MaxA-MinA)

MaxFAC=max(df$FrecuencyOfAlcoholConsumption)
MinFAC=min(df$FrecuencyOfAlcoholConsumption)
mediaFAC=(df$AgeAtTimeOfAnalysis[1]-MaxFAC)/(MaxFAC-MinFAC)

MaxSI=max(df$SurgicalIntervention)
MinSI=min(df$SurgicalIntervention)
mediaSI=(df$SurgicalIntervention[1]-MaxSI)/(MaxSI-MinSI)

MaxNHS=max(df$NumberOfHRSittingPerDay)
MinNHS=min(df$NumberOfHRSittingPerDay)
mediaNHS=(df$NumberOfHRSittingPerDay[1]-MaxNHS)/(MaxNHS-MinNHS)

#Particionamiento
index=createDataPartition(df$Diagnosis,p=0.8,list = FALSE)
train=df[index, ]
test=df[-index, ]

#Graficas de bigotes
boxplot(df$SeasonInWhichAnalysisWasPermormed~df$SurgicalIntervention)
boxplot(df$AgeAtTimeOfAnalysis~df$AccidentOrTrauma)
boxplot(df$ChildishDiseases~df$AgeAtTimeOfAnalysis)
boxplot(df$AccidentOrTrauma~df$SurgicalIntervention)
boxplot(df$HighFeversInLastYear~df$FrecuencyOfAlcoholConsumption)
boxplot(df$HighFeversInLastYear~df$SurgicalIntervention)-
boxplot(df$SmokingHabit~df$FrecuencyOfAlcoholConsumption)
boxplot(df$SmokingHabit~df$SurgicalIntervention)
boxplot(df$NumberOfHRSittingPerDay~df$SmokingHabit)
boxplot(df$NumberOfHRSittingPerDay~df$SurgicalIntervention)
boxplot(df$NumberOfHRSittingPerDay~df$HighFeversInLastYear)
boxplot(df$NumberOfHRSittingPerDay~df$FrecuencyOfAlcoholConsumption)
boxplot(df)


information.gain(Diagnosis~.,data = train)

#Modelo
modelof = knn(train[-5],test[-5],train$Diagnosis, k=3)

sumary(modelof)

#Matriz de confusion
matriz=table(modelof,test$Diagnosis)

#Calcular precision
precision<-sum(diag(matriz))/sum(matriz)*100

