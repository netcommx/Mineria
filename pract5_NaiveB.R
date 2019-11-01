library(e1071)
library(caret)

set.seed(1)
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

df$Disciplinary.failure[which(df$Disciplinary.failure== 0)] <- 2
df$Disciplinary.failure[which(df$Disciplinary.failure== "NA")] <- 1
df$Disciplinary.failure[which(df$Disciplinary.failure== '?')] <- 1
df$Distance.from.Residence.to.Work[which(df$Distance.from.Residence.to.Work== 0)] <- 2
df$Distance.from.Residence.to.Work[which(df$Distance.from.Residence.to.Work== "NA")] <- 1
df$Distance.from.Residence.to.Work[which(df$Distance.from.Residence.to.Work== '?')] <- 1
df$Absenteeism.time.in.hours[which(df$Absenteeism.time.in.hours== 0)] <- 2
df$Absenteeism.time.in.hours[which(df$Absenteeism.time.in.hours== "NA")] <- 1
df$Absenteeism.time.in.hours[which(df$Absenteeism.time.in.hours== '?')] <- 1
df$Reason.for.absence[which(df$Reason.for.absence== 0)] <- 2
df$Reason.for.absence[which(df$Reason.for.absence== "NA")] <- 1
df$Reason.for.absence[which(df$Reason.for.absence== '?')] <- 1

#Modelo
modelo = naiveBayes(as.factor(Reason.for.absence)~.,train)
#modelo=naiveBayes(as.factor(Reason.for.absence) ~., data=train)

prediccion=predict(modelo,train$Reason.for.absence)
#pred=predict(modelo,train)
table(pred,df$Reason.for.absence)

#Matriz de confusion
matriz = table(prediccion,train$Reason.for.absence)

#Precision
precision<-sum(diag(matriz))/sum(matriz)*100
