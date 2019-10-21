library(RWeka)
library(party)
library(caret)
library(rJava)
library(FSelector)
library(class)
library(dplyr)
set.seed(1)

#leer
df=read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data"),sep = ",", header = FALSE)
names(df)<-c("Gender","Age","Debt", "MaritalStatus", "BankCustomer", "EducationLevel", "Ethnicity", "YearsEmployed", "PriorDefault","Employed", "CreditScore", "DriversLicense", "Citizen", "ZipCode", "Income", "Approved");

#Eliminar atributos
df$Age<-NULL
df$Gender<-NULL
df$Ethnicity<-NULL
df$DriversLicense<-NULL
df$ZipCode<-NULL
df$Citizen<-NULL
df$Debt<-NULL
df$MaritalStatus<-NULL
#df$BankCustomer<-NULL
#df$EducationLevel<-NULL

#Convertir datos categoricos a numericos
df2=data.matrix(df)
dfca <- as.data.frame(df2)

#Normalizacion
MaxYE=max(dfca$YearsEmployed)
MinYE=min(dfca$YearsEmployed)
mediaYE=(dfca$YearsEmployed[1]-MaxYE)/(MaxYE-MinYE)

MaxPD=max(dfca$PriorDefault)
MinPD=min(dfca$PriorDefault)
mediaPD=(dfca$PriorDefault[1]-MaxPD)/(MaxPD-MinPD)

MaxE=max(dfca$Employed)
MinE=min(dfca$Employed)
mediaE=(dfca$Employed[1]-MaxE)/(MaxE-MinE)

MaxCS=max(dfca$CreditScore)
MinCS=min(dfca$CreditScore)
mediaCS=(dfca$CreditScore[1]-MaxCS)/(MaxCS-MinCS)

MaxI=max(dfca$Income)
MinI=min(dfca$Income)
mediaI=(dfca$Income[1]-MaxI)/(MaxI-MinI)

MaxA=max(dfca$Approved)
MinA=min(dfca$Approved)
mediaA=(dfca$Approved[1]-MaxA)/(MaxA-MinA)

#Graficas
boxplot(dfca$Gender~dfca$Age)
boxplot(dfca$Age~dfca$Debt)
boxplot(dfca$Debt~dfca$Gender)
boxplot(dfca$BankCustomer~dfca$Age)
boxplot(dfca$YearsEmployed~dfca$BankCustomer)
boxplot(dfca$PriorDefault~dfca$YearsEmployed)
boxplot(dfca$Employed~dfca$Income)
boxplot(dfca$Income~dfca$MaritalStatus)
boxplot(dfca$CreditScore~dfca$Ethnicity)
boxplot(dfca$MaritalStatus~dfca$PriorDefault)
boxplot(dfca$Ethnicity~dfca$EducationLevel)
boxplot(dfca$EducationLevel~dfca$Employed)
boxplot(dfca$DriversLicense~dfca$CreditScore)
boxplot(dfca$ZipCode~dfca$Citizen)
boxplot(dfca$Citizen~dfca$CreditScore)

#particionamiento
index=createDataPartition(dfca$Approved,p=0.8,list = FALSE)
train=dfca[index, ]
test=dfca[-index, ]

#entropia
freqsYE <- table(dfca$YeasrEmployed)/length(dfca$YeasrEmployed)
-sum(freqsYE * log2(freqsYE))

freqsPD <- table(dfca$PriorDefault)/length(dfca$PriorDefault)
-sum(freqsPD * log2(freqsPD))

freqsE <- table(dfca$Employed)/length(dfca$Employed)
-sum(freqsE * log2(freqsE))

freqsCS <- table(dfca$CreditScore)/length(dfca$CreditScore)
-sum(freqsCS * log2(freqsCS))

freqsI <- table(dfca$Income)/length(dfca$Income)
-sum(freqsI * log2(freqsI))

freqsA <- table(dfca$Approved)/length(dfca$Approved)
-sum(freqsA * log2(freqsA))

#ganancia de informacion
information.gain(Approved~.,data = train)

#Modelo
modelo=J48(as.factor(Approved)~.,data=train)

#matriz de confusion
prediccion_arbol <- predict(modelo, test[,-8], type = "class")
MC<-table(test$Approved,prediccion_arbol)

#Calcular precision
precision<-sum(diag(MC))/sum(MC)*100

#arbol
plot(modelo)


