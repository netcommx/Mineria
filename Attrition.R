attrition = Employee.Attrition

names(attrition)[1]<-"Age"


#Cambia atributos categoricos a numericos
attrition2=data.matrix(attrition)

#cambiar matriz a dataframe
attrition.df <- as.data.frame(attrition2)

#Eliminar atributos que no se necesiten
attrition.df$EmployeeCount<-NULL
attrition.df$Over18<-NULL
attrition.df$StandardHours<-NULL
attrition.df$EmployeeNumber<-NULL
attrition.df$JobRole<-NULL
attrition.df$JobInvolvement<-NULL
attrition.df$MaritalStatus<-NULL
attrition.df$Department<-NULL
attrition.df$Education<-NULL
attrition.df$EducationField<-NULL
attrition.df$Gender<-NULL
attrition.df$PerformanceRating<-NULL
attrition.df$RelationshipSatisfaction<-NULL
attrition.df$StockOptionLevel<-NULL
attrition.df$TrainingTimesLastYear<-NULL
attrition.df$WorkLifeBalance<-NULL
attrition.df$BusinessTravel<-NULL

#Normalizacion
MaxAge=max(attrition.df$Age)
MinAge=min(attrition.df$Age)
mediaAge=(attrition.df$Age[1]-MaxAge)/(MaxAge-MinAge)
MaxDR=max(attrition.df$DailyRate)
MinDR=min(attrition.df$DailyRate)
mediaDR=(attrition.df$DailyRate[1]-MaxDR)/(MaxDR-MinDR)
MaxDFH=max(attrition.df$DistanceFromHome)
MinDFH=min(attrition.df$DistanceFromHome)
mediaDFH=(attrition.df$DistanceFromHome[1]-MaxDFH)/(MaxDFH-MinDFH)
MaxES=max(attrition.df$EnvironmentSatisfaction)
MinES=min(attrition.df$EnvironmentSatisfaction)
mediaES=(attrition.df$EnvironmentSatisfaction[1]-MaxES)/(MaxES-MinES)
MaxHR=max(attrition.df$HourlyRate)
MinHR=min(attrition.df$HourlyRate)
mediaHR=(attrition.df$HourlyRate[1]-MaxHR)/(MaxHR-MinHR)
MaxJL=max(attrition.df$JobLevel)
MinJL=min(attrition.df$JobLevel)
mediaJL=(attrition.df$JobLevel[1]-MaxJL)/(MaxJL-MinJL)
MaxJS=max(attrition.df$JobSatisfaction)
MinJS=min(attrition.df$JobSatisfaction)
mediaJS=(attrition.df$JobSatisfaction[1]-MaxJS)/(MaxJS-MinJS)
MaxMI=max(attrition.df$MonthlyIncome)
MinMI=min(attrition.df$MonthlyIncome)
mediaMI=(attrition.df$MonthlyIncome[1]-MaxMI)/(MaxMI-MinMI)
MaxMR=max(attrition.df$MonthlyRate)
MinMR=min(attrition.df$MonthlyRate)
mediaMR=(attrition.df$MonthlyRate[1]-MaxMR)/(MaxMR-MinMR)
MaxNCW=max(attrition.df$NumCompaniesWorked)
MinNCW=min(attrition.df$NumCompaniesWorked)
mediaNCW=(attrition.df$NumCompaniesWorked[1]-MaxNCW)/(MaxNCW-MinNCW)
MaxO=max(attrition.df$OverTime)
MinO=min(attrition.df$OverTime)
mediaO=(attrition.df$OverTime[1]-MaxO)/(MaxO-MinO)
MaxPSH=max(attrition.df$PercentSalaryHike)
MinPSH=min(attrition.df$PercentSalaryHike)
mediaPSH=(attrition.df$PercentSalaryHike[1]-MaxPSH)/(MaxPSH-MinPSH)
MaxTWY=max(attrition.df$TotalWorkingYears)
MinTWY=min(attrition.df$TotalWorkingYears)
mediaTWY=(attrition.df$TotalWorkingYears[1]-MaxTWY)/(MaxTWY-MinTWY)
MaxYAC=max(attrition.df$YearsAtCompany)
MinYAC=min(attrition.df$YearsAtCompany)
mediaYAC=(attrition.df$YearsAtCompany[1]-MaxYAC)/(MaxYAC-MinYAC)
MaxYCR =max(attrition.df$YearsInCurrentRole)
MinYCR=min(attrition.df$YearsInCurrentRole)
mediaYCR=(attrition.df$YearsInCurrentRole[1]-MaxYCR)/(MaxYCR-MinYCR)
MaxYLP=max(attrition.df$YearsSinceLastPromotion)
MinYLP=min(attrition.df$YearsSinceLastPromotion)
mediaYLP=(attrition.df$YearsSinceLastPromotion[1]-MaxYLP)/(MaxYLP-MinYLP)
MaxYCM=max(attrition.df$YearsWithCurrManager)
MinYCM=min(attrition.df$YearsWithCurrManager)
mediaYCM=(attrition.df$YearsWithCurrManager[1]-MaxYCM)/(MaxYCM-MinYCM)

#graficas de atributos
boxplot(attrition.df$Age~attrition.df$Attrition)
boxplot(attrition.df$BusinessTravel~attrition.df$Attrition)-
boxplot(attrition.df$Attrition~attrition.df$Attrition)
boxplot(attrition.df$DailyRate~attrition.df$Attrition)
boxplot(attrition.df$Department~attrition.df$Attrition)-
boxplot(attrition.df$DistanceFromHome~attrition.df$Attrition)
boxplot(attrition.df$Education~attrition.df$Attrition)
boxplot(attrition.df$EducationField~attrition.df$Attrition)-
boxplot(attrition.df$EnvironmentSatisfaction~attrition.df$Attrition)
boxplot(attrition.df$Gender~attrition.df$Attrition)-
boxplot(attrition.df$HourlyRate~attrition.df$Attrition)
boxplot(attrition.df$JobLevel~attrition.df$Attrition)
boxplot(attrition.df$JobSatisfaction~attrition.df$Attrition)
boxplot(attrition.df$MonthlyIncome~attrition.df$Attrition)
boxplot(attrition.df$MonthlyRate~attrition.df$Attrition)
boxplot(attrition.df$NumCompaniesWorked)
boxplot(attrition.df$OverTime)
boxplot(attrition.df$PercentSalaryHike)
boxplot(attrition.df$PerformanceRating)-
boxplot(attrition.df$RelationshipSatisfaction)-
boxplot(attrition.df$StockOptionLevel)-
boxplot(attrition.df$TotalWorkingYears)
boxplot(attrition.df$TrainingTimesLastYear)-
boxplot(attrition.df$WorkLifeBalance)-
boxplot(attrition.df$YearsAtCompany)
boxplot(attrition.df$YearsInCurrentRole)
boxplot(attrition.df$YearsSinceLastPromotion)
boxplot(attrition.df$YearsWithCurrManager)

#particion de train y test
index=createDataPartition(attrition.df$Attrition,p=0.8, list = FALSE)
train=attrition.df[index, ];
test=attrition.df[-index, ]

#distancia manhattan
knn_attrition = function(train, test, k){
  d= abs(train$Age-test$Age[1]) + abs(train$DailyRate-test$DailyRate[1]) + abs(train$DistanceFromHome-test$DistanceFromHome[1]) + 
    abs(train$EnvironmentSatisfaction-test$EnvironmentSatisfaction[1]) + abs(train$HourlyRate-test$HourlyRate[1]) + abs(train$JobLevel-test$JobLevel[1]) + 
    abs(train$JobSatisfaction-test$JobSatisfaction[1]) + abs(train$MonthlyIncome-test$MonthlyIncome[1]) + abs(train$MonthlyRate-test$MonthlyRate[1]) + 
    abs(train$NumCompaniesWorked-test$NumCompaniesWorked[1]) + abs(train$OverTime-test$OverTime[1]) + abs(train$PercentSalaryHike-test$PercentSalaryHike[1]) + 
    abs(train$TotalWorkingYears-test$TotalWorkingYears[1]) + abs(train$YearsAtCompany-test$YearsAtCompany[1]) + abs(train$YearsInCurrentRole-test$YearsInCurrentRole[1]) + 
    abs(train$YearsSinceLastPromotion-test$YearsSinceLastPromotion[1]) + abs(train$YearsWithCurrManager-test$YearsWithCurrManager[1])
   #ordenar
  df_train=data.frame(train=c(train), d=c(d));
  df_order=df_train[order(df_train$d),];
  resultados=df_order$train.Attrition[1]
  return (resultados)
}

#distancia euclidean
knn_attr = function(train, test, k){
de=(sqrt(train$Age-test$Age[1])^2 + (train$DailyRate-test$DailyRate[1])^2 + (train$DistanceFromHome-test$DistanceFromHome[1])^2 + 
  (train$EnvironmentSatisfaction-test$EnvironmentSatisfaction[1])^2 + (train$HourlyRate-test$HourlyRate[1])^2 + abs(train$JobLevel-test$JobLevel[1])^2 + 
  (train$JobSatisfaction-test$JobSatisfaction[1])^2 + (train$MonthlyIncome-test$MonthlyIncome[1])^2 + (train$MonthlyRate-test$MonthlyRate[1])^2 + 
  (train$NumCompaniesWorked-test$NumCompaniesWorked[1])^2 + (train$OverTime-test$OverTime[1])^2 + (train$PercentSalaryHike-test$PercentSalaryHike[1])^2 + 
  (train$TotalWorkingYears-test$TotalWorkingYears[1])^2 + (train$YearsAtCompany-test$YearsAtCompany[1])^2 + (train$YearsInCurrentRole-test$YearsInCurrentRole[1])^2 + 
  (train$YearsSinceLastPromotion-test$YearsSinceLastPromotion[1])^2 + (train$YearsWithCurrManager-test$YearsWithCurrManager[1])^2)
return(de)
}

