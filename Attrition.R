attrition = Employee.Attrition

names(attrition)[1]<-"Age"

MaxAge=max(attrition$Age)
MinAge=min(attrition$Age)
MaxDR=max(attrition$DailyRate)
MinDR=min(attrition$DailyRate)
MaxDFH=max(attrition$DistanceFromHome)
MinDFH=min(attrition$DistanceFromHome)
MaxEducation=max(attrition$Education)
MinEducation=min(attrition$Education)

#Eliminar atributos que no se necesiten
attrition$EmployeeCount<-NULL
attrition$Over18<-NULL
attrition$StandardHours<-NULL
attrition$EmployeeNumber<-NULL
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
#Cambia atributos categoricos a numericos
attrition2=data.matrix(attrition)

#cambiar matriz a dataframe
attrition.df <- as.data.frame(attrition2)

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


index=createDataPartition(attrition.df$Attrition,p=0.7, list = FALSE)
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
  df_train=data.frame(train=c(train), d=c(d))
  df_order=df_train[order(df_train$d),]
  resultados=df_order$train.Attrition[1:3];
  
   return (resultados)
}