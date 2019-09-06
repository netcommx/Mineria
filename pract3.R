boxplot(datos_fallecimientos[-1])

conteo=table(mtcars$gear)
barplot(conteo, main = "Distribucion de autos",xlab = "Numero de engranes")

conteo=table(mtcars$gear)
barplot(conteo, main = "Distrubucion de autos", horiz = TRUE, names.arg = c("3 engranes", "4 engranes", "5 engranes"))

hist(mtcars$mpg)

data=read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer/breast-cancer.data"),sep = ",")

names(data)<-c("Class","Age","Menopause","Tumo size","inv nodes", "Node caps", "deg malig", "breast", "breast quad", "irradiat")

index=createDataPartition(data$Class,p=0.7, list = FALSE)
train=data[index, ];
test=data[-index, ]

##pie(train$Cl)
var=as.data.frame(table(train$Class))

pie(var$Freq)

plot(table(train$Age), type="l", main = "EDAD", col="orange", xlab="Edades",ylab="Numero de pacientes")

plot(table(train$`Tumo size`),type="p")







