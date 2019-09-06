#ejercicio1
boxplot(datos_fallecimientos[-1])

#ejercicio2
conteo=table(mtcars$gear)
barplot(conteo, main = "Distribucion de autos",xlab = "Numero de engranes")

#ejercicio3
conteo=table(mtcars$gear)
barplot(conteo, main = "Distrubucion de autos", horiz = TRUE, names.arg = c("3 engranes", "4 engranes", "5 engranes"))

#ejercicio4
hist(mtcars$mpg)

#lee data set desde pagina de UCI
data=read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer/breast-cancer.data"),sep = ",")

#Asigna nombres a las columnas
names(data)<-c("Class","Age","Menopause","Tumo size","inv nodes", "Node caps", "deg malig", "breast", "breast quad", "irradiat")

#Crea el particionamiento del train
index=createDataPartition(data$Class,p=0.7, list = FALSE)
train=data[index, ];
test=data[-index, ]

#gráfica de PIE
var=as.data.frame(table(train$Class))
labels <- c("Eventos de recurrencia", "Eventos de no recurrencia")
pie(var$Freq, main="Eventos", col=c("green","blue"), labels)

#gráfica de LINEA
plot(table(train$Age), type="l", main = "EDAD", col="orange", xlab="Edades",ylab="Numero de pacientes")

#gráfica de PUNTOS
plot(table(train$`Tumo size`),main="Tamaño Tumor",type="p",xlab = "tamaño", ylab = "Numero de pacientes", col="blue")

#gráfica de BARRAS ACUMULADAS
tab_breast <- table(train$`Node caps`, train$irradiat)
barplot(tab_breast, col = c("royalblue", "grey"),xlab="resultado", ylab = "Numero de pacientes",legend.text = c("irradiat", "node caps"))

#gráfica 3D
open3d()
plot3d(train$`Tumo size`, train$`deg malig`,train$`inv nodes`,col=rainbow(3), type = "s")
