library(neuralnet)
library(caret)

df=read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"),sep = ",", header = FALSE)
names(df)<-c("Class","Alcohol","MalicAcid", "Ash", "AlcalinityOfAsh", "Magnesium", "TotalPhenols", "Flavanoids", "NonflavanoidPhenols", "Proanthocyanins", "ColorIntensity", "Hue", "OD280/OD315OfDilutedWines", "Proline")


#Particionamiento
index=createDataPartition(df$Class,p=0.8, list = FALSE)
train=df[index, ];
test=df[-index, ]

modelo = neuralnet(Class~ Alcohol + MalicAcid + Ash, data=train)

plot(modelo)