#Dataset Iris
datos=iris
index=createDataPartition(iris$Species,p=0.8,list = FALSE)
train=datos[index, ]
test=datos[-index, ]

knn=function(train,test,k){
  #similitud
  dm=abs(test$Petal.Length[54]-train$Petal.Length)+abs(test$Petal.Width[54]-train$Petal.Width)
    +abs(test$Sepal.Length[54]-train$Sepal.Length)+abs(test$Sepal.Width[54]-train$Sepal.Width)
#return(dm)
  #ordenar
  ttrain=data.frame(train=c(train), dm=c(dm))
  
  torder=ttrain[order(ttrain$dm),]
  
  #Clase
  resultados=torder$train.Species[1:4]
  
  #muestra el de mayor frecuencia
  r=as.data.frame(table(resultados))
  #ejecutar distancias$resultados[1]
  #return(r)
  #ordena de mayor a menor
  rtorder=r[order(r$Freq, decreasing = TRUE),];
  return(rtorder)
 # return(View(r$resultados[1]));
  }

#correr funcion> distancias=knn(train,test,4), View(distancias)


                   