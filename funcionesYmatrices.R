
#vector
a=c(2,4,3,8,7);
b=c(2,4,3,8,7,7,7);

#funciones
se=seq(1,6);
re= rep(1,5);

#regresavalor indicado
w=which(a==4);

#regresa valor minimo
wmin=which.min(a);

#regresa valor maxico
wmax=which.max(a);

#crea grafico basado en plano cartesiano con los valores dentro de los parametros
x=5;
y=6;
plot(x,y)

#ordena de menor a mayor
so=sort(a);

#muestra vector sin valores repetidos
u=unique(b);

#suma valores de vector
suma=sum(a);

#matrices
#creando vectores
mat1<-c(1,2,3)
mat2<-c(4,5,6)
mat3<-c(8,9,10)
#agregandoselo a variable creando matriz
mat<-c(mat1,mat2,mat3)
mat

#utilizando cbind para agregar vectores
matr<-cbind(mat1,mat2,mat3)
matr

#creando matriz usando funcion matriz dando valores y especificaciones
matrx<-matrix(mat,ncol = 3,byrow = F)
matrx


