
#Similitud 
ar=c("bt","bb","d","nj","p","ss","ts","vw");
a=c(3.5,2,0,4.5,5,1.5,2.5,2);
b=c(2,3.5,4,0,2,3.5,0,3);
c=c(5,1,1,3,5,1,0,0);
h=c(0,4,1,4,0,0,4,1);
j=c(0,4.5,4,5,5,4.5,4,4);
s=c(5,2,0,3,5,4,5,0);
v=c(3,0,0,5,4,2.5,3,0);

#se crea dataframe con los artistas
datos=data.frame(ar=c(ar),a=c(a),b=c(b),c=c(c),h=c(h),j=c(j),s=c(s),v=c(v));
#Dataframe de los artistas que va a recibir la funcion
dft=data.frame(ar=c(ar),s=c(s),c=c(c));

#funcion recibe artistas y 
similitud=function(dft,a){
  dw=abs(a-dft$c)+abs(a-dft$s);
  return(dw);
}