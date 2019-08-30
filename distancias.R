
names=c("amy","bill","jim");
sc=c(5,2,1);
dt=c(5,5,4);
w=c(3,0,0);
wk=c(4,0,0);
ck=c(5,0,0);

mrx_sc=4;
mrx_dt=2;

#Distancia euclidian
de=function(names,sc,dt,mrx_sc,mrx_dt){
  d=sqrt((mrx_sc-sc)^2+(mrx_dt-dt)^2);
  pos=which.min(d);
  return(names[pos])
}
#Distancia manhattan
dm=function(names,sc,dt,mrx_sc,mrx_dt){
  m=abs(mrx_sc-sc);
  m2=abs(mrx_dt-dt);
  pos=which.min(m);
  return(names[pos])
}

