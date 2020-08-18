setwd("C:/Users/David Esteban/Universidad/Semestres/VII Semestre/Modelación lineal")
require(XLConnect)
wb = loadWorkbook("punto6.xlsx")
datos= readWorksheet(wb, sheet = "Hoja1", header = TRUE)
datos=datos[-27,]
datos$d2h=with(datos, (D^2)*H)
cor1<-cor(datos)

#Función de intervalos de confianza

IC=function(x){
  zp=function(x){
    z=0.5*log((1+x)/(1-x))
    return(z)
  }
  vz=sqrt(1/(30-3))
  t=1.96
  IC=zp(x)+c(-1,1)*t*vz
  return(IC)
}
class(cor1)
r=c(cor1[1,2],cor1[2,3],cor1[2,4])
r1=IC(r[1])
r2=IC(r[2])
r3=IC(r[3])

#Funcion invrersa de R
fi<-function(x){
  r=(exp(2*x)-1)/(exp(2*x)+1)
  return(r)
}
fi(r1)
fi(r2)
fi(r3)


tcor=cbind(r,rbind(fi(r1),fi(r2),fi(r2)))
colnames(tcor)=c("Estimado","Límite inferior","Límite superior")

#Modelo lineal 
pairs(datos)
datos$dh=datos$H*datos$D
mod1=lm(V~D+H+dh+d2h,datos)
summary(mod1)

mod2<-update(mod1, ~.-dh)
summary(mod2)

mod3<-update(mod2,~.-H)
summary(mod3)

mod4<-update(mod3,~.-D)
summary(mod4)

with(datos,plot(datos))
mod5<-lm(log(V)~log(D)+log(H),datos)
summary(mod5)

anova(mod2)
intercepto=exp(coefficients(mod2)[1]+(0.0025/2))

datos$Vc=with(datos, (1/40000)*H*pi*(D^2))
datos$ff=with(datos,V/Vc)

mod6=lm(ff~D+V+d2h+dh+H,datos)
summary(mod6)

mod7=lm(ff~V+d2h+D+H,datos)
summary(mod7)

mod8=lm(ff~V+d2h+D,datos)
summary(mod8)
  
mod9=lm(ff~d2h+V,datos)
summary(mod9)

