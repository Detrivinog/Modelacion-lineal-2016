setwd("C:/Users/David Esteban/Universidad/Semestres/VII Semestre/Modelación lineal")
library(car)

data<-read.csv2("dat1.csv")
with(data, plot(d,H))
mod1<-lm(H~d, data)
summary(mod1)
data$ei<-resid(mod1)

##Gráfica de los residuales modelo 1
with(data, plot(d,ei))
abline(h=0, col="blue")
abline(h=3,col="green")
abline(h=-3, col="green")
with(data, table(d))
xp=26

#### Modelo segmentado 
mod2=lm(H~d*(d<xp)+d*(d>xp), data)
summary(mod2)
data$e2<-resid(mod2)

#Gráfica de residuales
with(data, plot(d, e2))
abline(h=0, col="blue")
abline(h=3,col="green")
abline(h=-3, col="green")

data$d1=sort(unique(data$d))

#Gráfica de la polinomial segmentada 
with(data, plot(H~d))
lines(data$d,predict(mod2,list(d=data$d1)))
dia3=seq(1,range(data$d)[2], 1)
pred2=predict(mod2,list(d=dia3))
pred2
class(pred2)

durbinWatsonTest(mod1)
