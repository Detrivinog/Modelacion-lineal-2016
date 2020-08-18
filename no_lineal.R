##################
#### No Lineal ###
##################
setwd("C:/Users/David Esteban/Universidad/Semestres/VII Semestre/Modelación lineal")

require(XLConnect)
wb = loadWorkbook("lola.xlsx")
datos= readWorksheet(wb, sheet = "Hoja1", header = TRUE)
head(datos)

with(datos,plot(x,y),ylim=c(0,30),xlim=c(0,15),from=0, to=15)
par(new=T)
curve(coe[1]-coe[2]*exp(-coe[3]*x),ylim=c(0,30),xlim=c(0,10),from = 0, to=10,xlab = "",ylab = "",axes=F,col="red")
par(new=T)
curve(25-24*exp(-1.5*x),ylim=c(0,30),xlim=c(0,10),from = 0, to=10,xlab = "",ylab = "",axes=F,col="blue")
par(new=T)
curve(-coe[2]*exp(-coe[3]*x),ylim=c(0,30),xlim=c(0,10),from = 0, to=30,xlab = "",ylab = "",axes=F,col="red")


mod<-nls(y~a-b*exp(-c*x),start=list(a=25,b=24,c=1.5),data=datos)
summary(mod)
coe<-coefficients(mod)

mod2<-nls(y~a-b*exp(-c*x),start=list(a=23,b=22,c=0.8),data=datos)
summary(mod2)
coe<-coefficients(mod)

range(datos$x)
summary(mod)
?plot
