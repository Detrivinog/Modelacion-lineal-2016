setwd("C:/Users/David Esteban/Universidad/Semestres/VII Semestre/Modelación lineal")

datos<-read.csv2("Anillos.csv")
datos$lugar=as.factor(datos$lugar)
mod1=lm(E~N+I(N^2),datos)
summary(mod1)

mod2<-lm(E~N+p1+p2,datos)
summary(mod2)

mod3<-lm(E~N+I(N^2)+p2,datos)
summary(mod3)

#Conclusiones
mod4<-lm(E~N+p2,datos)
summary(mod4)

mod5<-lm(E~N+lugar,datos)
summary(mod5)
anova(mod5)

mod6<-lm(E~N+p1+p3,datos)
summary(mod6)

mod7<-lm(E~I(N^2)+p2,datos)
summary(mod7)

with(datos,plot(N,E))
abline(mod7,col=)
abline(mod4, col="red")
abline(mod1,col="blue")
