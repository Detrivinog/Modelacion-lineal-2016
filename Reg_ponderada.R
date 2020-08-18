setwd("C:/Users/David Esteban/Universidad/Semestres/VII Semestre/Modelación lineal")

datos<-read.csv2("dat3.csv")

mod1<-lm(est~edad,datos)
summary(mod1)

with(datos, plot(edad,est))
abline(mod1, col="red")

#Como los estimados no son significativos: entonces se ponderan los datos, para corregir los términos del error. 
# W=1/(pi*qi)
# W=1/(E(yi)*1-E(yi))

plot(mod1)

c=data.frame(edad=1:30)
predict(mod1, c)
datos$e1=resid(mod1)
with(datos, plot(e1, est))

datos$yi=predict(mod1)
datos$wi=with(datos, 1/(yi*(1-yi)))
datos$Yw=with(datos)

W=%*%datos$wi
?I
#POnderada
# y=0.0367+0.021117x
