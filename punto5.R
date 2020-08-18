setwd("C:/Users/David Esteban/Universidad/Semestres/VII Semestre/Modelación lineal")

require(XLConnect)
wb = loadWorkbook("Punto5.xlsx")
datos= readWorksheet(wb, sheet = "paralogistica", header = TRUE)
datos2= readWorksheet(wb, sheet = "P5", header = TRUE)


datos$P=datos$A/datos$N # Proporción
mod<-lm(P~D,datos)
summary(mod)

par(mfrow=c(2,2))
plot(mod)

datos$pi=with(datos, log(P/(1-P)))
mod1<-lm(pi~D,datos)
summary(mod1)

par(mfrow=c(2,2))
plot(mod1)

datos$w=with(datos, N*P*(1-P))
W=diag(datos$w)
X=cbind(1,datos$D)
Y=as.matrix(datos$pi)
B=solve(t(X)%*%W%*%X)%*%t(X)%*%W%*%Y


B
par(mfrow=c(1,1))
plot(datos$D,datos$P)

ml1=nls(P~(exp(a+b*D)/(1+exp(a+b*D))), data=datos, start = list(a=B[1,1],b=B[2,1]))
summary(ml1)
head(datos)
-(log(.1)/26)-(log((1/.2)-1)/26)

c=log((2/(20*.243))-(1/20))/-28
ml2=nls(P~(a/(1+b*exp(-c*D))), data=datos, start = list(a=2,b=20,c=c))
summary(ml2)


ml2=nls(P~(a/(1+b*exp(-c*D))), data=datos, start = list(a=1,b=10,c=0.0352))
summary(ml2)
mean(datos$D)
ml3=nls(P~(a+((b-a)/(1+exp(c*(d-D))))), data=datos, start = list(a=1,b=4,c=0.14,d=35))
summary(ml3)

datos2= readWorksheet(wb, sheet = "P5", header = TRUE)
with(datos2, plot(x1,y))
with(datos2, plot(x2,y))

datos2
with(datos2,tapply(y, t, mean))

datos2
pairs(datos2)
m1=lm(Y~x1+x2+I(N^2)+N,datos2)
summary(m1)

m2=update(m1,~.-x2)
summary(m2)
par(mfrow=c(2,2))
plot(m2)

m4=update(m2,~.-I(N^2))
summary(m4)

m3=update(m2,~.-x2)
summary(m3)


m5