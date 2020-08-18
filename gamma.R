setwd("C:/Users/David Esteban/Universidad/Semestres/VII Semestre/Modelación lineal")
?gamma
l=seq(0,70,10)
u=seq(10,80,10)
f=c(245,187,134,65,54,35,12,3)
datos=data.frame(l,u,f)
datos$c=seq(5,75,10)

datos$xm=with(datos,f*c)
write.csv2(datos,"gamma.csv")
media=sum(datos$xm)/sum(datos$f)

varianza=with(datos,sum(f*((c-media)^2))/(sum(f)-1))
gamma(datos$f)
#scale=B shape=a
progam<-dgamma(datos$u,scale=B, shape=a)
B=varianza/media
a=media/B

estimados<-progam*sum(datos$f)
#calculados sum(obs-est)^2/e
chisq.test
