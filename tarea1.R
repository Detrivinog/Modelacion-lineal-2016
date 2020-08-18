d<-c(20,20,20,20,30,30,30,30,40,40,40,40)
h<-c(18,18,24,24,18,18,24,24,18,18,24,24)
v<-c(101,105,118,127,142,138,151,149,216,202,225,238)
datos<-data.frame(d,h,v)
pairs(datos)
plot(d,v)
mod1<-lm(v~d+h)
summary(mod1)

#### Actualizar modelos ####   
?update   
mod2<-update(mod1, ~.-h)
summary(mod2)
mod3<-update(mod1, ~.-d)
summary(mod3)

### Ejercicio datos inventados 
set.seed(1)
y<-sort(rnorm(15, 4,1.1))
x1<-sort(rnorm(15,15,2))  #Ramas
x2<-rnorm(15,0.5,.3) #densidad
x3<-sort(rnorm(15, 62.4,7))

mod<-lm(y~x1+x2+x3)
summary(mod)

mod4<-update(mod, ~.-x2)
summary(mod4)

#Modelo datos desosrganizados + consideraciones previas 