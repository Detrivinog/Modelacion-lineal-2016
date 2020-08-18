setwd("C:/Users/David Esteban/Universidad/Semestres/VII Semestre/Modelación lineal")
require(leaps)
data<-read.csv2("dat2.csv") # y, x1= edad, x2=indice de manejo , x3= facilidad de manejo

leaps(x=data[,2:4], y=data[,1], names=names(data)[2:4], method="Cp") # Cp de Mallow
leaps(x=data[,2:4], y=data[,1], names=names(data)[2:4], method="r2") # R2p 
leaps(x=data[,2:4], y=data[,1], names=names(data)[2:4], method="adjr2") #R2 ajustado
data

mod1<-lm(y~x3, data)
summary(mod1)
extractAIC(mod1)
AIC(mod1)

# Cual es la diferencia de AIC y extractAIC
?extractAIC
?AIC

modgen<-lm(y~x1+x2+x3,data)
extractAIC(modgen)
AIC(modgen)

#####################################
####### Modelo stepwise forward #####
#####################################

m.step1<-step(mod1, scope=list(upper=modgen, lower=~1),direction = "forward",trace = T) 
extractAIC(m.step1)
summary(m.step1)

mod2<-update(m.step1,~.-x2)
summary(mod2)

mod3<-lm(y~x2,data)
summary(mod3)

?step

#####################################
####### Modelo stepwise backward ####
#####################################

m.step2<-step(modgen,direction = "backward",trace = T) # si trace=T permite ver los AIC con cada variable 
extractAIC(m.step2)
summary(m.step2)

###################
####### PRESS ####
##################

sum((mod$residuals/(1-hatvalues(mod)))^2)   #El menor valor es el mas atractivo 




