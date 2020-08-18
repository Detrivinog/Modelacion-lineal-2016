#Verosimilid

# f(x1, x2,..., xn)=f(x1)*f(x2)...f(xn)=L
#Evaluar si y=b0+b1*t es un buen modelo  metodo de máxima verosimilitud. 
#Modelo pleno para comprar: 
# y=b0+bix+b2x^2+b3x3^2

y<-c(236,184,327,254,298,156)
t<-c(14,11,28,19,23,8)
t2<-t^2
t3<-t^3
t4<-t^4
t5<-t^5
data<-data.frame(y,t,t2,t3,t4,t5)

mpleno<-lm(y~t+t2+t3+t4+t5)

predict(mpleno)
L=prod(predict(mpleno)) #Verosimilitud
log(L)
-2*(log(L)-6)


mod1<-lm(y~t)
summary(mod1)
predict(mod1)
L1=prod(predict(mod1))
-2*(log(L1)-2)

AIC(mod1)
extractAIC(mod1)

#AIC = 2(ln verosimilitud - # párametros)
BIC(mod1)
