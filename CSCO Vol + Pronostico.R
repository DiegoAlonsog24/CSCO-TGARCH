library(xts)
library(zoo)
library(astsa)
library(TTR)
library(tseries)
library(fGarch)
library(lmtest)
library(sandwich)
library(quantmod)
library(rugarch)

#preparo mis datos
getSymbols("CSCO")
getSymbols("CSCO", from='2019-01-01',to="2024-05-12")
CSCO<-ts(Cl(CSCO))
CSCO
plot.ts(CSCO, main="CSCO")
length(CSCO)

difCSCO<-diff(log(CSCO))
plot.ts(difCSCO, main="CSCO")
length(difCSCO)

adf.test(CSCO) # Tiene al menos una raíz unitaria
adf.test(difCSCO) # Rechazo, es estacionaria

#no es ruido blanco la serie diaria
acf(difCSCO,length(difCSCO)^.5,na.action = na.pass)
pacf(difCSCO,length(difCSCO)^.5,na.action = na.pass)
Box.test(difCSCO, lag =length(difCSCO)^.5, type = c("Box-Pierce"))
Box.test(difCSCO, lag =length(difCSCO)^.5, type = c("Ljung-Box"))



arma707<-arima(difCSCO,order =c(1,0,0),n.cond=6)
ar2<-arima(difCSCO,order =c(2,0,0),n.cond=5)
ar3<-arima(difCSCO,order =c(3,0,0),n.cond=4)
ar4<-arima(difCSCO,order =c(4,0,0),n.cond=3)
ar5<-arima(difCSCO,order =c(5,0,0),n.cond=2)
ar6<-arima(difCSCO,order =c(6,0,0),n.cond=1)
ar7<-arima(difCSCO,order =c(7,0,0))


#ARMA(0,q) con el n.con para que sea comparable el numero de obs 
ma1<-arima(difCSCO,order =c(0,0,1),n.cond=6)
ma2<-arima(difCSCO,order =c(0,0,2),n.cond=5)
ma3<-arima(difCSCO,order =c(0,0,3),n.cond=4)
ma4<-arima(difCSCO,order =c(0,0,4),n.cond=3)
ma5<-arima(difCSCO,order =c(0,0,5),n.cond=2)
ma6<-arima(difCSCO,order =c(0,0,6),n.cond=1)
ma7<-arima(difCSCO,order =c(0,0,7))

arma101<-arima(difCSCO,order =c(1,0,1),n.cond=6)
arma102<-arima(difCSCO,order =c(1,0,2),n.cond=5)
arma103<-arima(difCSCO,order =c(1,0,3),n.cond=4)
arma104<-arima(difCSCO,order =c(1,0,4),n.cond=3)
arma105<-arima(difCSCO,order =c(1,0,5),n.cond=2)
arma106<-arima(difCSCO,order =c(1,0,6),n.cond=1)
arma107<-arima(difCSCO,order =c(1,0,7))



arma201<-arima(difCSCO,order =c(2,0,1),n.cond=6)
arma202<-arima(difCSCO,order =c(2,0,2),n.cond=5)
arma203<-arima(difCSCO,order =c(2,0,3),n.cond=4)
arma204<-arima(difCSCO,order =c(2,0,4),n.cond=3)
arma205<-arima(difCSCO,order =c(2,0,5),n.cond=2)
arma206<-arima(difCSCO,order =c(2,0,6),n.cond=1)
arma207<-arima(difCSCO,order =c(2,0,7))


arma301<-arima(difCSCO,order =c(3,0,1),n.cond=6)
arma302<-arima(difCSCO,order =c(3,0,2),n.cond=5)
arma303<-arima(difCSCO,order =c(3,0,3),n.cond=4)
arma304<-arima(difCSCO,order =c(3,0,4),n.cond=3)
arma305<-arima(difCSCO,order =c(3,0,5),n.cond=2)
arma306<-arima(difCSCO,order =c(3,0,6),n.cond=1)
arma307<-arima(difCSCO,order =c(3,0,7))

arma401<-arima(difCSCO,order =c(4,0,1),n.cond=6)
arma402<-arima(difCSCO,order =c(4,0,2),n.cond=5)
arma403<-arima(difCSCO,order =c(4,0,3),n.cond=4)
arma404<-arima(difCSCO,order =c(4,0,4),n.cond=3)
arma405<-arima(difCSCO,order =c(4,0,5),n.cond=2)
arma406<-arima(difCSCO,order =c(4,0,6),n.cond=1)
arma407<-arima(difCSCO,order =c(4,0,7))


arma501<-arima(difCSCO,order =c(5,0,1),n.cond=6)
arma502<-arima(difCSCO,order =c(5,0,2),n.cond=5)
arma503<-arima(difCSCO,order =c(5,0,3),n.cond=4)
arma504<-arima(difCSCO,order =c(5,0,4),n.cond=3)
arma505<-arima(difCSCO,order =c(5,0,5),n.cond=2)
arma506<-arima(difCSCO,order =c(5,0,6),n.cond=1)
arma507<-arima(difCSCO,order =c(5,0,7))

arma601<-arima(difCSCO,order =c(6,0,1),n.cond=6)
arma602<-arima(difCSCO,order =c(6,0,2),n.cond=5)
arma603<-arima(difCSCO,order =c(6,0,3),n.cond=4)
arma604<-arima(difCSCO,order =c(6,0,4),n.cond=3)
arma605<-arima(difCSCO,order =c(6,0,5),n.cond=2)
arma606<-arima(difCSCO,order =c(6,0,6),n.cond=1)
arma607<-arima(difCSCO,order =c(6,0,7))

arma701<-arima(difCSCO,order =c(7,0,1),n.cond=6)
arma702<-arima(difCSCO,order =c(7,0,2),n.cond=5)
arma703<-arima(difCSCO,order =c(7,0,3),n.cond=4)
arma704<-arima(difCSCO,order =c(7,0,4),n.cond=3)
arma705<-arima(difCSCO,order =c(7,0,5),n.cond=2)
arma706<-arima(difCSCO,order =c(7,0,6),n.cond=1)
arma707<-arima(difCSCO,order =c(7,0,7))


#BIC
BIC(arma707)
BIC(ar2)
BIC(ar3)
BIC(ar4)
BIC(ar5)
BIC(ar6)
BIC(ar7)


BIC(ma1)
BIC(ma2)
BIC(ma3)
BIC(ma4)
BIC(ma5)
BIC(ma6)
BIC(ma7)

BIC(arma101)
BIC(arma102)
BIC(arma103)
BIC(arma104)
BIC(arma105)
BIC(arma106)
BIC(arma107)


BIC(arma201)
BIC(arma202)
BIC(arma203)
BIC(arma204)
BIC(arma205)
BIC(arma206)
BIC(arma207)


BIC(arma301)
BIC(arma302)
BIC(arma303)
BIC(arma304)
BIC(arma305)
BIC(arma306)
BIC(arma307)


BIC(arma401)
BIC(arma402)
BIC(arma403)
BIC(arma404)
BIC(arma405)
BIC(arma406)
BIC(arma407)


BIC(arma501)
BIC(arma502)
BIC(arma503)
BIC(arma504)
BIC(arma505)
BIC(arma506)
BIC(arma507)


BIC(arma601)
BIC(arma602)
BIC(arma603)
BIC(arma604)
BIC(arma605)
BIC(arma606)
BIC(arma607)


BIC(arma701)
BIC(arma702)
BIC(arma703)
BIC(arma704)
BIC(arma705)
BIC(arma706)
BIC(arma707)


#gana el BIC(arma707)-16365.27
arma707<-arima(difCSCO,order =c(7,0,7))
arma707
coeftest(ar)

residualesarma<-residuals(arma707)
plot.ts(residualesarma)
acf(residualesarma,na.action=na.pass)
pacf(residualesarma,na.action=na.pass)
Box.test(residualesarma, lag =length(residualesarma)^.5 , type = c("Box-Pierce"))
Box.test(residualesarma, lag =length(residualesarma)^.5 , type = c("Ljung-Box"))

#Ruido blanco


residualesarma2<-residualesarma^2
plot.ts(residualesarma2, main="Squared residuals of ARMA")

# Calculamos los residuales al cuadrado porque según la nota nos permite conocer la persistencia. 
# Nos permite ver los periodos de alta y baja volatilidad. Observamos los correlogramas y si hay persistencia lo comprobamos con la gráfica de autocorrelación 
acf(residualesarma2,length(residualesarma2)^.5,na.action = na.pass)
pacf(residualesarma2,length(residualesarma2)^.5,na.action = na.pass)

#GARCH 1,0 es el ARCH 1 

#garch
garch10 <- garchFit(residualesarma~garch(1,0), data = residualesarma)
garch20 <- garchFit(residualesarma~garch(2,0), data = residualesarma)
garch30 <- garchFit(residualesarma~garch(3,0), data = residualesarma)
garch40 <- garchFit(residualesarma~garch(4,0), data = residualesarma)
garch50 <- garchFit(residualesarma~garch(5,0), data = residualesarma)
garch60 <- garchFit(residualesarma~garch(6,0), data = residualesarma)
garch70 <- garchFit(residualesarma~garch(7,0), data = residualesarma)

garch11 <- garchFit(residualesarma~garch(1,1), data = residualesarma)
garch22 <- garchFit(residualesarma~garch(2,2), data = residualesarma)
garch33 <- garchFit(residualesarma~garch(3,3), data = residualesarma)
garch44 <- garchFit(residualesarma~garch(4,4), data = residualesarma)
garch55 <- garchFit(residualesarma~garch(5,5), data = residualesarma)
garch66 <- garchFit(residualesarma~garch(6,6), data = residualesarma)
garch77 <- garchFit(residualesarma~garch(7,7), data = residualesarma)

garch12 <- garchFit(residualesarma~garch(1,2), data = residualesarma)
garch13 <- garchFit(residualesarma~garch(1,3), data = residualesarma)
garch14 <- garchFit(residualesarma~garch(1,4), data = residualesarma)
garch15 <- garchFit(residualesarma~garch(1,5), data = residualesarma)
garch16 <- garchFit(residualesarma~garch(1,6), data = residualesarma)
garch17 <- garchFit(residualesarma~garch(1,7), data = residualesarma)

garch21 <- garchFit(residualesarma~garch(2,1), data = residualesarma)
garch23 <- garchFit(residualesarma~garch(2,3), data = residualesarma)
garch24 <- garchFit(residualesarma~garch(2,4), data = residualesarma)
garch25 <- garchFit(residualesarma~garch(2,5), data = residualesarma)
garch26 <- garchFit(residualesarma~garch(2,6), data = residualesarma)
garch27 <- garchFit(residualesarma~garch(2,7), data = residualesarma)

garch31 <- garchFit(residualesarma~garch(3,1), data = residualesarma)
garch32 <- garchFit(residualesarma~garch(3,2), data = residualesarma)
garch34 <- garchFit(residualesarma~garch(3,4), data = residualesarma)
garch35 <- garchFit(residualesarma~garch(3,5), data = residualesarma)
garch36 <- garchFit(residualesarma~garch(3,6), data = residualesarma)
garch37 <- garchFit(residualesarma~garch(3,7), data = residualesarma)

garch41 <- garchFit(residualesarma~garch(4,1), data = residualesarma)
garch42 <- garchFit(residualesarma~garch(4,2), data = residualesarma)
garch43 <- garchFit(residualesarma~garch(4,3), data = residualesarma)
garch45 <- garchFit(residualesarma~garch(4,5), data = residualesarma)
garch46 <- garchFit(residualesarma~garch(4,6), data = residualesarma)
garch47 <- garchFit(residualesarma~garch(4,7), data = residualesarma)

garch51 <- garchFit(residualesarma~garch(5,1), data = residualesarma)
garch52 <- garchFit(residualesarma~garch(5,2), data = residualesarma)
garch53 <- garchFit(residualesarma~garch(5,3), data = residualesarma)
garch54 <- garchFit(residualesarma~garch(5,4), data = residualesarma)
garch56 <- garchFit(residualesarma~garch(5,6), data = residualesarma)
garch57 <- garchFit(residualesarma~garch(5,7), data = residualesarma)

garch61 <- garchFit(residualesarma~garch(6,1), data = residualesarma)
garch62 <- garchFit(residualesarma~garch(6,2), data = residualesarma)
garch63 <- garchFit(residualesarma~garch(6,3), data = residualesarma)
garch64 <- garchFit(residualesarma~garch(6,4), data = residualesarma)
garch65 <- garchFit(residualesarma~garch(6,5), data = residualesarma)
garch67 <- garchFit(residualesarma~garch(6,7), data = residualesarma)

garch71 <- garchFit(residualesarma~garch(7,1), data = residualesarma)
garch72 <- garchFit(residualesarma~garch(7,2), data = residualesarma)
garch73 <- garchFit(residualesarma~garch(7,3), data = residualesarma)
garch74 <- garchFit(residualesarma~garch(7,4), data = residualesarma)
garch75 <- garchFit(residualesarma~garch(7,5), data = residualesarma)
garch76 <- garchFit(residualesarma~garch(7,6), data = residualesarma)

summary(garch10)
summary(garch20)
summary(garch30)
summary(garch40)
summary(garch50)
summary(garch60)
summary(garch70)

summary(garch11)
summary(garch22)
summary(garch33)
summary(garch44)
summary(garch55)
summary(garch66)
summary(garch77)

summary(garch12)
summary(garch13)
summary(garch14)
summary(garch15)
summary(garch16)
summary(garch17)

summary(garch21)
summary(garch23)
summary(garch24)
summary(garch25)
summary(garch26)
summary(garch27)

summary(garch31)
summary(garch32)
summary(garch34)
summary(garch35)
summary(garch36)
summary(garch37)

summary(garch41)
summary(garch42)
summary(garch43)
summary(garch45)
summary(garch46)
summary(garch47)

summary(garch51)
summary(garch52)
summary(garch53)
summary(garch54)
summary(garch56)
summary(garch57)

summary(garch61)
summary(garch62)
summary(garch63)
summary(garch64)
summary(garch65)
summary(garch67)

summary(garch71)
summary(garch72)
summary(garch73)
summary(garch74)
summary(garch75)
summary(garch76)

#el mejor es el garch11  BIC: -6.8067
garch11 <- garchFit(residualesarma~garch(1,1), data = residualesarma)

summary(garch11) # Vemos si solo hay ciclo y volatilidad 

#voy a estimar al mismo tiempo arma y garch
modelo1<-garchFit(difCSCO~arma(7,7)+garch(1,1), data=difCSCO)
summary(modelo1)

#Le estoy diciendo que mi modelo es el modelo original, y el garch 11 

#La beta es el coeficiente de garch, para que sea garch la beta tiene que ser significativa 
#Los componentes del ciclo SI SON signifcativos.
#si tiene ciclo, tiene volatilidad



##el mejor es el garch1,1  + arma 1,7
modelo1<-garchFit(difCSCO~arma(1,0)+garch(1,1), data=difCSCO)
summary(modelo1)

#modelos alternativos de volatilidad
#otro object
garchspec <- ugarchspec(variance.model=list(garchOrder=c(1,1)),
                        mean.model=list(armaOrder=c(7,7)), #??????
                        distribution.model="norm")
modelogarch <- ugarchfit(garchspec, data = difCSCO)
modelogarch

#Modelo garch original

#tgarch
tgarchspec <- ugarchspec(variance.model=list(model="gjrGARCH",
                                             garchOrder=c(1,1)),
                         mean.model=list(armaOrder=c(7,7)),
                         distribution.model="std")
modelotgarch <- ugarchfit(tgarchspec, data = difCSCO)
modelotgarch

#egarch

egarchspec <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),
                         mean.model=list(armaOrder=c(7,7)))
modeloegarch <- ugarchfit(egarchspec, data = difCSCO)
modeloegarch

# compare information criteria
model.list <- list("garch(1,1)" = modelogarch,
                   "egarch(1,1)" = modeloegarch,
                   "tgarch(1,1)" = modelotgarch)
info.mat <- sapply(model.list, infocriteria)
rownames(info.mat) <- rownames(infocriteria(modelogarch))
info.mat
#Por Bayes escogemos el T garch

# predecir el tgarch 
predicciones<-ugarchforecast(modelotgarch,data=difCSCO, n.ahead = 100) 

class(predicciones)
plot (predicciones)

slotNames(predicciones)
predicciones@forecast

predicciones@forecast$seriesFor


pronostico<-cumsum(predicciones@forecast$seriesFor)+log(48.06) #última observación de la base original
pronostico #Le quitamos las diferencias 
prediccionesfinal<-exp(pronostico) #Le quitamos el log
prediccionesfinal

#Intervalos de pronostico 
upper<-pronostico+1.96*predicciones@forecast$sigmaFor
print(exp(upper))

lower<-pronostico-1.96*predicciones@forecast$sigmaFor
print(exp(lower))

#pronostico
pronosticots<-ts(prediccionesfinal, start=c(2024,5,10),frequency=365)
pronosticots
lowerts<-ts(exp(lower), start=c(2024,5,10), frequency = 365)
upperts<-ts(exp(upper), start=c(2024,5,10), frequency = 365)






