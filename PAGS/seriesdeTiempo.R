
install.packages("openxlsx")
install.packages("forecast")
install.packages("fUnitRoots")
install.packages("ggfortify")
install.packages("xts")

library(forecast)
library(tseries)
library(fUnitRoots)
library(ggfortify)
library(xts)
library("openxlsx")

PagGuate <- read.xlsx("PAG-GT.xlsx", sheet = 1, startRow = 1, colNames = TRUE)

mesesN<-c(1,2,3,4,5,6,7,8,9,10,11,12)
mesesO<-c(1,2,3,4,5,6,7,8)

PagGuate <- read.xlsx("PAG-GT.xlsx", sheet = 1, startRow = 1, colNames = TRUE)

PagGuate$Pais<-"Guatemala"
View(head(PagGuate))

PagHon <- read.xlsx("PAG-HO.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
PagHon$Pais<-"Honduras"
View(head(PagHon))

PagEL <- read.xlsx("PAG-EL.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
PagEL$Pais<-"El Salvador"
View(head(PagEL))

PagNi <- read.xlsx("PAG-NI.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
PagNi$Pais<-"Nicaragua"
View(head(PagNi))

PagCentroamerica <- rbind(PagGuate, PagHon, PagEL, PagNi)
View(PagCentroamerica)

#2015
v201501<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201501"),11])
v201502<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201502"),11])
v201503<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201503"),11])
v201504<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201504"),11])
v201505<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201505"),11])
v201506<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201506"),11])
v201507<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201507"),11])
v201508<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201508"),11])
v201509<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201509"),11])
v201510<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201510"),11])
v201511<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201511"),11])
v201512<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201512"),11])

v2015<-rbind(v201501, v201502, v201503, v201504,v201505, v201506, v201507, v201508, v201509, v201510, v201511, v201512)
v2015<-as.data.frame(v2015)
v2015$Anio <- 2015
v2015$Mes<-mesesN
View(v2015)

#2016
v201501<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201601"),11])
v201502<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201602"),11])
v201503<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201603"),11])
v201504<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201604"),11])
v201505<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201605"),11])
v201506<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201606"),11])
v201507<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201607"),11])
v201508<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201608"),11])
v201509<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201609"),11])
v201510<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201610"),11])
v201511<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201611"),11])
v201512<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201612"),11])

v2016<-rbind(v201501, v201502, v201503, v201504,v201505, v201506, v201507, v201508, v201509, v201510, v201511, v201512)
v2016<-as.data.frame(v2016)
v2016$Anio <- 2016
v2016$Mes<-mesesN
View(v2016)

#2017
v201501<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201701"),11])
v201502<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201702"),11])
v201503<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201703"),11])
v201504<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201704"),11])
v201505<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201705"),11])
v201506<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201706"),11])
v201507<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201707"),11])
v201508<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201708"),11])
v201509<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201709"),11])
v201510<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201710"),11])
v201511<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201711"),11])
v201512<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201712"),11])

v2017<-rbind(v201501, v201502, v201503, v201504,v201505, v201506, v201507, v201508, v201509, v201510, v201511, v201512)
v2017<-as.data.frame(v2017)
v2017$Anio <- 2017
v2017$Mes<-mesesN
View(v2017)

#2018
v201501<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201801"),11])
v201502<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201802"),11])
v201503<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201803"),11])
v201504<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201804"),11])
v201505<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201805"),11])
v201506<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201806"),11])
v201507<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201807"),11])
v201508<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201808"),11])
v201509<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201809"),11])
v201510<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201810"),11])
v201511<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201811"),11])
v201512<-sum(PagCentroamerica[which(PagCentroamerica[,1] == "201812"),11])

v2018<-rbind(v201501, v201502, v201503, v201504,v201505, v201506, v201507, v201508, v201509, v201510, v201511, v201512)
v2018<-as.data.frame(v2018)
v2018$Anio <- 2018
v2018$Mes<-mesesN
View(v2018)

vector<-rbind(v2015, v2016, v2017, v2018)
View(vector)

# save a numeric vector containing 72 monthly observations
# from Jan 2009 to Dec 2014 as a time series object
myts <- ts(vector, start=c(2015, 1), end=c(2018, 12), frequency=12)

start(myts)
end(myts)
frequency(myts)

plot(myts, type="l", main = "Serie de Tiempo de Unidades Vendidas (2015-2019) en CA")

#Grafico sobre el comportamiento de la media en subsets de los anios
plot(aggregate(myts,FUN=mean))

#descomponemos la serie de tiempo en tendencia, estacionaridad e innovacion
decomposeUniVendidas<-decompose(myts)
plot(decomposeUniVendidas$trend)
plot(decomposeUniVendidas)
plot(decomposeUniVendidas$seasonal)


#######################
### TOP 5 PRODUCTOS FALTANTES
## 4123310128
## 4123660214
## 4123530132
## 4123660107
## 4123660489


PagCentroamerica$Producto<-as.character(PagCentroamerica$Producto)

producto1Fal<-PagCentroamerica[PagCentroamerica$Producto == "4123310128 ",]
producto2Fal<-PagCentroamerica[PagCentroamerica$Producto == "4123660214 ",]
producto3Fal<-PagCentroamerica[PagCentroamerica$Producto == "4123530132 ",]
producto4Fal<-PagCentroamerica[PagCentroamerica$Producto == "4123660107 ",]
producto5Fal<-PagCentroamerica[PagCentroamerica$Producto == "4123660489 ",]

## SERIE DE TIEMPO PARA CADA PRODUCTO

########################################### PRODUCTO 1 ########################################### 
# sumamos todas las ventas por anio
# 2015
View(producto1Fal)
v201501<-sum(producto1Fal[which(producto1Fal[,1] == "201501"),11])
v201502<-sum(producto1Fal[which(producto1Fal[,1] == "201502"),11])
v201503<-sum(producto1Fal[which(producto1Fal[,1] == "201503"),11])
v201504<-sum(producto1Fal[which(producto1Fal[,1] == "201504"),11])
v201505<-sum(producto1Fal[which(producto1Fal[,1] == "201505"),11])
v201506<-sum(producto1Fal[which(producto1Fal[,1] == "201506"),11])
v201507<-sum(producto1Fal[which(producto1Fal[,1] == "201507"),11])
v201508<-sum(producto1Fal[which(producto1Fal[,1] == "201508"),11])
v201509<-sum(producto1Fal[which(producto1Fal[,1] == "201509"),11])
v201510<-sum(producto1Fal[which(producto1Fal[,1] == "201510"),11])
v201511<-sum(producto1Fal[which(producto1Fal[,1] == "201511"),11])
v201512<-sum(producto1Fal[which(producto1Fal[,1] == "201512"),11])

pro12015<-rbind(v201501, v201502, v201503, v201504,v201505, v201506, v201507, v201508, v201509, v201510, v201511, v201512)
pro12015<-as.data.frame(pro12015)
pro12015$Anio <- 2015
pro12015$Mes<-mesesN
View(pro12015)

# 2016
v201601<-sum(producto1Fal[which(producto1Fal[,1] == "201601"),11])
v201602<-sum(producto1Fal[which(producto1Fal[,1] == "201602"),11])
v201603<-sum(producto1Fal[which(producto1Fal[,1] == "201603"),11])
v201604<-sum(producto1Fal[which(producto1Fal[,1] == "201604"),11])
v201605<-sum(producto1Fal[which(producto1Fal[,1] == "201605"),11])
v201606<-sum(producto1Fal[which(producto1Fal[,1] == "201606"),11])
v201607<-sum(producto1Fal[which(producto1Fal[,1] == "201607"),11])
v201608<-sum(producto1Fal[which(producto1Fal[,1] == "201608"),11])
v201609<-sum(producto1Fal[which(producto1Fal[,1] == "201609"),11])
v201610<-sum(producto1Fal[which(producto1Fal[,1] == "201610"),11])
v201611<-sum(producto1Fal[which(producto1Fal[,1] == "201611"),11])
v201612<-sum(producto1Fal[which(producto1Fal[,1] == "201612"),11])

pro12016<-rbind(v201601, v201602, v201603, v201604,v201605, v201606, v201607, v201608, v201609, v201610, v201611, v201612)
pro12016<-as.data.frame(pro12016)
pro12016$Anio <- 2016
pro12016$Mes<-mesesN
View(pro12016)

# 2017
v201701<-sum(producto1Fal[which(producto1Fal[,1] == "201701"),11])
v201702<-sum(producto1Fal[which(producto1Fal[,1] == "201702"),11])
v201703<-sum(producto1Fal[which(producto1Fal[,1] == "201703"),11])
v201704<-sum(producto1Fal[which(producto1Fal[,1] == "201704"),11])
v201705<-sum(producto1Fal[which(producto1Fal[,1] == "201705"),11])
v201706<-sum(producto1Fal[which(producto1Fal[,1] == "201706"),11])
v201707<-sum(producto1Fal[which(producto1Fal[,1] == "201707"),11])
v201708<-sum(producto1Fal[which(producto1Fal[,1] == "201708"),11])
v201709<-sum(producto1Fal[which(producto1Fal[,1] == "201709"),11])
v201710<-sum(producto1Fal[which(producto1Fal[,1] == "201710"),11])
v201711<-sum(producto1Fal[which(producto1Fal[,1] == "201711"),11])
v201712<-sum(producto1Fal[which(producto1Fal[,1] == "201712"),11])

pro12017<-rbind(v201701, v201702, v201703, v201704,v201705, v201706, v201707, v201708, v201709, v201710, v201711, v201712)
pro12017<-as.data.frame(pro12017)
pro12017$Anio <- 2017
pro12017$Mes<-mesesN
View(pro12017)

# 2018
v201801<-sum(producto1Fal[which(producto1Fal[,1] == "201801"),11])
v201802<-sum(producto1Fal[which(producto1Fal[,1] == "201802"),11])
v201803<-sum(producto1Fal[which(producto1Fal[,1] == "201803"),11])
v201804<-sum(producto1Fal[which(producto1Fal[,1] == "201804"),11])
v201805<-sum(producto1Fal[which(producto1Fal[,1] == "201805"),11])
v201806<-sum(producto1Fal[which(producto1Fal[,1] == "201806"),11])
v201807<-sum(producto1Fal[which(producto1Fal[,1] == "201807"),11])
v201808<-sum(producto1Fal[which(producto1Fal[,1] == "201808"),11])
v201809<-sum(producto1Fal[which(producto1Fal[,1] == "201809"),11])
v201810<-sum(producto1Fal[which(producto1Fal[,1] == "201810"),11])
v201811<-sum(producto1Fal[which(producto1Fal[,1] == "201811"),11])
v201812<-sum(producto1Fal[which(producto1Fal[,1] == "201812"),11])

pro12018<-rbind(v201801, v201802, v201803, v201804,v201805, v201806, v201807, v201808, v201809, v201810, v201811, v201812)
pro12018<-as.data.frame(pro12018)
pro12018$Anio <- 2018
pro12018$Mes<-mesesN
View(pro12018)

# 2019
v201901<-sum(producto1Fal[which(producto1Fal[,1] == "201901"),11])
v201902<-sum(producto1Fal[which(producto1Fal[,1] == "201902"),11])


# TIMESERIES
vector<-rbind(pro12015, pro12016, pro12017, pro12018)
View(vector)

# save a numeric vector containing 72 monthly observations
# from Jan 2009 to Dec 2014 as a time series object
myts <- ts(vector, start=c(2015, 1), end=c(2018, 12), frequency=12)
colnames(myts)[1] <- "Unidades Vendidas"

start(myts)
end(myts)
frequency(myts)

plot(myts[,1], type="l", main = "Serie de Tiempo de  PRO 1", ylab = "Unidades Vendidas")


decomP1<-decompose(myts)
plot(decomP1)

#Grafico sobre el comportamiento de la media en subsets de los anios
plot(aggregate(myts,FUN=mean))
View(myts)

#descomponemos la serie de tiempo en tendencia, estacionaridad e innovacion
plot(decomP1$trend)
plot(decomP1)
plot(decomP1$seasonal)

#forecasting
pts<-ts(myts)
pts[2] = 0
pts[6] = 0
pts[7] = 0
pts<-abs(pts)
fp1<-auto.arima(pts[,1])
fp1

# Forecast for the next 10 time units
arima_forecast = forecast(fp1, h = 1, level = 0.95)

# Plot forecasts
plot(arima_forecast, main = "Serie de Tiempo de  PRO 1", ylab = "Unidades Vendidas", xlab = "Meses")
arima_forecast$mean


#Aplicaremos una transformacion logaritmica
croston<-croston(myts[,1], h = 1)
plot(croston, main ="Prediccion para el Producto 1", xlab = "Tiempo", ylab = "Unidades Vendidas")
croston$fitted
croston$mean


logUniVendidas <- log(croston$fitted)
View(logUniVendidas)
plot(logUniVendidas, type="l", main="Serie de Tiempo de Unidades Vendias con Transf. Log", ylab="Unidades Vendidas")

############################################ EXTRA ############################################
#Grafico de autocorrelacion
acf(logUniVendidas, type="correlation", lag.max = 100, main = "Grafico de correlacion serie de tiempo Unidades Vendidas" )

#Utilizacion de prueba Dickey-Fuller para raices unitarias
logUniVendidas[c(1,2,3,4,5,6)] <- 0
adfTest(logUniVendidas)
adfTest(diff(logUniVendidas))


#######################
# funciones de autocorrelacion y autocorrelacion parcial
acf(diff(logUniVendidas),12) #2 valores que pasan la linea punteada
pacf(diff(logUniVendidas)) #6 valores pasan la linea punteada
#Funcion autoarima para encontrar valores propuestos de p, d y q
auto.arima(logUniVendidas) #5, 1, 1

# Al utilizar la funcion de auto-arima los valores del output no cambian
# en mÃ¡s de 1 nÃºmero con los obtenidos a traves de las funciones de autocorrelacion
# y autocorrelacion parcial, por lo que la funcion autoarima pareciera tener coherencia
# y sentido con el modelo
#
fit <- arima(logUniVendidas, c(0, 1, 0),seasonal = list(order = c(0, 1, 1), period = 12))
fit.2 <- arima(logUniVendidas, c(1, 1, 0),seasonal = list(order = c(0, 1, 1), period = 12))
#Utilizando valores p, d y q obtenidos en autoarima
fit.3 <- arima(logdiesel, c(5, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12))

#predicciones con cada uno de los modelos propuestos
pred <- predict(fit, n.ahead = 0.5*12)
pred.1 <- predict(fit.2, n.ahead = 0.5*12)
pred.2 <- predict(fit.3, n.ahead = 10*12)

ts.plot(myts,2.718^pred$pred, log = "y", lty = c(1,2))
?ts.plot
ts.plot(abs(myts),2.718^pred.1$pred, log = "y", lty = c(1,3))
ts.plot(serietiempo.diesel,2.718^pred.2$pred, log = "y", lty = c(1,3))
############################################ EXTRA ############################################



########################################### PRODUCTO 2 ########################################### 
# sumamos todas las ventas por anio
# 2015
View(producto2Fal$Descripcion)
v201501<-sum(producto2Fal[which(producto2Fal[,1] == "201501"),11])
v201502<-sum(producto2Fal[which(producto2Fal[,1] == "201502"),11])
v201503<-sum(producto2Fal[which(producto2Fal[,1] == "201503"),11])
v201504<-sum(producto2Fal[which(producto2Fal[,1] == "201504"),11])
v201505<-sum(producto2Fal[which(producto2Fal[,1] == "201505"),11])
v201506<-sum(producto2Fal[which(producto2Fal[,1] == "201506"),11])
v201507<-sum(producto2Fal[which(producto2Fal[,1] == "201507"),11])
v201508<-sum(producto2Fal[which(producto2Fal[,1] == "201508"),11])
v201509<-sum(producto2Fal[which(producto2Fal[,1] == "201509"),11])
v201510<-sum(producto2Fal[which(producto2Fal[,1] == "201510"),11])
v201511<-sum(producto2Fal[which(producto2Fal[,1] == "201511"),11])
v201512<-sum(producto2Fal[which(producto2Fal[,1] == "201512"),11])

pro22015<-rbind(v201501, v201502, v201503, v201504,v201505, v201506, v201507, v201508, v201509, v201510, v201511, v201512)
pro22015<-as.data.frame(pro22015)
pro22015$Anio <- 2015
pro22015$Mes<-mesesN
View(pro22015)

# 2016
v201601<-sum(producto2Fal[which(producto2Fal[,1] == "201601"),11])
v201602<-sum(producto2Fal[which(producto2Fal[,1] == "201602"),11])
v201603<-sum(producto2Fal[which(producto2Fal[,1] == "201603"),11])
v201604<-sum(producto2Fal[which(producto2Fal[,1] == "201604"),11])
v201605<-sum(producto2Fal[which(producto2Fal[,1] == "201605"),11])
v201606<-sum(producto2Fal[which(producto2Fal[,1] == "201606"),11])
v201607<-sum(producto2Fal[which(producto2Fal[,1] == "201607"),11])
v201608<-sum(producto2Fal[which(producto2Fal[,1] == "201608"),11])
v201609<-sum(producto2Fal[which(producto2Fal[,1] == "201609"),11])
v201610<-sum(producto2Fal[which(producto2Fal[,1] == "201610"),11])
v201611<-sum(producto2Fal[which(producto2Fal[,1] == "201611"),11])
v201612<-sum(producto2Fal[which(producto2Fal[,1] == "201612"),11])

pro22016<-rbind(v201601, v201602, v201603, v201604,v201605, v201606, v201607, v201608, v201609, v201610, v201611, v201612)
pro22016<-as.data.frame(pro22016)
pro22016$Anio <- 2016
pro22016$Mes<-mesesN
View(pro22016)

# 2017
v201701<-sum(producto2Fal[which(producto2Fal[,1] == "201701"),11])
v201702<-sum(producto2Fal[which(producto2Fal[,1] == "201702"),11])
v201703<-sum(producto2Fal[which(producto2Fal[,1] == "201703"),11])
v201704<-sum(producto2Fal[which(producto2Fal[,1] == "201704"),11])
v201705<-sum(producto2Fal[which(producto2Fal[,1] == "201705"),11])
v201706<-sum(producto2Fal[which(producto2Fal[,1] == "201706"),11])
v201707<-sum(producto2Fal[which(producto2Fal[,1] == "201707"),11])
v201708<-sum(producto2Fal[which(producto2Fal[,1] == "201708"),11])
v201709<-sum(producto2Fal[which(producto2Fal[,1] == "201709"),11])
v201710<-sum(producto2Fal[which(producto2Fal[,1] == "201710"),11])
v201711<-sum(producto2Fal[which(producto2Fal[,1] == "201711"),11])
v201712<-sum(producto2Fal[which(producto2Fal[,1] == "201712"),11])

pro22017<-rbind(v201701, v201702, v201703, v201704,v201705, v201706, v201707, v201708, v201709, v201710, v201711, v201712)
pro22017<-as.data.frame(pro22017)
pro22017$Anio <- 2017
pro22017$Mes<-mesesN
View(pro22017)

# 2018
v201801<-sum(producto2Fal[which(producto2Fal[,1] == "201801"),11])
v201802<-sum(producto2Fal[which(producto2Fal[,1] == "201802"),11])
v201803<-sum(producto2Fal[which(producto2Fal[,1] == "201803"),11])
v201804<-sum(producto2Fal[which(producto2Fal[,1] == "201804"),11])
v201805<-sum(producto2Fal[which(producto2Fal[,1] == "201805"),11])
v201806<-sum(producto2Fal[which(producto2Fal[,1] == "201806"),11])
v201807<-sum(producto2Fal[which(producto2Fal[,1] == "201807"),11])
v201808<-sum(producto2Fal[which(producto2Fal[,1] == "201808"),11])
v201809<-sum(producto2Fal[which(producto2Fal[,1] == "201809"),11])
v201810<-sum(producto2Fal[which(producto2Fal[,1] == "201810"),11])
v201811<-sum(producto2Fal[which(producto2Fal[,1] == "201811"),11])
v201812<-sum(producto2Fal[which(producto2Fal[,1] == "201812"),11])

pro22018<-rbind(v201801, v201802, v201803, v201804,v201805, v201806, v201807, v201808, v201809, v201810, v201811, v201812)
pro22018<-as.data.frame(pro22018)
pro22018$Anio <- 2018
pro22018$Mes<-mesesN
View(pro22018)

# 2019
v201901<-sum(producto2Fal[which(producto2Fal[,1] == "201901"),11])
v201902<-sum(producto2Fal[which(producto2Fal[,1] == "201902"),11])
v201903<-sum(producto2Fal[which(producto2Fal[,1] == "201901"),11])
v201904<-sum(producto2Fal[which(producto2Fal[,1] == "201902"),11])
v201905<-sum(producto2Fal[which(producto2Fal[,1] == "201901"),11])
v201906<-sum(producto2Fal[which(producto2Fal[,1] == "201902"),11])
v201907<-sum(producto2Fal[which(producto2Fal[,1] == "201901"),11])
v201908<-sum(producto2Fal[which(producto2Fal[,1] == "201902"),11])

pro22019<-rbind(v201901, v201902, v201903, v201904,v201905, v201906, v201907, v201908)
pro22019<-as.data.frame(pro22019)
pro22019$Anio <- 2019
pro22019$Mes<-mesesO
View(pro22019)

vector<-rbind(pro22015, pro22016, pro22017, pro22018)
View(vector)

# TIMESERIES

# save a numeric vector containing 72 monthly observations
# from Jan 2015 to Dec 2018 as a time series object
myts <- ts(vector, start=c(2015, 1), end=c(2018, 12), frequency=12)
colnames(myts)[1] <- "Unidades Vendidas"

start(myts)
end(myts)
frequency(myts)

plot(myts, type="l", main = "Serie de Tiempo de  PRO 2")
ts.plot(myts)

decomP2<-decompose(myts)
plot(decomP2)

#Grafico sobre el comportamiento de la media en subsets de los anios
plot(aggregate(myts,FUN=mean))
View(myts)

#descomponemos la serie de tiempo en tendencia, estacionaridad e innovacion
plot(decomP2$trend)
plot(decomP1$seasonal)

#forecasting
pts<-ts(myts)
pts[2] = 0
pts[6] = 0
pts[7] = 0
pts<-abs(pts)
fp1<-auto.arima(myts[,1])
fp1

# Forecast for the next 10 time units
arima_forecast = forecast(fp1, h = 12, level = c(80,95))
p1<-predict(fp1, n.ahead = 1*12)

# Plot forecasts
plot(arima_forecast)
arima_forecast$mean

#Aplicaremos una transformacion logaritmica
myts[c(11,22,47)] <- 0
croston<-croston(myts[,1], h = 8)
plot(croston, main ="Prediccion para el Producto 2", xlab = "Tiempo", ylab = "Unidades Vendidas")
croston$fitted
croston

p1<-sum(producto2Fal[which(producto2Fal[,1] == "201901"),10])
p2<-sum(producto2Fal[which(producto2Fal[,1] == "201902"),10])
p3<-sum(producto2Fal[which(producto2Fal[,1] == "201903"),10])
p4<-sum(producto2Fal[which(producto2Fal[,1] == "201904"),10])
p5<-sum(producto2Fal[which(producto2Fal[,1] == "201905"),10])
p6<-sum(producto2Fal[which(producto2Fal[,1] == "201906"),10])
p7<-sum(producto2Fal[which(producto2Fal[,1] == "201907"),10])
p8<-sum(producto2Fal[which(producto2Fal[,1] == "201908"),10])
pronosticoMV<-rbind(p1,p2,p3,p4,p5,p6,p7,p8)
pronosticoMV

########################################### PRODUCTO 3 ########################################### 
# sumamos todas las ventas por anio
# 2015

v201501<-sum(producto3Fal[which(producto3Fal[,1] == "201501"),11])
v201502<-sum(producto3Fal[which(producto3Fal[,1] == "201502"),11])
v201503<-sum(producto3Fal[which(producto3Fal[,1] == "201503"),11])
v201504<-sum(producto3Fal[which(producto3Fal[,1] == "201504"),11])
v201505<-sum(producto3Fal[which(producto3Fal[,1] == "201505"),11])
v201506<-sum(producto3Fal[which(producto3Fal[,1] == "201506"),11])
v201507<-sum(producto3Fal[which(producto3Fal[,1] == "201507"),11])
v201508<-sum(producto3Fal[which(producto3Fal[,1] == "201508"),11])
v201509<-sum(producto3Fal[which(producto3Fal[,1] == "201509"),11])
v201510<-sum(producto3Fal[which(producto3Fal[,1] == "201510"),11])
v201511<-sum(producto3Fal[which(producto3Fal[,1] == "201511"),11])
v201512<-sum(producto3Fal[which(producto3Fal[,1] == "201512"),11])

pro32015<-rbind(v201501, v201502, v201503, v201504,v201505, v201506, v201507, v201508, v201509, v201510, v201511, v201512)
pro32015<-as.data.frame(pro32015)
pro32015$Anio <- 2015
pro32015$Mes<-mesesN
View(pro32015)

# 2016
v201601<-sum(producto3Fal[which(producto3Fal[,1] == "201601"),11])
v201602<-sum(producto3Fal[which(producto3Fal[,1] == "201602"),11])
v201603<-sum(producto3Fal[which(producto3Fal[,1] == "201603"),11])
v201604<-sum(producto3Fal[which(producto3Fal[,1] == "201604"),11])
v201605<-sum(producto3Fal[which(producto3Fal[,1] == "201605"),11])
v201606<-sum(producto3Fal[which(producto3Fal[,1] == "201606"),11])
v201607<-sum(producto3Fal[which(producto3Fal[,1] == "201607"),11])
v201608<-sum(producto3Fal[which(producto3Fal[,1] == "201608"),11])
v201609<-sum(producto3Fal[which(producto3Fal[,1] == "201609"),11])
v201610<-sum(producto3Fal[which(producto3Fal[,1] == "201610"),11])
v201611<-sum(producto3Fal[which(producto3Fal[,1] == "201611"),11])
v201612<-sum(producto3Fal[which(producto3Fal[,1] == "201612"),11])

pro32016<-rbind(v201601, v201602, v201603, v201604,v201605, v201606, v201607, v201608, v201609, v201610, v201611, v201612)
pro32016<-as.data.frame(pro32016)
pro32016$Anio <- 2016
pro32016$Mes<-mesesN
View(pro32016)

# 2017
v201701<-sum(producto3Fal[which(producto3Fal[,1] == "201701"),11])
v201702<-sum(producto3Fal[which(producto3Fal[,1] == "201702"),11])
v201703<-sum(producto3Fal[which(producto3Fal[,1] == "201703"),11])
v201704<-sum(producto3Fal[which(producto3Fal[,1] == "201704"),11])
v201705<-sum(producto3Fal[which(producto3Fal[,1] == "201705"),11])
v201706<-sum(producto3Fal[which(producto3Fal[,1] == "201706"),11])
v201707<-sum(producto3Fal[which(producto3Fal[,1] == "201707"),11])
v201708<-sum(producto3Fal[which(producto3Fal[,1] == "201708"),11])
v201709<-sum(producto3Fal[which(producto3Fal[,1] == "201709"),11])
v201710<-sum(producto3Fal[which(producto3Fal[,1] == "201710"),11])
v201711<-sum(producto3Fal[which(producto3Fal[,1] == "201711"),11])
v201712<-sum(producto3Fal[which(producto3Fal[,1] == "201712"),11])

pro32017<-rbind(v201701, v201702, v201703, v201704,v201705, v201706, v201707, v201708, v201709, v201710, v201711, v201712)
pro32017<-as.data.frame(pro32017)
pro32017$Anio <- 2017
pro32017$Mes<-mesesN
View(pro32017)

# 2018
v201801<-sum(producto3Fal[which(producto3Fal[,1] == "201801"),11])
v201802<-sum(producto3Fal[which(producto3Fal[,1] == "201802"),11])
v201803<-sum(producto3Fal[which(producto3Fal[,1] == "201803"),11])
v201804<-sum(producto3Fal[which(producto3Fal[,1] == "201804"),11])
v201805<-sum(producto3Fal[which(producto3Fal[,1] == "201805"),11])
v201806<-sum(producto3Fal[which(producto3Fal[,1] == "201806"),11])
v201807<-sum(producto3Fal[which(producto3Fal[,1] == "201807"),11])
v201808<-sum(producto3Fal[which(producto3Fal[,1] == "201808"),11])
v201809<-sum(producto3Fal[which(producto3Fal[,1] == "201809"),11])
v201810<-sum(producto3Fal[which(producto3Fal[,1] == "201810"),11])
v201811<-sum(producto3Fal[which(producto3Fal[,1] == "201811"),11])
v201812<-sum(producto3Fal[which(producto3Fal[,1] == "201812"),11])

pro32018<-rbind(v201801, v201802, v201803, v201804,v201805, v201806, v201807, v201808, v201809, v201810, v201811, v201812)
pro32018<-as.data.frame(pro32018)
pro32018$Anio <- 2018
pro32018$Mes<-mesesN
View(pro32018)

# 2019
v201901<-sum(producto3Fal[which(producto3Fal[,1] == "201901"),11])
v201902<-sum(producto3Fal[which(producto3Fal[,1] == "201902"),11])
v201903<-sum(producto3Fal[which(producto3Fal[,1] == "201901"),11])
v201904<-sum(producto3Fal[which(producto3Fal[,1] == "201902"),11])
v201905<-sum(producto3Fal[which(producto3Fal[,1] == "201901"),11])
v201906<-sum(producto3Fal[which(producto3Fal[,1] == "201902"),11])
v201907<-sum(producto3Fal[which(producto3Fal[,1] == "201901"),11])
v201908<-sum(producto3Fal[which(producto3Fal[,1] == "201902"),11])

pro32019<-rbind(v201901, v201902, v201903, v201904,v201905, v201906, v201907, v201908)
pro32019<-as.data.frame(pro32019)
pro32019$Anio <- 2019
pro32019$Mes<-mesesO
View(pro32019)

vector<-rbind(pro32015, pro32016, pro32017, pro32018)
View(vector)

# TIMESERIES

# save a numeric vector containing 72 monthly observations
# from Jan 2015 to Dec 2018 as a time series object
myts <- ts(vector, start=c(2015, 1), end=c(2018, 12), frequency=12)
colnames(myts)[1] <- "Unidades Vendidas"

start(myts)
end(myts)
frequency(myts)

plot(myts, type="l", main = "Serie de Tiempo de  PRO 3")
ts.plot(myts)

decomP2<-decompose(myts)
plot(decomP2)

#Grafico sobre el comportamiento de la media en subsets de los anios
plot(aggregate(myts,FUN=mean))
View(myts)

#descomponemos la serie de tiempo en tendencia, estacionaridad e innovacion
plot(decomP2$trend)
plot(decomP1$seasonal)

#forecasting
fp1<-auto.arima(myts[,1])
fp1

# Forecast for the next 10 time units
arima_forecast = forecast(fp1, h = 1, level = c(80,95))
p1<-predict(fp1, n.ahead = 1*12)

# Plot forecasts
plot(arima_forecast, main = "Serie de Tiempo para el PRO 3", xlab = "Tiempo", ylab = "Unidades Vendidas")
arima_forecast$mean


#Aplicaremos una transformacion logaritmica
croston<-croston(myts[,1], h = 8)
plot(croston, main ="Prediccion para el Producto 2", xlab = "Tiempo", ylab = "Unidades Vendidas")
croston$fitted
croston

p1<-sum(producto3Fal[which(producto3Fal[,1] == "201901"),11])
p2<-sum(producto3Fal[which(producto3Fal[,1] == "201902"),11])
p3<-sum(producto3Fal[which(producto3Fal[,1] == "201903"),11])
p4<-sum(producto3Fal[which(producto3Fal[,1] == "201904"),11])
p5<-sum(producto3Fal[which(producto3Fal[,1] == "201905"),11])
p6<-sum(producto3Fal[which(producto3Fal[,1] == "201906"),11])
p7<-sum(producto3Fal[which(producto3Fal[,1] == "201907"),11])
p8<-sum(producto3Fal[which(producto3Fal[,1] == "201908"),11])
pronosticoMV<-rbind(p1,p2,p3,p4,p5,p6,p7,p8)
pronosticoMV
sum(pronosticoMV)

View(producto3Fal)

########################################### PRODUCTO 4 ########################################### 
# sumamos todas las ventas por anio
# 2015

v201501<-sum(producto4Fal[which(producto4Fal[,1] == "201501"),11])
v201502<-sum(producto4Fal[which(producto4Fal[,1] == "201502"),11])
v201503<-sum(producto4Fal[which(producto4Fal[,1] == "201503"),11])
v201504<-sum(producto4Fal[which(producto4Fal[,1] == "201504"),11])
v201505<-sum(producto4Fal[which(producto4Fal[,1] == "201505"),11])
v201506<-sum(producto4Fal[which(producto4Fal[,1] == "201506"),11])
v201507<-sum(producto4Fal[which(producto4Fal[,1] == "201507"),11])
v201508<-sum(producto4Fal[which(producto4Fal[,1] == "201508"),11])
v201509<-sum(producto4Fal[which(producto4Fal[,1] == "201509"),11])
v201510<-sum(producto4Fal[which(producto4Fal[,1] == "201510"),11])
v201511<-sum(producto4Fal[which(producto4Fal[,1] == "201511"),11])
v201512<-sum(producto4Fal[which(producto4Fal[,1] == "201512"),11])

pro42015<-rbind(v201501, v201502, v201503, v201504,v201505, v201506, v201507, v201508, v201509, v201510, v201511, v201512)
pro42015<-as.data.frame(pro42015)
pro42015$Anio <- 2015
pro42015$Mes<-mesesN
View(pro42015)

# 2016
v201601<-sum(producto4Fal[which(producto4Fal[,1] == "201601"),11])
v201602<-sum(producto4Fal[which(producto4Fal[,1] == "201602"),11])
v201603<-sum(producto4Fal[which(producto4Fal[,1] == "201603"),11])
v201604<-sum(producto4Fal[which(producto4Fal[,1] == "201604"),11])
v201605<-sum(producto4Fal[which(producto4Fal[,1] == "201605"),11])
v201606<-sum(producto4Fal[which(producto4Fal[,1] == "201606"),11])
v201607<-sum(producto4Fal[which(producto4Fal[,1] == "201607"),11])
v201608<-sum(producto4Fal[which(producto4Fal[,1] == "201608"),11])
v201609<-sum(producto4Fal[which(producto4Fal[,1] == "201609"),11])
v201610<-sum(producto4Fal[which(producto4Fal[,1] == "201610"),11])
v201611<-sum(producto4Fal[which(producto4Fal[,1] == "201611"),11])
v201612<-sum(producto4Fal[which(producto4Fal[,1] == "201612"),11])

pro42016<-rbind(v201601, v201602, v201603, v201604,v201605, v201606, v201607, v201608, v201609, v201610, v201611, v201612)
pro42016<-as.data.frame(pro42016)
pro42016$Anio <- 2016
pro42016$Mes<-mesesN
View(pro42016)

# 2017
v201701<-sum(producto4Fal[which(producto4Fal[,1] == "201701"),11])
v201702<-sum(producto4Fal[which(producto4Fal[,1] == "201702"),11])
v201703<-sum(producto4Fal[which(producto4Fal[,1] == "201703"),11])
v201704<-sum(producto4Fal[which(producto4Fal[,1] == "201704"),11])
v201705<-sum(producto4Fal[which(producto4Fal[,1] == "201705"),11])
v201706<-sum(producto4Fal[which(producto4Fal[,1] == "201706"),11])
v201707<-sum(producto4Fal[which(producto4Fal[,1] == "201707"),11])
v201708<-sum(producto4Fal[which(producto4Fal[,1] == "201708"),11])
v201709<-sum(producto4Fal[which(producto4Fal[,1] == "201709"),11])
v201710<-sum(producto4Fal[which(producto4Fal[,1] == "201710"),11])
v201711<-sum(producto4Fal[which(producto4Fal[,1] == "201711"),11])
v201712<-sum(producto4Fal[which(producto4Fal[,1] == "201712"),11])

pro42017<-rbind(v201701, v201702, v201703, v201704,v201705, v201706, v201707, v201708, v201709, v201710, v201711, v201712)
pro42017<-as.data.frame(pro42017)
pro42017$Anio <- 2017
pro42017$Mes<-mesesN
View(pro42017)

# 2018
v201801<-sum(producto4Fal[which(producto4Fal[,1] == "201801"),11])
v201802<-sum(producto4Fal[which(producto4Fal[,1] == "201802"),11])
v201803<-sum(producto4Fal[which(producto4Fal[,1] == "201803"),11])
v201804<-sum(producto4Fal[which(producto4Fal[,1] == "201804"),11])
v201805<-sum(producto4Fal[which(producto4Fal[,1] == "201805"),11])
v201806<-sum(producto4Fal[which(producto4Fal[,1] == "201806"),11])
v201807<-sum(producto4Fal[which(producto4Fal[,1] == "201807"),11])
v201808<-sum(producto4Fal[which(producto4Fal[,1] == "201808"),11])
v201809<-sum(producto4Fal[which(producto4Fal[,1] == "201809"),11])
v201810<-sum(producto4Fal[which(producto4Fal[,1] == "201810"),11])
v201811<-sum(producto4Fal[which(producto4Fal[,1] == "201811"),11])
v201812<-sum(producto4Fal[which(producto4Fal[,1] == "201812"),11])

pro42018<-rbind(v201801, v201802, v201803, v201804,v201805, v201806, v201807, v201808, v201809, v201810, v201811, v201812)
pro42018<-as.data.frame(pro42018)
pro42018$Anio <- 2018
pro42018$Mes<-mesesN
View(pro42018)

# 2019
v201901<-sum(producto4Fal[which(producto4Fal[,1] == "201901"),11])
v201902<-sum(producto4Fal[which(producto4Fal[,1] == "201902"),11])
v201903<-sum(producto4Fal[which(producto4Fal[,1] == "201901"),11])
v201904<-sum(producto4Fal[which(producto4Fal[,1] == "201902"),11])
v201905<-sum(producto4Fal[which(producto4Fal[,1] == "201901"),11])
v201906<-sum(producto4Fal[which(producto4Fal[,1] == "201902"),11])
v201907<-sum(producto4Fal[which(producto4Fal[,1] == "201901"),11])
v201908<-sum(producto4Fal[which(producto4Fal[,1] == "201902"),11])

pro42019<-rbind(v201901, v201902, v201903, v201904,v201905, v201906, v201907, v201908)
pro42019<-as.data.frame(pro42019)
pro42019$Anio <- 2019
pro42019$Mes<-mesesO
View(pro42019)

vector<-rbind(pro42015, pro42016, pro42017, pro42018)
View(vector)

# TIMESERIES

# save a numeric vector containing 72 monthly observations
# from Jan 2015 to Dec 2018 as a time series object
myts <- ts(vector, start=c(2015, 1), end=c(2018, 12), frequency=12)
colnames(myts)[1] <- "Unidades Vendidas"

start(myts)
end(myts)
frequency(myts)

plot(myts, type="l", main = "Serie de Tiempo de  PRO 4")
ts.plot(myts)

decomP2<-decompose(myts)
plot(decomP2)

#Grafico sobre el comportamiento de la media en subsets de los anios
plot(aggregate(myts,FUN=mean))
View(myts)

#descomponemos la serie de tiempo en tendencia, estacionaridad e innovacion
plot(decomP2$trend)
plot(decomP1$seasonal)

#forecasting
fp1<-auto.arima(myts[,1])
fp1

# Forecast for the next 10 time units
arima_forecast = forecast(fp1, h = 1, level = c(80,95))
p1<-predict(fp1, n.ahead = 1*12)

# Plot forecasts
plot(arima_forecast, main = "Serie de Tiempo para el PRO 4", xlab = "Tiempo", ylab = "Unidades Vendidas")
arima_forecast$mean


#Aplicaremos una transformacion logaritmica
myts[10] <- 0
croston<-croston(myts[,1], h = 8)
plot(croston, main ="Prediccion para el Producto 2", xlab = "Tiempo", ylab = "Unidades Vendidas")
croston$fitted
croston

p1<-sum(producto4Fal[which(producto4Fal[,1] == "201901"),11])
p2<-sum(producto4Fal[which(producto4Fal[,1] == "201902"),11])
p3<-sum(producto4Fal[which(producto4Fal[,1] == "201903"),11])
p4<-sum(producto4Fal[which(producto4Fal[,1] == "201904"),11])
p5<-sum(producto4Fal[which(producto4Fal[,1] == "201905"),11])
p6<-sum(producto4Fal[which(producto4Fal[,1] == "201906"),11])
p7<-sum(producto4Fal[which(producto4Fal[,1] == "201907"),11])
p8<-sum(producto4Fal[which(producto4Fal[,1] == "201908"),11])
pronosticoMV<-rbind(p1,p2,p3,p4,p5,p6,p7,p8)
pronosticoMV
sum(pronosticoMV)

View(producto4Fal)

########################################### PRODUCTO 5 ########################################### 
# sumamos todas las ventas por anio
# 2015

v201501<-sum(producto5Fal[which(producto5Fal[,1] == "201501"),11])
v201502<-sum(producto5Fal[which(producto5Fal[,1] == "201502"),11])
v201503<-sum(producto5Fal[which(producto5Fal[,1] == "201503"),11])
v201504<-sum(producto5Fal[which(producto5Fal[,1] == "201504"),11])
v201505<-sum(producto5Fal[which(producto5Fal[,1] == "201505"),11])
v201506<-sum(producto5Fal[which(producto5Fal[,1] == "201506"),11])
v201507<-sum(producto5Fal[which(producto5Fal[,1] == "201507"),11])
v201508<-sum(producto5Fal[which(producto5Fal[,1] == "201508"),11])
v201509<-sum(producto5Fal[which(producto5Fal[,1] == "201509"),11])
v201510<-sum(producto5Fal[which(producto5Fal[,1] == "201510"),11])
v201511<-sum(producto5Fal[which(producto5Fal[,1] == "201511"),11])
v201512<-sum(producto5Fal[which(producto5Fal[,1] == "201512"),11])

pro52015<-rbind(v201501, v201502, v201503, v201504,v201505, v201506, v201507, v201508, v201509, v201510, v201511, v201512)
pro52015<-as.data.frame(pro52015)
pro52015$Anio <- 2015
pro52015$Mes<-mesesN
View(pro52015)

# 2016
v201601<-sum(producto5Fal[which(producto5Fal[,1] == "201601"),11])
v201602<-sum(producto5Fal[which(producto5Fal[,1] == "201602"),11])
v201603<-sum(producto5Fal[which(producto5Fal[,1] == "201603"),11])
v201604<-sum(producto5Fal[which(producto5Fal[,1] == "201604"),11])
v201605<-sum(producto5Fal[which(producto5Fal[,1] == "201605"),11])
v201606<-sum(producto5Fal[which(producto5Fal[,1] == "201606"),11])
v201607<-sum(producto5Fal[which(producto5Fal[,1] == "201607"),11])
v201608<-sum(producto5Fal[which(producto5Fal[,1] == "201608"),11])
v201609<-sum(producto5Fal[which(producto5Fal[,1] == "201609"),11])
v201610<-sum(producto5Fal[which(producto5Fal[,1] == "201610"),11])
v201611<-sum(producto5Fal[which(producto5Fal[,1] == "201611"),11])
v201612<-sum(producto5Fal[which(producto5Fal[,1] == "201612"),11])

pro52016<-rbind(v201601, v201602, v201603, v201604,v201605, v201606, v201607, v201608, v201609, v201610, v201611, v201612)
pro52016<-as.data.frame(pro52016)
pro52016$Anio <- 2016
pro52016$Mes<-mesesN
View(pro52016)

# 2017
v201701<-sum(producto5Fal[which(producto5Fal[,1] == "201701"),11])
v201702<-sum(producto5Fal[which(producto5Fal[,1] == "201702"),11])
v201703<-sum(producto5Fal[which(producto5Fal[,1] == "201703"),11])
v201704<-sum(producto5Fal[which(producto5Fal[,1] == "201704"),11])
v201705<-sum(producto5Fal[which(producto5Fal[,1] == "201705"),11])
v201706<-sum(producto5Fal[which(producto5Fal[,1] == "201706"),11])
v201707<-sum(producto5Fal[which(producto5Fal[,1] == "201707"),11])
v201708<-sum(producto5Fal[which(producto5Fal[,1] == "201708"),11])
v201709<-sum(producto5Fal[which(producto5Fal[,1] == "201709"),11])
v201710<-sum(producto5Fal[which(producto5Fal[,1] == "201710"),11])
v201711<-sum(producto5Fal[which(producto5Fal[,1] == "201711"),11])
v201712<-sum(producto5Fal[which(producto5Fal[,1] == "201712"),11])

pro52017<-rbind(v201701, v201702, v201703, v201704,v201705, v201706, v201707, v201708, v201709, v201710, v201711, v201712)
pro52017<-as.data.frame(pro52017)
pro52017$Anio <- 2017
pro52017$Mes<-mesesN
View(pro42017)

# 2018
v201801<-sum(producto5Fal[which(producto5Fal[,1] == "201801"),11])
v201802<-sum(producto5Fal[which(producto5Fal[,1] == "201802"),11])
v201803<-sum(producto5Fal[which(producto5Fal[,1] == "201803"),11])
v201804<-sum(producto5Fal[which(producto5Fal[,1] == "201804"),11])
v201805<-sum(producto5Fal[which(producto5Fal[,1] == "201805"),11])
v201806<-sum(producto5Fal[which(producto5Fal[,1] == "201806"),11])
v201807<-sum(producto5Fal[which(producto5Fal[,1] == "201807"),11])
v201808<-sum(producto5Fal[which(producto5Fal[,1] == "201808"),11])
v201809<-sum(producto5Fal[which(producto5Fal[,1] == "201809"),11])
v201810<-sum(producto5Fal[which(producto5Fal[,1] == "201810"),11])
v201811<-sum(producto5Fal[which(producto5Fal[,1] == "201811"),11])
v201812<-sum(producto5Fal[which(producto5Fal[,1] == "201812"),11])

pro52018<-rbind(v201801, v201802, v201803, v201804,v201805, v201806, v201807, v201808, v201809, v201810, v201811, v201812)
pro52018<-as.data.frame(pro52018)
pro52018$Anio <- 2018
pro52018$Mes<-mesesN
View(pro52018)

# 2019
v201901<-sum(producto4Fal[which(producto4Fal[,1] == "201901"),11])
v201902<-sum(producto4Fal[which(producto4Fal[,1] == "201902"),11])
v201903<-sum(producto4Fal[which(producto4Fal[,1] == "201901"),11])
v201904<-sum(producto4Fal[which(producto4Fal[,1] == "201902"),11])
v201905<-sum(producto4Fal[which(producto4Fal[,1] == "201901"),11])
v201906<-sum(producto4Fal[which(producto4Fal[,1] == "201902"),11])
v201907<-sum(producto4Fal[which(producto4Fal[,1] == "201901"),11])
v201908<-sum(producto4Fal[which(producto4Fal[,1] == "201902"),11])

pro42019<-rbind(v201901, v201902, v201903, v201904,v201905, v201906, v201907, v201908)
pro42019<-as.data.frame(pro42019)
pro42019$Anio <- 2019
pro42019$Mes<-mesesO
View(pro42019)

vector<-rbind(pro52015, pro52016, pro52017, pro52018)
View(vector)

# TIMESERIES

# save a numeric vector containing 72 monthly observations
# from Jan 2015 to Dec 2018 as a time series object
myts <- ts(vector, start=c(2015, 1), end=c(2018, 12), frequency=12)
colnames(myts)[1] <- "Unidades Vendidas"

start(myts)
end(myts)
frequency(myts)

plot(myts, type="l", main = "Serie de Tiempo de  PRO 5")
ts.plot(myts)

decomP2<-decompose(myts)
plot(decomP2)

#Grafico sobre el comportamiento de la media en subsets de los anios
plot(aggregate(myts,FUN=mean))
View(myts)

#descomponemos la serie de tiempo en tendencia, estacionaridad e innovacion
plot(decomP2$trend)
plot(decomP1$seasonal)

#forecasting
fp1<-auto.arima(myts[,1])
fp1

# Forecast for the next 10 time units
arima_forecast = forecast(fp1, h = 8, level = c(80,95))
p1<-predict(fp1, n.ahead = 1*12)

# Plot forecasts
plot(arima_forecast, main = "Serie de Tiempo para el PRO 5", xlab = "Tiempo", ylab = "Unidades Vendidas")
myprono<-arima_forecast$mean
suprono<-



#Aplicaremos una transformacion logaritmica
myts[10] <- 0
croston<-croston(myts[,1], h = 8)
plot(croston, main ="Prediccion para el Producto 2", xlab = "Tiempo", ylab = "Unidades Vendidas")
croston$fitted
croston

p1<-sum(producto5Fal[which(producto5Fal[,1] == "201901"),11])
p2<-sum(producto5Fal[which(producto5Fal[,1] == "201902"),11])
p3<-sum(producto5Fal[which(producto5Fal[,1] == "201903"),11])
p4<-sum(producto5Fal[which(producto5Fal[,1] == "201904"),11])
p5<-sum(producto5Fal[which(producto5Fal[,1] == "201905"),11])
p6<-sum(producto5Fal[which(producto5Fal[,1] == "201906"),11])
p7<-sum(producto5Fal[which(producto5Fal[,1] == "201907"),11])
p8<-sum(producto5Fal[which(producto5Fal[,1] == "201908"),11])
pronosticoMV<-rbind(p1,p2,p3,p4,p5,p6,p7,p8)
pronosticoMV
sum(pronosticoMV)


View(ventasP52019)

ventasP52019<-as.data.frame(ventasP52019)
ventasP52019$Viejo<-pronosticoMV
ventasP52019$Nuevo<-myprono

sss<-abs(ventasP52019$V1 - ventasP52019$Viejo)
sss<-abs(ventasP52019$V1 - ventasP52019$Nuevo)

sum(ventasP52019$V1)



#######################
### TOP 5 PRODUCTOS EXCEDENTES
## 4123310125      
## 4123351013     
## 4123310126      
## 4123042257      
## 4123811146      

producto1Fal<-PagCentroamerica[PagCentroamerica$Producto == "4123310125 ",]
producto2Fal<-PagCentroamerica[PagCentroamerica$Producto == "4123351013 ",]
producto3Fal<-PagCentroamerica[PagCentroamerica$Producto == "4123310126 ",]
producto4Fal<-PagCentroamerica[PagCentroamerica$Producto == "4123042257 ",]
producto5Fal<-PagCentroamerica[PagCentroamerica$Producto == "4123811146 ",]


########################################### PRODUCTO 1 ########################################### 
# sumamos todas las ventas por anio
# 2015
View(producto1Fal)
v201501<-sum(producto1Fal[which(producto1Fal[,1] == "201501"),11])
v201502<-sum(producto1Fal[which(producto1Fal[,1] == "201502"),11])
v201503<-sum(producto1Fal[which(producto1Fal[,1] == "201503"),11])
v201504<-sum(producto1Fal[which(producto1Fal[,1] == "201504"),11])
v201505<-sum(producto1Fal[which(producto1Fal[,1] == "201505"),11])
v201506<-sum(producto1Fal[which(producto1Fal[,1] == "201506"),11])
v201507<-sum(producto1Fal[which(producto1Fal[,1] == "201507"),11])
v201508<-sum(producto1Fal[which(producto1Fal[,1] == "201508"),11])
v201509<-sum(producto1Fal[which(producto1Fal[,1] == "201509"),11])
v201510<-sum(producto1Fal[which(producto1Fal[,1] == "201510"),11])
v201511<-sum(producto1Fal[which(producto1Fal[,1] == "201511"),11])
v201512<-sum(producto1Fal[which(producto1Fal[,1] == "201512"),11])

pro12015<-rbind(v201501, v201502, v201503, v201504,v201505, v201506, v201507, v201508, v201509, v201510, v201511, v201512)
pro12015<-as.data.frame(pro12015)
pro12015$Anio <- 2015
pro12015$Mes<-mesesN
View(pro12015)

# 2016
v201601<-sum(producto1Fal[which(producto1Fal[,1] == "201601"),11])
v201602<-sum(producto1Fal[which(producto1Fal[,1] == "201602"),11])
v201603<-sum(producto1Fal[which(producto1Fal[,1] == "201603"),11])
v201604<-sum(producto1Fal[which(producto1Fal[,1] == "201604"),11])
v201605<-sum(producto1Fal[which(producto1Fal[,1] == "201605"),11])
v201606<-sum(producto1Fal[which(producto1Fal[,1] == "201606"),11])
v201607<-sum(producto1Fal[which(producto1Fal[,1] == "201607"),11])
v201608<-sum(producto1Fal[which(producto1Fal[,1] == "201608"),11])
v201609<-sum(producto1Fal[which(producto1Fal[,1] == "201609"),11])
v201610<-sum(producto1Fal[which(producto1Fal[,1] == "201610"),11])
v201611<-sum(producto1Fal[which(producto1Fal[,1] == "201611"),11])
v201612<-sum(producto1Fal[which(producto1Fal[,1] == "201612"),11])

pro12016<-rbind(v201601, v201602, v201603, v201604,v201605, v201606, v201607, v201608, v201609, v201610, v201611, v201612)
pro12016<-as.data.frame(pro12016)
pro12016$Anio <- 2016
pro12016$Mes<-mesesN
View(pro12016)

# 2017
v201701<-sum(producto1Fal[which(producto1Fal[,1] == "201701"),11])
v201702<-sum(producto1Fal[which(producto1Fal[,1] == "201702"),11])
v201703<-sum(producto1Fal[which(producto1Fal[,1] == "201703"),11])
v201704<-sum(producto1Fal[which(producto1Fal[,1] == "201704"),11])
v201705<-sum(producto1Fal[which(producto1Fal[,1] == "201705"),11])
v201706<-sum(producto1Fal[which(producto1Fal[,1] == "201706"),11])
v201707<-sum(producto1Fal[which(producto1Fal[,1] == "201707"),11])
v201708<-sum(producto1Fal[which(producto1Fal[,1] == "201708"),11])
v201709<-sum(producto1Fal[which(producto1Fal[,1] == "201709"),11])
v201710<-sum(producto1Fal[which(producto1Fal[,1] == "201710"),11])
v201711<-sum(producto1Fal[which(producto1Fal[,1] == "201711"),11])
v201712<-sum(producto1Fal[which(producto1Fal[,1] == "201712"),11])

pro12017<-rbind(v201701, v201702, v201703, v201704,v201705, v201706, v201707, v201708, v201709, v201710, v201711, v201712)
pro12017<-as.data.frame(pro12017)
pro12017$Anio <- 2017
pro12017$Mes<-mesesN
View(pro12017)

# 2018
v201801<-sum(producto1Fal[which(producto1Fal[,1] == "201801"),11])
v201802<-sum(producto1Fal[which(producto1Fal[,1] == "201802"),11])
v201803<-sum(producto1Fal[which(producto1Fal[,1] == "201803"),11])
v201804<-sum(producto1Fal[which(producto1Fal[,1] == "201804"),11])
v201805<-sum(producto1Fal[which(producto1Fal[,1] == "201805"),11])
v201806<-sum(producto1Fal[which(producto1Fal[,1] == "201806"),11])
v201807<-sum(producto1Fal[which(producto1Fal[,1] == "201807"),11])
v201808<-sum(producto1Fal[which(producto1Fal[,1] == "201808"),11])
v201809<-sum(producto1Fal[which(producto1Fal[,1] == "201809"),11])
v201810<-sum(producto1Fal[which(producto1Fal[,1] == "201810"),11])
v201811<-sum(producto1Fal[which(producto1Fal[,1] == "201811"),11])
v201812<-sum(producto1Fal[which(producto1Fal[,1] == "201812"),11])

pro12018<-rbind(v201801, v201802, v201803, v201804,v201805, v201806, v201807, v201808, v201809, v201810, v201811, v201812)
pro12018<-as.data.frame(pro12018)
pro12018$Anio <- 2018
pro12018$Mes<-mesesN
View(pro12018)

# 2019
v201901<-sum(producto1Fal[which(producto1Fal[,1] == "201901"),11])
v201902<-sum(producto1Fal[which(producto1Fal[,1] == "201902"),11])


# TIMESERIES
vector<-rbind(pro12015, pro12016, pro12017, pro12018)
View(vector)

# save a numeric vector containing 72 monthly observations
# from Jan 2009 to Dec 2014 as a time series object
myts <- ts(vector, start=c(2015, 1), end=c(2018, 12), frequency=12)
colnames(myts)[1] <- "Unidades Vendidas"

start(myts)
end(myts)
frequency(myts)

plot(myts, type="l", main = "Serie de Tiempo de  PRO 1")
ts.plot(myts)

decomP2<-decompose(myts)
plot(decomP2)

#Grafico sobre el comportamiento de la media en subsets de los anios
plot(aggregate(myts,FUN=mean))
View(myts)

#descomponemos la serie de tiempo en tendencia, estacionaridad e innovacion
plot(decomP2$trend)
plot(decomP1$seasonal)

#forecasting
fp1<-auto.arima(myts[,1])
fp1

# Forecast for the next 10 time units
arima_forecast = forecast(fp1, h = 8, level = c(80,95))
p1<-predict(fp1, n.ahead = 1*12)

# Plot forecasts
plot(arima_forecast, main = "Serie de Tiempo para el PRO 1", xlab = "Tiempo", ylab = "Unidades Vendidas")
myprono<-arima_forecast$mean
myprono

########################################### PRODUCTO 2 ########################################### 
# sumamos todas las ventas por anio
# 2015
View(producto2Fal$Descripcion)
v201501<-sum(producto2Fal[which(producto2Fal[,1] == "201501"),11])
v201502<-sum(producto2Fal[which(producto2Fal[,1] == "201502"),11])
v201503<-sum(producto2Fal[which(producto2Fal[,1] == "201503"),11])
v201504<-sum(producto2Fal[which(producto2Fal[,1] == "201504"),11])
v201505<-sum(producto2Fal[which(producto2Fal[,1] == "201505"),11])
v201506<-sum(producto2Fal[which(producto2Fal[,1] == "201506"),11])
v201507<-sum(producto2Fal[which(producto2Fal[,1] == "201507"),11])
v201508<-sum(producto2Fal[which(producto2Fal[,1] == "201508"),11])
v201509<-sum(producto2Fal[which(producto2Fal[,1] == "201509"),11])
v201510<-sum(producto2Fal[which(producto2Fal[,1] == "201510"),11])
v201511<-sum(producto2Fal[which(producto2Fal[,1] == "201511"),11])
v201512<-sum(producto2Fal[which(producto2Fal[,1] == "201512"),11])

pro22015<-rbind(v201501, v201502, v201503, v201504,v201505, v201506, v201507, v201508, v201509, v201510, v201511, v201512)
pro22015<-as.data.frame(pro22015)
pro22015$Anio <- 2015
pro22015$Mes<-mesesN
View(pro22015)

# 2016
v201601<-sum(producto2Fal[which(producto2Fal[,1] == "201601"),11])
v201602<-sum(producto2Fal[which(producto2Fal[,1] == "201602"),11])
v201603<-sum(producto2Fal[which(producto2Fal[,1] == "201603"),11])
v201604<-sum(producto2Fal[which(producto2Fal[,1] == "201604"),11])
v201605<-sum(producto2Fal[which(producto2Fal[,1] == "201605"),11])
v201606<-sum(producto2Fal[which(producto2Fal[,1] == "201606"),11])
v201607<-sum(producto2Fal[which(producto2Fal[,1] == "201607"),11])
v201608<-sum(producto2Fal[which(producto2Fal[,1] == "201608"),11])
v201609<-sum(producto2Fal[which(producto2Fal[,1] == "201609"),11])
v201610<-sum(producto2Fal[which(producto2Fal[,1] == "201610"),11])
v201611<-sum(producto2Fal[which(producto2Fal[,1] == "201611"),11])
v201612<-sum(producto2Fal[which(producto2Fal[,1] == "201612"),11])

pro22016<-rbind(v201601, v201602, v201603, v201604,v201605, v201606, v201607, v201608, v201609, v201610, v201611, v201612)
pro22016<-as.data.frame(pro22016)
pro22016$Anio <- 2016
pro22016$Mes<-mesesN
View(pro22016)

# 2017
v201701<-sum(producto2Fal[which(producto2Fal[,1] == "201701"),11])
v201702<-sum(producto2Fal[which(producto2Fal[,1] == "201702"),11])
v201703<-sum(producto2Fal[which(producto2Fal[,1] == "201703"),11])
v201704<-sum(producto2Fal[which(producto2Fal[,1] == "201704"),11])
v201705<-sum(producto2Fal[which(producto2Fal[,1] == "201705"),11])
v201706<-sum(producto2Fal[which(producto2Fal[,1] == "201706"),11])
v201707<-sum(producto2Fal[which(producto2Fal[,1] == "201707"),11])
v201708<-sum(producto2Fal[which(producto2Fal[,1] == "201708"),11])
v201709<-sum(producto2Fal[which(producto2Fal[,1] == "201709"),11])
v201710<-sum(producto2Fal[which(producto2Fal[,1] == "201710"),11])
v201711<-sum(producto2Fal[which(producto2Fal[,1] == "201711"),11])
v201712<-sum(producto2Fal[which(producto2Fal[,1] == "201712"),11])

pro22017<-rbind(v201701, v201702, v201703, v201704,v201705, v201706, v201707, v201708, v201709, v201710, v201711, v201712)
pro22017<-as.data.frame(pro22017)
pro22017$Anio <- 2017
pro22017$Mes<-mesesN
View(pro22017)

# 2018
v201801<-sum(producto2Fal[which(producto2Fal[,1] == "201801"),11])
v201802<-sum(producto2Fal[which(producto2Fal[,1] == "201802"),11])
v201803<-sum(producto2Fal[which(producto2Fal[,1] == "201803"),11])
v201804<-sum(producto2Fal[which(producto2Fal[,1] == "201804"),11])
v201805<-sum(producto2Fal[which(producto2Fal[,1] == "201805"),11])
v201806<-sum(producto2Fal[which(producto2Fal[,1] == "201806"),11])
v201807<-sum(producto2Fal[which(producto2Fal[,1] == "201807"),11])
v201808<-sum(producto2Fal[which(producto2Fal[,1] == "201808"),11])
v201809<-sum(producto2Fal[which(producto2Fal[,1] == "201809"),11])
v201810<-sum(producto2Fal[which(producto2Fal[,1] == "201810"),11])
v201811<-sum(producto2Fal[which(producto2Fal[,1] == "201811"),11])
v201812<-sum(producto2Fal[which(producto2Fal[,1] == "201812"),11])

pro22018<-rbind(v201801, v201802, v201803, v201804,v201805, v201806, v201807, v201808, v201809, v201810, v201811, v201812)
pro22018<-as.data.frame(pro22018)
pro22018$Anio <- 2018
pro22018$Mes<-mesesN
View(pro22018)

# 2019
v201901<-sum(producto2Fal[which(producto2Fal[,1] == "201901"),11])
v201902<-sum(producto2Fal[which(producto2Fal[,1] == "201902"),11])
v201903<-sum(producto2Fal[which(producto2Fal[,1] == "201901"),11])
v201904<-sum(producto2Fal[which(producto2Fal[,1] == "201902"),11])
v201905<-sum(producto2Fal[which(producto2Fal[,1] == "201901"),11])
v201906<-sum(producto2Fal[which(producto2Fal[,1] == "201902"),11])
v201907<-sum(producto2Fal[which(producto2Fal[,1] == "201901"),11])
v201908<-sum(producto2Fal[which(producto2Fal[,1] == "201902"),11])

pro22019<-rbind(v201901, v201902, v201903, v201904,v201905, v201906, v201907, v201908)
pro22019<-as.data.frame(pro22019)
pro22019$Anio <- 2019
pro22019$Mes<-mesesO
View(pro22019)

vector<-rbind(pro22015, pro22016, pro22017, pro22018)
View(vector)

# save a numeric vector containing 72 monthly observations
# from Jan 2009 to Dec 2014 as a time series object
myts <- ts(vector, start=c(2015, 1), end=c(2018, 12), frequency=12)
colnames(myts)[1] <- "Unidades Vendidas"

start(myts)
end(myts)
frequency(myts)

plot(myts, type="l", main = "Serie de Tiempo de  PRO 2")
ts.plot(myts)

decomP2<-decompose(myts)
plot(decomP2)

#Grafico sobre el comportamiento de la media en subsets de los anios
plot(aggregate(myts,FUN=mean))

#descomponemos la serie de tiempo en tendencia, estacionaridad e innovacion
plot(decomP2$trend)
plot(decomP1$seasonal)

#forecasting
fp1<-auto.arima(myts[,1])
fp1

# Forecast for the next 10 time units
arima_forecast = forecast(fp1, h = 1, level = c(80,95))
p1<-predict(fp1, n.ahead = 1*12)

# Plot forecasts
plot(arima_forecast, main = "Serie de Tiempo para el PRO 3", xlab = "Tiempo", ylab = "Unidades Vendidas")
myprono<-arima_forecast$mean
myprono

View(producto2Fal)
sum(myprono)

v201901<-sum(producto2Fal[which(producto2Fal[,1] == "201901"),10])
v201902<-sum(producto2Fal[which(producto2Fal[,1] == "201902"),10])
v201903<-sum(producto2Fal[which(producto2Fal[,1] == "201901"),10])
v201904<-sum(producto2Fal[which(producto2Fal[,1] == "201902"),10])
v201905<-sum(producto2Fal[which(producto2Fal[,1] == "201901"),10])
v201906<-sum(producto2Fal[which(producto2Fal[,1] == "201902"),10])
v201907<-sum(producto2Fal[which(producto2Fal[,1] == "201901"),10])
v201908<-sum(producto2Fal[which(producto2Fal[,1] == "201902"),10])

pro22019<-rbind(v201901, v201902, v201903, v201904,v201905, v201906, v201907, v201908)
pro22019<-as.data.frame(pro22019)
pro22019$Anio <- 2019
pro22019$Mes<-mesesO
View(producto2Fal)
sum(pro22019)

########################################### PRODUCTO 3 ########################################### 
# sumamos todas las ventas por anio
# 2015

v201501<-sum(producto3Fal[which(producto3Fal[,1] == "201501"),11])
v201502<-sum(producto3Fal[which(producto3Fal[,1] == "201502"),11])
v201503<-sum(producto3Fal[which(producto3Fal[,1] == "201503"),11])
v201504<-sum(producto3Fal[which(producto3Fal[,1] == "201504"),11])
v201505<-sum(producto3Fal[which(producto3Fal[,1] == "201505"),11])
v201506<-sum(producto3Fal[which(producto3Fal[,1] == "201506"),11])
v201507<-sum(producto3Fal[which(producto3Fal[,1] == "201507"),11])
v201508<-sum(producto3Fal[which(producto3Fal[,1] == "201508"),11])
v201509<-sum(producto3Fal[which(producto3Fal[,1] == "201509"),11])
v201510<-sum(producto3Fal[which(producto3Fal[,1] == "201510"),11])
v201511<-sum(producto3Fal[which(producto3Fal[,1] == "201511"),11])
v201512<-sum(producto3Fal[which(producto3Fal[,1] == "201512"),11])

pro32015<-rbind(v201501, v201502, v201503, v201504,v201505, v201506, v201507, v201508, v201509, v201510, v201511, v201512)
pro32015<-as.data.frame(pro32015)
pro32015$Anio <- 2015
pro32015$Mes<-mesesN
View(pro32015)

# 2016
v201601<-sum(producto3Fal[which(producto3Fal[,1] == "201601"),11])
v201602<-sum(producto3Fal[which(producto3Fal[,1] == "201602"),11])
v201603<-sum(producto3Fal[which(producto3Fal[,1] == "201603"),11])
v201604<-sum(producto3Fal[which(producto3Fal[,1] == "201604"),11])
v201605<-sum(producto3Fal[which(producto3Fal[,1] == "201605"),11])
v201606<-sum(producto3Fal[which(producto3Fal[,1] == "201606"),11])
v201607<-sum(producto3Fal[which(producto3Fal[,1] == "201607"),11])
v201608<-sum(producto3Fal[which(producto3Fal[,1] == "201608"),11])
v201609<-sum(producto3Fal[which(producto3Fal[,1] == "201609"),11])
v201610<-sum(producto3Fal[which(producto3Fal[,1] == "201610"),11])
v201611<-sum(producto3Fal[which(producto3Fal[,1] == "201611"),11])
v201612<-sum(producto3Fal[which(producto3Fal[,1] == "201612"),11])

pro32016<-rbind(v201601, v201602, v201603, v201604,v201605, v201606, v201607, v201608, v201609, v201610, v201611, v201612)
pro32016<-as.data.frame(pro32016)
pro32016$Anio <- 2016
pro32016$Mes<-mesesN
View(pro32016)

# 2017
v201701<-sum(producto3Fal[which(producto3Fal[,1] == "201701"),11])
v201702<-sum(producto3Fal[which(producto3Fal[,1] == "201702"),11])
v201703<-sum(producto3Fal[which(producto3Fal[,1] == "201703"),11])
v201704<-sum(producto3Fal[which(producto3Fal[,1] == "201704"),11])
v201705<-sum(producto3Fal[which(producto3Fal[,1] == "201705"),11])
v201706<-sum(producto3Fal[which(producto3Fal[,1] == "201706"),11])
v201707<-sum(producto3Fal[which(producto3Fal[,1] == "201707"),11])
v201708<-sum(producto3Fal[which(producto3Fal[,1] == "201708"),11])
v201709<-sum(producto3Fal[which(producto3Fal[,1] == "201709"),11])
v201710<-sum(producto3Fal[which(producto3Fal[,1] == "201710"),11])
v201711<-sum(producto3Fal[which(producto3Fal[,1] == "201711"),11])
v201712<-sum(producto3Fal[which(producto3Fal[,1] == "201712"),11])

pro32017<-rbind(v201701, v201702, v201703, v201704,v201705, v201706, v201707, v201708, v201709, v201710, v201711, v201712)
pro32017<-as.data.frame(pro32017)
pro32017$Anio <- 2017
pro32017$Mes<-mesesN
View(pro32017)

# 2018
v201801<-sum(producto3Fal[which(producto3Fal[,1] == "201801"),11])
v201802<-sum(producto3Fal[which(producto3Fal[,1] == "201802"),11])
v201803<-sum(producto3Fal[which(producto3Fal[,1] == "201803"),11])
v201804<-sum(producto3Fal[which(producto3Fal[,1] == "201804"),11])
v201805<-sum(producto3Fal[which(producto3Fal[,1] == "201805"),11])
v201806<-sum(producto3Fal[which(producto3Fal[,1] == "201806"),11])
v201807<-sum(producto3Fal[which(producto3Fal[,1] == "201807"),11])
v201808<-sum(producto3Fal[which(producto3Fal[,1] == "201808"),11])
v201809<-sum(producto3Fal[which(producto3Fal[,1] == "201809"),11])
v201810<-sum(producto3Fal[which(producto3Fal[,1] == "201810"),11])
v201811<-sum(producto3Fal[which(producto3Fal[,1] == "201811"),11])
v201812<-sum(producto3Fal[which(producto3Fal[,1] == "201812"),11])

pro32018<-rbind(v201801, v201802, v201803, v201804,v201805, v201806, v201807, v201808, v201809, v201810, v201811, v201812)
pro32018<-as.data.frame(pro32018)
pro32018$Anio <- 2018
pro32018$Mes<-mesesN
View(pro32018)

# 2019
v201901<-sum(producto3Fal[which(producto3Fal[,1] == "201901"),11])
v201902<-sum(producto3Fal[which(producto3Fal[,1] == "201902"),11])
v201903<-sum(producto3Fal[which(producto3Fal[,1] == "201901"),11])
v201904<-sum(producto3Fal[which(producto3Fal[,1] == "201902"),11])
v201905<-sum(producto3Fal[which(producto3Fal[,1] == "201901"),11])
v201906<-sum(producto3Fal[which(producto3Fal[,1] == "201902"),11])
v201907<-sum(producto3Fal[which(producto3Fal[,1] == "201901"),11])
v201908<-sum(producto3Fal[which(producto3Fal[,1] == "201902"),11])

pro32019<-rbind(v201901, v201902, v201903, v201904,v201905, v201906, v201907, v201908)
pro32019<-as.data.frame(pro32019)
pro32019$Anio <- 2019
pro32019$Mes<-mesesO
View(pro32019)

vector<-rbind(pro32015, pro32016, pro32017, pro32018)
View(vector)

# save a numeric vector containing 72 monthly observations
# from Jan 2009 to Dec 2014 as a time series object
myts <- ts(vector, start=c(2015, 1), end=c(2018, 12), frequency=12)
colnames(myts)[1] <- "Unidades Vendidas"

start(myts)
end(myts)
frequency(myts)

plot(myts, type="l", main = "Serie de Tiempo de  PRO 2")
ts.plot(myts)

decomP2<-decompose(myts)
plot(decomP2)

#Grafico sobre el comportamiento de la media en subsets de los anios
plot(aggregate(myts,FUN=mean))

#descomponemos la serie de tiempo en tendencia, estacionaridad e innovacion
plot(decomP2$trend)
plot(decomP1$seasonal)

#forecasting
fp1<-auto.arima(myts[,1])
fp1

# Forecast for the next 10 time units
arima_forecast = forecast(fp1, h = 1, level = c(80,95))
p1<-predict(fp1, n.ahead = 1*12)

# Plot forecasts
plot(arima_forecast, main = "Serie de Tiempo para el PRO 3", xlab = "Tiempo", ylab = "Unidades Vendidas")
myprono<-arima_forecast$mean
myprono

View(producto3Fal)
sum(myprono)


########################################### PRODUCTO 4 ########################################### 
# sumamos todas las ventas por anio
# 2015

v201501<-sum(producto4Fal[which(producto4Fal[,1] == "201501"),11])
v201502<-sum(producto4Fal[which(producto4Fal[,1] == "201502"),11])
v201503<-sum(producto4Fal[which(producto4Fal[,1] == "201503"),11])
v201504<-sum(producto4Fal[which(producto4Fal[,1] == "201504"),11])
v201505<-sum(producto4Fal[which(producto4Fal[,1] == "201505"),11])
v201506<-sum(producto4Fal[which(producto4Fal[,1] == "201506"),11])
v201507<-sum(producto4Fal[which(producto4Fal[,1] == "201507"),11])
v201508<-sum(producto4Fal[which(producto4Fal[,1] == "201508"),11])
v201509<-sum(producto4Fal[which(producto4Fal[,1] == "201509"),11])
v201510<-sum(producto4Fal[which(producto4Fal[,1] == "201510"),11])
v201511<-sum(producto4Fal[which(producto4Fal[,1] == "201511"),11])
v201512<-sum(producto4Fal[which(producto4Fal[,1] == "201512"),11])

pro42015<-rbind(v201501, v201502, v201503, v201504,v201505, v201506, v201507, v201508, v201509, v201510, v201511, v201512)
pro42015<-as.data.frame(pro42015)
pro42015$Anio <- 2015
pro42015$Mes<-mesesN
View(pro42015)

# 2016
v201601<-sum(producto4Fal[which(producto4Fal[,1] == "201601"),11])
v201602<-sum(producto4Fal[which(producto4Fal[,1] == "201602"),11])
v201603<-sum(producto4Fal[which(producto4Fal[,1] == "201603"),11])
v201604<-sum(producto4Fal[which(producto4Fal[,1] == "201604"),11])
v201605<-sum(producto4Fal[which(producto4Fal[,1] == "201605"),11])
v201606<-sum(producto4Fal[which(producto4Fal[,1] == "201606"),11])
v201607<-sum(producto4Fal[which(producto4Fal[,1] == "201607"),11])
v201608<-sum(producto4Fal[which(producto4Fal[,1] == "201608"),11])
v201609<-sum(producto4Fal[which(producto4Fal[,1] == "201609"),11])
v201610<-sum(producto4Fal[which(producto4Fal[,1] == "201610"),11])
v201611<-sum(producto4Fal[which(producto4Fal[,1] == "201611"),11])
v201612<-sum(producto4Fal[which(producto4Fal[,1] == "201612"),11])

pro42016<-rbind(v201601, v201602, v201603, v201604,v201605, v201606, v201607, v201608, v201609, v201610, v201611, v201612)
pro42016<-as.data.frame(pro42016)
pro42016$Anio <- 2016
pro42016$Mes<-mesesN
View(pro42016)

# 2017
v201701<-sum(producto4Fal[which(producto4Fal[,1] == "201701"),11])
v201702<-sum(producto4Fal[which(producto4Fal[,1] == "201702"),11])
v201703<-sum(producto4Fal[which(producto4Fal[,1] == "201703"),11])
v201704<-sum(producto4Fal[which(producto4Fal[,1] == "201704"),11])
v201705<-sum(producto4Fal[which(producto4Fal[,1] == "201705"),11])
v201706<-sum(producto4Fal[which(producto4Fal[,1] == "201706"),11])
v201707<-sum(producto4Fal[which(producto4Fal[,1] == "201707"),11])
v201708<-sum(producto4Fal[which(producto4Fal[,1] == "201708"),11])
v201709<-sum(producto4Fal[which(producto4Fal[,1] == "201709"),11])
v201710<-sum(producto4Fal[which(producto4Fal[,1] == "201710"),11])
v201711<-sum(producto4Fal[which(producto4Fal[,1] == "201711"),11])
v201712<-sum(producto4Fal[which(producto4Fal[,1] == "201712"),11])

pro42017<-rbind(v201701, v201702, v201703, v201704,v201705, v201706, v201707, v201708, v201709, v201710, v201711, v201712)
pro42017<-as.data.frame(pro42017)
pro42017$Anio <- 2017
pro42017$Mes<-mesesN
View(pro42017)

# 2018
v201801<-sum(producto4Fal[which(producto4Fal[,1] == "201801"),11])
v201802<-sum(producto4Fal[which(producto4Fal[,1] == "201802"),11])
v201803<-sum(producto4Fal[which(producto4Fal[,1] == "201803"),11])
v201804<-sum(producto4Fal[which(producto4Fal[,1] == "201804"),11])
v201805<-sum(producto4Fal[which(producto4Fal[,1] == "201805"),11])
v201806<-sum(producto4Fal[which(producto4Fal[,1] == "201806"),11])
v201807<-sum(producto4Fal[which(producto4Fal[,1] == "201807"),11])
v201808<-sum(producto4Fal[which(producto4Fal[,1] == "201808"),11])
v201809<-sum(producto4Fal[which(producto4Fal[,1] == "201809"),11])
v201810<-sum(producto4Fal[which(producto4Fal[,1] == "201810"),11])
v201811<-sum(producto4Fal[which(producto4Fal[,1] == "201811"),11])
v201812<-sum(producto4Fal[which(producto4Fal[,1] == "201812"),11])

pro42018<-rbind(v201801, v201802, v201803, v201804,v201805, v201806, v201807, v201808, v201809, v201810, v201811, v201812)
pro42018<-as.data.frame(pro42018)
pro42018$Anio <- 2018
pro42018$Mes<-mesesN
View(pro42018)

# 2019
v201901<-sum(producto4Fal[which(producto4Fal[,1] == "201901"),11])
v201902<-sum(producto4Fal[which(producto4Fal[,1] == "201902"),11])
v201903<-sum(producto4Fal[which(producto4Fal[,1] == "201901"),11])
v201904<-sum(producto4Fal[which(producto4Fal[,1] == "201902"),11])
v201905<-sum(producto4Fal[which(producto4Fal[,1] == "201901"),11])
v201906<-sum(producto4Fal[which(producto4Fal[,1] == "201902"),11])
v201907<-sum(producto4Fal[which(producto4Fal[,1] == "201901"),11])
v201908<-sum(producto4Fal[which(producto4Fal[,1] == "201902"),11])

pro42019<-rbind(v201901, v201902, v201903, v201904,v201905, v201906, v201907, v201908)
pro42019<-as.data.frame(pro42019)
pro42019$Anio <- 2019
pro42019$Mes<-mesesO
View(pro42019)

vector<-rbind(pro42015, pro42016, pro42017, pro42018)
View(vector)

# TIMESERIES

# save a numeric vector containing 72 monthly observations
# from Jan 2015 to Dec 2018 as a time series object
myts <- ts(vector, start=c(2015, 1), end=c(2018, 12), frequency=12)
colnames(myts)[1] <- "Unidades Vendidas"

start(myts)
end(myts)
frequency(myts)

plot(myts, type="l", main = "Serie de Tiempo de  PRO 4")
ts.plot(myts)

decomP2<-decompose(myts)
plot(decomP2)

#Grafico sobre el comportamiento de la media en subsets de los anios
plot(aggregate(myts,FUN=mean))
View(myts)

#descomponemos la serie de tiempo en tendencia, estacionaridad e innovacion
plot(decomP2$trend)
plot(decomP1$seasonal)

#forecasting
fp1<-auto.arima(myts[,1])
fp1

# Forecast for the next 10 time units
arima_forecast = forecast(fp1, h = 1, level = c(80,95))
arima_forecast

# Plot forecasts
plot(arima_forecast, main = "Serie de Tiempo para el PRO 4", xlab = "Tiempo", ylab = "Unidades Vendidas")
myprono<-arima_forecast$mean

View(producto4Fal)


p1<-sum(producto4Fal[which(producto4Fal[,1] == "201901"),10])
p2<-sum(producto4Fal[which(producto4Fal[,1] == "201902"),10])
p3<-sum(producto4Fal[which(producto4Fal[,1] == "201903"),10])
p4<-sum(producto4Fal[which(producto4Fal[,1] == "201904"),10])
p5<-sum(producto4Fal[which(producto4Fal[,1] == "201905"),10])
p6<-sum(producto4Fal[which(producto4Fal[,1] == "201906"),10])
p7<-sum(producto4Fal[which(producto4Fal[,1] == "201907"),10])
p8<-sum(producto4Fal[which(producto4Fal[,1] == "201908"),10])
pronosticoMV<-rbind(p1,p2,p3,p4,p5,p6,p7,p8)
pronosticoMV
sum(pronosticoMV)


View(ventasP52019)



########################################### PRODUCTO 5 ########################################### 
# sumamos todas las ventas por anio
# 2015

v201501<-sum(producto5Fal[which(producto5Fal[,1] == "201501"),11])
v201502<-sum(producto5Fal[which(producto5Fal[,1] == "201502"),11])
v201503<-sum(producto5Fal[which(producto5Fal[,1] == "201503"),11])
v201504<-sum(producto5Fal[which(producto5Fal[,1] == "201504"),11])
v201505<-sum(producto5Fal[which(producto5Fal[,1] == "201505"),11])
v201506<-sum(producto5Fal[which(producto5Fal[,1] == "201506"),11])
v201507<-sum(producto5Fal[which(producto5Fal[,1] == "201507"),11])
v201508<-sum(producto5Fal[which(producto5Fal[,1] == "201508"),11])
v201509<-sum(producto5Fal[which(producto5Fal[,1] == "201509"),11])
v201510<-sum(producto5Fal[which(producto5Fal[,1] == "201510"),11])
v201511<-sum(producto5Fal[which(producto5Fal[,1] == "201511"),11])
v201512<-sum(producto5Fal[which(producto5Fal[,1] == "201512"),11])

pro52015<-rbind(v201501, v201502, v201503, v201504,v201505, v201506, v201507, v201508, v201509, v201510, v201511, v201512)
pro52015<-as.data.frame(pro52015)
pro52015$Anio <- 2015
pro52015$Mes<-mesesN
View(pro52015)

# 2016
v201601<-sum(producto5Fal[which(producto5Fal[,1] == "201601"),11])
v201602<-sum(producto5Fal[which(producto5Fal[,1] == "201602"),11])
v201603<-sum(producto5Fal[which(producto5Fal[,1] == "201603"),11])
v201604<-sum(producto5Fal[which(producto5Fal[,1] == "201604"),11])
v201605<-sum(producto5Fal[which(producto5Fal[,1] == "201605"),11])
v201606<-sum(producto5Fal[which(producto5Fal[,1] == "201606"),11])
v201607<-sum(producto5Fal[which(producto5Fal[,1] == "201607"),11])
v201608<-sum(producto5Fal[which(producto5Fal[,1] == "201608"),11])
v201609<-sum(producto5Fal[which(producto5Fal[,1] == "201609"),11])
v201610<-sum(producto5Fal[which(producto5Fal[,1] == "201610"),11])
v201611<-sum(producto5Fal[which(producto5Fal[,1] == "201611"),11])
v201612<-sum(producto5Fal[which(producto5Fal[,1] == "201612"),11])

pro52016<-rbind(v201601, v201602, v201603, v201604,v201605, v201606, v201607, v201608, v201609, v201610, v201611, v201612)
pro52016<-as.data.frame(pro52016)
pro52016$Anio <- 2016
pro52016$Mes<-mesesN
View(pro52016)

# 2017
v201701<-sum(producto5Fal[which(producto5Fal[,1] == "201701"),11])
v201702<-sum(producto5Fal[which(producto5Fal[,1] == "201702"),11])
v201703<-sum(producto5Fal[which(producto5Fal[,1] == "201703"),11])
v201704<-sum(producto5Fal[which(producto5Fal[,1] == "201704"),11])
v201705<-sum(producto5Fal[which(producto5Fal[,1] == "201705"),11])
v201706<-sum(producto5Fal[which(producto5Fal[,1] == "201706"),11])
v201707<-sum(producto5Fal[which(producto5Fal[,1] == "201707"),11])
v201708<-sum(producto5Fal[which(producto5Fal[,1] == "201708"),11])
v201709<-sum(producto5Fal[which(producto5Fal[,1] == "201709"),11])
v201710<-sum(producto5Fal[which(producto5Fal[,1] == "201710"),11])
v201711<-sum(producto5Fal[which(producto5Fal[,1] == "201711"),11])
v201712<-sum(producto5Fal[which(producto5Fal[,1] == "201712"),11])

pro52017<-rbind(v201701, v201702, v201703, v201704,v201705, v201706, v201707, v201708, v201709, v201710, v201711, v201712)
pro52017<-as.data.frame(pro52017)
pro52017$Anio <- 2017
pro52017$Mes<-mesesN
View(pro42017)

# 2018
v201801<-sum(producto5Fal[which(producto5Fal[,1] == "201801"),11])
v201802<-sum(producto5Fal[which(producto5Fal[,1] == "201802"),11])
v201803<-sum(producto5Fal[which(producto5Fal[,1] == "201803"),11])
v201804<-sum(producto5Fal[which(producto5Fal[,1] == "201804"),11])
v201805<-sum(producto5Fal[which(producto5Fal[,1] == "201805"),11])
v201806<-sum(producto5Fal[which(producto5Fal[,1] == "201806"),11])
v201807<-sum(producto5Fal[which(producto5Fal[,1] == "201807"),11])
v201808<-sum(producto5Fal[which(producto5Fal[,1] == "201808"),11])
v201809<-sum(producto5Fal[which(producto5Fal[,1] == "201809"),11])
v201810<-sum(producto5Fal[which(producto5Fal[,1] == "201810"),11])
v201811<-sum(producto5Fal[which(producto5Fal[,1] == "201811"),11])
v201812<-sum(producto5Fal[which(producto5Fal[,1] == "201812"),11])

pro52018<-rbind(v201801, v201802, v201803, v201804,v201805, v201806, v201807, v201808, v201809, v201810, v201811, v201812)
pro52018<-as.data.frame(pro52018)
pro52018$Anio <- 2018
pro52018$Mes<-mesesN
View(pro52018)

# 2019
v201901<-sum(producto4Fal[which(producto4Fal[,1] == "201901"),11])
v201902<-sum(producto4Fal[which(producto4Fal[,1] == "201902"),11])
v201903<-sum(producto4Fal[which(producto4Fal[,1] == "201901"),11])
v201904<-sum(producto4Fal[which(producto4Fal[,1] == "201902"),11])
v201905<-sum(producto4Fal[which(producto4Fal[,1] == "201901"),11])
v201906<-sum(producto4Fal[which(producto4Fal[,1] == "201902"),11])
v201907<-sum(producto4Fal[which(producto4Fal[,1] == "201901"),11])
v201908<-sum(producto4Fal[which(producto4Fal[,1] == "201902"),11])

pro42019<-rbind(v201901, v201902, v201903, v201904,v201905, v201906, v201907, v201908)
pro42019<-as.data.frame(pro42019)
pro42019$Anio <- 2019
pro42019$Mes<-mesesO
View(pro42019)

vector<-rbind(pro52015, pro52016, pro52017, pro52018)
View(vector)

# TIMESERIES

# save a numeric vector containing 72 monthly observations
# from Jan 2015 to Dec 2018 as a time series object
myts <- ts(vector, start=c(2015, 1), end=c(2018, 12), frequency=12)
colnames(myts)[1] <- "Unidades Vendidas"

start(myts)
end(myts)
frequency(myts)

plot(myts, type="l", main = "Serie de Tiempo de  PRO 5")
ts.plot(myts)

decomP2<-decompose(myts)
plot(decomP2)

#Grafico sobre el comportamiento de la media en subsets de los anios
plot(aggregate(myts,FUN=mean))
View(myts)

#descomponemos la serie de tiempo en tendencia, estacionaridad e innovacion
plot(decomP2$trend)
plot(decomP1$seasonal)

#forecasting
fp1<-auto.arima(myts[,1])
fp1

# Forecast for the next 10 time units
arima_forecast = forecast(fp1, h = 8, level = c(80,95))
p1<-predict(fp1, n.ahead = 1*12)

# Plot forecasts
plot(arima_forecast, main = "Serie de Tiempo para el PRO 5", xlab = "Tiempo", ylab = "Unidades Vendidas")
myprono<-arima_forecast$mean
suprono<-
  
  
  
  #Aplicaremos una transformacion logaritmica
  myts[10] <- 0
croston<-croston(myts[,1], h = 8)
plot(croston, main ="Prediccion para el Producto 2", xlab = "Tiempo", ylab = "Unidades Vendidas")
croston$fitted
croston

p1<-sum(producto5Fal[which(producto5Fal[,1] == "201901"),11])
p2<-sum(producto5Fal[which(producto5Fal[,1] == "201902"),11])
p3<-sum(producto5Fal[which(producto5Fal[,1] == "201903"),11])
p4<-sum(producto5Fal[which(producto5Fal[,1] == "201904"),11])
p5<-sum(producto5Fal[which(producto5Fal[,1] == "201905"),11])
p6<-sum(producto5Fal[which(producto5Fal[,1] == "201906"),11])
p7<-sum(producto5Fal[which(producto5Fal[,1] == "201907"),11])
p8<-sum(producto5Fal[which(producto5Fal[,1] == "201908"),11])
pronosticoMV<-rbind(p1,p2,p3,p4,p5,p6,p7,p8)
pronosticoMV
sum(pronosticoMV)


View(ventasP52019)

ventasP52019<-as.data.frame(ventasP52019)
ventasP52019$Viejo<-pronosticoMV
ventasP52019$Nuevo<-myprono

sss<-abs(ventasP52019$V1 - ventasP52019$Viejo)
sss<-abs(ventasP52019$V1 - ventasP52019$Nuevo)

sum(ventasP52019$V1)


