
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

mesesN<-c(1,2,3,4,5,6,7,8,9,10,11,12)

PagGuate <- read.xlsx("PAG-GT.xlsx", sheet = 1, startRow = 1, colNames = TRUE)

PagGuate$Pais<-"Guatemala"
View(head(PagGuate))

PagHon <- read.xlsx("PAGS/PAG-HO.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
PagHon$Pais<-"Honduras"
View(head(PagHon))

PagEL <- read.xlsx("PAGS/PAG-EL.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
PagEL$Pais<-"El Salvador"
View(head(PagEL))

PagNi <- read.xlsx("PAGS/PAG-NI.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
PagNi$Pais<-"Nicaragua"
View(head(PagNi))

PagCentroamerica <- rbind(PagGuate, PagHon, PagEL, PagNi)
View(PagCentroamerica)
PagCentroamerica$Año.Mes<-as.factor(PagCentroamerica$Año.Mes)

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


#Aplicaremos una transformacion logaritmica
logUniVendidas <- log(myts)
plot(logUniVendidas, type="l", main="Serie de Tiempo de Unidades Vendias con Transf. Log", ylab="Unidades Vendidas")

#Grafico de autocorrelacion
acf(logUniVendidas, type="correlation", lag.max = 100, main = "Grafico de correlacion serie de tiempo Unidades Vendidas" )

#Utilizacion de prueba Dickey-Fuller para raices unitarias
adfTest(logUniVendidas)
adfTest(diff(logUniVendidas))


#######################3
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
fit <- arima(logdiesel, c(6, 1, 2),seasonal = list(order = c(0, 1, 1), period = 12))
fit.2 <- arima(logdiesel, c(6, 1, 2),seasonal = list(order = c(0, 1, 0), period = 12))
#Utilizando valores p, d y q obtenidos en autoarima
fit.3 <- arima(logdiesel, c(5, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12))

#predicciones con cada uno de los modelos propuestos
pred <- predict(fit, n.ahead = 10*12)
pred.1 <- predict(fit.2, n.ahead = 10*12)
pred.2 <- predict(fit.3, n.ahead = 10*12)

ts.plot(serietiempo.diesel,2.718^pred$pred, log = "y", lty = c(1,3))
ts.plot(serietiempo.diesel,2.718^pred.1$pred, log = "y", lty = c(1,3))
ts.plot(serietiempo.diesel,2.718^pred.2$pred, log = "y", lty = c(1,3))

##### PREDICCIONES #####

#PARA LAS PREDICCIONES SOLO VAMOS A USAR ANIOS DE 2001-2016
aniosPredicciones<-datos[datos$Anio <= 2016,]

#Hacemos una tabla unicamente con los valores de aÃ±o, mes y diesel importado
importacion.diesel <- datos[,c(1,2,9)]

#Pegamos en una misma columna el aÃ±o, el mes y agregamos un 1. Esto para obtener un formato de fecha estÃ¡ndar Y-M-D
importacion.diesel$Anio <- paste(importacion.diesel$Anio, importacion.diesel$Mes, 1, sep="-")
importacion.diesel$Mes <- NULL

#Cambiamos la clase de la variable aÃ±o a Date
importacion.diesel <- mutate(importacion.diesel, Anio = as.Date(Anio, format= "%Y-%m-%d"))

#Creamos una serie de tiempo con las fechas y volumenes de importacion de diesel
serietiempo.diesel <- xts(importacion.diesel$Diesel, order.by = importacion.diesel$Anio, frequency = 12)
serietiempo.diesel <- ts(serietiempo.diesel, start=c(2001,1), end=c(2016,12), frequency = 12)

#######################
4123660489
4123310128
4123320382

4123811184
4123660211
4123811146

PagCentroamerica[PagCentroamerica$Producto == 4123310128, ]

PagCentroamerica[page]

View(pro1)
