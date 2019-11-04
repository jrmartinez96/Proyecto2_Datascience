
install.packages("openxlsx")
library("openxlsx")

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
plot(decomposeUniVendidas)
plot(decomposeUniVendidas$seasonal)

