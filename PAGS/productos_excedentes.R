#Universidad del Valle de Guatemala
#Data science
#Proyecto número 2 - script para encontrar y predecir productos faltantes en stock 

#Paquetes a utilizar
install.packages("openxlsx")
install.packages("forecast")
install.packages("fUnitRoots")
install.packages("ggfortify")
install.packages("xts")

#Librerias
library(forecast)
library(tseries)
library(fUnitRoots)
library(ggfortify)
library(xts)
library(dplyr)
library("openxlsx")

# -> Cargamos los datos de cada país individual, los juntamos para formar centroamérica
PagGuate <- read.xlsx("PAGS/PAG-GT.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
PagGuate$Pais<-"Guatemala"

PagHon <- read.xlsx("PAGS/PAG-HO.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
PagHon$Pais<-"Honduras"

PagEL <- read.xlsx("PAGS/PAG-EL.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
PagEL$Pais<-"El Salvador"

PagNi <- read.xlsx("PAGS/PAG-NI.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
PagNi$Pais<-"Nicaragua"

#PagCentroamerica es la unión de todos los individuales
PagCentroamerica <- rbind(PagGuate, PagHon, PagEL, PagNi)

#Renombramos la columna de Año.Mes
names(PagCentroamerica)[1] = "anomes"

#Volvemos factor el año y mes del data frame
PagCentroamerica$anomes<-as.factor(PagCentroamerica$anomes)

#Seleccionamos un dataframe solo con el producto, las unidades vendidas, y el pronostico por unidad
p_uv_pro <- PagCentroamerica[,c(2,10,11)]

#Agrupamos por producto, y sumamos sus u/v y pronosticos a traves de los años
p_uv_pro <- aggregate(cbind(p_uv_pro$Pronostico, p_uv_pro$Unidades.Vendidas),by=list(Producto=p_uv_pro$Producto), FUN=sum)

names(p_uv_pro) = c("producto","pronostico", "un_vendidas")

p_uv_pro$diferencia <- p_uv_pro$pronostico - p_uv_pro$un_vendidas

#Faltantes
p_uv_pro_asc <- p_uv_pro[order(p_uv_pro$diferencia),]
#Excedente
p_uv_pro_desc <- p_uv_pro[order(-p_uv_pro$diferencia),]

