library(forecast)
library(tseries)
library(fUnitRoots)
library(ggfortify)
library(xts)
library("openxlsx")

PagGuate <- read.xlsx("PAG-GT.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
PagGuate$Pais<-"Guatemala"

PagHon <- read.xlsx("PAG-HO.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
PagHon$Pais<-"Honduras"

PagEL <- read.xlsx("PAG-EL.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
PagEL$Pais<-"El Salvador"

PagNi <- read.xlsx("PAG-NI.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
PagNi$Pais<-"Nicaragua"

PagCentroamerica <- rbind(PagGuate, PagHon, PagEL, PagNi)
PagCentroamerica$Año.Mes<-as.factor(PagCentroamerica$Año.Mes)

