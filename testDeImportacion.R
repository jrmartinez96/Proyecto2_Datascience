# -------------------------------- Proyecto numero 2 Data science -------------------------------
# ---------------------------------|          SCENTIA             |------------------------------
# Análisis de Ventas Directas en Centroamérica
# Catedrática: Lynette García 
# Maria Fernanda Rodas	17125
# Pablo Viana		      	16091
# Sergio Marchena		    16387
# Daniel Ixcoy		      16748
# José Martínez			    15163
# José Meneses		    	1514
#####


#Libreria
#Programa para juntar todos los xls en una tabla y exportar un RDATA
library("readxl")
library("dplyr")

# Unión de todos los datos individuales para lograr un conjunto centroamericano
el_salvador <- read_excel("el_salvador.xlsx", range = "A2:AE6370")
nicaragua <- read_excel("nicaragua.xlsx", range = "A2:AE6374")
honduras <- read_excel("honduras.xlsx", range = "A2:AE6685")
guatemala <- read_excel("guatemala.xlsx",range = "A2:AE7719")
View(el_salvador)
View(nicaragua)
View(honduras)
View(guatemala)

names(el_salvador)[names(el_salvador) == "conca"] <- "CONCA"
guatemala$`Año Mes` <- gsub("210811","201811",guatemala$`Año Mes`)

ventas.centroamerica<-rbind(guatemala,honduras,nicaragua,el_salvador)
colnames(ventas.centroamerica)
View(ventas.centroamerica)

#Guardamos la unión como un archivo RData
save(ventas.centroamerica, file = "ventas_centroamerica.RData")

#-----------------------------------------------------------------
# TABLA DE FRECUENCIA POR MES POR ANO

h1<-table(ventas.centroamerica$`Año Mes`)
h1[1:12]
meses<-c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago","Sept", "Oct", "Nov","Dic")
plot(h1[1:12])
?plot

