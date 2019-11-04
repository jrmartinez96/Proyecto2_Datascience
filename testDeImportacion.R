# --------------------------------- Proyecto numero 2 Data science -------------------------------
# ---------------------------------|          SCENTIA             |------------------------------
# Análisis de Ventas Directas en Centroamérica

# Catedrática: Lynette Garc�???a 
# Catedrática: Lynette Garc??a 
# Maria Fernanda Rodas	17125
# Pablo Viana		      	16091
# Sergio Marchena		    16387
# Daniel Ixcoy		      16748
# José Mart�???nez			  15163
# José Mart??nez			    15163
# José Meneses		    	1514

#Instalar librerias
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("frequency")
# install.packages("rela")	
# install.packages("psych")
# install.packages("FactoMineR")	
# install.packages("corrplot")	
# install.packages("NbClust")	
# install.packages("fpc")	
# install.packages("cluster")	
# install.packages("fpc")
# install.packages("factoextra")
# install.packages("e1071")#para cmeans
# # REGLAS DE ASOCIACI?N
# install.packages("arules")
# install.packages("stringr")
# install.packages("forecast")
# install.packages("rpart")
# install.packages("rpart.plot")
# install.packages("caret")


#Librerias
#Programa para juntar todos los xls en una tabla y exportar un RDATA
library(readxl)
library(dplyr)
library(frequency)
library(rela)	
library(psych)	
library(FactoMineR)	
library(corrplot)	
library(NbClust)	
library(fpc)	
library(cluster)	
library(fpc)
library(factoextra)
library(e1071)#para cmeans
# REGLAS DE ASOCIACI?N
library(arules)
library(stringr)
library(forecast)
library(rpart)
library(rpart.plot)
library(caret)

# Unión de todos los datos individuales para lograr un conjunto centroamericano
el_salvador <- read_excel("el_salvador.xlsx", range = "A2:AE6370")
nicaragua <- read_excel("nicaragua.xlsx", range = "A2:AE6374")
honduras <- read_excel("honduras.xlsx", range = "A2:AE6685")
guatemala <- read_excel("guatemala.xlsx",range = "A2:AE7719")

# sum(el_salvador$`Unidades Vendidas`)
# sum(nicaragua$`Unidades Vendidas`)
# sum(honduras$`Unidades Vendidas`)
# sum(guatemala$`Unidades Vendidas`)
# 
# table(ventas.centroamerica$Margen < 0)


ventasPpais<-c(3206412, 3206412, 3586348, 5210613)
nombres<-c("El Salvador", "Nicaragua", "Honduras", "Guatemala")

barplot(ventasPpais, main = "Ventas por Pais", names.arg = nombres)

#en el salvador el nombre de la categoria conca se encuentra en minusculas, se transforma a mayúsculas
#para poder llevar a cabo sin problemas la función rbind
names(el_salvador)[names(el_salvador) == "conca"] <- "CONCA"


###### AGREGAR NUEVAS COLUMNAS Y CAMBIAR DE TEXTO A FACTORES PARA TODOS LOS PAISES #######

View(guatemala)
guatemala$PaisTexto<-"Guatemala"
guatemala$PaisNumero<-1

el_salvador$PaisTexto<-"El Salvador"
el_salvador$PaisNumero<-2

honduras$PaisTexto<-"Honduras"
honduras$PaisNumero<-3

nicaragua$PaisTexto<-"Nicaragua"
nicaragua$PaisNumero<-4

# CAMBIO DE MONEDA POR PAIS
# EL SALVADOR
  # 2018
el_salvador$`Precio Catalogo`[str_detect(el_salvador$CONCA, "2018")] = el_salvador$`Precio Catalogo` * 7.4
el_salvador$`Precio Vta s/iva`[str_detect(el_salvador$CONCA, "2018")] = el_salvador$`Precio Vta s/iva` * 7.4
el_salvador$`Venta Neta s/iva`[str_detect(el_salvador$CONCA, "2018")] = el_salvador$`Venta Neta s/iva` * 7.4

  # 2019
el_salvador$`Precio Catalogo`[str_detect(el_salvador$CONCA, "2019")] = el_salvador$`Precio Catalogo` * 7.8
el_salvador$`Precio Vta s/iva`[str_detect(el_salvador$CONCA, "2019")] = el_salvador$`Precio Vta s/iva` * 7.8
el_salvador$`Venta Neta s/iva`[str_detect(el_salvador$CONCA, "2019")] = el_salvador$`Venta Neta s/iva` * 7.8


# HONDURAS
  # 2018
honduras$`Precio Catalogo`[str_detect(honduras$CONCA, "2018")] = honduras$`Precio Catalogo` * 3.3676
honduras$`Precio Vta s/iva`[str_detect(honduras$CONCA, "2018")] = honduras$`Precio Vta s/iva` * 3.3676
honduras$`Venta Neta s/iva`[str_detect(honduras$CONCA, "2018")] = honduras$`Venta Neta s/iva` * 3.3676

  # 2019
honduras$`Precio Catalogo`[str_detect(honduras$CONCA, "2019")] = honduras$`Precio Catalogo` * 3.2962
honduras$`Precio Vta s/iva`[str_detect(honduras$CONCA, "2019")] = honduras$`Precio Vta s/iva` * 3.2962
honduras$`Venta Neta s/iva`[str_detect(honduras$CONCA, "2019")] = honduras$`Venta Neta s/iva` * 3.2962


# NICARAGUA
  # 2018
nicaragua$`Precio Catalogo`[str_detect(nicaragua$CONCA, "2018")] = nicaragua$`Precio Catalogo` * 4.5830
nicaragua$`Precio Vta s/iva`[str_detect(nicaragua$CONCA, "2018")] = nicaragua$`Precio Vta s/iva` * 4.5830
nicaragua$`Venta Neta s/iva`[str_detect(nicaragua$CONCA, "2018")] = nicaragua$`Venta Neta s/iva` * 4.5830

  # 2019
nicaragua$`Precio Catalogo`[str_detect(nicaragua$CONCA, "2019")] = nicaragua$`Precio Catalogo` * 4.3513
nicaragua$`Precio Vta s/iva`[str_detect(nicaragua$CONCA, "2019")] = nicaragua$`Precio Vta s/iva` * 4.3513
nicaragua$`Venta Neta s/iva`[str_detect(nicaragua$CONCA, "2019")] = nicaragua$`Venta Neta s/iva` * 4.3513



# cambio de text a numeros GUATEMALA

as.factor(guatemala$Categoria)
guatemala$Categoria_Num<-""

l1<-guatemala[guatemala$Categoria == "CABELLO",] 
l1$Categoria_Num <- 1
l2<-guatemala[guatemala$Categoria == "CUIDADO DE LA PIEL",] 
l2$Categoria_Num <- 2
l3<-guatemala[guatemala$Categoria == "CUIDADO DE LAS U?AS",] 
l3$Categoria_Num <- 3
l4<-guatemala[guatemala$Categoria == "CUIDADO PERSONAL",] 
l4$Categoria_Num <- 4
l5<-guatemala[guatemala$Categoria == "DIVISION FARMA",] 
l5$Categoria_Num <- 5
l6<-guatemala[guatemala$Categoria == "ELLAS",] 
l6$Categoria_Num <- 6
l7<-guatemala[guatemala$Categoria == "ELLOS",] 
l7$Categoria_Num <- 7
l8<-guatemala[guatemala$Categoria == "GASTOS PROMOCIONALES",] 
l8$Categoria_Num <- 8
l9<-guatemala[guatemala$Categoria == "JOYERIA",] 
l9$Categoria_Num <- 9
l10<-guatemala[guatemala$Categoria == "LIMPIEZA",] 
l10$Categoria_Num <- 10
l11<-guatemala[guatemala$Categoria == "MAQUILLAJE",] 
l11$Categoria_Num <- 11
l12<-guatemala[guatemala$Categoria == "NI?OS",] 
l12$Categoria_Num <- 12
l13<-guatemala[guatemala$Categoria == "PAQUETES",] 
l13$Categoria_Num <- 13
l14<-guatemala[guatemala$Categoria == "PREMIOS",] 
l14$Categoria_Num <- 14
l15<-guatemala[guatemala$Categoria == "PROMOCIONALES VENDIDOS",] 
l15$Categoria_Num <- 15
l16<-guatemala[guatemala$Categoria == "SALUD Y BIENESTAR",] 
l16$Categoria_Num <- 16
l17<-guatemala[guatemala$Categoria == "TEENS",] 
l17$Categoria_Num <- 17
l18<-guatemala[guatemala$Categoria == "USO DIARIO",] 
l18$Categoria_Num <- 18
l19<-guatemala[is.na(guatemala$Categoria),] 
l19$Categoria_Num <- 19

guatemala<-rbind(l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l4,l15,l16,l17,l18,l19)
View(guatemala)

as.factor(guatemala$`Canal de Venta`)
guatemala$CanalVentaNum<-""

l1<-guatemala[guatemala$`Canal de Venta` == "Catalogo",]
l1$CanalVentaNum<-1
l2<-guatemala[guatemala$`Canal de Venta` == "Contingencia",]
l2$CanalVentaNum<-2
l3<-guatemala[guatemala$`Canal de Venta` == "Scentia Al Dia",]
l3$CanalVentaNum<-3

guatemala<-rbind(l1,l2,l3)
View(guatemala)

guatemala$PaginacionNum<-""
l1<-guatemala[guatemala$Pagina...22 == "Full spread",]
l1$PaginacionNum<- 1
l2<-guatemala[guatemala$Pagina...22 == "pagina derecha",]
l2$PaginacionNum<- 2
l3<-guatemala[guatemala$Pagina...22 == "pagina izquierda",]
l3$PaginacionNum<- 3

guatemala<-rbind(l1,l2,l3)
View(guatemala)

# cambio de text a numeros EL SALVADOR

as.factor(el_salvador$Categoria)
el_salvador$Categoria_Num<-""
View(el_salvador)

l1<-el_salvador[el_salvador$Categoria == "CABELLO",] 
l1$Categoria_Num <- 1
l2<-el_salvador[el_salvador$Categoria == "CUIDADO DE LA PIEL",] 
l2$Categoria_Num <- 2
l3<-el_salvador[el_salvador$Categoria == "CUIDADO DE LAS U?AS",] 
l3$Categoria_Num <- 3
l4<-el_salvador[el_salvador$Categoria == "CUIDADO PERSONAL",] 
l4$Categoria_Num <- 4
l5<-el_salvador[el_salvador$Categoria == "DIVISION FARMA",] 
l5$Categoria_Num <- 5
l6<-el_salvador[el_salvador$Categoria == "ELLAS",] 
l6$Categoria_Num <- 6
l7<-el_salvador[el_salvador$Categoria == "ELLOS",] 
l7$Categoria_Num <- 7
l8<-el_salvador[el_salvador$Categoria == "GASTOS PROMOCIONALES",] 
l8$Categoria_Num <- 8
l9<-el_salvador[el_salvador$Categoria == "JOYERIA",] 
l9$Categoria_Num <- 9
l10<-el_salvador[el_salvador$Categoria == "LIMPIEZA",] 
l10$Categoria_Num <- 10
l11<-el_salvador[el_salvador$Categoria == "MAQUILLAJE",] 
l11$Categoria_Num <- 11
l12<-el_salvador[el_salvador$Categoria == "NI?OS",] 
l12$Categoria_Num <- 12
l13<-el_salvador[el_salvador$Categoria == "PAQUETES",] 
l13$Categoria_Num <- 13
l14<-el_salvador[el_salvador$Categoria == "PREMIOS",] 
l14$Categoria_Num <- 14
l15<-el_salvador[el_salvador$Categoria == "PROMOCIONALES VENDIDOS",] 
l15$Categoria_Num <- 15
l16<-el_salvador[el_salvador$Categoria == "SALUD Y BIENESTAR",] 
l16$Categoria_Num <- 16
l17<-el_salvador[el_salvador$Categoria == "TEENS",] 
l17$Categoria_Num <- 17
l18<-el_salvador[el_salvador$Categoria == "USO DIARIO",] 
l18$Categoria_Num <- 18
l19<-el_salvador[is.na(el_salvador$Categoria),] 
l19$Categoria_Num <- 19

el_salvador<-rbind(l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l4,l15,l16,l17,l18,l19)
View(el_salvador)


as.factor(el_salvador$`Canal de Venta`)
el_salvador$CanalVentaNum<-""

l1<-el_salvador[el_salvador$`Canal de Venta` == "Catalogo",]
l1$CanalVentaNum<-1
l2<-el_salvador[el_salvador$`Canal de Venta` == "Contingencia",]
l2$CanalVentaNum<-2
l3<-el_salvador[el_salvador$`Canal de Venta` == "Scentia Al Dia",]
l3$CanalVentaNum<-3

el_salvador<-rbind(l1,l2,l3)
View(el_salvador)

el_salvador$PaginacionNum<-""
l1<-el_salvador[el_salvador$Pagina...22 == "Full spread",]
l1$PaginacionNum<-1
l2<-el_salvador[el_salvador$Pagina...22 == "pagina derecha",]
l2$PaginacionNum<-2
l3<-el_salvador[el_salvador$Pagina...22 == "pagina izquierda",]
l3$PaginacionNum<-3

el_salvador<-rbind(l1,l2,l3)
View(el_salvador)


# cambio de text a numeros HONDURAS

as.factor(honduras$Categoria)
honduras$Categoria_Num<-""

l1<-honduras[honduras$Categoria == "CABELLO",] 
l1$Categoria_Num <- 1
l2<-honduras[honduras$Categoria == "CUIDADO DE LA PIEL",] 
l2$Categoria_Num <- 2
l3<-honduras[honduras$Categoria == "CUIDADO DE LAS U?AS",] 
l3$Categoria_Num <- 3
l4<-honduras[honduras$Categoria == "CUIDADO PERSONAL",] 
l4$Categoria_Num <- 4
l5<-honduras[honduras$Categoria == "DIVISION FARMA",] 
l5$Categoria_Num <- 5
l6<-honduras[honduras$Categoria == "ELLAS",] 
l6$Categoria_Num <- 6
l7<-honduras[honduras$Categoria == "ELLOS",] 
l7$Categoria_Num <- 7
l8<-honduras[honduras$Categoria == "GASTOS PROMOCIONALES",] 
l8$Categoria_Num <- 8
l9<-honduras[honduras$Categoria == "JOYERIA",] 
l9$Categoria_Num <- 9
l10<-honduras[honduras$Categoria == "LIMPIEZA",] 
l10$Categoria_Num <- 10
l11<-honduras[honduras$Categoria == "MAQUILLAJE",] 
l11$Categoria_Num <- 11
l12<-honduras[honduras$Categoria == "NI?OS",] 
l12$Categoria_Num <- 12
l13<-honduras[honduras$Categoria == "PAQUETES",] 
l13$Categoria_Num <- 13
l14<-honduras[honduras$Categoria == "PREMIOS",] 
l14$Categoria_Num <- 14
l15<-honduras[honduras$Categoria == "PROMOCIONALES VENDIDOS",] 
l15$Categoria_Num <- 15
l16<-honduras[honduras$Categoria == "SALUD Y BIENESTAR",] 
l16$Categoria_Num <- 16
l17<-honduras[honduras$Categoria == "TEENS",] 
l17$Categoria_Num <- 17
l18<-honduras[honduras$Categoria == "USO DIARIO",] 
l18$Categoria_Num <- 18
l19<-honduras[is.na(honduras$Categoria),] 
l19$Categoria_Num <- 19

honduras<-rbind(l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l4,l15,l16,l17,l18,l19)
View(honduras)


as.factor(honduras$`Canal de Venta`)
honduras$CanalVentaNum<-""

l1<-honduras[honduras$`Canal de Venta` == "Catalogo",]
l1$CanalVentaNum<-1
l2<-honduras[honduras$`Canal de Venta` == "Contingencia",]
l2$CanalVentaNum<-2
l3<-honduras[honduras$`Canal de Venta` == "Scentia Al Dia",]
l3$CanalVentaNum<-3

honduras<-rbind(l1,l2,l3)
View(honduras)

honduras$PaginacionNum<-""
l1<-honduras[honduras$Pagina...22 == "Full spread",]
l1$PaginacionNum<-1
l2<-honduras[honduras$Pagina...22 == "pagina derecha",]
l2$PaginacionNum<-2
l3<-honduras[honduras$Pagina...22 == "pagina izquierda",]
l3$PaginacionNum<-3

honduras<-rbind(l1,l2,l3)
View(honduras)

# cambio de text a numeros NICARAGUA

as.factor(nicaragua$Categoria)
nicaragua$Categoria_Num<-""

l1<-nicaragua[nicaragua$Categoria == "CABELLO",] 
l1$Categoria_Num <- 1
l2<-nicaragua[nicaragua$Categoria == "CUIDADO DE LA PIEL",] 
l2$Categoria_Num <- 2
l3<-nicaragua[nicaragua$Categoria == "CUIDADO DE LAS U?AS",] 
l3$Categoria_Num <- 3
l4<-nicaragua[nicaragua$Categoria == "CUIDADO PERSONAL",] 
l4$Categoria_Num <- 4
l5<-nicaragua[nicaragua$Categoria == "DIVISION FARMA",] 
l5$Categoria_Num <- 5
l6<-nicaragua[nicaragua$Categoria == "ELLAS",] 
l6$Categoria_Num <- 6
l7<-nicaragua[nicaragua$Categoria == "ELLOS",] 
l7$Categoria_Num <- 7
l8<-nicaragua[nicaragua$Categoria == "GASTOS PROMOCIONALES",] 
l8$Categoria_Num <- 8
l9<-nicaragua[nicaragua$Categoria == "JOYERIA",] 
l9$Categoria_Num <- 9
l10<-nicaragua[nicaragua$Categoria == "LIMPIEZA",] 
l10$Categoria_Num <- 10
l11<-nicaragua[nicaragua$Categoria == "MAQUILLAJE",] 
l11$Categoria_Num <- 11
l12<-nicaragua[nicaragua$Categoria == "NI?OS",] 
l12$Categoria_Num <- 12
l13<-nicaragua[nicaragua$Categoria == "PAQUETES",] 
l13$Categoria_Num <- 13
l14<-nicaragua[nicaragua$Categoria == "PREMIOS",] 
l14$Categoria_Num <- 14
l15<-nicaragua[nicaragua$Categoria == "PROMOCIONALES VENDIDOS",] 
l15$Categoria_Num <- 15
l16<-nicaragua[nicaragua$Categoria == "SALUD Y BIENESTAR",] 
l16$Categoria_Num <- 16
l17<-nicaragua[nicaragua$Categoria == "TEENS",] 
l17$Categoria_Num <- 17
l18<-nicaragua[nicaragua$Categoria == "USO DIARIO",] 
l18$Categoria_Num <- 18
l19<-nicaragua[is.na(nicaragua$Categoria),] 
l19$Categoria_Num <- 19

nicaragua<-rbind(l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l4,l15,l16,l17,l18,l19)
View(nicaragua)

as.factor(nicaragua$`Canal de Venta`)
nicaragua$CanalVentaNum<-""

l1<-nicaragua[nicaragua$`Canal de Venta` == "Catalogo",]
l1$CanalVentaNum<-1
l2<-nicaragua[nicaragua$`Canal de Venta` == "Contingencia",]
l2$CanalVentaNum<-2
l3<-nicaragua[nicaragua$`Canal de Venta` == "Scentia Al Dia",]
l3$CanalVentaNum<-3

nicaragua<-rbind(l1,l2,l3)
View(nicaragua)

nicaragua$PaginacionNum<-""
l1<-nicaragua[nicaragua$Pagina...22 == "Full spread",]
l1$PaginacionNum<-1
l2<-nicaragua[nicaragua$Pagina...22 == "pagina derecha",]
l2$PaginacionNum<-2
l3<-nicaragua[nicaragua$Pagina...22 == "pagina izquierda",]
l3$PaginacionNum<-3

nicaragua<-rbind(l1,l2,l3)
View(nicaragua)

#Union de todas las tablas para crear un conjunto que represente a centroamérica
ventas.centroamerica<-rbind(guatemala,honduras,nicaragua,el_salvador)
View(ventas.centroamerica)

# ------------------------ Proceso de limpieza de los datos ---------------------------

#Cambio de nombres a columnas
names(ventas.centroamerica)[names(ventas.centroamerica) == "Pagina...7"] <- "Pagina_cat"
names(ventas.centroamerica)[names(ventas.centroamerica) == "Pagina...22"] <- "Pagina"
names(ventas.centroamerica)[names(ventas.centroamerica) == "%...24"] <- "Porcentaje"
names(ventas.centroamerica)[names(ventas.centroamerica) == "%...26"] <- "Porcentaje2"

#Arreglo nivel 210811 en categoria A?o mes
ventas.centroamerica$`Año Mes` <- gsub("210811","201811",ventas.centroamerica$`Año Mes`)

#Quitar caracteres \r|\n de  columna descripcion
ventas.centroamerica$Descripcion <- gsub("\r|\n","",ventas.centroamerica$Descripcion)

#transformación en columna contingencia de Farma -> farma
ventas.centroamerica$Contingencia <- gsub("farma","Farma",ventas.centroamerica$Contingencia)

#Pagina | pagina derecha Pagina derecha, Pagina izquierda pagina izquierda
ventas.centroamerica$Pagina <- gsub("Pagina derecha","pagina derecha", ventas.centroamerica$Pagina)
ventas.centroamerica$Pagina <- gsub("Pagina izquierda","pagina izquierda", ventas.centroamerica$Pagina)

#Tipo Precio | intruduccion Introduccion, precio normal Precio Normal
ventas.centroamerica$`Tipo Precio` <- gsub("Introduccion","introduccion",ventas.centroamerica$`Tipo Precio`)
ventas.centroamerica$`Tipo Precio` <- gsub("Precio Normal","precio normal",ventas.centroamerica$`Tipo Precio`)

#Atributo Neto | No aplica No Aplica
ventas.centroamerica$`Atributo Neto`<- gsub("No Aplica", "No aplica", ventas.centroamerica$`Atributo Neto`)

#Energy Chart | cierre Cierre, oferta esencial Oferta esencial
ventas.centroamerica$`Energy Chart` <- gsub("Cierre","cierre", ventas.centroamerica$`Energy Chart`)
ventas.centroamerica$`Energy Chart` <- gsub("Oferta esencial","oferta esencial", ventas.centroamerica$`Energy Chart`)

#Promociones | 1 x 2 x 1 X 2 X , 2 x 2 x 2x 2x, contingencia Contingencia, Producto gratis producto gratis Producto Gratis,
#promocion precio Promocion precio Promocion Precio 
ventas.centroamerica$Promociones <- gsub("1 X 2 X","1 x 2 x",ventas.centroamerica$Promociones)
ventas.centroamerica$Promociones <- gsub("2x 2x","2 x 2 x",ventas.centroamerica$Promociones)
ventas.centroamerica$Promociones <- gsub("Contingencia","contingencia",ventas.centroamerica$Promociones)
ventas.centroamerica$Promociones <- gsub("Producto gratis|Producto Gratis","producto gratis",ventas.centroamerica$Promociones)
ventas.centroamerica$Promociones <- gsub("Promocion Precio|Promocion precio","promocion precio",ventas.centroamerica$Promociones)

#Treboles extra | si SI
ventas.centroamerica$`Treboles extra` <- gsub("SI","si",ventas.centroamerica$`Treboles extra`)

#Cambio de columnas de tipo char a factor
col_names <- c("Año Mes","Producto","Codigo Catalogo","CONCA","Tipo Comision","Pagina_cat","Descripcion","Categoria","Linea","Observaciones","Canal de Venta","Contingencia","Pagina","Tipo Precio","Atributo Neto","Energy Chart","Promociones","Recursos Especiales","Treboles extra")
ventas.centroamerica$Costo <- as.numeric(ventas.centroamerica$Costo)
ventas.centroamerica[col_names] <- lapply(ventas.centroamerica[col_names] , factor)
ventas.centroamerica <- subset(ventas.centroamerica, !is.na(Producto))
View(ventas.centroamerica)

#Codigo utilizado para hacer un resumen del dataset con todos los niveles de los factores para corregir uno por uno
# sink("resumen_datos.png")
# print(summary(ventas.centroamerica, maxsum = max(lengths(lapply(ventas.centroamerica, unique)))))
# sink()

#Guardamos la unión como un archivo RData
#save(ventas.centroamerica, file = "ventas_centroamerica.RData")

#---------------------- Graficas para entender comportamiento  y estado de los datos -----------

# Gráfico de  ventas por AnoO - MES (ventas centroamerica)
h1<-table(ventas.centroamerica$`Año Mes`)
FRECventas2018<-h1[1:12]
meses<-c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago","Sept", "Oct", "Nov","Dic")
meses2<-c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul")
#Gráfico de ventas 2018
barplot(FRECventas2018, main = "VENTAS EN 2018", names.arg = meses)
FRECventas2019<-h1[13:19]
#Gráfico de ventas en 2019
barplot(FRECventas2019, main = "VENTAS EN 2019", names.arg = meses2)
#Gráfico de ventas totales, de 2018 a 2019
barplot(h1, main = "VENTAS TOTALES", names.arg = c(meses, meses2))

#Grafico de los 10 productos mas vendidos
productos<-table(ventas.centroamerica$`Unidades Vendidas`)
productos_ordenados<-productos[order(-productos)]
#Cantidad de cuantos se vendieron de los 10 productos mas vendidos
yy<-as.character(as.vector(head(productos_ordenados, n = 10)))
#Ajustamos margenes para el grafico
par(mar = c(4, 4, 4, 9), xpd = TRUE)
#Obtenemos los 10 productos mas vendidos
productos_ordenados <- head(productos_ordenados, n=10)
#Lo graficamos
diez_mas_vendidos<-barplot(productos_ordenados,legend.text = yy, args.legend = list(x = "topright", bty = "n", inset=c(-0.15, 0)),horiz = FALSE, main = "PRODUCTOS MAS VENDIDOS", col = c("darkseagreen","darkseagreen1","darkseagreen2","darkseagreen3","darkseagreen4","darkslategray1","darkslategray2","darkslategray3","darkslategray4","grey"))


p1<-(ventas.centroamerica[ventas.centroamerica$Producto == "4123800056",])
View(p1)
na.omit(p1$`Unidades Vendidas`)
sum(p1$`Unidades Vendidas`)

#Obtenemos la descripción de los 10 productos más vendidos
p1<-as.character(ventas.centroamerica$Descripcion[ventas.centroamerica$Producto == "4123800056"][1])
p2<-as.character(ventas.centroamerica$Descripcion[ventas.centroamerica$Producto == "4123800080"][1])
p3<-as.character(ventas.centroamerica$Descripcion[ventas.centroamerica$Producto == "4123800001"][1])
p4<-as.character(ventas.centroamerica$Descripcion[ventas.centroamerica$Producto == "4123800030"][2])
p5<-as.character(ventas.centroamerica$Descripcion[ventas.centroamerica$Producto == "4123811146"][1])
p6<-as.character(ventas.centroamerica$Descripcion[ventas.centroamerica$Producto == "4123660214"][1])
p7<-as.character(ventas.centroamerica$Descripcion[ventas.centroamerica$Producto == "4130000001"][1])
p8<-as.character(ventas.centroamerica$Descripcion[ventas.centroamerica$Producto == "4130000002"][1])
p9<-as.character(ventas.centroamerica$Descripcion[ventas.centroamerica$Producto == "4130000003"][1])
p10<-as.character(ventas.centroamerica$Descripcion[ventas.centroamerica$Producto == "4130000004"][1])
#Las unimos todas en un vector de descripciones
ps<-c(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10)
#Coordinadas del eje x donde poner las descripciones
xx <- c(0.7,1.9,3.1,4,5.5,6.7,7.9,9.1,10.4,11.5)
#Las agregamos al gráfico
text(diez_mas_vendidos, productos_ordenados/2, labels = ps, srt = 90, cex = 0.5)

# CODIGO CATEGORIA
categorias<-table(ventas.centroamerica$Categoria)
cat_ordenadas<-categorias[order(-categorias)]
barplot(cat_ordenadas)

#Grafica de canales de venta
h1<-ventas.centroamerica$`Canal de Venta`
plot(h1, main = "CANAL DE VENTA EN C.A.")

#Grafica de lineas mas pedidas
lineas<-table(ventas.centroamerica$Linea)
h1<-lineas[order(-lineas)]
head(h1,n=10)
ps<-c("FAMILY", "CLEAN HOUSE", "COLLECTION EXCLUSUVAS FEM", "COLLECTION EXCLUSUVAS MEN",
      "SCENTIA NATURALS", "NATURAL PERFECTION", "D'NINOS", "PROMOCIONAL", "ORO LIQUIDO", "TOP SECRET")
barplot(head(h1, n=10), names.arg = NA, main = "LINEAS MAS PEDIDAS")
axis(1, at=xx, labels=ps, tick=FALSE, las=2, line=-3, cex.axis=0.5)


#Tablas de frecuencia de todas las variables
freq(ventas.centroamerica$`Año Mes`)
freq(ventas.centroamerica$Producto)
freq(ventas.centroamerica$CONCA)
freq(ventas.centroamerica$`Codigo Catalogo`)
freq(ventas.centroamerica$Descripcion)
freq(ventas.centroamerica$Categoria)
freq(ventas.centroamerica$Pagina_cat)
freq(ventas.centroamerica$Linea)
freq(ventas.centroamerica$Observaciones)
freq(ventas.centroamerica$`Canal de Venta`)
freq(ventas.centroamerica$Contingencia)
freq(ventas.centroamerica$Pagina)
freq(ventas.centroamerica$`Tipo Precio`)
freq(ventas.centroamerica$`Tipo Comision`)
freq(ventas.centroamerica$`Atributo Neto`)
freq(ventas.centroamerica$`Energy Chart`)
freq(ventas.centroamerica$Promociones)
freq(ventas.centroamerica$`Recursos Especiales`)
freq(ventas.centroamerica$`Treboles extra`)


# separación de variables numéricas de las categóricas
num <- unlist(lapply(ventas.centroamerica, is.numeric))
cat <- unlist(lapply(ventas.centroamerica, is.factor))
# variables numéricas de ventas.centroamerica
ventas.centroamerica$Producto<-as.numeric(as.character(ventas.centroamerica$Producto))
ventas.centroamerica$Pagina_cat<-as.numeric(as.character(ventas.centroamerica$Pagina_cat))
ventas.centroamerica$`Codigo Catalogo`<-as.numeric(as.character(ventas.centroamerica$`Codigo Catalogo`))
vcnum <- ventas.centroamerica[,num]

View(vcnum)
# variables categóricas de ventas.centroamericas
vccat <- ventas.centroamerica[,cat]
# Realizando la matriz de correlación y su gráfica
matcorrelacion <- cor(vcnum,use = "pairwise.complete.obs")
round(matcorrelacion, digits = 6)
corrplot(matcorrelacion, method="shade", addCoef.col = "black", shade.col=NA, tl.col="black", tl.cex=0.6, tl.srt=45)

# ------------------------- Clustering ---------------------------------

# Método de ward para determinar número adecuado de clusters con K-means
wss <- (nrow(vcnum)-1)*sum(apply(vcnum,2,var))

for (i in 2:10) 
  wss[i] <- sum(kmeans(vcnum, centers=i)$withinss)

plot(1:10, wss, type="b", xlab="Number of Clusters",  ylab="Within groups sum of squares")

# Método K-means para crear los clusters 
km<-kmeans(vcnum,4)
vcnum$grupo_kmedias<-km$cluster
# gráficos que muestran la ubicación de cada cluster
plotcluster(vcnum,km$cluster)
fviz_cluster(km, data = vcnum,geom = "point", ellipse.type = "norm")
# Calculo de la silueta para K-means
silkm<-silhouette(km$cluster,dist(vcnum))
mean(silkm[,3])
vcnum$grupo<-NULL

#--- Con fuzzy c-means

#Fuzzy C-Means
fcm<-cmeans(vcnum,3)
vcnum$grupo_fuzzy<-fcm$cluster
silkm<-silhouette(fcm$cluster,dist(vcnum))
mean(silkm[,3])
plotcluster(vcnum,fcm$cluster)

# ---------------------------- Reglas de asociación  ------------------------

# El m?nimo nivel de soporte y confianza aceptados
#Debido a la gran cantidad de datos, se escogen únicamente reglas con 0.9 de soportey confianza, as�??? como se limitan parametros para el tiempo de búsqueda (maxtime y maxlen)
#Debido a la gran cantidad de datos, se escogen únicamente reglas con 0.9 de soportey confianza, as?? como se limitan parametros para el tiempo de búsqueda (maxtime y maxlen)
reglas<-apriori(vccat, parameter = list(support = 0.30,
                                       confidence = 0.60,
                                       minlen = 2,
                                       target = "rules"))

inspect(reglas)


# ---------------------------- Análisis PCA ---------------------


str(ventas.centroamerica)
str(vcnum)
PCA<-prcomp(vcnum, scale. = TRUE)

summary(PCA)
View(ventas.centroamerica)

datosPCA<-ventas.centroamerica[c(1,2,3,4,7, 9:18,24,26 )]
View(datosPCA)

ventas2018<-datosPCA[grep("2018", datosPCA$`Año Mes`),]
ventas2019<-datosPCA[grep("2019", datosPCA$`Año Mes`),]

View(ventas2018)
View(ventas2019)

ventas2018$AnioNuevo<- 2018
ventas2019$AnioNuevo<- 2019

#####

ene<-ventas2018[grep("201801", ventas2018$`Año Mes`),]
ene$MesNuevo <- 1
feb<-ventas2018[grep("201802", ventas2018$`Año Mes`),]
feb$MesNuevo <- 2
mar<-ventas2018[grep("201803", ventas2018$`Año Mes`),]
mar$MesNuevo <- 3
abr<-ventas2018[grep("201804", ventas2018$`Año Mes`),]
abr$MesNuevo <- 4
may<-ventas2018[grep("201805", ventas2018$`Año Mes`),]
may$MesNuevo <- 5
jun<-ventas2018[grep("201806", ventas2018$`Año Mes`),]
jun$MesNuevo <- 6
jul<-ventas2018[grep("201807", ventas2018$`Año Mes`),]
jul$MesNuevo <- 7
ago<-ventas2018[grep("201808", ventas2018$`Año Mes`),]
ago$MesNuevo <- 8
sep<-ventas2018[grep("201809", ventas2018$`Año Mes`),]
sep$MesNuevo <- 9
oct<-ventas2018[grep("201810", ventas2018$`Año Mes`),]
oct$MesNuevo <- 10
nov<-ventas2018[grep("201811", ventas2018$`Año Mes`),]
nov$MesNuevo <- 11
dic<-ventas2018[grep("201812", ventas2018$`Año Mes`),]
dic$MesNuevo <- 12

ventas2018<-rbind(ene,feb,mar,abr,may,jun,jul,ago,sep,oct,nov,dic)

ene<-ventas2019[grep("201901", ventas2019$`Año Mes`),]
ene$MesNuevo <- 1
feb<-ventas2019[grep("201902", ventas2019$`Año Mes`),]
feb$MesNuevo <- 2
mar<-ventas2019[grep("201903", ventas2019$`Año Mes`),]
mar$MesNuevo <- 3
abr<-ventas2019[grep("201904", ventas2019$`Año Mes`),]
abr$MesNuevo <- 4
may<-ventas2019[grep("201905", ventas2019$`Año Mes`),]
may$MesNuevo <- 5
jun<-ventas2019[grep("201906", ventas2019$`Año Mes`),]
jun$MesNuevo <- 6
jul<-ventas2019[grep("201907", ventas2019$`Año Mes`),]
jul$MesNuevo <- 7

ventas2019<-rbind(ene,feb,mar,abr,may,jun,jul)

View(ventas2018)
View(ventas2019)

############################################ PCA ############################################ 
View(vcnum)
# names(vcnum)[12] <- "Porcentaje2"
# vcnum <- vcnum[complete.cases(vcnum),]
# Analizar si se puede usar el análisis factorial para formar combinaciones lineales de las variables
vcnum$Pagina<-NULL
pafvcnum<-paf(as.matrix(vcnum))
pafvcnum$KMO 
pafvcnum$Bartlett 
summary(pafvcnum)
pafvcnum$Correlation
# Nivel de significación de la prueba
cortest.bartlett(vcnum)
# Se calculan componentes principales y normalizan los datos
compPrinc<-prcomp(na.omit(vcnum), scale = TRUE)
summary(compPrinc)
fviz_pca_var(compPrinc, col.var = "cos2",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

# Se hace el PCA
vcnumPCA<-PCA(vcnum,ncp=10, scale.unit = T)
names(vcnum)[1:10]
summary(vcnumPCA)
# Se obtiene la representación de cada variable en cada componente
var<-get_pca_var(vcnumPCA)
corrplot(var$cos2, is.corr = F)

ventas.centroamerica$Pagina...22 <- as.factor(ventas.centroamerica$Pagina...22)
ventas.centroamerica$`Tipo Precio` <- as.factor(ventas.centroamerica$`Tipo Precio`)
ventas.centroamerica$`Energy Chart` <- as.factor(ventas.centroamerica$`Energy Chart`)


############################ ARBOLES DE DECISION ###################################


fitPagina <- rpart(Pagina~ `Tipo Precio` + Categoria + Promociones, method = "class", data = ventas.centroamerica)
rpart.plot(fitPagina)

fitPromociones <- rpart(`Canal de Venta`~ Categoria + Promociones , data = ventas.centroamerica)
rpart.plot(fitPromociones)

fitRecursos <- rpart(`Recursos Especiales` ~ Categoria + Promociones ,data = ventas.centroamerica)
rpart.plot(fitRecursos)


############################ MODELOS DE PREDICCION ###################################


# REDES BAYESIANAS

num <- unlist(lapply(ventas.centroamerica, is.numeric))
cat <- unlist(lapply(ventas.centroamerica, is.factor))
vcnum <- ventas.centroamerica[,num]
View(vcnum)


# NAIVE BAYES
install.packages("e1071")
install.packages("caret")
install.packages("naivebayes")
library(naivebayes)
library(e1071)
library(caret)

porcentaje<-0.7
newVcnum<-na.omit(vcnum)
newVcnum$Pronostico<- as.factor(newVcnum$Pronostico)
newVcnum$`Unidades Vendidas` <- as.factor(newVcnum$`Unidades Vendidas`)
datosTraining<-newVcnum
set.seed(678)

View(datosTraining)
str(datosTraining$Pronostico)

corte<-sample(nrow(datosTraining),nrow(datosTraining)*porcentaje)
train<-datosTraining[corte,]
test<-datosTraining[-corte,]

modelo<-naiveBayes(`Unidades Vendidas`~.,data=train)

predBayes<-predict(modelo, newdata = test)
predBayes

hola<-test
hola$PronosticoBayes<-predBayes

cfmBayes<-confusionMatrix(predBayes,as.factor(test$`Unidades Vendidas`))
cfmBayes

prueba<-ventas.centroamerica
prueba<-data.frame(ventas.centroamerica,stringsAsFactors = TRUE)
View(prueba)


modelo2<-naive_bayes(`Unidades Vendidas`~.,data=train,laplace = 1)
predBayes2<-predict(modelo2, newdata = test)
cfmBayes2<-confusionMatrix(predBayes2, as.factor(test$`Unidades Vendidas`))
cfmBayes2
############################ Redes neuronales ###################################

#Seleccionamos un dataframe solo con el producto, las unidades vendidas, y el pronostico por unidad
p_uv_pro <- ventas.centroamerica[,c(2,11,12)]

#Agrupamos por producto, y sumamos sus u/v y pronosticos a traves de los años
p_uv_pro <- aggregate(cbind(p_uv_pro$Pronostico, p_uv_pro$`Unidades Vendidas`),by=list(Producto=p_uv_pro$Producto), FUN=sum)

p_uv_pro$diferencia <- p_uv_pro$V1 - p_uv_pro$V2


View(prueba)
######################## NUEVO ANALISIS EXPLORATORIO ######################## 


table(ventas.centroamerica$PaisTexto)
prueba<-guatemala
prueba[is.na(prueba)] <- 0
guatemala<-prueba

prueba<-el_salvador
prueba[is.na(prueba)]<- 0
el_salvador<-prueba

prueba<-honduras
prueba[is.na(prueba)]<- 0
honduras<-prueba

prueba<-nicaragua
prueba[is.na(prueba)]<- 0
nicaragua<-prueba

prueba<-vcnum
prueba[is.na(prueba)] <- 0
View(prueba)
vcnum <- prueba


hola<-vcnum[vcnum$CanalVentaNum == 1,]
View(hola)
VentasCatalogo<-sum(hola$`Unidades Vendidas`)

hola<-vcnum[vcnum$CanalVentaNum == 2,]
View(hola)
VentasContingencia<-sum(hola$`Unidades Vendidas`)

hola<-vcnum[vcnum$CanalVentaNum == 3,]
View(hola)
VentasScentiaAlDia<-sum(hola$`Unidades Vendidas`)

ventasCanalVentas<-c(VentasCatalogo,VentasContingencia,VentasScentiaAlDia)
ventasCanalVentas

barplot(ventasCanalVentas, xlab = "Canal de Venta", ylab = "Unidades Vendidas", main = "Ventas por Canal de Venta", names.arg = c("Catalogo", "Contingencia", "Scentia Al Dia"))
