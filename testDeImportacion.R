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

#Librerias
#Programa para juntar todos los xls en una tabla y exportar un RDATA
library("readxl")
library("dplyr")
library(frequency)

# Unión de todos los datos individuales para lograr un conjunto centroamericano
el_salvador <- read_excel("el_salvador.xlsx", range = "A2:AE6370")
nicaragua <- read_excel("nicaragua.xlsx", range = "A2:AE6374")
honduras <- read_excel("honduras.xlsx", range = "A2:AE6685")
guatemala <- read_excel("guatemala.xlsx",range = "A2:AE7719")

#Arreglo para poder correr rbind
names(el_salvador)[names(el_salvador) == "conca"] <- "CONCA"

#Union de todas las tablas
ventas.centroamerica<-rbind(guatemala,honduras,nicaragua,el_salvador)

#Cambio de nombres a columnas
names(ventas.centroamerica)[names(ventas.centroamerica) == "Pagina..7"] <- "Pagina_cat"
names(ventas.centroamerica)[names(ventas.centroamerica) == "Pagina..22"] <- "Pagina"
names(ventas.centroamerica)[names(ventas.centroamerica) == "%..24"] <- "Porcentaje"
names(ventas.centroamerica)[names(ventas.centroamerica) == "%..26"] <- "Porcentaje"

#Arreglo nivel 210811 en categoría Año mes
ventas.centroamerica$`Año Mes` <- gsub("210811","201811",ventas.centroamerica$`Año Mes`)

#Quitar caracteres de descripcion
ventas.centroamerica$Descripcion <- gsub("\r|\n","",ventas.centroamerica$Descripcion)

#contingencia FARMA farma
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

#Cambio de columnas. De tipo char a factor
col_names <- c("Año Mes","Producto","Codigo Catalogo","CONCA","Tipo Comision","Pagina_cat","Descripcion","Categoria","Linea","Observaciones","Canal de Venta","Contingencia","Pagina","Tipo Precio","Atributo Neto","Energy Chart","Promociones","Recursos Especiales","Treboles extra")
ventas.centroamerica$Costo <- as.numeric(ventas.centroamerica$Costo)
ventas.centroamerica[col_names] <- lapply(ventas.centroamerica[col_names] , factor)

#Codigo utilizado para hacer un resumen del dataset con todos los niveles de los factores para corregir uno por uno
# sink("resumen_datos.png")
# print(summary(ventas.centroamerica, maxsum = max(lengths(lapply(ventas.centroamerica, unique)))))
# sink()

#Guardamos la unión como un archivo RData
#save(ventas.centroamerica, file = "ventas_centroamerica.RData")

#-----------------------------------------------------------------
# ANIO - MES (ventas centroamerica)

h1<-table(ventas.centroamerica$`Año Mes`)
FRECventas2018<-h1[1:12]
meses<-c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago","Sept", "Oct", "Nov","Dic")
meses2<-c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul")
barplot(FRECventas2018, main = "VENTAS EN 2018", names.arg = meses)
FRECventas2019<-h1[13:19]
barplot(FRECventas2019, main = "VENTAS EN 2019", names.arg = meses2)
barplot(h1, main = "VENTAS TOTALES", names.arg = c(meses, meses2))

# PRODUCTO
productos<-table(as.factor(ventas.centroamerica$Producto))
str(productos)
h1<-productos[order(-productos)]
head(h1)
xx<-barplot(head(h1, n = 10), names.arg = NA, horiz = FALSE, main = "PRODUCTOS MAS VENDIDOS")
?barplot
head(h1, n = 10)
yy<-c(73,73,72,72,71,70,70,70,70,70)
text(x = xx, y = yy, labels = yy, pos = 1, cex = 0.8, col = "red")
# 4123800056 4123800080 4123800001 4123800030 4123811146 4123660214 4130000001 4130000002 4130000003 4130000004
p1<-ventas.centroamerica$Descripcion[ventas.centroamerica$Producto == "4123800056"][1]
p2<-ventas.centroamerica$Descripcion[ventas.centroamerica$Producto == "4123800080"][1]
p3<-ventas.centroamerica$Descripcion[ventas.centroamerica$Producto == "4123800001"][1]
p4<-ventas.centroamerica$Descripcion[ventas.centroamerica$Producto == "4123800030"][2]
p5<-ventas.centroamerica$Descripcion[ventas.centroamerica$Producto == "4123811146"][1]
p6<-ventas.centroamerica$Descripcion[ventas.centroamerica$Producto == "4123660214"][1]
p7<-ventas.centroamerica$Descripcion[ventas.centroamerica$Producto == "4130000001"][1]
p8<-ventas.centroamerica$Descripcion[ventas.centroamerica$Producto == "4130000002"][1]
p9<-ventas.centroamerica$Descripcion[ventas.centroamerica$Producto == "4130000003"][1]
p10<-ventas.centroamerica$Descripcion[ventas.centroamerica$Producto == "4130000004"][1]
ps<-c(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10)
ps
axis(1, at=xx, labels=ps, tick=FALSE, las=2, line=-11, cex.axis=0.5)
?barplot

# CODIGO CATEGORIA
categorias<-table(ventas.centroamerica$Categoria)
categorias
h1<-categorias[order(-categorias)]
h1
barplot(h1)

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

#Histograma de categoria pronostico
hist(ventas.centroamerica$Pronostico, breaks=10, col="blue", xlab="Petal Length", main="Colored histogram")

