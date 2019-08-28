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


Paginacion <- read_excel("Paginacion 2015-2019.xlsx")

colnames(ventas.centroamerica)

#Guardamos la unión como un archivo RData
save(ventas.centroamerica, file = "ventas_centroamerica.RData")

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
