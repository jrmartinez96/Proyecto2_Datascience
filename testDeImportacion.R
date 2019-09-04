# --------------------------------- Proyecto numero 2 Data science -------------------------------
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

# Unión de todos los datos individuales para lograr un conjunto centroamericano
el_salvador <- read_excel("el_salvador.xlsx", range = "A2:AE6370")
nicaragua <- read_excel("nicaragua.xlsx", range = "A2:AE6374")
honduras <- read_excel("honduras.xlsx", range = "A2:AE6685")
guatemala <- read_excel("guatemala.xlsx",range = "A2:AE7719")

#en el salvador el nombre de la categoria conca se encuentra en minusculas, se transforma a mayúsculas
#para poder llevar a cabo sin problemas la función rbind
names(el_salvador)[names(el_salvador) == "conca"] <- "CONCA"

#Union de todas las tablas para crear un conjunto que represente a centroamérica
ventas.centroamerica<-rbind(guatemala,honduras,nicaragua,el_salvador)

# ------------------------ Proceso de limpieza de los datos ---------------------------

#Cambio de nombres a columnas
names(ventas.centroamerica)[names(ventas.centroamerica) == "Pagina..7"] <- "Pagina_cat"
names(ventas.centroamerica)[names(ventas.centroamerica) == "Pagina..22"] <- "Pagina"
names(ventas.centroamerica)[names(ventas.centroamerica) == "%..24"] <- "Porcentaje"
names(ventas.centroamerica)[names(ventas.centroamerica) == "%..26"] <- "Porcentaje2"

#Arreglo nivel 210811 en categoría Año mes
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
View(ventas.centroamerica)
#Codigo utilizado para hacer un resumen del dataset con todos los niveles de los factores para corregir uno por uno
# sink("resumen_datos.png")
# print(summary(ventas.centroamerica, maxsum = max(lengths(lapply(ventas.centroamerica, unique)))))
# sink()

#Guardamos la unión como un archivo RData
#save(ventas.centroamerica, file = "ventas_centroamerica.RData")

#---------------------- Graficas para entender comportamiento  y estado de los datos -----------

# Gráfico de  ventas por AÑO - MES (ventas centroamerica)
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
productos<-table(ventas.centroamerica$Producto)
productos_ordenados<-productos[order(-productos)]
#Cantidad de cuantos se vendieron de los 10 productos mas vendidos
yy<-as.character(as.vector(head(productos_ordenados, n = 10)))
#Ajustamos margenes para el grafico
par(mar = c(4, 4, 4, 9), xpd = TRUE)
#Obtenemos los 10 productos mas vendidos
productos_ordenados <- head(productos_ordenados, n=10)
#Lo graficamos
diez_mas_vendidos<-barplot(productos_ordenados,legend.text = yy, args.legend = list(x = "topright", bty = "n", inset=c(-0.15, 0)),horiz = FALSE, main = "PRODUCTOS MAS VENDIDOS", col = c("darkseagreen","darkseagreen1","darkseagreen2","darkseagreen3","darkseagreen4","darkslategray1","darkslategray2","darkslategray3","darkslategray4","grey"))

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
#Debido a la gran cantidad de datos, se escogen únicamente reglas con 0.9 de soportey confianza, así como se limitan parametros para el tiempo de búsqueda (maxtime y maxlen)
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



