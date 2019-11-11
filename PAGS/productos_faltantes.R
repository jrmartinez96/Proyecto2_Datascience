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
library(neuralnet)

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

#Convertimos las columnas del dataframe para facilitar nuestro análisis
PagCentroamerica$anomes<-as.factor(PagCentroamerica$anomes)

#Cambiamos todos los NULL a 0 
PagCentroamerica[is.null(PagCentroamerica)] <- 0
PagCentroamerica[PagCentroamerica == "NULL"] <- 0
PagCentroamerica[is.na(PagCentroamerica)] <- 0

PagCentroamerica$Pagina <- as.numeric(PagCentroamerica$Pagina)
PagCentroamerica$Precio.Catalogo <- as.numeric(PagCentroamerica$Precio.Catalogo)
PagCentroamerica$`Precio.Vta.s/iva`<- as.numeric(PagCentroamerica$`Precio.Vta.s/iva`)
PagCentroamerica$Costo <- as.numeric(PagCentroamerica$Costo)

#Seleccionamos un dataframe solo con el producto, las unidades vendidas, y el pronostico por unidad
p_uv_pro <- PagCentroamerica[,c(2,10,11)]

#Agrupamos por producto, y sumamos sus u/v y pronosticos a traves de los años
p_uv_pro <- aggregate(cbind(p_uv_pro$Pronostico, p_uv_pro$Unidades.Vendidas),by=list(Producto=p_uv_pro$Producto), FUN=sum)

#Cambiar nombres
names(p_uv_pro) = c("producto","pronostico", "un_vendidas")

#Hacemos una diferencia 
p_uv_pro$diferencia <- p_uv_pro$pronostico - p_uv_pro$un_vendidas

#Faltantes
faltantes <- p_uv_pro[order(p_uv_pro$diferencia),]

#Excedente
#p_uv_pro_desc <- p_uv_pro[order(-p_uv_pro$diferencia),]3

# --> Analizamos cada uno de los primeros cinco productos 

#Producto numero 1 - DUO SOFT LADY SHAMPOO + SPRAY
lady_shampoo <- subset(PagCentroamerica, Producto == "4123310128 ")

#Utilizamos unicamente las variables numericas para hacer el modelo de red neuronal
lady_shampoo_num <- lady_shampoo[,unlist(lapply(lady_shampoo, is.numeric))]

porcentaje <- 0.8
corte_nn<-sample(nrow(lady_shampoo_num),nrow(lady_shampoo_num)*porcentaje)
train_nn<-lady_shampoo_num[corte_nn,]
test_nn<-lady_shampoo_num[-corte_nn,]

colnames(train_nn) <- c("pagina","precio_cat","precio_venta_sinva","pronostico","u_vendidas","venta_neta_sin_iva","costo","utilidad","margen","pedido_real","ratio")
colnames(test_nn) <- c("pagina","precio_cat","precio_venta_sinva","pronostico","u_vendidas","venta_neta_sin_iva","costo","utilidad","margen","pedido_real","ratio")

nn_model <- neuralnet(u_vendidas~., data = train_nn, hidden=c(4,3,2,1), linear.output=FALSE, threshold=0.01 )

pred <- compute(nn_model, test_nn)

