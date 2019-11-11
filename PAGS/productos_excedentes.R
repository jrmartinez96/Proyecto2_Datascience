#Universidad del Valle de Guatemala
#Data science
#Proyecto número 2 - script para encontrar y predecir productos faltantes en stock 

#Paquetes a utilizar
#install.packages("openxlsx")
#install.packages("forecast")
#install.packages("fUnitRoots")
#install.packages("ggfortify")
#install.packages("xts")
#install.packages("neuralnet")
install.packages("devtools")

#Librerias
library(forecast)
library(tseries)
library(fUnitRoots)
library(ggfortify)
library(xts)
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


set.seed(123)







# PRODUCTO 4123310125
p_1 <- subset(PagCentroamerica, Producto == "4123310125 ")

p_1 <- p_1[,num <- unlist(lapply(PagCentroamerica, is.numeric))]
p_1$Pronostico <- NULL
colnames(p_1) <- c("unidades_vendidas", "venta_neta_sin_iva", "utilidad", "margen", "pedido_real", "ratio")

max_1 = apply(p_1 , 2 , max)
min_1 = apply(p_1, 2 , min)
scaled_1 = as.data.frame(scale(p_1, center = min_1, scale = max_1 - min_1))

## TRAIN Y TEST 4123310125
smp_size_1 <- floor(0.90 * nrow(p_1))
train_ind_1 <- sample(seq_len(nrow(p_1)), size = smp_size_1)

train_1 <- scaled_1[train_ind_1, ]
test_1 <- scaled_1[-train_ind_1, ]
test_1 <- as.data.frame(test_1)

nn_model_1 <- neuralnet(unidades_vendidas ~., data = train_1, hidden=c(3,2,1), linear.output=TRUE, threshold=0.01 )
plot(nn_model_1, arrow.length=0.13)


predict_testNN_1 = compute(nn_model_1, test_1)
predict_testNN_1 = (predict_testNN_1$net.result * (max(train_1$unidades_vendidas) - min(train_1$unidades_vendidas))) + min(train_1$unidades_vendidas)

#plot(test_1$unidades_vendidas, predict_testNN_1, col='blue', pch=16, ylab = "predicted rating NN", xlab = "real rating")

#abline(0,1)


test_1_unscale <- test_1

test_1_unscale$unidades_vendidas <- predict_testNN_1

ps_1 <- unscale(test_1, center = min_1, scale = max_1 - min_1)
ps2_1 <- unscale(test_1_unscale, center = min_1, scale = max_1 - min_1)

comparasion_1 <- data.frame(actual=ps_1$unidades_vendidas, pred=ps2_1$unidades_vendidas)
View(comparasion_1)







# PRODUCTO 4123351013
p_2 <- subset(PagCentroamerica, Producto == "4123351013 ")

p_2 <- p_2[,num <- unlist(lapply(PagCentroamerica, is.numeric))]
p_2$Pronostico <- NULL
colnames(p_2) <- c("unidades_vendidas", "venta_neta_sin_iva", "utilidad", "margen", "pedido_real", "ratio")

max_2 = apply(p_2 , 2 , max)
min_2 = apply(p_2, 2 , min)
scaled_2 = as.data.frame(scale(p_2, center = min_2, scale = max_2 - min_2))

## TRAIN Y TEST 4123310125
smp_size_2 <- floor(0.90 * nrow(p_2))
train_ind_2 <- sample(seq_len(nrow(p_2)), size = smp_size_2)

train_2 <- scaled_2[train_ind_2, ]
test_2 <- scaled_2[-train_ind_2, ]
test_2 <- as.data.frame(test_2)

nn_model_2 <- neuralnet(unidades_vendidas ~., data = train_2, hidden=c(3,2,1), linear.output=TRUE, threshold=0.01 )
plot(nn_model_2, arrow.length=0.13)


predict_testNN_2 = compute(nn_model_2, test_2)
predict_testNN_2 = (predict_testNN_2$net.result * (max(train_2$unidades_vendidas) - min(train_2$unidades_vendidas))) + min(train_2$unidades_vendidas)

#plot(test_2$unidades_vendidas, predict_testNN_2, col='blue', pch=16, ylab = "predicted rating NN", xlab = "real rating")

#abline(0,1)


test_2_unscale <- test_2

test_2_unscale$unidades_vendidas <- predict_testNN_2

ps_2 <- unscale(test_2, center = min_2, scale = max_2 - min_2)
ps2_2 <- unscale(test_2_unscale, center = min_2, scale = max_2 - min_2)

comparasion_2 <- data.frame(actual=ps_2$unidades_vendidas, pred=ps2_2$unidades_vendidas)
View(comparasion_2)







# PRODUCTO 4123310126
p_3 <- subset(PagCentroamerica, Producto == "4123310126 ")

p_3 <- p_3[,num <- unlist(lapply(PagCentroamerica, is.numeric))]
p_3$Pronostico <- NULL
colnames(p_3) <- c("unidades_vendidas", "venta_neta_sin_iva", "utilidad", "margen", "pedido_real", "ratio")

max_3 = apply(p_3 , 2 , max)
min_3 = apply(p_3, 2 , min)
scaled_3 = as.data.frame(scale(p_3, center = min_3, scale = max_3 - min_3))

## TRAIN Y TEST 4123310125
smp_size_3 <- floor(0.90 * nrow(p_3))
train_ind_3 <- sample(seq_len(nrow(p_3)), size = smp_size_3)

train_3 <- scaled_3[train_ind_3, ]
test_3 <- scaled_3[-train_ind_3, ]
test_3 <- as.data.frame(test_3)

nn_model_3 <- neuralnet(unidades_vendidas ~., data = train_3, hidden=c(3,2,1), linear.output=TRUE, threshold=0.01 )
plot(nn_model_3, arrow.length=0.13)


predict_testNN_3 = compute(nn_model_3, test_3)
predict_testNN_3 = (predict_testNN_3$net.result * (max(train_3$unidades_vendidas) - min(train_3$unidades_vendidas))) + min(train_3$unidades_vendidas)

#plot(test_3$unidades_vendidas, predict_testNN_3, col='blue', pch=16, ylab = "predicted rating NN", xlab = "real rating")

#abline(0,1)


test_3_unscale <- test_3

test_3_unscale$unidades_vendidas <- predict_testNN_3

ps_3 <- unscale(test_3, center = min_3, scale = max_3 - min_3)
ps2_3 <- unscale(test_3_unscale, center = min_3, scale = max_3 - min_3)

comparasion_3 <- data.frame(actual=ps_3$unidades_vendidas, pred=ps2_3$unidades_vendidas)
View(comparasion_3)








# PRODUCTO 4123042257
p_4 <- subset(PagCentroamerica, Producto == "4123042257 ")

p_4 <- p_4[,num <- unlist(lapply(PagCentroamerica, is.numeric))]
p_4$Pronostico <- NULL
colnames(p_4) <- c("unidades_vendidas", "venta_neta_sin_iva", "utilidad", "margen", "pedido_real", "ratio")

max_4 = apply(p_4 , 2 , max)
min_4 = apply(p_4, 2 , min)
scaled_4 = as.data.frame(scale(p_4, center = min_4, scale = max_4 - min_4))

## TRAIN Y TEST 4123310125
smp_size_4 <- floor(0.90 * nrow(p_4))
train_ind_4 <- sample(seq_len(nrow(p_4)), size = smp_size_4)

train_4 <- scaled_4[train_ind_4, ]
test_4 <- scaled_4[-train_ind_4, ]
test_4 <- as.data.frame(test_4)

nn_model_4 <- neuralnet(unidades_vendidas ~., data = train_4, hidden=c(3,2,1), linear.output=TRUE, threshold=0.01 )
plot(nn_model_4, arrow.length=0.13)


predict_testNN_4 = compute(nn_model_4, test_4)
predict_testNN_4 = (predict_testNN_4$net.result * (max(train_4$unidades_vendidas) - min(train_4$unidades_vendidas))) + min(train_4$unidades_vendidas)

#plot(test_4$unidades_vendidas, predict_testNN_4, col='blue', pch=16, ylab = "predicted rating NN", xlab = "real rating")

#abline(0,1)


test_4_unscale <- test_4

test_4_unscale$unidades_vendidas <- predict_testNN_4

ps_4 <- unscale(test_4, center = min_4, scale = max_4 - min_4)
ps2_4 <- unscale(test_4_unscale, center = min_4, scale = max_4 - min_4)

comparasion_4 <- data.frame(actual=ps_4$unidades_vendidas, pred=ps2_4$unidades_vendidas)
View(comparasion_4)







# PRODUCTO 4123811146
p_5 <- subset(PagCentroamerica, Producto == "4123811146 ")

p_5 <- p_5[,num <- unlist(lapply(PagCentroamerica, is.numeric))]
p_5$Pronostico <- NULL
colnames(p_5) <- c("unidades_vendidas", "venta_neta_sin_iva", "utilidad", "margen", "pedido_real", "ratio")

max_5 = apply(p_5 , 2 , max)
min_5 = apply(p_5, 2 , min)
scaled_5 = as.data.frame(scale(p_5, center = min_5, scale = max_5 - min_5))

## TRAIN Y TEST 4123310125
smp_size_5 <- floor(0.90 * nrow(p_5))
train_ind_5 <- sample(seq_len(nrow(p_5)), size = smp_size_5)

train_5 <- scaled_5[train_ind_5, ]
test_5 <- scaled_5[-train_ind_5, ]
test_5 <- as.data.frame(test_5)

nn_model_5 <- neuralnet(unidades_vendidas ~., data = train_5, hidden=c(3,2,1), linear.output=TRUE, threshold=0.01 )
plot(nn_model_5, arrow.length=0.13)


predict_testNN_5 = compute(nn_model_5, test_5)
predict_testNN_5 = (predict_testNN_5$net.result * (max(train_5$unidades_vendidas) - min(train_5$unidades_vendidas))) + min(train_5$unidades_vendidas)

#plot(test_5$unidades_vendidas, predict_testNN_5, col='blue', pch=16, ylab = "predicted rating NN", xlab = "real rating")

#abline(0,1)


test_5_unscale <- test_5

test_5_unscale$unidades_vendidas <- predict_testNN_5

ps_5 <- unscale(test_5, center = min_5, scale = max_5 - min_5)
ps2_5 <- unscale(test_5_unscale, center = min_5, scale = max_5 - min_5)

comparasion_5 <- data.frame(actual=ps_5$unidades_vendidas, pred=ps2_5$unidades_vendidas)
View(comparasion_5)