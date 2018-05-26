setwd('C:\\Users\\fou-f\\Desktop\\MCE\\Second\\EstadisticaMultivariada\\labs\\lab6')
# Practica . Aplicacion de analisis de factores a un conjunto de datos con el fin de reducir la dimensionalidad de los datos
library("MASS")
library("alr3")
library("stats")
library("scatterplot3d")
library("psych")
library("datasets")
library("MVA")


######################################################################################

#Primer conjunto de datos

#Los datos consisten de las caracter?sticas de veh?culos de diferentes marcas  y modelos.
#El objetivo principal del estudio es predecir las ventas de autom?viles a partir del conjunto de variables.
#Sin embargo, algunas de estas variables est?n correlacionadas, lo cual  puede afectar desfavorablemente
#los resultados de la predicci?n.

#Las variables a considerar son:
# Tipo de veh?culo
#Precio del veh?culo
#Tama?o del motor
#Caballos de fuerza
#Distancia entre ejes
#Ancho
#Longitud
#Peso neto
#Capacidad de combustible
#Consumo

## setwd("c:/AdministracionProyectos/2017/CIMAT/Curso_analisis_multivariado_MCE_Enero_junio2017/AnalisisFactor_Datos/")

ventas_coche <- read.csv("car_sales.csv", header = TRUE, sep = ",", quote = "\"",dec = ".", fill = TRUE, na.strings = "NA")
ventas <- na.omit(ventas_coche)
#nombre de variables
lista <- names(ventas_coche)

#resumen de la estructura de los datos
str(ventas_coche)

# Se eliminan las filas con NA utilizando la funci?n na.omit()
ventas_coche <- na.omit(ventas_coche)

#se extraen los nombres de los casos
nombres_coches <- ventas_coche[,"manufact"]

# se extraen solo las variables de interes
ventas_coche1 <- ventas_coche[,5:14]

# Se revisa la correlaci?n entre las variables para ver si vale la pena realizar analisis de factores
ventas_coche1_corr <- cor(ventas_coche1)
(det_vc <- det(ventas_coche1_corr))


#prueba de esfericidad de bartlett en R para probar la hipotesis nula de que las variables no estan correlacionadas.
# La idea es rechazar la hipotesis nula para proseguir con un analisis de factores
cortest.bartlett(ventas_coche1_corr, n = dim(ventas_coche)[1])

# Se realiza un analisis de factores utilizando maxima verosimilitud para estimar los parametros del modelo
#(las cargas y las varianzas especificas).

#Por default el analisis de factores se realiza sobre los datos estandarizados z y utilizando la rotaci?n varimax
#se prueba la solucion con un factor (m=1)

#venta_coche.fa1p<- factanal(covmat=ventas_coche1_corr,factors=1,rotation = "none")

(venta_coche.fa1  <- factanal(ventas_coche1,factors=1))
(venta_coche.fa1p <- factanal(covmat=ventas_coche1_corr,factors=1))

#se prueba la solucion con dos factores (m=2)
(venta_coche.fa2 <- factanal(ventas_coche1, factors=2))

#se prueba la solucion con tres factores (m=3)
(venta_coche.fa3 <- factanal(ventas_coche1,factors=3))

#se prueba la solucion con cuatro factores (m=4)
(venta_coche.fa4 <- factanal(ventas_coche1,factors=4))

#se prueba la solucion con cinco factores (m=5)
(venta_coche.fa5 <- factanal(ventas_coche1,factors=5))

#se prueba la solucion con seis factores (m=6)
(venta_coche.fa6 <- factanal(ventas_coche1,factors=6))



#Se calcula la diferencia entre las correlaciones observadas y las predichas para m=3 factores

#Primero se obtiene la estimacion de la matriz de correlaciones
pred3_vc <- venta_coche.fa3$loadings%*%t(venta_coche.fa3$loadings)+diag(venta_coche.fa3$uniquenesses)
round(ventas_coche1_corr-pred3_vc,digits=3)


#Se calcula la diferencia entre las correlaciones observadas y las predichas para m=4 factores
pred4_vc <-venta_coche.fa4$loadings%*%t(venta_coche.fa4$loadings)+diag(venta_coche.fa4$uniquenesses)
round(ventas_coche1_corr-pred4_vc,digits=3)


#Se calcula la diferencia entre las correlaciones observadas y las predichas para m=5 factores
pred5_vc <-venta_coche.fa5$loadings%*%t(venta_coche.fa5$loadings)+diag(venta_coche.fa5$uniquenesses)
round(ventas_coche1_corr-pred5_vc,digits=3)

#Se calcula la diferencia entre las correlaciones observadas y las predichas para m=6 factores
pred6_vc <-venta_coche.fa6$loadings%*%t(venta_coche.fa6$loadings)+diag(venta_coche.fa6$uniquenesses)
round(ventas_coche1_corr-pred6_vc,digits=3)

# por lo anterior el modelo de 3 factores produce una matriz residual cercana a cero. Por tanto un modelo con tres factores puede ser suficiente
# explicar la variabilidad de los datos originales


#Interpretaci?n de los factores

#El factor 1 se relaciona al precio del coche, al tama?o del motor y a los caballos de fuerza, principalmente con el precio
#El factor 2 se relaciona a la distancia entre ejes,anchura y longitud del coche (principalmente con longitud)
#El factor 3 se relaciona a basicamente al tipo de vehiculo



#se calculan los factor scores con el modelo factorial para m=3
#ojo:cuando se deseen obtener los factor scores, se debe utilizar como datos de entrada la matriz original de datos y no la matriz de covarianzas

(venta_coche.fa3 <- factanal(ventas_coche1,factors=3,scores="regression"))
(venta_coche.fa3 <- factanal(ventas_coche1,factors=3,scores="Bartlett"))
scores <- venta_coche.fa3$scores

#se a?aden los nombres de los casos en los factor scores
dimnames(scores)[[1]] <- nombres_coches

#se grafican los casos de acuerdo a los factor scores
scatterplot3d(scores, angle=35, col.grid="lightblue", main="Grafica de los factor scores", pch=20)

#se genera la matriz con  los gr?ficos de los factor scores tomando dos factores a la vez
pairs(scores)

#se genera cada uno de los graficos de los factor scores

#f1 x f2
par(pty="s")
plot(scores[,1],scores[,2],
     ylim=range(scores[,1]),
     xlab="Factor 1",ylab="Factor 2",type="n",lwd=2)
text(scores[,1],scores[,2],
     labels=abbreviate(row.names(scores),minlength=8),cex=0.6,lwd=2)


#f1 x f3
par(pty="s")
plot(scores[,1],scores[,3],
     ylim=range(scores[,1]),
     xlab="Factor 1",ylab="Factor 3",type="n",lwd=2)
text(scores[,1],scores[,3],
     labels=abbreviate(row.names(scores),minlength=8),cex=0.6,lwd=2)


#f2 x f3
par(pty="s")
plot(scores[,2],scores[,3],
     ylim=range(scores[,2]),
     xlab="Factor 2",ylab="Factor 3",type="n",lwd=2)
text(scores[,2],scores[,3],
     labels=abbreviate(row.names(scores),minlength=8),cex=0.6,lwd=2)

#Conclusiones

#La dimension de los datos se redujo de 10 variables a tres factores aplicando an?lisis factorial con el metodo
#de estimacion de MV. Aunque la interpretaci?n de los factores depende de las relaciones definidas por la matriz de cargas
#rotadas, los beneficios de reducir la dimension de los datos y el uso de predictores no correlacionados
#son mayores y son muy utiles para analisis posteriores.
###############################################################################################################################


#Segundo conjunto de datos
telcom <- read.csv("telco.csv", header = TRUE, sep = ",", quote = "\"",dec = ".", fill = TRUE, na.strings = "NA")

#nombre de variables
lista<- names(telcom)

#se identifican las variables (servicios) de interes para el analisis
var_int<- c(16, 17, 18, 19, 20, 26, 27, 28, 29, 30, 31, 32, 33, 34)

# se extraen solo las variables de interes
telcom1 <-telcom[,var_int]


# Se eliminan las filas con NA utilizando la funci?n na.omit()
telcom1 <- na.omit(telcom1)


# Se revisa la correlaci?n entre las variables para ver si vale la pena realizar analisis de factores
telcom1_corr <- cor(telcom1)
(det_telcom <- det(telcom1_corr))
image(telcom1_corr)
#prueba de esfericidad de bartlett
p <- cortest.bartlett(telcom1)
str(p)
p$p.value
# se realiza un analisis de factores (utilizando MV en la estimacion), probando  de 1 a 6 factores y considerando una rotacion varimax
telcom_fa <- vector ("list",6)
telcom_fa <- c()
for(i in 1:6){
   telcom_fa[[i]] <-  factanal(covmat=telcom1_corr,factors=i,n.obs=1000)
}

# Se considera la soluci?n con 4 factores como la adecuada

(telcom.fa4 <- factanal(telcom1,factors=4))


#Se calcula la diferencia entre las correlaciones observadas y las predichas para m=4 factores
pred4_telcom <- telcom.fa4$loadings%*%t(telcom.fa4$loadings)+diag(telcom.fa4$uniquenesses)

round(telcom1_corr-pred4_telcom,digits=3)


#Interpretaci?n de los factores

#El factor 1: Servicio gratis el mes pasado, Identificaci?n de llamadas, Llamadas en espera,desvio de llamada,Llamada de 3 simultaneamente
#El factor 2:  Equipamiento el mes pasado,internet, Facturaci?n electr?nica
#El factor 3 :Larga distancia el mes pasado
#el factor 4 no tiene una clara relacion con algunas variables en particular auqnue parece mas relacionadsa a Wireless el mes pasado


#Por lo tanto, se puede identificar tres grandes grupos de servicios, seg?n la definici?n de los servicios que est?n m?s altamente correlacionados
#con los cuatro factores.

# Grupo1: grupo de servicios extras
# Grupo2: Grupo de servicios de tecnologia
#Grupo de larga distancia

## Conjuntos de datos 3
## Marcadores gen?ticos
hemangioma <- read.table("hemangioma.txt",header=TRUE)
                                          #los datos son marcadores geneticos 
## Revision de correlaciones
image(cor(hemangioma))

#prueba de esfericidad de bartlett
cortest.bartlett(hemangioma)

## Analisis de factores sencillo
factanal(hemangioma, factors = 3)

## Conjunto de datos 4

## Esperanza de vida
## str(life)
## Numero de factores
life <- read.csv("life.csv",row.names=1)
sapply(1:3, function(f) factanal(life, factors = f, method ="mle")$PVAL)

## Solucion de tres factores
p <- factanal(life, factors = 3, method ="mle")

## Scores de los factores
scores <- factanal(life, factors = 3, method = "mle", scores = "regression")$scores

gfactorx <- 1
gfactory <- 2

par(pty="s")
plot(scores[,gfactorx],scores[,gfactory],
     ylim=range(scores[,gfactory]),
     xlab=paste("Factor ", gfactorx),ylab=paste("Factor", gfactory), type="n",lwd=2)
text(scores[,gfactorx],scores[,gfactory],
     labels=abbreviate(row.names(scores),minlength=8),cex=0.6,lwd=2)

fnPlotF <- function(gfactorxf, gfactoryf,scoresf) {
  par(pty="s")
  plot(scoresf[,gfactorxf],scoresf[,gfactoryf],
       ylim=range(scoresf[,gfactoryf]),
       xlab=paste("Factor ", gfactorxf),ylab=paste("Factor", gfactoryf), type="n",lwd=2)
  text(scores[,gfactorxf],scoresf[,gfactoryf],
       labels=abbreviate(row.names(scoresf),minlength=8),cex=0.6,lwd=2)
}

fnPlotF(1,2,scores)
fnPlotF(1,3,scores)
fnPlotF(2,3,scores)

## Conjunto de datos 5
## uso de drogas
druguse <- as.matrix(read.csv("druguse.csv",row.names=1))
str(druguse)

## Numero de factores
sapply(1:7, function(nf) factanal(covmat = druguse, factors = nf, method = "mle", n.obs = 1634)$PVAL)

## Solucion propuesta
factanal(covmat = druguse, factors = 6,method = "mle", n.obs = 1634)

## Diferencia en las matrices calculadas y la real de covarianza
pfun <- function(nf) {
  nf <- 6
  (fa <- factanal(covmat = druguse, factors = nf, method = "mle", n.obs = 1634))
  (est <- tcrossprod(fa$loadings) + diag(fa$uniquenesses))
  (ret <- round(druguse - est, 3))
  (colnames(ret) <- rownames(ret) <- abbreviate(rownames(ret), 3))
  (ret)
}

pfun(6)
