setwd('C:\\Users\\fou-f\\Desktop\\MCE_CIMAT\\Second\\EstadisticaMultivariada\\labs\\LabMDS')

# Practica 5. Aplicacion de modelos de escalamiento multidimensional
library("MASS")
library("alr3")
library("stats")
library("scatterplot3d")
library("smacof")

#source("/OTRAS ACTIVIDADES ROD/curso_INEGI_AGS_MULTIVARIADO/analisis_factores/functions_imp.r")

######################################################################################

#se leen los datos de similaridades entre naciones. 

prox_nations <- read.csv("DATOS_NACIONES.csv", header = TRUE, sep = ",", quote = "\"",dec = ".", fill = TRUE, na.strings = "NA")

#nombre de las naciones
nombres<- names(prox_nations)

# Se aplica el modelo cl?sico de MDS (coordenadas principales)

#Nota: como los datos representan similaridades, debemos transformarlas a disimilaridades.
#primero identificamos el valor maximo en las similaridades, entonces las similaridades las transformamos a disimilaridades como disi=(max(sim)+1)-sim
maximo <- max(prox_nations)+1
disim_nations <- maximo-prox_nations

#sin embargo los elementos de la diagonal de la matriz de disimilaridades(disim_nations) son distintos de cero, todos
#son iguales al maximo de la similaridades. Entonces los elementos diagonales de la matriz de disimilaridades los
#convertimos en ceros
disim_nations <- disim_nations- diag(maximo,length(disim_nations),length(disim_nations))

#funci?n que realiza el mds clasico
resul_mds_clas<-cmdscale(disim_nations, k = 2, eig = TRUE, add = FALSE, x.ret = FALSE)

#k es la dimension de la solucion
#eig: indica si los valores propios se regresan
#add: cuando  se a?ade una constante aditiva para convertir las disimilaridades en distancias euclideas
#x.ret indica si la matriz de doble centrado (B) se regresa

#Nota: si no se especifica la dimension, por default la soluci?n se da en dos dimensiones


# se extrae la configuracion solucion X
config_nations<-resul_mds_clas$points

#se a?aden los nombres de las naciones a la configuracion solucion X
dimnames(config_nations)[[1]]<-nombres

#se grafica la configuracion solucion obtenida mediante mds clasico

plot(config_nations[,1],config_nations[,2],main="configuracion solucion mediante mds clasico",ylim=range(config_nations[,1]),
     xlab="dim 1",ylab="dim 2",type="n",lwd=2)
text(config_nations[,1],config_nations[,2],
     labels=abbreviate(row.names(config_nations),minlength=8),cex=0.6,lwd=2)

# se obtiene la proporcion de la varianza total explicada por las dos dimensiones
resul_mds_clas$GOF

########################################################################
# Se aplica el modelo de minimos cuadrados utilizando el algoritmo SMACOF
resul_mds_mc <- smacofSym(disim_nations, ndim=2,weightmat = NULL, init = "torgerson",
                           verbose = TRUE, relax = FALSE, itmax = 1000, eps = 1e-06) 

#init: indica la configuracion inicial que se eligira, por lo general se elige la solucion clasica
#tambien llamada de Torgeson


#se grafica la configuracion solucion obtenida mediante smacof,  incluyendo los nombres de los paises
config_nations_mc<-resul_mds_mc$conf

plot(config_nations_mc[,1],config_nations_mc[,2],main="configuracion solucion mediante smacof",ylim=range(config_nations_mc[,1]),
     xlab="Dim 1",ylab="Dim 2",type="n",lwd=2)
text(config_nations_mc[,1],config_nations_mc[,2],
     labels=abbreviate(nombres,minlength=8),cex=0.6,lwd=2)

#plot(resul_mds_mc,main = "Diagrama de Shepard", plot.type = "Shepard")

#se obtiene el diagrama de shepard, para evaluar la calidad de la soluci?n, graficando
#las disparidades vs las distancias ajustadas
plot(resul_mds_mc,
     main = "Grafica de las disparidades vs las distancias ajustadas",
     plot.type="histogram")
plot(as.vector(resul_mds_mc$delta)**2, as.vector(resul_mds_mc$dhat)**2)
(resul_mds_mc$delta- resul_mds_mc$dhat)

#Conclusiones
#El grafico muestra la oposicion que existia entre paises desarrollados y subdesarrollados y paises occidentales y comunistas
#en los a?os 70, reflejando la situaci?n socio politica de los paises en esa epoca

#la dim 1 se puede interpretar como una dimension de desarrollo econ?mico
#la dim 2 se puede interpretar como una dimension de alineacion pol?tica o regimen politico

#Nota: si la configuracion  es rotada y trasladada, las distancias entre los paisess no cambiaran, por tanto se dice que
#las soluciones de MDS son invariantes antes rotaciones y traslaciones. Por tanto las configuraciones se pueden rotar para
#obtener una solucion mas facil de interpretar.


  