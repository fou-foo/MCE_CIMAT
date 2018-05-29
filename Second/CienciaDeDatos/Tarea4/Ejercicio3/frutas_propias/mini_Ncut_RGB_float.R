setwd('/home/fou/Desktop/MCE_CIMAT/Second/CienciaDeDatos/Tarea4/Ejercicio3/frutas_propias/recortes')
path.escritura <- '/home/fou/Desktop/MCE_CIMAT/Second/CienciaDeDatos/Tarea4/Ejercicio3/frutas_propias/segmentacion/'
library(RcppEigen) #libreria para codigo c++
library(RSpectra) #libreria para lanczos
library(imager) #libreria para leer imagenes
library(Matrix)
library(RcppArmadillo)
library(Rcpp) #libreria para codigo c++
sourceCpp('/home/fou/Desktop/MCE_CIMAT/Second/CienciaDeDatos/Tarea4/Ejercicio3/W_RGB_float.cpp') #compilamos el programa en C++
t1 <- Sys.time() #medimos tiempo de ejecucion
imagenes <- dir()
i <- imagenes[1]
for(i in imagenes){
name <- paste0(path.escritura, paste0('seg_',i))
#print(i)
cat(i)
imagen <- load.image(i)
dim(imagen)
plot(imagen) #visualizamos la imagen
#############preprosesamiento  interpolacion 
imagen2 <- imagen

#standarizacion RGB canal por canal
imagen2 <- imagen2[, , 1, 1:3] 
 estandariza <- function(canal){
   M1 <- imagen2[,,canal]
 #  M1 <- (M1-min(M1))/(max(M1)-min(M1))
 return((M1))
 }
M <- lapply(1:3, FUN = estandariza)
library(abind)
M <- abind(M[[1]], M[[2]],M[[3]], along = 3)
gray.imagen <- as.cimg(M)
plot(gray.imagen) #imagen normalizada
##
sig <- 1
siz <- var(M)
#decidimos no usar filtros pasabajas
(h <- 100)
(w <- 100 )
## definimos parametros
porcentaje <- .1
edges <- ((h*w)*(h*w-1))*(porcentaje) #segun el paper se pueden remover hasta el 90% de las aristas deberia de ser ((h*w)*(h*w+1)/2)*.1
edges.sugerido <- edges/(h*w) #promedio de aristas por nodo
cuadrado <- edges.sugerido**.5 # vamos a fijar esta cantidad
cuadrado <- round(cuadrado) +1 
sigJ <- 0.05 #ver paper
sigd <- 10#ver paper
#r2 <- 25.*25. #se pueden quitar hasta el 90% de las aristas segun el paper
r2 <- cuadrado**2
#r2 <- 33
W <- Kernel_RGB(M, h, w, r2, sigJ, sigd, dimension=3)
W <- as(W, "sparseMatrix")
print('tick1')
remove(M)
gc()
d <- Matrix::colSums(W) #obtenemos suma por columnas
D_medio <- Matrix::.sparseDiagonal(n = h*w, x = d**(-.5) ) #calculamos la matriz D^{-1/2} para el problema de valores propios generalizado
W <- D_medio%*%(Matrix::.sparseDiagonal(n = h*w, x = d ) -W)%*%D_medio 
print('tick2')
Z <- eigs_sym(W, k=3, which='LM', sigma = 0) #usamos lanczos
Z$values #visualizamos los tres valores propios mas pequenios
remove(W) #ahorramos RAM
remove(d)
gc()
print('tick3')
#####################
Y1 <- D_medio%*%(Z$vectors[,1]) #MI GRAN DUDA ERA ESTA, como usar los vectores propios que se encontraron para segmentar
hist(as.matrix(Y1))
Y2 <- D_medio%*%Z$vectors[,2]
Y3 <- D_medio%*%Z$vectors[,3]
hist(as.matrix(Y1))
set.seed(0)
Aplica.Mascara <- function( canal){
  M1 <- imagen2[,,canal]
  M1 <- as.matrix(M1)*mascara
  return((M1))
}
set.seed(0)
data.a.segmentar <- cbind(Y1,Y2 )
kmeans2 <- kmeans(data.a.segmentar, centers = 2, nstart = 100)
kmeans2$cluster <- -1+kmeans2$cluster
print(table(kmeans2$cluster))
mascara <- matrix(kmeans2$cluster, ncol = h, byrow = TRUE)
segmentacion <- mascara
table(segmentacion)
M <- lapply( FUN = Aplica.Mascara, X=1:3)
library(abind)
M <- abind(M , along = 3)
imagen.final <- as.cimg(M)
plot(imagen.final)#CON MADRE
t1 <- Sys.time() -t1
print(t1)
#scan()
r <- imagen.final[,,1] == 0
imagen.final[,,1][r] <- 1
g <- imagen.final[,,2] == 0
imagen.final[,,1][g] <- 0
b <- imagen.final[,,3] == 0
imagen.final[,,1][b] <- 0
save.image(imagen.final, file = name)
gc()
}
