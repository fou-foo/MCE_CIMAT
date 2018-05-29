setwd('/home/fou/Desktop/MCE_CIMAT/Second/CienciaDeDatos/Tarea4/Ejercicio3/frutas_propias/recortes')
library(Rcpp) #libreria para codigo c++
library(RcppEigen) #libreria para codigo c++
library(RSpectra) #libreria para lanczos
library(imager) #libreria para leer imagenes
sourceCpp('/home/fou/Desktop/MCE_CIMAT/Second/CienciaDeDatos/Tarea4/Ejercicio3/W_float.cpp') #compilamos el programa en C++
t1 <- Sys.time() #medimos tiempo de ejecucion
dir()
imagen <- load.image('001-1.jpg')
dim(imagen)
plot(imagen) #visualizamos la imagen
#############preprosesamiento  160*120 jala bien, recortamos la imaagen para que quepa en memoria 
gray.imagen <- grayscale(imagen) #cambiamos a escala de grises
dim(gray.imagen) #verificamos tamanio de la imagen
remove(imagen) #removemos del ambiente la imagen original para ahorra memoria
gc()
#estandarizacion escala de grises
M <- as.matrix(gray.imagen)
M <- (M -min(M))/(max(M)-min(M))
sig <- 1
siz <- 2*round(3*sig) + 1
(h <- dim(M)[2])
(w <- dim(M)[1] )
vecinos <- .1
edges <- ((h*w)*(h*w-1))*vecinos #segun el paper se pueden remover hasta el 90% de las aristas deberia de ser ((h*w)*(h*w+1)/2)*.1
edges.sugerido <- edges/(h*w) #promedio de aristas por nodo
cuadrado <- edges.sugerido**.5 # vamos a fijar esta cantidad
cuadrado <- round(cuadrado) +1 
sigJ <- 0.05 #ver paper
sigd <- 10#ver paper
#r2 <- 25.*25. #se pueden quitar hasta el 90% de las aristas segun el paper pero hice mal la cuenta 
r2 <- cuadrado**2
dim(M)
plot(as.cimg(M))
W <- Kernel_float( M, h, w, r2, sigJ, sigd)
print('tick1')
remove(M) #removemos matriz para ahorrar espacio 
gc()
W <- as(W, "sparseMatrix") #casteamos a clase 'sparseMatrix'
gc()
hist(as.matrix(W))
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
hist(as.matrix(Y1)) #esto se puede omitir para matrices grandes
Y2 <- D_medio%*%Z$vectors[,2]
hist(as.matrix(Y2))
remove(D_medio) #ahorramos espacio
gc()
set.seed(0)
kmeans <- kmeans(unlist(Y2), centers = 2, nstart = 50)
print(table(kmeans$cluster))
mascara <- matrix(kmeans$cluster-1, ncol = h, byrow = TRUE)
segmentacion <- mascara
table(segmentacion)
imagen.segmentacion <- as.cimg(round(segmentacion,1))
plot(imagen.segmentacion)#CON MADRE
set.seed(0)
data.a.segmentar <- cbind(Y1,Y2 )
kmeans2 <- kmeans(data.a.segmentar, centers = 3, nstart = 50)
kmeans2$cluster <- kmeans2$cluster-1
kmeans2$cluster <- kmeans2$cluster/2
print(table(kmeans2$cluster))
mascara <- matrix(kmeans2$cluster, ncol = h, byrow = TRUE)
segmentacion <- mascara
imagen.segmentacion <- as.cimg((round(segmentacion,1)))
plot(imagen.segmentacion)#CON MADRE
t1 <- Sys.time() -t1
print(t1)
gc()

