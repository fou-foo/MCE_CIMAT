setwd('/home/fou/Desktop/MCE_CIMAT/Second/CienciaDeDatos/Tarea4/Ejercicio3/frutas_propias/recortes')
path.escritura <- '/home/fou/Desktop/MCE_CIMAT/Second/CienciaDeDatos/Tarea4/Ejercicio3/frutas_propias/segmentacion/'
library(RcppEigen) #libreria para codigo c++
library(RSpectra) #libreria para lanczos
library(imager) #libreria para leer imagenes
library(Matrix)
library(RcppArmadillo)
library(Rcpp) #libreria para codigo c++
sourceCpp('/home/fou/Desktop/MCE_CIMAT/Second/CienciaDeDatos/Tarea4/Ejercicio3/W_float.cpp') #compilamos el programa en C++
t1 <- Sys.time() #medimos tiempo de ejecucion
imagenes <- dir()
i <- imagenes[1]
for(i in imagenes)
{
    name <- paste0(path.escritura, paste0('seg_',i))
    imagen <- load.image(i)
dim(imagen)
plot(imagen) #visualizamos la imagen
#############preprosesamiento  160*120 jala bien, recortamos la imaagen para que quepa en memoria 
gray.imagen <- grayscale(imagen) #cambiamos a escala de grises
dim(gray.imagen) #verificamos tamanio de la imagen
#remove(imagen) #removemos del ambiente la imagen original para ahorra memoria
gc()
plot(as.cimg(gray.imagen))
#estandarizacion escala de grises
M <- as.matrix(gray.imagen)
#M <- (M -min(M))/(max(M)-min(M)) #aumentamos el contraste
(h <- dim(M)[2])
(w <- dim(M)[1] )
hist(M)
kapa <- M
indices <- M <= (median(M)+(var(as.vector(M)))**.5)
mascara <- indices
plot(as.cimg(indices))
#####################
mascara <- indices
gc()
Aplica.Mascara <- function( x){
    M1 <- imagen[,,1 , x]
    M1 <- as.matrix(M1)*mascara
    return((M1))
}
M <- lapply( FUN = Aplica.Mascara, X=1:3)
library(abind)
M <- abind(M , along = 3)
imagen.final <- as.cimg(M)
r <- imagen.final[,,1] == 0
imagen.final[,,1][r] <- 1
g <- imagen.final[,,2] == 0
imagen.final[,,2][g] <- 1
b <- imagen.final[,,3] == 0
imagen.final[,,3][b] <- 1
plot(imagen.final)#CON MADRE
save.image(imagen.final, file = name)
}
