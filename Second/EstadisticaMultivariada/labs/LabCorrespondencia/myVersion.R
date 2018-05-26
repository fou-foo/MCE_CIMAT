# Practica 5. Aplicacion de an?lisis de correspondencia en R
library("ca")
######################################################################################
##analisis de correspondencia

#vamos a realizar un analisis de correspondencia para representar las filas y las columnas en un espacio-euclidiano de baja dimension

#primer tabla de contingencia
#la tabla presenta la clasificacion de 5387 escolares escoceses por el color de 
#sus ojos que tiene cuatro categorias: claros,azules,casta?os y oscuros (I=4)
# y por el color de su cabello que tiene 5 categorias:rubio, pelirrojo,
#castano,oscuro y negro (J=5).
#La tabla tiene interes historico porque fue usada por Fisher en 1940 para
#ilustrar otro metodo de analisis tablas de contingencia.


# Se introduce la tabla
fisher_tab <- matrix( c(688, 116, 584, 188, 4, 326,38, 241, 110, 3, 343, 84, 909,
                        412, 26, 98, 48, 403, 681, 85), ncol=5, byrow=TRUE)
fisher_tab
n <- sum(fisher_tab)

#se calcula la chi cuadrada para probar la independencia de los 
#renglones y columnas de la tabla de contingencia y la inercia 
#total (medida de la variabilidad total de los datos en la tabla)

chiR <- chisq.test(fisher_tab)
inetot <- chiR$statistic/n

# Se calcula la matriz F de frecuencias relativas o matriz de correspondencia
nrow <- nrow(fisher_tab)
ncol <- ncol(fisher_tab)
F_fisher <- (fisher_tab)/n


#se calculan las frecuencias relativas de las filas y de las columnas de F o tambien llamadas masas (las ri y las cj)
r <- (apply(fisher_tab,1,sum))/n
c <- apply(fisher_tab,2,sum)/n

# los vectores obtenidos se  colocan en las matrices diagonales 
# correspondientes (D_r y D_c)
Dr <- diag(r)
Dc <- diag(c)

# se calcula la matriz de perfiles por fila (R)
table.pro <-   diag(1/r)%*%F_fisher  #matriz R de renglones

# se calcula la matriz de perfiles por columna (Rc)
table.pcol <- (F_fisher)%*% diag(1/c)  #matriz R de columnas

#se calcula la chi cuadrada para probar la independencia de los 
#renglones y columnas de la tabla de contingencia y la 
#inercia total (medida de la variabilidad total de los datos en la tabla)

t.rowsum <- n*r
t.colsum <- n*c
tabla_exp <- t.rowsum%o%t.colsum/n #matriz centrada
chi2 <- sum( ( fisher_tab - tabla_exp)^2/tabla_exp)
chi2
inetot1 <- chi2/n
inetot1

#se calcula la matriz Z
Z <- (sqrt( solve(Dr) ) )%*%F_fisher%*%( sqrt( solve(Dc) ) )
#se obtienen los vectores propios y valores propios mediante la descomposicion en valores singulares de Z
dvalsing <- svd(Z)


#Se obtienen las representaciones de las filas y columnas en un espacio 
#de dos dimensiones considerando los vectores propios ai y bi obtenidos de la svd
#OJO:se consideran solo los vectores propios asociados a valores propios 
#distintos de 1, en este caso se consideran las columnas 2 y 3 de 
#las matrices U y V de la svd.
ind <- c(2,3)
#Representacion de las filas en dos dimensiones
Cr <- ( sqrt( solve(Dr) ) )%*%Z%*%dvalsing$v
Cr <- Cr[, ind]

#Representacion de las columnas en dos dimensiones
Cc <- ( sqrt( solve(Dc) ) )%*%t(Z)%*%dvalsing$u
Cc <- Cc[, ind]

#se calcula la proporcion de la inercia explicada por las dos  dimensiones (asociadas a valores propios distintos de cero)
vp <- (dvalsing$d)^2
vp_dist1 <- vp[-1] #quitamos la solucion trivial
vp_dist1
var_expl <- sum(vp_dist1[1:2])/sum(vp_dist1) #inercia de la tabla

# Se obtiene la representacion conjunta de los renglones y columnas en el espacio de dos dimensiones
par(pty="s")
plot(Cr[,1],Cr[,2], xlim = c(-.55, 1.2), ylim=c(-.8,.8),
     xlab="Coordenada 1",ylab="Coordenada 2",lwd=2)
points(Cc[,1], Cc[, 2], col=2)
text(Cr[,1],Cr[,2], labels=c("o claros","o azules","o castanos","o oscuros"),col=1,lwd=2)
text(Cc[,1], Cc[, 2], labels=c("c rubio","c pelirrojo","c castanos","c oscuro","c negro"),col=2,lwd=4)

abline(h=0,lty=2)
abline(v=0,lty=2)


#Se calculan las distancias chi cuadrada de cada renglon de R a su centroide: (Ra-c)'Dc^-1(Ra-c)
dist.chicua.ren <- sqrt(apply( (t(table.pro)-c)^2/c,2,sum))
dist.chicua.ren
#se calcula la inercia de cada renglon, que se obtiene simplemente multiplicando las 
#distancia chicuadrada a su centroide (al cuadrado) por su masa correspondiente
inercia.ren <- ((dist.chicua.ren)^2) *r
inercia.ren

#Se calculan las distancias chi cuadrada de cada columna de R a su centroide: (Ra-c)'Dr^-1(Ra-c)
dist.chicua.col<-sqrt(apply(((table.pcol)-r)^2/r,2,sum))
dist.chicua.col
#se calcula la inercia de cada columna, que se obtiene simplemente 
#multiplicando las distancia chicuadrada a su centroide (al cuadrado) por su masa correspondiente
inercia.col <- ((dist.chicua.col)^2) *c
inercia.col


#ahora vamos usar la funcion "ca" de para realizar analisis de correspondencia sobre la misma 
#tabla de contingencia
#nota: esta funcion trabaja directamente con  la matriz F-Fesp, 
#la cual ya no tiene un valor propio de 1.

corres <- ca(F_fisher, nd = 2, suprow = NA, supcol = NA,
             subsetrow = NA, subsetcol = NA)
plot(corres)
Cr1 <- corres$rowcoord
Cc1 <- corres$colcoord

# Se obtiene la representacion conjunta de los renglones y columnas en el espacio de dos dimensiones

par(pty="s")

plot(Cr1[,1], Cr1[,2], xlim=range(Cr1[,1],Cc1[,1]),ylim=range(Cr1[,1],Cc1[,1]),
     xlab="Coordenada 1",ylab="Coordenada 2",lwd=2)
points(Cc1[, 1],Cc1[, 2], col=2)
text(Cr1,labels=c("o claros","o azules","o castanos","o oscuros"),col=1,lwd=2)
text(Cc1,labels=c("c rubio","c pelirrojo","c castanos","c oscuro","c negro"),col=2,lwd=4)

abline(h=0,lty=2)
abline(v=0,lty=2)

#conclusion: el gr?fico muestra  una relaci?n clara entre ambas variables, 
#es decir, existe dependencia entre las dos variables. La dimension principal gradua la tonalidad
#de claro a oscuro y la segunda separa los casta?os de los demas

################################################
svd <- svd( (solve(Dr)**.5) %*%  (F_fisher - r%*%t(c)) %*% (solve(Dc)**.5))
svd$d
ine <- sum(svd$d[1:2]**2)/sum(svd$d**2)
filas.proyectadas <- solve(Dr)%*%(  (solve(Dr))**.5 %*%svd$u  )%*%diag(svd$d)
columnas.proyectadas <- solve(Dc)%*%(  (solve(Dc))**.5 %*%svd$v  )%*%diag(svd$d)
plot(filas.proyectadas[, 1], filas.proyectadas[,2], pch = 20, col = 'blue', xlim = c())
points(columnas.proyectadas[, 1], columnas.proyectadas[,2], pch = 20, col = 'red')
#################
plot(corres)
a <- rbind(filas.proyectadas, columnas.proyectadas)
a <- scale(a)
plot(a[5:9, 1:2], col = 'blue', pch = 20)
points(a[1:4,1:2], col = 'red', pch =20)
