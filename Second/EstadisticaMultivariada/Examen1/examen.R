#########################################Ejercicio 3 
###capturamos los datos 
z <- matrix(c(-2, -1, 0, 1,2), ncol=1)
y1 <- matrix(c(5,3,4,2,1), ncol=1)
y2 <- matrix(c(-3,-1,-1,2,3), ncol=1)
##############
#inciso a) determinar los coeficientes por minimos cuadrados del modelo de 
# lineal bivariada Y_j1 = b_{01}+b_{11}z_{j1}+\epsilon_{j1}
# y Y_j2 = b_{02}+b_{12}z_{j1}+\epsilon_{j2}
# como comentario puedo hacer los dos modelos de una vez por la construcción de la 
# regresión multivariada
##############
#construcción de matriz de diseño 
Z <- cbind( matrix(rep(1,5), ncol=1), z)
Y <- cbind(y1,y2)
#estimamos usando minimos cuadrados usando el famosizimo resultado visto en clase
(B <- solve(t(Z)%*%Z)%*%t(Z)%*%Y)
#las entradas del primera columna de B son los coeficientes b_{01} y b_{11}
#las entradas del segunda columna de B son los coeficientes b_{02} y b_{12}
###############ejercicio 3.b
#calcular los valores ajustados y los residuales del modelo anterior 
Y_hat<- Z%*%B#matriz de estimaciones
epsilon <- Y- Y_hat#matriz de residuales 
#notemos que los residuales tienen media aprox cero 
apply(epsilon, 2, mean) 
(t(Y)%*%Y) - ( (t(Y_hat)%*%Y_hat)+ t(epsilon)%*%epsilon) # verfificamos que Y'Y = \hat{Y}'\hat{Y} + \hat{epsilon}'\hat{epsilon}
round((t(Y)%*%Y) - ( (t(Y_hat)%*%Y_hat)+ t(epsilon)%*%epsilon),2) # verfificamos que Y'Y = \hat{Y}'\hat{Y} + \hat{epsilon}'\hat{epsilon}
##############Ejercicio 3.c 
#calcula con los resultados anteriores un intervalo de congianza de 95%
# para la respuesta media de E(Y_{01})= b_{01}+b_{11}z_{01}  con z_{01} <- .5
z.01 <- matrix( c(1, .5), ncol = 1) # el uno es por el intercepto
# sabemos que estos intervalos estan dados por z_{01}B_1+-\sqrt((m(n-r-1)/(n-r-m)))\sqrt(z_0((Z'Z)^{-1})z_0(n/(n-r-1)\sigma_{11}))
alpha <- .05 #pues usamos intervalos T2 de hotelling y la prueba es de dos colas 
m <- dim(Y)[2] # numero de variables a predecir
n <- dim(Z)[1] # numero de observaciones
r <- dim(Z)[2]-1 #numero de variables exogenas
percentil <- qf(1-alpha, df1 =m, df2 = n-r-m   )
Sigma <- (t(epsilon)%*%epsilon)/(n-r-1) #estimador insesgado de \Sigma
sigma_11 <- Sigma[1,1] 
error1 <- sqrt((m*(n-r-1)/(n-r-m))*percentil) * 
  sqrt( (t(z.01)%*%solve(t(Z)%*%Z)%*%z.01 ) *(n/(n-r-1))*sigma_11  )
## entonce un intervalo de confianza  al 95% para la respuesta media de z_{01} esta dado por 
c((t(z.01)%*%B[,1]) - error1, (t(z.01)%*%B[,1])+error1)
#############################################################
############Ejercicio 4
######################
#Los datos que se nos proporcionaron corresponden a 
#x_1 = longitud de la cola en mm (primer columna )
#x_2 = logitud del ala en mm (segunda columna) para n = 45 aves hembra
# de una especie de agula 
##########inciso a)
#Construye y muestra una region de confianza (elipse del 95%) para la media 

setwd('C:\\Users\\fou-f\\Desktop\\MCE\\Second\\EstadisticaMultivariada\\Examen1')##me cambio de directorio 
aves <- read.table('bird_data.dat', header=FALSE)
colnames(aves) <- c('x1', 'x2')
p <- dim(aves)[2]
n <-dim(aves)[1]
alpha <- .05
S <- cov(aves)
media <- matrix(apply(aves, 2, mean), ncol=1)
xlim <- c(200,180)
ylim <- c(270, 290)
#uso una funcion que construi en una tarea
dibuja.elipse <- function(alpha, n, p, media, S, xlim, ylim)
{
  #alpha (numeric): nivel de significancia
  #n (numeric): numero de observaciones
  #p (numeric): numero de variables 
  #media (vector): vector-columna de medias 
  #S (matrix): matriz de covarianzas
  ejes <- eigen(S)
  eje_x <- ejes$vectors[,1]
  eje_y <- ejes$vectors[,2]
  nivel <- qf(1-alpha, df1 = p, df2 = n-p ) #cuantil de la distribucion f con confianza 1-alpha
  c <- ((p*(n-1))/(n*(n-p)) )*nivel #cuantil de la T de Hotelling
  longitud_x <- ejes$values[1]**.5*( c)**.5 #formulas vistas en clase
  longitud_y <- ejes$values[2]**.5*( c)**.5 #formulas vistas en clase
  #se dibujan los ejes
  g <- 100 #numero de puntos a dibujar, primero los genero centrados en el origen y 
  #luego roto y translado
  x <- seq(-longitud_x, longitud_x, length=g)
  y <- seq(-longitud_y, longitud_y, length=g)
  ejes.puntos <- matrix(c(x, rep(0,g )), byrow = TRUE, nrow = 2) 
  ejes.puntos <- (ejes$vectors)%*%ejes.puntos + matrix(rep(media, g), nrow = 2)
  plot(ejes.puntos[1,], ejes.puntos[2,], type='l', col='orange', 
       main='Elipse de 95% confianza para la media', 
       xlab=colnames(S)[1], ylab = colnames(S)[2], xlim=xlim, ylim=ylim)
  ejes.puntos <- matrix(c(rep(0,g ), y), byrow = TRUE, nrow = 2) 
  ejes.puntos <- (ejes$vectors)%*%ejes.puntos + matrix(rep(media, g), nrow = 2)
  points(ejes.puntos[1,], ejes.puntos[2,], type='l', col='orange')
  #se genera el perimetro, primero centrado y luego se rota y translada
  y <- ((c-x**2/ejes$values[1])*ejes$values[2])**.5 #funcion inversa de elipse con 
  #centro en elorigen 
  elipse.puntos <- matrix(c(x,y), ncol = g, byrow = TRUE)
  elipse.puntos <- ejes$vectors%*%(elipse.puntos) + matrix(rep(media, g), nrow = 2)
  points(elipse.puntos[1,], elipse.puntos[2,], col='purple', type='l')
  y <- -((c-x**2/ejes$values[1])*ejes$values[2])**.5 
  elipse.puntos <- matrix(c(x,y), ncol = g, byrow = TRUE)
  elipse.puntos <- ejes$vectors%*%(elipse.puntos) + matrix(rep(media, g), nrow = 2)
  points(elipse.puntos[1,], elipse.puntos[2,], col='purple', type='l')
}
dibuja.elipse(S=S , alpha=alpha, n=n ,p=p, media=media,
              xlim, ylim)
#dibujo parte de la muestra
points(aves$x1, aves$x2, col='gray', pch = 20)
#supon que se sabe que la media conjunta es (190, 275) para el macho 
# son datos plausibles para las hembras
points(190,275, col ='red', pch = 20) #dibujo la media de los machos 
#como la media de los machos esta en la region de confianza del 95% podemos
#decir que las mediciones de los machos (media) son una plausible media de las 
#hembras tambien
################ejercicio b
#construye los intervalos T2 simultaneos de las medias de las hembras
#uso una funcion que implemente en una tarea
T2.intervalos <- function(S,media, alpha, a, n, p)
{
  #alpha (numeric): nivel de signiificancia
  #n (numeric): numero de observaciones
  #p (numeric): numero de variables 
  #media (vector): vector-columna de medias 
  #S (matrix): matriz de covarianzas
  nivel <- sqrt(((p*(n-1)/(n*(n-p))))*(qf(1-alpha, df1 = p, df2=n-p)))
  errores <- nivel*sqrt(t(a)%*%S%*%a)
  #errores <- nivel*sqrt(ss) #es importante el orden 
  inf.intervalos <- media - errores
  sup.intervalos <- media + errores
  intervalos <- as.data.frame(cbind(inf.intervalos,media, sup.intervalos))
  colnames(intervalos) <- paste0(c('lim.inferior.T2intervalo.media.al',
                                   'media.muestral',
                                   'lim.superior.T2intervalo.media.al'),
                                 c(as.character(1-alpha), '',as.character(1-alpha)  )) 
  #row.names(intervalos) <- colnames(data)
  return(intervalos)
}
a <- matrix(c(1,0), ncol=1 )
media1 <- t(a)%*%media
(intervalos.t2.x1 <- T2.intervalos(S=S, media= media1, alpha=0.05,
                                  a = a, n=n, p=p))
#los intervalos T2 para la media de x1 es
#reporto el limite inferior, la media, poblacional, y el limite superior del intervalo
a <- matrix(c(0,1), ncol=1 )
media2 <- t(a)%*%media
#los intervalos T2 para la media de x2 es
#reporto el limite inferior, la media, poblacional, y el limite superior del intervalo
(intervalos.t2.x2 <- T2.intervalos(S=S, media= media2, alpha=0.05,
                                   a = a, n=n, p=p))
intervalosT2 <- rbind(intervalos.t2.x1,intervalos.t2.x2)
#calculo los intervalos de bonferroni  con una funcion que ya tenia 
Bonferroni.intervalos <- function(S,media, alpha, n, p, a)
{
  #alpha (numeric): nivel de significancia conjunto
  #n (numeric): numero de observaciones
  #p (numeric): numero de variables OJO DEBE DE SER LA DIMENSION DEL VECTOR ALEATORIO NORMAL MULTIVARIADO
  #media (vector): vector-columna de medias ORIGINALES MUESTRALES 
  #S (matrix): matriz de covarianzas ORIGINAL MUEsTRAL
  #a (vector): vector columna que expresa combinacion lineal
  media <- t(media)%*%a
  ss <- t(a)%*%S%*%a
  nivel <- qt(1-(alpha/(2*p)), df = n-1, lower.tail =TRUE)
  errores <- nivel*sqrt(ss/n)
  inf.intervalos <- media - rep(errores, length(media))
  sup.intervalos <- media + rep(errores, length(media))
  alpha_ <- alpha/(2*p)
  a <- round(1-alpha_,3)
  a <- as.character(a)
  intervalos <- as.data.frame(cbind(inf.intervalos,media, sup.intervalos))
  colnames(intervalos) <- paste0(c('lim.inferior.Bonferri.media.al',
                                   'media.muestral',
                                   'lim.superior.Bonferri.media.al'),
                                 c(a,'',a  )) 
  return(intervalos)
}
a <- as.matrix(c(1,0), ncol=1)
intervalos.bonferri.x1 <- Bonferroni.intervalos(S,media,
                              alpha=alpha,n=n,p=p, a=a  )
a <- as.matrix(c(0,1), ncol=1)
intervalos.bonferri.x2 <- Bonferroni.intervalos(S,media, alpha=alpha,
                                                n=n,p=p, a=a  )
intervalos.Bonferroni <- rbind(intervalos.bonferri.x1, intervalos.bonferri.x2)
###imprimimos los intervalos para compararlos 
(intervalos <- cbind(intervalosT2, intervalos.Bonferroni))
intervalos$lim.superior.Bonferri.media.al0.988-intervalos$lim.inferior.Bonferri.media.al0.988
intervalos$lim.superior.T2intervalo.media.al0.95-intervalos$lim.inferior.T2intervalo.media.al0.95
#de las dos lineas anteriores vemos la longitud de los intervalos 
# en donde es claso que como era de esperarse los intervalos de bonferroni 
# son de longitud menor y de paso de mayor confianza sin embargo no son simultaneos