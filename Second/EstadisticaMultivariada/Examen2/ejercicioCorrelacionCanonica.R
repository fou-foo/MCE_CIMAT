p <- 2
q <- 2
library(expm)#packages con funciones de matrices
(S11 <- matrix( c(1, .505, .505, 1), byrow = TRUE, ncol = p  ))
(S12 <- matrix( c(.569, .602, .422, .467), byrow = TRUE, ncol = q  ))
(S22 <- matrix( c(1, .926, .926, 1 ), byrow = TRUE, ncol = q  ))
#me voy por la segura sacar los valores propios más grandes 
W <- ( sqrtm(solve(S22)) %*% t(S12) %*% solve(S11) %*% S12%*%sqrtm(solve(S22))  )
prop <- eigen(W)
valores <- prop$values
correlaciones.canonicas <- valores**.5
i <- 1
var_expl <- sum(prop$values[i])/sum(prop$values) #cuantos se requeirem -> i 
var_expl #var acumulada deL PRIMER PAR canonico
(f <- prop$vectors) #los f_i
(V <- ( sqrtm(solve(S22))%*%f ))#coeficientes del segundo grupo POR COLUMNAS
#################checar a mano tener solo combinaciones lineales '+'
###solo por didactica (y porque es facil) obtengamos los e_i de la manera larga
W <- ( sqrtm(solve(S11)) %*% (S12) %*% solve(S22) %*% t(S12)%*%sqrtm(solve(S11))  )
prop <- eigen(W)
e <- prop$vectors
(U <- sqrtm(solve(S11)) %*%e ) #por columnas 
#################checar a mano tener solo combinaciones lineales '+'
U <- -U
########################################
###### Correlaciones del los pares canonicos con el primer conjunto de variables 
######NOTA: NOTAR EL TRASPUESTO => los resultados se leen por reglon 
t(U)%*% S11
### correlaciones de los pares canonicos con el segundo conjunto 
t(V)%*% S22
########### correlaciones del primer componente de los pares con el perimer grupo 
t(U)%*%S12
#########correlciones de la segunda componente canonica con el segundo grupo de variables
t(V)%*%t(S12)
####################aPROXIMACIONES usando ambos pares
sum(correlaciones.canonicas) #no podemos esperar mucho 
A <- t(U) #para unificar notacion 
A <- solve(A)
B <- t(V)
B <- solve(B)
i <- 1:2 #pares canonicos a usar
(S11.aprox <-  S11 - (A[,i] %*% t(A[, i]))) #con la primera)
(S22.aprox <-  S22 - (B[,i] %*% t(B[, i]))) #con la primera
(S12.aprox <-  S12 - (correlaciones.canonicas[i]* (A[,i] %*% t(B[, i])))) #con la primera, ATENCION CON EL INDICE DE correlacion del par acnonico
propo.S11 <- sum(diag(S11 -S11.aprox))/sum(diag(S11))
propo.S22 <- sum(diag(S11 -S22.aprox))/sum(diag(S22))
n <- 45 ##supongamos un tamanio de muestra
estadistico <- -n*log(prod(1-correlaciones.canonicas[i]**2))
valor.critico <- pchisq(1-.05, p*q)
ifelse(estadistico > valor.critico, 'Rechaza H0', 'No se rechaza H0')
