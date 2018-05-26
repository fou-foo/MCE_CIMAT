# Ejercicio 1:
#Escribe una función que tome un entero positivo j y produce un vector de la forma
#1, 1, 2, 1, 2, 3, 1,2, 3, 4, 1, 2, 3, 4, 5,...,1, 2, ...,j

f1 <- function(n)
{
  # n (int): numero j descrito en el ejercicicio 
  # SALIDA: vector de secuencias concadenadas 
  s <- lapply(1:n, FUN = function(i){1:i} ) 
  x <- unlist(s)
  return(x)
}
#casos test: f1(1), f1(3), f1(5) 

#Ejercicio 2:
#Si X se comporta como una normal estándar X^2 se distribuye como una chi cuadrada
#con un grado de libertad. La función de distribución acumulada en R se puede
#encontrar mediante la función pchisq(y,d) donde y es la ordenada y d son los 
#grados de libertad. Usa esta función para encontrar la probabilidad de una variable
#chi cuadrada con un grado libertad tome valores entre 0.5 y 1.2.
f2 <- function(a,b, df)
{
  pchisq(b, df = df) - pchisq(a, df = df)
}
# caso test f2(0.5, 1.2, 1)

#Ejercicio 3:
#La media absoluta de un conjunto de valores x_1, \dots x_n es el promedio de las
#diferencias absolutas entre las observaciones y la media, matemáticamente
#sd_abs = n^{-1}\sum_{i=1}^n|x_i-\bar{x}|
#Escribe una función que calcule la desviación absoluta media.
sd_abs <- function(x)
{
  media <- mean(x)
  return(sum(abs( x- media) )/length(x) ) #la funcion ya esta vectorizada
}
#Una manera de probar las propiedades de un estimador estadístico es introducir outliers grandes
#y ver como se comporta el resultado. Haz un pequeño experimento bajo la óptica 
#anterior introduciendo outliers cada más grandes y compara la desviación estándar
#con la desviación absoluta media. ¿Cuál es más robusto?
n <- 100
m <- 10
m.a <- mapply(FUN = rnorm,rep(n,m) ) #genero una muestra aleatoria de tamaño m de un vector n dimensional
m.a <- as.data.frame(t(m.a))
            #evaluo la funcion desviacion estandar absoluta sobre la muestra con outliers
            #guardo los resultados antes de introducir outliers
sd_abs.sin.out <- apply(m.a,1, sd_abs ) 
sd.sin.out <- apply(m.a,1, sd )
comparacion <- data.frame(sd_abs.sin.out = sd_abs.sin.out, sd.sin.out= sd.sin.out)
      #introduzco outliers en la muestra aleatoria
a <- 10 #que tan grande en comparacion del rangeo
m.a$outlier <- apply(m.a,1, function(x,a){max(x)*a}, a ) #outlier por arriba
m.a$outlier2 <- apply(m.a,1, function(x,a){min(x)*a}, a ) #outlier por abajo
          #evaluo la funcion desviacion estandar absoluta sobre la muestra con outliers
comparacion$sd_abs.con.outs <- apply(m.a,1, sd_abs ) 
comparacion$sd.con.outs <- apply(m.a,1, sd )
round(comparacion,3)

#4:
#Considera la función f(x) = 1/(2+sin(5pix))
# f(x) = \frac{1}{2+sin(5pix)}
f4 <- function(x)
{
  return( 1/(2+sin(5*pi*x)) )
}
#para valores 0\leq x \leq 1
#grafica la funcion 
library(ggplot2)
library(latex2exp)
a <- 0
b <- 1
g1 <- ggplot(data.frame(x=a, y= b), aes(x,y)) +
  stat_function(fun = f4, xlim = c(a,b), color='purple') +
  theme_minimal() + ggtitle(TeX('$f(x) = \\frac{1}{2+sin(5pix)}$')) + xlab('')+ ylab('')
plot(g1)