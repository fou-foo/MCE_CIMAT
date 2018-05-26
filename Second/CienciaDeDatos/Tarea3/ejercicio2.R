#generamos la semilla por reproducibilidad, y generamos la muestra
set.seed(0)
n <- 200
library(ggpubr)
#generamos una muestra exponencial con el mismo parametro
LDA.lm <- function(m, f, ...)
{
  set.seed(0)
  #ENTRADAS:
  #m: mitad de tamano de muestra suponemos muestra balanceada
  #f: funcion de la cual se desean generar la muestra
  #... : parametros adicionales de 'f'
  #SALIDA: UN MODELO LINEAL y la muestra generada
  #generamos la muestra dos dimensional
  x1 <- matrix(f(m*2), ncol = 2) #poblacion 1 
  x2 <- matrix(f(m*2), ncol = 2) #poblacion 2
  m.a <- data.frame(rbind(x1,x2))
  m.a$label <- ('orange')
  m.a$label[m.a$X1 <= m.a$X2] <- ('purple') #etiquetamos con la identidad
  ###############se agregan pesos#############
  library(dplyr)
  names(m.a) <- c('X1', 'X2', 'label')
  m.a <- m.a[ order( m.a$label,  m.a$X1, m.a$X2), ]
  cc <- apply( m.a[,1:2], 2, mean ) # centroide de los datos 
  c1 <- m.a%>% filter(label == 'orange')
  cc1 <- apply(c1[, 1:2], 2, mean) #ceontroide poblacion 1
  c2 <- m.a%>% filter(label == 'purple')
  cc2 <- apply(c2[,1:2], 2, mean) #centroide poblacion 2
  S1 <- var(c1[,1:2])
  S2 <- var(c2[,1:2])
  d1 <- mahalanobis(x = as.data.frame(c1[, 1:2]), center = cc1, cov = S1)
  d2 <- mahalanobis(x = as.data.frame(c2[, 1:2]), center = cc2, cov = S2)
  w <- c(d1, d2)
  #asignamos mismas aprioris
  m.mas <- dim(c1)[1]
  m.menos <-dim(c2)[1]
  m.a$Y <- -(m)/m.mas
  m.a$Y[m.a$label == 'purple' ] <- (m)/m.menos
  ##############################################
  modelo.lineal <- lm( Y ~ X1 +X2, data = m.a , weights = 1/w )
  x <- as.matrix(cbind(rep(1, m), m.a[,1:2]))
  m.a$y.hat <- sign(x%*%modelo.lineal$coefficients) 
  return(list(muestra= m.a, model= modelo.lineal, centers = data.frame(rbind(cc1, cc2))))
}
lda.exp  <- LDA.lm(n, f = rexp) #llamamos a la funcion con distros exponenciales
library(ggplot2)
foo <- lda.exp$muestra
sum(diag(table(lda.exp$muestra$Y, lda.exp$muestra$y.hat)))/(2*n)
p1 <- ggplot(lda.exp$muestra, aes(x=X1, y = X2 )) +geom_point(aes(color=label)) +
  scale_color_manual(values =c('orange', 'purple', 'red')) + theme(legend.position = 'none')+
  theme_minimal()  +ggtitle('Mestra generada a partir de Exp(1)') +
  xlab('')+ylab('')+  theme(legend.position = 'none')  +
  geom_point(data = lda.exp$centers, aes(x=X1, y=X2, color = 'red'), size = 3)
lda.runif  <- LDA.lm(m = n, f = runif) #llamamos a la funcion con distros uniformes
p2 <-ggplot(lda.runif$muestra, aes(x=X1, y = X2 )) +geom_point(aes(color=label)) +
  scale_color_manual(values =c('orange', 'purple', 'red')) +
  theme_minimal() + ggtitle('Mestra generada a partir de Unif(0,1)') +
  xlab('')+ylab('')+  theme(legend.position = 'none')  +
  geom_point(data = lda.runif$centers, aes(x=X1, y=X2, color = 'red'), size = 3)
ggarrange(p1,p2, nrow = 2 )
sum(diag(table(lda.runif$muestra$Y, lda.runif$muestra$y.hat)))/(2*n)
