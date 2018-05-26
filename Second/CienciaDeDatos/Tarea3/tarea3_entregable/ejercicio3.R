m.genera <- function(m, p, f, a, variables)
{
  #set.seed(0)
  #ENTRADAS:
  #m: mitad tamano de muestra suponemos balanceada
  #f: funcion de la cual se desean generar la muestra
  #p: dimension de la muestra
  #a : vector parametros adicionales de 'f'
  #SALIDA: la muestra generada y centros por grupo 
  #generamos la muestra p dimensional
  x1 <- f(m*p, a[1], a[2])
  x1 <- as.data.frame(matrix(x1, ncol = p)) #poblacion 1 
  x2 <- f(m*p, a[3], a[4])
  x2 <- as.data.frame(matrix(x2, ncol = p)) #poblacion 2
  x1$label <- ('orange') #de momento solo consideramos dos etiquetas, para mejorar la visualizacion de resultados
  x2$label <- ('purple')
  x1$Y <- 1   #relizamos el encoding 
  x2$Y <- -1
  m.a <- data.frame(rbind(x1,x2))
  names(m.a) <- variables #homologamos los nombres de las variables
  ###############se agregan pesos#############
  library(dplyr)
  m.a <- m.a[ order( m.a$label,  m.a$X1, m.a$X2), ]
  c1 <- m.a%>% filter(label == 'orange')
  cc1 <- apply(c1[, variables[1:(length(variables)-2)] ], 2, mean) #ceontroide poblacion 1
  c2 <- m.a%>% filter(label == 'purple')
  cc2 <- apply(c2[, 1:(length(variables)-2)], 2, mean) #centroide poblacion 2
  ##############################################
  return(list(muestra= m.a, centers = data.frame(rbind(cc1, cc2))))
}
################## Perceptron inline
Perceptron <- function(data, Y, p, delta = 1e-6, eta = .01,   max.iter = 200 )
{
  #ENTRADAS 
  #data:  conjunto de entrenamiento 
  #escalamos los datos por sugerencia del libro de Hestie T.
  #Y: labels
  #SALIDAS
  # el vector de pesos estimado \beta si el algoritmo convergio sino regresa 
  data <- as.data.frame(scale(data[,  1:(length(variables)-2)]))
  data$Y <- Y
  w <- runif(p+1,-0.7, 0.7) #recomendacion del Hastie
  w.old <- rep(5, p+1) #vector para guardar iteraciones anteriores
  for (i in 1:max.iter)
  {
    for(j in 1:dim(data)[1])
    {
      x.i <- as.numeric(data[i, 1:(length(variables)-2) ]) #obtenemos la j-esima observacion
      y.i <- as.numeric(data[i, c('Y')])                   # el label j-esimo
      carga <- w%*%( c(1,x.i) )#uso la funcion signo incluimos intercepto         #calculamos el signo 
      h <- ifelse( carga >=0, 1, -1)#uso la funcion signo  # implementación del Laplaciano de la funcion signo que no es continua
      if( h!= y.i ) #en caso de clasificar mal 
      {
        w <- w + (eta*y.i)*x.i #actualizamos coeficientes
      }
    }
    if( ( sum(w.old-w)**2 ) < delta) #si la solucion no cambia 'mucho' detenmos la ejecucion
    {
      return(list(parametros = w ))
    }
    w.old <- w
    if(i == max.iter) #si no convergio en un numero maximo de iteraciones 
    {
      return(0) #regresamos uno 
      break
    }
  }
  return(list(parametros = w )) #rregresamos el vector de parametros estimados
}
#checar complejidad en el hastie
###########################################################
p <- 2
m <- 1000
a <- c(0,2,10,5)
f <- rnorm
variables <- c(paste0(rep('X',p), 1:p), 'label', 'Y')
m.a <- m.genera(m,p,f,a = c(0,2,10,3), variables = c(paste0(rep('X',p), 1:p), 'label', 'Y'))
data <- m.a$muestra
################usamos la implementación 
w <- Perceptron(data, Y = data$Y, p=2)
data.test <- scale(data[,variables[1:2]])
data.test <- cbind( rep(1, dim(data.test)[1]), data.test)
y.hat <- as.matrix(data.test) %*% (w$parametros)
y.hat <- sign(y.hat)
data$y.hat <- y.hat
table(data$Y, data$y.hat)
p1 <- ggplot(data, aes(x=X1, y = X2, alpha=0.5 )) +geom_point(aes(color=label)) +
  scale_color_manual(values =c('orange', 'purple')) +
  theme_minimal() + #stat_function(fun = function(x){ x*(-w$parametros[2]/w$parametros[3]) - 
      #w$parametros[1]/w$parametros[3] },  col ='green4' ) +
  ggtitle('Clasificación inicial') +
  xlab('')+ylab('')+  theme(legend.position = 'none')  
p1
p2 <- ggplot(data, aes(x=X1, y = X2, alpha = 0.5 )) +
  geom_point(aes(color=factor(y.hat))) +
  scale_color_manual(values =c( 'purple', 'orange')) +
  theme_minimal() + # stat_function(fun = function(x){ x*(-w$parametros[2]/w$parametros[3]) - 
      #w$parametros[1]/w$parametros[3] },  col ='green4' ) +
  ggtitle('Clasificación con perceptron') +
  xlab('')+ylab('')+  theme(legend.position = 'none')  
p2
ggarrange(p1,p2, ncol = 2 ) ##ilustramos la diferencia
################Probaremos la implementacion con el famosizimo conjunto de datos PIMA INDIANS
setwd('C:\\Users\\fou-f\\Desktop\\MCE\\Second\\CienciaDeDatos\\Tarea3')
dir()
pima.train <- read.table('pima.tr', header = TRUE)
pima.test <- read.table('pima.te', header = TRUE)
summary(pima.test) #ya no hay ceros !!!!!!!!!!!!!!!!!!!! :D :D
summary(pima.train)
names(pima.test)
pima.train$Res <- 'Positivo'
pima.train$Res[ pima.train$Res!= 0 ]  <- 'Negativo'
pima.test$Res <- 'Positivo'
pima.test$Res[ pima.test$Res!= 0 ]  <- 'Negativo'
variables <- names(pima.test)
train <- as.data.frame(scale(pima.train[,variables[1:7]] ))
test <- as.data.frame(scale(pima.test[,variables[1:7]] ))
train$Y <- pima.train$type
test$Y <- pima.test$type
###Entrenamos el perceptron 
w <- Perceptron(train, Y = train$Y, p = 7) #requiero que el data set tenga otras dos columnas ademas de los features
# evaluamos y medimos errores en train 
train <- cbind(rep(1, dim(train)[1]), train)
test <- cbind(rep(1, dim(test)[1]), test)
y.hat <- as.matrix(train[, 1:8]) %*% (w$parametros)
y.hat <- sign(y.hat)
train$y.hat <- y.hat
sum(diag(table(pima.train$type , train$y.hat)))/ dim(train)[1]
############## evaluamos error en test 
y.hat <- as.matrix(test[, 1:8]) %*% (w$parametros)
y.hat <- sign(y.hat)
test$y.hat <- y.hat
sum(diag(table(pima.test$type , test$y.hat)))/dim(test)[1]
####graficamos con PCA
puntos <- princomp(pima.test[, 1:7], cor = TRUE )
puntos <- as.data.frame(puntos$scores[,1:2])
puntos$Type <- pima.test$type
p1 <- ggplot(puntos, aes(x=Comp.1, y = Comp.2, alpha=0.5 )) +
  geom_point(aes(color=Type, size = 1)) +
  scale_color_manual(values =c('orange', 'purple')) +
  theme_minimal() + #stat_function(fun = function(x){ x*(-w$parametros[2]/w$parametros[3]) - 
  #w$parametros[1]/w$parametros[3] },  col ='green4' ) +
  ggtitle('Clasificación inicial') +
  xlab('PC1')+ylab('PC2')+  theme(legend.position = 'none')  
p1
p2 <- ggplot(puntos, aes(x=Comp.1, y = Comp.2, alpha = 0.5 )) +
  geom_point(aes(color=factor(y.hat),  size = 1)) +
  scale_color_manual(values =c( 'orange', 'purple')) +
  theme_minimal() + # stat_function(fun = function(x){ x*(-w$parametros[2]/w$parametros[3]) - 
  #w$parametros[1]/w$parametros[3] },  col ='green4' ) +
  ggtitle('Clasificación con perceptron') +
  xlab('PC1')+ylab('PC2')+  theme(legend.position = 'none')  
p2
ggarrange(p1,p2, ncol = 2 ) ##ilustramos la diferencia
