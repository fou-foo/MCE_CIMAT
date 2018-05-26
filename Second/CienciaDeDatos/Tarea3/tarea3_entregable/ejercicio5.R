xx <- Sys.time()
directorio <-'C:\\Users\\fou-f\\Desktop\\MCE\\Second\\CienciaDeDatos\\tarea3'
setwd(directorio)
dir()
library(tm)
library(Rcpp)
library(RcppEigen)
sourceCpp('KernelLDA.cpp')
#primero veamos los mails de test
#para contrastarlas con las palabras del training
#realizar el contraste entre ambos grupos quitando las palabras que aparecen en 
#ambas bolsas de palabras
preprocesamiento <- function(x)
{
  #esta funcion tiene como finalidad extraer todas las palabras
  #regresa un dataset con las frecuencias acumuladas por palabras en el corpus
  # x (path): path en donde se encuentran lojados en disco duro los documentos
  encoding <- function(x)
  {
    iconv(x, "latin1", "ASCII")
  }
  f <- content_transformer(function(x, pattern) gsub(pattern, "A", x))
  library(tm)
  library(tm.plugin.mail)
  ####################
  negativos <- corp <- Corpus(DirSource(x,recursive=TRUE),
                              readerControl=list(language="en_US",reader=readMail))
  #preprosamiento
  negativos <- tm_map(negativos, content_transformer(encoding)) #remuevo todo lo que no sea ASCII
  negativos <- tm_map(negativos,stripWhitespace)
  negativos <- tm_map(negativos,removeNumbers) #tal vez los numero sean inportantes, resulto que no 
  negativos <- tm_map(negativos,content_transformer(tolower))
  negativos <- tm_map(negativos,removePunctuation) 
  negativos <- tm_map(negativos,removeWords,stopwords("english"))
  negativos <- tm_map(negativos,stemDocument)
  matriz.neg <- TermDocumentMatrix(negativos)#,control=list(minDocFreq=100))
  #trabajaremos con todos al inicio
  m.neg <- as.matrix(matriz.neg)
  v <- sort(rowSums(m.neg),decreasing=TRUE)
  d.neg <- data.frame(word = names(v),freq=v)
  d.neg <- d.neg[order(d.neg$word),]
  return(list(frecuencias= d.neg, mtd=m.neg ))
}
#######################
test <- preprocesamiento("email_test") #preprocesamos los documentos marcados como test 
train <- preprocesamiento("email_train" ) #preproseamiento de los documentos marcados como train
# identificamos las palabras comunes a ambos conjuntos  
comunes <- merge(test[['frecuencias']],train[['frecuencias']]  , by ='word')
#checamos la correlacion de las frecuencias de las palabras en ambos conjuntos 
#primero visualmente
#library(ggplot2)
#ggplot(comunes, aes(x = freq.x, y = freq.y, alpha=.0001 ))+
# geom_point()+theme_minimal()+ theme(legend.position="none")+
#  xlab('Conjunto de palabras en la bolsa "test"')+
# ylab('Conjunto de palabras en la bolsa "train"')+
#ggtitle('Frecuencia de palabas comunes en ambos conjuntos de mails ')
#parece indicar correlación positiva
#hacemos un test de significancia 
#cor.test(comunes$freq.x, comunes$freq.y, method = 'pearson', alternative='two.sided')
#cor(comunes$freq.x, comunes$freq.y)
#como el p-value es casi cero rechazamos la hipotesis nula por lo que la correlacion es alta y da elementos para descartarlas del analisis
#asumimos quue la frecuencia de las palabras es aproximadamente la misma 
################################################# formaremos el vector de features para entrenar el algoritmo marcado como cero si la variable-palabra no se encuentra en el test o en el train
todos <- preprocesamiento("mails") #todos los mails 
utiles <- todos[['mtd']]
utiles <- utiles[(row.names(utiles) %in% as.character(comunes$word)),   ]#filtramos del corpus general
gc()
gc()
df.m <- as.data.frame(t(utiles))
d <- todos[['frecuencias']]
d <- d[(d$word %in% as.character(comunes$word)),] #filtramos las palabras que se repiten en ambos conjuntos
#procedemos a determinar que cuantas palabras utilizar
plot(table(d$freq), xlim=c(1,300))
quantile(d$freq, probs = (1:99)/100)
d <- d[order(d$freq, decreasing = TRUE),] #ordenamos para indetificar de manera mas facil las palabras
remove("todos")
gc()


library(ggplot2)
####################################################################
treshold <- 500 #parametro que fija el numero de palabras (de mayor ocurrencia conjunta ocupar)
poquitas <- d$word[d$freq > treshold] #filtramos las palabras de inuestro interes
utiles <- utiles[(row.names(utiles) %in% as.character(poquitas)),   ]
#############################################################################
v <- sort(rowSums((utiles)),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v) #totales
##filtramos los conjuntos test y train 
d <- d[ order(d$word, d$freq), ]
test.in <- as.data.frame(test$mtd)
colnames(test.in) <- 1:dim(test.in)[2]
test.in$word <- row.names(test.in)
test.in <- merge(d, test.in, by = 'word', all.x = TRUE)
gc() #se libero espacio y lo liberamos
test.in[is.na(test.in)] <- 0 #una imputación rapida
test.in <- as.data.frame(t(test.in))
test.label <- rep(1, dim(test.in)[1]-2)#1 encoding de la variable respuesta: no spam
test.label[ 500:1000] <- 0 # -1 spam
test.in <- test.in[-c(1,2),]
test.in <- apply(test.in, 2, function(x){as.numeric(x)})
test.in <- as.data.frame(test.in)
## ajustamos los datos para que cada reglon sea un correo
train.in <- as.data.frame(train$mtd)
colnames(train.in) <- 1:dim(train.in)[2] #correcion de nombres
train.in$word <- row.names(train.in)
train.in <- merge(d, train.in, by = 'word', all.x = TRUE) #nos quedamos solo con los registros que si aparecen en el train
gc()
train.in[is.na(train.in)] <- 0 #una imputación rapida
train.in <- as.data.frame(t(train.in))
train.label <- rep(1, dim(train.in)[1]-2)#encoding de variable de respuesta
train.label[ 1400:2200 ] <- 0
train.in <- train.in[-c(1,2),]
train.in <- apply(train.in, 2, function(x){as.numeric(x)})
train.in <- as.data.frame(train.in)
gc()
########## como los datos tienen poca variabilidad usamos un jitter
p <- length(poquitas) #numero de maximo de palabras a utilizar
Y.train <- train.label
train.in <- apply(train.in, 2,  function(x){jitter(x)})
train.in <- as.data.frame(scale(train.in))
Y.test <- test.label
test.in[is.na(test.in)]  <- 0
test.in <- apply(test.in,2,  function(x){jitter(x)})
test.in <- as.data.frame(scale(test.in))
gc()
gc()
gc()
#####################################################
# comienza  el analisis
library(pROC)
library(glmnet) #de los mismos autores que el libro !!!! carga el packate para hacer calculos en paralelo :D
############ aplicamos regresion logistica
t.log <- Sys.time() 
log.model <- glmnet( x = as.matrix(train.in), (Y.train), family = "binomial", 
                     lambda = 0, nlambda = 0, alpha = 0) #por que se trata de una regresion sencilla sin penalizaciones
y.hat.log <- predict(log.model, as.matrix(test.in), type = "class")
acc.log <- sum(diag(table(Y.test, y.hat.log)))/length(y.hat.log)
t.log <- Sys.time() - t.log
auc.log  <- auc(Y.test, (y.hat.log))
log.roc <- roc(Y.test, as.numeric(y.hat.log))
regresion <- ggroc(log.roc, alpha = 0.5, colour = "purple", linetype = 1, size = 2) +
  theme_minimal() + 
  ggtitle(paste0('Regresión logística AUC: ',auc.log))
#####aplicamos LDA
t.lda <- Sys.time()
library(MASS)
lda.model <- lda( factor(Y.train)  ~ ., data = train.in )
y.hat.lda <- predict(lda.model, test.in)$class
acc.lda <- sum(diag(table(Y.test, y.hat.lda)))/length(y.hat.lda)
t.lda <- Sys.time() - t.lda
auc.lda <- auc(Y.test, as.numeric(y.hat.lda))
log.lda <- roc(Y.test, as.numeric(y.hat.lda))
lda <- ggroc(log.lda, alpha = 0.7, colour = "green4", linetype = 1, size = 2) +
  theme_minimal() + ggtitle(paste0('LDA AUC: ', auc.lda))
####aplicamos  QDA Como la implementación del package MASS
t.qda <- Sys.time() 
qda.model <- qda( (Y.train)  ~ ., data = train.in )
y.hat.qda <- predict(qda.model, test.in)$class
acc.qda <- sum(diag(table(Y.test, y.hat.qda)))/length(y.hat.lda)
t.qda <- Sys.time() - t.qda
auc.qda <- auc(Y.test, as.numeric(y.hat.qda))
roc.qda <- roc(Y.test, as.numeric(y.hat.qda))
qda <- ggroc(roc.qda, alpha = 0.7, colour = "navy", linetype = 1, size = 2) +
  theme_minimal() + ggtitle(paste0('QDA AUC: ',auc.qda))
##############################
## Aplicamos FDA (nuestra propia implementacion)
t.LDA <- Sys.time() 
LDA.lm <- function(m.a, y)
{
  #ENTRADAS:
  #m muestra
  #y etiquetas con encoding 1,-1
  #SALIDA: UN MODELO LINEAL y la muestra generada
  #generamos la muestra dos dimensional
  ###############se agregan pesos#############
  m <-  dim(m.a)[1]
  p <- dim(m.a)[2]
  cc <- apply( m.a, 2, mean ) # centroide de los datos 
  c1 <- m.a[ y == 1, ]
  cc1 <- apply(c1, 2, mean) #centroide poblacion 1
  c2 <- m.a[y == -1, ]
  cc2 <- apply(c2, 2, mean) #centroide poblacion 2
  S1 <- var(c1)
  S2 <- var(c2)
  d1 <- mahalanobis(x = as.data.frame(c1), center = cc1, cov = S1)
  d2 <- mahalanobis(x = as.data.frame(c2), center = cc2, cov = S2)
  w <- c(d1, d2)
  #asignamos mismas aprioris
  m.mas <- dim(c1)[1]
  m.menos <-dim(c2)[1]
  Y <- rep(-(m)/m.mas,m)
  Y[y ==-1 ] <- (m)/m.menos
  ##############################################
  #unidad <- rep(1, dim(m.a)[1])
  #m.a <- cbind(unidad ,m.a) 
  modelo.lineal <- lm( Y ~ ., data = m.a )#, weights = 1/w )
  beta <- modelo.lineal$coefficients
  m.a$y.hat <- sign( cbind(rep(1,m),as.matrix(m.a))%*%(beta)) 
  return(list(beta = beta, muestra= m.a, model= modelo.lineal, centers = data.frame(rbind(cc1, cc2))))
}
gc()
LDA.model <- LDA.lm( m.a = train.in, y = Y.train )
beta <- LDA.model$beta
dim(beta) <- c( length(beta) ,1)
y.hat.LDA <- sign( cbind(rep(1, dim(test.in)[1]), as.matrix(test.in)) %*% (beta))
acc.LDA.lm <- sum(diag(table(Y.test, y.hat.LDA)))/length(y.hat.LDA)
t.LDA <- Sys.time() - t.LDA
auc.LDA.lm <- auc(Y.test, as.numeric(y.hat.LDA))
roc.LDA <- roc(Y.test, as.numeric(y.hat.LDA))
LDA <- ggroc(roc.LDA, alpha = 0.7, colour = "pink2", linetype = 1, size = 2) +
  theme_minimal() + ggtitle(paste0('LDA AUC: ', auc.LDA.lm))
### aplicamos nuestra implementacion de KFLDA
#############load and compile 
#parametros encontrados en el ejercicio 4 
mu <- .046 #del ejercicio 4
sigma <- 44.29
################################
FLDA <- function(train, test, Y.train, Y.test, mu , sigma, search = TRUE)
{
  #Search : indica si se devuelve o no el vector de predicciones
  l.train <- dim(train)[1]
  l.1.train <- sum( Y.train == 1)
  l.2.train <- sum( Y.train == 0)
  #################################### a ver si centrando sale algo 
  # train <- scale(train)
  #################################### a ver si centrando sale algo 
  W <- matrix( rep(0, l.train*l.train), ncol = l.train)
  dimension <- dim(train)[2]
  W <- Kernel(W, as.matrix(train), l.train , sigma, dimension )
  #plot(image(W))
  M.1 <- apply(W[, 1:l.1.train ], 1 ,sum, na.rm  = TRUE)
  M.2 <- apply(W[, (l.1.train+1):l.2.train], 1 ,sum, na.rm  = TRUE)
  M.1 <- M.1*(1/l.1.train)
  M.2 <- M.2*(1/l.2.train)
  M <- (M.1 - M.2)%*%t(M.1 - M.2)
  #image(M)
  ############ resolviendo (otro) generalized eigenvalue problem  
  K.1 <- W[, 1:l.1.train]
  K.2 <- W[, (l.1.train+1):l.train]
  I.1 <- diag(rep(1, l.1.train))
  L1 <- matrix( rep(1/l.1.train, l.1.train**2 ), ncol = l.1.train)
  KK.1 <- K.1%*%(I.1-L1)%*%t(K.1) 
  #
  I.2 <- diag(rep(1, l.2.train))
  L2 <- matrix( rep(1/l.2.train , l.2.train**2 ), ncol = l.2.train)
  KK.2 <- K.2%*%(I.2-L2)%*%t(K.2)
  ############
  N <- KK.2 + KK.1
  det(N)
  ############## Forzando que N sea semidefinida positiva 
  N <- (N + mu*(diag(rep(1,l.train)))) 
  if(!is.finite(det(N)))
  {
    print(paste0('error en parametros mu: ', mu, ' sigma : ', sigma))
    return(list(mu = mu, sigma = sigma, train = -1, test = -1))
  }
  A <- (solve(N)%*%M)
  library(RSpectra)
  A2 <- as(A, "sparseMatrix") #casteamos a clase 'sparseMatrix'
  #isSymmetric(A2)
  alphas <- eigs(A2, k=1, which = 'LM') #usamos lanczos
  vectors <- alphas$vectors
  alpha <- vectors[, 1]
  alpha <- Re(alpha)
  ###########evaluando train
  y.hat.train <- W%*%alpha
  y.hat.train <- (Re(y.hat.train))
  #hist(y.hat.train)
  c1 <- mean(y.hat.train[ Y.train==1 ] ) 
  c2 <- mean(y.hat.train[ Y.train==-1] )
  y.hat.train <- data.frame(y.hat.train = y.hat.train )
  y.hat.train <- apply(y.hat.train, 1, function(x)
  {
    distancias <- c(Inf, Inf)
    distancias[1] <- sum((x-c1)**2)
    distancias[2] <- sum((x-c2)**2)
    a <- which.min(distancias)
    res <- ifelse(a == 1, 1, -1)
  })
  train.acc <- (sum(diag(table(y.hat.train, Y.train)))/l.train)#train con detalles
  l.test <- dim(test)[1]
  l.1.test <- sum( Y.test == 1)
  l.2.test <- sum( Y.test == -1)
  #################################### 
  W.test <- matrix( rep(0, l.test*(l.train)), ncol = l.train) 
  W.test <- KernelTest(W.test, as.matrix(test), as.matrix(train),  l.test,
                       l.train, sigma, dimension)
  
  #print(image(W.test))
  y.hat.test <- W.test%*%alpha
  #########
  y.hat.test <- apply(y.hat.test, 1, function(x)
  {
    distancias <- c(Inf, Inf)
    distancias[1] <- abs(x-c1)
    distancias[2] <- abs(x-c2)
    a <- which.min(distancias)
    res <- ifelse(a == 1, 1, -1)
  })
  test.acc <- (sum(diag(table(y.hat.test, Y.test)))/l.test)
  #dos posibles salidas
  if(search)
  {
    return(list(mu = mu, sigma = sigma, train = train.acc, test = test.acc ))
  } else return(list(mu = mu, sigma = sigma, train = train.acc, test = test.acc, Y.hat =  y.hat.test))
}
t.KFLDA <- Sys.time()
FLDA.model <- FLDA(train=train.in, test=test.in, Y.train=Y.train, 
                   Y.test = Y.test , mu = mu , sigma = sigma, search=FALSE) 
t.KFLDA <- Sys.time() - t.KFLDA
acc.KLDA <- sum(diag(table(Y.test, FLDA.model$Y.hat)))/length(FLDA.model$Y.hat)
auc.KLDA <- auc(Y.test, as.numeric(FLDA.model$Y.hat ))
roc.KFLDA <- roc(Y.test, as.numeric(FLDA.model$Y.hat))
KFLDA <- ggroc(roc.KFLDA, alpha = 0.7, colour = "blue2", linetype = 1, size = 2) +
  theme_minimal() + ggtitle(paste0('KFLDA AUC: ',auc.KLDA))
################## usamos redes neuronales
t.nn <- Sys.time() 
library(nnet)
par <-  round( dim(train.in)[2]*0.00000001)+1
nn.model <- nnet(factor(Y.train)  ~ ., data = train.in, size = 1) #siguiendo la recomendación de hestie
y.hat.nn <- predict(nn.model, test.in, type = "class")
acc.nn <- sum(diag(table(Y.test, y.hat.nn)))/length(y.hat.nn)
roc.nn <- roc(Y.test, as.numeric(y.hat.nn))
auc.nn <- auc(Y.test, as.numeric(y.hat.nn))
NN <- ggroc(roc.nn, alpha = 0.7, colour = "blue2", linetype = 1, size = 2) +
  theme_minimal() + ggtitle(paste0('NN AUC: ',auc.nn))
t.nn <- Sys.time() - t.nn
########## resultados de tiempo accuracy y AUC
t.log
auc.log
acc.log

t.lda
auc.lda
acc.lda

t.qda
auc.qda
acc.qda

t.LDA
acc.LDA.lm
auc.LDA.lm

t.KFLDA
acc.KLDA
auc.KLDA

t.nn
acc.nn
auc.nn
##############FIN y graficamos
xx <- Sys.time() -xx
library(ggpubr)
ggarrange(regresion, lda, qda, LDA, KFLDA, NN, ncol = 2, nrow = 3 )

