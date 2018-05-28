###Lectura de datos y formación d eun solo dataset para hacer cv
setwd('/home/fou/Desktop/MCE_CIMAT/Second/CienciaDeDatos/Tarea4/ejercicio2/')
test <- read.table('oef.test')
train <- read.table('oef.train')
data <- rbind(test,train)
saveRDS(data, 'data.rds')
##############################################################33
library(caret)
library(ripa)
set.seed(0)
################ control para las 10 k-folds ###########################33
###################33 eigenfaces ########################################
# de la tarea 1 dije que usar 50 componentes era suficiente
train.lm.cv <- function(cv, data)
{
    #funcion para evaluar por cv lm
    set.seed(0)
    #data <- data[ sample(1:dim(data)[1], dim(data)[1]),]
    p <- 50
    bloque <- round(dim(data)[1]/10)
    breaks <- c(seq(1, dim(data)[1], by = bloque  ), dim(data)[1])
    y <- data[,1]
    y <- factor(y)
    Y <- model.matrix(~y-1)
    data$V1 <- NULL
    acc.train <- rep(0, cv)
    acc.test <- rep(0, cv)
    for (i in 1:(length(breaks)-1))
    {
        indices <- breaks[i]:breaks[i+1]
        test <- data[ indices,   ]
        train <- data[ -indices,   ]
        pca <- princomp(train) #como los datos ya estan escalados en [-1, 1] uso la matriz de varianzas y covarianzas
        z <- pca$loadings[,1:p]
        Z <- as.matrix(train) %*%z
        y_hat <- lm(Y[ -indices,] ~ . -1 , data = as.data.frame(Z))
        b <- y_hat$coefficients
        y_hat <- Z%*%b
        y.ouput <-  apply(y_hat, 1, which.max)
        y.ouput <- y.ouput-1
        Matrix.C.train <- caret::confusionMatrix( factor(y.ouput), y[-indices]) #error de train
        Y.test.hat <- (as.matrix(data[indices,])%*%z)%*%b
        res2 <- apply(Y.test.hat, 1, which.max)
        res2 <- res2 - 1
        Matrix.C.test <- caret::confusionMatrix( factor(res2), y[indices])
        acc.train[i] <- Matrix.C.train$overall['Accuracy']
        acc.test[i] <-  Matrix.C.test$overall['Accuracy']
    }
    return( list(train=acc.train, test =acc.test) )
}
error.lm <- train.lm.cv(cv = 10, data)
error.lm  <- as.data.frame(error.lm)
error.lm$modelo <- 'lm'
##############vamos por el lda#############################################
train.LD_da.cv <- function(cv=10, data)
{
    library(MASS)
    #funcion para evaluar por cv lm
    set.seed(0)
    bloque <- round(dim(data)[1]/cv)
    breaks <- c(seq(1, dim(data)[1], by = bloque  ), dim(data)[1])
    acc.train <- rep(0, cv)
    acc.test <- rep(0, cv)
    for (i in 1:(length(breaks)-1))
    {
        indices <- breaks[i]:breaks[i+1]
        test <- data[ indices,   ]
        train <- data[ -indices,   ]
        y_hat <- lda( factor(V1) ~ ., data = data[-indices,]) 
        y.ouput <-  predict(y_hat, data[-indices, ])
        y.ouput <- y.ouput$class
        Matrix.C.train <- caret::confusionMatrix( factor(y.ouput), factor(data$V1[-indices])) #error de train
        Y.test.hat <- predict(y_hat, data[indices,])
        res2 <- Y.test.hat$class
        Matrix.C.test <- caret::confusionMatrix( factor(res2), factor(data$V1[indices]))
        acc.train[i] <- Matrix.C.train$overall['Accuracy']
        acc.test[i] <-  Matrix.C.test$overall['Accuracy']
    }
    return( list(train=acc.train, test =acc.test) )
}
error.lda <- train.LD_da.cv(cv = 10, data)
error.lda <- as.data.frame(error.lda)
error.lda$modelo <- 'lda'
###############vamos por el QDA   ########################################
train.QDA.cv <- function(cv = 10, data)
{
    #funcion para evaluar por cv qda
    set.seed(0)
    #data <- data[ sample(1:dim(data)[1], dim(data)[1]),]
    p <- 50
    bloque <- round(dim(data)[1]/10)
    breaks <- c(seq(1, dim(data)[1], by = bloque  ), dim(data)[1])
    y <- data[,1]
    y <- factor(y)
    data$V1 <- NULL
    acc.train <- rep(0, cv)
    acc.test <- rep(0, cv)
    for (i in 1:(length(breaks)-1))
    {
        indices <- breaks[i]:breaks[i+1]
        test <- data[ indices,   ]
        train <- data[ -indices,   ]
        pca <- princomp(train) #como los datos ya estan escalados en [-1, 1] uso la matriz de varianzas y covarianzas
        z <- pca$loadings[,1:p]
        Z <- as.matrix(train) %*%z
        y_hat <- qda(y[ -indices] ~ .  , data = as.data.frame(Z))
        y.ouput <-  predict(y_hat, as.data.frame(Z))
        y.ouput <- y.ouput$class
        Matrix.C.train <- caret::confusionMatrix( factor(y.ouput), factor(y[-indices])) #error de train
        Z.y <- as.matrix(test) %*% z
        Y.test.hat <- predict(y_hat, as.data.frame(Z.y))
        res2 <- Y.test.hat$class
        Matrix.C.test <- caret::confusionMatrix( factor(res2), factor(y[indices]))
        acc.train[i] <- Matrix.C.train$overall['Accuracy']
        acc.test[i] <-  Matrix.C.test$overall['Accuracy']
    }
    return( list(train=acc.train, test =acc.test) )
}
error.qda <- train.QDA.cv(cv = 10, data)
error.qda <- as.data.frame(error.qda)
error.qda$modelo <- 'qda'
################ vamos por la red ###############################3
train.NN <- function(cv=10, data)
{
    #funcion para evaluar por cv NN
    library(nnet)
    set.seed(0)
    bloque <- round(dim(data)[1]/cv)
    breaks <- c(seq(1, dim(data)[1], by = bloque  ), dim(data)[1])
    acc.train <- rep(0, cv)
    acc.test <- rep(0, cv)
    for (i in 1:(length(breaks)-1))
    {
        indices <- breaks[i]:breaks[i+1]
        test <- data[ indices,   ]
        train <- data[ -indices,   ]
        y_hat <- nnet(factor(V1)  ~ ., data = data[-indices,], size = 3) #siguiendo la recomendación de hestie
        y.ouput <-  predict(y_hat, data[-indices, ], type = 'class')
        Matrix.C.train <- caret::confusionMatrix( factor(y.ouput), factor(data$V1[-indices])) #error de train
        Y.test.hat <- predict(y_hat, data[indices,], type = 'class')
        res2 <- Y.test.hat
        Matrix.C.test <- caret::confusionMatrix( factor(res2), factor(data$V1[indices]))
        acc.train[i] <- Matrix.C.train$overall['Accuracy']
        acc.test[i] <-  Matrix.C.test$overall['Accuracy']
    }
    return( list(train=acc.train, test =acc.test) )
}
error.nn <- train.NN(cv = 10, data)
error.nn <- as.data.frame(error.nn)
error.nn$modelo <- 'nn'
################ vamor por el favorito de todos SVM###########################
train.SVM <- function(cv = 10, data, C)
{
    library(e1071)
    set.seed(0)
    bloque <- round(dim(data)[1]/cv)
    breaks <- c(seq(1, dim(data)[1], by = bloque  ), dim(data)[1])
    acc.train <- rep(0, cv)
    acc.test <- rep(0, cv)
    for (i in 1:(length(breaks)-1))
    {
        indices <- breaks[i]:breaks[i+1]
        test <- data[ indices,   ]
        train <- data[ -indices,   ]
        y_hat <- svm(factor(V1)  ~ ., data = data[-indices,], kernel='linear' ) #siguiendo la recomendación de hestie
        y.ouput <-  predict(y_hat, data[-indices, ], type = 'class')
        Matrix.C.train <- caret::confusionMatrix( factor(y.ouput), factor(data$V1[-indices])) #error de train
        Y.test.hat <- predict(y_hat, data[indices,], type = 'class')
        res2 <- Y.test.hat
        Matrix.C.test <- caret::confusionMatrix( factor(res2), factor(data$V1[indices]))
        acc.train[i] <- Matrix.C.train$overall['Accuracy']
        acc.test[i] <-  Matrix.C.test$overall['Accuracy']
    }
    return( list(train=acc.train, test =acc.test) )
}
error.SVM <- train.SVM(cv = 10, data)
error.SVM <- as.data.frame(SVM)
error.SVM$modelo <- 'SVM'
############################################3
c.tuning <- seq(0.1, 20, length=15)
mclapply(fun = train.SVM, red(data, 15), c.tuning)