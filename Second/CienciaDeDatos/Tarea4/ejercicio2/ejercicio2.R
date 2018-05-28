###Lectura de datos y formación d eun solo dataset para hacer cv
setwd('/home/fou/Desktop/MCE_CIMAT/Second/CienciaDeDatos/Tarea4/ejercicio2/')
test <- read.table('oef.test')
train <- read.table('oef.train')
data <- rbind(test,train)
##############################################################33
library(caret)
library(ripa)
set.seed(0)
################ control para las 10 k-folds ###########################33
fitControl <- trainControl( method = "repeatedcv",   number = 10, allowParallel = TRUE, repeats = 10)
###################33 eigenfaces ########################################
# de la tarea 1 dije que usar 50 componentes era suficiente
train.lm.cv <- function(cv, data)
{
    #funcion para evaluar por cv lm
    set.seed(0)
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
as.data.frame(error.lm)
error.lm$modelo <- 'lm'
#########################
train(Class ~ ., data = train, 
      method = "gbm", 
      trControl = fitControl,
      verbose = FALSE)
