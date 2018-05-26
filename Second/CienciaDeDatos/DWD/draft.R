library(MASS)  # for mvn sampling "mvrnorm"
library(class) # for k-nearest neighbors "knn"

set.seed(1000)
meansA = mvrnorm(10,c(1,0),diag(rep(1,2)))
meansB = mvrnorm(10,c(0,1),diag(rep(1,2)))
trainingAhidden = sample(10,100,TRUE)
trainingBhidden = sample(10,100,TRUE)
testAhidden = sample(10,5000,TRUE)
testBhidden = sample(10,5000,TRUE)
trainingAdata = t(sapply(trainingAhidden,function(x) mvrnorm(1,meansA[x,],diag(rep(1,2)/5))))
trainingBdata = t(sapply(trainingBhidden,function(x) mvrnorm(1,meansB[x,],diag(rep(1,2)/5))))
testAdata = t(sapply(testAhidden,function(x) mvrnorm(1,meansA[x,],diag(rep(1,2)/5))))
testBdata = t(sapply(testBhidden,function(x) mvrnorm(1,meansB[x,],diag(rep(1,2)/5))))
trainingY = gl(2,100,200)
testY = gl(2,5000,10000)
training = list(X = rbind(trainingAdata,trainingBdata),Y = trainingY)
test = list(X = rbind(testAdata,testBdata), Y = testY)
results = knn(training$X,test$X,training$Y,k=9)
tab = table(results,test$Y)
1 - sum(diag(tab))/sum(tab)
# calculating the theoretic distributions on a grid
library(mvtnorm)
za <- matrix(rep(0,100*100),100)
zb <- matrix(rep(0,100*100),100)
x <- seq(-4,4,length.out=100)
y <- seq(-2,2,length.out=100)
for (i in 1:100) {
  for (j in 1:100) {
    for (k in 1:10)
    {
      za[i,j] <- za[i,j]+0.1*dmvnorm(x[i,],meansA[k,],diag(rep(1,2)/5))
      zb[i,j] <- zb[i,j]+0.1*dmvnorm(c(x[i],y[j]),meansB[k,],diag(rep(1,2)/5))
    }
  }
}


# doing the integration to obtain the bayes error rate
dx=x[2]-x[1]
dy=y[2]-y[1]
bayes=0

for (i in 1:100) {
  for (j in 1:100) {
    bayes = bayes + 0.5*max(za[i,j],zb[i,j])*dx*dy
  }
}
1 - sum(diag(tab))/sum(tab)
1- bayes

# plotting the knn result with the bayes classifier
plot(test$X,pch=21,col=alpha(c(2,3)[results],0),bg=alpha(c(2,3)[results],1),cex=0.4,xlab="x",ylab="y")
points(training$X,pch=21,col=1,bg=c(2,3)[training$Y])
points(meansA,col=1,pch=21,bg=2,cex=2)
points(meansB,col=1,pch=21,bg=3,cex=2)
contour(x,y,za-zb,level=0,col=1,add=1)


# plotting the theoretic distributions with the bayes classifier
contour(x,y,za,add=0,col=2)
contour(x,y,zb,add=1,col=3)
contour(x,y,za-zb,level=0,col=1,add=1)