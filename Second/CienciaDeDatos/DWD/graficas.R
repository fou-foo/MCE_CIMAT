####################################
#####    J. Antonio Garcia #########
#####   jose.ramirez@cimat.mx ######
####################################
#setwd('/home/fou/Desktop/MCE_CIMAT/Second/CienciaDeDatos/DWD/')
#########################################
# Construccion de la primer ilustracion # 
#########################################
#generamos una muestra de dim 39 mormal multi y la guardamos
n <- 20
d <- 1000
set.seed(123)
I <- diag(rep(1, d))
library(MASS)
pos <- as.data.frame(mvrnorm(n = n, mu = rep(2.2, d), Sigma = I))
neg <- as.data.frame(mvrnorm(n = n, mu = rep(-2.2, d), Sigma = I))
remove(I)
save.image(file='datos.RData')
#save(pos, file =  'pos1000.rdata')
#save(neg, file =  'neg1000.rdata')
#saveRDS(pos, 'pos1000.RDS')
#saveRDS(neg, 'neg1000.RDS')
stack <- rbind(pos,neg)
pos.mean <- apply(pos, 2, mean)
neg.mean <- apply(neg, 2, mean)
w <- ginv(I) %*% (pos.mean - neg.mean)
w <- w/sum(w**2)**.5 #normalizamos el vector MDP
X <- ginv(cov(pos)) %*% (pos.mean - neg.mean)
X <- X/sum(X**2)**.5 #normalizamos el vector que define al frontera de Bayes
#acos(sum(X*w))*360/(2*pi)
stack$label <- 1
stack$label[(n+1):(2*n)] <- -1 
Y <- X
Y[-c(1,2)] <- 0
Y[1] <- X[2]
Y[2] <- -X[1] #elegimos un vector perpenticular a X
Y <- Y/sum(Y**2)**.5
sum(Y*w)
M <- cbind(X, Y,  w)
pos.proyec <- as.matrix(pos)%*%M
neg.proyec <- as.matrix(neg)%*%M
pos.proyec <- as.data.frame(pos.proyec)
neg.proyec <- as.data.frame(neg.proyec)
pos.proyec$label <- '+1'
neg.proyec$label <- '-1'
proyec <- rbind(pos.proyec, neg.proyec)
datos <- list(proyec, w )
proyec <- (datos[[2]])
library(ggplot2)
p1 <- ggplot(data = proyec, aes(x=V2, y=V3, color=label))+geom_point() +
  stat_function(fun = function(z){z*(w[1]/w[2])}, aes(colour = I('pink')), size=1.5)+
  geom_hline(yintercept=0, aes(colour=I('red')),     show.legend = NA)+
  ggtitle('Proyección en la dirección opima de Bayes y MaxDataPiling') +
   theme_minimal()+  xlab('Bayes') + ylab('') + 
  scale_colour_manual(
    labels = c('-1', '+1', 'MDP'),
    values = c("purple", "orange", "blue")
  )+ theme(legend.title = element_blank())
plot(p1) #puntos pegados a la frontera
p2 <- ggplot(data = proyec, aes(x=V3, fill=label, colour=label))+geom_density()+
  geom_rug(sides="b")+ggtitle('Distribución en la dirección de Bayes') + theme_minimal()+
  xlab('Bayes') + ylab('') + 
  scale_fill_manual(  labels = c('-1', '+1'), values = c("purple", "orange"))+
  scale_color_manual(  labels = c('-1', '+1'), values = c("purple", "orange"))+
  theme(legend.title = element_blank())
plot(p2) #distro en bayes
#p3 <- ggplot(data = proyec, aes(x=label, y=V2, fill=label, colour=label))+
#geom_boxplot() +ggtitle(as.character(j)) 
#plot(p3) valores de piling
#r <- scan()
remove(list = ls())
gc()
}
