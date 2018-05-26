Delta <- matrix(c(0,627,351,550,488,603,
                  627,0,361,1043,565,1113,
                  351,361,0,567,564,954,
                  550,1043,567,0,971,950,
                  488,565,564,971,0,713,
                  603,1113,954,950,713,0), byrow = TRUE, ncol = 6)
colnames(Delta )<- row.names(Delta) <- c('M', 'B', 'V', 'S', 'SS', 'LC')
H <- diag(6)-1/6*rep(1,6)%*%t(rep(1,6))
B <- (-.5*H%*%Delta**2%*% H)*(1/100000)
eigen <- eigen(B)
plot(-eigen$values[1]*eigen$vectors[,1], eigen$values[2]*eigen$vectors[,2], pch = 20, col='purple')
text(-eigen$values[1]*eigen$vectors[,1], eigen$values[2]*eigen$vectors[,2], colnames(Delta))
cumsum(abs(eigen$values))/sum(abs(eigen$values))
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
?map_data
