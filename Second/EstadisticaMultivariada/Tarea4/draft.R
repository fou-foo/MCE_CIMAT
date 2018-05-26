setwd(dir = 'C:\\Users\\fou-f\\Desktop\\ROLOCTS')
dir()
#archivo <-'EXAMPLE1.TXT_lambda.txt'
#archivo <- 'E543.TXT_lambda.txt'
#archivo <-'E87113.TXT_lambda.txt'
archivo <- 'GEN.TXT_lambda.txt'
datos <- read.table(archivo, header=TRUE, sep ='\t')
library(ggplot2)
p1<- ggplot(datos, aes(x=expexted_value, y =risk )) +
  geom_point()+theme_minimal()+
  theme(legend.position="none")+ggtitle(archivo)
p1
Pareto.frontier <- function(l)
{
  a1 <- subset(datos, lambda==l)
  M <- 1000000
  no.dominado <- c(M, M)
  for(i in 1: dim(a1)[1])
  {
    if(a1[i, 'expexted_value']<=no.dominado[1] && 
       a1[i, 'risk']<= no.dominado[2] )
    {
      index <- i
      print(index)
      no.dominado <- c(a1[i, 'expexted_value'], a1[i, 'risk'])
    }
  }
  return(as.matrix(c(index,no.dominado[1], no.dominado[2])))
}
frontera <- mapply(FUN = Pareto.frontier, sort(unique(datos$lambda)) )
frontera <- data.frame(iteracion=frontera[1,], 
                       expexcted_value=frontera[2,], risk = frontera[3,] )
frontera$l <- sort(unique(datos$lambda))
p1 <- p1 + geom_point(data = frontera, aes(x=expexcted_value ,
                                           y = risk, col=as.factor(l)))
p1
optimo.pareto <- function(frontera)
{
  i <- which.min(frontera$expexcted_value+frontera$risk)
  return(frontera[i, ])
}
optim.pareto <- data.frame(optimo.pareto(frontera))
p1 + geom_point(data = optim.pareto, aes(x=expexcted_value,
                                         y = risk), size=5,  colour=I('red'))
                