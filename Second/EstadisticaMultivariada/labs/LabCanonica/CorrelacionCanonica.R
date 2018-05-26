# Conjunto de datos para ejemplo

data(LifeCycleSavings)
head(LifeCycleSavings,10)

# Correr correlacion canonica

pop <- LifeCycleSavings[, 2:3]    # seleccionar pop15 and pop75
oec <- LifeCycleSavings[, -(2:3)] # seleccionar los dem?s
debug(cancor)
a <- cancor(pop, oec)
undebug(cancor)
# CCA package for F-test, varimax rotation, graphs
plot(a)
#install.packages("CCA")
library(CCA)
matcor(pop,oec)
res.cc=cc(pop,oec)
plt.cc(res.cc,type="i")

# yacca package

install.packages("yacca")
library(yacca)
options(scipen=999)
cca.fit = cca(pop,oec)
?F.test.cca(cca.fit)
plot(cca.fit)
