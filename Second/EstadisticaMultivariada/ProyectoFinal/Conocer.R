library(readxl)
setwd('C:\\Users\\fou-f\\Desktop\\MCE\\Second\\EstadisticaMultivariada\\ProyectoFinal\\CONOCER_Data_Docs\\CONOCER_Data_Docs')
encuesta <- read_excel(path = 'CONOCER.xlsx', sheet = 'CONOCER FINAL' )
summary(encuesta)
nulos <- is.na(encuesta)
sum(nulos)
encuesta.completa <- na.omit(encuesta)
