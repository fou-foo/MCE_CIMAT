###############################################################
## Ejemplo que captura un numero y lo proyecta en los primeros
## 50 componentes principales previamente calculados en un
## conjunto de datos de entrenamiento y guardados en pc.rds
###############################################################
library(shiny) 
library(pixels)


shinyServer(function(input, output) 
{
  ## carga el objeto que contiene los PC calculados previamente
  z <- readRDS("componentes.rds")
  ## coeficientes
  B <- readRDS('coeficientes.rds')
  r.scale <- function(m)
  {
    #funcion para que la entrada de pixeles tenga la misma escala que los datos
    max <- max(m)
    min <- min(m)
    m <- ((m - min)/ (max - min)) # m \in [0,1]
    m*2-1     #asi m \in [-1, 1]
  }
  
  output$pixels <- shiny_render_pixels(
    show_pixels(grid=c(16,16), brush=matrix(c(1,1,1,1),2,2))) #obtencion de los pixeles
  
    output$prompt <- renderText("Dibuja un número de una cifra")
    observeEvent(input$captureDigit, 
                 {
                   dig <<- (input$pixels)
                   data.digit <- matrix(dig,nrow=16,ncol=16,byrow=T)        
                   output$pixels <- shiny_render_pixels(
                            show_pixels(grid=c(16,16),
                          brush=matrix(c(.5,1,.5,1,1,1,.5,1,.5),3,3))
                            )
    output$matriz <- renderPrint({
      entrada <- r.scale(dig)
      proj <-entrada%*%z
      Y.hat.probs <- proj %*%B
      Y.hat <- which.max(Y.hat.probs)
      Y.hat <- Y.hat - 1 
      Y.hat
    })
    
    }) 
})
