###############################################################
## Ejemplo que captura un numero y lo proyecta en los primeros
## 50 componentes principales previamente calculados en un
## conjunto de datos de entrenamiento y guardados en pc.rds
###############################################################
library(shiny) 
library(shinythemes)
library(shinydashboard)
library(pixels)
shinyUI(fluidPage(
  theme = shinytheme("cerulean"),
  h1("Clasificación usando PCA!"),
  
  mainPanel(
    tabsetPanel(
      id = 'Clasificador',
      tabPanel('Predicción usando PCR',
               tabItem(tabName="main",hr(), 
                       fluidRow( column(5, h4("Pinta el número"),
                                        shiny_pixels_output( outputId = "pixels"),
                                        actionButton("captureDigit", "Capturar")
                       ),
                       column(5,h3("Número reconocido como:"),
                              verbatimTextOutput("matriz") )))),
                       
      tabPanel("PCA",
               withMathJax(includeMarkdown('clasificador_PCR.md'))),
      tabPanel("Doc de la app",
             tabItem(tabName="foo2",
                     withMathJax(includeMarkdown("doc_app.Rmd")) ) 
      )
    )
   )      
  )
)           