# Librerias 

library(shiny)
library(shinydashboard)
library(tidyverse)
library(readr)

base <- read_delim("~/Proyecto-de-XS-0129/student-por.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(base)
# Dashboard
ui <- dashboardPage(title = "Proyecto Shiny", 
                    skin = "yellow",
  dashboardHeader(
    title = "Proyecto test"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Estudio y rendimiento académico"
      ),
      menuItem(
        "Brecha digital en la educación", 
        tabName = "gráfico_2"
      ),
      menuItem(
        "Educación parental"
      ),
      menuItem(
        "Apoyo educativo"
      ),
      menuItem(
        "Parametros editables",
        menuSubItem(
          "Primer parametro global"
        ),
        menuSubItem(
          "Segundo parametro global"
        ),
        menuSubItem(
          "Tercer parametro global"
        )
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "gráfico_2",
        h2("Brecha digital en la educación"),
        box(
          width = 12,
          radioButtons(
            inputId = "tipoGrafico",
            label = "Tipo Análisis",
            choices = c("Análisis Formal" = "Formal", "Análisis Visual" = "Visual"),
            selected = "Visual",
            inline = TRUE
          ),
          plotOutput("grafico")
        )
      )
    )
  )
)

  
  
# Servidor
server = function(input, output){
  output$grafico <- renderPlot ({
    if(input$tipoGrafico == "Visual") {
    boxplot(
      base$G3~base$internet,
      main = "Brecha en la educación",
      xlab = "Acceso a Internet",
      ylab = "Valor",
      col = "grey"
    ) } else {
      z = seq(-4,4, length.out = 1000)
      plot(z,dnorm(z))
    }
  })
  
}

shinyApp(ui = ui, server = server)
