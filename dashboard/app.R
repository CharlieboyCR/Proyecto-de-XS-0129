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
          plotOutput("boxplot")
        )
      )
    )
  )
)
  
  
# Servidor
server = function(input, output){
  output$boxplot <- renderPlot ({
    boxplot(
      base$G3~base$internet,
      main = "Brecha en la educación",
      xlab = "Acceso a Internet"
      ylab = "Valor",
      col = "grey"
    )
  })
  
}

shinyApp(ui = ui, server = server)
