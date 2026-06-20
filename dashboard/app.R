# Librerias 

library(shiny)
library(shinydashboard)
library(tidyverse)
library(readr)

# Dashboard
ui <- dashboardPage(title = "Proyecto Shiny", 
                    skin = "green",
  dashboardHeader(
    title = "Proyecto test"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Datos"
      ),
      menuItem(
        "Primera pregunta"
      ),
      menuItem(
        "Segunda pregunta"
      ),
      menuItem(
        "Tercera pregunta"
      ),
      menuItem(
        "Cuarta pregunta"
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
    
  )
)
  
  
# Servidor
server = function(input, output){
  
}

shinyApp(ui = ui, server = server)
