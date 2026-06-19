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
                      
                    ),
                    
                    dashboardBody(
                      
                    )
)


# Servidor
server = function(input, output){
  
}

shinyApp(ui = ui, server = server)