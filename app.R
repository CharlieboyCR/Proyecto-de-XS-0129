# Librerias 

library(shiny)
library(shinydashboard)
library(tidyverse)
library(readr)

getwd()
base <- read_delim("student-por.csv",
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Dashboard
ui <- dashboardPage(title = "Proyecto Shiny", 
                    skin = "yellow",
  dashboardHeader(
    title = "Proyecto test"
  ),
  
  dashboardSidebar(
  ),
  
  dashboardBody(
    
    box(
      plotOutput("plot_1", height = 250)
    )
    
  )
)

  
# Servidor
server = function(input, output){
  
  output$plot_1 <- renderPlot({
    ggplot(data = base, aes(x = absences, y = G3, color = case_when(G3 <= 9 ~ "Desempeño insuficiente", G3 < 13 ~ "Satisfactorio", G3 < 17 ~ "Bueno", G3 >= 17 ~ "Excelente"))) + geom_point() + labs(color = "Categoría") + geom_smooth(method = "lm", se = FALSE)
  })
}

shinyApp(ui = ui, server = server)
