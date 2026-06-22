# Librerias 

library(shiny)
library(shinydashboard)
library(tidyverse)
library(readr)

base <- read_delim("~/Proyecto-de-XS-0129/student-por.csv",
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
    ggplot(data = base, aes(x = absences, y = G3, colour = case)) 
    + geom_point() 
    + geom_smooth(formula = y ~ x, method = "lm")
    
  })
}

shinyApp(ui = ui, server = server)
