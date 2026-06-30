# Librerias 

library(shiny)
library(shinydashboard)
library(tidyverse)
library(readr)
library(DT)

base <- read_delim("~/Proyecto-de-XS-0129/student-por.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)
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
        "Brecha digital en la educación"
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
    
    box(
      plotOutput("plot_4", height = 400)
    )
    
  )
)
  
  
# Servidor
server = function(input, output){
  output$plot_4 <- renderPlot({
    ggplot(base, aes(x = schoolsup, y = G3, fill = schoolsup)) +
      geom_boxplot(alpha = 0.7, outlier.shape = NA) + 
      geom_jitter(width = 0.2, alpha = 0.3, color = "black") + 
      scale_fill_manual(values = c("orange", "lightblue"), labels = c("No recibe apoyo", "Recibe apoyo")) +
      labs(
        title = "Distribución de Calificaciones Finales (G3)",
        subtitle = "Comparativa según recepción de apoyo educativo extra",
        x = "Apoyo educativo extra",
        y = "Calificación Final (G3)",
        fill = "Grupo"
      )
  })
}


shinyApp(ui = ui, server = server)