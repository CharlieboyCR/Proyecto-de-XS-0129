# Librerias
library(shiny)
library(shinydashboard)
library(tidyverse)
library(readr)

base <- read_delim("student-por.csv", 
                   delim = ";", 
                   escape_double = FALSE, 
                   trim_ws = TRUE)

# UI del Dashboard
ui <- dashboardPage(title = "Proyecto Shiny", 
                    skin = "yellow",
                    dashboardHeader(
                      title = "Proyecto test"
                    ),
                    
                    dashboardSidebar(
                      sidebarMenu(
                        # Mantenemos el texto bonito para el usuario, pero simplificamos el tabName
                        menuItem("Estudio y rendimiento académico", tabName = "estudio"),
                        menuItem("Brecha digital en la educación", tabName = "brecha"),
                        menuItem("Educación parental", tabName = "parental"),
                        menuItem("Apoyo educativo", tabName = "apoyo"),
                        menuItem("Parametros editables",
                                 menuSubItem("Primer parametro global", tabName = "param1"),
                                 menuSubItem("Segundo parametro global", tabName = "param2"),
                                 menuSubItem("Tercer parametro global", tabName = "param3")
                        )
                      )
                    ),
                    
                    dashboardBody(
                      tabItems(
                        # Conectamos con el tabName simplificado "parental"
                        tabItem(tabName = "parental",
                                fluidRow(
                                  box(
                                    plotOutput("plot_1", height = 250)
                                  )
                                )
                        ),
                        # Conectamos con el tabName simplificado "estudio"
                        tabItem(tabName = "estudio",
                                h2("prueba de texto")
                        )
                      )
                    )
)

# Servidor
server = function(input, output){
  
  output$plot_1 <- renderPlot({
    ggplot(data = base, aes(x = absences, y = G3, 
                            color = case_when(G3 <= 9 ~ "Desempeño insuficiente", 
                                              G3 < 13 ~ "Satisfactorio", 
                                              G3 < 17 ~ "Bueno", 
                                              G3 >= 17 ~ "Excelente"))) + 
      geom_point() + 
      labs(color = "Categoría") + 
      geom_smooth(method = "lm", se = FALSE)
  })
}

shinyApp(ui = ui, server = server)

