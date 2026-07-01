#Cambios de Raque
library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)


library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(ggplot2)

base <- read_delim("student-por.csv", 
                   delim = ";", escape_double = FALSE, trim_ws = TRUE)

# UI del Dashboard
ui <- dashboardPage(
  title = "Proyecto Shiny", 
  skin = "yellow",
  
  dashboardHeader(
    title = "Proyecto test"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs", 
      menuItem(
        "Estudio y rendimiento académico", 
        tabName = "estudio"
      ),
      menuItem(
        "Brecha digital en la educación", 
        tabName = "grafico_2"
      ),
      menuItem(
        "Educación parental", 
        tabName = "grafico_3"
      ),
      menuItem(
        "Apoyo educativo",
        tabName = "apoyo_educativo"
      ),
      menuItem(
        "Parámetros editables",
        icon = icon("gear"),
        menuSubItem(
          text = radioButtons("variable_apoyo", 
                              label = "Tipo de apoyo:",
                              choices = c("Apoyo escolar" = "schoolsup",
                                          "Apoyo familiar" = "famsup"),
                              selected = "schoolsup")
        ),
        menuSubItem(
          "Segundo parámetro global"
        ),
        menuSubItem(
          "Tercer parámetro global"
        )
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "estudio",
              h2("Estudio y rendimiento académico"),
              p("Contenido en desarrollo...")
      ),
      tabItem(tabName = "grafico_2",
              h2("Brecha digital en la educación"),
              p("Contenido en desarrollo...")
      ),
      tabItem(tabName = "grafico_3",
              h2("Educación parental"),
              p("Contenido en desarrollo...")
      ),
      
      # Pestaña para apoyo educativo
      tabItem(tabName = "apoyo_educativo",
              fluidRow(
                box(
                  title = "Análisis de calificaciones de Portugués con respecto al apoyo escolar y familiar", 
                  width = 12, 
                  status = "primary", 
                  solidHeader = TRUE,
                  
                  tabBox(
                    title = "Resultados",
                    id = "tabset_resultados", 
                    width = 12,
                    
                    tabPanel("Tabla Resumen", 
                             br(),
                             tags$h3("Métricas Descriptivas de G3"),
                             p("A continuación se presentan la media, mediana, desviación estándar y conteo de estudiantes según el apoyo recibido:"),
                             tableOutput("tabla_resumen")
                    ),
                    
                    tabPanel("Gráficos de Distribución", 
                             br(),
                             plotOutput("boxplot_g3"),
                             br(),
                             plotOutput("histograma_g3")
                    )
                  )
                )
              )
      )
    )
  )
)


server <- function(input, output, session) {
  
  # Tabla Resumen 
  output$tabla_resumen <- renderTable({
    base %>%
      group_by(Soporte = .data[[input$variable_apoyo]]) %>%
      summarise(
        `Total Estudiantes` = n(),
        `Media de notas (G3)`   = round(mean(G3, na.rm = TRUE), 2),
        `Mediana de notas (G3)` = median(G3, na.rm = TRUE),
        `Desviación Estándar`   = round(sd(G3, na.rm = TRUE), 2)
      )
  })
  
  # Boxplot 
  output$boxplot_g3 <- renderPlot({
    ggplot(base, aes(x = .data[[input$variable_apoyo]], y = G3, fill = .data[[input$variable_apoyo]])) +
      geom_boxplot(alpha = 0.7, outlier.colour = "#891A1E", outlier.shape = 16) +
      labs(
        title = paste("Distribución de G3 según tipo de apoyo ", input$variable_apoyo),
        x = paste("Recibe", input$variable_apoyo ),
        y = "Calificación Final (G3)"
      ) +
      theme_minimal() +
      scale_fill_brewer(palette = "Set1") +
      theme(legend.position = "none", plot.title = element_text(face = "bold", size = 14))
  })
  
  # Histograma 
  output$histograma_g3 <- renderPlot({
    ggplot(base, aes(x = G3, fill = .data[[input$variable_apoyo]])) +
      geom_histogram(binwidth = 1, alpha = 0.6, position = "identity", color = "black") +
      labs(
        title = paste("Histograma de Frecuencias de G3 por tipo de apoyo", input$variable_apoyo),
        x = "Calificación Final (G3)",
        y = "Frecuencia Absoluta",
        fill = input$variable_apoyo
      ) +
      theme_minimal() +
      scale_fill_brewer(palette = "Set1") +
      theme(plot.title = element_text(face = "bold", size = 14))
  })
}

# Desplegar la aplicación
shinyApp(ui = ui, server = server)