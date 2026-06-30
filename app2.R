library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)


base <- read_delim("student-por.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

ui <- fluidPage(
  theme = shinytheme("cosmo"), 
  
  titlePanel("Análisis de Calificaciones de Portugués y Soporte Institucional/Familiar"),
  
  sidebarLayout(
    sidebarPanel(
      tags$h4("Filtros de Análisis"),
      hr(),
      
      radioButtons("variable_apoyo", 
                   label = "Seleccione el tipo de soporte a analizar:",
                   choices = c("Soporte Educativo Extra (schoolsup)" = "schoolsup",
                               "Soporte Familiar (famsup)" = "famsup"),
                   selected = "schoolsup"),
      
      hr(),
      tags$small("Este panel analiza la influencia del soporte en la nota final (G3).")
    ),
    
    mainPanel(
      tabsetPanel(
       
        tabPanel("Tabla Resumen", 
                 br(),
                 tags$h3("Métricas Descriptivas de G3"),
                 p("A continuación se presentan la media, mediana, desviación estándar y conteo de estudiantes según el soporte recibido:"),
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


server <- function(input, output) {
  

  output$tabla_resumen <- renderTable({
    base %>%
      group_by(Soporte = .data[[input$variable_apoyo]]) %>%
      summarise(
        `Total Estudiantes` = n(),
        `Nota Media (G3)`   = round(mean(G3, na.rm = TRUE), 2),
        `Nota Mediana (G3)` = median(G3, na.rm = TRUE),
        `Desviación Estg.`  = round(sd(G3, na.rm = TRUE), 2)
      )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = 'c')
  
  
  output$boxplot_g3 <- renderPlot({
    ggplot(base, aes(x = .data[[input$variable_apoyo]], y = G3, fill = .data[[input$variable_apoyo]])) +
      geom_boxplot(alpha = 0.7, outlier.colour = "red", outlier.shape = 16) +
      labs(
        title = paste("Distribución de G3 según", input$variable_apoyo),
        x = paste("¿Recibe", input$variable_apoyo, "?"),
        y = "Calificación Final (G3)"
      ) +
      theme_minimal() +
      scale_fill_brewer(palette = "Set2") +
      theme(legend.position = "none", plot.title = element_text(face = "bold", size = 14))
  })
  
  
  output$histograma_g3 <- renderPlot({
    ggplot(base, aes(x = G3, fill = .data[[input$variable_apoyo]])) +
      geom_histogram(binwidth = 1, alpha = 0.6, position = "identity", color = "white") +
      labs(
        title = paste("Histograma de Frecuencias de G3 por", input$variable_apoyo),
        x = "Calificación Final (G3)",
        y = "Frecuencia Absoluta",
        fill = input$variable_apoyo
      ) +
      theme_minimal() +
      scale_fill_brewer(palette = "Set2") +
      theme(plot.title = element_text(face = "bold", size = 14))
  })
}

# Desplegar la aplicación
shinyApp(ui = ui, server = server)