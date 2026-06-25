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



####

student <- read_delim("student-por.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(student)

# Resumen de las calificaciones (G3) por apoyo escolar (schoolsup)
resumen_apoyo <- student %>%
 group_by(schoolsup) %>%
  summarise(
    Cantidad_Estudiantes = n(),
    Media = mean(G3, na.rm = TRUE),
    Mediana = median(G3, na.rm = TRUE),
    Desviacion_Estandar = sd(G3, na.rm = TRUE),
    Minimo = min(G3),
    Maximo = max(G3)
  )

print(resumen_apoyo)






tabla_resumen <- student %>%
  group_by(schoolsup, famsup) %>%
  summarise(
    Total_Estudiantes = n(),
    Mediana = median(G3, na.rm = TRUE),
    Promedio = round(mean(G3, na.rm = TRUE), 2),
    Desviacion_Estandar = round(sd(G3, na.rm = TRUE), 2),
    .groups = 'drop'
  )

# 4. Renderizar la Tabla Resumen Interactiva
datatable(
  tabla_resumen,
  colnames = c(
    "Apoyo Escolar", 
    "Apoyo Familiar", 
    "cantidad de Estudiantes", 
    "Mediana", 
    "Promedio", 
    "Desviación Estándar"
  ))
  options = list(
    pageLength = 5,       # Cantidad de filas visibles por defecto
    dom = 't',            # Muestra solo la tabla (puedes cambiarlo a 'lfrtip' si deseas buscador)
    initComplete = JS(    # Estilo personalizado para las cabeceras
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#2c3e50', 'color': '#fff'});",
      "}"
    )
  )
