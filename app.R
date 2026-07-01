# Librerias

library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(ggplot2)
base <- read_delim("student-por.csv", 
                   delim = ";", escape_double = FALSE, trim_ws = TRUE)

# UI del Dashboard
ui <- dashboardPage(
  title = "Análisis del rendimiento en escuelas de portugal", 
  skin = "yellow",
  
  dashboardHeader(
    title = "Análisis del rendimiento en escuelas de portugal"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Rendimiento académico y asistencias", 
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
      )
      # Nota: Se eliminó el menú de "Parámetros editables" de aquí
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # 1. Pestaña Estudio
      tabItem(tabName = "estudio",
              h2("Rendimiento académico y asistencias a clases"),
              fluidRow(
                box(
                  plotOutput("plot_1", height = 500),
                  width = 12
                )
              )),
      
      # 2. Pestaña Brecha Digital
      tabItem(
        tabName = "grafico_2", 
        h2("Brecha digital en la educación"),
        box(
          width = 12,
          radioButtons(
            inputId = "tipoGrafico",
            label = "Tipo Análisis",
            choices = c("Análisis Formal" = "Formal", "Análisis Visual" = "Visual"),
            selected = "Visual",
            inline = TRUE
          ),
          conditionalPanel(
            condition = "input.tipoGrafico == 'Formal'",
            checkboxInput(
              inputId = "Limpio",
              label = "Eliminar los valores extremos"
            ), 
            sliderInput(
              inputId = "alpha",
              label = "Nivel de significancia",
              min = 0.01,
              max = 0.2,
              value = 0.05
            )
          ),
          plotOutput("grafico")
        )
      ),
      
      # 3. Pestaña Educación Parental
      tabItem(tabName = "grafico_3",
              h2("Educación parental"),
              p("Contenido en desarrollo...")
      ),
      
      # 4. Pestaña Apoyo Educativo (MODIFICADA)
      tabItem(tabName = "apoyo_educativo",
              fluidRow(
                # NUEVO: Caja para los parámetros de apoyo
                box(
                  title = "Parámetros",
                  width = 3, # Toma 3 de las 12 columnas disponibles
                  status = "warning",
                  solidHeader = TRUE,
                  radioButtons("variable_apoyo", 
                               label = "Tipo de apoyo:",
                               choices = c("Apoyo escolar" = "schoolsup",
                                           "Apoyo familiar" = "famsup"),
                               selected = "schoolsup")
                ),
                
                # Caja principal con los resultados
                box(
                  title = "Análisis de calificaciones de Portugués", 
                  width = 9, # Toma las 9 columnas restantes
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
      )})
    
  output$grafico <- renderPlot ({
    if(input$tipoGrafico == "Visual") {
        boxplot(
        base$G3~base$internet,
        main = "Brecha en la educación",
        xlab = "Acceso a Internet",
        ylab = "Valor",
        col = "grey"
      )} else {
        if(input$Limpio){
        Datos_Limpios2 <- base %>% 
          mutate(Q1 = quantile(base$G3, 0.25) ,
                 Q3 = quantile(base$G3, 0.75),
                 IQR = Q3-Q1,
                 lim_inf = Q1- IQR *1.5 , 
                 lim_sup =Q3 + IQR *1.5) %>% 
          filter(between(G3, lim_inf, lim_sup))
        grupo1 <- Datos_Limpios2 %>% 
          filter(internet == "yes")
        grupo2 <- Datos_Limpios2 %>% 
          filter(internet == "no")
        
        n1 <- nrow(grupo1)
        n2 <- nrow(grupo2)
        
        x1 <- mean(grupo1$G3)
        x2 <- mean(grupo2$G3)
        
        s1 <- sd(grupo1$G3)
        s2 <- sd(grupo2$G3)
        
        sp <- sqrt(((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2))
        gl= length(Datos_Limpios2$G3)-2
        
        TObs <- (x1-x2)/(sp*sqrt(1/n1+1/n2))
        TCrit <- qt(1-input$alpha/2, df = gl)
        
        z = seq(-4,4, length.out = 1000)
        datosg2 <- data.frame(z = z, y = dt(z, df = gl))
        ggplot(datosg2, aes(x=z, y=y))+
          geom_line(size=1)+
          geom_area(data = subset(datosg2, z <= -TCrit), fill = "red")+
          geom_area(data = subset(datosg2, z >= TCrit), fill = "red")+
          geom_point(aes(x=TObs, y = dt(TObs, df = gl)), size = 2)
        
        
        } else {
      grupo1 <- base %>% 
        filter(internet == "yes")
      grupo2 <- base %>% 
        filter(internet == "no")
        
      n1 <- nrow(grupo1)
      n2 <- nrow(grupo2)
      
      x1 <- mean(grupo1$G3)
      x2 <- mean(grupo2$G3)
      
      s1 <- sd(grupo1$G3)
      s2 <- sd(grupo2$G3)
      
      sp <- sqrt(((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2))
      gl= length(base$G3)-2
      
      TObs <- (x1-x2)/(sp*sqrt(1/n1+1/n2))
      TCrit <- qt(1-input$alpha/2, df = gl)
      
      z = seq(-4,4, length.out = 1000)
      datosg2 <- data.frame(z = z, y = dt(z, df = gl))
      ggplot(datosg2, aes(x=z, y=y))+
        geom_line(size=1)+
        geom_area(data = subset(datosg2, z <= -TCrit), fill = "red")+
        geom_area(data = subset(datosg2, z >= TCrit), fill = "red")+
        geom_point(aes(x=TObs, y = dt(TObs, df = gl)), size = 2)
      
        }
      }
  
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
  
  #Gráfico de dispersión
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
