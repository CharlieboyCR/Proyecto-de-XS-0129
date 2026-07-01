# Librerias

library(shiny)
library(shinydashboard)
library(readr)
library(DT)
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
      id = "tabs",
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
      conditionalPanel(
        condition = "input.tabs == 'grafico_3'",
        radioButtons(
          "Selector_madre_padre",
          "Seleccione el progenitor",
          choices = c(
            "Madre",
            "Padre"
          )
        ),
        
        radioButtons(
          "Selector_genero",
          "Seleccione el género",
          choices = c(
            "Ambos",
            "Hombres",
            "Mujeres"
          ),
          selected = "Ambos"
        )
      ),
      menuItem(
        "Apoyo educativo",
        tabName = "apoyo_educativo"
      )
      # Nota: Se eliminó el menú de "Parámetros editables" de aquí
    )
  ),
  dashboardBody(
    tags$style(HTML(
      "table.dataTable{font-size:16px}")
      ),
    
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
        tabName = "grafico_3",
    fluidRow(
        box(width = 4,
        h2("Aspiraciones de educación superior vs. educación parental"),
        p(style = "font-size:20px", "El siguiente gráfico de barras muestra las proporciones de los estudiantes si tienen intención de cursar sus estudios superiores y cómo se relacionan dependiendo del nivel de estudio alcanzado por sus padres. Esto permite identificar posibles asociaciones entre la educación parental y las aspiraciones académicas de los estudiantes.")
        ),
      box(width = 8,
          align = "center",
          plotOutput("grafico_3", height = "500px")
        )
      ),
    fluidRow(
      box(style = "text-align:center",
        width = 12,
        h3("Tabla resumen de frecuencias y proporciones según nivel educativo parental"),
        p(style = "font-size:18px", "Como complemento al gráfico, se presenta a continuación una tabla de frecuencias absolutas y relativas de los estudiantes que desean o no continuar con estudios superiores, según cada nivel educativo alcanzado por la madre o del padre. Esta información facilita la interpretación de las proporciones observadas en el gráfico:"),
        DTOutput("tabla_resumen_3")
            )
          )
      )
    )
  )
)
tema_grafico <- theme_classic() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 12)
  )

niveles <- c(
  "0" = "Sin\neducación",
  "1" = "Primaria",
  "2" = "5°-9°",
  "3" = "Secundaria",
  "4" = "Superior"
)

colores <- c(
  "yes"="#2C7FB8",
  "no"="#BDBDBD"
)

grafico_base <- list(
  geom_bar(position="fill"),
  scale_x_discrete(labels=niveles),
  scale_fill_manual(
    values=colores,
    labels=c("Sí","No"),
    name="¿Desea cursar estudios superiores?"
  ),
  tema_grafico
)

#server original

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
  
  output$grafico_3 <- renderPlot({
    
    if(input$Selector_madre_padre == "Madre"){
      
      if (input$Selector_genero == "Hombres") {
        
        ggplot(base |> filter(sex == "M"), aes(x = factor(Medu), fill = higher)) +
          
          grafico_base +
          
          labs(x= "Nivel educativo de la madre", y = "Proporción de estudiantes")
        
        
      } else if (input$Selector_genero == "Mujeres") {
        
        ggplot(base |> filter(sex == "F"), aes(x = factor(Medu), fill = higher)) +
          
          grafico_base +
          
          labs(x= "Nivel educativo de la madre", y = "Proporción de estudiantes")
        
        
      } else {
        
        ggplot(base, aes(x = factor(Medu), fill = higher)) +
          
          grafico_base +
          
          labs(x= "Nivel educativo de la madre", y = "Proporción de estudiantes")
        
      }
      
    } else {
      
      if (input$Selector_genero == "Hombres") {
        
        ggplot(base |> filter(sex == "M"), aes(x = factor(Fedu), fill = higher)) +
          
          grafico_base +
          
          labs(x= "Nivel educativo del padre", y = "Proporción de estudiantes")
        
        
      } else if (input$Selector_genero == "Mujeres"){
        
        ggplot(base |> filter(sex == "F"), aes(x = factor(Fedu), fill = higher)) +
          
          grafico_base +
          
          labs(x= "Nivel educativo del padre", y = "Proporción de estudiantes")
        
        
      } else {
        
        ggplot(base, aes(x = factor(Fedu), fill = higher)) +
          
          grafico_base +
          
          labs(x= "Nivel educativo del padre", y = "Proporción de estudiantes")
        
      }
      
    }
    
  }) ## TABLA 
  output$tabla_resumen_3 <- renderDT({ 
    
    
    Tabla_3 = if(input$Selector_madre_padre == "Madre"){
      
      if (input$Selector_genero == "Hombres") {
        
        base |> 
          filter(base$sex == "M") |>
          group_by(Medu, higher) |>
          summarise(
            Frecuencia = n(),
            .groups = "drop"
          ) |>
          group_by(Medu) |>
          mutate(
            Proporcion = round(Frecuencia/sum(Frecuencia)*100,2)
          )
      } else if(input$Selector_genero == "Mujeres") {
        base |> 
          filter(base$sex == "F") |>
          group_by(Medu, higher) |>
          summarise(
            Frecuencia = n(),
            .groups = "drop"
          ) |>
          group_by(Medu) |>
          mutate(
            Proporcion = round(Frecuencia/sum(Frecuencia)*100,2)
          )
        
      } else {
        base |> 
          group_by(Medu, higher) |>
          summarise(
            Frecuencia = n(),
            .groups = "drop"
          ) |>
          group_by(Medu) |>
          mutate(
            Proporcion = round(Frecuencia/sum(Frecuencia)*100,2)
          )
      }
      
    } else {
      
      if (input$Selector_genero == "Hombres"){
        base |> 
          filter(base$sex == "M") |>
          group_by(Fedu, higher) |>
          summarise(
            Frecuencia = n(),
            .groups = "drop"
          ) |>
          group_by(Fedu) |>
          mutate(
            Proporcion = round(Frecuencia/sum(Frecuencia)*100,2)
          )
      } else if(input$Selector_genero == "Mujeres") {
        
        base |> 
          filter(base$sex == "F") |>
          group_by(Fedu, higher) |>
          summarise(
            Frecuencia = n(),
            .groups = "drop"
          ) |>
          group_by(Fedu) |>
          mutate(
            Proporcion = round(Frecuencia/sum(Frecuencia)*100,2)
          )
      } else {
        base |>
          group_by(Fedu, higher) |>
          summarise(
            Frecuencia = n(),
            .groups = "drop"
          ) |>
          group_by(Fedu) |>
          mutate(
            Proporcion = round(Frecuencia/sum(Frecuencia)*100,2)
          )
      }
    }
    
    datatable(
      Tabla_3,
      rownames = FALSE,
      options = list(
        dom = "t",
        paging = FALSE,
        searching = FALSE,
        ordering = FALSE,
        info = FALSE,
        autoWidth = TRUE
      )
    )
  })
    
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
