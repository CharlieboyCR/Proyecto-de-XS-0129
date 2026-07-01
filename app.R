# Librerias 

library(shiny)
library(shinydashboard)
library(tidyverse)
library(readr)
library(DT)
# Dashboard
base <- read_delim("student-por.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Dashbase# Dashboard
ui <- dashboardPage(title = "Proyecto Shiny", 
                    skin = "yellow",
  dashboardHeader(
    title = "Proyecto test"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem(
        "Estudio y rendimiento académico"
      ),
      menuItem(
        "Brecha digital en la educación"
      ),
      menuItem(
        "Educación parental", tabName = "grafico_3"
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
    tags$style(HTML(
      "table.dataTable{font-size:16px}")
      ),
    
    tabItems(
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

# Servidor
server = function(input, output){

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
}
shinyApp(ui = ui, server = server)

