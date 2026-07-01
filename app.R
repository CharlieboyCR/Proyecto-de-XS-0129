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
      menuItem(
        "Estudio y rendimiento académico"
      ),
      menuItem(
        "Brecha digital en la educación"
      ),
      menuItem(
        "Educación parental", tabName = "grafico_3"
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
        box(width = 12,
        h2("Aspiraciones de educación superior vs. educación parental"),
        p(style = "font-size:20px", "El siguiente gráfico de barras permite explorar las proporciones de los estudiantes si tienen intencion de cursar sus estudios superiores y como se relacionan dependiendo del nivel de estudio alcanzado por sus padres. Las barras muestran la proporción de estudiantes que desean o no continuar con eduación superior para cada nivel educativo alcanzado por sus padres. Esto permite identificar posibles asociaciones entre la educación parental y las aspiraciones académicas de los estudiantes"),
        
        div(style = "font-size:20px; text-align:center",
            
          radioButtons(
            inputId = "Selector_madre_padre", 
            label = "Escoja uno de los padres para ser representado en el gráfico",
            choices = c(
              "Madre" = "Madre", "Padre" = "Padre")
          ),
          
          radioButtons(
            inputId = "Selector_genero",
            label = "Seleccione un genero",
            choices = c(
              "Ambos" = "Ambos",
              "Hombres" = "Hombres",
              "Mujeres" = "Mujeres"
            )
          )
        )
      )
    ),
    fluidRow(
      box(width = 12,
          align = "center",
          plotOutput("grafico_3", width = "800px", height = "500px")
      ),
    fluidRow(
      box(style = "text-align:center",
        width = 12,
        h3("Tabla de frecuencias de educación parental"),
        p(style = "font-size:18px", "Como complemento al gráfico, se presenta a continuación una tabla de frecuencias absolutas y relativas para observar y comprender lo que sucede con cada nivel de educación del madre o la padre hacia el estudiante referente si desea continuar o no con sus estududios superiores:"),
        DTOutput("tabla_resumen_3"),
            )
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
          
          labs(x= "Nivel educativo de la madre", y = "proporción")
          
        
      } else if (input$Selector_genero == "Mujeres") {
        
        ggplot(base |> filter(sex == "F"), aes(x = factor(Medu), fill = higher)) +
          
          grafico_base +
          
          labs(x= "Nivel educativo de la madre", y = "proporción")
          
        
      } else {
        
        ggplot(base, aes(x = factor(Medu), fill = higher)) +
          
          grafico_base +
          
          labs(x= "Nivel educativo de la madre", y = "proporción")
          
      }
      
    } else {
      
      if (input$Selector_genero == "Hombres") {
        
        ggplot(base |> filter(sex == "M"), aes(x = factor(Fedu), fill = higher)) +
          
          grafico_base +
          
          labs(x= "Nivel educativo del padre", y = "proporción")
          
        
      } else if (input$Selector_genero == "Mujeres"){
        
        ggplot(base |> filter(sex == "F"), aes(x = factor(Fedu), fill = higher)) +
          
          grafico_base +
          
          labs(x= "Nivel educativo del padre", y = "proporción")
        
        
      } else {
        
        ggplot(base, aes(x = factor(Fedu), fill = higher)) +
          
          grafico_base +
          
          labs(x= "Nivel educativo del padre", y = "proporción")
        
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
            Frecuencia = n()
          ) |>
            group_by(Medu) |>
            mutate(
              Proporcion = round(Frecuencia/sum(Frecuencia)*100,3)
            )
        } else if(input$Selector_genero == "Mujeres") {
          base |> 
            filter(base$sex == "F") |>
            group_by(Medu, higher) |>
            summarise(
              Frecuencia = n()
            ) |>
            group_by(Medu) |>
            mutate(
              Proporcion = round(Frecuencia/sum(Frecuencia)*100,3)
            )
            
        } else {
          base |> 
            group_by(Medu, higher) |>
            summarise(
              Frecuencia = n()
            ) |>
            group_by(Medu) |>
            mutate(
              Proporcion = round(Frecuencia/sum(Frecuencia)*100,3)
            )
        }
          
      } else {
        
        if (input$Selector_genero == "Hombres"){
          base |> 
            filter(base$sex == "M") |>
            group_by(Fedu, higher) |>
            summarise(
              Frecuencia = n()
            ) |>
            group_by(Fedu) |>
            mutate(
              Proporcion = round(Frecuencia/sum(Frecuencia)*100,3)
          )
        } else if(input$Selector_genero == "Mujeres") {
          
          base |> 
            filter(base$sex == "F") |>
            group_by(Fedu, higher) |>
            summarise(
              Frecuencia = n()
            ) |>
            group_by(Fedu) |>
            mutate(
              Proporcion = round(Frecuencia/sum(Frecuencia)*100,3)
            )
        } else {
          base |>
            group_by(Fedu, higher) |>
            summarise(
              Frecuencia = n()
            ) |>
            group_by(Fedu) |>
            mutate(
              Proporcion = round(Frecuencia/sum(Frecuencia)*100,3)
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

