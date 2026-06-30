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
    tabItems(
      tabItem(
        tabName = "grafico_3",
        fluidRow(
        box(width = 12,
        h2("Aspiraciones de educación superior vs. educación parental"),
        p(style = "font-size:20px", "El siguiente gráfico de barras permite explorar las proporciones de los estudiantes si tienen intencion de cursar sus estudios superiores y como se relacionan dependiendo del nivel de estudio alcanzado por sus padres. Las barras muestran la proporción de estudiantes que desean o no continuar con eduación superior para cada nivel educativo alcanzado por sus padres. Esto permite identificar posibles asociaciones entre la educación parental y las aspiraciones académicas de los estudiantes"),
        p(style = "font-size:20px", "Los niveles de educación alcanzada por los padres, son los siguientes:"),
        div(style = "font-size:18px",
          p("0: No posee educación"),
          p("1: Educación primeria (hasta 4to grado) completada"),
          p("2: De 5to grado a 9no grado completos"),
          p("3: educación secundaria completada"),
          p("4: educación superior completada")
        ),
        
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
        h3("A partir del gráfico, visualmente se puede inferir"),
        uiOutput("interpretacion_3"),
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
# Servidor
server = function(input, output){

    output$grafico_3 <- renderPlot({
      
      if(input$Selector_madre_padre == "Madre"){
        
        if (input$Selector_genero == "Hombres") {
          
          ggplot(base |> filter(sex == "M"), aes(x = Medu, fill = higher)) +
            
            geom_bar(position = "fill") +
            
            labs(x= "Nivel educativo de la madre", y = "proporción")+
            
            scale_fill_manual(values = c("yes" = "#2C7FB8", "no" = "#BDBDBD"),
                              labels = c("Sí", "No"), name = "¿Desea cursar estudios superiores?") +
            theme(
              legend.position = "bottom"
            )
            
          
        } else if (input$Selector_genero == "Mujeres") {
          
          ggplot(base |> filter(sex == "F"), aes(x = Medu, fill = higher)) +
            
            geom_bar(position = "fill")+
            
            
            labs(x= "Nivel educativo de la madre", y = "proporción")+
            
            scale_fill_manual(values = c("yes" = "#2C7FB8", "no" = "#BDBDBD"),
                              labels = c("Sí", "No"), name = "¿Desea cursar estudios superiores?") +
            theme(
              legend.position = "bottom"
            )
          
        } else {
          
          ggplot(base, aes(x = Medu, fill = higher)) +
            
            geom_bar(position = "fill")+
            
            
            labs(x= "Nivel educativo de la madre", y = "proporción")+
            
            scale_fill_manual(values = c("yes" = "#2C7FB8", "no" = "#BDBDBD"),
                              labels = c("Sí", "No"), name = "¿Desea cursar estudios superiores?") +
            theme(
              legend.position = "bottom"
            )
        }
        
      } else {
          
          if (input$Selector_genero == "Hombres") {
            
            ggplot(base |> filter(sex == "M"), aes(x = Fedu, fill = higher)) +
              
              geom_bar(position = "fill")+
              
              labs(x= "Nivel educativo de la madre", y = "proporción")+
              
              scale_fill_manual(values = c("yes" = "#2C7FB8", "no" = "#BDBDBD"),
                                labels = c("Sí", "No"), name = "¿Desea cursar estudios superiores?") +
              theme(
                legend.position = "bottom"
              )
            
            
          } else if (input$Selector_genero == "Mujeres"){
            
            ggplot(base |> filter(sex == "F"), aes(x = Fedu, fill = higher)) +
              
              geom_bar(position = "fill")+
              
              
              labs(x= "Nivel educativo de la madre", y = "proporción")+
              
              scale_fill_manual(values = c("yes" = "#2C7FB8", "no" = "#BDBDBD"),
                                labels = c("Sí", "No"), name = "¿Desea cursar estudios superiores?") +
              theme(
                legend.position = "bottom"
              )

            
          } else {
            
            ggplot(base, aes(x = Fedu, fill = higher)) +
              
              geom_bar(position = "fill")+
              
              
              labs(x= "Nivel educativo de la madre", y = "proporción")+
              
              scale_fill_manual(values = c("yes" = "#2C7FB8", "no" = "#BDBDBD"),
                                labels = c("Sí", "No"), name = "¿Desea cursar estudios superiores?") +
              theme(
                legend.position = "bottom",
              )
            
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
    
    output$interpretacion_3 <- renderUI({
      
      if(input$Selector_madre_padre == "Madre"){
        
        if(input$Selector_genero == "Hombres"){
          
          p(
            style = "font-size:18px;",
            "Entre los estudiantes hombres, el gráfico muestra la relación entre el nivel educativo de la madre y la intención de cursar estudios superiores, se observa lo siguiente:
            1)
            "
          )
          
        } else if(input$Selector_genero == "Mujeres"){
          
          p(
            style = "font-size:18px;",
            "Entre las estudiantes mujeres, el gráfico muestra la relación entre el nivel educativo de la madre y la intención de cursar estudios superiores."
          )
          
        } else {
          
          p(
            style = "font-size:18px;",
            "Entre todos los estudiantes, el gráfico muestra la relación entre el nivel educativo de la madre y la intención de cursar estudios superiores."
          )
          
        }
        
      } else {
        
        if(input$Selector_genero == "Hombres"){
          
          p(
            style = "font-size:18px;",
            "Entre los hombres, el gráfico muestra la relación entre el nivel educativo del padre y la intención de cursar estudios superiores."
          )
          
        } else if(input$Selector_genero == "Mujeres"){
          
          p(
            style = "font-size:18px;",
            "Entre las mujeres, el gráfico muestra la relación entre el nivel educativo del padre y la intención de cursar estudios superiores."
          )
          
        } else {
          
          p(
            style = "font-size:18px;",
            "Considerando ambos géneros, el gráfico muestra la relación entre el nivel educativo del padre y la intención de cursar estudios superiores."
          )
          
        }
        
      }
      
    })
}

shinyApp(ui = ui, server = server)

