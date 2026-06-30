# Librerias 

library(shiny)
library(shinydashboard)
library(tidyverse)
library(readr)

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
        p(style = "font-size:20px", "El siguiente gráfico de barras permite explorar las proporciones de los estudiantes si tienen intencion de cursar sus estudios superiores y como se relacionan dependiendo del nivel de estudio alcanzado por sus padres. Las barras muestran la proporción de estudiantes que planena continuar con eduación superior para cada nivel educativo alcanzado por sus padres. Esto permite identificar posibles asociaciones entre la educación parental y las aspiraciones académicas de los estudiantes"),
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
      box(width = 12,
        h3("Podemos llegar a las siguientes concluciones viendo el gráfico de barras:"),
        p(""))
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
            
            scale_fill_manual(values = c("yes" = "darkgreen", "no" = "red"),
                              labels = c("Sí", "No"), name = "¿Desea cursar estudios superiores?") +
            theme(
              legend.position = "bottom"
            )
            
          
        } else if (input$Selector_genero == "Mujeres") {
          
          ggplot(base |> filter(sex == "F"), aes(x = Medu, fill = higher)) +
            
            geom_bar(position = "fill")+
            
            
            labs(x= "Nivel educativo de la madre", y = "proporción")+
            
            scale_fill_manual(values = c("yes" = "darkgreen", "no" = "red"),
                              labels = c("Sí", "No"), name = "¿Desea cursar estudios superiores?") +
            theme(
              legend.position = "bottom"
            )
          
        } else {
          
          ggplot(base, aes(x = Medu, fill = higher)) +
            
            geom_bar(position = "fill")+
            
            
            labs(x= "Nivel educativo de la madre", y = "proporción")+
            
            scale_fill_manual(values = c("yes" = "darkgreen", "no" = "red"),
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
              
              scale_fill_manual(values = c("yes" = "darkgreen", "no" = "red"),
                                labels = c("Sí", "No"), name = "¿Desea cursar estudios superiores?") +
              theme(
                legend.position = "bottom"
              )
            
            
          } else if (input$Selector_genero == "Mujeres"){
            
            ggplot(base |> filter(sex == "F"), aes(x = Fedu, fill = higher)) +
              
              geom_bar(position = "fill")+
              
              
              labs(x= "Nivel educativo de la madre", y = "proporción")+
              
              scale_fill_manual(values = c("yes" = "darkgreen", "no" = "red"),
                                labels = c("Sí", "No"), name = "¿Desea cursar estudios superiores?") +
              theme(
                legend.position = "bottom"
              )

            
          } else {
            ggplot(base, aes(x = Fedu, fill = higher)) +
              
              geom_bar(position = "fill")+
              
              
              labs(x= "Nivel educativo de la madre", y = "proporción")+
              
              scale_fill_manual(values = c("yes" = "darkgreen", "no" = "red"),
                                labels = c("Sí", "No"), name = "¿Desea cursar estudios superiores?") +
              theme(
                legend.position = "bottom",
              )
            
      }
      
  }
})
}

shinyApp(ui = ui, server = server)

