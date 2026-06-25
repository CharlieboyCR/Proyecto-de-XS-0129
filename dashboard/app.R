# Librerias 

library(shiny)
library(shinydashboard)
library(tidyverse)
library(readr)

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
        p("Lorem ipsum dolor sit amet consectetur adipiscing elit torquent dis est, imperdiet potenti porttitor suspendisse per ultrices nullam morbi suscipit curabitur, lectus conubia nec felis mollis tellus etiam vitae parturient. Consequat in eget himenaeos nascetur fermentum mollis tempor pretium, vulputate justo porta congue ornare condimentum cras nullam, tellus natoque nisi felis laoreet semper lectus. Purus aptent nibh tempor iaculis class egestas platea dui auctor et, vestibulum ad sem nascetur sociosqu faucibus tellus est habitant, odio phasellus montes sagittis duis pellentesque justo ac himenaeos.

Libero euismod facilisi bibendum non nullam elementum porta lobortis, dui aliquet morbi mattis ligula commodo cras, penatibus hac et maecenas vel integer tristique. Proin velit fermentum congue augue pretium magna leo platea dictumst aliquet vestibulum montes aliquam et mi, morbi at fames ad taciti ante vulputate fusce hac maecenas mauris nunc donec. Bibendum proin sociis mattis nec eleifend fames eget dignissim risus at elementum, eros massa feugiat et porttitor aptent pellentesque ornare purus hac, venenatis accumsan duis suscipit fringilla mollis vivamus convallis egestas volutpat."))
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
        
        ggplot(base, aes(x = Medu, fill = higher)) +
          
          geom_bar(position = "fill")+
          
          labs(x= "Nivel educativo de la madre", y = "proporción")+
          
          scale_fill_manual(values = c("yes" = "darkgreen", "no" = "red"))
          
        
      } else {
        ggplot(base, aes(x = Fedu, fill = higher)) +
          
          geom_bar(position = "fill")+
          
          labs(x= "Nivel educativo del padre", y = "proporción")+
          
          scale_fill_manual(values = c("yes" = "darkgreen", "no" = "red"))
        
      }
      
    })
  }


shinyApp(ui = ui, server = server)

