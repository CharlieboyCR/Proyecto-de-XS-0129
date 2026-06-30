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
        "Brecha digital en la educación", 
        tabName = "gráfico_2"
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
        tabName = "gráfico_2",
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
          plotOutput("grafico")
        )
      )
    )
  )
)

  
  
# Servidor
server = function(input, output){
  output$grafico <- renderPlot ({
    if(input$tipoGrafico == "Visual") {
    boxplot(
      base$G3~base$internet,
      main = "Brecha en la educación",
      xlab = "Acceso a Internet",
      ylab = "Valor",
      col = "grey"
    ) } else {
      
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
      TCrit <- qt(1-0.05/2, df = gl)
      
      z = seq(-4,4, length.out = 1000)
      datosg2 <- data.frame(z = z, y = dt(z, df = gl))
      ggplot(datosg2, aes(x=z, y=y))+
        geom_line(size=1)+
        geom_area(data = subset(datosg2, z <= -TCrit), fill = "red")+
        geom_area(data = subset(datosg2, z >= TCrit), fill = "red")+
        geom_point(aes(x=TObs, y = dt(TObs, df = gl)), size = 2)
      
      
      }
  
  })
}

shinyApp(ui = ui, server = server)
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

