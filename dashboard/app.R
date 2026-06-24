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
        h2("Aspiraciones de educación superior vs. educación parental"),
        p("Lorem, ipsum dolor sit amet consectetur adipisicing elit. Magni ratione rem, odit dignissimos, laboriosam iure perferendis commodi reprehenderit, officiis ullam similique! Corporis, quaerat? Neque quae excepturi dolorem blanditiis magnam recusandae?"),
        radioButtons(
          inputId = "Selector_madre_padre", label = "Seleccionar familiar", inline = TRUE,
          choices = c(
            "Madre" = "Madre", "Padre" = "Padre"
          )
        ),
        plotOutput("grafico_3"),
        p("Lorem, ipsum dolor sit amet consectetur adipisicing elit. Magni ratione rem, odit dignissimos, laboriosam iure perferendis commodi reprehenderit, officiis ullam similique! Corporis, quaerat? Neque quae excepturi dolorem blanditiis magnam recusandae?")
      )
    )
  )
)
  
  
# Servidor
server = function(input, output){
  
  # NOTA: Este es el gráfico inicial
  #output$grafico_3 = renderPlot({
    #ggplot(base, aes(x = Medu, fill = higher)) +
      #geom_bar(position = "fill")+
      #labs(x= "nivel eduvactivo de la madre", y= "Proporcion")+
      #scale_fill_manual(
        #values = c("yes" = "darkgreen",
                   #"no" = "red"))
    
    output$grafico_3 <- renderPlot({
      
      if(input$Selector_madre_padre == "Madre"){
        ggplot(base, aes(x = Medu, fill = higher)) +
          geom_bar(position = "fill")+
          labs(x= "Nivel educativo de la madre")+
          scale_fill_manual(values = c("yes" = "darkgreen",
                     "no" = "red"))
          
        
      } else {
        ggplot(base, aes(x = Fedu, fill = higher)) +
          geom_bar(position = "fill")+
          labs(x= "Nivel educativo del padre")+
          scale_fill_manual(values = c("yes" = "darkgreen",
                     "no" = "red"))
        
      }
      
    })
  }


shinyApp(ui = ui, server = server)

