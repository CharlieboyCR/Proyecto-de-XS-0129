# Librerias 

library(shiny)
library(shinydashboard)
library(tidyverse)
library(readr)
library(ggplot2)

base <- read_delim("~/Proyecto-de-XS-0129/student-por.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(base)
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
        "Brecha digital en la educación", 
        tabName = "gráfico_2"
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
