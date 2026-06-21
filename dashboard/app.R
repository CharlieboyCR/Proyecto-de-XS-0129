# Librerias 

library(shiny)
library(shinydashboard)
library(tidyverse)
library(readr)

base <- read_delim("https://github.com/CharlieboyCR/Proyecto-de-XS-0129/blob/51bf4cb8a55c1f6955c89ff7b6e49d0271cc317a/student-por.csv",
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

grafico <- ggplot(base, aes(x = Medu, fill = higher)) + # Nota: hacer interactivo para cambiar entre padre y madre 
  geom_bar(position = "fill")+
  labs(x= "nivel eduvactivo de la madre", y= "Proporcion")+
  scale_fill_manual(
    values = c("yes" = "darkgreen",
               "no" = "red"))

grafico

grafico
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
    
  )
)
  
  
# Servidor
server = function(input, output){
  
}

shinyApp(ui = ui, server = server)
