# shiny app using bslib
library(shiny)
library(bslib)

ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  titlePanel("Análisis de prensa: Presidenciales 2025"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("concepto", "Concepto:", value = "delincuencia"),
      actionButton("buscar", "Buscar")
    ),
    
    mainPanel(
      tableOutput("resultados")
    )
  )
)
server <- function(input, output) {

}
shinyApp(ui = ui, server = server)