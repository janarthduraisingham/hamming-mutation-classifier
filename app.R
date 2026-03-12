library(shiny)
library(bslib)

ui <- fluidPage(
  
  titlePanel("Hamming Distance Mutation Classifier")
  
)

server <- function(input, output) {
  
  test_sequence <- 'AAAAAAAA'
           
  reference_sequence <- 'ATGCATGC'
}

shinyApp(ui = ui,
         server = server)

