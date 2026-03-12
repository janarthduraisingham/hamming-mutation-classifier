library(shiny)
library(bslib)

ui <- page_fluid()

server <- function(input, output) {
  
  test_sequence <- 'AAAAAAAA'
           
  reference_sequence <- 'ATGCATGC'
}

shinyApp(ui = ui,
         server = server)

