library(shiny)
library(bslib)

ui <- page_fluid(
  
  titlePanel("Hamming Distance Mutation Classifier"),
  
  card(
    card_header("Setup")
  ),
  
  card(
    card_header("Run Hypothesis Test"),
    
    actionButton("run", "Run")
    
  )
)

server <- function(input, output) {
  
  source("R/hamming_distance.R")
  source("R/hypothesis_test.R")
  source("R/p_value.R")
  
  test_sequence <- 'AAAAAAAA'
           
  reference_sequence <- 'ATGCATGC'
  
  observeEvent(input$run,
               hypothesis_test(test_sequence = 'AAABBBBB',
                  reference_sequence = 'AAAAAAAA',
                  hamming_distance_function = hamming_distance)
  )
  
}

shinyApp(ui = ui,
         server = server)

