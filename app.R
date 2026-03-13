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
    
  ),
  
  card(
    card_header("Results"),
    
    textOutput("run_complete"),
    textOutput("test_sequence")
  )
)

server <- function(input, output) {
  
  source("R/hamming_distance.R")
  source("R/hypothesis_test.R")
  source("R/p_value.R")
  
  reactive_values <- reactiveValues(run_complete = '')
  
  reactive_values$test_sequence <- 'AAAAAAAA'
           
  reactive_values$reference_sequence <- 'ATGCATGC'
  
  #reactive_values$run_complete <- ''
  
  # Initialise print messages
  output$test_sequence = renderText(paste0("Test sequence: ", reactive_values$test_sequence))
  output$run_complete = renderText(paste0(reactive_values$run_complete))
  
  
  # Run Button
  observeEvent(input$run,
               {
               # Run hypothesis test
               hypothesis_test(test_sequence = reactive_values$test_sequence,
                  reference_sequence = reactive_values$reference_sequence,
                  hamming_distance_function = hamming_distance)
               
               # Print message
               reactive_values$run_complete = 'Hypothesis test complete'
               
               }
  )
  
}

shinyApp(ui = ui,
         server = server)

