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
  
  layout_columns(
  card(
    
    card_header("Results intuitively phrased")
    
  ),
  
  card(
    card_header("Results"),
    
    textOutput("run_complete"),
    textOutput("test_sequence"),
    textOutput("reference_sequence"),
    textOutput("distribution"),
    textOutput("single_mutation_probability"),
    textOutput("h0"),
    textOutput("h1"),
    textOutput("p_value_message"),
    textOutput("conclusion")
  )
  )
)

server <- function(input, output) {
  
  source("R/hamming_distance.R")
  source("R/hypothesis_test.R")
  source("R/p_value.R")
  
  reactive_values <- reactiveValues(p_value = '',
                                    run_complete = '',
                                    hamming_distance_distribution = '',
                                    single_mutation_probability = '',
                                    h0 = '',
                                    h1 ='',
                                    test_sequence = '',
                                    reference_seqeunce = '',
                                    conclusion = '')
  
  test_sequence <- 'AAAAAAAA'
           
  reference_sequence <- 'ATGCATGC'
  
  distribution = 'binomial'
  single_mutation_probability = 0.1
  
  # Print messages
  output$test_sequence = renderText(paste0("Test sequence: ", reactive_values$test_sequence))
  output$run_complete = renderText(reactive_values$run_complete)
  output$p_value_message = renderText(reactive_values$p_value)
  output$distribution = renderText(reactive_values$hamming_distance_distribution)
  output$single_mutation_probability = renderText(reactive_values$single_mutation_probability)
  output$test_sequence = renderText(reactive_values$test_sequence)
  output$reference_sequence = renderText(reactive_values$reference_sequence)
  output$h0 = renderText(reactive_values$h0)
  output$h1 = renderText(reactive_values$h1)
  output$conclusion = renderText(reactive_values$conclusion)
  
  # Run Button
  observeEvent(input$run,
               {
               
               # Run hypothesis test
               p_value = hypothesis_test(test_sequence = test_sequence,
                                         reference_sequence = reference_sequence,
                                         hamming_distance_function = hamming_distance,
                                         distribution = distribution,
                                         single_mutation_probability = single_mutation_probability)
               
               if (p_value < 0.05) {
                 conclusion = 'Reject'
               } else {'Do not reject'}
               
               
               # Messages
               reactive_values$run_complete = 'Hypothesis test complete'
               reactive_values$p_value = paste0("p value: ", format(p_value, scientific = F))
               reactive_values$hamming_distance_distribution = paste0("Hamming distance distribution: ", distribution)
               reactive_values$single_mutation_probability = paste0("Null Hypothesis single independent mutation probability: ", single_mutation_probability)
               reactive_values$test_sequence = paste0("Test sequence: ", test_sequence)
               reactive_values$reference_sequence = paste0("Reference sequence: ", reference_sequence)
               reactive_values$h0 = paste0("Null Hypothesis: The Hamming distance between the test sequence and the reference sequence follows a ", distribution, " distribution, with single independent mutation probability, ", single_mutation_probability)
               reactive_values$h1 = paste0("Alternative Hypothesis: The Hamming distance between the test sequence and the reference sequence does not follow a ", distribution, " distribution, with single independent mutation probability, ", single_mutation_probability)
               reactive_values$conclusion = paste0("Conclusion: ", conclusion, " the Null Hypothesis")
               
               
               }
  )
  
}

shinyApp(ui = ui,
         server = server)

