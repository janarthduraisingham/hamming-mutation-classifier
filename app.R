library(shiny)
library(bslib)

ui <- fluidPage(
  
  titlePanel("Hamming Distance Mutation Classifier")
  
)

server <- function(input, output) {
  
  source("R/hamming_distance.R")
  source("R/hypothesis_test.R")
  source("R/p_value.R")
  
  test_sequence <- 'AAAAAAAA'
           
  reference_sequence <- 'ATGCATGC'
  
  hypothesis_test(test_sequence = 'AAABBBBB',
                  reference_sequence = 'AAAAAAAA',
                  hamming_distance_function = hamming_distance)
  
}

shinyApp(ui = ui,
         server = server)

