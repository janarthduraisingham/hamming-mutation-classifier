library(shiny)
library(bslib)
library(tidyverse)

ui <- page_fluid(
  
  titlePanel("Intuitive Hypothesis Testing"),
  
  card(
    
    card_header("Introduction"),
    
    h1("Hypothesis Testing is all about beliefs and extreme observations"),
   
    h2("If an observation is extreme, based on a belief about the world, we might feel compelled to rethink that belief"),
   
    p("\"Google maps' route is taking ages, this can't have been the fastest way\""),
   
    p("\"I don't think this coke is diet\""),
   
    p("\"If my initial belief were true, the probability of seeing something at least as extreme as what I have just seen is so unlikely that I no longer believe my initial belief\""),
    
    h3("But, of course, we can never know for sure")
    
  ),
  
  card(
    
    card_header("The Hypothesis Test"),
    
    h2("Hypothesis testing jargon:"),
    
    h3("Null Hypothesis: the initial belief"),
    h3("Test Statistic: the observation itself"),
    h3("p-value: the probability of seeing something at least that extreme, if the initial belief were true"),
    h3("Significance Level: how unlikely the observation needs to be for us to reject the belief")
    
  ),
  
  card(
    
    card_header("DNA and the Hamming Distance"),
    
    h2("We want to judge whether a given sequence of DNA is a mutated version of a known sequence, or a different sequence completely"),
    
    p("DNA sequence: A literal sequence of A's, T's, G's, and C's, in any order"),
    p("Mutation: When a DNA sequence gets copied, some letters may be copied incorrectly, according to some biological probability"),
    p("Hamming Distance: The number of positions in which two sequences differ"),
    
    h3("The Hamming Distance between ATGC and ATGG is 1"),
    h3("The Hamming Distance between ATGC and ATGC is 0"),
    h3("The Hamming Distance between ATGC and CGTG is 4"),
    
    h2("A large Hamming Distance suggests different DNA, rather than a mutation")
    
    
  ),
  
  card(
    card_header("Setup"),
    
    layout_columns(
    
    textInput("test_sequence",
              "Test Sequence",
              placeholder = "e.g. ATCGATCG"),
    
    textInput("reference_sequence",
              "Reference sequence",
              value = "ATCGATCG")
    
    ),
    
    numericInput("bernoulli_prob",
              "Independent single mutation probability",
              value = 0.1,
              step = 0.1)
    
  ),
  
  card(
    
    card_header("Visualisations"),
    
    plotOutput("hamming_distance_distribution_plot"),
    
    numericInput("sig_level",
                 "Threshold for test statistic to be considered extreme (1 - significance level)",
                 value = 0.95,
                 step = 0.01),
    plotOutput("hamming_distance_cumulative_distribution_plot"),
    plotOutput("hamming_distance_p_value_plot")
  ),
  
  card(
    card_header("Run Hypothesis Test"),
    
    actionButton("run", "Run")
    
  ),
  
  layout_columns(
  card(
    
    card_header("Results intuitively phrased"),
    
    textOutput("run_complete_message_intu"),
    textOutput("test_sequence_message_intu"),
    textOutput("reference_sequence_message_intu"),
    textOutput("distribution_message_intu"),
    textOutput("sequence_length_message_intu"),
    textOutput("single_mutation_probability_message_intu"),
    textOutput("h0_message_intu"),
    textOutput("h1_message_intu"),
    textOutput("test_statistic_message_intu"),
    textOutput("sig_level_message_intu"),
    textOutput("p_value_message_intu"),
    textOutput("conclusion_message_intu")

    
  ),
  
  card(
    card_header("Results"),
    
    textOutput("run_complete_message"),
    textOutput("test_sequence_message"),
    textOutput("reference_sequence_message"),
    textOutput("distribution_message"),
    textOutput("sequence_length_message"),
    textOutput("single_mutation_probability_message"),
    textOutput("h0_message"),
    textOutput("h1_message"),
    textOutput("test_statistic_message"),
    textOutput("sig_level_message"),
    textOutput("p_value_message"),
    textOutput("conclusion_message")
  )
  )
)

server <- function(input, output) {
  
  source("R/hamming_distance.R")
  source("R/hypothesis_test.R")
  source("R/p_value.R")
  source("R/hamming_distance_distribution.R")
  source("R/hamming_distance_cumulative_distribution.R")
  source("R/hamming_distance_p_value_graph.R")
  
  reactive_values <- reactiveValues(run_complete_message = "Click Run button to produce results")
  
  # hard code parameter - perhaps add more distribution options later
  distribution = 'binomial'

  
  ### Visualisations
  # Hammming distance probability distribution
  output$hamming_distance_distribution_plot <- renderPlot({
    hamming_distance_distribution_plotter(trials = nchar(input$reference_sequence),
                                          single_mutation_probability = input$bernoulli_prob)
  })
  
  # Hamming distance cumulative probability distribution
  output$hamming_distance_cumulative_distribution_plot <- renderPlot({
    hamming_distance_cumulative_distribution_plotter(trials = nchar(input$reference_sequence),
                                          single_mutation_probability = input$bernoulli_prob,
                                          threshold = input$sig_level)
    
    
  })
  
  # p value against test statistic bar chart
  output$hamming_distance_p_value_plot <- renderPlot({
    hamming_distance_p_value_plotter(trials = nchar(input$reference_sequence),
                                                     single_mutation_probability = input$bernoulli_prob,
                                                     threshold = input$sig_level)
    
  })
  
  # Print messages to UI
  output$test_sequence_message = renderText(paste0("Test sequence: ", reactive_values$test_sequence))
  output$test_sequence_message_intu = renderText(paste0("Sequence we want to investigate: ", reactive_values$test_sequence))
  
  output$reference_sequence_message = renderText(paste0("Reference sequence: ", reactive_values$reference_sequence))
  output$reference_sequence_message_intu = renderText(paste0("Sequence we want to compare against: ", reactive_values$reference_sequence))
  
  output$run_complete_message = renderText(reactive_values$run_complete_message)
  output$run_complete_message_intu = renderText(reactive_values$run_complete_message)
  
  output$p_value_message = renderText(paste0("p value: ", reactive_values$p_value))
  output$p_value_message_intu = renderText(paste0("Probability of observing a result at least as extreme as the test statistic, if the Null Hypothesis is true: ", reactive_values$p_value))
  
  output$distribution_message = renderText(paste0("Hamming distance distribution: ", reactive_values$hamming_distance_distribution))
  output$distribution_message_intu = renderText(paste0("Hamming distance distribution: ", reactive_values$hamming_distance_distribution))

  
  output$single_mutation_probability_message = renderText(paste0("Null Hypothesis single independent mutation probability: ", reactive_values$single_mutation_probability))
  output$single_mutation_probability_message_intu = renderText(paste0("Null Hypothesis single independent mutation probability: ", reactive_values$single_mutation_probability))
  
  output$h0_message = renderText(paste0("Null Hypothesis: ", reactive_values$h0))
  output$h0_message_intu = renderText(paste0("Initial / current belief: ", reactive_values$h0))
  
  output$h1_message = renderText(paste0("Alternative Hypothesis: ", reactive_values$h1))
  output$h1_message_intu = renderText(paste0("Alternative belief: ", reactive_values$h1_intu))
  
  output$conclusion_message = renderText(paste0("Conclusion: ", reactive_values$conclusion))
  output$conclusion_message_intu = renderText(paste0("Conclusion: ", reactive_values$conclusion))
  
  output$sequence_length_message = renderText(paste0("Sequence length: ", reactive_values$sequence_length))
  output$sequence_length_message_intu = renderText(paste0("Sequence length: ", reactive_values$sequence_length))
  
  
  output$test_statistic_message = renderText(paste0("Test statistic: ", reactive_values$test_statistic))
  output$test_statistic_message_intu = renderText(paste0("Hamming distance (no. of differences): ", reactive_values$test_statistic))
  
  output$sig_level_message = renderText(paste0("Significance level: ", reactive_values$sig_level))
  output$sig_level_message_intu = renderText(paste0("Extremeness threshold to reject initial belief: ", reactive_values$sig_level))
  
  # Run Button
  observeEvent(input$run,
               {
               
               # Run hypothesis test
               hypothesis_test = hypothesis_test(test_sequence = input$test_sequence,
                                         reference_sequence = input$reference_sequence,
                                         hamming_distance_function = hamming_distance,
                                         distribution = distribution,
                                         single_mutation_probability = input$bernoulli_prob,
                                         sig_level = 1-input$sig_level)
               
               p_value = hypothesis_test$p_value
               test_statistic = hypothesis_test$test_statistic
               
               # Compute hypothesis test result
               if (p_value < 1-input$sig_level) {
                 conclusion = 'Reject'
               } else {
                 conclusion = 'Do not reject'
                 }
               
               
               # Load hypothesis test results into reactive variables for printing to UI
               reactive_values$run_complete_message = 'Hypothesis test complete'
               
               reactive_values$p_value = format(p_value, scientific = F)
               
               reactive_values$hamming_distance_distribution = distribution
               
               reactive_values$single_mutation_probability = input$bernoulli_prob
               
               reactive_values$test_sequence = input$test_sequence

               reactive_values$reference_sequence = input$reference_sequence
               
               reactive_values$h0 = paste0("The Hamming distance between the test sequence and the reference sequence follows a ", distribution, " distribution with single independent mutation probability, ", input$bernoulli_prob, ", and ", nchar(input$test_sequence), " trials.")
               
               reactive_values$h1 = paste0("The Hamming distance between the test sequence and the reference sequence does not follow a ", distribution, " distribution with single independent mutation probability, ", input$bernoulli_prob, ", and ", nchar(input$test_sequence), " trials.")
               reactive_values$h1_intu = "The initial / current belief is wrong\n\n"
               
               reactive_values$conclusion = paste0(conclusion, " the Null Hypothesis")
               
               reactive_values$test_statistic = test_statistic
               
               reactive_values$sequence_length = nchar(input$test_sequence)
               
               reactive_values$sig_level = 1 - input$sig_level
               
               }
  )
  
}

shinyApp(ui = ui,
         server = server)

