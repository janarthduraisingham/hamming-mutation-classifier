library(tidyverse)

hamming_distance_distribution_plotter <- function(trials,
                                                  single_mutation_probability){
  
  x = seq(1:n)
  y = dbinom(x,
             size = trials,
             prob = single_mutation_probability)
  
  df = data.frame(list(mutations = x, probability = y))
  
  ggplot(df, aes(x = mutations, y = probability)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    theme_classic() +
    labs(title = "Hamming Distance Probability Distribution",
         subtitle = paste0("Trials: ", trials, "\nSingle mutation probability: ", single_mutation_probability),
         x = "Hamming distance",
         y = "Probability")
  
  
}

hamming_distance_distribution_plotter(trials = 8,
                                      single_mutation_probability = 0.1)


  
  
