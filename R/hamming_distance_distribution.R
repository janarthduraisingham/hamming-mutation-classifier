library(tidyverse)

hamming_distance_distribution_plotter <- function(trials,
                                                  single_mutation_probability){
  
  x = seq(0, trials)
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
         y = "Probability") +
    scale_x_continuous(breaks = seq(0, trials, by = 1)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.05))
}



  
  
