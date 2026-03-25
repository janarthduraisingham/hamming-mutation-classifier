library(tidyverse)

hamming_distance_p_value_plotter <- function(trials,
                                                  single_mutation_probability,
                                                  threshold){
  
  x = seq(0, trials)
  y = pbinom(x,
             size = trials,
             prob = single_mutation_probability)
  
  df = data.frame(list(mutations = x, probability = 1-y))
  
  ggplot(df, aes(x = mutations + 1, y = probability)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    theme_classic() +
    labs(title = "Probability that Hamming Distance >= x (p value)",
         subtitle = paste0("Trials (length of sequence): ", trials, "\nSingle mutation probability: ", single_mutation_probability),
         x = "Hamming distance (Number of mutations)",
         y = "1 - Cumulative Probability") +
    scale_x_continuous(breaks = seq(0, trials, by = 1)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.05)) +
    geom_hline(yintercept = 1-threshold, color = "red")
}



  
  
