library(tidyverse)

# Function to take two sequences and return hamming distance
hamming_distance <- function(seq_1, seq_2) {
  
  # Convert string to list of elements
  list_seq_1 <- seq_1 %>%
    str_split('')
  
  list_seq_2 <- seq_2 %>%
    str_split('')
  
  comparison = pmap(list(list_seq_1,
                      list_seq_2),
                    ~ .x == .y)
  
  hamming_distance = sum(unlist(comparison))
  
  return(hamming_distance)
  
}
