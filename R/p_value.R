# Function to get p value of hamming distance based on distribution
p_value <- function(distribution = 'binomial',
                    test_statistic,
                    sequence_length,
                    single_mutation_probability) {
  
  if (distribution == 'binomial') {
    
    p_value = pbinom(q = test_statistic - 1,
                     size = sequence_length,
                     prob = single_mutation_probability,
                     lower.tail = FALSE)
    
  }
  
  return(p_value)
  
}