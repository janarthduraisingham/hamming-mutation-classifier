# Perform hypothesis test

hypothesis_test <- function(test_sequence,
                            reference_sequence,
                            hamming_distance_function,
                            p_value_function = p_value,
                            distribution = 'binomial',
                            single_mutation_probability = 0.1,
                            sig_level) {
  
  cat("Assumptions:\n")
  cat("\tThe probability of mutation at a given position is constant\n\tand independent of the mutation / non-mutation of other positions in the sequence.\n\n")
  
  cat("Parameters:\n")
  cat("\tSequence length:\n")
  cat("\t\t", nchar(test_sequence), "\n\n")
  cat("\tTest sequence:\n\t\t", test_sequence, "\n\n")
  cat("\tReference sequence:\n")
  cat("\t\t", reference_sequence, "\n\n")
  
  cat("NULL HYPOTHESIS\n")
  cat("\tHamming distance from reference sequence follows a", distribution, " distribution\n")
  cat("\twith independent probability of single-position mutation:", single_mutation_probability, "\n\n")
  
  cat("ALTERNATIVE HYPOTHESIS\n")
  cat("\tHamming distance from reference sequence does not follow a", distribution, " distribution\n")
  cat("\twith independent probability of single-position mutation:", single_mutation_probability, "\n\n")
  
  hamming_distance = hamming_distance_function(test_sequence,
                                               reference_sequence)
  
  p_value = p_value_function(distribution = distribution,
                             test_statistic = hamming_distance,
                             sequence_length = nchar(test_sequence),
                             single_mutation_probability = single_mutation_probability)
  
  cat("Under the Null Hypothesis, the probability of an observation at least as extreme as this:\n")
  cat("\t", p_value, "\n\n")
  
  cat("i.e. p value:\n")
  cat("\t", p_value, "\n\n")
  
  if (p_value < sig_level) {
    
    conclusion = "Since the proability of observing a result at least extreme as this is less than 5%\nwe reject the null hypothesis"
    
  } else {
    
    conclusion = "Since the probability of observing a result at least extreme as this is not less than 5%\nwe do not reject the null hypothesis"
    
  }
  
  cat("Conclusion:\n")
  cat("\t", conclusion, "\n\n")

  return(list('p_value' = p_value,
              'test_statistic' = hamming_distance))
  
}
