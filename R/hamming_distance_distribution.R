library(tidyverse)

n = 8
p = 0.9
x = seq(1:n)
y = dbinom(x,
           size = n,
           prob = p)
df = data.frame(list(mutations = x, probability = y))

ggplot(df, aes(x = mutations, y = probability)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_classic() +
  labs(title = "Hamming dist dist",
       subtitle = "subtitle",
       x = "Hamming distance",
       y = "Probability")
