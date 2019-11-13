# Chapter 18: Model Basics with modelr

# Started Jan. 11, 2019
# By Roxanne Ready

# Load packages
#install.packages(c("magrittr"))
library(tidyverse)
library(modelr)
options(na.action = na.warn)

# SHORTCUTS
# cmd-shft-m:     %>% 
# cmd-shft-c:     comment line/block
# cmd-opt-up/dn:  duplicate line
# cmd-rtrn:       run code line/block
# cmd-opt-e:      run code until EoF
# cmd-shft-r:     insert section break

# \


# # Basics ----------------------------------------------------------------

?sim1

# Plot a simple simulaed dataset
ggplot(sim1, aes(x, y)) +
  geom_point()

models <- tibble(
  a1 = runif(250, -20, 40),
  a2 = runif(250, -5, 5)
)

ggplot(sim1, aes(x, y)) +
  # Randomly generate models from a similar family, overlayed on the data
  geom_abline(
    aes(intercept = a1, slope = a2),
    data = models,
    alpha = 1/4
  ) +
  geom_point()




