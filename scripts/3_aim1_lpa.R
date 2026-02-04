# this script is for the second part of aim 1: lpa analyses.
# ==============================================================================

library(here)
library(tidyverse)


# Correlations for inflammatory composite (relevant for Aim 2)
# Need to first log transform variables
immune_data <- data_relabeled %>% 
  select(immune_vars[1:3]) %>%
  drop_na()

# print(cor(immune_data))