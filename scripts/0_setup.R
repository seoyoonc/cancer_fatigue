# convert original spss dataset -> csv format for future analyses
# check basic data structure
# ==============================================================================

library(here)
library(haven)
library(tidyverse)

# Load SPSS dataset
raw_data <- haven::read_sav(file = here("data", "1_raw", "raw_dataset.sav"))

# Check structure
cat("Data dimensions:", dim(raw_data)[1], "rows x", dim(raw_data)[2], "columns\n")
print(names(raw_data))
# str(raw_data)

# Convert to R dataframe
raw_clean <- raw_data %>%
  haven::as_factor() %>%  # Convert labeled values to factors
  as.data.frame() %>%
  select(-VAR00002, -VAR00003)

# Save as CSV
write.csv(raw_clean, 
          file = here("data", "1_raw", "raw_dataset.csv"),
          row.names = FALSE
)
