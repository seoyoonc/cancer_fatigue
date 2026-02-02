# this file is for preprocessing, identifying and relabeling key variables,
# checking correlations, and saving complete data for sensitivity analyses
# ==============================================================================

library(here)
library(tidyverse)
library(psych)

data <- read.csv(here("data", "1_raw", "raw_dataset.csv"))

# Checking unique subjects and visits
cat("Unique subjects:", length(unique(data$subject)), "\n")
cat("Unique visits:", length(unique(data$visit)), "\n")
cat("Expected total rows (subjects x visits):", 
    length(unique(data$subject)) * length(unique(data$visit)), "\n\n")

# Group x visit distribution
cat("\nGROUP x VISIT\n")
table(data$group, data$visit, useNA = "always")

# -------------------------------------------------
# Missingness by variable
missing_count <- colSums(is.na(data))
missing_perc <- round(missing_count / nrow(data) * 100, 2)

missing_summary <- rbind(
  n_missing = missing_count,
  percent_missing = missing_perc
)

# print(missing_summary)

# -------------------------------------------------
# Key variables

# MFSI subscales (w/out total)
mfsi_vars <- c("MFSIgen", "MFSIphys", "MFSIemot", "MFSIment", "MFSIvigor")

# Immune markers
immune_vars <- c("IL1b", "IL6", "TNF", "lgEBV", "lgCMV")

# Covariates
covariates <- c("age", "sex", "BMI", 
                "Smoker", "ModExerciseMinWk", "InsomniaScore", 
                "AlcWk", "Comorbid", "CancerTx")

# -------------------------------------------------
data_relabeled <- data %>%
  # Labeling visit
  mutate(
    visit = factor(visit, 
                   levels = c(1, 2, 3), 
                   labels = c("Baseline", "6 months", "18 months"))
  ) %>%
  
  # Create binary smoker variable in case
  mutate(smoker_binary = ifelse(Smoker == "yes", 1, 0))

# -------------------------------------------------
# Correlations for inflammatory composite (relevant for Aim 2)
immune_data <- data_relabeled %>% 
  select(immune_vars[1:3]) %>%
  drop_na()

cat("\nIMMUNE MARKER CORRELATIONS\n")
print(cor(immune_data))

---------------------------------------------------
# Save cleaned datasets

# Relabeled dataset for reference
write.csv(data_relabeled,
          file = here("data", "2_processed", "relabeleded.csv"),
          row.names = FALSE)

# ---------------------------------------------------
# Participants with completed visits - for sensitivity analyses

# subjects_complete_visits <- data_clean %>%
#   group_by(subject) %>%
#   summarise(n_visits = n_distinct(visit)) %>%
#   filter(n_visits == 3) %>%
#   pull(subject)
# 
# cat("Subjects with all 3 visits:", length(subjects_complete_visits), "\n")
# cat("Subjects with incomplete visits:", 
#     length(unique(data_clean$subject)) - length(subjects_complete_visits), "\n")
# 
# # Create complete-case dataset
# data_complete <- data_clean %>%
#   filter(subject %in% subjects_complete_visits)
# 
# # Save dataset for participants with complete visits
# write.csv(data_complete,
#           file = here("data", "2_processed", "relabeleded_complete.csv"),
#           row.names = FALSE)