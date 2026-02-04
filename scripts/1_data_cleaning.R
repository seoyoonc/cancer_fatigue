# this script is for identifying key variables, checking correlations, 
# and saving complete data for sensitivity analyses
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
table(data$group, data$visit, useNA = "always")

# -------------------------------------------------
# Missingness by variable
missing_count <- colSums(is.na(data))
missing_perc <- round(missing_count / nrow(data) * 100, 2)

missing_summary <- rbind(
  n_missing = missing_count,
  percent_missing = missing_perc
)

# Retention by group
# First match controls by cancer type
data_with_groups <- data %>%
  mutate(
    cancer_controls = case_when(
      group == "control" & subjecttype == "breast" ~ "Breast-matched Control",
      group == "control" & subjecttype == "colorectal" ~ "CRC-matched Control",
      group == "cancer" & subjecttype == "breast" ~ "BC",
      group == "cancer" & subjecttype == "colorectal" ~ "CRC",
      TRUE ~ NA_character_
    )
  )

# table(data_with_groups$cancer_controls, useNA = "always")

retention <- data_with_groups %>%
  group_by(cancer_controls, visit) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(cancer_controls) %>%
  mutate(
    baseline_n = n[visit == 1],  # N at baseline (visit 1)
    retention_rate = n / baseline_n,  # Proportion retained
    retention_pct = round(retention_rate * 100, 1),  # Percentage
    n_lost = baseline_n - n  # Number lost since baseline
  ) %>%
  ungroup()

# -------------------------------------------------
# Relabeling smoker variable to binary

data_relabeled <- data %>%
  mutate(smoker_binary = ifelse(Smoker == "yes", 1, 0))

# Relabeled dataset for reference
write.csv(data_relabeled,
          file = here("data", "2_processed", "relabeled.csv"),
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