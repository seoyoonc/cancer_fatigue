# this script is for the first part of aim 1: hlm analyses.
# ==============================================================================

library(here)
library(tidyverse)
library(lme4)
library(lmerTest) # p-values for HLM
library(emmeans) # adjusted means/contrasts

data <- read.csv(here("data", "2_processed", "relabeled.csv")) %>%
  mutate(
    visit = factor(visit, levels = c(1, 2, 3), labels = c("Baseline", "6mo", "18mo")),
    cancer_group = factor(case_when(
      group == "control" ~ "Control",
      group == "cancer" & subjecttype == "breast" ~ "BC",
      group == "cancer" & subjecttype == "colorectal" ~ "CRC"
    ), levels = c("Control", "BC", "CRC")),
    sex = factor(sex),
    subject = factor(subject),
    time_numeric = as.numeric(visit) - 1,
    
    # Controls should be labeled as 'none' for treatment type
    CancerTx_clean = factor(ifelse(is.na(CancerTx), "None", as.character(CancerTx)))
  )

# Define variables
mfsi_vars <- c("MFSIgen", "MFSIphys", "MFSIemot", "MFSIment", "MFSIvigor", "MFSItot")
mfsi_sscales <- c("General", "Physical", "Emotional", "Mental", "Vigor", "Total")

covariates <- "age + sex + BMI + smoker_binary + ModExerciseMinWk + InsomniaScore + AlcWk + Comorbid + CancerTx_clean"

# HLMs
models <- map2(mfsi_vars, mfsi_sscales, ~{
  formula <- as.formula(paste(.x, "~ cancer_group * visit +", covariates, "+ (1 + time_numeric | subject)"))
  lmer(formula, data = data, REML = TRUE)
}) %>% set_names(mfsi_sscales)

# summary(models$Total)  # Total fatigue model
# summary(models$General)  # General fatigue model

# -------------------------------------------------
# Get marginal means and pairwise contrasts
results <- map(mfsi_sscales, ~{
  
  # Marginal means
  emm <- emmeans(models[[.x]], 
                 ~ cancer_group | visit,
                 cov.reduce = mean)
  
  # Pairwise contrasts at each visit
  contrasts <- contrast(emm, method = "pairwise")
  
  list(emmeans = as.data.frame(emm) %>% mutate(outcome = .x),
       contrasts = as.data.frame(contrasts) %>% mutate(outcome = .x))
}) %>% set_names(mfsi_sscales)

# Combine
all_emmeans <- map_df(results, "emmeans")
all_contrasts <- map_df(results, "contrasts")

# View significant contrasts (move to Markdown)
sig_contrasts <- all_contrasts %>% 
  filter(p.value < 0.05) %>%
  select(outcome, visit, contrast, estimate, p.value) %>%
  arrange(outcome, visit)

print(sig_contrasts)

# Check significant interactions for ALL subscales
for(subscale in mfsi_sscales) {
  int_coefs <- summary(models[[subscale]])$coefficients %>%
    as.data.frame() %>%
    rownames_to_column("term") %>%
    filter(grepl("cancer_group.*:.*visit", term), `Pr(>|t|)` < 0.05)
  
  if(nrow(int_coefs) > 0) {
    cat("\n", subscale, " - Significant Interactions:\n", sep = "")
    print(int_coefs %>% select(term, Estimate, `Pr(>|t|)`))
  }
}

# -------------------------------------------------
# Visualize trajectories
for(subscale in mfsi_sscales[1:5]) {
  indiv_plots <- ggplot(plot_data %>% filter(outcome == subscale), 
              aes(x = visit, y = emmean, color = cancer_group, group = cancer_group)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15, size = 0.75) +
    scale_color_manual(values = c("Control" = "#838B8B", "BC" = "#CD1076", "CRC" = "#1874CD")) +
    labs(title = paste(subscale, "Fatigue Trajectory"), 
         x = "Study Visit", y = "Adjusted Mean Score", color = "Group") +
    theme_classic()
  
  # Save
  ggsave(here("output", "figures", paste0("aim1_hlm_", subscale, ".png")),
         plot = indiv_plots,
         width = 8, height = 6, dpi = 300)
}