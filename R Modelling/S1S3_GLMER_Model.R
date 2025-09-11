# # Load required libraries
# #install.packages(c("dplyr", "readr", "lme4"))
library(dplyr)
library(readr)
library(lme4)

# Load the combined dataset
df <- read_csv("S1_combined_cleaned_windowed.csv")

# Filter rows: remove missing Label and remove '1back', '2back'
df_filtered <- df %>%
  filter(!is.na(Label) & !(Label %in% c("1back", "2back")))

# Map labels into binary: 'drowsy' vs 'alert'
df_filtered <- df_filtered %>%
  mutate(binary_label = ifelse(Label %in% c("slightly", "moderately", "very"), "drowsy", "alert")) %>%
  mutate(binary_label = as.factor(binary_label))

# Select predictors
predictors <- c("ECG_HR", "GSR_mean", "GSR_std","laneDev_std", "speed_mean", "speed_std", "swAngle_SWRR")

# Subset dataset and include ID as random effect grouping factor
df_model <- df_filtered %>%
  select(all_of(predictors), binary_label, ID) %>%
  na.omit()

# Fit GLMM with ID as a random intercept
glmer_model <- glmer(binary_label ~ ECG_HR + GSR_mean + GSR_std + laneDev_std + speed_std + speed_mean + swAngle_SWRR + (1 | ID),
                     data = df_model,
                     family = binomial)

# glmer_model <- glmer(binary_label ~ ECG_HR + GSR_std + laneDev_std + speed_mean + (1 | ID),
#                      data = df_model,
#                      family = binomial)

# Show model summary
summary(glmer_model)

# ==============================
# VIF ANALYSIS (via logistic glm)
# ==============================

# Fit regular logistic model for VIF analysis (no random effects)
glm_model <- glm(binary_label ~ ECG_HR + GSR_mean + GSR_std + laneDev_std +
                   speed_mean + speed_std + swAngle_SWRR,
                 data = df_model,
                 family = binomial)

# Calculate VIFs
vif_values <- vif(glm_model)
print(vif_values)

# 
# residuals <- resid(glmer_model)
# std_residuals <- residuals / sd(residuals)
# fitted_values <- fitted(glmer_model)
# 
# 
# plot(fitted_values, std_residuals, 
#      xlab = "Fitted Values", 
#      ylab = "Residuals", 
#      main = "Residuals vs Fitted Values: Combined S1 Data")
# abline(h = 0, col = "red", lwd = 2)  # Add a horizontal line at 0

# install.packages("car")

library(dplyr)
library(readr)
library(ggplot2)
library(broom)

# --- Load and clean data ---
df <- read_csv("S1_combined_cleaned_windowed.csv") %>%
  filter(!is.na(Label), !(Label %in% c("1back", "2back"))) %>%
  mutate(Label = factor(Label))  # ensure it's a factor

# --- Select predictors and remove NA ---
predictors <- c("ECG_HR", "GSR_mean", "GSR_std",
                "laneDev_std", "speed_mean", "speed_std", "swAngle_SWRR")

df_model <- df %>%
  select(all_of(predictors), Label) %>%
  na.omit()

# --- One-vs-Rest Logistic Regression ---
label_levels <- levels(df_model$Label)

results_list <- lapply(label_levels, function(lvl) {
  df_bin <- df_model %>%
    mutate(binary_label = ifelse(Label == lvl, 1, 0))  # 1-vs-rest
  
  model <- glm(binary_label ~ ., data = df_bin %>% select(-Label), family = binomial)
  
  # Get tidy summary with p-values
  tidy(model) %>%
    mutate(label = lvl)
})

# Combine all results
all_results <- bind_rows(results_list)

# --- View Summary Table ---
print(all_results)

# Optional: Filter only significant predictors (p < 0.05)
sig_results <- all_results %>%
  filter(term != "(Intercept)", p.value < 0.05)

print(sig_results)