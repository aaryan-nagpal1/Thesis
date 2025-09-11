# Load required libraries
library(dplyr)
library(readr)

# Load the dataset
df <- read_csv("P9_S3_windowed_cleaned.csv")

# Filter rows: remove missing Label and remove '1back', '2back'
df_filtered <- df %>%
  filter(!is.na(Label) & !(Label %in% c("1back", "2back")))

# Map labels into binary: 'drowsy' vs 'alert'
df_filtered <- df_filtered %>%
  mutate(binary_label = case_when(
    Label %in% c("Slightly drowsy", "Moderately drowsy", "Very drowsy") ~ "drowsy",
    Label == "Not drowsy" ~ "alert"
  )) %>%
  mutate(binary_label = as.factor(binary_label))

# Select predictors
all_predictors <- c("ECG_HR", "GSR_mean", "laneDev_std", "speed_std", "swAngle_SWRR")

# Subset dataset for modeling
df_model <- df_filtered %>%
  select(all_of(all_predictors), binary_label) %>%
  na.omit()

# ------------------ Null and Full Models ------------------
# Null model (intercept only)
null_model <- glm(binary_label ~ 1, data = df_model, family = binomial)

# Full model (all predictors)
full_model <- glm(binary_label ~ ., data = df_model, family = binomial)

# ------------------ Forward Selection ------------------
forward_model <- step(null_model,
                      scope = list(lower = null_model, upper = full_model),
                      direction = "forward",
                      trace = 1)

cat("\n=== Forward Selection Model Summary ===\n")
summary(forward_model)

# ------------------ Backward Elimination ------------------
backward_model <- step(full_model,
                       direction = "backward",
                       trace = 1)

cat("\n=== Backward Elimination Model Summary ===\n")
summary(backward_model)

# ------------------ Stepwise (Both Directions) ------------------
stepwise_model <- step(null_model,
                       scope = list(lower = null_model, upper = full_model),
                       direction = "both",
                       trace = 1)

cat("\n=== Stepwise Selection Model Summary ===\n")
summary(stepwise_model)
