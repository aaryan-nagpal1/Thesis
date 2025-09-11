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
predictors <- c("ECG_HR", "GSR_mean", "laneDev_std", "speed_std", "swAngle_SWRR")

# Subset dataset for model
df_model <- df_filtered %>%
  select(all_of(predictors), binary_label) %>%
  na.omit()

# Fit logistic regression model
glm_model <- glm(binary_label ~ ., data = df_model, family = binomial)

# Show model summary
summary(glm_model)
