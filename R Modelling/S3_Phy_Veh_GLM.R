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

# ------------------ Physiological Model ------------------
physio_predictors <- c("ECG_HR", "GSR_mean")

df_physio <- df_filtered %>%
  select(all_of(physio_predictors), binary_label) %>%
  na.omit()

glm_physio <- glm(binary_label ~ ., data = df_physio, family = binomial)

cat("\n=== Physiological Model Summary ===\n")
summary(glm_physio)

# ------------------ Vehicle Model ------------------
vehicle_predictors <- c("swAngle_SWRR", "laneDev_std", "speed_std")

df_vehicle <- df_filtered %>%
  select(all_of(vehicle_predictors), binary_label) %>%
  na.omit()

glm_vehicle <- glm(binary_label ~ ., data = df_vehicle, family = binomial)

cat("\n=== Vehicle Model Summary ===\n")
summary(glm_vehicle)