# Load required libraries
library(dplyr)
library(readr)

# Load dataset
df <- read_csv("P5_S1_windowed_cleaned.csv")

# Filter rows: remove missing Label and remove '1back', '2back'
df_filtered <- df %>%
  filter(!is.na(Label) & !(Label %in% c("1back", "2back")))

# Map labels into binary: 'drowsy' vs 'alert'
df_filtered <- df_filtered %>%
  mutate(binary_label = ifelse(Label %in% c("slightly", "moderately", "very"), "drowsy", "alert")) %>%
  mutate(binary_label = as.factor(binary_label))

# --------- Model 1: Physiological signals only ---------
physio_vars <- c("ECG_HR", "GSR_mean", "GSR_std")

df_physio <- df_filtered %>%
  select(all_of(physio_vars), binary_label) %>%
  na.omit()

glm_physio <- glm(binary_label ~ ., data = df_physio, family = binomial)
cat("\n===== Physiological Model Summary =====\n")
summary(glm_physio)

# --------- Model 2: Vehicle data only ---------
vehicle_vars <- c("swAngle_SWRR", "laneDev_std", "speed_std", "speed_mean")

df_vehicle <- df_filtered %>%
  select(all_of(vehicle_vars), binary_label) %>%
  na.omit()

glm_vehicle <- glm(binary_label ~ ., data = df_vehicle, family = binomial)
cat("\n===== Vehicle Model Summary =====\n")
summary(glm_vehicle)