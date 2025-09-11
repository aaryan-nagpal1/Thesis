# Load required libraries
library(dplyr)
library(readr)

# Load the dataset
df <- read_csv("Windowed and Cleaned Data/P8_S1_windowed_interpolated.csv")

# Filter rows: remove missing Label and remove '1back', '2back'
df_filtered <- df %>%
  filter(!is.na(Label) & !(Label %in% c("1back", "2back")))

# Map labels into binary: 'drowsy' vs 'alert'
df_filtered <- df_filtered %>%
  mutate(binary_label = ifelse(Label %in% c("slightly", "moderately", "very"), "drowsy", "alert")) %>%
  mutate(binary_label = as.factor(binary_label))

# Select predictors
predictors <- c("ECG_HR", "GSR_mean", "GSR_std", "laneDev_std", "speed_mean", "speed_std", "swAngle_SWRR")
# predictors <- c("ECG_HR", "GSR_mean", "GSR_std", "laneDev_std", "speed_mean", "speed_std")

# Subset dataset for model
df_model <- df_filtered %>%
  select(all_of(predictors), binary_label) %>%
  na.omit()

# Fit logistic regression model
glm_model <- glm(binary_label ~ ., data = df_model, family = binomial)

# Show model summary
summary(glm_model)
# 
# # Power Transformation of predictors
# library(car)
# df_model[predictors] <- lapply(df_model[predictors], function(x) {
#   if (is.numeric(x)) {
#     return(powerTransform(x))
#   }
#   return(x)
# })

# library(dplyr)
# library(readr)
# library(car)
# 
# # --- load & prepare (your code unchanged up to df_model) ---
# df <- read_csv("P5_S1_windowed_cleaned.csv")
# 
# df_filtered <- df %>%
#   filter(!is.na(Label) & !(Label %in% c("1back", "2back"))) %>%
#   mutate(binary_label = ifelse(Label %in% c("slightly", "moderately", "very"), "drowsy", "alert"),
#          binary_label = factor(binary_label))
# 
# predictors <- c("ECG_HR", "GSR_mean", "GSR_std", "laneDev_std", "speed_mean", "speed_std", "swAngle_SWRR")
# 
# df_model <- df_filtered %>%
#   select(all_of(predictors), binary_label) %>%
#   na.omit()
# 
# # --- Box–Cox per predictor (handle zeros with shift), then refit GLM ---
# eps <- 1e-6
# lambdas <- numeric(length(predictors))
# names(lambdas) <- predictors
# shifts  <- numeric(length(predictors))
# names(shifts) <- predictors
# 
# for (col in predictors) {
#   x <- df_model[[col]]
#   # ensure strictly positive by shifting if needed
#   shift <- if (min(x, na.rm = TRUE) <= 0) abs(min(x, na.rm = TRUE)) + eps else 0
#   x_pos <- x + shift
#   # estimate lambda on this variable
#   pt <- powerTransform(x_pos ~ 1)      # ~1 is fine for a single variable
#   lam <- pt$lambda
#   # apply transform
#   df_model[[col]] <- bcPower(x_pos, lam)
#   
#   # keep for reference
#   lambdas[col] <- lam
#   shifts[col]  <- shift
# }
# 
# cat("Estimated Box–Cox lambdas:\n")
# print(lambdas)
# cat("Applied shifts (to make variables > 0):\n")
# print(shifts)
# 
# # fit GLM on transformed predictors
# glm_bc <- glm(binary_label ~ ., data = df_model, family = binomial)
# summary(glm_bc)

