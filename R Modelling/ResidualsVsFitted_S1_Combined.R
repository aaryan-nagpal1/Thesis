# # Packages
# # install.packages("ggplot2")
# # install.packages("tidyr")
# library(tidyr)
# library(dplyr)
# library(readr)
# library(lme4)
# library(ggplot2)
# library(car)  # for Box–Cox transformation
# 
# # Load and filter
# df <- read_csv("/Users/aaryannagpal1/Documents/Thesis/Classification_Combined_Data/S1_combined_cleaned_windowed.csv") %>%
#   filter(!is.na(Label), !(Label %in% c("1back","2back"))) %>%
#   mutate(binary_label = factor(ifelse(Label %in% c("slightly", "moderately", "very"), "drowsy", "alert")))
# 
# # Set predictors
# predictors <- c("ECG_HR", "GSR_mean", "GSR_std", "laneDev_std", "speed_mean", "speed_std", "swAngle_SWRR")
# 
# # Keep relevant data and drop NAs
# df_model <- df %>%
#   select(all_of(predictors), binary_label, ID) %>%
#   na.omit()
# 
# # --- Apply Box–Cox Transformations ---
# df_trans <- df_model
# eps <- 1e-6
# lambdas <- numeric(length(predictors))
# names(lambdas) <- predictors
# shifts <- numeric(length(predictors))
# 
# for (col in predictors) {
#   x <- df_model[[col]]
#   shift <- if (min(x) <= 0) abs(min(x)) + eps else 0
#   x_pos <- x + shift
# 
#   pt <- powerTransform(x_pos ~ 1)
#   lam <- pt$lambda
# 
#   df_trans[[col]] <- bcPower(x_pos, lam)
#   lambdas[col] <- lam
#   shifts[col] <- shift
# }
# 
# cat("\n--- Box–Cox Lambdas ---\n")
# print(lambdas)
# cat("\n--- Applied Shifts ---\n")
# print(shifts)
# 
# # --- Standardize Transformed Predictors ---
# df_trans[predictors] <- scale(df_trans[predictors])
# 
# # --- Fit LMMs: transformed_predictor ~ binary_label + (1 | ID) ---
# models <- lapply(predictors, function(y) {
#   form <- as.formula(paste(y, "~ binary_label + (1 | ID)"))
#   lmer(form, data = df_trans, REML = TRUE)
# })
# names(models) <- predictors
# 
# # --- Residuals vs Fitted Plots ---
# diag_df <- do.call(rbind, lapply(names(models), function(nm){
#   m <- models[[nm]]
#   data.frame(
#     predictor = nm,
#     fitted    = fitted(m),
#     std_resid = resid(m) / sigma(m),
#     stringsAsFactors = FALSE
#   )
# }))
# 
# ggplot(diag_df, aes(fitted, std_resid)) +
#   geom_point(alpha = 0.5, size = 1) +
#   geom_hline(yintercept = 0, color = "red") +
#   facet_wrap(~ predictor, scales = "free_x") +
#   labs(
#     title = "Residuals vs Fitted by Predictor\n(Box–Cox Transformed + LMM: predictor ~ binary_label + (1 | ID))",
#     x = "Fitted values",
#     y = "Standardized residuals"
#   ) +
#   theme_bw()
# 
# # --- QQ Plots for Residuals ---
# qq_df <- do.call(rbind, lapply(names(models), function(nm){
#   m <- models[[nm]]
#   res <- resid(m) / sigma(m)
#   data.frame(predictor = nm, std_resid = res)
# }))
# 
# ggplot(qq_df, aes(sample = std_resid)) +
#   stat_qq(alpha = 0.5, size = 1) +
#   stat_qq_line(color = "red") +
#   facet_wrap(~ predictor, scales = "free") +
#   labs(
#     title = "Normal QQ Plots of Standardized Residuals\n(Box–Cox Transformed + LMM)",
#     x = "Theoretical Quantiles",
#     y = "Sample Quantiles"
#   ) +
#   theme_bw()
# 
# # --- Boxplots: Each predictor by binary_label (alert vs drowsy) ---
# df_long <- df_model %>%
#   select(all_of(predictors), binary_label) %>%
#   pivot_longer(cols = all_of(predictors), names_to = "predictor", values_to = "value")
# 
# ggplot(df_long, aes(x = binary_label, y = value, fill = binary_label)) +
#   geom_boxplot(outlier.shape = NA, alpha = 0.7) +
#   facet_wrap(~ predictor, scales = "free_y") +
#   labs(
#     title = "Boxplots of Predictors by Label",
#     x = "Label",
#     y = "Transformed & Scaled Value"
#   ) +
#   theme_bw() +
#   theme(legend.position = "none")
# 
# # Plot violin plots
# # Step 1: Reshape to long format WITHOUT scaling
# df_long_raw <- df_model %>%
#   select(all_of(predictors), binary_label) %>%
#   pivot_longer(cols = all_of(predictors), names_to = "Predictor", values_to = "Value")
# 
# # Step 2: Plot
# ggplot(df_long_raw, aes(x = binary_label, y = Value, fill = binary_label)) +
#   geom_violin(trim = FALSE, alpha = 0.7) +
#   geom_boxplot(width = 0.1, outlier.shape = NA, color = "black") +
#   facet_wrap(~ Predictor, scales = "free_y", ncol = 3) +
#   labs(title = "Density Box Plots of Raw Predictor Values by Label",
#        x = "Label",
#        y = "Raw Value") +
#   scale_fill_manual(values = c("alert" = "#FF9999", "drowsy" = "#00BFC4")) +
#   theme_minimal(base_size = 14) +
#   theme(legend.position = "none",
#         strip.text = element_text(face = "bold"))
# 
# # Normalize predictors for visualization
# df_scaled <- df_model %>%
#   select(all_of(predictors), binary_label) %>%
#   mutate(across(all_of(predictors), ~ scale(.)[,1])) %>%
#   pivot_longer(cols = all_of(predictors), names_to = "Predictor", values_to = "Value")
# 
# ggplot(df_scaled, aes(x = binary_label, y = Value, fill = binary_label)) +
#   geom_violin(trim = FALSE, alpha = 0.7) +
#   geom_boxplot(width = 0.1, outlier.shape = NA, color = "black") +
#   facet_wrap(~ Predictor, scales = "free_y", ncol = 3) +
#   labs(title = "Density Box Plots of Predictors by Label",
#        x = "Label",
#        y = "Standardized Value") +
#   scale_fill_manual(values = c("alert" = "#FF9999", "drowsy" = "#00BFC4")) +
#   theme_minimal(base_size = 14) +
#   theme(legend.position = "none",
#         strip.text = element_text(face = "bold"))


# Packages
# install.packages("ggplot2")
# install.packages("tidyr")
library(tidyr)
library(dplyr)
library(readr)
library(lme4)
library(ggplot2)
library(car)  # for Box–Cox transformation

# Load and filter
df <- read_csv("/Users/aaryannagpal1/Documents/Thesis/Classification_Combined_Data/S1_combined_cleaned_windowed.csv") %>%
  filter(!is.na(Label), !(Label %in% c("1back","2back"))) %>%
  mutate(binary_label = factor(ifelse(Label %in% c("slightly", "moderately", "very"), "drowsy", "alert")))

# Set predictors
predictors <- c("ECG_HR", "GSR_mean", "GSR_std", "laneDev_std", "speed_mean", "speed_std", "swAngle_SWRR")

# Pretty names for plots/tables
pretty_names <- c(
  ECG_HR       = "Heart Rate",
  GSR_mean     = "Avg GSR",
  speed_mean   = "Avg Speed",
  swAngle_SWRR = "SWRR",
  GSR_std      = "Std GSR",
  laneDev_std  = "SDLP",
  speed_std    = "Std Speed"
)

predictor_levels <- unname(pretty_names[predictors])

# Keep relevant data and drop NAs
df_model <- df %>%
  select(all_of(predictors), binary_label, ID) %>%
  na.omit()

# --- Apply Box–Cox Transformations ---
df_trans <- df_model
eps <- 1e-6
lambdas <- numeric(length(predictors))
names(lambdas) <- predictors
shifts <- numeric(length(predictors))
names(shifts) <- predictors

for (col in predictors) {
  x <- df_model[[col]]
  shift <- if (min(x) <= 0) abs(min(x)) + eps else 0
  x_pos <- x + shift

  pt <- powerTransform(x_pos ~ 1)
  lam <- pt$lambda

  df_trans[[col]] <- bcPower(x_pos, lam)
  lambdas[col] <- lam
  shifts[col] <- shift
}

cat("\n--- Box–Cox Lambdas ---\n")
print(lambdas)
cat("\n--- Applied Shifts ---\n")
print(shifts)

# --- Standardize Transformed Predictors ---
df_trans[predictors] <- scale(df_trans[predictors])

# --- Fit LMMs: transformed_predictor ~ binary_label + (1 | ID) ---
models <- lapply(predictors, function(y) {
  form <- as.formula(paste(y, "~ binary_label + (1 | ID)"))
  lmer(form, data = df_trans, REML = TRUE)
})
names(models) <- predictors

# --- Residuals vs Fitted Plots ---
diag_df <- do.call(rbind, lapply(names(models), function(nm){
  m <- models[[nm]]
  data.frame(
    predictor = factor(pretty_names[nm], levels = predictor_levels),
    fitted    = fitted(m),
    std_resid = resid(m) / sigma(m),
    stringsAsFactors = FALSE
  )
}))

ggplot(diag_df, aes(fitted, std_resid)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_hline(yintercept = 0, color = "red") +
  facet_wrap(~ predictor, scales = "free_x") +
  labs(
    title = "Residuals vs Fitted by Predictor\n(Box–Cox Transformed + LMM: predictor ~ binary_label + (1 | ID))",
    x = "Fitted values",
    y = "Standardized residuals"
  ) +
  theme_bw()

# --- QQ Plots for Residuals ---
qq_df <- do.call(rbind, lapply(names(models), function(nm){
  m <- models[[nm]]
  res <- resid(m) / sigma(m)
  data.frame(
    predictor = factor(pretty_names[nm], levels = predictor_levels),
    std_resid = res,
    stringsAsFactors = FALSE
  )
}))

ggplot(qq_df, aes(sample = std_resid)) +
  stat_qq(alpha = 0.5, size = 1) +
  stat_qq_line(color = "red") +
  facet_wrap(~ predictor, scales = "free") +
  labs(
    title = "Normal QQ Plots of Standardized Residuals\n(Box–Cox Transformed + LMM)",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_bw()

# --- Boxplots: Each predictor by binary_label (alert vs drowsy) ---
df_long <- df_model %>%
  select(all_of(predictors), binary_label) %>%
  pivot_longer(cols = all_of(predictors), names_to = "predictor", values_to = "value") %>%
  mutate(predictor = factor(pretty_names[predictor], levels = predictor_levels))

ggplot(df_long, aes(x = binary_label, y = value, fill = binary_label)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  facet_wrap(~ predictor, scales = "free_y") +
  labs(
    title = "Boxplots of Predictors by Label",
    x = "Label",
    y = "Raw Value"
  ) +
  theme_bw() +
  theme(legend.position = "none")

# --- Violin plots (raw values) ---
df_long_raw <- df_model %>%
  select(all_of(predictors), binary_label) %>%
  pivot_longer(cols = all_of(predictors), names_to = "Predictor", values_to = "Value") %>%
  mutate(Predictor = factor(pretty_names[Predictor], levels = predictor_levels))

ggplot(df_long_raw, aes(x = binary_label, y = Value, fill = binary_label)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.1, outlier.shape = NA, color = "black") +
  facet_wrap(~ Predictor, scales = "free_y", ncol = 3) +
  labs(
    title = "Density Box Plots of Raw Predictor Values by Label",
    x = "Label",
    y = "Raw Value"
  ) +
  scale_fill_manual(values = c("alert" = "#FF9999", "drowsy" = "#00BFC4")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold")
  )

# --- Violin plots (standardized for visualization only) ---
df_scaled <- df_model %>%
  select(all_of(predictors), binary_label) %>%
  mutate(across(all_of(predictors), ~ scale(.)[,1])) %>%
  pivot_longer(cols = all_of(predictors), names_to = "Predictor", values_to = "Value") %>%
  mutate(Predictor = factor(pretty_names[Predictor], levels = predictor_levels))

ggplot(df_scaled, aes(x = binary_label, y = Value, fill = binary_label)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.1, outlier.shape = NA, color = "black") +
  facet_wrap(~ Predictor, scales = "free_y", ncol = 3) +
  labs(
    title = "Density Box Plots of Predictors by Label",
    x = "Label",
    y = "Standardized Value"
  ) +
  scale_fill_manual(values = c("alert" = "#FF9999", "drowsy" = "#00BFC4")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold")
  )