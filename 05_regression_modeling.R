# ===============================================================================
# BIKE SHARING DEMAND ANALYSIS PROJECT
# Phase 5: Regression Modeling
# ===============================================================================

# Load required libraries
library(tidymodels)
library(dplyr)
library(readr)
library(ggplot2)
library(glmnet)
library(randomForest)
library(future)

# Set up parallel processing
plan(multisession)

# Set seed for reproducibility
set.seed(123)

cat("Starting Regression Modeling...\n")

# ===============================================================================
# TASK 1: Load Data and Split into Train/Test Sets
# ===============================================================================

cat("\nTASK 1: Split data into train/test sets\n")

# Load cleaned dataset
seoul_bikes <- read_csv("data/clean/seoul_bike_sharing_clean.csv")

# Convert categorical variables to factors
seoul_bikes <- seoul_bikes %>%
  mutate(
    SEASONS = factor(SEASONS),
    HOLIDAY = factor(HOLIDAY),
    FUNCTIONING_DAY = factor(FUNCTIONING_DAY),
    HOUR_CATEGORY = factor(HOUR_CATEGORY),
    DATE = as.Date(DATE, format = "%d/%m/%Y")
  )

# Create train/test split (80/20)
bike_split <- initial_split(seoul_bikes, prop = 0.8, strata = RENTED_BIKE_COUNT)
bike_train <- training(bike_split)
bike_test <- testing(bike_split)

cat("  ✓ Data split completed\n")
cat("    Training set:", nrow(bike_train), "rows\n")
cat("    Test set:", nrow(bike_test), "rows\n")

# ===============================================================================
# TASK 2: Build Linear Regression Model - Weather Variables Only
# ===============================================================================

cat("\nTASK 2: Linear regression model with weather variables\n")

# Define weather variables
weather_vars <- c("TEMPERATURE_C", "HUMIDITY", "WIND_SPEED_M_S", "VISIBILITY_10M", 
                  "DEW_POINT_TEMPERATURE_C", "SOLAR_RADIATION_MJ_M2", "RAINFALL_MM", "SNOWFALL_CM")

# Create recipe for weather model
weather_recipe <- recipe(RENTED_BIKE_COUNT ~ TEMPERATURE_C + HUMIDITY + WIND_SPEED_M_S + VISIBILITY_10M + 
                         DEW_POINT_TEMPERATURE_C + SOLAR_RADIATION_MJ_M2 + RAINFALL_MM + SNOWFALL_CM, 
                         data = bike_train) %>%
  step_zv(all_predictors()) %>%
  step_corr(all_numeric_predictors(), threshold = 0.9) %>%
  step_normalize(all_numeric_predictors())

# Define linear regression model
lm_spec <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

# Create workflow
weather_workflow <- workflow() %>%
  add_recipe(weather_recipe) %>%
  add_model(lm_spec)

# Fit the model
weather_fit <- weather_workflow %>%
  fit(data = bike_train)

# Evaluate on test set
weather_pred <- weather_fit %>%
  predict(bike_test) %>%
  bind_cols(bike_test %>% select(RENTED_BIKE_COUNT))

weather_metrics <- weather_pred %>%
  metrics(truth = RENTED_BIKE_COUNT, estimate = .pred)

cat("  ✓ Weather model completed\n")
print(weather_metrics)

# ===============================================================================
# TASK 3: Build Linear Regression Model - Time/Date Variables
# ===============================================================================

cat("\nTASK 3: Linear regression model with time/date variables\n")

# Create time/date features
time_recipe <- recipe(RENTED_BIKE_COUNT ~ HOUR + SEASONS + HOLIDAY + FUNCTIONING_DAY + HOUR_CATEGORY,
                      data = bike_train) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_corr(all_numeric_predictors(), threshold = 0.9) %>%
  step_normalize(all_numeric_predictors())

# Create workflow for time model
time_workflow <- workflow() %>%
  add_recipe(time_recipe) %>%
  add_model(lm_spec)

# Fit the model
time_fit <- time_workflow %>%
  fit(data = bike_train)

# Evaluate on test set
time_pred <- time_fit %>%
  predict(bike_test) %>%
  bind_cols(bike_test %>% select(RENTED_BIKE_COUNT))

time_metrics <- time_pred %>%
  metrics(truth = RENTED_BIKE_COUNT, estimate = .pred)

cat("  ✓ Time model completed\n")
print(time_metrics)

# ===============================================================================
# TASK 4: Evaluate Models and Identify Important Variables
# ===============================================================================

cat("\nTASK 4: Model evaluation and variable importance\n")

# Extract coefficients for weather model
weather_coefs <- weather_fit %>%
  extract_fit_parsnip() %>%
  tidy() %>%
  arrange(desc(abs(estimate)))

cat("  Weather model - Top 5 important variables:\n")
print(weather_coefs %>% head(6))  # 6 to include intercept

# Extract coefficients for time model
time_coefs <- time_fit %>%
  extract_fit_parsnip() %>%
  tidy() %>%
  arrange(desc(abs(estimate)))

cat("  Time model - Top 5 important variables:\n")
print(time_coefs %>% head(6))

# ===============================================================================
# TASK 5: Combined Model with All Variables
# ===============================================================================

cat("\nTASK 5: Combined model with all variables\n")

# Create comprehensive recipe - use all variables except DATE
combined_recipe <- recipe(RENTED_BIKE_COUNT ~ ., data = bike_train) %>%
  step_rm(DATE) %>%  # Remove DATE column
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%  # Remove zero variance predictors first
  step_corr(all_numeric_predictors(), threshold = 0.9) %>%  # Remove highly correlated predictors
  step_normalize(all_numeric_predictors())

# Create workflow
combined_workflow <- workflow() %>%
  add_recipe(combined_recipe) %>%
  add_model(lm_spec)

# Fit the model
combined_fit <- combined_workflow %>%
  fit(data = bike_train)

# Evaluate on test set
combined_pred <- combined_fit %>%
  predict(bike_test) %>%
  bind_cols(bike_test %>% select(RENTED_BIKE_COUNT))

combined_metrics <- combined_pred %>%
  metrics(truth = RENTED_BIKE_COUNT, estimate = .pred)

cat("  ✓ Combined model completed\n")
print(combined_metrics)

# ===============================================================================
# TASK 6: Add Higher-Order Terms
# ===============================================================================

cat("\nTASK 6: Model with higher-order terms\n")

# Create recipe with polynomial terms
poly_recipe <- recipe(RENTED_BIKE_COUNT ~ ., data = bike_train) %>%
  step_rm(DATE) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%  # Remove zero variance predictors first
  step_poly(TEMPERATURE_C, HUMIDITY, WIND_SPEED_M_S, degree = 2) %>%
  step_corr(all_numeric_predictors(), threshold = 0.9) %>%  # Remove highly correlated predictors
  step_normalize(all_numeric_predictors())

# Create workflow
poly_workflow <- workflow() %>%
  add_recipe(poly_recipe) %>%
  add_model(lm_spec)

# Fit the model
poly_fit <- poly_workflow %>%
  fit(data = bike_train)

# Evaluate
poly_pred <- poly_fit %>%
  predict(bike_test) %>%
  bind_cols(bike_test %>% select(RENTED_BIKE_COUNT))

poly_metrics <- poly_pred %>%
  metrics(truth = RENTED_BIKE_COUNT, estimate = .pred)

cat("  ✓ Polynomial model completed\n")
print(poly_metrics)

# ===============================================================================
# TASK 7: Add Interaction Terms
# ===============================================================================

cat("\nTASK 7: Model with interaction terms\n")

# Create recipe with interactions
interaction_recipe <- recipe(RENTED_BIKE_COUNT ~ ., data = bike_train) %>%
  step_rm(DATE) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_interact(~ TEMPERATURE_C:HUMIDITY) %>%
  step_corr(all_numeric_predictors(), threshold = 0.9) %>%
  step_normalize(all_numeric_predictors())

# Create workflow
interaction_workflow <- workflow() %>%
  add_recipe(interaction_recipe) %>%
  add_model(lm_spec)

# Fit the model
interaction_fit <- interaction_workflow %>%
  fit(data = bike_train)

# Evaluate
interaction_pred <- interaction_fit %>%
  predict(bike_test) %>%
  bind_cols(bike_test %>% select(RENTED_BIKE_COUNT))

interaction_metrics <- interaction_pred %>%
  metrics(truth = RENTED_BIKE_COUNT, estimate = .pred)

cat("  ✓ Interaction model completed\n")
print(interaction_metrics)

# ===============================================================================
# TASK 8: Add Regularization (Ridge and Lasso)
# ===============================================================================

cat("\nTASK 8: Regularized models (Ridge and Lasso)\n")

# Ridge regression
ridge_spec <- linear_reg(penalty = 0.1, mixture = 0) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

ridge_workflow <- workflow() %>%
  add_recipe(combined_recipe) %>%
  add_model(ridge_spec)

ridge_fit <- ridge_workflow %>%
  fit(data = bike_train)

ridge_pred <- ridge_fit %>%
  predict(bike_test) %>%
  bind_cols(bike_test %>% select(RENTED_BIKE_COUNT))

ridge_metrics <- ridge_pred %>%
  metrics(truth = RENTED_BIKE_COUNT, estimate = .pred)

cat("  ✓ Ridge regression completed\n")
print(ridge_metrics)

# Lasso regression
lasso_spec <- linear_reg(penalty = 0.1, mixture = 1) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

lasso_workflow <- workflow() %>%
  add_recipe(combined_recipe) %>%
  add_model(lasso_spec)

lasso_fit <- lasso_workflow %>%
  fit(data = bike_train)

lasso_pred <- lasso_fit %>%
  predict(bike_test) %>%
  bind_cols(bike_test %>% select(RENTED_BIKE_COUNT))

lasso_metrics <- lasso_pred %>%
  metrics(truth = RENTED_BIKE_COUNT, estimate = .pred)

cat("  ✓ Lasso regression completed\n")
print(lasso_metrics)

# ===============================================================================
# TASK 9: Experiment with Random Forest (Best Performing Model)
# ===============================================================================

cat("\nTASK 9: Random Forest model (advanced)\n")

# Random Forest specification
rf_spec <- rand_forest(trees = 500, mtry = tune(), min_n = tune()) %>%
  set_engine("randomForest") %>%
  set_mode("regression")

# Create grid for tuning
rf_grid <- grid_regular(
  mtry(range = c(3, 8)),
  min_n(range = c(5, 25)),
  levels = 3
)

# Create CV folds for tuning
bike_folds <- vfold_cv(bike_train, v = 5)

# Tune the model
cat("  - Tuning Random Forest hyperparameters (this may take a few minutes)...\n")
rf_tune <- tune_grid(
  rf_spec,
  preprocessor = combined_recipe,
  resamples = bike_folds,
  grid = rf_grid,
  metrics = metric_set(rmse, rsq)
)

# Select best parameters
best_rf <- rf_tune %>%
  select_best(metric = "rmse")

# Finalize and fit
final_rf_spec <- finalize_model(rf_spec, best_rf)

rf_workflow <- workflow() %>%
  add_recipe(combined_recipe) %>%
  add_model(final_rf_spec)

rf_fit <- rf_workflow %>%
  fit(data = bike_train)

# Evaluate
rf_pred <- rf_fit %>%
  predict(bike_test) %>%
  bind_cols(bike_test %>% select(RENTED_BIKE_COUNT))

rf_metrics <- rf_pred %>%
  metrics(truth = RENTED_BIKE_COUNT, estimate = .pred)

cat("  ✓ Random Forest model completed\n")
print(rf_metrics)

# ===============================================================================
# MODEL COMPARISON
# ===============================================================================

cat("\n", paste(rep("=", 50), collapse=""), "\n")
cat("MODEL COMPARISON SUMMARY\n")
cat(paste(rep("=", 50), collapse=""), "\n")

# Compile all model results
model_results <- bind_rows(
  weather_metrics %>% mutate(model = "Weather Only"),
  time_metrics %>% mutate(model = "Time Only"),
  combined_metrics %>% mutate(model = "Combined"),
  poly_metrics %>% mutate(model = "Polynomial"),
  interaction_metrics %>% mutate(model = "Interactions"),
  ridge_metrics %>% mutate(model = "Ridge"),
  lasso_metrics %>% mutate(model = "Lasso"),
  rf_metrics %>% mutate(model = "Random Forest")
) %>%
  select(model, .metric, .estimate) %>%
  pivot_wider(names_from = .metric, values_from = .estimate) %>%
  arrange(rmse)

cat("Model Performance (sorted by RMSE):\n")
print(model_results)

# Save model results
if (!dir.exists("models")) {
  dir.create("models")
}

write_csv(model_results, "models/model_comparison.csv")

# Save the best model
best_model_name <- model_results$model[1]
cat("\nBest performing model:", best_model_name, "\n")

# Save the final fitted workflow object
saveRDS(rf_fit, "models/best_model_workflow.rds")
cat("  ✓ Best model workflow saved to 'models/best_model_workflow.rds'\n")

# Create prediction vs actual plot for best model
if (best_model_name == "Random Forest") {
  best_pred <- rf_pred
} else if (best_model_name == "Lasso") {
  best_pred <- lasso_pred
} else if (best_model_name == "Ridge") {
  best_pred <- ridge_pred
} else {
  best_pred <- combined_pred
}

pred_plot <- ggplot(best_pred, aes(x = RENTED_BIKE_COUNT, y = .pred)) +
  geom_point(alpha = 0.6) +
  geom_abline(color = "red", linetype = "dashed") +
  labs(
    title = paste("Predicted vs Actual -", best_model_name, "Model"),
    x = "Actual Bike Count",
    y = "Predicted Bike Count",
    subtitle = paste("R² =", round(model_results$rsq[1], 3), 
                     "| RMSE =", round(model_results$rmse[1], 1))
  ) +
  theme_minimal()

ggsave("models/best_model_predictions.png", pred_plot, width = 10, height = 8, dpi = 300)

cat("\nRegression modeling completed!\n")
cat("Next steps:\n")
cat("1. Proceed to Phase 6: R Shiny Dashboard\n")
cat("2. Run the dashboard script: source('06_shiny_dashboard.R')\n") 