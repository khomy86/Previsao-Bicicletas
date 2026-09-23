library(tidymodels)
library(readr)

tidymodels_prefer()
set.seed(42)
dir.create("models", showWarnings = FALSE)

# On non-functioning days the system was offline and every count is zero, so
# those rows say nothing about demand.
bikes <- read_csv("data/clean/seoul_bike_sharing.csv", show_col_types = FALSE) |>
  filter(FUNCTIONING_DAY == "Yes")

# Solar radiation is left out on purpose: the forecast API doesn't provide it,
# and the best model is reused by the dashboard to predict from forecasts.
weather_vars <- c("TEMPERATURE_C", "HUMIDITY", "WIND_SPEED_M_S", "VISIBILITY_10M",
                  "DEW_POINT_TEMPERATURE_C", "RAINFALL_MM", "SNOWFALL_CM")
time_vars <- c("HOUR", "SEASONS", "HOLIDAY")
all_vars <- c(weather_vars, time_vars)

split <- initial_split(bikes, prop = 0.8, strata = RENTED_BIKE_COUNT)
train <- training(split)
test <- testing(split)
folds <- vfold_cv(train, v = 5)

# Models -----------------------------------------------------------------------

linear_recipe <- function(vars) {
  rec <- recipe(reformulate(vars, "RENTED_BIKE_COUNT"), data = train)
  if ("HOUR" %in% vars) {
    rec <- step_mutate(rec, HOUR = factor(HOUR, levels = 0:23))
  }
  rec |>
    step_dummy(all_nominal_predictors()) |>
    step_zv(all_predictors()) |>
    step_normalize(all_numeric_predictors())
}

ols <- linear_reg()
glmnet_spec <- function(mixture) {
  linear_reg(penalty = tune(), mixture = mixture) |> set_engine("glmnet")
}
rf_spec <- rand_forest(trees = 500, mtry = tune(), min_n = tune()) |>
  set_engine("ranger", importance = "impurity",
             num.threads = parallel::detectCores()) |>
  set_mode("regression")

candidates <- list(
  "Weather only (linear)" = workflow(linear_recipe(weather_vars), ols),
  "Hour and season (linear)" = workflow(linear_recipe(time_vars), ols),
  "All variables (linear)" = workflow(linear_recipe(all_vars), ols),
  "Polynomial" = workflow(
    linear_recipe(all_vars) |>
      step_poly(TEMPERATURE_C, HUMIDITY, degree = 2),
    ols
  ),
  "Interactions" = workflow(
    linear_recipe(all_vars) |>
      step_interact(~ starts_with("HOUR_"):TEMPERATURE_C + TEMPERATURE_C:HUMIDITY),
    ols
  ),
  "Ridge" = workflow(linear_recipe(all_vars), glmnet_spec(mixture = 0)),
  "Lasso" = workflow(linear_recipe(all_vars), glmnet_spec(mixture = 1)),
  "Random Forest" = workflow(
    recipe(reformulate(all_vars, "RENTED_BIKE_COUNT"), data = train),
    rf_spec
  )
)

fit_candidate <- function(wf, name) {
  message("Fitting ", name)
  if (nrow(extract_parameter_set_dials(wf)) > 0) {
    tuned <- tune_grid(wf, resamples = folds, grid = 10,
                       metrics = metric_set(rmse))
    wf <- finalize_workflow(wf, select_best(tuned, metric = "rmse"))
  }
  fit(wf, train)
}

fits <- imap(candidates, fit_candidate)

# Evaluation -------------------------------------------------------------------

test_metrics <- metric_set(rmse, rsq, mae)

comparison <- imap(fits, \(fit, name) {
  augment(fit, test) |>
    test_metrics(RENTED_BIKE_COUNT, .pred) |>
    mutate(model = name)
}) |>
  list_rbind() |>
  select(model, .metric, .estimate) |>
  pivot_wider(names_from = .metric, values_from = .estimate) |>
  mutate(rmse = round(rmse, 1), rsq = round(rsq, 3), mae = round(mae, 1)) |>
  arrange(rmse)

print(comparison)
write_csv(comparison, "models/model_comparison.csv")

best_name <- comparison$model[1]
best_fit <- fits[[best_name]]
saveRDS(best_fit, "models/best_model.rds")
message("Best model: ", best_name)

importance <- extract_fit_engine(fits[["Random Forest"]])$variable.importance
tibble(variable = names(importance),
       importance = round(100 * importance / sum(importance), 1)) |>
  arrange(desc(importance)) |>
  write_csv("models/feature_importance.csv")

p <- augment(best_fit, test) |>
  ggplot(aes(RENTED_BIKE_COUNT, .pred)) +
  geom_point(alpha = 0.4) +
  geom_abline(colour = "#d7301f", linetype = "dashed") +
  coord_obs_pred() +
  labs(
    title = paste("Predicted vs. actual:", best_name),
    subtitle = sprintf("Test set · R² = %.3f · RMSE = %.0f",
                       comparison$rsq[1], comparison$rmse[1]),
    x = "Actual rentals", y = "Predicted rentals"
  ) +
  theme_minimal(base_size = 12)

ggsave("models/best_model_predictions.png", p,
       width = 8, height = 8, dpi = 150, bg = "white")
