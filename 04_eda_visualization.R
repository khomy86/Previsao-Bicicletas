# ===============================================================================
# BIKE SHARING DEMAND ANALYSIS PROJECT
# Phase 4: Exploratory Data Analysis with Visualization
# ===============================================================================

# Load required libraries
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(lubridate)

# Create visualizations directory
if (!dir.exists("visualizations")) {
  dir.create("visualizations")
}

cat("Starting EDA with Visualization...\n")

# ===============================================================================
# TASK 1: Load Dataset
# ===============================================================================

cat("\nTask 1 - Load the dataset\n")
seoul_bikes <- read_csv("data/clean/seoul_bike_sharing_clean.csv")
cat("  ✓ Dataset loaded:", nrow(seoul_bikes), "rows,", ncol(seoul_bikes), "columns\n")

# ===============================================================================  
# TASK 2: Reformat DATE as Date
# ===============================================================================

cat("\nTask 2 - Reshape DATE as date\n")
# Parse date using the correct format "%d/%m/%Y"
seoul_bikes <- seoul_bikes %>%
  mutate(DATE = as.Date(DATE, format = "%d/%m/%Y"))

cat("  ✓ DATE column reformatted\n")
cat("    Date range:", min(seoul_bikes$DATE), "to", max(seoul_bikes$DATE), "\n")

# ===============================================================================
# TASK 3: Cast HOUR as Categorical Variable
# ===============================================================================

cat("\nTask 3 - Broadcast HOURS as a categorical variable\n")
seoul_bikes <- seoul_bikes %>%
  mutate(HOUR = factor(HOUR, levels = 0:23, ordered = TRUE))

cat("  ✓ HOUR converted to ordered categorical variable\n")
cat("    Levels:", length(levels(seoul_bikes$HOUR)), "\n")

# ===============================================================================
# TASK 4: Dataset Summary
# ===============================================================================

cat("\nTask 4 - Dataset Summary\n")
dataset_summary <- summary(seoul_bikes[c("RENTED_BIKE_COUNT", "TEMPERATURE_C", "HUMIDITY", 
                                        "WIND_SPEED_M_S", "VISIBILITY_10M", "RAINFALL_MM")])
print(dataset_summary)

# ===============================================================================
# TASK 5: Calculate Number of Holidays
# ===============================================================================

cat("\nTask 5 - Calculate how many holidays there are\n")
holiday_count <- seoul_bikes %>%
  filter(HOLIDAY == "Holiday") %>%
  nrow()
cat("  Number of holiday records:", holiday_count, "\n")

# ===============================================================================
# TASK 6: Calculate Percentage of Holiday Records
# ===============================================================================

cat("\nTask 6 - Calculate the percentage of records that fall on a holiday\n")
holiday_percentage <- round(holiday_count / nrow(seoul_bikes) * 100, 2)
cat("  Percentage of holiday records:", holiday_percentage, "%\n")

# ===============================================================================
# TASK 7: Expected Records for Full Year
# ===============================================================================

cat("\nTask 7 - Determine how many records we expect to have\n")
expected_records <- 365 * 24  # 365 days * 24 hours
cat("  Expected records for full year:", expected_records, "\n")
cat("  Actual records:", nrow(seoul_bikes), "\n")

# ===============================================================================
# TASK 8: Expected Records Given Functioning Days
# ===============================================================================

cat("\nTask 8 - How many records should there be based on 'FUNCTIONING_DAY'\n")
functioning_records <- seoul_bikes %>%
  filter(FUNCTIONING_DAY == "Yes") %>%
  nrow()
cat("  Records with functioning day = Yes:", functioning_records, "\n")

# ===============================================================================
# TASK 9: Seasonal Precipitation and Snowfall
# ===============================================================================

cat("\nTask 9 - Total seasonal precipitation and snowfall\n")
seasonal_precip <- seoul_bikes %>%
  group_by(SEASONS) %>%
  summarise(
    total_precipitation = sum(RAINFALL_MM, na.rm = TRUE),
    total_snowfall = sum(SNOWFALL_CM, na.rm = TRUE),
    .groups = "drop"
  )
print(seasonal_precip)

# ===============================================================================
# TASK 10: Time Series Scatter Plot
# ===============================================================================

cat("\nTask 10 - Create a scatter plot of RENTED_BIKE_COUNT vs DATE\n")
plot10 <- ggplot(seoul_bikes, aes(x = DATE, y = RENTED_BIKE_COUNT)) +
  geom_point(alpha = 0.6, size = 0.8) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(
    title = "Seoul Bike Rental Count Over Time",
    x = "Date",
    y = "Rented Bike Count",
    subtitle = "Time series showing seasonal patterns in bike usage"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("visualizations/task10_time_series_scatter.png", plot10, width = 12, height = 6, dpi = 300)
cat("  ✓ Time series scatter plot saved\n")

# ===============================================================================
# TASK 11: Time Series with Hour as Color
# ===============================================================================

cat("\nTask 11 - Same chart with HOURS as color\n")
plot11 <- ggplot(seoul_bikes, aes(x = DATE, y = RENTED_BIKE_COUNT, color = HOUR)) +
  geom_point(alpha = 0.7, size = 0.6) +
  scale_color_viridis_d(name = "Hour") +
  labs(
    title = "Seoul Bike Rental Count Over Time by Hour",
    x = "Date", 
    y = "Rented Bike Count",
    subtitle = "Different hours shown in different colors"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("visualizations/task11_time_series_hour_color.png", plot11, width = 12, height = 6, dpi = 300)
cat("  ✓ Time series with hour colors saved\n")

# ===============================================================================
# TASK 12: Histogram with Kernel Density
# ===============================================================================

cat("\nTask 12 - Histogram overlaid with kernel density curve\n")
plot12 <- ggplot(seoul_bikes, aes(x = RENTED_BIKE_COUNT)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, alpha = 0.7, fill = "skyblue", color = "black") +
  geom_density(color = "red", linewidth = 1.2) +
  labs(
    title = "Distribution of Bike Rental Counts",
    x = "Rented Bike Count",
    y = "Density",
    subtitle = "Histogram with kernel density overlay"
  ) +
  theme_minimal()

ggsave("visualizations/task12_histogram_density.png", plot12, width = 10, height = 6, dpi = 300)
cat("  ✓ Histogram with density curve saved\n")

# ===============================================================================
# TASK 13: Correlation Scatter Plot
# ===============================================================================

cat("\nTask 13 - RENTED_BIKE_COUNT vs TEMPERATURE scatter plot by SEASONS\n")
plot13 <- ggplot(seoul_bikes, aes(x = TEMPERATURE_C, y = RENTED_BIKE_COUNT, color = HOUR)) +
  geom_point(alpha = 0.6, size = 1) +
  facet_wrap(~SEASONS, scales = "free") +
  scale_color_viridis_d(name = "Hour") +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.8) +
  labs(
    title = "Bike Rental Count vs Temperature by Season",
    x = "Temperature (°C)",
    y = "Rented Bike Count",
    subtitle = "Correlation patterns across different seasons, colored by hour"
  ) +
  theme_minimal()

ggsave("visualizations/task13_correlation_scatter.png", plot13, width = 14, height = 8, dpi = 300)
cat("  ✓ Correlation scatter plot saved\n")

# ===============================================================================
# TASK 14: Boxplots by Hour and Season
# ===============================================================================

cat("\nTask 14 - Display of four boxplots of RENTED_BIKE_COUNT vs. TIME grouped by SEASONS\n")
plot14 <- ggplot(seoul_bikes, aes(x = HOUR, y = RENTED_BIKE_COUNT, fill = SEASONS)) +
  geom_boxplot(alpha = 0.8) +
  facet_wrap(~SEASONS, scales = "free_y", ncol = 2) +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  labs(
    title = "Bike Rental Count Distribution by Hour and Season",
    x = "Hour of Day",
    y = "Rented Bike Count",
    subtitle = "Boxplots showing hourly patterns across seasons"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"  # Remove legend since facets show seasons
  )

ggsave("visualizations/task14_boxplots_hour_season.png", plot14, width = 14, height = 10, dpi = 300)
cat("  ✓ Boxplots by hour and season saved\n")

# ===============================================================================
# TASK 15: Daily Precipitation and Snowfall
# ===============================================================================

cat("\nTask 15 - Group data by DATE and calculate daily precipitation/snowfall\n")
daily_weather <- seoul_bikes %>%
  group_by(DATE) %>%
  summarise(
    daily_precipitation = sum(RAINFALL_MM, na.rm = TRUE),
    daily_snowfall = sum(SNOWFALL_CM, na.rm = TRUE),
    avg_bike_count = mean(RENTED_BIKE_COUNT, na.rm = TRUE),
    .groups = "drop"
  )

# Create visualization for daily weather patterns
plot15 <- daily_weather %>%
  select(DATE, daily_precipitation, daily_snowfall) %>%
  gather(key = "weather_type", value = "amount", -DATE) %>%
  ggplot(aes(x = DATE, y = amount, color = weather_type)) +
  geom_line(alpha = 0.7) +
  geom_point(alpha = 0.5, size = 0.8) +
  scale_color_manual(values = c("daily_precipitation" = "blue", "daily_snowfall" = "red"),
                     labels = c("Precipitation", "Snowfall")) +
  labs(
    title = "Daily Precipitation and Snowfall Over Time",
    x = "Date",
    y = "Amount (mm/cm)",
    color = "Weather Type",
    subtitle = "Daily totals of precipitation and snowfall"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("visualizations/task15_daily_weather.png", plot15, width = 12, height = 6, dpi = 300)
cat("  ✓ Daily weather patterns saved\n")

# ===============================================================================
# TASK 16: Determine Days with Snowfall
# ===============================================================================

cat("\nTarefa 16 - Determinar quantos dias tiveram queda de neve\n")
days_with_snow <- daily_weather %>%
    filter(daily_snowfall > 0) %>%
    nrow()

cat("  Number of days with snowfall:", days_with_snow, "\n")

# ===============================================================================
# EDA VISUALIZATION SUMMARY
# ===============================================================================

cat("\n", paste(rep("=", 50), collapse=""))
cat("\nEDA VISUALIZATION SUMMARY\n")
cat(paste(rep("=", 50), collapse=""), "\n")
cat("✓ All visualization tasks completed.\n")
cat("  - Plots saved to 'visualizations' directory\n")
cat("Next steps:\n")
cat("1. Proceed to Phase 5: Regression Modeling\n")
cat("2. Run the modeling script: source('05_regression_modeling.R')\n")
cat("\nEDA with visualization completed!\n") 