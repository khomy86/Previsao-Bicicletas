library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)

dir.create("visualizations", showWarnings = FALSE)

bikes <- read_csv("data/clean/seoul_bike_sharing.csv", show_col_types = FALSE) |>
  mutate(
    HOUR = factor(HOUR, levels = 0:23),
    SEASONS = factor(SEASONS, levels = c("Winter", "Spring", "Summer", "Autumn"))
  )

save_plot <- function(plot, name, width = 10, height = 6) {
  ggsave(file.path("visualizations", paste0(name, ".png")), plot,
         width = width, height = height, dpi = 150, bg = "white")
}

theme_set(theme_minimal(base_size = 12))

# Quick facts -----------------------------------------------------------------

holidays <- sum(bikes$HOLIDAY == "Holiday")
daily <- bikes |>
  group_by(DATE) |>
  summarise(
    rainfall = sum(RAINFALL_MM),
    snowfall = sum(SNOWFALL_CM),
    rentals = sum(RENTED_BIKE_COUNT)
  )

message(sprintf("Records: %d (a full year has %d)", nrow(bikes), 365 * 24))
message(sprintf("Records on functioning days: %d", sum(bikes$FUNCTIONING_DAY == "Yes")))
message(sprintf("Holiday records: %d (%.1f%%)", holidays, 100 * holidays / nrow(bikes)))
message(sprintf("Days with snowfall: %d", sum(daily$snowfall > 0)))

bikes |>
  group_by(SEASONS) |>
  summarise(rainfall_mm = sum(RAINFALL_MM), snowfall_cm = sum(SNOWFALL_CM)) |>
  print()

summary(bikes[c("RENTED_BIKE_COUNT", "TEMPERATURE_C", "HUMIDITY",
                "WIND_SPEED_M_S", "VISIBILITY_10M", "RAINFALL_MM")]) |>
  print()

# Plots -----------------------------------------------------------------------

p <- ggplot(bikes, aes(DATE, RENTED_BIKE_COUNT)) +
  geom_point(alpha = 0.3, size = 0.6) +
  geom_smooth(method = "loess", formula = y ~ x, colour = "#d7301f", se = FALSE) +
  labs(title = "Hourly rentals over the year",
       x = NULL, y = "Bikes rented")
save_plot(p, "rentals_over_time", width = 12)

p <- ggplot(bikes, aes(DATE, RENTED_BIKE_COUNT, colour = HOUR)) +
  geom_point(alpha = 0.6, size = 0.6) +
  scale_colour_viridis_d(name = "Hour") +
  labs(title = "Hourly rentals over the year, by hour of day",
       x = NULL, y = "Bikes rented")
save_plot(p, "rentals_over_time_by_hour", width = 12)

p <- ggplot(bikes, aes(RENTED_BIKE_COUNT)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50,
                 fill = "#9ecae1", colour = "white") +
  geom_density(colour = "#d7301f", linewidth = 1) +
  labs(title = "Distribution of hourly rentals",
       x = "Bikes rented", y = "Density")
save_plot(p, "rentals_distribution")

p <- ggplot(bikes, aes(TEMPERATURE_C, RENTED_BIKE_COUNT)) +
  geom_point(aes(colour = HOUR), alpha = 0.5, size = 0.8) +
  geom_smooth(method = "lm", formula = y ~ x, colour = "black", se = FALSE) +
  facet_wrap(~SEASONS) +
  scale_colour_viridis_d(name = "Hour") +
  labs(title = "Rentals vs. temperature, by season",
       x = "Temperature (°C)", y = "Bikes rented")
save_plot(p, "rentals_vs_temperature", width = 12, height = 8)

p <- ggplot(bikes, aes(HOUR, RENTED_BIKE_COUNT, fill = SEASONS)) +
  geom_boxplot(outlier.size = 0.4, show.legend = FALSE) +
  facet_wrap(~SEASONS, ncol = 2) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Rentals by hour of day, by season",
       x = "Hour", y = "Bikes rented")
save_plot(p, "hourly_rentals_by_season", width = 12, height = 8)

p <- daily |>
  pivot_longer(c(rainfall, snowfall), names_to = "type", values_to = "amount") |>
  ggplot(aes(DATE, amount, colour = type)) +
  geom_line() +
  scale_colour_manual(values = c(rainfall = "#2171b5", snowfall = "#969696"),
                      labels = c("Rain (mm)", "Snow (cm)"), name = NULL) +
  labs(title = "Daily precipitation", x = NULL, y = NULL)
save_plot(p, "daily_precipitation", width = 12)
