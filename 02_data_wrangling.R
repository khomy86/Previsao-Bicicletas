library(dplyr)
library(readr)
library(stringr)
library(lubridate)

dir.create("data/clean", recursive = TRUE, showWarnings = FALSE)

clean_names <- function(df) {
  names(df) <- names(df) |>
    str_to_upper() |>
    str_replace_all("[^A-Z0-9]+", "_") |>
    str_remove_all("^_|_$")
  df
}

season_of <- function(date) {
  c("Winter", "Winter", "Spring", "Spring", "Spring", "Summer",
    "Summer", "Summer", "Autumn", "Autumn", "Autumn", "Winter")[month(date)]
}

# Magnus approximation, used when the API doesn't return a dew point
dew_point <- function(temp, humidity) {
  gamma <- log(humidity / 100) + 17.62 * temp / (243.12 + temp)
  243.12 * gamma / (17.62 - gamma)
}

# Bike-sharing systems --------------------------------------------------------

remove_refs <- function(x) str_squish(str_remove_all(x, "\\[[^\\]]*\\]"))

systems <- read_csv("data/raw/bike_sharing_systems.csv",
                    show_col_types = FALSE) |>
  clean_names() |>
  rename(CITY = CITY_REGION) |>
  mutate(
    across(where(is.character), remove_refs),
    LAUNCH_YEAR = as.integer(str_extract(LAUNCHED, "\\b(18|19|20)\\d{2}\\b")),
    ACTIVE = is.na(DISCONTINUED) | DISCONTINUED == ""
  ) |>
  filter(!is.na(COUNTRY), !is.na(CITY))

write_csv(systems, "data/clean/bike_sharing_systems.csv")
message("Bike-sharing systems: ", nrow(systems), " rows, ",
        sum(systems$ACTIVE), " active")

# Seoul bike rentals ----------------------------------------------------------

seoul <- read_csv(
  "data/raw/seoul_bike_sharing.csv",
  locale = locale(encoding = "latin1"),
  show_col_types = FALSE
) |>
  clean_names() |>
  filter(!is.na(RENTED_BIKE_COUNT)) |>
  mutate(
    DATE = dmy(DATE),
    across(c(RAINFALL_MM, SNOWFALL_CM), \(x) coalesce(x, 0)),
    across(TEMPERATURE_C:SOLAR_RADIATION_MJ_M2,
           \(x) coalesce(x, median(x, na.rm = TRUE)))
  )

write_csv(seoul, "data/clean/seoul_bike_sharing.csv")
message("Seoul bike rentals: ", nrow(seoul), " rows, ",
        min(seoul$DATE), " to ", max(seoul$DATE))

# Weather forecast ------------------------------------------------------------
# The API returns 3-hour steps in UTC. The model was trained on hourly local
# data, so interpolate to hourly values and shift to each city's local time.
# Units are converted to match the Seoul dataset.

if (file.exists("data/raw/weather_forecast.csv")) {
  forecast <- read_csv("data/raw/weather_forecast.csv",
                       show_col_types = FALSE) |>
    mutate(
      dew_point = coalesce(dew_point, dew_point(temperature, humidity)),
      visibility = coalesce(visibility, 10000),
      across(c(rain_3h, snow_3h), \(x) coalesce(x, 0))
    ) |>
    group_by(city, country, lat, lon, utc_offset) |>
    reframe(
      time = seq(min(datetime), max(datetime), by = "hour"),
      across(c(temperature, humidity, dew_point, wind_speed, visibility),
             \(x) approx(datetime, x, xout = time)$y),
      across(c(rain_3h, snow_3h),
             \(x) approx(datetime, x, xout = time, method = "constant")$y)
    ) |>
    mutate(local_time = time + utc_offset) |>
    transmute(
      CITY = city,
      COUNTRY = country,
      LAT = lat,
      LON = lon,
      LOCAL_TIME = format(local_time, "%Y-%m-%d %H:%M"),
      HOUR = hour(local_time),
      SEASONS = season_of(local_time),
      HOLIDAY = "No Holiday",
      TEMPERATURE_C = temperature,
      HUMIDITY = humidity,
      WIND_SPEED_M_S = wind_speed,
      VISIBILITY_10M = pmin(visibility / 10, 2000),
      DEW_POINT_TEMPERATURE_C = dew_point,
      RAINFALL_MM = rain_3h / 3,
      SNOWFALL_CM = snow_3h / 30
    ) |>
    mutate(across(TEMPERATURE_C:SNOWFALL_CM, \(x) round(x, 2)))

  write_csv(forecast, "data/clean/weather_forecast.csv")
  message("Weather forecast: ", nrow(forecast), " hourly rows")
}
