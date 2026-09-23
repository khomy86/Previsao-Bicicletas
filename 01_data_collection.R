library(rvest)
library(httr)
library(jsonlite)
library(dplyr)
library(readr)

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

# Bike-sharing systems (Wikipedia) --------------------------------------------

wiki_url <- "https://en.wikipedia.org/wiki/List_of_bicycle-sharing_systems"

systems <- read_html(wiki_url) |>
  html_element("table.wikitable") |>
  html_table()

# The country header spans two cells (flag + name), so it comes back twice.
systems <- systems[, -1]
write_csv(systems, "data/raw/bike_sharing_systems.csv")
message("Bike-sharing systems: ", nrow(systems), " rows")

# Seoul bike rentals (UCI dataset) --------------------------------------------

seoul_url <- paste0(
  "https://raw.githubusercontent.com/Navneet2409/",
  "bike-sharing-demand-prediction/main/SeoulBikeData.csv"
)

download.file(
  seoul_url,
  "data/raw/seoul_bike_sharing.csv",
  mode = "wb",
  quiet = TRUE
)
message("Seoul bike rentals: downloaded")

# 5-day weather forecast (OpenWeather) ----------------------------------------

cities <- c(
  "Seoul", "Tokyo", "Hangzhou", "New York", "Montreal", "London",
  "Paris", "Barcelona", "Lisbon", "Berlin", "Amsterdam", "Copenhagen"
)

fetch_forecast <- function(city, api_key) {
  res <- GET(
    "https://api.openweathermap.org/data/2.5/forecast",
    query = list(q = city, appid = api_key, units = "metric")
  )
  stop_for_status(res, task = paste("fetch the forecast for", city))

  body <- fromJSON(content(res, as = "text", encoding = "UTF-8"))
  f <- body$list

  tibble(
    city = body$city$name,
    country = body$city$country,
    lat = body$city$coord$lat,
    lon = body$city$coord$lon,
    utc_offset = body$city$timezone,
    datetime = as.POSIXct(f$dt, origin = "1970-01-01", tz = "UTC"),
    temperature = f$main$temp,
    humidity = f$main$humidity,
    dew_point = f$main$dew_point %||% NA_real_,
    wind_speed = f$wind$speed,
    visibility = f$visibility %||% NA_real_,
    rain_3h = f$rain$`3h` %||% NA_real_,
    snow_3h = f$snow$`3h` %||% NA_real_,
    weather = vapply(f$weather, \(w) w$description[1], character(1))
  )
}

api_key <- Sys.getenv("OPENWEATHER_API_KEY")

if (api_key == "") {
  warning("OPENWEATHER_API_KEY is not set, skipping the forecast download. ",
          "The dashboard map will be empty.")
} else {
  forecast <- bind_rows(lapply(cities, \(city) {
    Sys.sleep(0.5)
    fetch_forecast(city, api_key)
  }))
  write_csv(forecast, "data/raw/weather_forecast.csv")
  message("Weather forecast: ", nrow(forecast), " rows for ",
          n_distinct(forecast$city), " cities")
}
