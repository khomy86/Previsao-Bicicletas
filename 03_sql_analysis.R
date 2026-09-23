library(DBI)
library(RSQLite)
library(dplyr)
library(readr)

dir.create("results", showWarnings = FALSE)

con <- dbConnect(SQLite(), ":memory:")

# SQLite has no date type, so dates go in as ISO strings to keep them sortable
seoul <- read_csv("data/clean/seoul_bike_sharing.csv", show_col_types = FALSE) |>
  mutate(DATE = as.character(DATE))
systems <- read_csv("data/clean/bike_sharing_systems.csv", show_col_types = FALSE)

dbWriteTable(con, "SEOUL_BIKE_SHARING", seoul)
dbWriteTable(con, "BIKE_SHARING_SYSTEMS", systems)

queries <- list(
  record_count = "
    SELECT COUNT(*) AS records
    FROM SEOUL_BIKE_SHARING",

  hours_with_rentals = "
    SELECT COUNT(*) AS hours
    FROM SEOUL_BIKE_SHARING
    WHERE RENTED_BIKE_COUNT > 0",

  seasons = "
    SELECT DISTINCT SEASONS
    FROM SEOUL_BIKE_SHARING",

  date_range = "
    SELECT MIN(DATE) AS first_date, MAX(DATE) AS last_date
    FROM SEOUL_BIKE_SHARING",

  busiest_hour = "
    SELECT DATE, HOUR, RENTED_BIKE_COUNT
    FROM SEOUL_BIKE_SHARING
    WHERE RENTED_BIKE_COUNT = (SELECT MAX(RENTED_BIKE_COUNT) FROM SEOUL_BIKE_SHARING)",

  top_hours_by_season = "
    SELECT SEASONS, HOUR,
           ROUND(AVG(TEMPERATURE_C), 1) AS avg_temperature,
           ROUND(AVG(RENTED_BIKE_COUNT)) AS avg_rentals
    FROM SEOUL_BIKE_SHARING
    GROUP BY SEASONS, HOUR
    ORDER BY avg_rentals DESC
    LIMIT 10",

  rentals_by_season = "
    SELECT SEASONS,
           ROUND(AVG(RENTED_BIKE_COUNT)) AS avg_rentals,
           MIN(RENTED_BIKE_COUNT) AS min_rentals,
           MAX(RENTED_BIKE_COUNT) AS max_rentals,
           ROUND(SQRT(AVG(RENTED_BIKE_COUNT * RENTED_BIKE_COUNT)
                      - AVG(RENTED_BIKE_COUNT) * AVG(RENTED_BIKE_COUNT))) AS sd_rentals
    FROM SEOUL_BIKE_SHARING
    WHERE FUNCTIONING_DAY = 'Yes'
    GROUP BY SEASONS
    ORDER BY avg_rentals DESC",

  weather_by_season = "
    SELECT SEASONS,
           ROUND(AVG(TEMPERATURE_C), 1) AS avg_temperature,
           ROUND(AVG(HUMIDITY), 1) AS avg_humidity,
           ROUND(AVG(WIND_SPEED_M_S), 2) AS avg_wind_speed,
           ROUND(AVG(VISIBILITY_10M)) AS avg_visibility,
           ROUND(AVG(SOLAR_RADIATION_MJ_M2), 2) AS avg_solar_radiation,
           ROUND(SUM(RAINFALL_MM), 1) AS total_rainfall,
           ROUND(SUM(SNOWFALL_CM), 1) AS total_snowfall,
           ROUND(AVG(RENTED_BIKE_COUNT)) AS avg_rentals
    FROM SEOUL_BIKE_SHARING
    GROUP BY SEASONS
    ORDER BY avg_rentals DESC",

  systems_by_country = "
    SELECT COUNTRY,
           COUNT(*) AS systems,
           SUM(ACTIVE) AS active
    FROM BIKE_SHARING_SYSTEMS
    GROUP BY COUNTRY
    ORDER BY systems DESC
    LIMIT 10",

  launches_by_year = "
    SELECT LAUNCH_YEAR, COUNT(*) AS systems
    FROM BIKE_SHARING_SYSTEMS
    WHERE LAUNCH_YEAR IS NOT NULL
    GROUP BY LAUNCH_YEAR
    ORDER BY LAUNCH_YEAR",

  korean_systems = "
    SELECT CITY, NAME, LAUNCHED, ACTIVE
    FROM BIKE_SHARING_SYSTEMS
    WHERE COUNTRY = 'South Korea'
    ORDER BY LAUNCH_YEAR"
)

if (file.exists("data/clean/weather_forecast.csv")) {
  forecast <- read_csv("data/clean/weather_forecast.csv",
                       col_types = cols(LOCAL_TIME = col_character()),
                       show_col_types = FALSE)
  dbWriteTable(con, "WEATHER_FORECAST", forecast)

  queries$seoul_next_hours <- "
    SELECT LOCAL_TIME, TEMPERATURE_C, HUMIDITY, WIND_SPEED_M_S, RAINFALL_MM
    FROM WEATHER_FORECAST
    WHERE CITY = 'Seoul'
    ORDER BY LOCAL_TIME
    LIMIT 3"
}

for (name in names(queries)) {
  result <- as_tibble(dbGetQuery(con, queries[[name]]))
  cat("\n--", name, "\n")
  print(result, n = 15)
  write_csv(result, file.path("results", paste0(name, ".csv")))
}

dbDisconnect(con)
