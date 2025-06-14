# ===============================================================================
# BIKE SHARING DEMAND ANALYSIS PROJECT
# Phase 3: SQL Analysis
# ===============================================================================

# Load required libraries
library(DBI)
library(RSQLite)
library(dplyr)
library(readr)

cat("Starting SQL Analysis...\n")

# ===============================================================================
# 1. DATABASE SETUP
# ===============================================================================

cat("\n1. Setting up database and loading data...\n")

# Create SQLite database connection
con <- dbConnect(RSQLite::SQLite(), ":memory:")

# Load cleaned datasets
bike_systems <- read_csv("data/clean/bike_sharing_systems_clean.csv")
weather_forecast <- read_csv("data/clean/cities_weather_forecast_clean.csv")
world_cities <- read_csv("data/clean/world_cities_clean.csv")
seoul_bikes <- read_csv("data/clean/seoul_bike_sharing_clean.csv")

# Create tables in database
dbWriteTable(con, "BIKE_SHARING_SYSTEMS", bike_systems, overwrite = TRUE)
dbWriteTable(con, "CITIES_WEATHER_FORECAST", weather_forecast, overwrite = TRUE)
dbWriteTable(con, "WORLD_CITIES", world_cities, overwrite = TRUE)
dbWriteTable(con, "SEOUL_BIKE_SHARING", seoul_bikes, overwrite = TRUE)

cat("  ✓ Database created and tables loaded\n")
cat("    - BIKE_SHARING_SYSTEMS:", nrow(bike_systems), "rows\n")
cat("    - CITIES_WEATHER_FORECAST:", nrow(weather_forecast), "rows\n")
cat("    - WORLD_CITIES:", nrow(world_cities), "rows\n")
cat("    - SEOUL_BIKE_SHARING:", nrow(seoul_bikes), "rows\n")

# ===============================================================================
# 2. SQL ANALYSIS TASKS (11 TASKS)
# ===============================================================================

cat("\n2. Executing SQL Analysis Tasks...\n")

# TASK 1: Record Count
cat("\nTask 1 - Record Count\n")
task1_query <- "
SELECT COUNT(*) as total_records 
FROM SEOUL_BIKE_SHARING
"
task1_result <- dbGetQuery(con, task1_query)
cat("  Total records in seoul_bike_sharing:", task1_result$total_records, "\n")

# TASK 2: Operating Hours  
cat("\nTask 2 - Operating Hours\n")
task2_query <- "
SELECT COUNT(*) as non_zero_hours
FROM SEOUL_BIKE_SHARING 
WHERE RENTED_BIKE_COUNT > 0
"
task2_result <- dbGetQuery(con, task2_query)
cat("  Hours with non-zero bike rentals:", task2_result$non_zero_hours, "\n")

# TASK 3: Weather Forecast (Seoul next 3 hours)
cat("\nTask 3 - Weather Forecast\n")
task3_query <- "
SELECT CITY, DATETIME, TEMPERATURE, HUMIDITY, WIND_SPEED, WEATHER_MAIN, WEATHER_DESCRIPTION
FROM CITIES_WEATHER_FORECAST 
WHERE CITY = 'Seoul'
ORDER BY DATETIME 
LIMIT 1
"
task3_result <- dbGetQuery(con, task3_query)
cat("  Seoul weather forecast (next 3 hours):\n")
if (nrow(task3_result) > 0) {
  cat("    Temperature:", task3_result$TEMPERATURE, "°C\n")
  cat("    Humidity:", task3_result$HUMIDITY, "%\n")
  cat("    Wind Speed:", task3_result$WIND_SPEED, "m/s\n")
  cat("    Weather:", task3_result$WEATHER_DESCRIPTION, "\n")
} else {
  cat("    No Seoul weather data available\n")
}

# TASK 4: Seasons
cat("\nTask 4 - Seasons\n")
task4_query <- "
SELECT DISTINCT SEASONS 
FROM SEOUL_BIKE_SHARING 
ORDER BY SEASONS
"
task4_result <- dbGetQuery(con, task4_query)
cat("  Seasons in dataset:", paste(task4_result$SEASONS, collapse = ", "), "\n")

# TASK 5: Date Range
cat("\nTask 5 - Date Range\n")
task5_query <- "
SELECT 
    MIN(DATE) as first_date,
    MAX(DATE) as last_date
FROM SEOUL_BIKE_SHARING
"
task5_result <- dbGetQuery(con, task5_query)
cat("  Date range: from", task5_result$first_date, "to", task5_result$last_date, "\n")

# TASK 6: Peak Rental Time
cat("\nTask 6 - Subquery - 'historical maximum'\n")
task6_query <- "
SELECT DATE, HOUR, RENTED_BIKE_COUNT
FROM SEOUL_BIKE_SHARING 
WHERE RENTED_BIKE_COUNT = (SELECT MAX(RENTED_BIKE_COUNT) FROM SEOUL_BIKE_SHARING)
"
task6_result <- dbGetQuery(con, task6_query)
cat("  Peak rental time:\n")
cat("    Date:", task6_result$DATE, "\n")
cat("    Hour:", task6_result$HOUR, "\n") 
cat("    Bike Count:", task6_result$RENTED_BIKE_COUNT, "\n")

# TASK 7: Hourly Popularity and Temperature by Season
cat("\nTask 7 - Hourly popularity and temperature by season\n")
task7_query <- "
SELECT 
    SEASONS,
    HOUR,
    ROUND(AVG(TEMPERATURE_C), 2) as avg_temperature,
    ROUND(AVG(RENTED_BIKE_COUNT), 2) as avg_bike_count
FROM SEOUL_BIKE_SHARING 
GROUP BY SEASONS, HOUR
ORDER BY avg_bike_count DESC
LIMIT 10
"
task7_result <- dbGetQuery(con, task7_query)
cat("  Top 10 hourly averages by season:\n")
for (i in 1:nrow(task7_result)) {
  cat("   ", i, ".", task7_result$SEASONS[i], "at", task7_result$HOUR[i], "h:", 
      task7_result$avg_bike_count[i], "bikes,", task7_result$avg_temperature[i], "°C\n")
}

# TASK 8: Seasonal Rental Patterns
cat("\nTask 8 - Rental Seasonality\n")
task8_query <- "
SELECT 
    SEASONS,
    ROUND(AVG(RENTED_BIKE_COUNT), 2) as avg_count,
    MIN(RENTED_BIKE_COUNT) as min_count,
    MAX(RENTED_BIKE_COUNT) as max_count,
    ROUND(
        SQRT(AVG((RENTED_BIKE_COUNT - (SELECT AVG(RENTED_BIKE_COUNT) FROM SEOUL_BIKE_SHARING s2 WHERE s2.SEASONS = s1.SEASONS)) * 
                 (RENTED_BIKE_COUNT - (SELECT AVG(RENTED_BIKE_COUNT) FROM SEOUL_BIKE_SHARING s3 WHERE s3.SEASONS = s1.SEASONS)))), 2
    ) as std_dev
FROM SEOUL_BIKE_SHARING s1
GROUP BY SEASONS
ORDER BY avg_count DESC
"
task8_result <- dbGetQuery(con, task8_query)
cat("  Seasonal rental statistics:\n")
for (i in 1:nrow(task8_result)) {
  cat("   ", task8_result$SEASONS[i], "- Avg:", task8_result$avg_count[i], 
      ", Min:", task8_result$min_count[i], ", Max:", task8_result$max_count[i], 
      ", StdDev:", task8_result$std_dev[i], "\n")
}

# TASK 9: Weather Seasonality
cat("\nTask 9 - Weather Seasonality\n")
task9_query <- "
SELECT 
    SEASONS,
    ROUND(AVG(TEMPERATURE_C), 2) as avg_temperature,
    ROUND(AVG(HUMIDITY), 2) as avg_humidity,
    ROUND(AVG(WIND_SPEED_M_S), 2) as avg_wind_speed,
    ROUND(AVG(VISIBILITY_10M), 2) as avg_visibility,
    ROUND(AVG(DEW_POINT_TEMPERATURE_C), 2) as avg_dew_point,
    ROUND(AVG(SOLAR_RADIATION_MJ_M2), 2) as avg_solar_radiation,
    ROUND(AVG(RAINFALL_MM), 2) as avg_precipitation,
    ROUND(AVG(SNOWFALL_CM), 2) as avg_snowfall,
    ROUND(AVG(RENTED_BIKE_COUNT), 2) as avg_bike_count
FROM SEOUL_BIKE_SHARING 
GROUP BY SEASONS
ORDER BY avg_bike_count DESC
"
task9_result <- dbGetQuery(con, task9_query)
cat("  Weather patterns by season (ordered by bike count):\n")
for (i in 1:nrow(task9_result)) {
  cat("   ", task9_result$SEASONS[i], ":\n")
  cat("     Bikes:", task9_result$avg_bike_count[i], 
      ", Temp:", task9_result$avg_temperature[i], "°C",
      ", Humidity:", task9_result$avg_humidity[i], "%\n")
  cat("     Wind:", task9_result$avg_wind_speed[i], "m/s",
      ", Visibility:", task9_result$avg_visibility[i], 
      ", Precipitation:", task9_result$avg_precipitation[i], "mm\n")
}

# TASK 10: Total bicycle count and information about the city of Seoul
task10_query <- "
SELECT 
    w.city as city,
    w.country as country,
    w.lat as lat,
    w.lng as lon,
    w.population as population,
    b.TOTAL_BIKES as total_bikes
FROM WORLD_CITIES w
LEFT JOIN BIKE_SHARING_SYSTEMS b 
    ON UPPER(w.city) = UPPER(b.CITY) AND UPPER(w.country) = UPPER(b.COUNTRY)
WHERE UPPER(w.city) = 'SEOUL'
"
task10_result <- dbGetQuery(con, task10_query)
if (nrow(task10_result) > 0) {
  cat("  Seoul city information:\n")
  cat("    City:", task10_result$city, "\n")
  cat("    Country:", task10_result$country, "\n") 
  cat("    Coordinates:", task10_result$lat, ",", task10_result$lon, "\n")
  cat("    Population:", formatC(task10_result$population, format='d', big.mark=','), "\n")
  cat("    Total Bikes:", formatC(task10_result$total_bikes, format='d', big.mark=','), "\n")
} else {
  cat("  Seoul information not found in world cities dataset\n")
}

# TASK 11: Cities with Similar Bike Fleet Size
cat("\nTask 11 - Find cities with comparable bicycle scale\n")
task11_query <- "
SELECT 
    b.CITY as city,
    b.COUNTRY as country,
    w.LAT as lat,
    w.LNG as lng,
    w.POPULATION as population,
    b.TOTAL_BIKES as total_bikes
FROM BIKE_SHARING_SYSTEMS b
LEFT JOIN WORLD_CITIES w
    ON UPPER(b.CITY) = UPPER(w.CITY) AND UPPER(b.COUNTRY) = UPPER(b.COUNTRY)
WHERE b.TOTAL_BIKES BETWEEN 15000 AND 20000
ORDER BY b.TOTAL_BIKES DESC
"
task11_result <- dbGetQuery(con, task11_query)
cat("  Cities with bike fleets between 15,000 and 20,000:\n")
if (nrow(task11_result) > 0) {
    for (i in 1:nrow(task11_result)) {
        cat("   ", task11_result$city[i], ",", task11_result$country[i], 
            "- Bikes:", formatC(task11_result$total_bikes[i], format='d', big.mark=','),
            ", Pop:", formatC(task11_result$population[i], format='d', big.mark=','), "\n")
    }
} else {
    cat("   No cities found in this range.\n")
}

# ===============================================================================
# 3. SAVE RESULTS
# ===============================================================================

cat("\n3. Saving SQL analysis results...\n")

# Create results directory
if (!dir.exists("results")) {
  dir.create("results")
}

# Save all results to CSV files
write_csv(task1_result, "results/task1_record_count.csv")
write_csv(task2_result, "results/task2_operating_hours.csv") 
write_csv(task3_result, "results/task3_weather_forecast.csv")
write_csv(task4_result, "results/task4_seasons.csv")
write_csv(task5_result, "results/task5_date_range.csv")
write_csv(task6_result, "results/task6_peak_rental.csv")
write_csv(task7_result, "results/task7_hourly_patterns.csv")
write_csv(task8_result, "results/task8_seasonal_rentals.csv")
write_csv(task9_result, "results/task9_weather_seasonality.csv")
write_csv(task10_result, "results/task10_seoul_info.csv")
write_csv(task11_result, "results/task11_similar_cities.csv")

cat("  ✓ All task results saved to results/ directory\n")

# ===============================================================================
# 4. CLEANUP
# ===============================================================================

# Close database connection properly
tryCatch({
  if (DBI::dbIsValid(con)) {
    dbDisconnect(con)
    cat("  ✓ Database connection closed successfully\n")
  }
}, error = function(e) {
  cat("  ⚠️ Warning closing database:", e$message, "\n")
}, finally = {
  # Ensure connection is cleaned up
  if (exists("con")) {
    try(dbDisconnect(con), silent = TRUE)
  }
})

cat("\n", paste(rep("=", 50), collapse=""), "\n")
cat("SQL ANALYSIS SUMMARY\n")
cat(paste(rep("=", 50), collapse=""), "\n")

cat("Completed SQL Tasks:\n")
cat("  ✓ Task 1: Record count analysis\n")
cat("  ✓ Task 2: Operating hours analysis\n")
cat("  ✓ Task 3: Weather forecast for Seoul\n")
cat("  ✓ Task 4: Seasons identification\n")
cat("  ✓ Task 5: Date range analysis\n")
cat("  ✓ Task 6: Peak rental time identification\n")
cat("  ✓ Task 7: Hourly popularity by season\n")
cat("  ✓ Task 8: Seasonal rental patterns\n")
cat("  ✓ Task 9: Weather seasonality analysis\n")
cat("  ✓ Task 10: Seoul city information\n")
cat("  ✓ Task 11: Cities with similar scale\n")

cat("\nNext steps:\n")
cat("1. Proceed to Phase 4: Exploratory Data Analysis with Visualization\n")
cat("2. Run the EDA script: source('04_eda_visualization.R')\n")

cat("\nSQL analysis completed!\n") 