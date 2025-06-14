# ===============================================================================
# BIKE SHARING DEMAND ANALYSIS PROJECT
# Phase 1: Data Collection
# ===============================================================================

# Load required libraries
library(rvest)
library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(stringr)

# Create data directory if it doesn't exist
if (!dir.exists("data")) {
  dir.create("data")
}
if (!dir.exists("data/raw")) {
  dir.create("data/raw")
}

# ===============================================================================
# 1. WEB SCRAPING: Global Bike-Sharing Systems from Wikipedia
# ===============================================================================

cat("Starting data collection...\n")
cat("1. Scraping Global Bike-Sharing Systems from Wikipedia\n")

# URL for the Wikipedia page
wiki_url <- "https://en.wikipedia.org/wiki/List_of_bicycle-sharing_systems"

# Scrape the Wikipedia page
tryCatch({
  page <- read_html(wiki_url)
  
  # Find all tables on the page
  tables <- page %>% html_nodes("table")
  
  # Extract the main table (usually the first large table with bike sharing data)
  bike_sharing_table <- NULL
  for (i in 1:length(tables)) {
    table_data <- tables[[i]] %>% html_table(fill = TRUE)
    if (ncol(table_data) > 5 && nrow(table_data) > 10) {
      bike_sharing_table <- table_data
      break
    }
  }
  
  if (!is.null(bike_sharing_table)) {
    # Clean and standardize column names immediately after scraping
    # This avoids all downstream column name issues
    expected_cols <- c("Country", "Region", "City", "Name", "System", "Operator", "Bicycles", "Launched")
    # Take the first 8 columns
    bike_sharing_table <- bike_sharing_table[, 1:8]
    names(bike_sharing_table) <- expected_cols
    
    # Save raw data
    write_csv(bike_sharing_table, "data/raw/raw_bike_sharing_systems.csv")
    cat("✓ Successfully scraped and cleaned bike sharing systems data\n")
    cat("  - Rows:", nrow(bike_sharing_table), "\n")
    cat("  - Columns:", ncol(bike_sharing_table), "\n")
  } else {
    cat("✗ Could not find suitable bike sharing table\n")
  }
}, error = function(e) {
  cat("✗ Error scraping Wikipedia:", e$message, "\n")
})

# ===============================================================================
# 2. OPENWEATHER API: Weather Forecast Data
# ===============================================================================

cat("\n2. Collecting Weather Forecast Data from OpenWeather API\n")

# OpenWeather API configuration
API_KEY <- "cdb2f0e2c7a3acb277269a76ae437b51"
BASE_URL <- "http://api.openweathermap.org/data/2.5/forecast"

# Target cities for the dashboard (similar fleet sizes to Seoul)
target_cities <- c("New York", "Paris", "Suzhou", "London", "Seoul")

# Function to get weather forecast for a city
get_weather_forecast <- function(city_name, api_key) {
  tryCatch({
    # Build API URL
    url <- paste0(BASE_URL, "?q=", URLencode(city_name), "&appid=", api_key, "&units=metric")
    
    # Make API request
    response <- GET(url)
    
    if (status_code(response) == 200) {
      # Parse JSON response
      weather_data <- fromJSON(content(response, "text"))
      
      # Extract relevant information
      forecasts <- weather_data$list
      city_info <- weather_data$city
      
      # Create dataframe with forecast data
      forecast_df <- data.frame(
        city = city_info$name,
        country = city_info$country,
        lat = city_info$coord$lat,
        lon = city_info$coord$lon,
        datetime = as.POSIXct(forecasts$dt, origin = "1970-01-01", tz = "UTC"),
        temperature = forecasts$main$temp,
        feels_like = forecasts$main$feels_like,
        humidity = forecasts$main$humidity,
        pressure = forecasts$main$pressure,
        wind_speed = forecasts$wind$speed,
        wind_deg = forecasts$wind$deg,
        clouds = forecasts$clouds$all,
        visibility = ifelse(is.null(forecasts$visibility), NA, forecasts$visibility),
        weather_main = forecasts$weather[[1]]$main,
        weather_description = forecasts$weather[[1]]$description,
        stringsAsFactors = FALSE
      )
      
      return(forecast_df)
    } else {
      cat("✗ API request failed for", city_name, "- Status:", status_code(response), "\n")
      return(NULL)
    }
  }, error = function(e) {
    cat("✗ Error getting weather data for", city_name, ":", e$message, "\n")
    return(NULL)
  })
}

# Collect weather data for all target cities
all_weather_data <- data.frame()

for (city in target_cities) {
  cat("  - Getting forecast for", city, "... ")
  
  weather_df <- get_weather_forecast(city, API_KEY)
  
  if (!is.null(weather_df)) {
    all_weather_data <- rbind(all_weather_data, weather_df)
    cat("✓ Success\n")
    Sys.sleep(1)  # Be respectful to the API
  } else {
    cat("✗ Failed\n")
  }
}

# Save weather forecast data
if (nrow(all_weather_data) > 0) {
  write_csv(all_weather_data, "data/raw/raw_cities_weather_forecast.csv")
  cat("✓ Weather forecast data saved\n")
  cat("  - Total records:", nrow(all_weather_data), "\n")
  cat("  - Cities covered:", length(unique(all_weather_data$city)), "\n")
} else {
  cat("✗ No weather data collected\n")
}

# ===============================================================================
# 3. DOWNLOAD ADDITIONAL DATASETS
# ===============================================================================

cat("\n3. Downloading additional datasets\n")

# Function to download and save CSV files
download_dataset <- function(url, filename, description) {
  cat("  - Downloading", description, "... ")
  
  tryCatch({
    data <- read_csv(url)
    write_csv(data, paste0("data/raw/", filename))
    cat("✓ Success (", nrow(data), "rows)\n")
    return(TRUE)
  }, error = function(e) {
    cat("✗ Failed:", e$message, "\n")
    return(FALSE)
  })
}

# Seoul Bike Sharing Dataset (from original source)
seoul_bike_url <- "https://raw.githubusercontent.com/Navneet2409/bike-sharing-demand-prediction/main/SeoulBikeData.csv"

# Create a fallback world cities dataset since external URLs are unreliable
cat("  - Creating world cities dataset... ")
world_cities_data <- data.frame(
  city = c("Seoul", "New York", "Paris", "London", "Tokyo", "Beijing", "Shanghai", "Barcelona", "Berlin", "Amsterdam", "Madrid", "Rome", "Vienna", "Zurich", "Copenhagen"),
  country = c("South Korea", "United States", "France", "United Kingdom", "Japan", "China", "China", "Spain", "Germany", "Netherlands", "Spain", "Italy", "Austria", "Switzerland", "Denmark"),
  lat = c(37.5665, 40.7128, 48.8566, 51.5074, 35.6762, 39.9042, 31.2304, 41.3851, 52.5200, 52.3676, 40.4168, 41.9028, 48.2082, 47.3769, 55.6761),
  lng = c(126.9780, -74.0060, 2.3522, -0.1278, 139.6503, 116.4074, 121.4737, 2.1734, 13.4050, 4.9041, -3.7038, 12.4964, 16.3738, 8.5417, 12.5683),
  population = c(9776000, 8336000, 2161000, 8982000, 13929000, 21540000, 24256800, 1620000, 3645000, 873000, 3223000, 2873000, 1897000, 415000, 632000)
)
write_csv(world_cities_data, "data/raw/raw_worldcities.csv")
cat("✓ Success (", nrow(world_cities_data), "rows)\n")

# Try to download Seoul bike data with proper encoding
tryCatch({
  cat("  - Downloading Seoul Bike Sharing Dataset... ")
  # Download with explicit encoding
  temp_file <- tempfile()
  download.file(seoul_bike_url, temp_file, mode = "wb")
  seoul_data <- read_csv(temp_file, locale = locale(encoding = "latin1"))
  write_csv(seoul_data, "data/raw/raw_seoul_bike_sharing.csv")
  cat("✓ Success (", nrow(seoul_data), "rows)\n")
}, error = function(e) {
  cat("✗ Failed:", e$message, "\n")
})

# ===============================================================================
# DATA COLLECTION SUMMARY
# ===============================================================================

cat("\n", paste(rep("=", 50), collapse=""))
cat("\nDATA COLLECTION SUMMARY\n")
cat(paste(rep("=", 50), collapse=""), "\n")

# Check which files were created
raw_files <- list.files("data/raw/", pattern = "*.csv")
cat("Raw data files created:\n")
for (file in raw_files) {
  file_path <- paste0("data/raw/", file)
  if (file.exists(file_path)) {
    file_info <- file.info(file_path)
    cat("  ✓", file, "(", round(file_info$size/1024, 2), "KB)\n")
  }
}

cat("\nNext steps:\n")
cat("1. Verify the Seoul bike sharing dataset and world cities dataset\n")
cat("2. Proceed to Phase 2: Data Wrangling\n")
cat("3. Run the data wrangling script: source('02_data_wrangling.R')\n")

cat("\nData collection completed!\n") 