# ===============================================================================
# BIKE SHARING DEMAND ANALYSIS PROJECT
# Phase 2: Data Wrangling
# ===============================================================================

# Load required libraries
library(dplyr)
library(readr)
library(stringr)
library(tidyr)

# Create cleaned data directory
if (!dir.exists("data/clean")) {
  dir.create("data/clean")
}

cat("Starting data wrangling...\n")

# ===============================================================================
# 1. STRING PROCESSING & REGEX OPERATIONS
# ===============================================================================

cat("\n1. String Processing & Regex Operations\n")

# Load all raw datasets
cat("  - Loading raw datasets...\n")
bike_systems <- read_csv("data/raw/raw_bike_sharing_systems.csv")
weather_forecast <- read_csv("data/raw/raw_cities_weather_forecast.csv")
world_cities <- read_csv("data/raw/raw_worldcities.csv")
seoul_bikes <- read_csv("data/raw/raw_seoul_bike_sharing.csv", locale = locale(encoding = "latin1"))

cat("    ✓ Bike sharing systems:", nrow(bike_systems), "rows\n")
cat("    ✓ Weather forecast:", nrow(weather_forecast), "rows\n")
cat("    ✓ World cities:", nrow(world_cities), "rows\n")
cat("    ✓ Seoul bike sharing:", nrow(seoul_bikes), "rows\n")

# TASK: Standardize column names for all datasets
cat("  - Standardizing column names...\n")

# Create a function to standardize column names
standardize_column_names <- function(df) {
  # For bike_systems, names are already clean from the collection script
  if ("Bicycles" %in% names(df)) {
      # This is the bike_systems dataframe, handle it manually
      names(df) <- names(df) %>%
        str_to_upper()
      return(df)
  }

  names(df) <- names(df) %>%
    str_to_upper() %>%                    # Convert to uppercase
    str_replace_all("[^A-Z0-9]+", "_") %>% # Replace one or more non-alphanumeric characters with a single underscore
    str_replace_all("_+", "_") %>%        # Replace multiple underscores with single
    str_replace_all("^_|_$", "")          # Remove leading/trailing underscores
  return(df)
}

# Apply standardization to all datasets
bike_systems <- standardize_column_names(bike_systems)
weather_forecast <- standardize_column_names(weather_forecast)
world_cities <- standardize_column_names(world_cities)
seoul_bikes <- standardize_column_names(seoul_bikes)

cat("    ✓ Column names standardized for all datasets\n")

# TASK: Remove unwanted reference links using regex
cat("  - Removing reference links using regex...\n")

# Function to remove reference links (e.g., [1], [ref], etc.)
remove_reference_links <- function(text_vector) {
  if (!is.character(text_vector)) return(text_vector)
  
  cleaned <- text_vector %>%
    str_replace_all("\\[\\d+\\]", "") %>%        # Remove [1], [12], etc.
    str_replace_all("\\[ref\\]", "") %>%         # Remove [ref]
    str_replace_all("\\[citation needed\\]", "") %>%  # Remove [citation needed]
    str_replace_all("\\[\\w+\\]", "") %>%        # Remove other bracketed references
    str_trim()                                   # Remove leading/trailing whitespace
  
  return(cleaned)
}

# Apply to character columns in bike_systems (most likely to have references)
bike_systems <- bike_systems %>%
  mutate_if(is.character, remove_reference_links)

cat("    ✓ Reference links removed\n")

# TASK: Extract numerical values using regex
cat("  - Extracting numerical values using regex...\n")

# This task is critical for joining bike system data later.
# The 'BICYCLES' column from Wikipedia is now clean from the previous script.

if ("BICYCLES" %in% names(bike_systems)) {
    cat("    - Cleaning the BICYCLES column to extract bike counts...\n")
    
    # Keep the original for reference
    bike_systems$BICYCLES_ORIGINAL <- bike_systems$BICYCLES

    # Robustly extract numbers:
    # 1. Remove commas from numbers (e.g., "15,000" -> "15000")
    # 2. Extract the first numerical group. This handles cases like "7500 (+300 electric)"
    # 3. Convert to numeric type
    bike_systems$TOTAL_BIKES <- bike_systems$BICYCLES %>%
        str_replace_all(",", "") %>%
        str_extract("^[0-9]+") %>%
        as.numeric()

    cat("    ✓ TOTAL_BIKES column created with numeric values\n")
    # Report how many were successfully converted
    successful_conversions <- sum(!is.na(bike_systems$TOTAL_BIKES))
    cat("      -", successful_conversions, "of", nrow(bike_systems), "rows converted to numbers\n")

} else {
    cat("    ! Warning: 'BICYCLES' column not found in bike_systems data. SQL joins might fail.\n")
}

# For weather data, ensure all numeric columns are properly formatted
weather_forecast <- weather_forecast %>%
  mutate_at(vars(matches("TEMPERATURE|HUMIDITY|WIND|PRESSURE|CLOUDS")), 
            ~ as.numeric(as.character(.)))

cat("    ✓ Numerical values extracted and converted\n")

# ===============================================================================
# 2. DATA CLEANING WITH DPLYR
# ===============================================================================

cat("\n2. Data Cleaning with dplyr\n")

# TASK: Detect and handle missing values
cat("  - Detecting and handling missing values...\n")

# Function to report missing values
report_missing <- function(df, dataset_name) {
  missing_summary <- df %>%
    summarise_all(~ sum(is.na(.))) %>%
    gather(key = "column", value = "missing_count") %>%
    mutate(missing_percent = round(missing_count / nrow(df) * 100, 2)) %>%
    filter(missing_count > 0)
  
  if (nrow(missing_summary) > 0) {
    cat("    ", dataset_name, "missing values:\n")
    for (i in 1:nrow(missing_summary)) {
      cat("      -", missing_summary$column[i], ":", missing_summary$missing_count[i], 
          "(", missing_summary$missing_percent[i], "%)\n")
    }
  } else {
    cat("    ✓", dataset_name, "- No missing values\n")
  }
  
  return(missing_summary)
}

# Check missing values in all datasets
missing_bike_systems <- report_missing(bike_systems, "Bike Systems")
missing_weather <- report_missing(weather_forecast, "Weather Forecast")
missing_cities <- report_missing(world_cities, "World Cities")
missing_seoul <- report_missing(seoul_bikes, "Seoul Bikes")

# Handle missing values in Seoul bikes dataset (most important for modeling)
seoul_bikes_clean <- seoul_bikes %>%
  # Remove rows where the target variable (bike count) is missing
  filter(!is.na(RENTED_BIKE_COUNT)) %>%
  # For weather variables, use median imputation for small amounts of missing data
  mutate(
    TEMPERATURE_C = ifelse(is.na(TEMPERATURE_C), median(TEMPERATURE_C, na.rm = TRUE), TEMPERATURE_C),
    HUMIDITY = ifelse(is.na(HUMIDITY), median(HUMIDITY, na.rm = TRUE), HUMIDITY),
    WIND_SPEED_M_S = ifelse(is.na(WIND_SPEED_M_S), median(WIND_SPEED_M_S, na.rm = TRUE), WIND_SPEED_M_S),
    VISIBILITY_10M = ifelse(is.na(VISIBILITY_10M), median(VISIBILITY_10M, na.rm = TRUE), VISIBILITY_10M),
    DEW_POINT_TEMPERATURE_C = ifelse(is.na(DEW_POINT_TEMPERATURE_C), median(DEW_POINT_TEMPERATURE_C, na.rm = TRUE), DEW_POINT_TEMPERATURE_C),
    SOLAR_RADIATION_MJ_M2 = ifelse(is.na(SOLAR_RADIATION_MJ_M2), median(SOLAR_RADIATION_MJ_M2, na.rm = TRUE), SOLAR_RADIATION_MJ_M2),
    RAINFALL_MM = ifelse(is.na(RAINFALL_MM), 0, RAINFALL_MM),  # Assume 0 if missing
    SNOWFALL_CM = ifelse(is.na(SNOWFALL_CM), 0, SNOWFALL_CM)   # Assume 0 if missing
  )

cat("    ✓ Missing values handled\n")

# TASK: Create dummy variables for categorical variables
cat("  - Creating dummy variables for categorical variables...\n")

# Create dummy variables for Seoul bikes dataset
seoul_bikes_with_dummies <- seoul_bikes_clean %>%
  # Create season dummies
  mutate(
    SEASON_WINTER = ifelse(SEASONS == "Winter", 1, 0),
    SEASON_SPRING = ifelse(SEASONS == "Spring", 1, 0),
    SEASON_SUMMER = ifelse(SEASONS == "Summer", 1, 0),
    SEASON_AUTUMN = ifelse(SEASONS == "Autumn", 1, 0),
    
    # Create holiday dummy
    IS_HOLIDAY = ifelse(HOLIDAY == "Holiday", 1, 0),
    
    # Create functioning day dummy
    IS_FUNCTIONING_DAY = ifelse(FUNCTIONING_DAY == "Yes", 1, 0),
    
    # Create hour categories (useful for modeling)
    HOUR_CATEGORY = case_when(
      HOUR %in% c(6, 7, 8, 9) ~ "MORNING_RUSH",
      HOUR %in% c(17, 18, 19) ~ "EVENING_RUSH", 
      HOUR %in% c(10, 11, 12, 13, 14, 15, 16) ~ "DAYTIME",
      HOUR %in% c(20, 21, 22, 23) ~ "EVENING",
      TRUE ~ "NIGHT"
    )
  ) %>%
  # Create hour category dummies
  mutate(
    HOUR_MORNING_RUSH = ifelse(HOUR_CATEGORY == "MORNING_RUSH", 1, 0),
    HOUR_EVENING_RUSH = ifelse(HOUR_CATEGORY == "EVENING_RUSH", 1, 0),
    HOUR_DAYTIME = ifelse(HOUR_CATEGORY == "DAYTIME", 1, 0),
    HOUR_EVENING = ifelse(HOUR_CATEGORY == "EVENING", 1, 0),
    HOUR_NIGHT = ifelse(HOUR_CATEGORY == "NIGHT", 1, 0)
  )

cat("    ✓ Dummy variables created\n")

# TASK: Normalize numerical data
cat("  - Normalizing numerical data...\n")

# Function to normalize (min-max scaling)
normalize_minmax <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# Function to standardize (z-score)
standardize_zscore <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

# Create normalized version of numerical variables for Seoul bikes
seoul_bikes_normalized <- seoul_bikes_with_dummies %>%
  mutate(
    # Min-max normalization for bounded variables
    HUMIDITY_NORM = normalize_minmax(HUMIDITY),
    HOUR_NORM = normalize_minmax(HOUR),
    
    # Z-score standardization for unbounded variables
    TEMPERATURE_STD = standardize_zscore(TEMPERATURE_C),
    WIND_SPEED_STD = standardize_zscore(WIND_SPEED_M_S),
    VISIBILITY_STD = standardize_zscore(VISIBILITY_10M),
    DEW_POINT_STD = standardize_zscore(DEW_POINT_TEMPERATURE_C),
    SOLAR_RADIATION_STD = standardize_zscore(SOLAR_RADIATION_MJ_M2),
    RAINFALL_STD = standardize_zscore(RAINFALL_MM),
    SNOWFALL_STD = standardize_zscore(SNOWFALL_CM)
  )

cat("    ✓ Numerical data normalized\n")

# ===============================================================================
# 3. SAVE CLEANED DATASETS
# ===============================================================================

cat("\n3. Saving cleaned datasets...\n")

# Save all cleaned datasets
write_csv(bike_systems, "data/clean/bike_sharing_systems_clean.csv")
write_csv(weather_forecast, "data/clean/cities_weather_forecast_clean.csv")
write_csv(world_cities, "data/clean/world_cities_clean.csv")
write_csv(seoul_bikes_normalized, "data/clean/seoul_bike_sharing_clean.csv")

cat("  ✓ bike_sharing_systems_clean.csv saved\n")
cat("  ✓ cities_weather_forecast_clean.csv saved\n")
cat("  ✓ world_cities_clean.csv saved\n")
cat("  ✓ seoul_bike_sharing_clean.csv saved\n")

# ===============================================================================
# 4. DATA WRANGLING SUMMARY
# ===============================================================================

cat("\n", paste(rep("=", 50), collapse=""), "\n")
cat("DATA WRANGLING SUMMARY\n")
cat(paste(rep("=", 50), collapse=""), "\n")

cat("Raw Data:\n")
cat("  - Bike sharing systems: ", nrow(bike_systems), "rows,", ncol(bike_systems), "columns\n")
cat("  - Weather forecast: ", nrow(weather_forecast), "rows,", ncol(weather_forecast), "columns\n")
cat("  - World cities: ", nrow(world_cities), "rows,", ncol(world_cities), "columns\n")
cat("  - Seoul bikes: ", nrow(seoul_bikes_normalized), "rows,", ncol(seoul_bikes_normalized), "columns\n")

cat("\nData Processing Completed:\n")
cat("  ✓ Column names standardized\n")
cat("  ✓ Reference links removed\n") 
cat("  ✓ Numerical values extracted\n")
cat("  ✓ Missing values handled\n")
cat("  ✓ Dummy variables created\n")
cat("  ✓ Data normalized\n")

cat("\nNext steps:\n")
cat("1. Proceed to Phase 3: SQL Analysis\n")
cat("2. Run the SQL analysis script: source('03_sql_analysis.R')\n")

cat("\nData wrangling completed!\n") 