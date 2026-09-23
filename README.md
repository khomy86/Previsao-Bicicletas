# Bike-Sharing Demand Forecast

Analysis and forecasting of bike-sharing demand, based on Seoul's hourly rental
data (2017–2018). The project covers the full workflow in R: data collection,
cleaning, SQL, exploratory analysis, modeling with tidymodels, and a Shiny
dashboard that applies the best model to the 5-day weather forecast of several
cities.

## Data

- **Seoul Bike Sharing Demand** ([UCI](https://archive.ics.uci.edu/dataset/560/seoul+bike+sharing+demand)):
  hourly rentals over one year, with the weather for each hour.
- **Bike-sharing systems** ([Wikipedia](https://en.wikipedia.org/wiki/List_of_bicycle-sharing_systems)):
  scraped table with country, city, launch date and status of each system.
- **5-day weather forecast** ([OpenWeather](https://openweathermap.org/forecast5)):
  3-hour steps for 12 cities.

## Pipeline

| Script | What it does |
|---|---|
| `01_data_collection.R` | Scrapes Wikipedia, downloads the Seoul data and calls the OpenWeather API |
| `02_data_wrangling.R` | Standardizes column names, strips citation markers with regex, extracts launch years, parses dates, fills missing values; the forecast is interpolated to hourly steps and shifted to each city's local time |
| `03_sql_analysis.R` | SQLite queries (aggregations, subqueries, seasonal statistics); results go to `results/` |
| `04_eda_visualization.R` | Exploratory plots with ggplot2; saved to `visualizations/` |
| `05_regression_modeling.R` | Trains and compares 8 models; the best one is saved to `models/` |
| `06_shiny_dashboard.R` | Interactive dashboard |

## Results

Test set metrics (20% of the data, days when the system was operating):

| Model | RMSE | R² |
|---|---|---|
| Random Forest (ranger) | 228 | 0.871 |
| Linear with hour × temperature interactions | 339 | 0.716 |
| Polynomial | 366 | 0.668 |
| Linear (all variables) | 377 | 0.648 |
| Lasso / Ridge | 377 | 0.648 |
| Linear (hour and season) | 434 | 0.533 |
| Linear (weather only) | 483 | 0.423 |

Numbers may shift slightly between runs and package versions.

Some takeaways:

- Hour of day and temperature are by far the most important variables.
- There are two daily peaks, at 8:00 and 18:00, which fits commuting.
- Summer averages about 4.5 times as many rentals per hour as winter.
- The effect of the hour is strongly non-linear, which is why the Random Forest clearly beats the linear models.

Solar radiation is left out of the models: the forecast API doesn't provide it,
and the final model is used to predict demand from that forecast.

## Running it

Requires R 4.1 or later.

```r
install.packages(c(
  "rvest", "httr", "jsonlite", "dplyr", "readr", "stringr", "tidyr",
  "lubridate", "DBI", "RSQLite", "ggplot2", "tidymodels", "glmnet", "ranger",
  "shiny", "shinydashboard", "DT", "plotly", "leaflet"
))
```

The weather forecast needs a free [OpenWeather](https://home.openweathermap.org/api_keys)
API key. Copy `.Renviron.example` to `.Renviron` and put the key there. Without
a key the pipeline still runs, but the forecast map stays empty.

From the project root:

```sh
Rscript run_pipeline.R      # collects data and trains the models (~1 min)
Rscript launch_dashboard.R  # serves on http://127.0.0.1:3838
```

## Dashboard

- **Forecast**: map with each city's predicted peak demand, plus the hourly curve for a selected city.
- **Seoul in 2018**: daily rentals, distribution and a seasonal summary.
- **Weather**: temperature vs. rentals, filterable by season and temperature.
- **Hourly patterns**: average rentals by hour of day for each season.
- **Models**: performance comparison and variable importance.

Predictions for other cities assume a fleet and riding habits similar to
Seoul's. They're useful for comparing cities, not as absolute numbers.

## Authors

- Ivanilson Braga
- Zakhar Khomyakivskyy
- Ektiandro Elizabeth
