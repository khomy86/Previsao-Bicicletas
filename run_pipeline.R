packages <- c(
  "rvest", "httr", "jsonlite", "dplyr", "readr", "stringr", "tidyr",
  "lubridate", "DBI", "RSQLite", "ggplot2", "tidymodels", "glmnet", "ranger",
  "shiny", "shinydashboard", "DT", "plotly", "leaflet"
)

missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  stop("Missing packages: ", paste(missing, collapse = ", "), "\n",
       "Install them with install.packages(c(\"",
       paste(missing, collapse = "\", \""), "\"))", call. = FALSE)
}

steps <- c(
  "01_data_collection.R",
  "02_data_wrangling.R",
  "03_sql_analysis.R",
  "04_eda_visualization.R",
  "05_regression_modeling.R"
)

for (step in steps) {
  message("\n== ", step)
  elapsed <- system.time(source(step, local = new.env()))[["elapsed"]]
  message(sprintf("== %s done in %.0fs", step, elapsed))
}

message("\nDone. Start the dashboard with: Rscript launch_dashboard.R")
