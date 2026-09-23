required <- c(
  "data/clean/seoul_bike_sharing.csv",
  "models/best_model.rds",
  "models/model_comparison.csv",
  "models/feature_importance.csv"
)

missing <- required[!file.exists(required)]
if (length(missing) > 0) {
  stop("Missing ", paste(missing, collapse = ", "),
       "\nRun the pipeline first: Rscript run_pipeline.R", call. = FALSE)
}

shiny::runApp("06_shiny_dashboard.R", port = 3838, launch.browser = interactive())
