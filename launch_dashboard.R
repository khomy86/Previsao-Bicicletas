cat("🚀 Starting Seoul Bike Sharing Dashboard...\n")

# Check if required packages are installed
required_packages <- c("shiny", "shinydashboard", "DT", "plotly", "dplyr", "readr", "ggplot2", "leaflet", "tidymodels", "lubridate")

cat("📦 Checking required packages...\n")
missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if(length(missing_packages) > 0) {
  cat("❌ Missing packages:", paste(missing_packages, collapse = ", "), "\n")
  cat("📥 Installing missing packages...\n")
  install.packages(missing_packages, dependencies = TRUE)
  cat("✅ Packages installed successfully!\n")
} else {
  cat("✅ All required packages are installed!\n")
}

# Load required libraries silently
suppressMessages({
  library(shiny)
  library(shinydashboard)
  library(DT)
  library(plotly)
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(leaflet)
  library(tidymodels)
  library(lubridate)
})

cat("📊 Libraries loaded successfully!\n")

# Check if essential files exist
essential_files <- c("06_shiny_dashboard.R", "models/best_model_workflow.rds", "data/clean/seoul_bike_sharing_clean.csv")
missing_essentials <- essential_files[!sapply(essential_files, file.exists)]

if(length(missing_essentials) > 0) {
    cat("\n❌ Error: Missing essential files!\n")
    for (f in missing_essentials) {
        cat("  -", f, "\n")
    }
    cat("\n👉 Please run the 'run_pipeline.R' script first to generate these files.\n")
    stop("Execution halted due to missing files.")
}

cat("🔍 All essential files found!\n")

# Display project info
cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("🚴 SEOUL BIKE SHARING DEMAND ANALYSIS DASHBOARD\n")
cat(paste(rep("=", 60), collapse=""), "\n")
cat("\n🌐 The dashboard will open automatically in your web browser.\n")
cat("🔗 If it doesn't open, navigate to the URL shown below.\n")
cat("⏹️  Press CTRL+C in this console to stop the dashboard.\n")
cat(paste(rep("=", 60), collapse=""), "\n\n")

# Add a small delay for better user experience
Sys.sleep(1)

cat("🌐 Launching dashboard in browser...\n")

# Launch the Shiny app with automatic browser opening
tryCatch({
  shiny::runApp(
    appDir = "06_shiny_dashboard.R",
    launch.browser = TRUE,
    host = "127.0.0.1",
    port = 3838,
    display.mode = "normal"
  )
}, error = function(e) {
  cat("❌ Error launching dashboard:", e$message, "\n")
  cat("📋 If issues persist, try running from R/RStudio console:\n")
  cat("   shiny::runApp('06_shiny_dashboard.R')\n")
})

cat("\n👋 Dashboard session ended.\n") 