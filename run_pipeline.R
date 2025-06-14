# Load required base packages for optimization
if (!require(parallel, quietly = TRUE)) install.packages("parallel")
library(parallel)

# Function to check and install missing packages efficiently
check_and_install_packages <- function(packages) {
  # Check which packages are missing
  missing_packages <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_packages) > 0) {
    cat(paste0("📦 Installing ", length(missing_packages), " missing packages: ", 
               paste(missing_packages, collapse = ", "), "\n"))
    
    # Install missing packages with better error handling
    tryCatch({
      install.packages(missing_packages, dependencies = TRUE, quiet = TRUE)
      cat("✅ Package installation completed successfully\n")
    }, error = function(e) {
      cat("❌ Package installation failed:", e$message, "\n")
      stop("Cannot proceed without required packages")
    })
  } else {
    cat("✅ All required packages are already installed\n")
  }
  
  # Load all packages
  invisible(sapply(packages, library, character.only = TRUE, quietly = TRUE))
}

# List of required packages for the entire project
required_packages <- c(
  "rvest", "httr", "jsonlite", "dplyr", "readr", "stringr", 
  "tidyr", "DBI", "RSQLite", "ggplot2", "tidymodels", "glmnet", 
  "randomForest", "doParallel", "shiny", "shinydashboard", 
  "DT", "plotly", "leaflet", "lubridate", "tictoc", "R.utils"  # Added for timeout functionality
)

# Run the package check
check_and_install_packages(required_packages)

# Setup parallel processing
num_cores <- max(1, detectCores() - 1)  # Leave one core free
cat(paste("🖥️  Using", num_cores, "cores for parallel processing\n"))

# --- Enhanced script running function ---
run_script <- function(script_name, description, timeout_seconds = 300) {
  cat(paste("\n▶️  Running:", description, paste0("(", script_name, ")"), "...\n"))
  
  # Check if script file exists
  if (!file.exists(script_name)) {
    cat(paste("  ❌ Script file not found:", script_name, "\n"))
    return(FALSE)
  }
  
  # Start timing
  start_time <- Sys.time()
  
  # Create a separate R process to run the script with timeout
  result <- tryCatch({
    # Set timeout for long-running scripts
    R.utils::withTimeout({
      source(script_name, local = new.env())
    }, timeout = timeout_seconds)
    
    end_time <- Sys.time()
    execution_time <- round(as.numeric(difftime(end_time, start_time, units = "secs")), 2)
    
    cat(paste("  ✅ Finished:", description, "in", execution_time, "seconds\n"))
    return(TRUE)
    
  }, TimeoutException = function(e) {
    cat(paste("  ⏰ Timeout:", script_name, "exceeded", timeout_seconds, "seconds\n"))
    return(FALSE)
    
  }, error = function(e) {
    end_time <- Sys.time()
    execution_time <- round(as.numeric(difftime(end_time, start_time, units = "secs")), 2)
    cat(paste("  ❌ Error in", script_name, "after", execution_time, "seconds:", e$message, "\n"))
    return(FALSE)
  })
  
  return(result)
}

# --- Enhanced pipeline execution ---
execute_pipeline <- function(continue_on_error = FALSE) {
  total_start_time <- Sys.time()
  cat("🚀 Starting Full Project Pipeline...\n")
  cat("This will collect data, clean it, and train the model. This may take several minutes.\n")
  cat(paste(rep("=", 60), collapse=""), "\n")
  
  # Define pipeline steps with dependencies and configurations
  pipeline_steps <- list(
    list(script = "01_data_collection.R", 
         description = "Data Collection", 
         timeout = 600,  # 10 minutes for data collection
         critical = TRUE),
    
    list(script = "02_data_wrangling.R", 
         description = "Data Wrangling", 
         timeout = 300,  # 5 minutes
         critical = TRUE),
    
    list(script = "03_sql_analysis.R", 
         description = "SQL Analysis", 
         timeout = 180,  # 3 minutes
         critical = FALSE),
    
    list(script = "04_eda_visualization.R", 
         description = "EDA & Visualization", 
         timeout = 240,  # 4 minutes
         critical = FALSE),
    
    list(script = "05_regression_modeling.R", 
         description = "Regression Modeling & Training", 
         timeout = 900,  # 15 minutes for model training
         critical = TRUE)
  )
  
  # Track results
  results <- list()
  failed_steps <- c()
  
  # Execute pipeline steps
  for (i in seq_along(pipeline_steps)) {
    step <- pipeline_steps[[i]]
    
    cat(paste("\n📊 Step", i, "of", length(pipeline_steps), "\n"))
    
    # Run the script
    success <- run_script(step$script, step$description, step$timeout)
    results[[step$script]] <- success
    
    # Handle failures
    if (!success) {
      failed_steps <- c(failed_steps, step$description)
      
      if (step$critical && !continue_on_error) {
        cat(paste("💥 Critical step failed:", step$description, "\n"))
        cat("Pipeline execution stopped. Use continue_on_error=TRUE to proceed despite failures.\n")
        return(list(success = FALSE, failed_steps = failed_steps, results = results))
      } else {
        cat(paste("⚠️  Non-critical step failed:", step$description, "- continuing...\n"))
      }
    }
    
    # Memory cleanup between steps
    if (i < length(pipeline_steps)) {
      gc(verbose = FALSE)  # Garbage collection
      cat("🧹 Memory cleanup completed\n")
    }
  }
  
  # Calculate total execution time
  total_end_time <- Sys.time()
  total_time <- round(as.numeric(difftime(total_end_time, total_start_time, units = "mins")), 2)
  
  # Print final summary
  cat("\n", paste(rep("=", 60), collapse=""), "\n")
  
  if (length(failed_steps) == 0) {
    cat("🎉 PIPELINE COMPLETED SUCCESSFULLY! 🎉\n")
  } else {
    cat("⚠️  PIPELINE COMPLETED WITH WARNINGS ⚠️\n")
    cat("Failed steps:", paste(failed_steps, collapse = ", "), "\n")
  }
  
  cat(paste("⏱️  Total execution time:", total_time, "minutes\n"))
  cat("📁 All generated files are ready for the dashboard.\n")
  cat("🚀 You can now run 'launch_dashboard.R' to start the application.\n")
  cat(paste(rep("=", 60), collapse=""), "\n")
  
  return(list(
    success = length(failed_steps) == 0,
    failed_steps = failed_steps,
    results = results,
    total_time = total_time
  ))
}

# --- Execute the pipeline ---
# You can modify these parameters as needed
pipeline_result <- execute_pipeline(continue_on_error = FALSE)

# Optional: Save pipeline execution log
if (exists("pipeline_result")) {
  saveRDS(pipeline_result, "pipeline_execution_log.rds")
  cat("📝 Pipeline execution log saved to 'pipeline_execution_log.rds'\n")
} 