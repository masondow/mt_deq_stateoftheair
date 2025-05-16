library(dplyr)
library(RAQSAPI)

# Set AQS credentials
aqs_credentials(username = "Cameron.Nealy@mt.gov", key = "copperhawk98")

library(AQS)  # Assuming you're using aqsapi or similar package
library(tidyverse)

# Define year range and state
byear <- 2000
eyear <- 2025
state_fips <- 30

# Parameter codes with names
param_codes <- c(
  CO = "42101",
  Pb_TSP = "14129",
  Pb_PM10 = "85129",
  NO2 = "42602",
  O3 = "44201",
  PM10 = "81102",
  PM25 = "88101",
  SO2 = "42401"
)

# Create folder to save outputs
dir.create("data/aqs_data", showWarnings = FALSE, recursive = TRUE)

# Loop through each parameter code
for (param_name in names(param_codes)) {
  param_code <- param_codes[[param_name]]
  
  message("Downloading ", param_name, " (", param_code, ")...")
  
  tryCatch({
    data <- aqs_dailysummary_by_state(
      parameter = param_code,
      bdate = as.Date(paste0(byear, "0101"), format = "%Y%m%d"),
      edate = as.Date(paste0(eyear, "1231"), format = "%Y%m%d"),
      stateFIPS = state_fips
    )
    
    if (!is.null(data) && nrow(data) > 0) {
      # ✅ Corrected save path here
      saveRDS(data, file = file.path("data", "aqs_data", paste0(param_name, "_", param_code, ".rds")))
      message("Saved: ", param_name)
    } else {
      warning("No data returned for ", param_name, " (", param_code, ")")
    }
  }, error = function(e) {
    warning("Error for ", param_name, ": ", conditionMessage(e))
  })
}

