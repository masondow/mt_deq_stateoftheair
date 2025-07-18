library(dplyr)
library(RAQSAPI)
library(readr)
library(tidyverse)

# Set AQS credentials
aqs_credentials(username = "Cameron.Nealy@mt.gov", key = "copperhawk98")

# Load EPA AQS parameter codes (downloaded here: https://aqs.epa.gov/aqsweb/documents/codetables/methods_all.html)
parameter_codes <- read_csv("data/aqs_data/parameter_codes/methods.csv")

# Define the search terms (pollutants)
pollutants <- c("carbon monoxide", "lead", "nitrogen dioxide",
                "ozone", "PM2.5", "PM10", "sulfur dioxide")

# Create a single regex pattern (case-insensitive)
pattern <- str_c(pollutants, collapse = "|")

# Filter rows where 'Parameter' column matches any pollutant
filtered_codes <- parameter_codes %>%
  filter(str_detect(tolower(Parameter), tolower(pattern)))

# Set date range and state
begin <- "2000-01-01"
end <- "2025-12-31"
state_fips <- 30

# Get unique parameters
unique_parameters <- unique(filtered_codes$`Parameter Code`)

# Initialize an empty data frame
monitor_df <- tibble()

# Loop through parameters and append directly
for (param in unique_parameters) {
  message("🔍 Processing parameter: ", param)
  
  try({
    monitors <- aqs_monitors_by_state(
      parameter = as.character(param),
      bdate = as.Date(begin),
      edate = as.Date(end),
      stateFIPS = state_fips
    )
    
    # Bind to growing data frame
    monitor_df <- bind_rows(monitor_df, monitors)
  }, silent = TRUE)
}

saveRDS(monitor_df, "data/aqs_data/monitors/monitors_2000-2025.rds")

