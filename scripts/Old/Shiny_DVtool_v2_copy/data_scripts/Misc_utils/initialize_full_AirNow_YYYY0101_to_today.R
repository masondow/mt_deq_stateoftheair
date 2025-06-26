# download AQS data for every site in Montana
# download last 3 days at a time in case data was missing and updated later
# apply metadata using coordinates from AQS data frame
# merge with "master" AirNow data frame for the current year 
# need to handle annual switch_over

library(readr)
library(dplyr)
library(tidyverse)

# Define column names
col_names <- c("Valid_date", "AQSID", "site_name", "parameter_name", "reporting_units", 
               "value", "averaging_period", "data_source", "AQI", "AQI_Category", 
               "latitude", "longitude", "Full_AQSID")

# Generate sequence of dates for 2025 from Jan 1 to Mar 24
dates <- seq(as.Date("2025-01-01"), as.Date("2025-03-24"), by = "day")

# Function to process a single day's data
process_daily_data <- function(date) {
  # Format date to YYYYMMDD
  date_str <- format(date, "%Y%m%d")
  
  # Construct URL for the given date
  url <- paste0("https://s3-us-west-1.amazonaws.com/files.airnowtech.org/airnow/2025/", date_str, "/daily_data_v2.dat")
  
  # Try to read the data; return NULL if it fails
  tryCatch({
    daily_data <- read_delim(url, delim = "|", col_names = col_names, skip = 0)
    
    # Add extracted AQSID components
    daily_data <- daily_data %>%
      mutate(
        country_code = substr(Full_AQSID, 1, 3),
        state_code   = substr(Full_AQSID, 4, 5),
        county_code  = substr(Full_AQSID, 6, 8),
        site_number  = substr(Full_AQSID, 9, 12)
      ) %>%
      # Convert to numeric and filter for US (840) and Montana (30)
      mutate(
        country_code = as.numeric(country_code),
        state_code   = as.numeric(state_code)
      ) %>%
      filter(country_code == 840, state_code == 30)
    
    return(daily_data)
  }, error = function(e) {
    message(paste("Failed to read data for", date_str))
    return(NULL)
  })
}

# Loop over all dates and combine the data
air_data <- map_df(dates, process_daily_data)

library(here)
setwd("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/Shiny_DVtool_v2_copy")
here::i_am(".here")
here()

saveRDS(air_data, here("data", "AirNow", "AirNow_all_MT.rds"))
readRDS(here("data", "AirNow", "AirNow_all_MT.rds"))
