#data functions
check_and_update_sites <- function() {
  library(lubridate)
  library(here)
  
  # Define the file path for sites.RDS
  sites_file <- here("data", "sites.RDS")
  
  # Check if the file exists
  if (!file.exists(sites_file)) {
    message("sites.RDS not found. Running update_sites.R...")
    source(here("data_scripts", "update_sites.R"))
    return()
  }
  
  # Get the file modification date
  file_mod_time <- file.info(sites_file)$mtime
  file_mod_month <- month(file_mod_time)
  file_mod_year <- year(file_mod_time)
  
  # Get the current system date
  current_month <- month(Sys.Date())
  current_year <- year(Sys.Date())
  
  # Check if the file was modified this year but in a previous month
  if (file_mod_year == current_year && file_mod_month < current_month) {
    message("sites.RDS is outdated (last modified in ", file_mod_month, "). Running update_sites.R...")
    source(here("data_scripts", "update_sites.R"))
  } else if (file_mod_year < current_year) {
    # This handles the case where the year has changed (e.g., from 2024 to 2025)
    message("sites.RDS is outdated (last modified in ", file_mod_year, "). Running update_sites.R...")
    source(here("data_scripts", "update_sites.R"))
  } else {
    message("sites.RDS is up to date. No need to update.")
  }
}

# Function to update and save data
update_and_save_AQS_data <- function(parameters, data_dir, pollutant_label) {
  
  # Define the file paths using here
  daily_file <- here(data_dir, paste0("daily_", pollutant_label, "_all_MT.rds"))
  hourly_file <- here(data_dir, paste0("hourly_", pollutant_label, "_all_MT.rds"))
  wf_flags_file <- here(data_dir, paste0("wf_flags_", pollutant_label, "_all_MT.rds"))
  
  # Check if the daily and hourly files exist
  if (file.exists(daily_file) && file.exists(hourly_file)) {
    
    # Get the last modified time of the files
    daily_mod_time <- file.info(daily_file)$mtime
    hourly_mod_time <- file.info(hourly_file)$mtime
    
    # Get the current year and month
    current_year <- format(Sys.Date(), "%Y")
    current_month <- format(Sys.Date(), "%m")
    
    # Get the year and month of the file modification
    daily_mod_year <- format(daily_mod_time, "%Y")
    daily_mod_month <- format(daily_mod_time, "%m")
    hourly_mod_year <- format(hourly_mod_time, "%Y")
    hourly_mod_month <- format(hourly_mod_time, "%m")
    
    if ((daily_mod_year == current_year && as.numeric(daily_mod_month) < as.numeric(current_month)) ||
        (daily_mod_year < current_year) || 
        (hourly_mod_year == current_year && as.numeric(hourly_mod_month) < as.numeric(current_month)) ||
        (hourly_mod_year < current_year)) {
      message("One or more files are outdated for parameter ", pollutant_label, ". Running update...")
      
      # Get the current year as a numeric value
      current_year <- as.numeric(format(Sys.Date(), "%Y"))
      
      # Assign values to byear and eyear
      byear <- current_year - 1 #conservative measure to make sure any flagging changes to last year's data is accounted for
      eyear <- current_year
      
      #BELOW CHUNK USED TO ITERATE AS SUBFUNCTION WHERE parameter in daily
      # and hourly was defined as "param in parameters"
      
      # Download the new data for daily and hourly
      daily <- aqs_dailysummary_by_state(
        parameter = parameters,
        bdate = as.Date(paste0(byear, "0101"), format = "%Y%m%d"),
        edate = as.Date(paste0(eyear, "1231"), format = "%Y%m%d"),
        stateFIPS = 30
      )
      
      hourly <- aqs_sampledata_by_state(
        parameter = parameters,
        bdate = as.Date(paste0(byear, "0101"), format = "%Y%m%d"),
        edate = as.Date(paste0(eyear, "1231"), format = "%Y%m%d"),
        stateFIPS = 30
      )
      
      # Read existing data
      daily_existing <- readRDS(daily_file)
      hourly_existing <- readRDS(hourly_file)
      
      # Define key columns for matching
      key_columns_daily <- c("state_code", "county_code", "site_number", 
                             "parameter_code", "poc", "parameter", 
                             "sample_duration", "date_local")
      
      # Define key columns for matching
      key_columns_hourly <- c("state_code", "county_code", "site_number", 
                              "parameter_code", "poc", "parameter", 
                              "sample_duration", "date_local", "time_local")
      
      # Remove old matching rows before adding new data
      daily_existing <- anti_join(daily_existing, daily, by = key_columns_daily)
      hourly_existing <- anti_join(hourly_existing, hourly, by = key_columns_hourly)
      
      # Combine new daily/hourly data with filtered existing data
      daily_combined <- bind_rows(daily_existing, daily)
      hourly_combined <- bind_rows(hourly_existing, hourly)
      
      # Save the updated data back to the RDS files
      saveRDS(daily_combined, daily_file)
      saveRDS(hourly_combined, hourly_file)
      
      #END OF CHUNK (see note above)
      
      # Extract wildfire qualifiers and save
      qualifiers <- c(
        "RT - Wildfire-U. S.",
        "RF - Fire - Canadian.",
        "IF - Fire - Canadian.",
        "IT - Wildfire-U. S.",
        "E - Forest Fire."
      )
      
      flags <- hourly_combined %>%
        mutate(date_local = as.Date(date_local, format = "%Y-%m-%d")) %>%
        group_by(date_local, state_code, county_code, site_number, poc) %>%
        summarize(qualifier = paste(
          unique(na.omit(qualifier[qualifier %in% qualifiers])), 
          collapse = ", "
        )) %>%
        ungroup()
      
      saveRDS(flags, wf_flags_file)
      
      # MESSAGE
      # Convert 'date_local' columns to Date format in both daily_combined and hourly_combined
      daily_combined$date_local <- as.Date(daily_combined$date_local)
      hourly_combined$date_local <- as.Date(hourly_combined$date_local)
      
      # Find the earliest and latest dates
      daily_min_date <- min(daily_combined$date_local, na.rm = TRUE)
      daily_max_date <- max(daily_combined$date_local, na.rm = TRUE)
      hourly_min_date <- min(hourly_combined$date_local, na.rm = TRUE)
      hourly_max_date <- max(hourly_combined$date_local, na.rm = TRUE)
      
      # Construct message with the date range
      message("Files updated for parameter ", pollutant_label, 
              ". Daily data range: ", daily_min_date, " to ", daily_max_date, 
              ". Hourly data range: ", hourly_min_date, " to ", hourly_max_date, ".")
      
    } else {
      message("Files for parameter ", pollutant_label, " are up to date. No need to update.")
    }
    
  } else {
    # If the files do not exist, run the update
    message("Files not found for parameter ", pollutant_label, ". Running update...")
    
    # Get the current year as a numeric value
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    
    # Assign values to byear and eyear
    byear <- current_year - 1 #conservative measure to make sure any flagging changes to last year's data is accounted for
    eyear <- current_year
    
    # Download the new data for daily and hourly
    daily <- aqs_dailysummary_by_state(
      parameter = parameters,
      bdate = as.Date(paste0(byear, "0101"), format = "%Y%m%d"),
      edate = as.Date(paste0(eyear, "1231"), format = "%Y%m%d"),
      stateFIPS = 30
    )
    
    hourly <- aqs_sampledata_by_state(
      parameter = parameters,
      bdate = as.Date(paste0(byear, "0101"), format = "%Y%m%d"),
      edate = as.Date(paste0(eyear, "1231"), format = "%Y%m%d"),
      stateFIPS = 30
    )

    # Extract wildfire qualifiers and save
    qualifiers <- c(
      "RT - Wildfire-U. S.",
      "RF - Fire - Canadian.",
      "IF - Fire - Canadian.",
      "IT - Wildfire-U. S.",
      "E - Forest Fire."
    )
    
    flags <- hourly %>%
      mutate(date_local = as.Date(date_local, format = "%Y-%m-%d")) %>%
      group_by(date_local, state_code, county_code, site_number, poc) %>%
      summarize(qualifier = paste(
        unique(na.omit(qualifier[qualifier %in% qualifiers])), 
        collapse = ", "
      )) %>%
      ungroup()

    # Save the new data to the respective files
    saveRDS(daily, daily_file)
    saveRDS(hourly, hourly_file)
    saveRDS(flags, wf_flags_file)
    
    message("Files created and updated for parameter ", pollutant_label)
  }
}

# Function to iterate over parameters and update data
update_AQS_for_each_parameter <- function() {
  
  # Define parameters
  PM10 <- 81102
  PM25 <- c(88101, 88502)
  
  # Define file directories based on parameters
  pm10_dir <- "data/PM10"
  pm25_dir <- "data/PM25"
  
  # Update data for PM10
  update_and_save_AQS_data(PM10, pm10_dir, "PM10")
  
  # Update data for PM25 (combine 88101 & 88502 into the same file)
  update_and_save_AQS_data(PM25, pm25_dir, "PM25")
}


