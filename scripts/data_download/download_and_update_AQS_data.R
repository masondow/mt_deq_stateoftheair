#download AQS data

library(dplyr)
library(readxl)
library(RAQSAPI)
library(tidyverse)
library(readr)

# Set AQS credentials
aqs_credentials(username = "Cameron.Nealy@mt.gov", key = "copperhawk98")

#load monitor list
monitors <- readRDS("data/aqs_data/monitors/monitors_2000-2025.rds")

monitors_filtered <- monitors %>%
  filter(monitoring_agency == 'MT Dept Of Environmental Quality, Air Quality Division')

#load list of target parameters (hand-picked from monitors_filtered)
target_parameters <- read_excel("data/aqs_data/parameter_codes/target_parameter_codes.xlsx")

#----------DOWNLOAD AQS DAILY DATA--------------------------------------------------------------------------------------
download_and_save_daily_data <- function(target_parameters, byear = 2000, state_fips = 30, save_dir = "data/aqs_data/daily_raw") {
  
  eyear <- as.integer(format(Sys.Date(), "%Y"))    #download end year = current year
  
  if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)    #creates daily_raw/ subfolder
  
  unique_pollutants <- unique(target_parameters$criteria_pollutant)    #defines pollutants to download (some have multiple parameter codes)
  
  for (pollutant in unique_pollutants) {
    file_path <- file.path(save_dir, paste0(pollutant, "_daily.rds"))    
    file_exists <- file.exists(file_path)
    
    if (file_exists) {
      message("📂 File exists for ", pollutant, ": ", file_path)
      existing_data <- readRDS(file_path)    #unless a file gets deleted, this loads the existing data frame for a given pollutant
      
      # Get latest available year in existing data
      latest_date <- suppressWarnings(max(as.Date(existing_data$date_local)))    #reads most recent "date_local" from existing data frame
      new_byear <- as.integer(format(latest_date, "%Y"))    #changes download begin year from default of 2000 to the year in latest_date

      message("🔄 Updating data starting from: ", new_byear)
      byear_to_use <- new_byear
    } else {
      message("🆕 Creating new data file for: ", pollutant)
      byear_to_use <- byear
    }
    
    # Get parameter codes
    codes <- target_parameters %>%    #defines all target parameter codes for given pollutant 
      filter(criteria_pollutant == pollutant) %>%
      pull(parameter_code)
    
    all_data <- tibble()
    
    for (param_code in codes) {
      message("  ➤ Parameter code: ", param_code)
      
      try({
        data <- aqs_dailysummary_by_state(    #this is the actual AQS query, run for each parameter code for each pollutant (as defined in target_parameters)
          parameter = as.character(param_code),
          bdate = as.Date(paste0(byear_to_use, "0101"), format = "%Y%m%d"),
          edate = as.Date(paste0(eyear, "1231"), format = "%Y%m%d"),
          stateFIPS = state_fips
        )
        
        if (!is.null(data) && nrow(data) > 0) {
          data$parameter_code <- param_code
          all_data <- bind_rows(all_data, data)    #all_data is the 'new data' from this download added to the empty all_data tibble
        } else {
          message("    ⚠️ No data returned for parameter code ", param_code)
        }
      }, silent = FALSE)
    }
    
    # Save the combined data, ensuring no overlap
    if (nrow(all_data) > 0) {
      if (file_exists) {
        # Convert date_local to Date safely
        existing_data$date_local <- as.Date(existing_data$date_local)
        all_data$date_local <- as.Date(all_data$date_local)    #again, all_data is the newly downloaded data
        
        # Identify years of new data
        all_data_years <- unique(format(all_data$date_local, "%Y"))
        
        # Count how many existing rows will be replaced
        replaced_rows <- existing_data %>%
          filter(format(date_local, "%Y") %in% all_data_years)    #filters existing data to only include rows with the year(s) from the new data download (typically this will just be the current year to date, unless the last update was run more than 1 year ago)
        num_replaced <- nrow(replaced_rows)
        
        # Remove old rows from those years
        cleaned_existing_data <- existing_data %>%    #removes from existing data any rows that overlap the time period of new data (this is to avoid inadvertently keeping/duplicating rows of data that may have received minor updates when the new data is appended)              
          filter(!(format(date_local, "%Y") %in% all_data_years))
        
        # Combine old + new
        combined_data <- bind_rows(cleaned_existing_data, all_data)   #replaces (any of the removed overlapping data) and appends any new (non-overlapping) to the data frame
        
        saveRDS(combined_data, file = file_path)
        message("✅ Updated file: ", file_path, 
                " (added ", nrow(all_data), " new rows, replaced ", num_replaced, " old rows)")    #if current year is the same as the last updated year, then rows added and rows replaced will likely be the same (unless data for a new quarter has been added since the last update)
      } else {
        saveRDS(all_data, file = file_path)
        message("✅ Created new file: ", file_path, " with ", nrow(all_data), " rows")
      }
    }
    else {
      message("❌ No new data found for pollutant: ", pollutant)
    }
  }
}

download_and_save_daily_data(target_parameters)



#----------DOWNLOAD AQS HOURLY DATA (only PM2.5 right now [to get qualifiers])----------------------------------------
target_parameters_pm25 <- target_parameters %>%
  filter(criteria_pollutant == 'PM2.5')

download_and_save_hourly_data <- function(target_parameters, byear = 2000, state_fips = 30, save_dir = "data/aqs_data/hourly_raw") {
  
  eyear <- as.integer(format(Sys.Date(), "%Y"))    #download end year = current year
  
  if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)    #creates daily_raw/ subfolder
  
  unique_pollutants <- unique(target_parameters$criteria_pollutant)    #defines pollutants to download (some have multiple parameter codes)
  
  for (pollutant in unique_pollutants) {
    file_path <- file.path(save_dir, paste0(pollutant, "_hourly.rds"))    
    file_exists <- file.exists(file_path)
    
    if (file_exists) {
      message("📂 File exists for ", pollutant, ": ", file_path)
      existing_data <- readRDS(file_path)    #unless a file gets deleted, this loads the existing data frame for a given pollutant
      
      # Get latest available year in existing data
      latest_date <- suppressWarnings(max(as.Date(existing_data$date_local)))    #reads most recent "date_local" from existing data frame
      new_byear <- as.integer(format(latest_date, "%Y"))    #changes download begin year from default of 2000 to the year in latest_date
      
      message("🔄 Updating data starting from: ", new_byear)
      byear_to_use <- new_byear
    } else {
      message("🆕 Creating new data file for: ", pollutant)
      byear_to_use <- byear
    }
    
    # Get parameter codes
    codes <- target_parameters %>%    #defines all target parameter codes for given pollutant 
      filter(criteria_pollutant == pollutant) %>%
      pull(parameter_code)
    
    all_data <- tibble()
    
    for (param_code in codes) {
      message("  ➤ Parameter code: ", param_code)
      
      try({
        data <- aqs_sampledata_by_state(    #this is the actual AQS query, run for each parameter code for each pollutant (as defined in target_parameters)
          parameter = as.character(param_code),
          bdate = as.Date(paste0(byear_to_use, "0101"), format = "%Y%m%d"),
          edate = as.Date(paste0(eyear, "1231"), format = "%Y%m%d"),
          stateFIPS = state_fips
        )
        
        if (!is.null(data) && nrow(data) > 0) {
          #data$parameter_code <- param_code
          all_data <- bind_rows(all_data, data)    #all_data is the 'new data' from this download added to the empty all_data tibble
        } else {
          message("    ⚠️ No data returned for parameter code ", param_code)
        }
      }, silent = FALSE)
    }
    
    # Save the combined data, ensuring no overlap
    if (nrow(all_data) > 0) {
      if (file_exists) {
        # Convert date_local to Date safely
        existing_data$date_local <- as.Date(existing_data$date_local)
        all_data$date_local <- as.Date(all_data$date_local)    #again, all_data is the newly downloaded data
        
        # Identify years of new data
        all_data_years <- unique(format(all_data$date_local, "%Y"))
        
        # Count how many existing rows will be replaced
        replaced_rows <- existing_data %>%
          filter(format(date_local, "%Y") %in% all_data_years)    #filters existing data to only include rows with the year(s) from the new data download (typically this will just be the current year to date, unless the last update was run more than 1 year ago)
        num_replaced <- nrow(replaced_rows)
        
        # Remove old rows from those years
        cleaned_existing_data <- existing_data %>%    #removes from existing data any rows that overlap the time period of new data (this is to avoid inadvertently keeping/duplicating rows of data that may have received minor updates when the new data is appended)              
          filter(!(format(date_local, "%Y") %in% all_data_years))
        
        # Combine old + new
        combined_data <- bind_rows(cleaned_existing_data, all_data)   #replaces (any of the removed overlapping data) and appends any new (non-overlapping) to the data frame
        
        saveRDS(combined_data, file = file_path)
        message("✅ Updated file: ", file_path, 
                " (added ", nrow(all_data), " new rows, replaced ", num_replaced, " old rows)")    #if current year is the same as the last updated year, then rows added and rows replaced will likely be the same (unless data for a new quarter has been added since the last update)
      } else {
        saveRDS(all_data, file = file_path)
        message("✅ Created new file: ", file_path, " with ", nrow(all_data), " rows")
      }
    }
    else {
      message("❌ No new data found for pollutant: ", pollutant)
    }
  }
}

download_and_save_hourly_data(target_parameters_pm25)

