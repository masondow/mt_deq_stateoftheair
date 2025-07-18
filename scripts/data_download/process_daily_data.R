#Data Processing (template for all pollutants, minus adding flags)
library(dplyr)
library(lubridate)
library(readr)
library(purrr)

pollutants <- c("PM2.5", "PM10", "SO2", "O3", "CO", "NO2", "Pb")

# Load monitor metadata once
monitors <- readRDS("data/aqs_data/monitors/monitors_2000-2025.rds") %>%
  mutate(parameter_code = as.double(parameter_code)) %>%    #need monitor parameter_code formatted the same as our daily data
  mutate(    #creates column site_name, fixing issue where local_site_name is empty
    site_name = case_when(
      !is.na(local_site_name) ~ local_site_name,
      is.na(local_site_name) & !tolower(city_name) %in% c("not in a city") ~ city_name,
      is.na(local_site_name) & tolower(city_name) == "not in a city" ~ paste0(address, ", ", county_name, " Co.")
    )
  )

for (pollutant in pollutants) {
  message("🔄 Processing: ", pollutant)
  
  daily <- readRDS(paste0("data/aqs_data/daily_raw/", pollutant, "_daily.rds"))
  
  #------------------(1) Add monitor metadata----------------------
  daily_plus <- left_join(daily, monitors)
  
  #-----------------(2) Filter for unique rows (deduplicate)---------------------
  #filter monitoring_agency = MT Dept Of Environmental Quality, Air Quality Division, validity_indicator = Y
  #group_by state_code, county_code, site_number, date_local
  #prioritize naaqs_primary_monitor = Y, sample_duration_code = 24-HR BLK AVG for PM2.5, PM10, SO2; 8-HR RUN AVG BEGIN HOUR for O3; 8-HR RUN AVG END HOUR for CO; 1 HOUR for NO2; and 24 HOUR for Pb
  #prioritize pollutant_standard = Ozone 8-hour 2015 for ozone (different sample numbers and averaging compared to older standards???)
  ###sample_duration_code 24 HOUR = filter, 1 HOUR = raw arithmetic mean of hourly sampling, 24-HR BLK AVG = rounded value of hourly sampling
  
  daily_plus_dedup <- daily_plus %>%
    filter(
      monitoring_agency == 'MT Dept Of Environmental Quality, Air Quality Division',
      validity_indicator == 'Y'
    ) %>%
    group_by(state_code, county_code, site_number, date_local) %>%
    arrange(
      # Highest priority: primary monitor
      desc(naaqs_primary_monitor == 'Y'),
      # Then prioritize sample duration codes
      desc(sample_duration_code %in% c('24-HR BLK AVG', '8-HR RUN AVG BEGIN HOUR', '8-HR RUN AVG END HOUR')),
      # Then prioritize ozone standard
      desc(pollutant_standard == 'Ozone 8-hour 2015')
    ) %>%
    slice(1) %>%  # Keep top-ranked row
    ungroup()
  
  #-------------------(3) Add month, quarter, season, year--------------
  daily_plus_dedup <- daily_plus_dedup %>%
    mutate(
      date_local = as.Date(date_local, format = "%Y-%m-%d"),
      year = year(date_local),
      month = month(date_local),
      quarter = case_when(
        month %in% 1:3 ~ 1,
        month %in% 4:6 ~ 2,
        month %in% 7:9 ~ 3,
        month %in% 10:12 ~ 4
      ),
      season = if_else(month %in% 5:10, "warm", "cool")
    ) %>%
    relocate(year, month, quarter, season, .after = date_local)
  
  #-------------------(4) Add PM2.5 Flags--------------------
  if (pollutant == "PM2.5") {
    message("🚩 Adding PM2.5 flags...")
    
    hourly <- readRDS("data/aqs_data/hourly_raw/PM2.5_hourly.rds")
    
    qualifiers <- c(
      "RT - Wildfire-U. S.",
      "RF - Fire - Canadian.",
      "IF - Fire - Canadian.",
      "IT - Wildfire-U. S.",
      "E - Forest Fire"
    )
    
    flags <- hourly %>%
      select(state_code, county_code, site_number, poc, date_local, qualifier) %>%
      group_by(state_code, county_code, site_number, poc, date_local) %>%
      arrange(desc(!is.na(qualifier))) %>%
      slice(1) %>%
      ungroup() %>%
      filter(!is.na(qualifier))
    
    daily_plus_dedup <- daily_plus_dedup %>%
      left_join(flags, by = c("date_local", "state_code", "county_code", "site_number", "poc"))
  }
  
  # Save the processed file
  output_path <- paste0("data/aqs_data/daily_processed/", pollutant, "_daily_processed.rds")
  saveRDS(daily_plus_dedup, output_path)
  
  message("✅ Saved: ", output_path, " (", nrow(daily_plus_dedup), " rows)")
}

#---------------------Utilities (not required to run)--------------------------

# check sample durations of all pollutants
pollutants <- c("PM2.5", "PM10", "SO2", "O3", "CO", "NO2", "Pb")
data_dir <- "data/aqs_data/daily_raw"

get_sample_durations <- function(pollutant) {
  file_path <- file.path(data_dir, paste0(pollutant, "_daily.rds"))
  
  if (file.exists(file_path)) {
    daily <- readRDS(file_path)
    
    # Extract unique sample_duration values
    unique_durations <- daily %>%
      distinct(sample_duration) %>%
      mutate(pollutant = pollutant) %>%
      select(pollutant, sample_duration)
    
    return(unique_durations)
  } else {
    warning("File not found for: ", pollutant)
    return(NULL)
  }
}

# Apply function to all pollutants and bind results
all_sample_durations <- pollutants %>%
  map_df(get_sample_durations)

