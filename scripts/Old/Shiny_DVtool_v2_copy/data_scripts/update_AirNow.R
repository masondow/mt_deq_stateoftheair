# Load the packages


#### Download AirNow ####
# Define start date and end date for the sequence
start_date <- Sys.Date() - 3
today <- Sys.Date()

# Create a sequence of dates from start_date to today
date_sequence <- seq.Date(start_date, today, by = "day")

#all_data <- data.frame() #use if need to initialize an all-new dataset
all_data <- readRDS(here("data","AirNow","hourly_AirNow_all_MT.rds"))

# Iterate through each date in the date_sequence
for (current_date in date_sequence) {
  
  # Define startDate (1 day before the current_date)
  chunk_start <- format(as.Date(current_date) - 2, "%Y-%m-%d")
  chunk_end <- format(as.Date(current_date), "%Y-%m-%d")
  
  # Format dates as YYYY-MM-DD for URL
  start_date_str <- format(as.Date(chunk_start), "%Y-%m-%d")
  end_date_str <- format(as.Date(chunk_end), "%Y-%m-%d")
  
  # Insert into URL
  url <- paste0("https://www.airnowapi.org/aq/data/?startDate=", start_date_str, "T07",
                "&endDate=", end_date_str, "T06",
                "&parameters=PM25,PM10&BBOX=-116.202774,44.045890,-103.722305,49.229925",
                "&dataType=C&format=text/csv&verbose=1&monitorType=0&includerawconcentrations=0",
                "&API_KEY=4A314159-4658-4690-8CE9-F716E5EABC20")
  
  col_names <- c("latitude", "longitude", "date_gmt", "parameter", "sample_measurement", 
                 "units_of_measure", "site_name", "monitoring_agency", "AQSID", "Full_AQSID")
  
  # Make the GET request
  response <- GET(url)
  
  # Check if the request was successful
  if (status_code(response) == 200) {
    csv_data <- content(response, "text")
    
    # Read the data into a data frame
    air_quality_data <- read_csv(csv_data)
    colnames(air_quality_data) <- col_names
    
    # Process the data (adjust time and create date_local, time_local)
    AirNow <- air_quality_data %>%
      mutate(
        # Adjust the time by -7 hours for MST (Montana is UTC-7)
        adjusted_time = date_gmt - hours(7),
        
        # Create date_local with the correct date considering the time shift
        date_local = format(adjusted_time, "%Y-%m-%d"),
        
        # Create time_local with the correct time considering the time shift
        time_local = format(adjusted_time, "%H:%M"),
        
        # Ensure AQSID and Full_AQSID are both characters
        AQSID = as.character(AQSID),
        Full_AQSID = as.character(Full_AQSID),
        country_code = substr(Full_AQSID, 1, 3),
        state_code   = substr(Full_AQSID, 4, 5),
        county_code  = substr(Full_AQSID, 6, 8),
        site_number  = substr(Full_AQSID, 9, 12)) %>%
      filter(country_code == "840", state_code == "30")
    
    # Define key columns (excluding sample_measurement)
    key_columns <- c("latitude", "longitude", "date_gmt", "parameter", 
                     "units_of_measure", "site_name", "monitoring_agency", 
                     "AQSID", "Full_AQSID", "adjusted_time", "date_local", "time_local")
    
    # Remove duplicates in all_data where AirNow has updated data
    all_data <- all_data %>%
      anti_join(AirNow, by = key_columns) %>%  # Remove old versions of the updated rows
      bind_rows(AirNow) %>%  # Add updated and new rows
      distinct()  # Ensure uniqueness
  } else {
    print(paste("Failed to fetch data for", current_date, "Status code:", status_code(response)))
  }
}

# Remove rows where sample_measurement is less than -900
all_data <- all_data %>%
  filter(sample_measurement >= -900) %>%
  mutate(date_local = as.Date(date_local)) %>%
  filter(date_local >= Sys.Date() - years(3)) %>% #keep last 3 years of data; data_processing will handle last 2 years of data to allow for retroactive updates and to give 1 year buffer to archived AirNow
  mutate(date_local = as.character(date_local))

#### Save Hourly File ####

saveRDS(all_data, here("data","AirNow","hourly_AirNow_all_MT.rds"))

#### Average and Save Daily File ####

daily_data <- all_data %>%
  group_by(site_name, date_local, parameter) %>%
  summarise(
    latitude = first(latitude),  # Keeping the first latitude (assumes consistent values per site)
    longitude = first(longitude),  # Same for longitude
    monitoring_agency = first(monitoring_agency),  # Assumes each site has the same agency
    parameter = first(parameter),  # Assuming parameter is consistent
    units_of_measure = first(units_of_measure),  # Keeping units
    arithmetic_mean = mean(sample_measurement, na.rm = TRUE),  # Compute mean while ignoring NAs
    state_code   = first(state_code),
    county_code  = first(county_code),
    site_number  = first(site_number),
    .groups = "drop"  # Ungroup after summarization
  )

saveRDS(daily_data, here("data","AirNow","daily_AirNow_all_MT.rds"))




