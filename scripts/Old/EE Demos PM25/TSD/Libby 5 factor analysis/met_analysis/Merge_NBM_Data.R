
library(pbapply)
library(dplyr)
test <- readRDS("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/EE Demos/TSD/Libby 5 factor analysis/met_analysis/Libby NBM data/LBBM8_NBM_20201231_00.rds")

# Set the folder path
folder_path <- "/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/EE Demos/TSD/Libby 5 factor analysis/met_analysis/Libby NBM data"

# List all .rds files in the folder
rds_files <- list.files(folder_path, pattern = "\\.rds$", full.names = TRUE)

# Define a function to extract year, month, day, and model_HR from the file name
extract_date_info <- function(file_path) {
  file_name <- basename(file_path)
  
  # Modify the regex to capture the model_HR after the date
  date_match <- stringr::str_match(file_name, "_(\\d{4})(\\d{2})(\\d{2})_(\\d{2})")
  
  if (!is.na(date_match[1, 2])) {
    year <- as.integer(date_match[1, 2])
    month <- as.integer(date_match[1, 3])
    day <- as.integer(date_match[1, 4])
    model_hr <- as.integer(date_match[1, 5])  # Capture model_HR
    
    return(list(YEAR = year, MONTH = month, DAY = day, model_HR = model_hr))
  } else {
    return(list(YEAR = NA, MONTH = NA, DAY = NA, model_HR = NA))
  }
}

# Get all unique column names across all files (only once)
all_columns <- unique(unlist(lapply(rds_files, function(f) {
  names(readRDS(f))
})))

# Initialize an empty list for the processed data
combined_data_list <- pblapply(rds_files, function(file) {
  # Read the file
  df <- readRDS(file)
  
  # Extract date info from the file, including model_HR
  date_info <- extract_date_info(file)
  
  # Debug: Print date_info to ensure it contains YEAR, MONTH, DAY, and model_HR
  # print(date_info)
  
  # Add YEAR, MONTH, DAY, and model_HR columns to each data frame
  df <- df %>%
    mutate(
      YEAR = date_info$YEAR,       # Use the actual year from the file
      MONTH = date_info$MONTH,     # Use the actual month from the file
      DAY = date_info$DAY,         # Use the actual day from the file
      model_HR = date_info$model_HR # Use the actual model hour from the file
    )
  
  # Debug: Print df after adding YEAR, MONTH, DAY, and model_HR to check the structure
  # print("After mutate:")
  # print(head(df))  # Print the first few rows of df
  
  # Add missing columns as NA to match the full set of columns across all files
  missing_columns <- setdiff(all_columns, names(df))
  df[missing_columns] <- NA
  
  # Return the modified data frame
  return(df)
})

# Now, combine all data frames in the list into one data frame
combined_data <- do.call(rbind, combined_data_list)

# Remove leading and trailing spaces from column names
colnames(combined_data) <- trimws(colnames(combined_data))

# Remove forecast hours greater than 1-hr (i.e. anything that isn't 1 or -23)
combined_data <- combined_data %>%
  dplyr::select(YEAR, MONTH, DAY, UTC, model_HR, everything()) %>%
  mutate(diff = UTC - model_HR) %>%
  filter(diff == 1 | diff == -23) %>%
  dplyr::select(-diff)

# Adjust date columns to match forecast hour (UTC time); i.e. make 0-hr the next day
combined_data <- combined_data %>%
  mutate(
    # Adjust YEAR, MONTH, DAY when UTC is 0
    DATE = as.Date(paste(YEAR, MONTH, DAY), format = "%Y %m %d") + ifelse(UTC == 0, 1, 0),
    # Update YEAR, MONTH, and DAY based on adjusted DATE
    YEAR = as.integer(format(DATE, "%Y")),
    MONTH = as.integer(format(DATE, "%m")),
    DAY = as.integer(format(DATE, "%d"))
  ) %>%
  dplyr::select(-DATE)

# Assuming combined_data_test is your data frame
combined_data <- combined_data_test %>%
  # Create a datetime column in UTC
  mutate(datetime_utc = ymd_h(paste(YEAR, MONTH, DAY, UTC, sep = "-"))) %>%
  # Adjust the datetime to MST (UTC - 7 hours)
  mutate(datetime_mst = with_tz(datetime_utc, tzone = "America/Denver")) %>%
  # Extract YEAR, MONTH, DAY, and HOUR from datetime_mst
  mutate(
    YEAR_MST = year(datetime_mst),
    MONTH_MST = month(datetime_mst),
    DAY_MST = day(datetime_mst),
    HOUR_MST = hour(datetime_mst)
  ) %>%
  # Optionally, remove the intermediate columns if you no longer need them
  dplyr::select(YEAR_MST, MONTH_MST, DAY_MST, HOUR_MST, -datetime_utc, -datetime_mst, everything())


saveRDS(combined_data, "/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/EE Demos/TSD/Libby 5 factor analysis/met_analysis/LBBM8_2021_2023_NBM_data.rds")



