# Load required libraries
library(dplyr)
library(readr)
library(tidyr)

# Define the function to download and process the station data
process_station_data <- function(file_path, station_id = "LBBM8") {
  # Read all lines of the file
  lines <- readLines(file_path)
  
  # Find the line index that contains the station_id, allowing for whitespace
  start_index <- grep(paste0("^\\s*", station_id, "\\s*"), lines)
  
  # Error handling if the station_id is not found
  if (length(start_index) == 0) {
    stop("Station header not found in file.")
  }
  
  # Find the next blank line after the start_index to set as the end_index
  end_index <- grep("^\\s*$", lines)
  end_index <- end_index[end_index > start_index[1]][1]  # First blank line after start_index
  
  # Extract the relevant lines between start_index and end_index
  data_lines <- lines[(start_index + 1):(end_index - 1)]  # Exclude header and blank line
  
  # Write the data lines to a temporary file
  temp_file <- tempfile()
  writeLines(data_lines, temp_file)
  
  # Define the widths for each column (first column is 5 characters, others are 3)
  column_widths <- c(5, rep(3, length(strsplit(data_lines[1], "")[[1]]) - 4))
  
  # Read the fixed-width data from the temporary file
  data_df <- read.fwf(temp_file, widths = column_widths, stringsAsFactors = FALSE)
  
  # Clean up the temporary file
  unlink(temp_file)
  
  # Use the first column as row names and then transpose the data
  rownames(data_df) <- data_df[[1]]   # Set the first column as row names
  data_df <- data_df[, -1]            # Remove the first column from the data
  
  # Transpose the data frame
  data_df_t <- as.data.frame(t(data_df), stringsAsFactors = FALSE)
  
  # Return the transposed data frame with variables as column headers
  return(data_df_t)
}

# Define start and end dates
start_datetime <- as.POSIXct("2023-07-31 06:00:00", tz = "UTC")
end_datetime <- as.POSIXct("2024-01-01 23:00:00", tz = "UTC")

# Create a sequence of all hours between start and end dates
date_hours_seq <- seq.POSIXt(from = start_datetime, to = end_datetime, by = "hour")

# Create a data frame with year, month, day, and hour columns
date_hours_df <- data.frame(
  year = format(date_hours_seq, "%Y"),
  month = format(date_hours_seq, "%m"),
  day = format(date_hours_seq, "%d"),
  hour = format(date_hours_seq, "%H")
)

# Preview the resulting data frame
head(date_hours_df)
tail(date_hours_df)

# Iterate over each row in date_hours_df to download and process files
for (i in 1:nrow(date_hours_df)) {
  # Extract year, month, day, and hour from the data frame for the current row
  year <- date_hours_df$year[i]
  month <- sprintf("%02d", as.numeric(date_hours_df$month[i]))  # Ensure month is two digits
  day <- sprintf("%02d", as.numeric(date_hours_df$day[i]))      # Ensure day is two digits
  hour_str <- sprintf("%02d", as.numeric(date_hours_df$hour[i]))  # Ensure hour is two digits
  
  # Construct the file URL
  file_url <- sprintf("https://noaa-nbm-grib2-pds.s3.amazonaws.com/blend.%s%s%s/%s/text/blend_nbhtx.t%sz",
                      year, month, day, hour_str, hour_str)
  
  # Print the file URL for verification
  print(file_url)
  
  # Define a local path to save the file (this would depend on your system)
  download_path <- tempfile()  # Use a temporary file for download
  
  # Attempt to download the file, skip if it doesn't exist
  tryCatch({
    # Download the file
    download.file(file_url, download_path, mode = "wb")
    
    # Process the station data for the given file
    station_header <- "LBBM8"  # Set the desired header
    station_df <- process_station_data(download_path, station_header)
    
    # Keep only the first row of station_df
    station_df <- station_df[1, , drop = FALSE]
    
    # Save the processed dataframe (modify file path as needed)
    output_file <- sprintf("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/EE Demos/TSD/Libby 5 factor analysis/met_analysis/Libby NBM data/LBBM8_NBM_%s%s%s_%s.rds",
                           year, month, day, hour_str)
    saveRDS(station_df, output_file)
    
    # Optional: Print or log the progress
    print(paste("Processed and saved data for:", year, month, day, hour_str))
  }, error = function(e) {
    # If download fails, print a message and skip to the next iteration
    message(paste("Data not available for:", year, month, day, hour_str, "- Skipping this hour."))
  }, finally = {
    # Remove the temporary file after processing, regardless of success or failure
    unlink(download_path)
  })
}


