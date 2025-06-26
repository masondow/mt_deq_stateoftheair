library(dplyr)
library(lubridate)
library(ggplot2)
library(purrr)

##### Load Data #####

# Load the data frame from the RDS file
pm25_samples <- readRDS("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/MARA Fires/R/Data/pm25_samples_all_continuous.rds")

# Convert the date_local column to Date format
pm25_samples$date_local <- as.Date(pm25_samples$date_local, format = "%Y-%m-%d")

# Define breakpoints for PM2.5 AQI categories
breakpoints <- tibble(
  category = c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy", "Very Unhealthy", "Hazardous"),
  min = c(-1000, 9.1, 35.5, 55.5, 125.5, 225.5),
  max = c(9.0999, 35.4999, 55.4999, 125.4999, 225.4999, Inf)
)

# Add a new column to categorize each row based on the value in 'arithmetic_mean'
pm25_samples <- pm25_samples %>%
  rowwise() %>%
  mutate(AQI_category = breakpoints$category[which(arithmetic_mean >= breakpoints$min & arithmetic_mean <= breakpoints$max)]) %>%
  ungroup()

# Slope (m) and intercept (b) based on your points
m <- -1
b <- -64

# Add a new column categorizing data as "east" or "west"
pm25_samples <- pm25_samples %>%
  mutate(region = ifelse(latitude > m * longitude + b, "east", "west"))

# Create a list of cities for each region
site_list_by_region <- pm25_samples %>%
  group_by(region) %>%
  summarize(sites = list(unique(local_site_name)))  # Replace 'city' with the actual column name if different

# Add day of year and year to the data
pm25_samples <- pm25_samples %>%
  mutate(
    day_of_year = yday(date_local),  # Extract day of year
    year = year(date_local)          # Extract year
  )

pm25_samples <- pm25_samples %>%
  select(latitude, longitude, date_local, arithmetic_mean,local_site_name, AQI_category, day_of_year, year, region)

##### Filter Highest Days #####

# Calculate 98th percentile for each local_site_name and filter data
pm25_above_99th <- pm25_samples %>%
  group_by(local_site_name) %>%                             # Group by site
  mutate(percentile_99 = quantile(arithmetic_mean, 0.99)) %>% # Calculate 98th percentile
  filter(arithmetic_mean > percentile_99) %>%               # Filter for values above the 98th percentile
  ungroup()                                                 # Ungroup to return a standard data frame

# Get unique date_local values for rows where region is "east"
days_east <- pm25_above_99th %>%
  filter(region == "east") %>%
  distinct(date_local)

# Get unique date_local values for rows where region is "east"
days_west <- pm25_above_99th %>%
  filter(region == "west") %>%
  distinct(date_local)

# Get the dates that are unique to the east region (not in west)
unique_days_east <- days_east %>%
  anti_join(days_west, by = "date_local")

# Get the dates that are unique to the west region (not in east)
unique_days_west <- days_west %>%
  anti_join(days_east, by = "date_local")

##### GIS Files #####

# Load necessary libraries
library(sf)
library(dplyr)
library(lubridate)
library(stringr)

# Read shapefile
US_shp_data <- st_read("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/MARA Fires/R/Data/mtbs_perimeter_data (1)/mtbs_perims_DD.shp")
CAN_shp_data <- st_read("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/MARA Fires/R/Data/nbac_1972_2023_20240530_shp/nbac_1972_2023_20240530.shp")
US_shp_data_2023 <- st_read("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/MARA Fires/R/Data/2023 US perimeters/2023_US_fire_perimeter_data.geojson")

##### US Fire Data #####

# US data
US_shp_data_df <- US_shp_data %>%
  as.data.frame() %>%
  select(Event_ID, Incid_Type, BurnBndAc, Ig_Date) %>%
  rename(
    ID = Event_ID,
    type = Incid_Type,
    acres = BurnBndAc,
    sdate = Ig_Date
  )
  
# Create a regex pattern for state codes
state_pattern <- "^WA|^OR|^ID|^CA|^NV|^MT|^WY"

# Filter
US_shp_data_df <- US_shp_data_df %>%
  filter(str_detect(ID, state_pattern) & 
           type == "Wildfire") %>%
  mutate(sdate = as.Date(sdate))

##### US 2023 Fire Data (gap fill) #####

# US 2023 supplement
US_shp_data_2023_df <- US_shp_data_2023 %>%
  as.data.frame() %>%
  mutate(type = "Wildfire") %>%
  select(UNIQFIREID, type, GISACRES, DISCOVERYDATETIME) %>%
  rename(
    ID = UNIQFIREID,
    type = type,
    acres = GISACRES,
    sdate = DISCOVERYDATETIME
  )

# Create a regex pattern to match state codes in the 6th and 7th position
state_pattern_2023 <- "^.{5}(WA|OR|ID|CA|NV|MT|WY)"

# Filter the data frame based on the ID column
US_shp_data_2023_df <- US_shp_data_2023_df %>%
  filter(str_detect(ID, state_pattern_2023) &
           acres > 1000)

# Convert sdate from milliseconds since Unix epoch to YYYY-MM-DD in MST
US_shp_data_2023_df <- US_shp_data_2023_df %>%
  mutate(
    # Convert milliseconds to seconds
    sdate_seconds = sdate / 1000,
    # Convert to POSIXct in UTC
    sdate_utc = as.POSIXct(sdate_seconds, origin = "1970-01-01", tz = "UTC"),
    # Convert to MST (UTC-7)
    sdate_mst = with_tz(sdate_utc, tzone = "MST"),
    # Format to YYYY-MM-DD
    sdate = format(sdate_mst, "%Y-%m-%d"),
    sdate = as.Date(sdate)
  )

# Drop intermediate columns
US_shp_data_2023_df <- US_shp_data_2023_df %>%
  select(-sdate_seconds, -sdate_utc, -sdate_mst)

# Combine data frames by stacking rows
US_shp_data_df <- bind_rows(US_shp_data_2023_df, US_shp_data_df)


##### CAN Fire Data #####
CAN_shp_data_df <- CAN_shp_data %>%
  as.data.frame() %>%
  filter(is.na(PRESCRIBED) | PRESCRIBED != "true") %>%  # Exclude rows where PRESCRIBED is "true"
  mutate(
    acres = ADJ_HA * 2.47105,  # Convert hectares to acres
    sdate = pmin(HS_SDATE, AG_SDATE, na.rm = TRUE),  # Get the earlier of the two dates, ignoring NA
    PRESCRIBED = "Wildfire"
  ) %>%
  filter(ADMIN_AREA %in% c("AB", "BC", "YT", "NT") &
           acres > 1000) %>%  # Filter by ADMIN_AREA
  select(
    ID = NFIREID, 
    type = PRESCRIBED,  # Set type to "Wildfire" for all rows
    acres, 
    sdate
  )


##### MT Regions and Acres Comparison #####

# Create a vector of dates with 2 weeks preceding each date in unique_days_west_vector
date_ranges_west <- unique_days_west %>%
  mutate(start_date = date_local - 14,  # 14 days preceding each date
         end_date = date_local) %>%
  select(start_date, end_date)

# Create a vector of dates with 2 weeks preceding each date in unique_days_west_vector
date_ranges_east <- unique_days_east %>%
  mutate(start_date = date_local - 14,  # 14 days preceding each date
         end_date = date_local) %>%
  select(start_date, end_date)

# Filter and summarize the data based on these date ranges
west_CAN_fire <- CAN_shp_data_df %>%
  # Join the CAN_shp_data_df with the date ranges to filter by each range
  rowwise() %>%
  filter(any(sdate >= date_ranges_west$start_date & sdate <= date_ranges_west$end_date)) %>%
  ungroup() %>%
  # Summarize the total acres
  summarize(total_acres = sum(acres, na.rm = TRUE)) %>%
  # Add additional columns
  mutate(`Fire Location` = "Canada",
         montana = "Western")

# Create a new data frame that filters and sums acres
west_US_fire <- US_shp_data_df %>%
  # Join the CAN_shp_data_df with the date ranges to filter by each range
  rowwise() %>%
  filter(any(sdate >= date_ranges_west$start_date & sdate <= date_ranges_west$end_date)) %>%
  ungroup() %>%
  # Summarize the total acres
  summarize(total_acres = sum(acres, na.rm = TRUE)) %>%
  # Add additional columns
  mutate(`Fire Location` = "US",
         montana = "Western")

east_CAN_fire <- CAN_shp_data_df %>%
  # Join the CAN_shp_data_df with the date ranges to filter by each range
  rowwise() %>%
  filter(any(sdate >= date_ranges_east$start_date & sdate <= date_ranges_east$end_date)) %>%
  ungroup() %>%
  # Summarize the total acres
  summarize(total_acres = sum(acres, na.rm = TRUE)) %>%
  # Add additional columns
  mutate(`Fire Location` = "Canada",
         montana = "Eastern")

east_US_fire <- US_shp_data_df %>%
  # Join the CAN_shp_data_df with the date ranges to filter by each range
  rowwise() %>%
  filter(any(sdate >= date_ranges_east$start_date & sdate <= date_ranges_east$end_date)) %>%
  ungroup() %>%
  # Summarize the total acres
  summarize(total_acres = sum(acres, na.rm = TRUE)) %>%
  # Add additional columns
  mutate(`Fire Location` = "US",
         montana = "Eastern")

# Combine the data frames into one
combined_fire_data <- bind_rows(
  west_CAN_fire,
  west_US_fire,
  east_CAN_fire,
  east_US_fire
)

# Calculate percentages
combined_fire_data <- combined_fire_data %>%
  # Calculate total acres for each montana group
  group_by(montana) %>%
  mutate(total_acres_montana = sum(total_acres, na.rm = TRUE)) %>%
  # Calculate percent for each row
  mutate(percent = (total_acres / total_acres_montana) * 100) %>%
  # Drop the extra column used for calculation
  select(-total_acres_montana) %>%
  ungroup()

##### Site Averages Highest Days #####

# Assuming unique_days_east is a data frame with a column 'date_local'
unique_days_east_vector <- unique_days_east$date_local

# Filter pm25_samples to include only the days in unique_days_east
east_highest_averages <- pm25_samples %>%
  filter(date_local %in% unique_days_east_vector)

# Calculate the average arithmetic mean for each local_site_name
# and retain longitude, latitude, and region for each site
east_highest_averages <- east_highest_averages %>%
  group_by(local_site_name, latitude, longitude, region) %>%
  summarize(average_arithmetic_mean = mean(arithmetic_mean, na.rm = TRUE), .groups = 'drop')

# Assuming unique_days_east is a data frame with a column 'date_local'
unique_days_west_vector <- unique_days_west$date_local

# Filter pm25_samples to include only the days in unique_days_east
west_highest_averages <- pm25_samples %>%
  filter(date_local %in% unique_days_west_vector)

# Calculate the average arithmetic mean for each local_site_name
# and retain longitude, latitude, and region for each site
west_highest_averages <- west_highest_averages %>%
  group_by(local_site_name, latitude, longitude, region) %>%
  summarize(average_arithmetic_mean = mean(arithmetic_mean, na.rm = TRUE), .groups = 'drop')

##### Download HMS smoke #####

library(httr)

# Define the base URL for downloading files
base_url <- "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile"

# Define the destination directory
dest_dir <- "/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/MARA Fires/R/Data/HMS smoke/east"

# Function to construct the URL and download the file
download_file <- function(date) {
  # Extract year, month, and day from the date
  year <- format(date, "%Y")
  month <- format(date, "%m")
  day <- format(date, "%d")
  
  # Construct the URL based on the date
  url <- paste0(base_url, "/", year, "/", month, "/hms_smoke", year, month, day, ".zip")
  
  # Define the destination file path
  destfile <- file.path(dest_dir, paste0("hms_smoke_", year, month, day, ".zip"))
  
  # Download the file
  tryCatch({
    download.file(url, destfile, mode = "wb")
    message("Successfully downloaded: ", destfile)
  }, error = function(e) {
    message("Failed to download: ", url)
    message("Error: ", e$message)
  })
}

# Iterate over each date and download the corresponding file
lapply(unique_days_east_vector, download_file)

##### PLOTTING #####

# Ensure 'montana' is a factor and set levels to control the order
combined_fire_data$montana <- factor(combined_fire_data$montana, levels = c("Western", "Eastern"))

# Create the bar plot with adjusted legend and label sizes
p <- ggplot(combined_fire_data, aes(x = montana, y = total_acres, fill = `Fire Location`)) +
  geom_bar(stat = "identity", position = "dodge") +  # Create bars with dodge position
  labs(
    title = "Wildfires on Montana Smokiest Days",
    x = "Montana",
    y = "Acres Burned"
  ) +
  theme_minimal() +  # Use a minimal theme
  scale_fill_manual(values = c("Canada" = "#009ADE", "US" = "#004A98")) +  # Custom colors for regions
  scale_y_continuous(labels = scales::comma, breaks = seq(0, max(combined_fire_data$total_acres), by = 5000000)) +  # Y-axis labels every 5,000,000
  theme(
    panel.grid.major.x = element_blank(),  # Remove vertical grid lines
    panel.grid.minor.x = element_blank(),  # Remove minor vertical grid lines (if any)
    legend.text = element_text(size = 12),  # Adjust legend text size
    legend.title = element_text(size = 14),  # Adjust legend title size
    text = element_text(size = 14),  # Adjust size of all text elements in the plot
    plot.title = element_text(size = 16, face = "bold"),  # Adjust title size and make it bold
    axis.title.x = element_text(size = 14),  # Adjust x-axis title size
    axis.title.y = element_text(size = 14),  # Adjust y-axis title size
    axis.text = element_text(size = 12)  # Adjust axis label sizes
  )

# Print the plot to check
print(p)

# Save the plot as a PNG with transparent background
ggsave("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/MARA Fires/Final Figures/wildfire_acres_smokiest_days.png", plot = p, bg = "transparent", width = 10, height = 6, dpi = 300)



##### Combine HMS Smoke Files - East #####

# Define the base directory
base_dir_east <- "/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/MARA Fires/R/Data/HMS smoke/east"

# List all subdirectories
folders_east <- list.dirs(base_dir_east, full.names = TRUE, recursive = FALSE)

# Create a vector of shapefile paths
shapefiles_east <- sapply(folders_east, function(folder) {
  file_path <- list.files(folder, pattern = "\\.shp$", full.names = TRUE)
  return(file_path)
}, simplify = TRUE)

# Read and combine shapefiles into a single sf object
shapefiles_list_east <- lapply(shapefiles_east, function(file) {
  shp <- st_read(file)
  
  # Check and repair invalid geometries
  shp <- st_make_valid(shp)
  
  return(shp)
})

# Combine all shapefiles into one
combined_shapefiles_east <- do.call(rbind, shapefiles_list_east)

##### Combine HMS Smoke Files - West #####

# Define the base directory
base_dir_west <- "/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/MARA Fires/R/Data/HMS smoke/west"

# List all subdirectories
folders_west <- list.dirs(base_dir_west, full.names = TRUE, recursive = FALSE)

# Create a vector of shapefile paths
shapefiles_west <- sapply(folders_west, function(folder) {
  file_path <- list.files(folder, pattern = "\\.shp$", full.names = TRUE)
  return(file_path)
}, simplify = TRUE)

# Read and combine shapefiles into a single sf object
shapefiles_list_west <- lapply(shapefiles_west, function(file) {
  shp <- st_read(file)
  
  # Check and repair invalid geometries
  shp <- st_make_valid(shp)
  
  return(shp)
})

# Combine all shapefiles into one
combined_shapefiles_west <- do.call(rbind, shapefiles_list_west)

##### Fires Highest West #####

# Merge HS_SDATE and AG_SDATE into a new column, keeping the earlier date
CAN_shp_data <- CAN_shp_data %>%
  mutate(sdate = pmin(HS_SDATE, AG_SDATE, na.rm = TRUE))

CAN_fires_west <- CAN_shp_data %>%
  filter(is.na(PRESCRIBED) | PRESCRIBED != "true") %>%  # Exclude rows where PRESCRIBED is "true"
  mutate(
    acres = ADJ_HA * 2.47105,  # Convert hectares to acres
    sdate = pmin(HS_SDATE, AG_SDATE, na.rm = TRUE),  # Get the earlier of the two dates, ignoring NA
    PRESCRIBED = "Wildfire"
  ) %>%
  filter(ADMIN_AREA %in% c("AB", "BC", "SK","MB") & 
           acres > 5000) %>%  # Filter by ADMIN_AREA
  rowwise() %>%
  filter(any(sdate >= date_ranges_west$start_date & sdate <= date_ranges_west$end_date)) %>%
  ungroup()

# Create a regex pattern for state codes
state_pattern_map <- "^WA|^OR|^ID|^CA|^NV|^MT|^WY|^SD|^ND|^CO|^AZ|^NM|^UT|^NE|^MN|^IA|"

US_fires_west <- US_shp_data %>%
  # Join the CAN_shp_data_df with the date ranges to filter by each range
  rowwise() %>%
  filter(any(Ig_Date >= date_ranges_west$start_date & Ig_Date <= date_ranges_west$end_date)) %>%
  ungroup() %>%
  filter(str_detect(Event_ID, state_pattern_map) & Incid_Type == "Wildfire" & BurnBndAc > 5000)

# Create a regex pattern to match state codes in the 6th and 7th position
state_pattern_2023_map <- "^.{5}(WA|OR|ID|CA|NV|MT|WY|SD|ND|CO|AZ|NM|UT|NE|MN|IA)"

# Convert sdate from milliseconds since Unix epoch to YYYY-MM-DD in MST
US_fires_west_2023 <- US_shp_data_2023 %>%
  mutate(
    # Convert milliseconds to seconds
    sdate_seconds = DISCOVERYDATETIME / 1000,
    # Convert to POSIXct in UTC
    sdate_utc = as.POSIXct(sdate_seconds, origin = "1970-01-01", tz = "UTC"),
    # Convert to MST (UTC-7)
    sdate_mst = with_tz(sdate_utc, tzone = "MST"),
    # Format to YYYY-MM-DD
    sdate = format(sdate_mst, "%Y-%m-%d"),
    sdate = as.Date(sdate)
  ) %>%
  rowwise() %>%
  filter(any(sdate >= date_ranges_west$start_date & sdate <= date_ranges_west$end_date)) %>%
  ungroup() %>%
  filter(str_detect(UNIQFIREID, state_pattern_2023_map) & 
           GISACRES > 5000)

##### Fires Highest East #####

CAN_fires_east <- CAN_shp_data %>%
  filter(is.na(PRESCRIBED) | PRESCRIBED != "true") %>%  # Exclude rows where PRESCRIBED is "true"
  mutate(
    acres = ADJ_HA * 2.47105,  # Convert hectares to acres
    sdate = pmin(HS_SDATE, AG_SDATE, na.rm = TRUE),  # Get the earlier of the two dates, ignoring NA
    PRESCRIBED = "Wildfire"
  ) %>%
  filter(ADMIN_AREA %in% c("AB", "BC", "SK","MB") & acres > 5000) %>%  # Filter by ADMIN_AREA
  rowwise() %>%
  filter(any(sdate >= date_ranges_east$start_date & sdate <= date_ranges_east$end_date)) %>%
  ungroup()

US_fires_east <- US_shp_data %>%
  # Join the CAN_shp_data_df with the date ranges to filter by each range
  rowwise() %>%
  filter(any(Ig_Date >= date_ranges_east$start_date & Ig_Date <= date_ranges_east$end_date)) %>%
  ungroup() %>%
  filter(str_detect(Event_ID, state_pattern_map) & Incid_Type == "Wildfire" & BurnBndAc > 5000)

# Convert sdate from milliseconds since Unix epoch to YYYY-MM-DD in MST
US_fires_east_2023 <- US_shp_data_2023 %>%
  mutate(
    # Convert milliseconds to seconds
    sdate_seconds = DISCOVERYDATETIME / 1000,
    # Convert to POSIXct in UTC
    sdate_utc = as.POSIXct(sdate_seconds, origin = "1970-01-01", tz = "UTC"),
    # Convert to MST (UTC-7)
    sdate_mst = with_tz(sdate_utc, tzone = "MST"),
    # Format to YYYY-MM-DD
    sdate = format(sdate_mst, "%Y-%m-%d"),
    sdate = as.Date(sdate)
  ) %>%
  rowwise() %>%
  filter(any(sdate >= date_ranges_east$start_date & sdate <= date_ranges_east$end_date)) %>%
  ungroup() %>%
  filter(str_detect(UNIQFIREID, state_pattern_2023_map) & 
           GISACRES > 5000)

##### MAPPING #####

library(elevatr)
library(ggplot2)
library(rnaturalearth)
library(sf)
library(ggspatial)
library(readxl)
library(ggnewscale)
library(rmapshaper)

# Load world boundaries
world <- ne_countries(scale = "medium", returnclass = "sf")

# Load state and province boundaries for specific countries
states_provinces <- ne_states(country = c("united states of america", "canada"), returnclass = "sf")

# Check if 'elevation' is already defined in the global environment
if (!exists("elevation")) {
  # Define your area of interest with bounding box
  bbox <- c(left = -130, bottom = 35, right = -95, top = 60)
  
  # Create points and convert them to an sf object with a CRS
  pts <- data.frame(x = c(bbox[1], bbox[3]), y = c(bbox[2], bbox[4]))
  pts_sf <- st_as_sf(pts, coords = c("x", "y"), crs = 4326)  # CRS 4326 is WGS 84
  
  # Get elevation data
  elevation <- get_elev_raster(pts_sf, z = 6, clip = "bbox")
}

# Convert to a data frame for ggplot
elevation_df <- as.data.frame(as(elevation, "SpatialPixelsDataFrame"))
colnames(elevation_df) <- c("elevation", "x", "y")

# Define fixed breaks and limits for the color scale
elevation_limits <- c(-2000, 4000)  # Define your fixed limits here
elevation_breaks <- c(-2000, 0, 10, 500, 1500, 2000, 2500, 4000)  # Define your fixed breaks here
elevation_labels <- seq(from = 0, to = 4000, by = 1000) # Create labels for every 1000 meters within the range

# EAST

# Replace NA with a default value, e.g., "Unknown"
combined_shapefiles_east$Density[is.na(combined_shapefiles_east$Density)] <- "Unknown"
combined_shapefiles_east$Density <- as.factor(combined_shapefiles_east$Density)

# Ensure Density is a factor and handle NA values
combined_shapefiles_east$Density <- factor(combined_shapefiles_east$Density, levels = c("Light", "Medium", "Heavy", "Unknown"))

# Simplify using st_simplify
CAN_fires_east_simplified <- ms_simplify(CAN_fires_east, keep = 0.05) # Adjust tolerance as needed
US_fires_east_simplified <- ms_simplify(US_fires_east, keep = 0.05)
US_fires_east_2023_simplified <- ms_simplify(US_fires_east_2023, keep = 0.05)

ggplot() + 
  # Add topography shading with its own color scale
  geom_raster(data = elevation_df, aes(x = x, y = y, fill = elevation), alpha = 0.1) + 
  scale_fill_gradientn(
    colours = c("#394A66", "#90BBD0", "#769C92", "#D1B792", "#2B7A5F", "#9E7455", "gray", "white"),
    values = scales::rescale(elevation_breaks),
    limits = elevation_limits,
    breaks = elevation_labels,
    labels = scales::label_comma(),
    guide = FALSE
  ) + 
  
  # Add new fill scale for density data
  new_scale_fill() + 
  geom_sf(data = combined_shapefiles_east, aes(fill = Density), color = NA, alpha = 0.02) + 
  scale_fill_manual(
    values = c("Light" = "gray90", "Medium" = "gray13", "Heavy" = "black", "Unknown" = "grey"), 
    name = "Density"
  ) + 
  
  # Plot polygons for CAN_fires_west and US_fires_west
  geom_sf(data = CAN_fires_east_simplified, fill = "red", color = NA, alpha = 1) + 
  geom_sf(data = US_fires_east_simplified, fill = "red", color = NA, alpha = 1) + 
  geom_sf(data = US_fires_east_2023_simplified, fill = "red", color = NA, alpha = 1) +
  
  # Add state/province boundaries
  geom_sf(data = states_provinces, fill = NA, color = "black", linetype = "solid") + 
  
  # Add labels and scale
  labs(
    title = "Aggregate Smoke and Fires - Eastern MT Highest 1% PM2.5 Days",
    x = "Longitude", 
    y = "Latitude",
    size = "Canadian Point Sources\n   tons per year\n(NOx+SOx+PM2.5+PM10)"
  ) + 
  theme_minimal() + 
  coord_sf(xlim = c(-130, -95), ylim = c(35, 60), expand = FALSE) 




# WEST
# Process input data:

# Replace NA with a default value, e.g., "Unknown"
combined_shapefiles_west$Density[is.na(combined_shapefiles_west$Density)] <- "Unknown"
combined_shapefiles_west$Density <- as.factor(combined_shapefiles_west$Density)

# Ensure Density is a factor and handle NA values
combined_shapefiles_west$Density <- factor(combined_shapefiles_west$Density, levels = c("Light", "Medium", "Heavy", "Unknown"))

# Simplify using st_simplify
CAN_fires_west_simplified <- ms_simplify(CAN_fires_west, keep = 0.05)
US_fires_west_simplified <- ms_simplify(US_fires_west, keep = 0.05)
US_fires_west_2023_simplified <- ms_simplify(US_fires_west_2023, keep = 0.05)

ggplot() + 
  # Add topography shading with its own color scale
  geom_raster(data = elevation_df, aes(x = x, y = y, fill = elevation), alpha = 0.1) + 
  scale_fill_gradientn(
    colours = c("#394A66", "#90BBD0", "#769C92", "#D1B792", "#2B7A5F", "#9E7455", "gray", "white"),
    values = scales::rescale(elevation_breaks),
    limits = elevation_limits,
    breaks = elevation_labels,
    labels = scales::label_comma(),
    guide = FALSE
  ) + 
  
  # Add new fill scale for density data
  new_scale_fill() + 
  geom_sf(data = combined_shapefiles_west, aes(fill = Density), color = NA, alpha = 0.02) + 
  scale_fill_manual(
    values = c("Light" = "gray90", "Medium" = "gray13", "Heavy" = "black", "Unknown" = "grey"), 
    name = "Density"
  ) + 
  
  # Plot polygons for CAN_fires_west and US_fires_west
  geom_sf(data = CAN_fires_west_simplified, fill = "red", color = NA, alpha = 1) + 
  geom_sf(data = US_fires_west_simplified, fill = "red", color = NA, alpha = 1) + 
  geom_sf(data = US_fires_west_2023_simplified, fill = "red", color = NA, alpha = 1) +
  
  # Add state/province boundaries
  geom_sf(data = states_provinces, fill = NA, color = "black", linetype = "solid") + 
  
  # Add labels and scale
  labs(
    title = "Aggregate Smoke and Fires - Western MT Highest 1% PM2.5 Days",
    x = "Longitude", 
    y = "Latitude",
    size = "Canadian Point Sources\n   tons per year\n(NOx+SOx+PM2.5+PM10)"
  ) + 
  theme_minimal() + 
  coord_sf(xlim = c(-130, -95), ylim = c(35, 60), expand = FALSE) 

