library(dplyr)
library(lubridate)
library(stringr)
library(readxl)
library(magrittr)
library(ggplot2)

##### Libby Daily PM2.5 #####

daily_pm25 <- readRDS("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/DATA/2003_2023_daily_pm25_88101_88502_all_MT.rds")
hourly_pm25 <- readRDS("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/DATA/2003_2023_hourly_pm25_88101_88502_all_MT.rds")

# Define qualifiers (for exclusion later)
qualifiers <- c(
  "RT - Wildfire-U. S.",
  "RF - Fire - Canadian.",
  "IF - Fire - Canadian.",
  "IT - Wildfire-U. S.",
  "E - Forest Fire."
)

# Convert date_local to Date and summarize only the specified qualifiers
flags <- hourly_pm25 %>%
  mutate(date_local = as.Date(date_local, format = "%Y-%m-%d")) %>%
  group_by(date_local, state_code, county_code, site_number, poc) %>%
  summarize(qualifier = paste(
    unique(na.omit(qualifier[qualifier %in% qualifiers])), 
    collapse = ", "
  )) %>%
  ungroup()

daily_pm25_flags <- daily_pm25 %>%
  filter(sample_duration == "1 HOUR" & validity_indicator == "Y") %>%
  mutate(
    date_local = as.Date(date_local, format = "%Y-%m-%d"),
    year = year(date_local),       # Extract year from date_local
    month = month(date_local),     # Extract month from date_local
    quarter = case_when(           # Calculate quarter based on month
      month %in% 1:3 ~ 1,
      month %in% 4:6 ~ 2,
      month %in% 7:9 ~ 3,
      month %in% 10:12 ~ 4
    )
  ) %>%
  distinct(date_local, local_site_name, poc, .keep_all = TRUE)

# Merge flags with daily_pm25 by multiple columns
daily_pm25_flags <- daily_pm25_flags %>% 
  left_join(flags, by = c("date_local", "state_code", "county_code", "site_number", "poc"))

Libby_pm25_flags <- daily_pm25_flags %>%
  filter(local_site_name == "Libby Courthouse Annex" &
           year >= 2021 & year <= 2023) %>%
  select(date_local, arithmetic_mean, qualifier)

##### HYSPLIT #####

am <- read.csv("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/EE Demos/TSD/Libby 5 factor analysis/HYSPLIT points/300530018_AM24.csv", header = FALSE)
colnames(am) <- c("SiteID", "Date", "TrajHr", "Lat", "Lon")
am <- am %>%
  mutate(run = "am")

pm <- read.csv("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/EE Demos/TSD/Libby 5 factor analysis/HYSPLIT points/300530018_PM24.csv", header = FALSE)
colnames(pm) <- c("SiteID", "Date", "TrajHr", "Lat", "Lon")
pm <- pm %>%
  mutate(run = "pm")

# Merge am and pm data frames by stacking rows
hys <- rbind(am, pm)

# View the first few rows of the combined data
head(hys)

# Convert Libby_pm25_flags$date_local to integer
Libby_pm25_flags <- Libby_pm25_flags %>%
  mutate(Date = as.integer(format(date_local, "%Y%m%d")))  # Convert to YYYYMMDD format

# Merge hys with Libby_pm25_flags based on Date
hys <- hys %>%
  left_join(Libby_pm25_flags %>% select(Date, arithmetic_mean, qualifier), by = "Date")

# Filter the data to only include rows where qualifier is not empty
hys_EE <- hys %>%
  filter(qualifier != "")

# Filter the data to only include rows where qualifier is not empty
hys_no_EE <- hys %>%
  filter(qualifier == "")

# Filter the data to include only rows above the 80th percentile in one step
hys_80 <- hys %>%
  filter(arithmetic_mean > quantile(arithmetic_mean, probs = 0.8, na.rm = TRUE))

# Filter the data to include only rows above the 80th percentile in one step
hys_20 <- hys %>%
  filter(arithmetic_mean < quantile(arithmetic_mean, probs = 0.2, na.rm = TRUE))

##### MAP Parameters #####

library(sf)
library(terra)
library(tigris)

# Get the county boundaries for the desired states (WA, ID, MT)
counties <- counties(cb = TRUE)  # Download county boundaries for all US

# Filter for Washington (WA), Idaho (ID), and Montana (MT)
counties_filtered <- counties[counties$STUSPS %in% c("WA", "ID", "MT"), ]

# Convert to sf object if needed
counties_sf <- st_as_sf(counties_filtered)

# Get the state boundaries for the desired states (WA, ID, MT)
states <- states(cb = TRUE)  # Download state boundaries for all US

# Filter for Washington (WA), Idaho (ID), and Montana (MT)
states_filtered <- states[states$STUSPS %in% c("WA", "ID", "MT"), ]

# Convert to sf object if needed
states_sf <- st_as_sf(states_filtered)

# Define the coordinates for the point (48.392, -115.553)
Libby <- st_sfc(st_point(c(-115.553, 48.392)), crs = 4326)

# Define the latitude and longitude limits you want to zoom in on
lon_min <- -118.25
lon_max <- -114.5
lat_min <- 46.5
lat_max <- 49.1

##### MAP Upper 20% #####

# Create POINT geometries for each Lat/Lon pair using purrr::pmap to iterate over rows
library(purrr)

# Create POINT geometries
points_80 <- pmap(list(hys_80$Lon, hys_80$Lat), ~st_point(c(..1, ..2)))

# Convert to an sf object with the POINT geometries
hys_sf_points_80 <- st_sf(hys_80, geometry = st_sfc(points_80, crs = 4326))

# Check the structure of the new sf object
head(hys_sf_points_80)

# Group by 'run' and 'Date' and summarize the data
hys_sf_lines_80 <- hys_sf_points_80 %>% 
  dplyr::group_by(run, Date) %>% 
  dplyr::summarise(
    geometry = st_combine(geometry),
    arithmetic_mean = first(arithmetic_mean),  # Retain the arithmetic_mean
    .groups = "drop"  # Avoid keeping the grouping after summarizing
  ) %>% 
  st_cast("LINESTRING")

# Remove lines where 'arithmetic_mean' is NA
hys_sf_lines_clean_80 <- hys_sf_lines_80 %>%
  dplyr::filter(!is.na(arithmetic_mean))

# Extract the bounding box from the sf object
bbox_80 <- st_bbox(hys_sf_lines_clean_80)

# Create the raster template
raster_template_80 <- rast(
  xmin = bbox_80["xmin"], xmax = bbox_80["xmax"],
  ymin = bbox_80["ymin"], ymax = bbox_80["ymax"],
  resolution = 0.05,  # Set resolution in degrees
  crs = "EPSG:4326"
)

# Rasterize the lines, summing the 'arithmetic_mean' values for each intersected cell
raster_counts_sum_80 <- rasterize(
  hys_sf_lines_clean_80, raster_template_80, 
  fun = "count",  # Sum the 'arithmetic_mean' values
  background = 0  # Cells with no intersections will have the value 0
)

library(stars)
# Assuming raster_counts is your SpatRaster object
# Convert SpatRaster to stars object
raster_counts_stars_80 <- st_as_stars(raster_counts_sum_80)

raster_values_80 <- raster_counts_stars_80$layer

raster_binned_80 <- cut(
  raster_values_80,
  breaks = c(0, 10, 20, 30, 40, 50, 60, Inf),  # Specify bin edges
  labels = c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60+"),
  include.lowest = TRUE
)

raster_counts_stars_80$layer_binned <- as.factor(raster_binned_80)

ggplot() +
  geom_stars(data = raster_counts_stars_80, aes(fill = layer_binned)) +  # Use binned layer
  scale_fill_manual(
    values = c("#ffffcc", "#a1dab4", "#41b6c4", "#2c7fb8", "#253494", "#d73027", "#7f0000"),  # Custom colors
    name = "Trajectories"
  ) +
  geom_sf(data = counties_sf, fill = NA, color = "gray", size = 0.5) +
  geom_sf(data = states_sf, fill = NA, color = "black") +
  geom_sf(data = Libby, color = "black", size = 3) +  # Add the point
  labs(title = "Transport on Highest PM2.5 Days",
       subtitle = "80th percentile and above",
       x = "Longitude", y = "Latitude") +
  theme_minimal() +
  coord_sf(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max)) + 
  theme(
    axis.title.x = element_blank(),   # Remove x axis title
    axis.title.y = element_blank(),   # Remove y axis title
    axis.text.x = element_blank(),    # Remove x axis text
    axis.text.y = element_blank()     # Remove y axis text
  )

##### MAP Lower 20% #####

# Create POINT geometries for each Lat/Lon pair using purrr::pmap to iterate over rows
library(purrr)

# Create POINT geometries
points_20 <- pmap(list(hys_20$Lon, hys_20$Lat), ~st_point(c(..1, ..2)))

# Convert to an sf object with the POINT geometries
hys_sf_points_20 <- st_sf(hys_20, geometry = st_sfc(points_20, crs = 4326))

# Check the structure of the new sf object
head(hys_sf_points_20)

# Group by 'run' and 'Date' and summarize the data
hys_sf_lines_20 <- hys_sf_points_20 %>% 
  dplyr::group_by(run, Date) %>% 
  dplyr::summarise(
    geometry = st_combine(geometry),
    arithmetic_mean = first(arithmetic_mean),  # Retain the arithmetic_mean
    .groups = "drop"  # Avoid keeping the grouping after summarizing
  ) %>% 
  st_cast("LINESTRING")

# Remove lines where 'arithmetic_mean' is NA
hys_sf_lines_clean_20 <- hys_sf_lines_20 %>%
  dplyr::filter(!is.na(arithmetic_mean))

# Extract the bounding box from the sf object
bbox_20 <- st_bbox(hys_sf_lines_clean_20)

# Create the raster template
raster_template_20 <- rast(
  xmin = bbox_20["xmin"], xmax = bbox_20["xmax"],
  ymin = bbox_20["ymin"], ymax = bbox_20["ymax"],
  resolution = 0.05,  # Set resolution in degrees
  crs = "EPSG:4326"
)

# Rasterize the lines, summing the 'arithmetic_mean' values for each intersected cell
raster_counts_sum_20 <- rasterize(
  hys_sf_lines_clean_20, raster_template_20, 
  fun = "count",  # Sum the 'arithmetic_mean' values
  background = 0  # Cells with no intersections will have the value 0
)

library(stars)
# Assuming raster_counts is your SpatRaster object
# Convert SpatRaster to stars object
raster_counts_stars_20 <- st_as_stars(raster_counts_sum_20)

raster_values_20 <- raster_counts_stars_20$layer

raster_binned_20 <- cut(
  raster_values_20,
  breaks = c(0, 10, 20, 30, 40, 50, 60, Inf),  # Specify bin edges
  labels = c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60+"),
  include.lowest = TRUE
)

raster_counts_stars_20$layer_binned <- as.factor(raster_binned_20)

ggplot() +
  geom_stars(data = raster_counts_stars_20, aes(fill = layer_binned)) +  # Use binned layer
  scale_fill_manual(
    values = c("#ffffcc", "#a1dab4", "#41b6c4", "#2c7fb8", "#253494", "#d73027", "#7f0000"),  # Custom colors
    name = "Trajectories"
  ) +
  geom_sf(data = counties_sf, fill = NA, color = "gray", size = 0.5) +
  geom_sf(data = states_sf, fill = NA, color = "black") +
  geom_sf(data = Libby, color = "black", size = 3) +  # Add the point
  labs(title = "Transport on Lowest PM2.5 Days",
       subtitle = "20th percentile and below",
       x = "Longitude", y = "Latitude") +
  theme_minimal() +
  coord_sf(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max)) + 
  theme(
    axis.title.x = element_blank(),   # Remove x axis title
    axis.title.y = element_blank(),   # Remove y axis title
    axis.text.x = element_blank(),    # Remove x axis text
    axis.text.y = element_blank()     # Remove y axis text
  )

##### Statistics #####

# Calculate the average of the arithmetic_mean column
average_80 <- mean(hys_sf_lines_clean_80$arithmetic_mean, na.rm = TRUE)

# Print the result
average_80

# Calculate the average of the arithmetic_mean column
average_20 <- mean(hys_sf_lines_clean_20$arithmetic_mean, na.rm = TRUE)

# Print the result
average_20

