library(dplyr)
library(lubridate)
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

##### Site List #####

# Create the site_list data frame
site_list <- pm25_samples %>%
  select(latitude, longitude, region, local_site_name) %>%  # Select relevant columns
  distinct(local_site_name, .keep_all = TRUE)  # Keep distinct local_site_name values

##### Mapping #####

# Load required libraries
library(ggplot2)
library(rnaturalearth)
library(sf)

# Download US states as sf object
us_states <- ne_states(country = "United States of America", returnclass = "sf")

# Filter Montana from the US states data
montana <- us_states[us_states$name == "Montana", ]

# Create a map of Montana with site points
p <- ggplot() +
  geom_sf(data = montana, fill = "gray90", color = "white", size = 1) +  # Montana with gray fill and white borders
  # Add point data from site_list with different colors for "west" and "east"
  geom_point(data = site_list, aes(x = longitude, y = latitude, color = region),
             size = 15) +  # Adjust size of points as needed
  # Set custom colors for "west" and "east" regions
  scale_color_manual(values = c("west" = "#004A98", "east" = "#009ADE")) +
  # Remove the legend
  guides(color = "none") +
  # Set theme with minimal background and axis lines removed
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA),  # Remove background border
    plot.background = element_rect(fill = "transparent", color = NA)  # Remove plot border
  )

# Print the plot to check
print(p)

# Save the plot as a PNG with transparent background
ggsave("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/MARA Fires/Final Figures/MT_east_west_map.png", plot = p, bg = "transparent", width = 10, height = 6, dpi = 300)


