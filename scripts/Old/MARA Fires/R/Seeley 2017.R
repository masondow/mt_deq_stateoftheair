library(dplyr)
library(lubridate)
library(ggplot2)
library(purrr)
library(tidyr)

##### Load Data #####

# Load the data frame from the RDS file
pm25_samples <- readRDS("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/MARA Fires/R/Data/pm25_samples_all_continuous.rds")

# Convert the date_local column to Date format
pm25_samples$date_local <- as.Date(pm25_samples$date_local, format = "%Y-%m-%d")
# Truncate the numbers in the arithmetic_mean column to the first decimal place
pm25_samples$arithmetic_mean <- floor(pm25_samples$arithmetic_mean * 10) / 10

# Define breakpoints for PM2.5 AQI categories
breakpoints <- tibble(
  category = c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy", "Very Unhealthy", "Hazardous"),
  min = c(-1000, 9.1, 35.5, 55.5, 125.5, 225.5),
  max = c(9.0, 35.4, 55.4, 125.4, 225.4, Inf)
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

##### Seeley Lake #####

# Calculate daily average for Seeley Elementary School excluding year 2017
seeley_daily_average <- pm25_samples %>%
  filter(local_site_name == "Seeley Elementary School") %>%  # Filter for the specific site
  filter(year != 2017) %>%                                    # Exclude year 2017
  select(year, day_of_year, arithmetic_mean) %>%              # Select relevant columns
  group_by(day_of_year) %>%                                  # Group by day_of_year
  summarize(daily_average = mean(arithmetic_mean, na.rm = TRUE))  # Calculate the daily average

seeley_2017 <- pm25_samples %>%
  filter(year == 2017 & local_site_name == "Seeley Elementary School") %>%
  select(day_of_year, arithmetic_mean)

# Ensure unique values for day_of_year, keeping the first occurrence
seeley_2017<- seeley_2017 %>%
  distinct(day_of_year, .keep_all = TRUE)


##### PLOTTING #####

library(ggplot2)

# Define breakpoints for PM2.5 AQI categories
breakpoints <- tibble(
  category = c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy", "Very Unhealthy", "Hazardous"),
  breaks = c(9.0, 35.4, 55.4, 125.4, 225.4, Inf)
)

# Define AQI colors
aqi_colors <- c(
  "Good" = "#00E400",                          # Green
  "Moderate" = "#FFFF00",                       # Yellow
  "Unhealthy for Sensitive Groups" = "#FF7E00", # Orange
  "Unhealthy" = "#FF0000",                      # Red
  "Very Unhealthy" = "#8F3F97",                 # Purple
  "Hazardous" = "#7E0023"                       # Dark Red
)

# Assign AQI categories based on breakpoints
seeley_2017 <- seeley_2017 %>%
  mutate(
    AQI_category = cut(arithmetic_mean,
                       breaks = c(-Inf, breakpoints$breaks),
                       labels = breakpoints$category,
                       right = TRUE)
  )

# Define month breaks
month_breaks <- c(
  yday(as.Date("2000-01-01")),  # January
  yday(as.Date("2000-02-01")),  # February
  yday(as.Date("2000-03-01")),  # March
  yday(as.Date("2000-04-01")),  # April
  yday(as.Date("2000-05-01")),  # May
  yday(as.Date("2000-06-01")),  # June
  yday(as.Date("2000-07-01")),  # July
  yday(as.Date("2000-08-01")),  # August
  yday(as.Date("2000-09-01")),  # September
  yday(as.Date("2000-10-01")),  # October
  yday(as.Date("2000-11-01")),  # November
  yday(as.Date("2000-12-01"))   # December
)

# Define month labels
month_labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# Define day-of-year range for May 1st to October 31st
start_day <- 121
end_day <- 304

# Assuming 'seeley_2017' is your dataset
seeley_2017 <- seeley_2017 %>%
  mutate(AQI_category = factor(AQI_category, levels = c(
    "Hazardous", 
    "Very Unhealthy", 
    "Unhealthy", 
    "Unhealthy for Sensitive Groups", 
    "Moderate", 
    "Good"
  )))

# Create the plot with both datasets
p <- ggplot() +  
  geom_line(data = seeley_2017, 
            aes(x = day_of_year, y = arithmetic_mean, color = AQI_category), 
            size = 3, color = "gray", alpha = 0.5) +
  geom_point(data = seeley_2017, 
             aes(x = day_of_year, y = arithmetic_mean, color = AQI_category), 
             size = 5) +
  scale_color_manual(values = aqi_colors) +
  scale_fill_manual(values = "black") +  # Fill color for the ribbon
  scale_x_continuous(
    limits = c(121, 305),  # Assuming May 1st to October 31st range; adjust as needed
    breaks = month_breaks,
    labels = month_labels,
    expand = c(0, 0)  # Remove extra space on x-axis
  ) +
  scale_y_continuous(
    limits = c(0, 675),
    expand = c(0, 0)  # Remove extra space on y-axis
  ) +
  labs(
    title = "Seeley Lake 2017 Wildfire Season Air Quality", 
    x = "", 
    y = expression("PM"[2.5] * " (µg/m"^3 * ") 24-hr Average"), 
    color = "AQI Category", 
    fill = "Historical Data"
  ) +
  theme_minimal() +
  theme(
    plot.margin = margin(1, 1, 1, 1),  # Remove margins around the plot
    legend.position = c(0.4, 0.9),     # Position legend at the top right
    legend.justification = c(1, 1),    # Anchor the legend at the top right
    
    # Adjust text sizes
    plot.title = element_text(size = 16, face = "bold"),  # Title text size
    axis.title.x = element_text(size = 14),  # X-axis title text size
    axis.title.y = element_text(size = 14),  # Y-axis title text size
    axis.text.x = element_text(size = 12),   # X-axis tick text size
    axis.text.y = element_text(size = 12),   # Y-axis tick text size
    legend.text = element_text(size = 12),   # Legend text size
    legend.title = element_text(size = 14),  # Legend title text size
    
    # Set transparent background
    panel.background = element_rect(fill = "transparent", color = NA), # Background of the plotting area
    plot.background = element_rect(fill = "transparent", color = NA),  # Background of the entire plot
    legend.background = element_rect(fill = "transparent", color = NA), # Background of the legend
    
    #  vertical grid lines
    panel.grid.major.x = element_line(color = alpha("gray", 0.2), size = 0.5),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = alpha("gray", 0.2), size = 0.5),
    panel.grid.minor.y = element_line(color = alpha("gray", 0.2), size = 0.5)
  )

# Print the plot to check
print(p)

# Save the plot as a PNG with transparent background
ggsave("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/MARA Fires/Final Figures/Seeley_2017_daily_pm25.png", plot = p, bg = "transparent", width = 10, height = 6, dpi = 300)










