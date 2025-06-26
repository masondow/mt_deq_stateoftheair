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

##### Annual Averages #####

# Assuming pm25_samples is your data frame
pm25_filtered <- pm25_samples %>%
  # Convert date_local to Date if it's not already
  mutate(date_local = as.Date(date_local)) %>%
  # Filter for dates between May 1st and October 31st
  filter(format(date_local, "%m-%d") >= "05-01" & format(date_local, "%m-%d") <= "10-31") %>%
  # Group by year and calculate the average of arithmetic_mean
  group_by(year) %>%
  summarise(average_arithmetic_mean = mean(arithmetic_mean, na.rm = TRUE)) %>%
  # Optionally, you can ungroup if needed
  ungroup()

# Assuming pm25_samples is your data frame
pm25_filtered_east <- pm25_samples %>%
  # Convert date_local to Date if it's not already
  mutate(date_local = as.Date(date_local)) %>%
  # Filter for dates between May 1st and October 31st
  filter(region == "east" & format(date_local, "%m-%d") >= "05-01" & format(date_local, "%m-%d") <= "10-31") %>%
  # Group by year and calculate the average of arithmetic_mean
  group_by(year) %>%
  summarise(average_arithmetic_mean = mean(arithmetic_mean, na.rm = TRUE)) %>%
  # Optionally, you can ungroup if needed
  ungroup() %>%
  mutate(`MT Region` = "Eastern")

# Assuming pm25_samples is your data frame
pm25_filtered_west <- pm25_samples %>%
  # Convert date_local to Date if it's not already
  mutate(date_local = as.Date(date_local)) %>%
  # Filter for dates between May 1st and October 31st
  filter(region == "west" & format(date_local, "%m-%d") >= "05-01" & format(date_local, "%m-%d") <= "10-31") %>%
  # Group by year and calculate the average of arithmetic_mean
  group_by(year) %>%
  summarise(average_arithmetic_mean = mean(arithmetic_mean, na.rm = TRUE)) %>%
  # Optionally, you can ungroup if needed
  ungroup() %>%
  mutate(`MT Region` = "Western")

##### Trend Lines #####

# Fit a linear model
model <- lm(average_arithmetic_mean ~ year, data = pm25_filtered_east)

# Use the model to generate predictions
pm25_filtered_east <- pm25_filtered_east %>%
  mutate(trend_line = predict(model, newdata = .))

# Fit a linear model
model <- lm(average_arithmetic_mean ~ year, data = pm25_filtered_west)

# Use the model to generate predictions
pm25_filtered_west <- pm25_filtered_west %>%
  mutate(trend_line = predict(model, newdata = .))

##### PLOTTING #####

pm25_east_west <- bind_rows(pm25_filtered_east, pm25_filtered_west)
pm25_east_west <- pm25_east_west %>%
  filter(year >= 2008 & year <= 2023)

pm25_east <- pm25_east_west %>%
  filter(`MT Region` == "Eastern")

pm25_west <- pm25_east_west %>%
  filter(`MT Region` == "Western")

# Group by local_site_name and year, then calculate the annual average arithmetic_mean
annual_pm25_average_by_site <- pm25_samples %>%
  filter(year >= 2008 & format(date_local, "%m-%d") >= "05-01" & format(date_local, "%m-%d") <= "10-31") %>%
  group_by(local_site_name, year) %>%
  summarize(average_arithmetic_mean = mean(arithmetic_mean, na.rm = TRUE)) %>%
  ungroup()  # Ungroup after summarizing

library(grid)

# Read the image file as a grob (graphical object)
img <- png::readPNG("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/MARA Fires/Final Figures/MT_east_west_map.png")
img_grob <- grid::rasterGrob(img, interpolate = TRUE)

p <- ggplot() +
  geom_line(data = annual_pm25_average_by_site,
            aes(x = year, y = average_arithmetic_mean, group = local_site_name, color = "All Monitors"),
            linewidth = 1, alpha = .5) +
  geom_line(data = pm25_east, 
            aes(x = year, y = average_arithmetic_mean, color = "Eastern Montana Average"), 
            linewidth = 1.5) +
  geom_line(data = pm25_west, 
            aes(x = year, y = average_arithmetic_mean, color = "Western Montana Average"), 
            linewidth = 1.5) +
  geom_smooth(data = pm25_east_west, 
              aes(x = year, y = average_arithmetic_mean, color = "Statewide Trend"), 
              method = "lm", linetype = "solid", se = FALSE, linewidth = 2) +
  scale_y_continuous(
    limits = c(0, 28),
    expand = c(0,0)
  ) +
  scale_x_continuous(
    breaks = seq(2005, 2023, by = 5)  # Tick marks every year
  ) +
  scale_color_manual(
    values = c("All Monitors" = "gray80", 
               "Eastern Montana Average" = "#009ADE", 
               "Western Montana Average" = "#004A98", 
               "Statewide Trend" = "#F54D28"),
    breaks = c("All Monitors", "Eastern Montana Average", "Western Montana Average", "Statewide Trend")  # Order of legend items
  ) + 
  labs(
    title = bquote(bold("Wildfire Smoke Season Average PM"[2.5])),  # Title with subscript
    subtitle = "May - October",  # Subtitle not bold
    x = "", 
    y = expression("Average PM"[2.5] * " (µg/m³)"),  # Subscript in y-axis label
    color = ""  # Custom legend title
  ) +
  theme_classic() +  # Classic theme for clean appearance
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14),  # Adjust x-axis text size
    axis.text.y = element_text(size = 14),  # Adjust y-axis text size
    axis.title.x = element_text(size = 14),  # Adjust x-axis title size
    axis.title.y = element_text(size = 15),  # Adjust y-axis title size
    plot.title = element_text(size = 20, face = "bold", margin = margin(b = 10)),  # Adjust title size and margin
    plot.subtitle = element_text(size = 18, face = "plain", vjust = 3),  # Subtitle not bold
    legend.position = c(0.17, 0.93),  # Move the legend to the top left (x, y format)
    legend.title = element_text(size = 12),  # Adjust legend title size
    legend.text = element_text(size = 14),  # Adjust legend text size
    panel.background = element_rect(fill = "transparent"),  # Set plot background to transparent
    plot.background = element_rect(fill = "transparent", color = NA),  # Set overall plot background to transparent with no border
    legend.background = element_rect(fill = "transparent", color = NA),  # Set legend background to transparent with no border
    panel.border = element_blank()  # Remove any panel border
  ) +
  # Add image inset
  annotation_custom(
    img_grob,
    xmin = 2006.5, xmax = 2012.5,  # Adjust the x coordinates to place the inset in the top left
    ymin = 14, ymax = 22  # Adjust the y coordinates as necessary
  )
  
# Print the plot to check
print(p)

# Save the plot as a PNG with transparent background
ggsave("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/MARA Fires/Final Figures/annual_average_pm25.png", plot = p, bg = "transparent", width = 10, height = 6, dpi = 300)

