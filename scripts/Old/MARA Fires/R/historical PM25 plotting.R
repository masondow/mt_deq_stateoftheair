library(dplyr)
library(lubridate)

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
city_list_by_region <- pm25_samples %>%
  group_by(region) %>%
  summarize(cities = list(unique(city)))  # Replace 'city' with the actual column name if different

# Add day of year and year to the data
pm25_samples <- pm25_samples %>%
  mutate(
    day_of_year = yday(date_local),  # Extract day of year
    year = year(date_local)          # Extract year
  )

# Filter data for the 'east' region
pm25_samples_east <- pm25_samples %>%
  filter(region == "east")

# Filter data for the 'east' region
pm25_samples_west <- pm25_samples %>%
  filter(region == "west")

# Calculate aggregate daily average of arithmetic means for each day of the year
daily_averages <- pm25_samples %>%
  group_by(day_of_year) %>%
  summarize(daily_average = mean(arithmetic_mean, na.rm = TRUE))

# Calculate daily averages for the period 2003-2013
daily_averages_2003_2013 <- pm25_samples %>%
  filter(year >= 2003 & year <= 2013) %>%
  group_by(day_of_year) %>%
  summarize(daily_average = mean(arithmetic_mean, na.rm = TRUE)) %>%
  mutate(period = "2003-2013")

# Calculate daily averages for the period 2014-2023
daily_averages_2014_2023 <- pm25_samples %>%
  filter(year >= 2014 & year <= 2023) %>%
  group_by(day_of_year) %>%
  summarize(daily_average = mean(arithmetic_mean, na.rm = TRUE)) %>%
  mutate(period = "2014-2023")

# Combine the two data frames
combined_daily_averages <- bind_rows(daily_averages_2003_2013, daily_averages_2014_2023)

##### PLOTTING #####

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

##### PERIOD AVERAGES #####

# Plot with fill under the "2003-2013" line
p <- ggplot(combined_daily_averages, aes(x = day_of_year, y = daily_average, color = period)) +
  geom_ribbon(data = subset(combined_daily_averages, period == "2003-2013"),
              aes(ymin = 0, ymax = daily_average), fill = "gray80", alpha = 0.3) +  # Fill under 2003-2013 line
  geom_line(linewidth = 1) +  # Line for both periods
  labs(title = bquote(bold("Historical Daily Average PM"[2.5])),
       subtitle = "All Montana Monitors 2003 - 2023",
       x = "",
       y = expression("Daily Average PM"[2.5] * " (µg/m"^3 * ")"),
       color = "") +
  scale_color_manual(values = c("2003-2013" = "gray80", "2014-2023" = "#F54D28")) +
  scale_x_continuous(
    breaks = month_breaks,  # Set x-axis breaks for each month
    labels = month_labels,   # Corresponding month labels
    expand = c(0,0)
    ) +
  scale_y_continuous(
    expand = c(0,0),
    breaks = seq(0, 30, by = 5)  
    ) +
  theme_classic() +  # Classic theme for clean appearance
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 16, vjust = -.5),  # Adjust x-axis text size
    axis.text.y = element_text(size = 14),  # Adjust y-axis text size
    axis.title.x = element_text(size = 14),  # Adjust x-axis title size
    axis.title.y = element_text(size = 15),  # Adjust y-axis title size
    plot.title = element_text(size = 20, face = "bold", margin = margin(b = 10), vjust = -1),  # Adjust title size and margin
    plot.subtitle = element_text(size = 17, margin = margin(b = 10), vjust = 0),  # Adjust subtitle size, style, and margin
    legend.position = c(0.2, 0.85),  # Move the legend to the top left (x, y format)
    legend.title = element_text(size = 12),  # Adjust legend title size
    legend.text = element_text(size = 18),  # Adjust legend text size
    legend.key.size = unit(1.2, 'cm'),  # Adjust the overall size of legend keys
    legend.key.width = unit(1.0, 'cm'),  # Adjust the width of legend keys
    legend.key.height = unit(0.8, 'cm'),  # Adjust the height of legend keys
    panel.background = element_rect(fill = "transparent"),  # Set plot background to transparent
    plot.background = element_rect(fill = "transparent", color = NA),  # Set overall plot background to transparent with no border
    legend.background = element_rect(fill = "transparent", color = NA),  # Set legend background to transparent with no border
    panel.border = element_blank()  # Remove any panel border
  )

# Print the plot to check
print(p)

# Save the plot as a PNG with transparent background
ggsave("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/MARA Fires/Final Figures/historical_pm25_comparison.png", plot = p, bg = "transparent", width = 10, height = 6, dpi = 300)


##### WESTERN MT SCATTER #####

# Define colors for AQI categories and special cases for 2023 and other years
aqi_colors <- c(
  "Hazardous" = "#7E0023",       # Dark Red
  "Very Unhealthy" = "#8F3F97",  # Purple
  "Unhealthy" = "#FF0000",       # Red
  "Unhealthy for Sensitive Groups" = "#FF7E00",  # Orange
  "Good and Moderate" = "gray60",           # Gray for Good and Moderate in 2023
  "Other Years" = "gray80"             # Gray for non-2023 and other AQI categories
)

# Add a new column to distinguish color and size
pm25_samples_west <- pm25_samples_west %>%
  mutate(
    color_group = case_when(
      year == 2017 & AQI_category %in% names(aqi_colors) ~ AQI_category,
      year == 2017 ~ "Good and Moderate",
      TRUE ~ "Other Years"
    ),
    size_group = ifelse(year == 2017 & color_group == "Good and Moderate", 2, 3)  # Smaller size for Good and Moderate in 2023
  )

# Create the scatter plot with layered points
ggplot(pm25_samples_west) +
  # Plot other years data with gray color
  geom_point(data = subset(pm25_samples_west, year != 2017),
             aes(x = day_of_year, y = arithmetic_mean),
             color = "gray80", alpha = 0.5, size = 2,  
             show.legend = TRUE) +  # Ensure 'Other Years' is included in the legend
  # Plot 2023 data with AQI colors and specific sizes
  geom_point(data = subset(pm25_samples_west, year == 2017),
             aes(x = day_of_year, y = arithmetic_mean, color = color_group, size = size_group),
             alpha = 1) +  # Plot 2023 data on top
  scale_x_continuous(
    breaks = month_breaks,  # Set x-axis breaks for each month
    labels = month_labels   # Corresponding month labels
  ) +
  scale_color_manual(values = aqi_colors) +  # Define colors for AQI categories
  scale_size_continuous(range = c(2, 3)) +   # Adjust size range if needed
  labs(title = "Western MT - PM2.5 Daily Averages 2003-2023",
       x = "Month",
       y = "PM2.5 ug/m3",
       color = "2017 AQI Category") +
  guides(size = "none") +  # Remove size legend
  theme_minimal()

##### EASTERN MT and SCATTER #####

# Add a new column to distinguish color and size
pm25_samples_east <- pm25_samples_east %>%
  mutate(
    color_group = case_when(
      year == 2023 & AQI_category %in% names(aqi_colors) ~ AQI_category,
      year == 2023 ~ "Good and Moderate",
      TRUE ~ "Other Years"
    ),
    size_group = ifelse(year == 2023 & color_group == "Good and Moderate", 2, 3)  # Smaller size for Good and Moderate in 2023
  )

# Create the scatter plot with layered points
ggplot(pm25_samples_east) +
  # Plot other years data with gray color
  geom_point(data = subset(pm25_samples_east, year != 2023),
             aes(x = day_of_year, y = arithmetic_mean),
             color = "gray80", alpha = 0.5, size = 2,  
             show.legend = TRUE) +  # Ensure 'Other Years' is included in the legend
  # Plot 2023 data with AQI colors and specific sizes
  geom_point(data = subset(pm25_samples_east, year == 2023),
             aes(x = day_of_year, y = arithmetic_mean, color = color_group, size = size_group),
             alpha = 1) +  # Plot 2023 data on top
  scale_x_continuous(
    breaks = month_breaks,  # Set x-axis breaks for each month
    labels = month_labels   # Corresponding month labels
  ) +
  scale_color_manual(values = aqi_colors) +  # Define colors for AQI categories
  scale_size_continuous(range = c(2, 3)) +   # Adjust size range if needed
  labs(title = "Eastern MT - PM2.5 Daily Averages 2003-2023",
       x = "Month",
       y = "PM2.5 ug/m3",
       color = "2023 AQI Category") +
  guides(size = "none") +  # Remove size legend
  theme_minimal()

##### AQI Days/Year #####

# Define day of year range for May 1st to October 31st
start_day <- 121  # May 1st
end_day <- 304    # October 31st

# Filter data between May 1st and October 31st for all years
filtered_data <- pm25_samples %>%
  filter(day_of_year >= start_day & day_of_year <= end_day)

# Count the number of days per AQI category for each year
daily_counts <- filtered_data %>%
  group_by(year, AQI_category) %>%
  summarize(day_count = n(), .groups = 'drop')  # Count the number of rows in each group

# Count the total number of days within the period for each year
total_days_per_year <- filtered_data %>%
  group_by(year) %>%
  summarize(total_days = n(), .groups = 'drop')  # Count the number of rows (i.e., days) for each year

# Join the daily counts with the total days
final_counts <- daily_counts %>%
  left_join(total_days_per_year, by = "year") %>%
  mutate(percentage = (day_count / total_days) * 100)  # Calculate percentage of days for each AQI category

# Define colors for AQI categories
aqi_colors <- c(
  "Hazardous" = "#7E0023",            # Dark Red
  "Very Unhealthy" = "#8F3F97",       # Purple
  "Unhealthy" = "#FF0000",            # Red
  "Unhealthy for Sensitive Groups" = "#FF7E00",  # Orange
  "Moderate" = "#FFFF00",             # Yellow
  "Good" = "#00FF00"                  # Green
)

# Ensure AQI_category is a factor with levels in the reverse order of aqi_colors
final_counts <- final_counts %>%
  mutate(AQI_category = factor(AQI_category, levels = rev(names(aqi_colors))))

# Create stacked bar plot
p <- ggplot(final_counts, aes(x = factor(year), y = percentage, fill = AQI_category)) +
  geom_bar(stat = "identity") +
  labs(title = bquote(bold("Air Quality Index") ~ "(AQI)" ~ bold("During Wildfire Smoke Season") ~ "(May - October)"),
       x = "",
       y = "Percentage of Days",
       fill = "AQI Category") +
  scale_fill_manual(values = aqi_colors) +
  scale_y_continuous(
    expand = c(0, 0)
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ # Angle x-axis labels
  theme(
    plot.margin = margin(1, 1, 1, 1),  # Remove margins around the plot
    
    # Adjust text sizes
    plot.title = element_text(size = 16, face = "bold"),  # Title text size
    axis.title.x = element_text(size = 14),  # X-axis title text size
    axis.title.y = element_text(size = 14),  # Y-axis title text size
    axis.text.x = element_text(size = 10),   # X-axis tick text size
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
ggsave("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/MARA Fires/Final Figures/wildfire_season_AQI_days.png", plot = p, bg = "transparent", width = 10, height = 6, dpi = 300)


##### West AQI Days/Year #####

# Define day of year range for May 1st to October 31st
start_day <- 121  # May 1st
end_day <- 304    # October 31st

# Filter data between May 1st and October 31st for all years
filtered_data <- pm25_samples_west %>%
  filter(day_of_year >= start_day & day_of_year <= end_day)

# Count the number of days per AQI category for each year
daily_counts <- filtered_data %>%
  group_by(year, AQI_category) %>%
  summarize(day_count = n(), .groups = 'drop')  # Count the number of rows in each group

# Count the total number of days within the period for each year
total_days_per_year <- filtered_data %>%
  group_by(year) %>%
  summarize(total_days = n(), .groups = 'drop')  # Count the number of rows (i.e., days) for each year

# Join the daily counts with the total days
final_counts <- daily_counts %>%
  left_join(total_days_per_year, by = "year") %>%
  mutate(percentage = (day_count / total_days) * 100)  # Calculate percentage of days for each AQI category

# Define colors for AQI categories
aqi_colors <- c(
  "Hazardous" = "#7E0023",            # Dark Red
  "Very Unhealthy" = "#8F3F97",       # Purple
  "Unhealthy" = "#FF0000",            # Red
  "Unhealthy for Sensitive Groups" = "#FF7E00",  # Orange
  "Moderate" = "#FFFF00",             # Yellow
  "Good" = "#00FF00"                  # Green
)

# Ensure AQI_category is a factor with levels in the reverse order of aqi_colors
final_counts <- final_counts %>%
  mutate(AQI_category = factor(AQI_category, levels = rev(names(aqi_colors))))

# Create stacked bar plot
ggplot(final_counts, aes(x = factor(year), y = percentage, fill = AQI_category)) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of AQI Category Days (May-October All Monitors)",
       x = "Year",
       y = "Percentage of Days",
       fill = "AQI Category") +
  scale_fill_manual(values = aqi_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Angle x-axis labels

##### East AQI Days/Year #####

# Define day of year range for May 1st to October 31st
start_day <- 121  # May 1st
end_day <- 304    # October 31st

# Filter data between May 1st and October 31st for all years
filtered_data <- pm25_samples_east %>%
  filter(day_of_year >= start_day & day_of_year <= end_day)

# Count the number of days per AQI category for each year
daily_counts <- filtered_data %>%
  group_by(year, AQI_category) %>%
  summarize(day_count = n(), .groups = 'drop')  # Count the number of rows in each group

# Count the total number of days within the period for each year
total_days_per_year <- filtered_data %>%
  group_by(year) %>%
  summarize(total_days = n(), .groups = 'drop')  # Count the number of rows (i.e., days) for each year

# Join the daily counts with the total days
final_counts <- daily_counts %>%
  left_join(total_days_per_year, by = "year") %>%
  mutate(percentage = (day_count / total_days) * 100)  # Calculate percentage of days for each AQI category

# Define colors for AQI categories
aqi_colors <- c(
  "Hazardous" = "#7E0023",            # Dark Red
  "Very Unhealthy" = "#8F3F97",       # Purple
  "Unhealthy" = "#FF0000",            # Red
  "Unhealthy for Sensitive Groups" = "#FF7E00",  # Orange
  "Moderate" = "#FFFF00",             # Yellow
  "Good" = "#00FF00"                  # Green
)

# Ensure AQI_category is a factor with levels in the reverse order of aqi_colors
final_counts <- final_counts %>%
  mutate(AQI_category = factor(AQI_category, levels = rev(names(aqi_colors))))

# Create stacked bar plot
ggplot(final_counts, aes(x = factor(year), y = percentage, fill = AQI_category)) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of AQI Category Days (May-October All Monitors)",
       x = "Year",
       y = "Percentage of Days",
       fill = "AQI Category") +
  scale_fill_manual(values = aqi_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Angle x-axis labels
