library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)

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

# Define day of year range for May 1st to October 31st
start_day <- 121  # May 1st
end_day <- 304    # October 31st

# Filter data between May 1st and October 31st for all years
filtered_data <- pm25_samples %>%
  filter(day_of_year >= start_day & day_of_year <= end_day)

# Create a grid of all combinations of years and AQI categories
all_combinations <- expand.grid(
  year = unique(filtered_data$year),
  AQI_category = breakpoints$category
)

# Count the number of days per AQI category for each year
daily_counts <- filtered_data %>%
  group_by(year, AQI_category) %>%
  summarize(day_count = n(), .groups = 'drop')

# Merge with the full combination of years and categories to ensure every category/year combo is represented
complete_counts <- all_combinations %>%
  left_join(daily_counts, by = c("year", "AQI_category")) %>%
  replace_na(list(day_count = 0))  # Replace missing counts with 0

# Count the total number of days within the period for each year
total_days_per_year <- filtered_data %>%
  group_by(year) %>%
  summarize(total_days = n(), .groups = 'drop')

# Join the complete counts with total days and calculate percentages
final_counts <- complete_counts %>%
  left_join(total_days_per_year, by = "year") %>%
  mutate(percentage = (day_count / total_days) * 100)

# Define colors for AQI categories
aqi_colors <- c(
  "Hazardous" = "#7E0023",            # Dark Red
  "Very Unhealthy" = "#8F3F97",       # Purple
  "Unhealthy" = "#FF0000",            # Red
  "Unhealthy for Sensitive Groups" = "#FF7E00",  # Orange
  "Moderate" = "#FFFF00",             # Yellow
  "Good" = "#00FF00"                  # Green
)

final_counts <- final_counts %>%
  mutate(AQI_category = factor(AQI_category, levels = c("Hazardous", "Very Unhealthy", "Unhealthy", "Unhealthy for Sensitive Groups"))) %>%
  ungroup() %>%
  filter(AQI_category %in% c("Unhealthy for Sensitive Groups", "Unhealthy", "Very Unhealthy", "Hazardous"))

# Create stacked bar plot
p <- ggplot(final_counts, aes(x = factor(year), y = percentage, fill = AQI_category)) +
  geom_bar(stat = "identity") +
  labs(title = bquote(bold("Air Quality Index") ~ "(AQI)" ~ bold("During Wildfire Smoke Season")),
       subtitle = "May - October",
       x = "",
       y = "Percentage of Days",
       fill = "AQI Category") +
  scale_fill_manual(values = aqi_colors) +
  scale_y_continuous(
    expand = c(0, 0)
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16),  # Adjust x-axis text size
    axis.text.y = element_text(size = 16),  # Adjust y-axis text size
    axis.title.x = element_text(size = 14),  # Adjust x-axis title size
    axis.title.y = element_text(size = 18),  # Adjust y-axis title size
    plot.title = element_text(hjust = 0, vjust = 0, size = 20, face = "bold", family = "sans"),  
    plot.subtitle = element_text(hjust = 0, vjust = 1, size = 18),
    legend.position = c(0.25, 0.75),  # Move the legend to the top left (x, y format)
    legend.title = element_text(size = 16),  # Adjust legend title size
    legend.text = element_text(size = 14),  # Adjust legend text size
    panel.background = element_rect(fill = "transparent"),  # Set plot background to transparent
    plot.background = element_rect(fill = "transparent", color = NA),  # Set overall plot background to transparent with no border
    legend.background = element_rect(fill = "transparent", color = NA),  # Set legend background to transparent with no border
    panel.border = element_blank()  # Remove any panel border
  )

# Print the plot to check
print(p)

# Save the plot as a PNG with transparent background
ggsave("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/MARA Fires/Final Figures/AQI_days_percent.png", 
       plot = p, 
       bg = "transparent", 
       width = 8.5, 
       height = 6, 
       dpi = 300)



##### Trends #####

library(broom)

# Fit linear models and tidy results
trend_results <- final_counts %>%
  group_by(AQI_category) %>%
  do(tidy(lm(percentage ~ year, data = .))) %>%
  filter(term == "year")

# Extract slopes and p-values
trend_summary <- trend_results %>%
  select(AQI_category, estimate, p.value) %>%
  rename(slope = estimate, p_value = p.value)


