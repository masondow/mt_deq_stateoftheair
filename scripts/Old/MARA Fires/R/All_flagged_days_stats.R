library(dplyr)
library(lubridate)
library(stringr)
library(readxl)
library(magrittr)
library(ggplot2)

##### Download AQS Data #####

daily_pm25 <- readRDS("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/DATA/2003_2023_daily_pm25_88101_88502_all_MT.rds")
hourly_pm25 <- readRDS("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/DATA/2003_2023_hourly_pm25_88101_88502_all_MT.rds")

##### Add Flags #####

# Define qualifiers (for exclusion later)
qualifiers <- c(
  "RT - Wildfire-U. S.",
  "IT - Wildfire-U. S.",
  "RF - Fire - Canadian.",
  "IF - Fire - Canadian.",
  "E - Forest Fire."
)

# Convert date_local to Date and summarize only the specified qualifiers
flags <- hourly_pm25 %>%
  mutate(date_local = as.Date(date_local, format = "%Y-%m-%d")) %>%
  group_by(date_local, state_code, county_code, site_number) %>%
  summarize(qualifier = paste(
    unique(na.omit(qualifier[qualifier %in% qualifiers])), 
    collapse = ", "
  )) %>%
  ungroup()

# Filter, convert the date_local column, and ensure uniqueness
daily_pm25 <- daily_pm25 %>%
  filter(sample_duration == "1 HOUR" & validity_indicator == "Y") %>%
  mutate(date_local = as.Date(date_local, format = "%Y-%m-%d")) %>%
  distinct(date_local, local_site_name, .keep_all = TRUE)

# Merge flags with daily_pm25 by multiple columns
daily_pm25 <- daily_pm25 %>% 
  left_join(flags, by = c("date_local", "state_code", "county_code", "site_number"))

##### Flagged Days Scatter #####

daily_pm25_flagged <- daily_pm25 %>%
  select(date_local, arithmetic_mean, local_site_name, qualifier) %>%
  filter(qualifier != "") %>%
  mutate(year = format(date_local, "%Y"))

# Create the scatter plot
ggplot(daily_pm25_flagged, aes(x = date_local, y = arithmetic_mean)) +
  geom_point(color = "blue", size = 2) +  # Add points in blue
  labs(title = "Daily PM2.5 Values",
       x = "Date",
       y = "Arithmetic Mean (PM2.5)") +
  theme_minimal()  # Use a minimal theme for clean look

##### Flagged Days Annual Average Value #####

daily_pm25_flagged_average <- daily_pm25_flagged %>%
  group_by(year) %>%
  summarize(average = mean(arithmetic_mean, na.rm = TRUE))

# Ensure the 'year' variable is numeric
daily_pm25_flagged_average$year <- as.numeric(as.character(daily_pm25_flagged_average$year))

# Create the scatter plot with a trend line, connecting line, and filled area
ggplot(daily_pm25_flagged_average, aes(x = year, y = average)) +
  geom_line(color = "#004A98", size = 1) +  # Line connecting points
  geom_area(aes(y = average), fill = "gray", alpha = 0.4) +  # Fill area under the line
  geom_smooth(method = "lm", color = "gray", se = FALSE) +  # Add a linear trend line, without confidence intervals
  labs(title = "Wildfire Smoke Days Average PM2.5 Concentration",
       x = "Year",
       y = "PM2.5 (ug/m3)") +
  theme_minimal() +  # Use a minimal theme for a clean look
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

##### AQI Category Percentage - Flagged Days #####

daily_pm25_flagged$arithmetic_mean <- floor(daily_pm25_flagged$arithmetic_mean * 10) / 10 #only first decimal place

# Define breakpoints for PM2.5 AQI categories
breakpoints <- tibble(
  category = c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy", "Very Unhealthy", "Hazardous"),
  min = c(-1000, 9.1, 35.5, 55.5, 125.5, 225.5),
  max = c(9.0, 35.4, 55.4, 125.4, 225.4, Inf)
)

# Add a new column to categorize each row based on the value in 'arithmetic_mean'
daily_pm25_flagged <- daily_pm25_flagged %>%
  rowwise() %>%
  mutate(AQI_category = breakpoints$category[which(arithmetic_mean >= breakpoints$min & arithmetic_mean <= breakpoints$max)]) %>%
  ungroup()

# Calculate the percentage of each AQI category within each year
aqi_percentage_per_year <- daily_pm25_flagged %>%
  group_by(year, AQI_category) %>%                          # Group by year and AQI category
  summarize(count = n(), .groups = "drop") %>%              # Count occurrences of each AQI category
  mutate(total = sum(count), .by = year) %>%                # Calculate total number of rows per year
  mutate(percentage = (count / total) * 100)                # Calculate percentage for each AQI category

# Define AQI colors
aqi_colors <- c(
  "Good" = "#00E400",                          # Green
  "Moderate" = "#FFFF00",                       # Yellow
  "Unhealthy for Sensitive Groups" = "#FF7E00", # Orange
  "Unhealthy" = "#FF0000",                      # Red
  "Very Unhealthy" = "#8F3F97",                 # Purple
  "Hazardous" = "#7E0023"                       # Dark Red
)

# Ensure AQI_category is a factor with levels in the reverse order of aqi_colors
aqi_percentage_per_year <- aqi_percentage_per_year %>%
  mutate(AQI_category = factor(AQI_category, levels = names(aqi_colors)))

# Create stacked bar plot
ggplot(aqi_percentage_per_year, aes(x = year, y = percentage, fill = AQI_category)) +
  geom_bar(stat = "identity") +
  labs(title = bquote(bold("Air Quality Index") ~ "(AQI)" ~ bold("on Wildfire Smoke Days")),
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

##### Flagged Days per Year #####

# Unique dates for flagged days per year
daily_pm25_distinct <- daily_pm25_flagged %>%  
  distinct(date_local, .keep_all = TRUE)

# Count the number of unique days per year
days_per_year <- daily_pm25_distinct %>%
  group_by(year) %>%
  summarise(unique_days = n_distinct(date_local))

# Create the bar plot
ggplot(days_per_year, aes(x = year, y = unique_days)) +
  geom_bar(stat = "identity", fill = "steelblue") +  # Create bars with a fill color
  labs(title = "Number of Unique Days per Year",
       x = "Year",
       y = "Number of Unique Days") +
  theme_minimal() +  # Use a minimal theme for a clean look
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
