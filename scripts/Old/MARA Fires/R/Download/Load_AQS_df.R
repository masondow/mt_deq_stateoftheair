library(dplyr)

# Define the directory containing the .rds files
dir_path <- "/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/Shiny/AQS/AQS_data/PM25/"

# List all .rds files in the directory
rds_files <- list.files(path = dir_path, pattern = "\\.rds$", full.names = TRUE)

# Initialize an empty list to store each data frame temporarily
data_frames <- list()

# Iterate over each file, load the data, add the 'Site' column, and store in the list
for (file_path in rds_files) {
  # Load the data frame from the current file
  data <- readRDS(file_path)
  
  # Extract the site name from the file name
  site_name <- sub(".*PM25_(.*?)_AQS_data.rds", "\\1", basename(file_path))
  
  # Add the 'Site' column to the data frame
  data$Site <- site_name
  
  # Add the data frame to the list
  data_frames[[site_name]] <- data
  
  # Assign the data frame to a variable named after the site
  assign(site_name, data)
}

# Combine all the data frames into one
combined_data <- bind_rows(data_frames)

# Filter for valid measurements (completeness >= 75%)
valid_data <- combined_data %>%
  filter(completeness >= 75)

# Calculate the number of valid days per year and site
valid_days_per_year <- valid_data %>%
  group_by(YearGroup, Site) %>%
  summarize(valid_days = n_distinct(date_local))

# Define total days per year as 365
total_days_per_year <- combined_data %>%
  group_by(YearGroup, Site) %>%
  summarize(total_days = 365)

# Merge valid days and total days
days_summary <- left_join(valid_days_per_year, total_days_per_year, by = c("YearGroup", "Site"))

# Calculate the percentage of valid days and filter
days_summary <- days_summary %>%
  mutate(valid_percentage = valid_days / total_days * 100) %>%
  filter(valid_percentage >= 75)

# Filter the original data for the valid years and sites
filtered_data <- combined_data %>%
  inner_join(days_summary, by = c("YearGroup", "Site"))

# Group by 'YearGroup' and 'Site' and calculate the annual average of 'average_measurement'
annual_averages <- filtered_data %>%
  group_by(YearGroup, Site) %>%
  summarize(annual_avg = mean(average_measurement, na.rm = TRUE))

# View the resulting annual averages
print(annual_averages)

# Calculate whole state averages by grouping by year
whole_state_averages <- annual_averages %>%
  group_by(YearGroup) %>%
  summarize(state_avg = mean(annual_avg, na.rm = TRUE))

# View the resulting whole state averages
print(whole_state_averages)

# Define breakpoints for PM2.5 AQI categories
breakpoints <- tibble(
  category = c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy", "Very Unhealthy", "Hazardous"),
  min = c(-1000, 9.1, 35.5, 55.5, 125.5, 225.5),
  max = c(9.0999, 35.4999, 55.4999, 125.4999, 225.4999, Inf)
)

# Assuming 'valid_data' is already defined
# Add AQI Category based on PM2.5 concentrations
valid_data_with_category <- valid_data %>%
  rowwise() %>%
  mutate(
    AQI_Category = case_when(
      average_measurement >= breakpoints$min[1] & average_measurement <= breakpoints$max[1] ~ breakpoints$category[1],
      average_measurement >= breakpoints$min[2] & average_measurement <= breakpoints$max[2] ~ breakpoints$category[2],
      average_measurement >= breakpoints$min[3] & average_measurement <= breakpoints$max[3] ~ breakpoints$category[3],
      average_measurement >= breakpoints$min[4] & average_measurement <= breakpoints$max[4] ~ breakpoints$category[4],
      average_measurement >= breakpoints$min[5] & average_measurement <= breakpoints$max[5] ~ breakpoints$category[5],
      TRUE ~ breakpoints$category[6]  # For values above the last breakpoint
    )
  )

# View the updated data with AQI categories
print(valid_data_with_category)

# Calculate the number of valid days per site per year
valid_days_per_site_year <- valid_data_with_category %>%
  group_by(YearGroup, Site) %>%
  summarize(valid_days = n(), .groups = 'drop')

# Filter to keep only sites with at least 75% of 365 measurements
filtered_sites <- valid_days_per_site_year %>%
  filter(valid_days >= 0.75 * 365)

# Filter the original data to include only the filtered sites
filtered_valid_data <- valid_data_with_category %>%
  semi_join(filtered_sites, by = c("YearGroup", "Site"))

# Count the number of unique sites for each year after filtering
filtered_sites_per_year <- filtered_valid_data %>%
  group_by(YearGroup) %>%
  summarize(num_sites = n_distinct(Site), .groups = 'drop')

# Count the number of days in each AQI category for each year (including all repetitions)
filtered_days_by_aqi_category <- filtered_valid_data %>%
  group_by(YearGroup, AQI_Category) %>%
  summarize(num_days = n(), .groups = 'drop')

# Join the counts of days with the number of sites to calculate the relative number of days per site
relative_days_per_site <- filtered_days_by_aqi_category %>%
  left_join(filtered_sites_per_year, by = "YearGroup") %>%
  mutate(relative_days_per_site = num_days / num_sites)

# View the resulting data frame
print(relative_days_per_site)


######## Plotting #########
library(ggplot2)

# Create a bar plot of whole state averages
ggplot(whole_state_averages, aes(x = YearGroup, y = state_avg)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Year", y = "Average PM2.5 Measurement", title = "Annual Average PM2.5 Measurements by Year") +
  theme_minimal()



# Filter out the "Good" and "Moderate" categories
filtered_relative_days_per_site <- relative_days_per_site %>%
  filter(!AQI_Category %in% c("Good", "Moderate"))

# Reverse the order of the AQI categories
filtered_relative_days_per_site$AQI_Category <- factor(
  filtered_relative_days_per_site$AQI_Category,
  levels = c("Hazardous", "Very Unhealthy", "Unhealthy", "Unhealthy for Sensitive Groups")
)

# Define the colors for each AQI category
aqi_colors <- c(
  "Hazardous" = "#7E0023",       # Dark Red
  "Very Unhealthy" = "#8F3F97",  # Purple
  "Unhealthy" = "#FF0000",       # Red
  "Unhealthy for Sensitive Groups" = "#FF7E00"  # Orange
)

# Create the stacked bar plot with the reversed order and custom colors
ggplot(filtered_relative_days_per_site, aes(x = YearGroup, y = relative_days_per_site, fill = AQI_Category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = aqi_colors) +
  labs(
    title = "Stacked Bar Plot of AQI Categories (Excluding Good and Moderate)",
    x = "Year Group",
    y = "Relative Days per Site",
    fill = "AQI Category"
  ) +
  theme_minimal()

###### Specific Site Plotting #######

# Filter the data for the specific site of "Malta"
site_data <- filtered_valid_data %>%
  filter(Site == "Sidney 201")

# Count the number of days in each AQI category for Malta
days_by_aqi_category_site <- site_data %>%
  group_by(YearGroup, AQI_Category) %>%
  summarize(num_days = n(), .groups = 'drop')

# Filter out the "Good" and "Moderate" categories
filtered_days_by_aqi_category_site <- days_by_aqi_category_site %>%
  filter(!AQI_Category %in% c("Good", "Moderate"))

# Reverse the order of the AQI categories
filtered_days_by_aqi_category_site$AQI_Category <- factor(
  filtered_days_by_aqi_category_site$AQI_Category,
  levels = c("Hazardous", "Very Unhealthy", "Unhealthy", "Unhealthy for Sensitive Groups")
)

# Define the colors for each AQI category
aqi_colors <- c(
  "Hazardous" = "#7E0023",       # Dark Red
  "Very Unhealthy" = "#8F3F97",  # Purple
  "Unhealthy" = "#FF0000",       # Red
  "Unhealthy for Sensitive Groups" = "#FF7E00"  # Orange
)

# Create the stacked bar plot with the reversed order and custom colors
ggplot(filtered_days_by_aqi_category_site, aes(x = YearGroup, y = num_days, fill = AQI_Category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = aqi_colors) +
  labs(
    title = "Stacked Bar Plot of AQI Categories for Malta (Excluding Good and Moderate)",
    x = "Year Group",
    y = "Number of Days",
    fill = "AQI Category"
  ) +
  theme_minimal()
