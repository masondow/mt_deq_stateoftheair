# Load necessary libraries
library(sf)
library(dplyr)
library(lubridate)
library(stringr)

##### LOAD DATA #####

# Read shapefile
US_shp_data <- st_read("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/MARA Fires/R/Data/mtbs_perimeter_data (1)/mtbs_perims_DD.shp")
CAN_shp_data <- st_read("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/MARA Fires/R/Data/nbac_1972_2023_20240530_shp/nbac_1972_2023_20240530.shp")

##### CREATE US DATA FRAMES ######

# Convert the sf object to a data frame while keeping relevant columns and converting lat/lon to numeric
US_shp_data_df <- US_shp_data %>%
  as.data.frame() %>%
  select(Event_ID, Incid_Type, BurnBndAc, Ig_Date, BurnBndLat, BurnBndLon) %>%
  mutate(
    BurnBndLat = as.numeric(BurnBndLat),
    BurnBndLon = as.numeric(BurnBndLon)
  )

# Create a regex pattern for state codes
state_pattern <- "^WA|^OR|^ID|^CA|^NV|^MT|^WY"

# Filter the data frame for specific states and Incid_Type "Wildfire"
filtered_data <- US_shp_data_df %>%
  filter(str_detect(Event_ID, state_pattern) & 
           Incid_Type == "Wildfire")

# Convert Ig_Date to Date type
filtered_data <- filtered_data %>%
  mutate(Ig_Date = as.Date(Ig_Date))

##### FIRE ACRES ######

# Calculate monthly and yearly sums of BurnBndAc
monthly_sums <- filtered_data %>%
  group_by(Year = year(Ig_Date), Month = month(Ig_Date)) %>%
  summarise(Monthly_BurnBndAc_Sum = sum(BurnBndAc, na.rm = TRUE))

yearly_sums <- filtered_data %>%
  group_by(Year = year(Ig_Date)) %>%
  summarise(Yearly_BurnBndAc_Sum = sum(BurnBndAc, na.rm = TRUE))

# Print the results
print(monthly_sums)
print(yearly_sums)

# Define periods
periods <- list(
  "1984-2009" = 1984:2009,
  "2010-2022" = 2010:2022
)

# Calculate monthly averages for each period
monthly_averages <- monthly_sums %>%
  # Create a period column based on the year
  mutate(period = case_when(
    Year %in% periods[["1984-2009"]] ~ "1984-2009",
    Year %in% periods[["2010-2022"]] ~ "2010-2022"
  )) %>%
  # Group by period and month to calculate monthly averages
  group_by(period, Month) %>%
  summarize(monthly_average = mean(Monthly_BurnBndAc_Sum, na.rm = TRUE), .groups = 'drop')

# Load necessary library
library(ggplot2)
library(scales)  # For formatting functions

# Create the bar plot with a trend line
ggplot(yearly_sums, aes(x = Year, y = Yearly_BurnBndAc_Sum)) +
  geom_bar(stat = "identity", fill = "#004A98") +  # Bar plot
  labs(title = "Annual Wildfire Acres Burned - WA, OR, CA, ID, NV, WY, MT",
       x = "Year",
       y = "Acres") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)  # Format y-axis labels as comma-separated


# Create bar plot
ggplot(monthly_averages, aes(x = factor(Month, levels = 1:12, labels = month.abb), y = monthly_average, fill = period)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Monthly Average Wildfire Acres Burned - WA, OR, CA, ID, NV, WY, MT",
       x = "Month",
       y = "Acres",
       fill = "Period") +
  theme_minimal() +
  scale_x_discrete(breaks = month.abb) +  # Labels for months
  scale_fill_manual(values = c("1984-2009" = "gray80", "2010-2022" = "#F54D28")) +
  scale_y_continuous(labels = scales::comma)  # Format y-axis labels as comma-separated

##### NUMBER OF FIRES ######

# Extract year and month
filtered_data <- filtered_data %>%
  mutate(Year = year(Ig_Date),
         Month = month(Ig_Date, label = TRUE))  # Label = TRUE gives abbreviated month names

# Calculate the number of wildfires per month
monthly_wildfires <- filtered_data %>%
  group_by(Year, Month) %>%
  summarise(Num_Wildfires = n()) %>%
  ungroup()

# Divide the data into two periods: 1984-2009 and 2010-2022
monthly_wildfires <- monthly_wildfires %>%
  mutate(Period = case_when(
    Year >= 1984 & Year <= 2009 ~ "1984-2009",
    Year >= 2010 & Year <= 2022 ~ "2010-2022"
  ))

# Calculate the monthly average number of wildfires for each period
monthly_averages <- monthly_wildfires %>%
  group_by(Period, Month) %>%
  summarise(Average_Wildfires = mean(Num_Wildfires, na.rm = TRUE)) %>%
  ungroup()

# View the result
print(monthly_averages)

# Plot the monthly averages on a bar plot
ggplot(monthly_averages, aes(x = Month, y = Average_Wildfires, fill = Period)) +
  geom_bar(stat = "identity", position = "dodge") +  # Bar plot with side-by-side bars
  labs(title = "Monthly Average Number of Wildfires: 1984-2009 vs. 2010-2022",
       x = "Month",
       y = "Average Number of Wildfires") +
  theme_minimal() +
  theme(legend.position = "bottom")
