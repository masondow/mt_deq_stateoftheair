# Load the necessary library
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)

# Load the Excel file
file_path <- "/Users/cameronnealy/Downloads/US fires by month and year.xlsx"
data <- read_excel(file_path)

# Separate the 'Date' column into 'Year' and 'Month' columns
data$Year <- substr(data$Date, 1, 4)
data$Month <- substr(data$Date, 5, 6)

# Convert Year and Month to numeric
data$Year <- as.numeric(data$Year)
data$Month <- as.numeric(data$Month)

# Group the data by Year and calculate the difference between months
data <- data %>%
  group_by(Year) %>%
  mutate(
    Acres_Burned = Acres_Burned - lag(Acres_Burned, default = 0),
    Number_of_Fires = Number_of_Fires - lag(Number_of_Fires, default = 0)
  ) %>%
  ungroup()

# Calculate Acres_Burned_per_Fire for each row
data <- data %>%
  mutate(Acres_Burned_per_Fire = Acres_Burned / Number_of_Fires)

# Reshape the data for each variable
acres_burned_df <- data %>%
  select(Year, Month, Acres_Burned) %>%
  pivot_wider(names_from = Month, values_from = Acres_Burned)

number_of_fires_df <- data %>%
  select(Year, Month, Number_of_Fires) %>%
  pivot_wider(names_from = Month, values_from = Number_of_Fires)

acres_burned_per_fire_df <- data %>%
  select(Year, Month, Acres_Burned_per_Fire) %>%
  pivot_wider(names_from = Month, values_from = Acres_Burned_per_Fire)

# Save each data frame to a separate Excel file or sheet
write_xlsx(list(
  "Acres_Burned" = acres_burned_df,
  "Number_of_Fires" = number_of_fires_df,
  "Acres_Burned_per_Fire" = acres_burned_per_fire_df
), "/Users/cameronnealy/Downloads/US fires by month and year_R_mods.xlsx")


