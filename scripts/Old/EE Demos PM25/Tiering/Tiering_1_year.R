#later in this script, only T2+ R-flag days are filtered to be highlighted because
#of two days that were I-flagged but not included in demo. In future when determining
#final EE days to flip to R-flags from I-flags, remember to filter for I and R flags
library(dplyr)
library(lubridate)
library(stringr)
library(readxl)
library(magrittr)

#sites: "French Town - Beckwith"  "Libby Courthouse Annex"
site_filter <- "Libby Courthouse Annex"
file_sitename <- "Libby"

##### AQS Data #####
daily_pm25 <- readRDS("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/DATA/2003_2024_daily_pm25_88101_88502_all_MT.rds")
hourly_pm25 <- readRDS("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/DATA/2003_2024_hourly_pm25_88101_88502_all_MT.rds")

##### Add Flags #####
daily_allyr <- daily_pm25 %>%
  filter(sample_duration == "1 HOUR" & 
           validity_indicator == "Y") %>%
  mutate(
    date_local = as.Date(date_local, format = "%Y-%m-%d"),
    year = year(date_local),       # Extract year from date_local
    month = month(date_local)     # Extract month from date_local
  ) %>%
  distinct(date_local, local_site_name, poc, .keep_all = TRUE)

qualifiers <- c(
  "RT - Wildfire-U. S.",
  "RF - Fire - Canadian.",
  "IF - Fire - Canadian.",
  "IT - Wildfire-U. S."
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

# Merge flags with daily_pm25 by multiple columns
daily_allyr_flags <- daily_allyr %>% 
  left_join(flags, by = c("date_local", "state_code", "county_code", "site_number", "poc"))

# Filter for sites

EE_samples <- daily_allyr_flags %>%
  filter(local_site_name == site_filter &
           year <= 2024 &
           year >= 2022)

##### Original EPA Tiering Values (using same tiers for 2021-2023) #####

# Define the path to the Excel file
epa_tiers_path <- "/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/EE Demos/Tiering/Tier Data/tiering_2019_2023.xlsx"

# Read the Excel file
tiering_data_21_23 <- read_excel(epa_tiers_path)
tiering_data_21_23 <- tiering_data_21_23 %>%
  filter(STATE_NAME == "Montana") %>%
  mutate(
    state_code = substr(SITE_ID, 1, 2),   # First 2 digits
    county_code = substr(SITE_ID, 3, 5),  # Next 3 digits
    site_number = substr(SITE_ID, 6, 9)   # Last 4 digits
  ) %>%
  rename(month_98th = Monthly_98th,
         min_ann_98th = annual_98th_minimum) %>%
  mutate(tier_year = 2023) %>%
  select(state_code,
         county_code,
         site_number,
         month,
         month_98th,
         min_ann_98th,
         tier_year)

tiering_data_21_23 <- tiering_data_21_23 %>%
  left_join(
    daily_pm25 %>%
      select(county_code, site_number, local_site_name) %>%
      distinct(),  # Ensure unique site names per county/site
    by = c("county_code", "site_number")
  ) %>%
  filter(local_site_name == site_filter)

##### Updated Tiering Values (for 2024 and beyond)

tiering_data_24 <- readRDS("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/EE Demos/Tiering/Tier Data/tiers_2020_2024.rds")
tiering_data_24 <- tiering_data_24 %>%
  ungroup() %>%
  filter(local_site_name == site_filter) %>%
  mutate(tier_year = 2024) %>%
    select(local_site_name,
           state_code,
           county_code,
           site_number,
           month,
           month_98th,
           min_ann_98th,
           tier_year)

tiering_data <- bind_rows(tiering_data_24, tiering_data_21_23)

###### Add Tiers #####

# add tier_year column such that data year 2021-2023 = tier_year 2023; for subsequent years, tier_year = year
EE_samples_tier <- EE_samples %>%
  mutate(tier_year = ifelse(year <= 2023, 2023, year)) %>%
  left_join(tiering_data)
  
##### Add Tiers #####

# Label which threshold is the lower of the two
EE_samples_tier <- EE_samples_tier %>%
  mutate(threshold = ifelse(month_98th < min_ann_98th, "Month", "Annual"))

# Add 'Tier 2' column with the lesser of Monthly_98th and min_annual_98th
EE_samples_tier <- EE_samples_tier %>%
  mutate(Tier_2 = pmin(month_98th, min_ann_98th))

# Add 'Tier 1' column with 1.5 times the value of Tier 2
EE_samples_tier <- EE_samples_tier %>%
  mutate(Tier_1 = 1.5 * Tier_2)

# Add 'Tier_Label' column based on the given conditions
EE_samples_tier <- EE_samples_tier %>%
  mutate(Tier_Label = case_when(
    # Check if any of the specified qualifiers are present in the 'qualifier' column
    str_detect(qualifier, "RT - Wildfire-U. S.|IT - Wildfire-U. S.|RF - Fire - Canadian.|IF - Fire - Canadian.") ~
      case_when(
        arithmetic_mean > Tier_1 ~ "Tier 1",
        arithmetic_mean > Tier_2 ~ "Tier 2",
        TRUE ~ "Tier 3"
      ),
    TRUE ~ NA_character_  # If the qualifier does not match the specified values, set NA
  ))

##### Identify EE Days

# Step 3: Filter the data to include only rows from the most recent 3 years with a non-missing Tier_Label
EE_days_all_tiers <- EE_samples_tier %>%
  filter(!is.na(Tier_Label))

# Filter just T2+"
EE_days_t2_plus <- EE_samples_tier %>%
  filter(Tier_Label %in% c("Tier 1", "Tier 2")) #%>%
  #filter(str_detect(qualifier, "RT - Wildfire-U. S.") | str_detect(qualifier, "RF - Fire - Canadian."))

##### PLOTTING Options #####

library(ggplot2)

plotting_data <- daily_allyr_flags %>%
  filter(year >= 2020 &
           local_site_name == site_filter) %>%
  left_join(EE_samples_tier) %>%
  mutate(day_of_year = yday(date_local))

# Step 2: Create a color group based on your criteria
plotting_data <- plotting_data %>%
  mutate(color_group = case_when(
    #year(date_local) %in% 2021:2023 & Tier_Label == "Tier 1" ~ "2021-2023, Tier 1 Submitted EE Days",
    #year(date_local) %in% 2021:2023 & Tier_Label == "Tier 2" ~ "2021-2023, Tier 2 Submitted EE Days",
    year(date_local) %in% 2024 & Tier_Label == "Tier 1" ~ "2024, Tier 1 Submitted EE Days",
    year(date_local) %in% 2024 & Tier_Label == "Tier 2" ~ "2024, Tier 2 Submitted EE Days",
    year(date_local) %in% 2020:2023 & grepl("RT - Wildfire-U. S.|IT - Wildfire-U. S.|RF - Fire - Canadian.|IF - Fire - Canadian.", qualifier) ~ "2020-2023, Wildfire Smoke Days (RT, RF, IT, IF Flags)",
    TRUE ~ "2020-2023, non-Wildfire Smoke Days"
  ))

# Step 3: Explicitly reorder the rows so "2017-2023" is plotted first
plotting_data <- plotting_data %>%
  arrange(desc(color_group == "2020-2023, non-Wildfire Smoke Days"))

# Step 4: Create a scatter plot with custom colors, ordered legend, and a white background box for the legend

# Define month boundaries for vertical lines
month_boundaries <- cumsum(c(0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30))

##### PLOTTING  #####

p1 <- ggplot() +
  geom_vline(xintercept = month_boundaries, color = "gray90", alpha = .5, linetype = "solid") +
  geom_point(data = plotting_data, aes(x = day_of_year, y = arithmetic_mean, color = color_group)) +
  scale_x_continuous(breaks = month_boundaries,
                     labels = month.abb,
                     expand = c(0,0),
                     limits = c(0, 366)) +  # Handle leap years by allowing 366 days
  scale_color_manual(
    name = bquote(bold(.(file_sitename) ~ " 2020-2024")),
    values = c(
    "2024, Tier 1 Submitted EE Days" = "#004a98",  # Red for 2021-2023 Tier 1
    "2024, Tier 2 Submitted EE Days" = "#009ADE",  # Orange for 2021-2023 Tier 2
    "2020-2023, Wildfire Smoke Days (RT, RF, IT, IF Flags)" = "gray70",  # Yellow for Wildfire flagged days
    "2020-2023, non-Wildfire Smoke Days" = "gray85"  # Gray for other data
  ),
  limits = c(  # Ensure the order in the legend matches this
    "2024, Tier 1 Submitted EE Days",
    "2024, Tier 2 Submitted EE Days",
    "2020-2023, Wildfire Smoke Days (RT, RF, IT, IF Flags)",
    "2020-2023, non-Wildfire Smoke Days"
  )) +
  labs(x = "Month", y = expression("24-hr Average PM"[2.5]*" (µg/m"^3*")")) +
  theme_minimal() +
  theme(
    legend.position = c(0.05, 0.95),  # Position the legend in the top left
    legend.justification = c(0, 1),   # Align the legend to the top left corner
    legend.background = element_rect(fill = "white", color = "black"),  # White background box with black border
    panel.grid.major.x = element_blank(),  # Remove vertical grid lines
    panel.grid.minor.x = element_blank(),  # Remove minor vertical grid lines
    legend.title = element_text(face = "bold")  # Make legend title bold
  )

print(p1)
ggsave(paste0("/Users/cameronnealy/Downloads/", file_sitename, "_EEdays.png"), plot = p1, width = 8, height = 5, dpi = 300)

##### Historical Averages #####

# May to Oct 95th percentile
may_to_oct <- plotting_data %>%
  filter(month(date_local) >= 5 & month(date_local) <= 9)

may_oct_percentile_95 <- quantile(may_to_oct$arithmetic_mean, probs = 0.95, na.rm = TRUE)

# May to Oct 95th percentile - no flags
dates_to_exclude <- plotting_data %>%
  filter(qualifier %in% qualifiers) %>%
  pull(date_local) %>%
  unique()

may_oct_no_flags <- plotting_data %>%
  filter(!date_local %in% dates_to_exclude) %>%
  filter(month(date_local) >= 5 & month(date_local) <= 9)

may_oct_percentile_95_no_flags <- quantile(may_oct_no_flags$arithmetic_mean, probs = 0.95, na.rm = TRUE)

# Daily Average
pm25_daily_avg <- plotting_data %>%
  group_by(day_of_year) %>%
  summarize(daily_avg = mean(arithmetic_mean, na.rm = TRUE)) %>%
  ungroup()

# Daily Average with flags
pm25_daily_avg_no_flags <- plotting_data %>%
  filter(!date_local %in% dates_to_exclude)

# Calculate the daily average by grouping day_of_year
pm25_daily_avg_no_flags <- pm25_daily_avg_no_flags %>%
  group_by(day_of_year) %>%
  summarize(daily_avg = mean(arithmetic_mean, na.rm = TRUE)) %>%
  ungroup()

##### Wildfire Flagged Days #####

# Step 2: Filter pm25_samples_ee for these dates
pm25_flagged_days <- plotting_data %>%
  filter(date_local %in% dates_to_exclude)

##### PLOTTING - Historical Averages #####

# Create a data frame for horizontal lines with corresponding labels
naaqs_lines <- data.frame(
  x = rep(c(0, 366), 2),  # X values: start at 0 and end at 366 for both lines
  y = c(rep(9, 2), rep(35, 2)),  # Y values: 9 for the first line, 35 for the second
  label = rep(c("2024 PM2.5 Annual NAAQS (9 µg/m³)", "2006 PM2.5 24-hr NAAQS (35 µg/m³)"), each = 2)  # Labels for each line
)

# Define month boundaries for vertical lines
month_boundaries <- cumsum(c(0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30))

p2 <- ggplot() +
  # Add vertical lines at month boundaries
  geom_vline(xintercept = month_boundaries, color = "gray90", alpha = .5, linetype = "solid") +
  # Plot points for 2017-2023 24-hr PM2.5 Avg with legend
  geom_point(data = plotting_data, aes(x = day_of_year, y = arithmetic_mean, color = "24-hr PM2.5 Avg (no wildfire flags)"), size = 2) +
  # Plot points for flagged days with legend
  geom_point(data = pm25_flagged_days, aes(x = day_of_year, y = arithmetic_mean, color = "24-hr PM2.5 Avg - Wildfire Smoke Days (RT, RF, IT, IF, flags)"), size = 2) +
  # Plot daily averages as lines with legend
  geom_line(data = pm25_daily_avg, aes(x = day_of_year, y = daily_avg, color = "Daily Average (with RT, RF, IT, IF, flags)"), size = 1) +
  # Plot daily averages with no flags as lines with legend
  geom_line(data = pm25_daily_avg_no_flags, aes(x = day_of_year, y = daily_avg, color = "Daily Average (no wildfire flags)"), size = 1) +
  # Add dashed lines for percentiles using geom_line
  geom_line(data = naaqs_lines, aes(x = x, y = y, color = label), linetype = "dashed") +
  # Define month breaks and labels
  scale_x_continuous(
    breaks = month_boundaries,  # Breaks at month boundaries
    labels = month.abb,  # Month abbreviations
    limits = c(0, 366),
    expand = c(0, 0)  # Remove padding
  ) +
  scale_y_continuous(
    limits = c(0, 75),
    breaks = seq(0, 75, by = 10)  # Set breaks at intervals of 10
  ) +
  # Add labels and theme
  labs(
    x = "Month", 
    y = expression(PM[2.5]~(ug/m^3)),
    color = bquote(bold(.(file_sitename) ~ " 2020-2024"))
  ) +
  scale_color_manual(values = c(
    "24-hr PM2.5 Avg (no wildfire flags)" = "gray85",
    "24-hr PM2.5 Avg - Wildfire Smoke Days (RT, RF, IT, IF, flags)" = "gray70",
    "Daily Average (with RT, RF, IT, IF, flags)" = "#F54D28",
    "Daily Average (no wildfire flags)" = "gray40",
    "2006 PM2.5 24-hr NAAQS (35 µg/m³)" = "#009ADE",
    "2024 PM2.5 Annual NAAQS (9 µg/m³)" = "black"
  )) +
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 0, hjust = .5),  # Rotate x-axis labels for better readability
    legend.position = c(0.27, 0.75),  # Position the legend in the top-left corner
    legend.background = element_rect(fill = "white", color = NA),  # White background for legend box
    legend.box.background = element_rect(color = "black"),  # Black border around the legend box (optional)
    panel.grid.major.x = element_blank(),  # Remove vertical grid lines
    panel.grid.minor.x = element_blank(),  # Remove minor vertical grid lines
    legend.title = element_text(face = "bold")  # Make legend title bold
  )

print(p2)
ggsave(paste0("/Users/cameronnealy/Downloads/", file_sitename, "_EE_historical.png"), plot = p2, width = 8, height = 5, dpi = 300)


##### Seasonal Hourly #####

hourly <- hourly_pm25 %>%
  left_join(daily_allyr %>%
              select(local_site_name,
                     county_code,
                     site_number) %>%
              distinct(),
            by = c("county_code", "site_number")) %>%
  mutate(
    date_local = as.Date(date_local, format = "%Y-%m-%d"),
    year = year(date_local),       # Extract year from date_local
    month = month(date_local)     # Extract month from date_local
  ) %>%
  filter(local_site_name == site_filter &
           year >= 2020)

# Perform the left join to add Tier_Label to hourly_pm25_samples
hourly_pm25_samples <- hourly %>%
  left_join(EE_days_t2_plus %>%
              filter(local_site_name == site_filter) %>%
              select(date_local, Tier_Label), 
              by = "date_local")

##### PLOTTING - Hourly Fire Seasons #####

# Combine date_local and time_local into a single datetime column and sort by datetime
fire_season_a <- hourly_pm25_samples %>%
  mutate(
    # Combine date and time into a single POSIXct datetime column
    datetime = as.POSIXct(paste(date_local, time_local), format = "%Y-%m-%d %H:%M")
  ) %>%
  filter(datetime >= as.POSIXct("2024-05-01 00:00") & datetime <= as.POSIXct("2024-11-01 23:59")) %>%
  filter(year(datetime) == 2024) %>%
  arrange(datetime) %>%
  # Calculate time differences between consecutive rows
  mutate(time_diff = c(NA, diff(datetime))) %>%
  # Create a group identifier where time_diff exceeds 1 hour
  mutate(group = cumsum(time_diff > 3600 | is.na(time_diff))) %>%
  # Add a column for NAAQS value
  mutate(NAAQS_9 = 9,
         NAAQS_35 = 35) %>%
  # Remove temporary columns
  select(-time_diff)

# Plot the data
p3 <- ggplot(fire_season_a, aes(x = datetime, y = sample_measurement)) +
  geom_line(aes(color = ifelse(!is.na(Tier_Label), "Submitted EE Day Hourly Values", "Non-EE Day Hourly Values"), group = group), size = 0.5) +
  geom_line(aes(y = NAAQS_9, color = "2024 PM2.5 Annual NAAQS (9 µg/m³)"), size = 0.8, linetype = "dashed") +  # Add horizontal line
  geom_line(aes(y = NAAQS_35, color = "2006 PM2.5 24-hr NAAQS (35 µg/m³)"), size = 0.8, linetype = "dashed") +
  scale_color_manual(values = c("Submitted EE Day Hourly Values" = "#F54D28", 
                                "Non-EE Day Hourly Values" = "gray70",
                                "2024 PM2.5 Annual NAAQS (9 µg/m³)" = "gray40",
                                "2006 PM2.5 24-hr NAAQS (35 µg/m³)"= "#009ADE")) +  # Update color scale to include horizontal line
  labs(
    x = "Month",
    y = bquote("1-hr PM"[2.5]~"Measurement (µg/m"^3*")"),  # Add superscript for PM2.5
    color = bquote(bold(.(file_sitename) ~ "- 2024 Wildfire Smoke Season"))  # Color label
  ) +
  theme_minimal() +
  scale_x_datetime(
    date_breaks = "1 month",   # Adjust the frequency of x-axis labels to monthly
    date_labels = "%b",      # Format x-axis labels to show month
    expand = c(0, 0)         # Remove padding around x-axis
  ) +
  scale_y_continuous(      # Extend y-axis up to 100
    expand = c(0, 0)         # Remove padding around y-axis
  ) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = .5),  # Rotate x-axis labels for readability
    legend.position = c(0.02, 1),                          # Position the legend at the top-left
    legend.justification = c(0, 1),                     # Adjust the legend's justification
    legend.background = element_rect(fill = "white"),    # Add a white box behind the legend
    legend.title = element_text(face = "bold")           # Make legend title bold
  )

print(p3)
ggsave(paste0("/Users/cameronnealy/Downloads/", file_sitename, "_EE_hourly.png"), plot = p3, width = 8, height = 5, dpi = 300)




