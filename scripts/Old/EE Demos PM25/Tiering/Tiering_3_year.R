#later in this script, only T2+ R-flag days are filtered to be highlighted because
#of two days that were I-flagged but not included in demo. In future when determining
#final EE days to flip to R-flags from I-flags, remember to filter for I and R flags
library(dplyr)
library(lubridate)
library(stringr)
library(readxl)
library(RAQSAPI)
library(magrittr)

##### Download AQS Data #####

# Set AQS credentials
aqs_credentials(username = "Cameron.Nealy@mt.gov", key = "copperhawk98")

# Pick a site
stateFIPS <- "30"
countycode <- "053"
sitenum <- "0018"

# Download daily data; check date range; need 5 years
daily <- aqs_dailysummary_by_site(
  parameter = "88101",
  bdate = as.Date("20170101", format = "%Y%m%d"),
  edate = as.Date("20231231", format = "%Y%m%d"),
  stateFIPS = stateFIPS,
  countycode = countycode,
  sitenum = sitenum
)

# Download hourly data with flags; check date range; need 5 years
hourly <- aqs_sampledata_by_site(
  parameter = "88101",
  bdate = as.Date("20170101", format = "%Y%m%d"),
  edate = as.Date("20231231", format = "%Y%m%d"),
  stateFIPS = stateFIPS,
  countycode = countycode,
  sitenum = sitenum
)

##### Add Flags #####

# Define qualifiers (for exclusion later)
qualifiers <- c(
  "RT - Wildfire-U. S.",
  "IT - Wildfire-U. S.",
  "RF - Fire - Canadian.",
  "IF - Fire - Canadian."
)

# Convert date_local to Date and summarize only the specified qualifiers
flags <- hourly %>%
  mutate(date_local = as.Date(date_local, format = "%Y-%m-%d")) %>%
  group_by(date_local) %>%
  summarize(qualifier = paste(
    unique(na.omit(qualifier[qualifier %in% qualifiers])), 
    collapse = ", "
  )) %>%
  ungroup()
  
# Filter and convert the date_local column in one step
pm25_samples <- daily %>%
  filter(sample_duration == "1 HOUR" &
           validity_indicator == "Y") %>%
  mutate(date_local = as.Date(date_local, format = "%Y-%m-%d"))

# Merge flags with pm25_samples by date_local
pm25_samples <- pm25_samples %>%
  left_join(flags, by = "date_local")

##### Unique Daily #####

# Filter pm25_samples for unique date_local values
pm25_samples <- pm25_samples %>%
  distinct(date_local, .keep_all = TRUE)

##### Tiering Values #####

# Define the path to the Excel file
epa_tiers_path <- "/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/EE Demos/Tiering/tiering_2019_2023.xlsx"

# Concatenate to form site_id
site_id <- paste0(stateFIPS, countycode, sitenum)

# Read the Excel file
tiering_data <- read_excel(epa_tiers_path)

# Filter for the specified site and select the required columns
monthly_98th_percentile <- tiering_data %>%
  filter(SITE_ID == site_id) %>%
  select(month, Monthly_98th)

# Define min_annual_98th
min_annual_98th <- tiering_data %>%
  filter(SITE_ID == site_id) %>%
  pull(annual_98th_minimum) %>%
  unique()  # Ensure it's a single value

##### Add Tiers #####

# Convert the month in pm25_samples to numeric
pm25_samples_ee <- pm25_samples %>%
  # Extract the month from date_local and convert it to numeric
  mutate(month = as.numeric(format(date_local, "%m"))) %>%
  # Add the min_annual_98th as a constant value for all rows
  mutate(min_annual_98th = min_annual_98th) %>%
  # Join the monthly_98th_percentile based on the month
  left_join(monthly_98th_percentile, by = "month")

# Reduce pm25_samples_ee to only include the specified columns
pm25_samples_ee <- pm25_samples_ee %>%
  select(date_local, arithmetic_mean, local_site_name, qualifier, min_annual_98th, Monthly_98th)

# Add the 'threshold' column based on the comparison
pm25_samples_ee <- pm25_samples_ee %>%
  mutate(threshold = ifelse(Monthly_98th < min_annual_98th, "Month", "Annual"))

# Add 'Tier 2' column with the lesser of Monthly_98th and min_annual_98th
pm25_samples_ee <- pm25_samples_ee %>%
  mutate(Tier_2 = pmin(Monthly_98th, min_annual_98th))

# Add 'Tier 1' column with 1.5 times the value of Tier 2
pm25_samples_ee <- pm25_samples_ee %>%
  mutate(Tier_1 = 1.5 * Tier_2)

# Add 'Tier_Label' column based on the given conditions
pm25_samples_ee <- pm25_samples_ee %>%
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

# Step 2: Identify the most recent 3 years in the dataset
recent_years <- pm25_samples_ee %>%
  mutate(year = format(date_local, "%Y")) %>%
  pull(year) %>%
  unique() %>%
  as.numeric() %>%
  sort(decreasing = TRUE) %>%
  head(3)

# Step 3: Filter the data to include only rows from the most recent 3 years with a non-missing Tier_Label
pm25_all_tier_ee <- pm25_samples_ee %>%
  mutate(year = format(date_local, "%Y")) %>%
  filter(as.numeric(year) %in% recent_years) %>%
  filter(!is.na(Tier_Label))

# Filter just T2+"
pm25_t2_plus <- pm25_all_tier_ee %>%
  filter(Tier_Label %in% c("Tier 1", "Tier 2")) %>%
  filter(str_detect(qualifier, "RT - Wildfire-U. S.") | str_detect(qualifier, "RF - Fire - Canadian."))

##### PLOTTING - EE Days #####

library(ggplot2)

# Step 1: Create a column for the day of the year
pm25_samples_ee <- pm25_samples_ee %>%
  mutate(day_of_year = yday(date_local))

# Step 2: Create a color group based on your criteria
pm25_samples_ee <- pm25_samples_ee %>%
  mutate(color_group = case_when(
    year(date_local) %in% 2021:2023 & Tier_Label == "Tier 1" ~ "2021-2023, Tier 1 Submitted EE Days",
    year(date_local) %in% 2021:2023 & Tier_Label == "Tier 2" ~ "2021-2023, Tier 2 Submitted EE Days",
    year(date_local) %in% 2017:2023 & grepl("RT - Wildfire-U. S.|IT - Wildfire-U. S.|RF - Fire - Canadian.|IF - Fire - Canadian.", qualifier) ~ "2017-2023, Wildfire Smoke Days (RT, RF, IT, IF Flags)",
    TRUE ~ "2017-2023, non-Wildfire Smoke Days"
  ))

# Step 3: Explicitly reorder the rows so "2017-2023" is plotted first
pm25_samples_ee <- pm25_samples_ee %>%
  arrange(desc(color_group == "2017-2023, non-Wildfire Smoke Days"))

# Step 4: Create a scatter plot with custom colors, ordered legend, and a white background box for the legend

# Define month boundaries for vertical lines
month_boundaries <- cumsum(c(0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30))

ggplot() +
  geom_vline(xintercept = month_boundaries, color = "gray90", alpha = .5, linetype = "solid") +
  geom_point(data = pm25_samples_ee, aes(x = day_of_year, y = arithmetic_mean, color = color_group)) +
  scale_x_continuous(breaks = month_boundaries,
                     labels = month.abb,
                     expand = c(0,0),
                     limits = c(0, 366)) +  # Handle leap years by allowing 366 days
  scale_color_manual(
    name = "Libby  2017-2023",
    values = c(
    "2021-2023, Tier 1 Submitted EE Days" = "#004a98",  # Red for 2021-2023 Tier 1
    "2021-2023, Tier 2 Submitted EE Days" = "#009ADE",  # Orange for 2021-2023 Tier 2
    "2017-2023, Wildfire Smoke Days (RT, RF, IT, IF Flags)" = "gray70",  # Yellow for Wildfire flagged days
    "2017-2023, non-Wildfire Smoke Days" = "gray85"  # Gray for other data
  ),
  limits = c(  # Ensure the order in the legend matches this
    "2021-2023, Tier 1 Submitted EE Days",
    "2021-2023, Tier 2 Submitted EE Days",
    "2017-2023, Wildfire Smoke Days (RT, RF, IT, IF Flags)",
    "2017-2023, non-Wildfire Smoke Days"
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

##### Historical Averages #####

# May to Oct 95th percentile
may_to_oct <- pm25_samples_ee %>%
  filter(month(date_local) >= 5 & month(date_local) <= 9)

may_oct_percentile_95 <- quantile(may_to_oct$arithmetic_mean, probs = 0.95, na.rm = TRUE)

# May to Oct 95th percentile - no flags
dates_to_exclude <- pm25_samples_ee %>%
  filter(qualifier %in% qualifiers) %>%
  pull(date_local) %>%
  unique()

may_oct_no_flags <- pm25_samples_ee %>%
  filter(!date_local %in% dates_to_exclude) %>%
  filter(month(date_local) >= 5 & month(date_local) <= 9)

may_oct_percentile_95_no_flags <- quantile(may_oct_no_flags$arithmetic_mean, probs = 0.95, na.rm = TRUE)

# Daily Average
pm25_daily_avg <- pm25_samples_ee %>%
  group_by(day_of_year) %>%
  summarize(daily_avg = mean(arithmetic_mean, na.rm = TRUE)) %>%
  ungroup()

# Daily Average with flags
pm25_daily_avg_no_flags <- pm25_samples_ee %>%
  filter(!date_local %in% dates_to_exclude)

# Calculate the daily average by grouping day_of_year
pm25_daily_avg_no_flags <- pm25_daily_avg_no_flags %>%
  group_by(day_of_year) %>%
  summarize(daily_avg = mean(arithmetic_mean, na.rm = TRUE)) %>%
  ungroup()

##### Wildfire Flagged Days #####

# Step 2: Filter pm25_samples_ee for these dates
pm25_flagged_days <- pm25_samples_ee %>%
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

ggplot() +
  # Add vertical lines at month boundaries
  geom_vline(xintercept = month_boundaries, color = "gray90", alpha = .5, linetype = "solid") +
  # Plot points for 2017-2023 24-hr PM2.5 Avg with legend
  geom_point(data = pm25_samples_ee, aes(x = day_of_year, y = arithmetic_mean, color = "24-hr PM2.5 Avg (no wildfire flags)"), size = 2) +
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
    color = "Libby  2017-2023"
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
    legend.position = c(0.25, 0.75),  # Position the legend in the top-left corner
    legend.background = element_rect(fill = "white", color = NA),  # White background for legend box
    legend.box.background = element_rect(color = "black"),  # Black border around the legend box (optional)
    panel.grid.major.x = element_blank(),  # Remove vertical grid lines
    panel.grid.minor.x = element_blank(),  # Remove minor vertical grid lines
    legend.title = element_text(face = "bold")  # Make legend title bold
  )

##### Seasonal Hourly #####

# Select specific columns and ensure date_local is of Date type
hourly_pm25_samples <- hourly %>%
  select(date_local, time_local, sample_measurement) %>%
  mutate(date_local = as.Date(date_local))

# Perform the left join to add Tier_Label to hourly_pm25_samples
hourly_pm25_samples <- hourly_pm25_samples %>%
  left_join(pm25_t2_plus %>% select(date_local, Tier_Label), by = "date_local")

##### PLOTTING - Hourly Fire Seasons #####

# Combine date_local and time_local into a single datetime column and sort by datetime
fire_season_a <- hourly_pm25_samples %>%
  mutate(
    # Combine date and time into a single POSIXct datetime column
    datetime = as.POSIXct(paste(date_local, time_local), format = "%Y-%m-%d %H:%M")
  ) %>%
  filter(datetime >= as.POSIXct("2021-05-01 00:00") & datetime <= as.POSIXct("2021-11-01 23:59")) %>%
  filter(year(datetime) == 2021) %>%
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
ggplot(fire_season_a, aes(x = datetime, y = sample_measurement)) +
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
    color = "Libby - 2021 Wildfire Smoke Season"  # Color label
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
    legend.position = c(0, 1),                          # Position the legend at the top-left
    legend.justification = c(0, 1),                     # Adjust the legend's justification
    legend.background = element_rect(fill = "white"),    # Add a white box behind the legend
    legend.title = element_text(face = "bold")           # Make legend title bold
  )

##### PLOTTING - Wildfire Season 2 #####

# Combine date_local and time_local into a single datetime column and sort by datetime
fire_season_b <- hourly_pm25_samples %>%
  mutate(
    # Combine date and time into a single POSIXct datetime column
    datetime = as.POSIXct(paste(date_local, time_local), format = "%Y-%m-%d %H:%M")
  ) %>%
  filter(datetime >= as.POSIXct("2022-05-01 00:00") & datetime <= as.POSIXct("2022-11-01 23:59")) %>%
  filter(year(datetime) == 2022) %>%
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
ggplot(fire_season_b, aes(x = datetime, y = sample_measurement)) +
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
    color = "Libby - 2022 Wildfire Smoke Season"  # Color label
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
    legend.position = c(0, 1),                          # Position the legend at the top-left
    legend.justification = c(0, 1),                     # Adjust the legend's justification
    legend.background = element_rect(fill = "white"),    # Add a white box behind the legend
    legend.title = element_text(face = "bold")           # Make legend title bold
  )

##### PLOTTING - Wildfire Season 3 #####

# Combine date_local and time_local into a single datetime column and sort by datetime
fire_season_c <- hourly_pm25_samples %>%
  mutate(
    # Combine date and time into a single POSIXct datetime column
    datetime = as.POSIXct(paste(date_local, time_local), format = "%Y-%m-%d %H:%M")
  ) %>%
  filter(datetime >= as.POSIXct("2023-05-01 00:00") & datetime <= as.POSIXct("2023-11-01 23:59")) %>%
  filter(year(datetime) == 2023) %>%
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
ggplot(fire_season_c, aes(x = datetime, y = sample_measurement)) +
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
    color = "Libby - 2023 Wildfire Smoke Season"  # Color label
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
    legend.position = c(0, 1),                          # Position the legend at the top-left
    legend.justification = c(0, 1),                     # Adjust the legend's justification
    legend.background = element_rect(fill = "white"),    # Add a white box behind the legend
    legend.title = element_text(face = "bold")           # Make legend title bold
  )

