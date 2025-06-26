library(dplyr)
library(lubridate)
library(stringr)
library(readxl)
library(magrittr)
library(ggplot2)

#sites: "French Town - Beckwith"  "Libby Courthouse Annex"
site_filter1 <- "French Town - Beckwith"
short_name1 <- "Frenchtown"

site_filter2 <- "Libby Courthouse Annex"
short_name2 <- "Libby"

DV_year <- 2024

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
  filter(local_site_name %in% c(site_filter1, site_filter2) &
           year <= DV_year &
           year >= DV_year-2)

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
  filter(local_site_name %in% c(site_filter1, site_filter2))

##### Updated Tiering Values (for 2024 and beyond)

tiering_data_24 <- readRDS("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/EE Demos/Tiering/Tier Data/tiers_2020_2024.rds")
tiering_data_24 <- tiering_data_24 %>%
  ungroup() %>%
  filter(local_site_name %in% c(site_filter1, site_filter2)) %>%
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

# Truncate the arithmetic_mean values after the first decimal place
EE_samples_tier <- EE_samples_tier %>%
  mutate(arithmetic_mean = floor(arithmetic_mean * 10) / 10)

##### SITE 1 Calculate DV #####

site1 <- EE_samples_tier %>%
  filter(local_site_name == site_filter1)

# site1
# Step 1: Create a quarter column based on the month
site1_DV <- site1 %>%
  mutate(quarter = case_when(
    month %in% 1:3 ~ 1,
    month %in% 4:6 ~ 2,
    month %in% 7:9 ~ 3,
    month %in% 10:12 ~ 4
  ))

# Step 2: Calculate the average arithmetic_mean for each quarter within each year
site1_quarterly_averages <- site1_DV %>%
  group_by(year, quarter) %>%
  summarize(quarterly_avg = mean(arithmetic_mean, na.rm = TRUE), .groups = 'drop')

# Step 3: Calculate the annual average by averaging the quarterly averages
site1_annual_averages <- site1_quarterly_averages %>%
  group_by(year) %>%
  summarize(annual_avg = mean(quarterly_avg, na.rm = TRUE), .groups = 'drop')

# Step 4: Calculate the design value by averaging the annual averages across three years
site1_design_value <- site1_annual_averages %>%
  summarize(design_value = mean(annual_avg, na.rm = TRUE))

# View the design value
print(site1_design_value)

##### Calculate DV w/o Tier 1 #####

# site1
# Step 1: Remove T1 R-flagged days from previous 2 years and any T1 days from current year
site1_DV_no_T1 <- site1_DV %>%
  filter(!(Tier_Label == "Tier 1" & qualifier %in% c("RT - Wildfire-U. S.", "RF - Fire - Canadian.") & year < DV_year)) %>%
  filter(!(Tier_Label == "Tier 1" & DV_year == year & !is.na(qualifier) & qualifier != ""))

# Step 2: Calculate the average arithmetic_mean for each quarter within each year
site1_quarterly_averages_no_T1 <- site1_DV_no_T1 %>%
  group_by(year, quarter) %>%
  summarize(quarterly_avg = mean(arithmetic_mean, na.rm = TRUE), .groups = 'drop')

# Step 3: Calculate the annual average by averaging the quarterly averages
site1_annual_averages_no_T1 <- site1_quarterly_averages_no_T1 %>%
  group_by(year) %>%
  summarize(annual_avg = mean(quarterly_avg, na.rm = TRUE), .groups = 'drop')

# Step 4: Calculate the design value by averaging the annual averages across three years
site1_design_value_no_T1 <- site1_annual_averages_no_T1 %>%
  summarize(design_value = mean(annual_avg, na.rm = TRUE))

# View the design value
print(site1_design_value_no_T1)

##### Calculate DV w/o Tier 1 & Tier 2 #####

# site1
# Step 1: Remove T1 R-flagged days from previous 2 years and any T1 days from current year
site1_DV_no_T1_T2 <- site1_DV_no_T1 %>%
  filter(!(Tier_Label == "Tier 2" & qualifier %in% c("RT - Wildfire-U. S.", "RF - Fire - Canadian.") & year < DV_year)) %>%
  filter(!(Tier_Label == "Tier 2" & DV_year == year & !is.na(qualifier) & qualifier != ""))

# Step 2: Calculate the average arithmetic_mean for each quarter within each year
site1_quarterly_averages_no_T1_T2 <- site1_DV_no_T1_T2 %>%
  group_by(year, quarter) %>%
  summarize(quarterly_avg = mean(arithmetic_mean, na.rm = TRUE), .groups = 'drop')

# Step 3: Calculate the annual average by averaging the quarterly averages
site1_annual_averages_no_T1_T2 <- site1_quarterly_averages_no_T1_T2 %>%
  group_by(year) %>%
  summarize(annual_avg = mean(quarterly_avg, na.rm = TRUE), .groups = 'drop')

# Step 4: Calculate the design value by averaging the annual averages across three years
site1_design_value_no_T1_T2 <- site1_annual_averages_no_T1_T2 %>%
  summarize(design_value = mean(annual_avg, na.rm = TRUE))

# View the design value
print(site1_design_value_no_T1_T2)

##### SITE 1 Calculate DV #####

site2 <- EE_samples_tier %>%
  filter(local_site_name == site_filter2)

# site2
# Step 1: Create a quarter column based on the month
site2_DV <- site2 %>%
  mutate(quarter = case_when(
    month %in% 1:3 ~ 1,
    month %in% 4:6 ~ 2,
    month %in% 7:9 ~ 3,
    month %in% 10:12 ~ 4
  ))

# Step 2: Calculate the average arithmetic_mean for each quarter within each year
site2_quarterly_averages <- site2_DV %>%
  group_by(year, quarter) %>%
  summarize(quarterly_avg = mean(arithmetic_mean, na.rm = TRUE), .groups = 'drop')

# Step 3: Calculate the annual average by averaging the quarterly averages
site2_annual_averages <- site2_quarterly_averages %>%
  group_by(year) %>%
  summarize(annual_avg = mean(quarterly_avg, na.rm = TRUE), .groups = 'drop')

# Step 4: Calculate the design value by averaging the annual averages across three years
site2_design_value <- site2_annual_averages %>%
  summarize(design_value = mean(annual_avg, na.rm = TRUE))

# View the design value
print(site2_design_value)

##### Calculate DV w/o Tier 1 #####

# site2
# Step 1: Remove T1 R-flagged days from previous 2 years and any T1 days from current year
site2_DV_no_T1 <- site2_DV %>%
  filter(!(Tier_Label == "Tier 1" & qualifier %in% c("RT - Wildfire-U. S.", "RF - Fire - Canadian.") & year < DV_year)) %>%
  filter(!(Tier_Label == "Tier 1" & DV_year == year & !is.na(qualifier) & qualifier != ""))

# Step 2: Calculate the average arithmetic_mean for each quarter within each year
site2_quarterly_averages_no_T1 <- site2_DV_no_T1 %>%
  group_by(year, quarter) %>%
  summarize(quarterly_avg = mean(arithmetic_mean, na.rm = TRUE), .groups = 'drop')

# Step 3: Calculate the annual average by averaging the quarterly averages
site2_annual_averages_no_T1 <- site2_quarterly_averages_no_T1 %>%
  group_by(year) %>%
  summarize(annual_avg = mean(quarterly_avg, na.rm = TRUE), .groups = 'drop')

# Step 4: Calculate the design value by averaging the annual averages across three years
site2_design_value_no_T1 <- site2_annual_averages_no_T1 %>%
  summarize(design_value = mean(annual_avg, na.rm = TRUE))

# View the design value
print(site2_design_value_no_T1)

##### Calculate DV w/o Tier 1 & Tier 2 #####

# site2
# Step 1: Remove T1 R-flagged days from previous 2 years and any T1 days from current year
site2_DV_no_T1_T2 <- site2_DV_no_T1 %>%
  filter(!(Tier_Label == "Tier 2" & qualifier %in% c("RT - Wildfire-U. S.", "RF - Fire - Canadian.") & year < DV_year)) %>%
  filter(!(Tier_Label == "Tier 2" & DV_year == year & !is.na(qualifier) & qualifier != ""))

# Step 2: Calculate the average arithmetic_mean for each quarter within each year
site2_quarterly_averages_no_T1_T2 <- site2_DV_no_T1_T2 %>%
  group_by(year, quarter) %>%
  summarize(quarterly_avg = mean(arithmetic_mean, na.rm = TRUE), .groups = 'drop')

# Step 3: Calculate the annual average by averaging the quarterly averages
site2_annual_averages_no_T1_T2 <- site2_quarterly_averages_no_T1_T2 %>%
  group_by(year) %>%
  summarize(annual_avg = mean(quarterly_avg, na.rm = TRUE), .groups = 'drop')

# Step 4: Calculate the design value by averaging the annual averages across three years
site2_design_value_no_T1_T2 <- site2_annual_averages_no_T1_T2 %>%
  summarize(design_value = mean(annual_avg, na.rm = TRUE))

# View the design value
print(site2_design_value_no_T1_T2)

##### PLOT #####

site1_design_value <- site1_design_value %>%
  mutate(site = paste0(short_name1),
         tier = "All EE Days Included")
site1_design_value_no_T1 <- site1_design_value_no_T1 %>%
  mutate(site = paste0(short_name1),
         tier = "Tier 1 EE Days Excluded")
site1_design_value_no_T1_T2 <- site1_design_value_no_T1_T2 %>%
  mutate(site = paste0(short_name1),
         tier = "Tier 1 & 2 EE Days Excluded")
site2_design_value <- site2_design_value %>%
  mutate(site = paste0(short_name2),
         tier = "All EE Days Included")
site2_design_value_no_T1 <- site2_design_value_no_T1 %>%
  mutate(site = paste0(short_name2),
         tier = "Tier 1 EE Days Excluded")
site2_design_value_no_T1_T2 <- site2_design_value_no_T1_T2 %>%
  mutate(site = paste0(short_name2),
         tier = "Tier 1 & 2 EE Days Excluded")
DV_plot <- bind_rows(site1_design_value,
                     site1_design_value_no_T1,
                     site1_design_value_no_T1_T2,
                     site2_design_value,
                     site2_design_value_no_T1,
                     site2_design_value_no_T1_T2)

# Plotting
p <- ggplot(DV_plot, aes(x = site, y = design_value, fill = tier)) +
  geom_col(position = position_dodge(width = 0), width = 1.25) + # Dodges columns to avoid stacking
  geom_hline(yintercept = 9, linetype = "dashed", color = "#F54D28", linewidth = 0.75) + # Add dashed line at y = 9
  #geom_hline(yintercept = 12, linetype = "dashed", color = "gray80", linewidth = 0.75) +
  scale_y_continuous(limits = c(0,14),
                     expand = c(0,0),
                     breaks = seq(0,14, by = 1)) +
  scale_fill_manual(
    values = c(
      "All EE Days Included" = "#004A98",
      "Tier 1 EE Days Excluded" = "#009ADE",
      "Tier 1 & 2 EE Days Excluded" = "gray80"
    ),
    breaks = c(
      "All EE Days Included",
      "Tier 1 EE Days Excluded",
      "Tier 1 & 2 EE Days Excluded"
    )
  ) +
  labs(
    title = paste0(DV_year-2,"-",DV_year," Annual PM2.5 Design Values"),
    x = "",
    y = expression("PM"["2.5"] ~ "(µg/m"^3 ~ ")"),
    fill = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),  # Adjust x-axis text size
    axis.text.y = element_text(size = 13),  # Adjust y-axis text size
    axis.title.x = element_text(size = 14),  # Adjust x-axis title size
    axis.title.y = element_text(size = 14),  # Adjust y-axis title size
    plot.title = element_text(size = 15, face = "bold"),  # Adjust plot title size
    legend.text = element_text(size = 12),  # Adjust legend text size
    legend.title = element_blank(),  # Remove legend title
    legend.position = c(0.32, 0.9),  # Position legend in the top-left corner
    legend.background = element_rect(fill = "white", color = NA),  # Add white background to legend
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),  # Adjust margin around legend items
    legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0),  # Adjust margin around the legend box
    panel.border = element_rect(color = "gray50", fill = NA, size = .75),  # Border around plot area
    
    # Grid line settings
    panel.grid.major.x = element_blank(),  # Remove vertical grid lines
    panel.grid.minor.x = element_blank(),  # Remove minor vertical grid lines
    panel.grid.major.y = element_line(color = "gray90"),  # Keep major horizontal grid lines
    panel.grid.minor.y = element_blank()  # Remove minor horizontal grid lines
  )

# Print the plot to check
print(p)

# Save the plot as a PNG with transparent background
ggsave(paste0("/Users/cameronnealy/Downloads/", DV_year,"_DV_bar_plot.png"), 
       plot = p, 
       bg = "transparent", 
       width = 5, 
       height = 6, 
       dpi = 300)


