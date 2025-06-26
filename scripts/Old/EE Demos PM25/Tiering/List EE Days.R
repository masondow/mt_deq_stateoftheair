#later in this script, only T2+ R-flag days are filtered to be highlighted because
#of two days that were I-flagged but not included in demo. In future when determining
#final EE days to flip to R-flags from I-flags, remember to filter for I and R flags
library(dplyr)
library(lubridate)
library(stringr)
library(readxl)
library(magrittr)

#sites: "French Town - Beckwith"  "Libby Courthouse Annex"
#site_filter <- "Libby Courthouse Annex"
#file_sitename <- "Libby"
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

# Filter for all sites
EE_samples <- daily_allyr_flags %>%
  filter(year <= DV_year &
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
  mutate(tier_year = 2023,
         poc = 3) %>%
  select(state_code,
         county_code,
         site_number,
         poc,
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
  ) 

##### Updated Tiering Values (for 2024 and beyond)

tiering_data_24 <- readRDS("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/EE Demos/Tiering/Tier Data/tiers_2020_2024.rds")
tiering_data_24 <- tiering_data_24 %>%
  ungroup() %>%
  mutate(tier_year = 2024) %>%
  select(local_site_name,
         state_code,
         county_code,
         site_number,
         poc,
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
  filter(!is.na(Tier_Label)) %>%
  filter(parameter_code == 88101,
         poc == 3) %>%
  select(
    local_site_name,
    state_code,
    county_code,
    site_number,
    poc,
    date_local,
    city,
    county,
    month,
    year,
    arithmetic_mean,
    qualifier,
    threshold,
    Tier_1,
    Tier_2,
    Tier_Label
  )

library(writexl)
# Save a data frame to an Excel file
write_xlsx(EE_days_all_tiers, paste0("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/EE Demos/Tiering/", DV_year,"_DV_PM25_EE_days.xlsx"))



