library(dplyr)
library(lubridate)

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

tier_year <- 2024

# Filter for 5 most-recent years to calculate tiers
daily_5yr <- daily_allyr_flags %>%
  filter(is.na(qualifier) | qualifier == "" & 
           year <= tier_year &
           year >= paste0(tier_year-4)
           )

monthly_tier <- daily_5yr %>%
  group_by(local_site_name, city, state_code, county_code, site_number, poc, month) %>%
  summarize(month_98th = quantile(arithmetic_mean, probs = 0.98, na.rm = TRUE))

annual_tier <- daily_5yr %>%
  group_by(local_site_name, city, state_code, county_code, site_number, poc, year) %>%
  summarize(min_ann_98th = quantile(arithmetic_mean, probs = 0.98, na.rm = TRUE)) %>%
  group_by(local_site_name, city, poc) %>%
  filter(min_ann_98th == min(min_ann_98th), na.rm = TRUE)

tiers <- left_join(monthly_tier, annual_tier, by = c("local_site_name", "city", "state_code", "county_code", "site_number","poc"))

saveRDS(tiers, "/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/EE Demos/Tiering/Tier Data/tiers_2020_2024.rds")

#read <- readRDS("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/EE Demos/Tiering/Tier Data/tiers_2020_2024.rds")
