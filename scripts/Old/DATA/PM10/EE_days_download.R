library(dplyr)
library(lubridate)
library(stringr)
library(readxl)
library(RAQSAPI)
library(magrittr)
library(ggplot2)

##### AQS Data #####

daily_pm10 <- readRDS("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/DATA/PM10/2000_2024_daily_pm10_81102_all_MT.rds")
hourly_pm10 <- readRDS("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/DATA/PM10/2000_2024_hourly_pm10_81102_all_MT.rds")

##### Add Flags #####

# Define qualifiers (for exclusion later)
qualifiers <- c(
  "RT - Wildfire-U. S.",
  "RF - Fire - Canadian.",
  "IF - Fire - Canadian.",
  "IT - Wildfire-U. S.",
  "E - Forest Fire."
)

# Convert date_local to Date and summarize only the specified qualifiers
flags <- hourly_pm10 %>%
  mutate(date_local = as.Date(date_local, format = "%Y-%m-%d")) %>%
  group_by(date_local, state_code, county_code, site_number, poc) %>%
  summarize(qualifier = paste(
    unique(na.omit(qualifier[qualifier %in% qualifiers])), 
    collapse = ", "
  )) %>%
  ungroup()

daily_pm10_flags <- daily_pm10 %>%
  filter(sample_duration == "1 HOUR" & validity_indicator == "Y") %>%
  mutate(
    date_local = as.Date(date_local, format = "%Y-%m-%d"),
    year = year(date_local),       # Extract year from date_local
    month = month(date_local),     # Extract month from date_local
    quarter = case_when(           # Calculate quarter based on month
      month %in% 1:3 ~ 1,
      month %in% 4:6 ~ 2,
      month %in% 7:9 ~ 3,
      month %in% 10:12 ~ 4
    )
  ) %>%
  distinct(date_local, local_site_name, poc, .keep_all = TRUE)

# Merge flags with daily_pm25 by multiple columns
daily_pm10_flags <- daily_pm10_flags %>% 
  left_join(flags, by = c("date_local", "state_code", "county_code", "site_number", "poc"))

PM10_EE_wildfire_days <- daily_pm10_flags %>%
  filter(!is.na(qualifier) & qualifier != "")

library(writexl)
write_xlsx(PM10_EE_wildfire_days, "/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/DATA/PM10/2000_2024_PM10_EE_wildfire_days.xlsx")
