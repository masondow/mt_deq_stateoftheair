library(dplyr)
library(lubridate)
library(stringr)
library(readxl)
library(RAQSAPI)
library(magrittr)
library(ggplot2)

##### Setup Metadata #####
sites <- read_excel("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/EE Demos/AQS sites.xlsx")
sites_merge <- sites %>%
  select(
    site,
    state_code,
    county_code,
    site_number,
    parameter_code,
    poc,
    parameter_name,
    open_date,
    measurement_scale,
    measurement_scale_def,
    monitoring_objective,
    last_method_code,
    naaqs_primary_monitor,
    monitor_type,
    networks,
    monitoring_agency,
    latitude,
    longitude,
    datum,
    elevation,
    probe_height,
    pl_probe_location
  )

##### Import Raw AQS PM25 Data #####
daily_pm25 <- readRDS("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/DATA/2003_2023_daily_pm25_88101_88502_all_MT.rds")

##### Filter data 2021-2023 #####
# filter data and add year, month columns
daily_pm25_simple <- daily_pm25 %>%
  mutate(
    date_local = as.Date(date_local, format = "%Y-%m-%d"),
    year = year(date_local),  # Extract year from date_local and add as a new column
    month = month(date_local)
  ) %>%
  filter(sample_duration == "1 HOUR" & 
           validity_indicator == "Y" &
           year >= 2021 & year <= 2023) %>%
  distinct(date_local, local_site_name, .keep_all = TRUE) 

##### Merge Data and Metadata #####
# Ensure columns are the same type for both the data and metadata
daily_pm25_simple <- daily_pm25_simple %>%
  mutate(state_code = as.character(state_code),
    county_code = as.character(county_code),
    site_number = as.character(site_number),
    parameter_code = as.character(parameter_code),
    poc = as.character(poc)
    )

sites_merge <- sites_merge %>%
  mutate(state_code = as.character(state_code),
         county_code = as.character(county_code),
         site_number = as.character(site_number),
         parameter_code = as.character(parameter_code),
         poc = as.character(poc)
  )

# Perform the left join
daily_pm25_combined <- daily_pm25_simple %>%
  left_join(sites_merge, by = c("state_code", "county_code", "site_number", "parameter_code", "poc"))
# Update the 'site' column only if it has NA values
daily_pm25_combined <- daily_pm25_combined %>%
  mutate(site = if_else(is.na(site), local_site_name, site))
#_________________________

##### Prepare for Completeness Calcs #####
daily_pm25_completeness <- daily_pm25_combined %>%
  select(date_local, 
         arithmetic_mean,
         site,
         year,
         month)

##### Completeness #####
# Add a quarter column
daily_pm25_completeness <- daily_pm25_completeness %>%
  mutate(quarter = case_when(
    month %in% 1:3 ~ "Q1",
    month %in% 4:6 ~ "Q2",
    month %in% 7:9 ~ "Q3",
    month %in% 10:12 ~ "Q4"
  ))

# Function to get the total days in a quarter
get_total_days <- function(year, quarter) {
  if (quarter == "Q1") {
    return(ifelse(lubridate::leap_year(year), 91, 90))
  } else if (quarter == "Q4") {
    return(92)
  } else {
    return(90)
  }
}

# Calculate data completeness per site, year, and quarter
completeness_summary <- daily_pm25_completeness %>%
  group_by(site, year, quarter) %>%
  summarise(
    data_days = n(),  # Count the number of data days
    total_days = get_total_days(first(year), first(quarter)),  # Total days in the quarter
    completeness = data_days / total_days * 100  # Calculate percentage completeness
  ) %>%
  ungroup()

##### Check #####

# Create a data frame of all possible site, year, and quarter combinations for 2021-2023
all_combinations <- expand.grid(
  site = unique(completeness_summary$site),
  year = 2021:2023,
  quarter = c("Q1", "Q2", "Q3", "Q4")
)

# Left join the completeness summary with the all_combinations data frame
completeness_check <- all_combinations %>%
  left_join(completeness_summary, by = c("site", "year", "quarter")) %>%
  mutate(
    data_days = ifelse(is.na(data_days), 0, data_days),          # Replace missing data_days with 0
    total_days = ifelse(is.na(total_days), 90, total_days),      # Assume 90 days for missing quarters
    completeness = ifelse(is.na(completeness), 0, completeness)  # Replace missing completeness with 0%
  )

# Check if a quarter is considered complete (completeness >= 75%)
completeness_check <- completeness_check %>%
  mutate(is_complete = completeness >= 75)

# Summarize to check for 4 complete quarters per year and site
site_year_complete <- completeness_check %>%
  group_by(site, year) %>%
  summarise(complete_quarters = sum(is_complete)) %>%
  ungroup()

# Identify sites with at least one complete quarter in 2023
sites_with_2023_quarters <- site_year_complete %>%
  filter(year == 2023 & complete_quarters >= 1) %>%
  pull(site)

#filter to only include sites active in 2023
site_year_complete_active <- site_year_complete %>%
  filter(site %in% sites_with_2023_quarters)

# Add a "complete" column based on the criteria
site_year_complete_active <- site_year_complete_active %>%
  group_by(site) %>%
  mutate(complete = ifelse(sum(complete_quarters) == 12, "yes", "no")) %>%
  ungroup()
  
##### Save Data #####

# Saving the data frame as an RDS file
saveRDS(site_year_complete_active, "/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/EE Demos/TSD/site_completeness_2021_2023.rds")


# Saving the data frame as an RDS file
saveRDS(completeness_check, "/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/EE Demos/TSD/quarterly_completeness_2021_2023.rds")

