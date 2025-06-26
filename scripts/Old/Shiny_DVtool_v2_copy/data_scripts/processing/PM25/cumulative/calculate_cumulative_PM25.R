library(lubridate)
library(dplyr)
library(tidyr)
library(readxl)
# PM25 cumulative processing

daily <- readRDS(here("data_processed","merged_daily","merged_daily_PM25.rds"))
DVs <- read_excel(here("data_processed","DVs","ann_PM25_DVs.xlsx"))
PM25_sites_filtered <- readRDS(here("data_processed", "site_input_map","PM25_sites_filtered.RDS"))

#### List ACTIVE Sites (to filter for Cumulative PM25 and DV tracker sites) ####

# Step 1: select best poc if multiple for given year
sites_filtered <- DVs %>% # these are the site-pocs that should be selected for a given year (i.e., they have the greatest amount of representative data over the DV period for that DV year)
  semi_join(PM25_sites_filtered %>%
              select(site_name, local_site_name, county_code, site_number, poc, year))

# Step 2: Get the current year - 1 day
current_year_minus_1_day <- year(Sys.Date() - days(1))

# Step 3: Filter for sites producing data this year
sites_filtered_recent <- sites_filtered %>%
  filter(year == current_year_minus_1_day) %>%
  distinct(site_name, local_site_name, county_code, site_number, poc) # Keep only unique site_name and poc

#### Calculate Cumulative PM25 ####
cumulative <- daily %>%
  semi_join(sites_filtered_recent, by = c("site_name", "county_code", "site_number", "poc")) %>% # filter only sites producing data in current year
  select(site_name, local_site_name, county_code, site_number, poc, date_time, year, quarter, arithmetic_mean, source)

cumulative_PM25 <- cumulative %>% # calculate cumulative PM25
  group_by(county_code, site_number, poc, year) %>%
  arrange(date_time, .by_group = TRUE) %>%  # Ensure data is sorted by date
  mutate(cumulative_PM25 = cumsum(arithmetic_mean)) %>%  # Calculate cumulative sum
  ungroup()

cumulative_PM25 <- cumulative_PM25 %>% # add labels for mapping plot colors later
  group_by(county_code, site_number, poc) %>%
  mutate(most_recent_year = max(year)) %>%  # Get the most recent year for each group
  mutate(color_label = case_when(
    year == most_recent_year ~ "DV yr 3",
    year == most_recent_year - 1 ~ "DV yr 2",
    year == most_recent_year - 2 ~ "DV yr 1",
    TRUE ~ NA_character_  # Assign NA if outside the range
  )) %>%
  ungroup() %>%
  mutate(
    # Get min/max years excluding DV years for non-DV period labels
    min_year = min(year[is.na(color_label)]),
    max_year = max(year[is.na(color_label)]),
    
    # Assign range label to remaining years
    color_label = if_else(
      is.na(color_label),
      paste0(min_year, "-", max_year),
      color_label
    )
  ) %>%
  ungroup() %>%
  select(-min_year, -max_year)  # Remove temporary columns

# Adjust cumulative PM25 with fraction to flatten cumulative value
cumulative_PM25 <- cumulative_PM25 %>%
  arrange(county_code, site_number, poc, date_time) %>%
  group_by(county_code, site_number, poc, year) %>%
  mutate(
    data_days = cumsum(!is.na(arithmetic_mean)),  # Count only non-NA days
    cumulative_avg_flat = cumulative_PM25/data_days
  ) %>%
  ungroup()

#### Calculate DV Threshold ####

DVs_filtered <- DVs %>% # filter for 1st 2 years of current DV period
  semi_join(sites_filtered_recent, by = c("site_name", "poc")) %>% # filter only sites producing data in current year
  select(site_name, local_site_name, county_code, site_number, poc, annual_average, year) %>%
  group_by(site_name, local_site_name, county_code, site_number, poc) %>%
  filter(year >= current_year_minus_1_day-2 & year <= current_year_minus_1_day-1) %>%
  ungroup()

threshold <- DVs_filtered %>%
  group_by(site_name, local_site_name, county_code, site_number, poc) %>%
  summarise(
    prior_2yr_avg_sum = mean(annual_average)*2,  # Sum of the 2 available annual averages (mult. avg x 2 to account for sites with only 1 year of data)
    required_annual_avg = (9.05 * 3) - prior_2yr_avg_sum  # Solve for the missing year
  ) %>%
  ungroup()

#### Calculate DV Tracker ####
DV_tracker <- cumulative_PM25 %>%
  select(-site_name, -local_site_name) %>%
  left_join(sites_filtered_recent, by = c("county_code","site_number","poc")) %>%
  filter(year == current_year_minus_1_day) %>%
  select(-cumulative_PM25, -most_recent_year, -color_label, -data_days, -cumulative_avg_flat)

# Generate full sequence of dates from Jan 1 of last year to yesterday
site_keys <- DV_tracker %>%
  distinct(county_code, site_number, poc)

full_dates <- expand.grid(
  date_time = seq.POSIXt(
    from = as.POSIXct(paste0(current_year_minus_1_day, "-01-01"), tz = "UTC"), 
    to = as.POSIXct(Sys.Date() - 1, tz = "UTC"), 
    by = "day"
  )
) %>%
  merge(site_keys, by = NULL)  # Add all date × site combos

DV_tracker <- full_dates %>%
  left_join(DV_tracker, by = c("county_code", "site_number", "poc", "date_time")) %>%
  arrange(county_code, site_number, poc, date_time) %>%
  group_by(county_code, site_number, poc, quarter, year) %>%
  mutate(
    quarter_data_days = cumsum(!is.na(arithmetic_mean)),  
    quarter_cumulative_PM25 = cumsum(arithmetic_mean),
    rolling_avg_quarter_mean = quarter_cumulative_PM25 / quarter_data_days
  ) %>%
  fill(quarter_data_days, quarter_cumulative_PM25, .direction = "down") %>%
  ungroup() %>%
  mutate(
    Q1 = ifelse(quarter == 1, rolling_avg_quarter_mean, NA_real_),
    Q2 = ifelse(quarter == 2, rolling_avg_quarter_mean, NA_real_),
    Q3 = ifelse(quarter == 3, rolling_avg_quarter_mean, NA_real_),
    Q4 = ifelse(quarter == 4, rolling_avg_quarter_mean, NA_real_)
  ) %>%
  group_by(site_name, year) %>%
  fill(Q1, Q2, Q3, Q4, .direction = "down") %>%  # Fill missing values within each site and year
  ungroup() %>%
  # Calculate the sum of only Q1, Q2, Q3, and Q4
  mutate(rolling_avg_ann_mean = rowMeans(select(., Q1, Q2, Q3, Q4), na.rm = TRUE)) %>%
  filter(!is.na(quarter))

DV_tracker <- DV_tracker %>%
  left_join(
    threshold %>%
      select(site_name, prior_2yr_avg_sum),
    by = c("site_name")
  ) %>%
  group_by(site_name) %>%
  mutate(
    rolling_DV = (prior_2yr_avg_sum+rolling_avg_ann_mean)/3
  ) %>%
  ungroup()

DV_tracker <- DV_tracker %>%
  arrange(site_name, date_time) %>%
  group_by(site_name) %>%
  mutate(NAAQS = 9) %>%
  ungroup()


saveRDS(cumulative_PM25,here("data_processed","cumulative","cumulative_PM25.rds"))
saveRDS(DV_tracker,here("data_processed","cumulative","DV_tracker.rds"))







