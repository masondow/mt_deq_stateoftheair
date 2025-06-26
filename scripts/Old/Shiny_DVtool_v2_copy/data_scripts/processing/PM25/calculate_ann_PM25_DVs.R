
##### AQS/AirNow Data #####

daily_PM25 <- readRDS(here("data_processed","merged_daily","merged_daily_PM25.rds"))

##### Add Flags #####

# Define qualifiers (for exclusion later)
qualifiers <- c(
  "RT - Wildfire-U. S.",
  "RF - Fire - Canadian.",
  "IF - Fire - Canadian.",
  "IT - Wildfire-U. S.",
  "E - Forest Fire."
)

daily_PM25_flags <- daily_PM25 %>%
  filter(!is.na(poc))

##### Quarterly Completeness #####

daily_PM25_flags <- daily_PM25_flags %>%
  mutate(data_days = case_when(
    sample_frequency %in% c("HOURLY", "EVERY DAY") ~ 1,
    sample_frequency == "EVERY 3RD DAY" ~ 3,
    sample_frequency == "EVERY 6TH DAY" ~ 6,
    sample_frequency == "EVERY 12TH DAY" ~ 12,
    TRUE ~ 1
  ))

# Define a function to get total days for a given year and quarter
get_total_days <- function(year, quarter) {
  if (quarter == 1) {
    return(as.numeric(as.Date(paste0(year, "-03-31")) - as.Date(paste0(year, "-01-01")) + 1))
  } else if (quarter == 2) {
    return(as.numeric(as.Date(paste0(year, "-06-30")) - as.Date(paste0(year, "-04-01")) + 1))
  } else if (quarter == 3) {
    return(as.numeric(as.Date(paste0(year, "-09-30")) - as.Date(paste0(year, "-07-01")) + 1))
  } else if (quarter == 4) {
    return(as.numeric(as.Date(paste0(year, "-12-31")) - as.Date(paste0(year, "-10-01")) + 1))
  }
}

# Calculate data completeness by site, year, and quarter
quarterly_completeness_summary <- daily_PM25_flags %>%
  group_by(county_code, site_number, poc, year, quarter) %>%
  summarise(
    data_days_sum = sum(data_days),  # Count unique days with data
    total_days = get_total_days(first(year), first(quarter)),  # Get total days for each quarter
    quarter_completeness = data_days_sum / total_days * 100  # Calculate completeness percentage
  ) %>%
  ungroup()

##### Annual Completeness #####

# Create the complete_quarters column with unique quarters per year and site
annual_completeness <- quarterly_completeness_summary %>%
  group_by(county_code, site_number, year, poc) %>%
  summarize(
    complete_quarters = n_distinct(quarter[quarter_completeness >= 75], na.rm = TRUE)
  ) %>%
  ungroup()

##### DV Completeness #####

DV_completeness <- annual_completeness %>%
  group_by(county_code, site_number, poc) %>%
  arrange(year) %>%  # Ensure data is sorted
  mutate(DV_period_complete_quarters = sapply(year, function(y) {
    # Select only years in the range [y-2, y]
    sum(complete_quarters[year >= (y - 2) & year <= y], na.rm = TRUE)
  })) %>%
  ungroup()

#### Average Annual Samples ####

DV_samples <- daily_PM25 %>%
  group_by(county_code, site_number, poc, year) %>%
  summarize(annual_samples = sum(!is.na(arithmetic_mean)))

# Calculate rolling 3-year period count for each site_name and poc
DV_samples <- DV_samples %>%
  arrange(county_code, site_number, poc, year) %>%
  group_by(county_code, site_number, poc) %>%
  mutate(DV_period_samples = rollapply(
    annual_samples, 
    width = 3,       # Rolling window size (3 years)
    FUN = sum,       # Summing the count of valid samples
    align = "right", # Align the window to the right (current year + 2 previous years)
    fill = NA        # Fill missing values with NA (incomplete windows)
  )) %>%
  ungroup()

DV_completeness <- DV_completeness %>%
  left_join(DV_samples)

##### Design Values #####
# filter for active sites and remove redundant POCs
PM25_DV_all <- daily_PM25_flags %>%
  mutate(arithmetic_mean = floor(arithmetic_mean*10)/10)

# quarterly averages
PM25_DV_all <- PM25_DV_all %>%
  group_by(site_name, local_site_name, county_code, site_number, poc, year, quarter) %>%
  summarize(quarterly_average = mean(arithmetic_mean, na.rm = TRUE), .groups = 'drop') 

# annual averages
PM25_DV_all <- PM25_DV_all %>%
  group_by(site_name, local_site_name, county_code, site_number, poc, year) %>%
  summarize(annual_average = mean(quarterly_average, na.rm = TRUE), .groups = 'drop')

# design value
PM25_DV_all <- PM25_DV_all %>%
  group_by(site_name, local_site_name, county_code, site_number, poc) %>%
  arrange(year) %>%  # Ensure data is sorted
  mutate(design_value = sapply(year, function(y) {
    # Select only years in the range [y-2, y]
    mean(annual_average[year >= (y - 2) & year <= y], na.rm = TRUE)
  })) %>%
  ungroup


##### Design Values no wildfire flags #####
# filter for active county_code, site_numbers, remove redundant POCs, and remove wildfire flagged data 
PM25_DV_no_flags <- daily_PM25_flags %>%
  filter((is.na(qualifier) | qualifier == "")) %>%  # Exclude rows where qualifier is not empty or NA
  mutate(arithmetic_mean = floor(arithmetic_mean * 10) / 10) # truncate to 1st decimal place (this is how data is presented by EPA from daily data download)

# quarterly averages
PM25_DV_no_flags <- PM25_DV_no_flags %>%
  group_by(site_name, local_site_name, county_code, site_number, poc, year, quarter) %>%
  summarize(quarterly_average = mean(arithmetic_mean, na.rm = TRUE), .groups = 'drop') 

# annual averages
PM25_DV_no_flags <- PM25_DV_no_flags %>%
  group_by(site_name, local_site_name, county_code, site_number, poc, year) %>%
  summarize(annual_average_no_flags = mean(quarterly_average, na.rm = TRUE), .groups = 'drop')

# design value
PM25_DV_no_flags <- PM25_DV_no_flags %>%
  group_by(site_name, local_site_name, county_code, site_number, poc) %>%
  arrange(year) %>%  # Ensure data is sorted
  mutate(design_value_no_wildfire = sapply(year, function(y) {
    # Select only years in the range [y-2, y]
    mean(annual_average_no_flags[year >= (y - 2) & year <= y], na.rm = TRUE)
  })) %>%
  ungroup

##### Max Daily Values ####
max_daily_values <- daily_PM25 %>%
  group_by(county_code, site_number, poc, year) %>%
  summarize(max_daily_value = round(max(arithmetic_mean, na.rm = TRUE), 1), .groups = "drop")

##### Summaries #####
sites <- readRDS(here("data","sites.rds")) %>%
  filter(parameter_code %in% PM25_codes)

site_metadata <- sites %>% # Merge site history (changes in parameter code or NAAQS primary monitor) by poc; ensures unique site-poc combos
  group_by(county_code, site_number, poc, monitoring_agency, county_name) %>%
  summarise(
    # Combine unique parameter codes into a sorted, comma-separated string
    parameter_code = paste(
      parameter_code[order(open_date)], collapse = ", "),
    open_date = paste(sort(unique(open_date)), collapse = ", "),
    close_date = paste(sort(unique(close_date)), collapse = ", "),
    naaqs_primary_monitor = paste(
      naaqs_primary_monitor[order(open_date)], collapse = ", "
    ),
    .groups = "drop"
  )

DV_list <- left_join(PM25_DV_all, PM25_DV_no_flags) %>%
  mutate(across(c(annual_average, annual_average_no_flags, design_value, design_value_no_wildfire), ~ round(.x, 1))) %>%
  left_join(DV_completeness, by =c("county_code", "site_number", "poc", "year")) %>%
  left_join(DV_samples) %>%
  left_join(site_metadata) %>%
  left_join(max_daily_values, by = c("county_code", "site_number", "poc", "year")) %>%
  mutate(`Complete Dataset?` = ifelse(DV_period_complete_quarters == 12, 
                                      "Yes", 
                                      "No"))

write_xlsx(DV_list, path = here("data_processed","DVs","ann_PM25_DVs.xlsx"))
