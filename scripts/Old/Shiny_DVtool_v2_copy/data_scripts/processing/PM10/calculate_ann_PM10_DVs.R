
##### AQS/AirNow Data #####

daily_PM10 <- readRDS(here("data_processed","merged_daily","merged_daily_PM10.rds"))

##### Add Flags #####

# Define qualifiers (for exclusion later)
qualifiers <- c(
  "RT - Wildfire-U. S.",
  "RF - Fire - Canadian.",
  "IF - Fire - Canadian.",
  "IT - Wildfire-U. S.",
  "E - Forest Fire."
)

daily_PM10_flags <- daily_PM10 %>%
  filter(!is.na(poc))

daily_PM10_no_flags <- daily_PM10_flags %>%
  filter(is.na(qualifier) | qualifier == "")

##### Quarterly Completeness #####

daily_PM10_flags <- daily_PM10_flags %>%
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
quarterly_completeness_summary <- daily_PM10_flags %>%
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

DV_samples <- daily_PM10 %>%
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


#### Determine 3-yr annual average DVs ####

annual_avg <- daily_PM10_flags %>%
  group_by(site_name, local_site_name, county_code,
           site_number,
           poc,
           year) %>%
  summarize(annual_avg = mean(arithmetic_mean)) %>%
  ungroup()

# design value
DV_all <- annual_avg %>%
  group_by(site_name, local_site_name, county_code, site_number, poc) %>%
  arrange(year) %>%  # Ensure data is sorted
  mutate(design_value = sapply(year, function(y) {
    # Select values only in the range [y-2, y]
    values <- annual_avg[year >= (y - 2) & year <= y]
    
    # Set to NA if there are fewer than 3 years of data
    if(length(values) < 3) {
      return(NA)
    } else {
      return(mean(values, na.rm = TRUE))
    }
  })) %>%
  ungroup()

#### Determine 3-yr annual average DVs (NO WILDFIRE) ####

annual_avg_no_flags <- daily_PM10_flags %>%
  filter((is.na(qualifier) | qualifier == "")) %>%
  group_by(site_name, local_site_name, county_code,
           site_number,
           poc,
           year) %>%
  summarize(annual_avg_no_wildfire = mean(arithmetic_mean)) %>%
  ungroup()

# design value
DV_no_flags <- annual_avg_no_flags %>%
  group_by(site_name, local_site_name, county_code, site_number, poc) %>%
  arrange(year) %>%  # Ensure data is sorted
  mutate(design_value_no_wildfire = sapply(year, function(y) {
    # Select values only in the range [y-2, y]
    values <- annual_avg_no_wildfire[year >= (y - 2) & year <= y]
    
    # Set to NA if there are fewer than 3 years of data
    if(length(values) < 3) {
      return(NA)
    } else {
      return(mean(values, na.rm = TRUE))
    }
  })) %>%
  ungroup()

##### Max Daily Values ####
max_daily_values <- daily_PM10 %>%
  group_by(county_code, site_number, poc, year) %>%
  summarize(max_daily_value = round(max(arithmetic_mean, na.rm = TRUE), 1), .groups = "drop")

#### Merge and Summarize DVs ####
sites <- readRDS(here("data","sites.rds")) %>%
  filter(parameter_code %in% PM10_codes)

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

DV_list <- left_join(annual_avg, annual_avg_no_flags) %>%
  left_join(DV_all) %>%
  left_join(DV_no_flags) %>%
  mutate(across(c(annual_avg, annual_avg_no_wildfire, design_value, design_value_no_wildfire), ~ round(.x, 1))) %>%
  left_join(DV_completeness, by =c("county_code", "site_number", "poc", "year")) %>%
  left_join(site_metadata) %>%
  left_join(max_daily_values, by = c("county_code", "site_number", "poc", "year")) %>%
  mutate(`Complete Dataset?` = ifelse(DV_period_complete_quarters == 12, 
                                      "Yes", 
                                      "No"))

write_xlsx(DV_list, path = here("data_processed","DVs","ann_PM10_DVs.xlsx"))


