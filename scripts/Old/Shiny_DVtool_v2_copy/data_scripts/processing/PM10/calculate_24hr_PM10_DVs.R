
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
  filter(!(arithmetic_mean > 98 & !is.na(qualifier) & qualifier != ""))

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


#### Determine nth Rank for 1-yr and 3-yr look-up DV  ####

# count the number of days for 1-year and 3-year DV groupings
sample_counts <- daily_PM10_flags %>%
  count(site_name, local_site_name, county_code, site_number, poc, year, name = "sample_count") %>%
  full_join(
    daily_PM10_no_flags %>%
      count(county_code, site_number, poc, year, name = "sample_count_no_wildfire"),
    by = c("county_code", "site_number", "poc", "year")
  )

# Compute rolling 3-year sum, ensuring at least 3 years of data for LMP DV
sample_counts <- sample_counts %>%
  group_by(site_name, local_site_name, county_code, site_number, poc) %>%
  mutate(rolling_3yr_count = sapply(year, function(y) {
    # Get valid years in the 3-year window
    valid_years <- sum(year >= (y - 2) & year <= y)
    sum(sample_count[year >= (y - 2) & year <= y], na.rm = TRUE)
  }),
  rolling_3yr_count_no_wildfire = sapply(year, function(y) {
    valid_years <- sum(year >= (y - 2) & year <= y)
    sum(sample_count_no_wildfire[year >= (y - 2) & year <= y], na.rm = TRUE)
  })
  ) %>%
  ungroup()

# Define a function to determine rank based on sample count
get_rank_position <- function(count) {
  if (is.na(count)) {
    return(NA)  # Return NA if count is missing
  }
  case_when(
    count <= 347 ~ 1,
    count <= 695 ~ 2,
    count <= 1042 ~ 3,
    count <= 1390 ~ 4,
    count <= 1738 ~ 5,
    TRUE ~ 6 # Default case if count exceeds 1738
  )
}

# Compute rank positions
ranked_data <- sample_counts %>%
  group_by(site_name, local_site_name, county_code, site_number, poc, year) %>%
  mutate(
    # Determine rank position for 1-year and 5 year counts
    rank_3yr = get_rank_position(rolling_3yr_count),
    rank_3yr_no_wildfire = get_rank_position(rolling_3yr_count_no_wildfire)
  ) %>%
  ungroup()

#### Calculate 3-yr DVs (LMP look-up method) ####

get_3yr_DV <- function(daily_PM10_flags, ranked_data) {
  
  # Ensure ranked_data has only necessary columns
  ranked_data <- ranked_data %>%
    select(county_code, site_number, poc, year, rank_3yr, rolling_3yr_count)
  
  # Process each site separately
  result <- daily_PM10_flags %>%
    group_by(county_code, site_number, poc) %>%
    nest() %>%
    mutate(data = map(data, ~mutate(.x, 
                                    county_code = first(county_code),
                                    site_number = first(site_number),
                                    poc = first(poc)))) %>%  # ✅ Keep grouping vars in site_data
    left_join(ranked_data, by = c("county_code", "site_number", "poc")) %>%
    mutate(DV_3yr = map2(data, year, function(site_data, y) {
      
      # Ensure site_data is not empty
      if (nrow(site_data) == 0) return(NA)
      
      # Define the rolling 5-year window
      rolling_window <- site_data %>%
        filter(year >= (y - 2) & year <= y) %>%
        arrange(desc(arithmetic_mean))
      
      # Get rank_3yr for this site-year combination
      rank_info <- ranked_data %>%
        filter(county_code == first(site_data$county_code),
               site_number == first(site_data$site_number),
               poc == first(site_data$poc),
               year == y) %>%
        pull(rank_3yr) %>%
        first() 
      
      # Extract nth highest value if valid
      return(rolling_window$arithmetic_mean[rank_info])
    })) %>%
    unnest(cols = DV_3yr) %>%
    select(county_code, site_number, poc, year, rank_3yr, rolling_3yr_count, DV_3yr)
  
  return(result)
}
get_3yr_DV_no_wildfire <- function(daily_PM10_no_flags, ranked_data) {
  
  # Ensure ranked_data has only necessary columns
  ranked_data <- ranked_data %>%
    select(county_code, site_number, poc, year, rank_3yr)
  
  # Process each site separately
  result <- daily_PM10_no_flags %>%
    group_by(county_code, site_number, poc) %>%
    nest() %>%
    mutate(data = map(data, ~mutate(.x, 
                                    county_code = first(county_code),
                                    site_number = first(site_number),
                                    poc = first(poc)))) %>%  # ✅ Keep grouping vars in site_data
    left_join(ranked_data, by = c("county_code", "site_number", "poc")) %>%
    mutate(DV_3yr_no_wildfire = map2(data, year, function(site_data, y) {
      
      # Ensure site_data is not empty
      if (nrow(site_data) == 0) return(NA)
      
      # Define the rolling 3-year window
      rolling_window <- site_data %>%
        filter(year >= (y - 2) & year <= y) %>%
        arrange(desc(arithmetic_mean))
      
      # Get rank_3yr for this site-year combination
      rank_info <- ranked_data %>%
        filter(county_code == first(site_data$county_code),
               site_number == first(site_data$site_number),
               poc == first(site_data$poc),
               year == y) %>%
        pull(rank_3yr) %>% # using same rank as w/wildfire to avoid changing brackets (i.e. increasing the DV)
        first() 
      
      # Extract nth highest value if valid
      return(rolling_window$arithmetic_mean[rank_info])
    })) %>%
    unnest(cols = DV_3yr_no_wildfire) %>%
    select(county_code, site_number, poc, year, DV_3yr_no_wildfire)
  
  return(result)
}

# Run the function
ranked_3yr_values <- get_3yr_DV(daily_PM10_flags, ranked_data)
ranked_3yr_values_no_wildfire <- get_3yr_DV_no_wildfire(daily_PM10_no_flags, ranked_data)

#### LMP DV ####

LMP_DV <- ranked_3yr_values %>%
  group_by(county_code, site_number, poc) %>%
  arrange(year) %>%
  mutate(
  LMP_DV = slide_dbl(
    DV_3yr,
    .f = ~ {
      if (sum(!is.na(.x)) == 3) {
        mean(.x, na.rm = TRUE)
      } else {
        NA_real_
      }
    },
    .before = 2,
    .complete = TRUE
    )
  ) %>%
  ungroup()

LMP_DV_no_wildfire <- ranked_3yr_values_no_wildfire %>%
  group_by(county_code, site_number, poc) %>%
  arrange(year) %>%
  mutate(
    LMP_DV_no_wildfire = slide_dbl(
      DV_3yr_no_wildfire,
      .f = ~ {
        if (sum(!is.na(.x)) == 3) {
          mean(.x, na.rm = TRUE)
        } else {
          NA_real_
        }
      },
      .before = 2,
      .complete = TRUE
    )
  ) %>%
  ungroup()

#### LMP CDV ####

CDV <- LMP_DV %>%
  arrange(county_code, site_number, poc, year) %>%
  group_by(county_code, site_number, poc) %>%
  mutate(
    CDV_info = slide_index_dfr(
      .x = DV_3yr,
      .i = year,
      .f = ~{
        values <- .x[!is.na(.x)]
        n <- length(values)
        if (n >= 3) {
          mean_val <- mean(values)
          sd_val <- sd(values)
          cv <- sd_val / mean_val
          tc <- qt(0.90, df = n - 1)  # One-tailed, alpha = 0.10
          cdv <- 150 / (1 + tc * cv)
        } else {
          cdv <- NA_real_
        }
        tibble(
          CDV = cdv,
          CDV_DV_years = n
        )
      },
      .before = 10,
      .complete = FALSE
    )
  ) %>%
  unnest_wider(CDV_info) %>%
  ungroup()

CDV_no_wildfire <- LMP_DV_no_wildfire %>%
  arrange(county_code, site_number, poc, year) %>%
  group_by(county_code, site_number, poc) %>%
  mutate(
    CDV_info = slide_index_dfr(
      .x = DV_3yr_no_wildfire,
      .i = year,
      .f = ~{
        values <- .x[!is.na(.x)]
        n <- length(values)
        if (n >= 3) {
          mean_val <- mean(values)
          sd_val <- sd(values)
          cv <- sd_val / mean_val
          tc <- qt(0.90, df = n - 1)  # One-tailed, alpha = 0.10
          cdv <- 150 / (1 + tc * cv)
        } else {
          cdv <- NA_real_
        }
        tibble(
          CDV_no_wildfire = cdv,
          CDV_DV_years = n
        )
      },
      .before = 10,
      .complete = FALSE
    )
  ) %>%
  unnest_wider(CDV_info) %>%
  ungroup()

CDV <- left_join(CDV, CDV_no_wildfire)

#### Exceedances (NAAQS Design Value) ####

ex_1yr <- daily_PM10_flags %>%
  mutate(arithmetic_mean = round(arithmetic_mean / 10) * 10) %>%
  group_by(site_name, local_site_name, county_code, site_number, poc, year) %>%
  summarise(
    exceedances = sum(arithmetic_mean > 150, na.rm = TRUE),  # Count exceedances
    valid_days = sum(!is.na(arithmetic_mean)),  # Count valid days (non-missing)
    .groups = "drop"
  ) %>%
  mutate(
    est_exceedances = ifelse(valid_days > 0, (exceedances * 365) / valid_days, NA)
  )

# Compute exceedances without wildfire
ex_1yr_no_wildfire <- daily_PM10_no_flags %>%
  mutate(arithmetic_mean = round(arithmetic_mean / 10) * 10) %>%
  group_by(site_name, local_site_name, county_code, site_number, poc, year) %>%
  summarise(
    exceedances_no_wildfire = sum(arithmetic_mean > 150, na.rm = TRUE),  # Count exceedances
    .groups = "drop"
  ) %>%
  # Join with ex_1yr to get valid_days
  left_join(select(ex_1yr, county_code, site_number, poc, year, valid_days), 
            by = c("county_code", "site_number", "poc", "year")) %>%
  mutate(
    est_exceedances_no_wildfire = ifelse(valid_days > 0, (exceedances_no_wildfire * 365) / valid_days, NA)
  )


# Compute rolling 3-year estimated exceedances
ex_3yr <- ex_1yr %>%
  group_by(site_name, local_site_name, county_code, site_number, poc) %>%
  arrange(year) %>%
  mutate(DV_3yr_avg_exceedance = slide_dbl(
    est_exceedances, 
    ~ if(length(.x) == 3) mean(.x, na.rm = TRUE) else NA_real_,
    .before = 2,  # Include previous 2 years in window
    .complete = TRUE
  )) %>%
  mutate(DV_3yr_avg_exceedance = round(DV_3yr_avg_exceedance, 1)) %>%  # Round to the nearest tenth
  ungroup()

# Compute rolling 3-year estimated exceedances
ex_3yr_no_wildfire <- ex_1yr_no_wildfire %>%
  group_by(site_name, local_site_name, county_code, site_number, poc) %>%
  arrange(year) %>%
  mutate(DV_3yr_avg_exceedance_no_wildfire = slide_dbl(
    est_exceedances_no_wildfire, 
    ~ if(length(.x) == 3) mean(.x, na.rm = TRUE) else NA_real_,
    .before = 2,  # Include previous 2 years in window
    .complete = TRUE
  )) %>%
  mutate(DV_3yr_avg_exceedance_no_wildfire = round(DV_3yr_avg_exceedance_no_wildfire, 1)) %>%  # Round to the nearest tenth
  ungroup()

exceedances <- ex_1yr %>%
  left_join(ex_1yr_no_wildfire) %>%
  left_join(ex_3yr) %>%
  left_join(ex_3yr_no_wildfire) %>%
  select(-exceedances, -exceedances_no_wildfire)  # Drop the columns

##### Max Daily Values ####
max_daily_values <- daily_PM10 %>%
  group_by(county_code, site_number, poc, year) %>%
  summarize(max_daily_value = round(max(arithmetic_mean, na.rm = TRUE), 1), .groups = "drop")

##### Summaries #####
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

DV_list <- CDV %>%
  left_join(exceedances) %>%
  mutate(across(c(LMP_DV, LMP_DV_no_wildfire, CDV, CDV_no_wildfire), ~ round(.x, 1))) %>%
  left_join(DV_completeness, by =c("county_code", "site_number", "poc", "year")) %>%
  left_join(site_metadata) %>%
  left_join(max_daily_values, by = c("county_code", "site_number", "poc", "year")) %>%
  mutate(`Complete Dataset?` = ifelse(DV_period_complete_quarters == 12, 
                                      "Yes", 
                                      "No")) 


write_xlsx(DV_list, path = here("data_processed","DVs","24hr_PM10_DVs.xlsx"))





