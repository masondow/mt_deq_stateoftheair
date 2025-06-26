# DV calculator functions
# load necessary inputs
get_sites <- function(pollutant, avg_period, year_filter) {
  current_year <- as.numeric(year_filter)
  period_filter <- seq(current_year - 2, current_year)
  period_filter_PM10_LMP <- seq(current_year - 4, current_year)
  
  if (pollutant == "PM25" && avg_period == "24hr_PM10_LMP") {
    # Explicitly return NULL if PM25 and 24hr_PM10_LMP are selected
    return(NULL)
  } else if (pollutant == "PM25") {
    sites <- readRDS(here("data_processed", "site_input_map", "PM25_sites_filtered.rds")) %>%
      filter(year %in% period_filter) %>%
      select(site_name, local_site_name, county_code, site_number, poc) %>%
      distinct()
  } else if (pollutant == "PM10" && avg_period == "24hr_PM10_LMP") {
    sites <- readRDS(here("data_processed", "site_input_map", "PM10_sites_filtered.rds")) %>%
      filter(year %in% period_filter_PM10_LMP) %>%
      select(site_name, local_site_name, county_code, site_number, poc) %>%
      distinct()
  } else if (avg_period == "24hr_PM10_LMP") {
    # If the pollutant is not "PM10" and the avg_period is "24hr_PM10_LMP", return NULL
    return(NULL)
  } else if (pollutant == "PM10") {
    sites <- readRDS(here("data_processed", "site_input_map", "PM10_sites_filtered.rds")) %>%
      filter(year %in% period_filter) %>%
      select(site_name, local_site_name, county_code, site_number, poc) %>%
      distinct()
  }
  
  # Sort sites alphabetically and place numeric site names at the end
  sites <- sites %>%
    mutate(is_numeric = grepl("^[0-9]", site_name)) %>%  # Create a column to identify numeric site names
    arrange(site_name, is_numeric) %>%  # First alphabetically, then numeric at the end
    select(-is_numeric)  # Drop the temporary column used for sorting
  
  return(sites)
}

get_filtered_data <- function(pollutant, avg_period, site_filter, year_filter) {
  current_year <- as.numeric(year_filter)
  period_filter <- seq(current_year - 2, current_year)
  period_filter_PM10_LMP <- seq(current_year - 4, current_year)
  # Load daily data
  daily_PM25 <- readRDS(here("data_processed", "merged_daily", "merged_daily_PM25.rds"))
  daily_PM10 <- readRDS(here("data_processed", "merged_daily", "merged_daily_PM10.rds"))
  
  qualifiers <- c("RT - Wildfire-U. S.", "RF - Fire - Canadian.",
                  "IF - Fire - Canadian.", "IT - Wildfire-U. S.", "E - Forest Fire.")
  
  # Filter daily data by site_filter (site names)
  if (pollutant == "PM25") {
    daily_data <- daily_PM25 %>% filter(site_name %in% site_filter,
                                        year %in% period_filter)
    wf_data <- daily_data %>% filter(qualifier %in% qualifiers,
                                     year %in% period_filter)
  } else if (pollutant == "PM10" && avg_period == "24hr_PM10_LMP") {
    daily_data <- daily_PM10 %>% filter(site_name %in% site_filter,
                                        year %in% period_filter_PM10_LMP)
    wf_data <- daily_data %>% filter(qualifier %in% qualifiers,
                                     year %in% period_filter_PM10_LMP)
  } else {
    daily_data <- daily_PM10 %>% filter(site_name %in% site_filter,
                                        year %in% period_filter)
    wf_data <- daily_data %>% filter(qualifier %in% qualifiers,
                                     year %in% period_filter)
  }
  
  return(list(
    daily_data = daily_data,
    wf_data = wf_data
  ))
}

#test result
#result <- get_filtered_data("PM25", "ann", "Frenchtown", 2023)
#daily_data <- result$daily_data
#wf_data <- result$wf_data
#sites <- result$sites

##################

calculate_DV <- function(pollutant, avg_period, year_filter, daily_data) {
  # necessary inputs
  pollutant_codes <- readRDS(here("data", "pollutant_codes.rds"))
  PM10_codes <- unlist(pollutant_codes$Parameter_Code[pollutant_codes$Pollutant == "PM10"])
  PM25_codes <- unlist(pollutant_codes$Parameter_Code[pollutant_codes$Pollutant == "PM25"])
  
  if (pollutant == "PM25" && avg_period == "ann") {
    ##### Add Flags #####
    
    daily_PM25_flags <- daily_data %>%
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
    
    DV_samples <- daily_data %>%
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
    
    ##### Summaries #####
    DV_result <- left_join(PM25_DV_all, PM25_DV_no_flags) %>%
      mutate(across(c(annual_average, annual_average_no_flags, design_value, design_value_no_wildfire), ~ round(.x, 2))) %>%
      left_join(DV_completeness, by =c("county_code", "site_number", "poc", "year")) %>%
      left_join(DV_samples) %>%
      mutate(`Complete Dataset?` = ifelse(DV_period_complete_quarters == 12, 
                                          "Yes", 
                                          "No")) %>%
      filter(year == year_filter) %>%
      select(design_value)
  }
  if (pollutant == "PM25" && avg_period == "24hr") {
    daily_PM25_flags <- daily_data %>%
      filter(!is.na(poc))
    
    daily_PM25_no_flags <- daily_PM25_flags %>%
      filter(is.na(qualifier) | qualifier == "")
    
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
    
    DV_samples <- daily_data %>%
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
    
    annual_98th <- daily_PM25_flags %>%
      group_by(site_name, local_site_name, county_code, site_number, poc, year) %>%
      summarize(annual_98th_percentile = quantile(arithmetic_mean, 0.98, na.rm = TRUE)) %>%
      ungroup()
    
    design_value <- annual_98th %>%
      group_by(site_name, local_site_name, county_code, site_number, poc) %>%
      mutate(design_value = sapply(year, function(y) {
        # Select values only in the range [y-2, y]
        values <- annual_98th_percentile[year >= (y - 2) & year <= y]
        
        # Set to NA if there are fewer than 3 years of data
        if(length(values) < 3) {
          return(NA)
        } else {
          return(mean(values, na.rm = TRUE))
        }
      })) %>%
      ungroup()
    
    ##### Design Values #####
    
    annual_98th_no_flags <- daily_PM25_flags %>%
      filter((is.na(qualifier) | qualifier == "")) %>%
      group_by(site_name, local_site_name, county_code, site_number, poc, year) %>%
      summarize(annual_98th_percentile_no_wildfire = quantile(arithmetic_mean, 0.98, na.rm = TRUE)) %>%
      ungroup()
    
    design_value_no_flags <- annual_98th_no_flags %>%
      group_by(site_name, local_site_name, county_code, site_number, poc) %>%
      mutate(design_value_no_wildfire = sapply(year, function(y) {
        # Select values only in the range [y-2, y]
        values <- annual_98th_percentile_no_wildfire[year >= (y - 2) & year <= y]
        
        # Set to NA if there are fewer than 3 years of data
        if(length(values) < 3) {
          return(NA)
        } else {
          return(mean(values, na.rm = TRUE))
        }
      })) %>%
      ungroup()
    
    ##### Summaries #####
    DV_result <- left_join(design_value, design_value_no_flags) %>%
      mutate(across(c(annual_98th_percentile, annual_98th_percentile_no_wildfire, design_value, design_value_no_wildfire), ~ round(.x, 2))) %>%
      left_join(DV_completeness, by =c("county_code", "site_number", "poc", "year")) %>%
      mutate(`Complete Dataset?` = ifelse(DV_period_complete_quarters == 12, 
                                          "Yes", 
                                          "No")) %>%
      filter(year == year_filter) %>%
      select(design_value)
  }
  if (pollutant == "PM10" && avg_period == "ann") {
    daily_PM10_flags <- daily_data %>%
      filter(!is.na(poc))
    
    daily_PM10_no_flags <- daily_data %>%
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
    
    DV_samples <- daily_data %>%
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
    
    #### Merge and Summarize DVs ####
    DV_result <- left_join(annual_avg, annual_avg_no_flags) %>%
      left_join(DV_all) %>%
      left_join(DV_no_flags) %>%
      mutate(across(c(annual_avg, annual_avg_no_wildfire, design_value, design_value_no_wildfire), ~ round(.x, 1))) %>%
      left_join(DV_completeness, by =c("county_code", "site_number", "poc", "year")) %>%
      mutate(`Complete Dataset?` = ifelse(DV_period_complete_quarters == 12, 
                                          "Yes", 
                                          "No")) %>%
      filter(year == year_filter) %>%
      select(design_value)
  }
  if (pollutant == "PM10" && avg_period == "24hr") {
    daily_PM10_flags <- daily_data %>%
      filter(!is.na(poc))
    
    daily_PM10_no_flags <- daily_PM10_flags %>%
      filter(is.na(qualifier) | qualifier == "")
    
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
    
    # Compute rolling 3-year estimated exceedances (no wildfire)
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
    
    ##### Summaries #####
    
    DV_result <- exceedances %>%
      filter(year == year_filter) %>%
      rename(design_value = DV_3yr_avg_exceedance) %>%
      select(design_value)
  }
  if (pollutant == "PM10" && avg_period == "24hr_PM10_LMP") {
    daily_PM10_flags <- daily_data %>%
      filter(!is.na(poc))
    
    daily_PM10_no_flags <- daily_PM10_flags %>%
      filter(is.na(qualifier) | qualifier == "")
    
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
        
        # If there are at least 5 years of data, compute sum; otherwise, set NA
        if (valid_years == 3) {
          sum(sample_count[year >= (y - 2) & year <= y], na.rm = TRUE)
        } else {
          NA
        }
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
        rank_1yr = get_rank_position(sample_count),
        rank_3yr = get_rank_position(rolling_3yr_count),
      ) %>%
      ungroup()
    
    #### Calculate 1-yr DVs ####
    
    # Compute CDV_1yr from daily_PM10_flags
    ranked_1yr_values <- daily_PM10_flags %>%
      group_by(site_name, local_site_name, county_code, site_number, poc, year) %>%
      arrange(desc(arithmetic_mean)) %>%  
      mutate(row_number = row_number()) %>%
      left_join(ranked_data) %>%
      filter(row_number == rank_1yr) %>%
      select(site_name, local_site_name, county_code, site_number, poc, year, CDV_1yr = arithmetic_mean)
    
    #### Calculate 3-yr DVs (LMP CDV) ####
    
    get_3yr_DV <- function(daily_PM10_flags, ranked_data) {
      
      # Ensure ranked_data has only necessary columns
      ranked_data <- ranked_data %>%
        select(county_code, site_number, poc, year, rank_3yr)
      
      # Process each site separately
      result <- daily_PM10_flags %>%
        group_by(county_code, site_number, poc) %>%
        nest() %>%
        mutate(data = map(data, ~mutate(.x, 
                                        county_code = first(county_code),
                                        site_number = first(site_number),
                                        poc = first(poc)))) %>%  # ✅ Keep grouping vars in site_data
        left_join(ranked_data, by = c("county_code", "site_number", "poc")) %>%
        mutate(CDV_3yr = map2(data, year, function(site_data, y) {
          
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
          if (!is.na(rank_info) && rank_info > 0 && rank_info <= nrow(rolling_window)) {
            return(rolling_window$arithmetic_mean[rank_info])
          } else {
            return(NA)
          }
        })) %>%
        unnest(cols = CDV_3yr) %>%
        select(county_code, site_number, poc, year, CDV_3yr)
      
      return(result)
    }
    
    # Run the function
    ranked_3yr_values <- get_3yr_DV(daily_PM10_flags, ranked_data)

    #### LMP DV ####
    
    LMP_DV <- ranked_3yr_values %>%
      group_by(county_code, site_number, poc) %>%
      arrange(year) %>%
      mutate(
        LMP_DV = slide_dbl(
          CDV_3yr,
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
    
    # Summary
    DV_result <- LMP_DV %>%
      rename(design_value = LMP_DV) %>%
      filter(year == year_filter) %>%
      ungroup() %>%
      select(design_value) 
  }
  return(DV_result)
}

#test result
#test <- calculate_DV("PM25", "ann", 2023)



