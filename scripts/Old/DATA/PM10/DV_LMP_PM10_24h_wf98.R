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

# Remove wildfire days (only removing >98 wildfire flags per LMP guidance)
daily_pm10_no_flags <- daily_pm10_flags %>%
  filter(is.na(qualifier) | qualifier == "" | arithmetic_mean < 98)

##### Quarterly Completeness #####

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
quarterly_completeness_summary <- daily_pm10_flags %>%
  group_by(county_code, site_number, poc, year, quarter) %>%
  summarise(
    data_days = n_distinct(date_local),  # Count unique days with data
    total_days = get_total_days(first(year), first(quarter)),  # Get total days for each quarter
    quarter_completeness = data_days / total_days * 100  # Calculate completeness percentage
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

library(zoo)
DV_completeness <- annual_completeness %>%
  group_by(county_code, site_number, poc) %>%
  arrange(year) %>%  # Ensure data is sorted
  mutate(DV_period_complete_quarters = sapply(year, function(y) {
    # Select only years in the range [y-2, y]
    sum(complete_quarters[year >= (y - 2) & year <= y], na.rm = TRUE)
  })) %>%
  ungroup()

#### Determine nth Rank for 1-yr and 3-yr DV ####

# count the number of days for 1-year and 3-year DV groupings
sample_counts <- daily_pm10_flags %>%
  count(county_code, site_number, poc, year, name = "sample_count") %>%
  full_join(
    daily_pm10_no_flags %>%
      count(county_code, site_number, poc, year, name = "sample_count_no_wildfire"),
    by = c("county_code", "site_number", "poc", "year")
  )

# Compute rolling 3-year sum, ensuring at least 5 years of data
sample_counts <- sample_counts %>%
  group_by(county_code, site_number, poc) %>%
  mutate(rolling_5yr_count = sapply(year, function(y) {
    # Get valid years in the 3-year window
    valid_years <- sum(year >= (y - 4) & year <= y)
    
    # If there are at least 3 years of data, compute sum; otherwise, set NA
    if (valid_years == 5) {
      sum(sample_count[year >= (y - 4) & year <= y], na.rm = TRUE)
    } else {
      NA
    }
  }),
  rolling_5yr_count_no_wildfire = sapply(year, function(y) {
    valid_years <- sum(year >= (y - 4) & year <= y)
    if (valid_years == 5) {
      sum(sample_count_no_wildfire[year >= (y - 4) & year <= y], na.rm = TRUE)
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
  group_by(county_code, site_number, poc, year) %>%
  mutate(
    # Determine rank position for 1-year and 3-year counts
    rank_1yr = get_rank_position(sample_count),
    rank_1yr_no_wildfire = get_rank_position(sample_count_no_wildfire),
    rank_5yr = get_rank_position(rolling_5yr_count),
    rank_5yr_no_wildfire = get_rank_position(rolling_5yr_count_no_wildfire)
  ) %>%
  ungroup()

#### Calculate 1-yr DVs ####

# Compute CDV_1yr from daily_pm10_flags
ranked_1yr_values <- daily_pm10_flags %>%
  group_by(county_code, site_number, poc, year) %>%
  arrange(desc(arithmetic_mean)) %>%  
  mutate(row_number = row_number()) %>%
  left_join(ranked_data, by = c("county_code", "site_number", "poc", "year")) %>%
  filter(row_number == rank_1yr) %>%
  select(county_code, site_number, poc, year, CDV_1yr = arithmetic_mean)

# Compute CDV_1yr_no_wildfire from daily_pm10_no_flags
ranked_1yr_values_no_wildfire <- daily_pm10_no_flags %>%
  group_by(county_code, site_number, poc, year) %>%
  arrange(desc(arithmetic_mean)) %>%
  mutate(row_number = row_number()) %>%
  left_join(ranked_data, by = c("county_code", "site_number", "poc", "year")) %>%
  filter(row_number == rank_1yr_no_wildfire) %>%
  select(county_code, site_number, poc, year, CDV_1yr_no_wildfire = arithmetic_mean)

# Merge both datasets
ranked_1yr_values <- full_join(ranked_1yr_values, ranked_1yr_values_no_wildfire, 
                               by = c("county_code", "site_number", "poc", "year"))

#### Calculate 3-yr DVs ####

library(tidyr)
library(purrr)

get_5yr_DV <- function(daily_pm10_flags, ranked_data) {
  
  # Ensure ranked_data has only necessary columns
  ranked_data <- ranked_data %>%
    select(county_code, site_number, poc, year, rank_5yr)
  
  # Process each site separately
  result <- daily_pm10_flags %>%
    group_by(county_code, site_number, poc) %>%
    nest() %>%
    mutate(data = map(data, ~mutate(.x, 
                                    county_code = first(county_code),
                                    site_number = first(site_number),
                                    poc = first(poc)))) %>%  # ✅ Keep grouping vars in site_data
    left_join(ranked_data, by = c("county_code", "site_number", "poc")) %>%
    mutate(CDV_5yr = map2(data, year, function(site_data, y) {
      
      # Ensure site_data is not empty
      if (nrow(site_data) == 0) return(NA)
      
      # Define the rolling 5-year window
      rolling_window <- site_data %>%
        filter(year >= (y - 4) & year <= y) %>%
        arrange(desc(arithmetic_mean))
      
      # Get rank_5yr for this site-year combination
      rank_info <- ranked_data %>%
        filter(county_code == first(site_data$county_code),
               site_number == first(site_data$site_number),
               poc == first(site_data$poc),
               year == y) %>%
        pull(rank_5yr) %>%
        first() 
      
      # Extract nth highest value if valid
      if (!is.na(rank_info) && rank_info > 0 && rank_info <= nrow(rolling_window)) {
        return(rolling_window$arithmetic_mean[rank_info])
      } else {
        return(NA)
      }
    })) %>%
    unnest(cols = CDV_5yr) %>%
    select(county_code, site_number, poc, year, CDV_5yr)
  
  return(result)
}
get_5yr_DV_no_wildfire <- function(daily_pm10_no_flags, ranked_data) {
  
  # Ensure ranked_data has only necessary columns
  ranked_data <- ranked_data %>%
    select(county_code, site_number, poc, year, rank_5yr_no_wildfire)
  
  # Process each site separately
  result <- daily_pm10_no_flags %>%
    group_by(county_code, site_number, poc) %>%
    nest() %>%
    mutate(data = map(data, ~mutate(.x, 
                                    county_code = first(county_code),
                                    site_number = first(site_number),
                                    poc = first(poc)))) %>%  # ✅ Keep grouping vars in site_data
    left_join(ranked_data, by = c("county_code", "site_number", "poc")) %>%
    mutate(CDV_5yr_no_wildfire = map2(data, year, function(site_data, y) {
      
      # Ensure site_data is not empty
      if (nrow(site_data) == 0) return(NA)
      
      # Define the rolling 5-year window
      rolling_window <- site_data %>%
        filter(year >= (y - 4) & year <= y) %>%
        arrange(desc(arithmetic_mean))
      
      # Get rank_3yr for this site-year combination
      rank_info <- ranked_data %>%
        filter(county_code == first(site_data$county_code),
               site_number == first(site_data$site_number),
               poc == first(site_data$poc),
               year == y) %>%
        pull(rank_5yr_no_wildfire) %>%
        first() 
      
      # Extract nth highest value if valid
      if (!is.na(rank_info) && rank_info > 0 && rank_info <= nrow(rolling_window)) {
        return(rolling_window$arithmetic_mean[rank_info])
      } else {
        return(NA)
      }
    })) %>%
    unnest(cols = CDV_5yr_no_wildfire) %>%
    select(county_code, site_number, poc, year, CDV_5yr_no_wildfire)
  
  return(result)
}

# Run the function
ranked_5yr_values <- get_5yr_DV(daily_pm10_flags, ranked_data)
ranked_5yr_values_no_wildfire <- get_5yr_DV_no_wildfire(daily_pm10_no_flags, ranked_data)

#### Exceedances (NAAQS Design Value) ####

ex_1yr <- daily_pm10_flags %>%
  group_by(county_code, site_number, poc, year) %>%
  summarise(
    exceedances = sum(arithmetic_mean > 150, na.rm = TRUE),  # Count exceedances
    valid_days = sum(!is.na(arithmetic_mean)),  # Count valid days (non-missing)
    .groups = "drop"
  ) %>%
  mutate(
    est_exceedances = ifelse(valid_days > 0, (exceedances * 365) / valid_days, NA)
  )

# Compute exceedances without wildfire
ex_1yr_no_wildfire <- daily_pm10_no_flags %>%
  group_by(county_code, site_number, poc, year) %>%
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

library(slider)
# Compute rolling 3-year estimated exceedances
ex_3yr <- ex_1yr %>%
  group_by(county_code, site_number, poc) %>%
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
  group_by(county_code, site_number, poc) %>%
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
  

DV_list <- left_join(ranked_5yr_values, ranked_5yr_values_no_wildfire) %>%
  left_join(ranked_1yr_values) %>%
  left_join(ranked_1yr_values_no_wildfire) %>%
  left_join(exceedances) %>%
  mutate(across(c(CDV_5yr, CDV_5yr_no_wildfire, CDV_1yr, CDV_1yr_no_wildfire), ~ round(.x, 1))) %>%
  left_join(DV_completeness, by =c("county_code", "site_number", "poc", "year")) %>%
  left_join(daily_pm10_flags %>%
              select(county_code, site_number, poc, local_site_name, county, city, site_address, latitude, longitude) %>%
              distinct(), 
            by = c("county_code", "site_number", "poc")) %>%
  mutate(local_site_name = case_when(
    !is.na(local_site_name) ~ local_site_name,  # Keep existing local_site_name if present
    !is.na(city) & city != "Not in a city" ~ city,  # Fill with city if available and not "Not in a city"
    !is.na(site_address) ~ site_address,  # Otherwise, fill with site_address
    TRUE ~ NA_character_  # Default to NA if all options are missing
  )) %>%
  mutate(`Complete Dataset?` = ifelse(DV_period_complete_quarters == 12, 
                                      "Yes", 
                                      "No")) %>%
  select(local_site_name,
         county_code,
         site_number,
         poc,
         year,
         CDV_1yr,
         CDV_1yr_no_wildfire,
         CDV_5yr,
         CDV_5yr_no_wildfire,
         valid_days,
         est_exceedances,
         est_exceedances_no_wildfire,
         DV_3yr_avg_exceedance,
         DV_3yr_avg_exceedance_no_wildfire,
         city,
         county,
         latitude,
         longitude,
         site_address,
         DV_period_complete_quarters,
         `Complete Dataset?`
         )

library(writexl)
# Write data frame to an Excel file
write_xlsx(DV_list, "/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/DATA/PM10/2000_2024_5yr_PM10_DVs_24h_wf98.xlsx")

#### Map DVs ####
library(leaflet)
DV_list <- read_excel("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/DATA/PM10/2000_2024_5yr_PM10_DVs_24h_wf98.xlsx")

pollutant <- "PM10"
CDV <- 98
year <- 2024

# Filter for the most recent year per site
DV_list_filtered <- DV_list %>%
  mutate(pollutant = pollutant) %>%
  filter(year == !!year)

# Define a color palette based on design_value
pal <- colorNumeric(
  palette = "YlOrRd",  # Choose a color scale ("YlOrRd" is Yellow-Orange-Red)
  domain = DV_list_filtered$CDV_5yr,  # Define the range of values
  na.color = "lightgray"  # Color for NA values
)

leaflet(DV_list_filtered) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    popup = ~paste0("<b>Site Name:</b> ", local_site_name, "<br>",
                    "<b>County:</b> ", county, "<br>",
                    "<b>Pollutant (period):</b> ", pollutant, " (24-hour)<br>",
                    "<b>CDV Form:</b> 5-yr n-th highest (look-up method)<br>",
                    "<b>DV Form:</b> 3-yr avg. ann. exceedances<br>",
                    "<b>Year:</b> ", year, "<br>",
                    "<b>CDV:</b> ", CDV_5yr, "<br>",
                    "<b>CDV (No Wildfire):</b> ", CDV_5yr_no_wildfire, "<br>",
                    "<b>DV:</b> ", DV_3yr_avg_exceedance, "<br>",
                    "<b>DV (No Wildfire):</b> ", DV_3yr_avg_exceedance_no_wildfire, "<br>",
                    "<b>Complete Dataset?:</b> ", `Complete Dataset?`),
    radius = ~ifelse(is.na(CDV_5yr), 5,
                     ifelse(CDV_5yr > CDV, 9, 7)),
    color = ~ifelse(is.na(CDV_5yr), "gray",  # Gray border if NA
                    ifelse(CDV_5yr > CDV, "red", "black")),  # Red if >35, black otherwise
    weight = ~ifelse(is.na(CDV_5yr), 1,  # Thinner border if NA
                     ifelse(CDV_5yr > CDV, 4, 2)),  # Thicker border if >35
    opacity = 1,
    fillColor = ~ifelse(is.na(CDV_5yr), "lightgray", pal(CDV_5yr)),  # Light gray fill if NA
    fillOpacity = 1
  ) %>%
  addLegend(
    position = "bottomleft",
    pal = pal,
    values = DV_list_filtered$CDV_5yr,
    title = paste0(year, " CDV"),
    opacity = 1
  )



