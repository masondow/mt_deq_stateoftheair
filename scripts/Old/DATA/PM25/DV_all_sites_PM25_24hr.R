library(dplyr)
library(lubridate)
library(stringr)
library(readxl)
library(RAQSAPI)
library(magrittr)
library(ggplot2)

##### AQS Data #####

daily_pm25 <- readRDS("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/DATA/PM25/2000_2024_daily_pm25_88101_88502_all_MT.rds")
hourly_pm25 <- readRDS("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/DATA/PM25/2000_2024_hourly_pm25_88101_88502_all_MT.rds")

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
flags <- hourly_pm25 %>%
  mutate(date_local = as.Date(date_local, format = "%Y-%m-%d")) %>%
  group_by(date_local, state_code, county_code, site_number, poc) %>%
  summarize(qualifier = paste(
    unique(na.omit(qualifier[qualifier %in% qualifiers])), 
    collapse = ", "
  )) %>%
  ungroup()

daily_pm25_flags <- daily_pm25 %>%
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
daily_pm25_flags <- daily_pm25_flags %>% 
  left_join(flags, by = c("date_local", "state_code", "county_code", "site_number", "poc"))

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
quarterly_completeness_summary <- daily_pm25_flags %>%
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


##### Design Values #####

annual_98th <- daily_pm25_flags %>%
  group_by(county_code, site_number, poc, year) %>%
  summarize(annual_98th_percentile = quantile(arithmetic_mean, 0.98, na.rm = TRUE)) %>%
  ungroup()

design_value <- annual_98th %>%
  group_by(county_code, site_number, poc) %>%
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

annual_98th_no_flags <- daily_pm25_flags %>%
  filter((is.na(qualifier) | qualifier == "")) %>%
  group_by(county_code, site_number, poc, year) %>%
  summarize(annual_98th_percentile_no_wildfire = quantile(arithmetic_mean, 0.98, na.rm = TRUE)) %>%
  ungroup()

design_value_no_flags <- annual_98th_no_flags %>%
  group_by(county_code, site_number, poc) %>%
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

DV_list <- left_join(design_value, design_value_no_flags) %>%
  mutate(across(c(annual_98th_percentile, annual_98th_percentile_no_wildfire, design_value, design_value_no_wildfire), ~ round(.x, 1))) %>%
  left_join(DV_completeness, by =c("county_code", "site_number", "poc", "year")) %>%
  left_join(daily_pm25_flags %>%
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
                                      "No"))

library(writexl)
write_xlsx(DV_list, path = "/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/DATA/PM25/2000_2024_all_sites_PM25_DVs_24hr.xlsx")

#### Map DVs ####

DV_list <- read_excel("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/DATA/PM25/2000_2024_all_sites_PM25_DVs_24hr.xlsx")

pollutant <- "PM25"
NAAQS <- 35
year <- 2024

# Filter for the most recent year per site
DV_list_filtered <- DV_list %>%
  mutate(pollutant = pollutant) %>%
  filter(year == !!year) %>%
  group_by(county_code, site_number) %>%
  arrange(poc != 3) %>%  # Prioritize POC 3
  slice(1) %>%  # Select the first (POC 3 if available)
  ungroup()

# Define a color palette based on design_value
pal <- colorNumeric(
  palette = "YlOrRd",  # Choose a color scale ("YlOrRd" is Yellow-Orange-Red)
  domain = DV_list_filtered$design_value,  # Define the range of values
  na.color = "lightgray"  # Color for NA values
)

library(leaflet)
leaflet(DV_list_filtered) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    popup = ~paste0("<b>Site Name:</b> ", local_site_name, "<br>",
                    "<b>County:</b> ", county, "<br>",
                    "<b>Pollutant (period):</b> ", pollutant, " (24-hour)<br>",
                    "<b>DV Form:</b> 98%-ile of 24-hr values (3-yr avg)<br>",
                    "<b>Year:</b> ", year, "<br>",
                    "<b>98th Percentile:</b> ", annual_98th_percentile, "<br>",
                    "<b>98th Percentile (No Wildfire):</b> ", annual_98th_percentile_no_wildfire, "<br>",
                    "<b>Design Value:</b> ", design_value, "<br>",
                    "<b>Design Value (No Wildfire):</b> ", design_value_no_wildfire, "<br>",
                    "<b>Complete Dataset?:</b> ", `Complete Dataset?`),
    radius = ~ifelse(is.na(design_value), 5,
                     ifelse(design_value > NAAQS, 9, 7)),
    color = ~ifelse(is.na(design_value), "gray",  # Gray border if NA
                    ifelse(design_value > NAAQS, "red", "black")),  # Red if >35, black otherwise
    weight = ~ifelse(is.na(design_value), 1,  # Thinner border if NA
                     ifelse(design_value > NAAQS, 4, 2)),  # Thicker border if >35
    opacity = 1,
    fillColor = ~ifelse(is.na(design_value), "lightgray", pal(design_value)),  # Light gray fill if NA
    fillOpacity = 1
  ) %>%
  addLegend(
    position = "bottomleft",
    pal = pal,
    values = DV_list_filtered$design_value,
    title = paste0(year, " DV"),
    opacity = 1
  )



