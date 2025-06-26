
aqs_credentials(username = "Cameron.Nealy@mt.gov", key = "copperhawk98")

# Load necessary library
# Helps with date manipulation
# Define start and end dates
bdate <- as.Date("2000-01-01")  # Manually set start date
edate <- Sys.Date()  # Automatically set to today's date

PM10 <- 81102
PM25 <- c(88101, 88502)
parameters <- c(PM10, PM25)

# Iterate over each parameter and bind results
sites <- map_dfr(parameters, ~ aqs_monitors_by_state(
  parameter = .x,
  bdate = bdate,
  edate = edate,
  stateFIPS = "30"
))

# View results
print(sites)

# Read AirNow data
AirNow <- readRDS(here("data", "AirNow", "hourly_AirNow_all_MT.rds"))

# Extract unique active site information
AirNow_sites_active <- AirNow %>%
  distinct(county_code, site_number, site_name)

# Merge and update site_name if a new value exists
sites_updated <- sites %>%
  left_join(AirNow_sites_active)

# Bind any rows in AirNow_sites_active that *didn't* match (i.e., weren't already in sites)
# Identify unmatched rows
unmatched_airnow <- AirNow_sites_active %>%
  anti_join(sites, by = c("county_code", "site_number"))

# Combine the joined sites with unmatched AirNow rows
sites_combined <- bind_rows(sites_updated, unmatched_airnow) %>%
  distinct()


# Fill gaps in site_name from AirNow with AQS local_site_name (or city/address if local_site_name is NA)
sites <- sites_combined %>%
  distinct() %>%
  mutate(site_name = case_when(
    !is.na(site_name) ~ site_name,  # Keep existing site_name if present
    !is.na(local_site_name) ~ local_site_name,  # Use local_site_name if available
    !is.na(city_name) & !(city_name %in% c("Not in a city", "Not in a City")) ~ city_name,  # Use city if available and not "Not in a city"
    !is.na(address) ~ address,  # Otherwise, use address
    TRUE ~ NA_character_  # Default to NA if all options are missing
  ))

saveRDS(sites, here("data", "sites.rds"))
message("sites.RDS updated successfully.")



