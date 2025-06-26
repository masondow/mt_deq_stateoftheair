# use this script to process entire available archived record; then use primary script that only processes and anti-joins last 2 years of data
#### WARNING: do not run unless available 3-yr AirNow archive is long enough to extend back to sites that last reported to AQS in March 2024 (see ex: PM2.5 Seeley Lake)
####          If not, run AirNow update to gap-fill, then run this initialize script
library(dplyr)
library(lubridate)
library(data.table)  

AirNow_parameter_name <- "PM10"

AirNow_hourly <- readRDS(here("data", "AirNow", "hourly_AirNow_all_MT.rds"))
AirNow_daily <- readRDS(here("data", "AirNow", "daily_AirNow_all_MT.rds"))
AQS_hourly_PM10 <- readRDS(here("data","PM10","hourly_PM10_all_MT.rds"))
AQS_daily_PM10 <- readRDS(here("data","PM10","daily_PM10_all_MT.rds"))
wf_flags_PM10 <- readRDS(here("data","PM10","wf_flags_PM10_all_MT.rds"))

site_names <- readRDS(here("data","sites.rds")) %>%
  select(site_name, local_site_name, county_code, site_number) %>%
  distinct()
AQS_pocs_PM10 <- readRDS(here("data", "PM10","AQS_PM10_pocs.rds"))


# Filter the AQS data to only include AQS primary pocs
AQS_hourly_PM10 <- AQS_hourly_PM10 %>%
  semi_join(AQS_pocs_PM10, by = c("county_code", "site_number", "poc"))
AQS_daily_PM10 <- AQS_daily_PM10 %>%
  semi_join(AQS_pocs_PM10, by = c("county_code", "site_number", "poc"))

#### Last Best POC ####
# use to apply to AirNow data
poc_values <- AQS_pocs_PM10 %>%
  group_by(county_code, site_number) %>%
  slice_max(order_by = year, n = 1, with_ties = FALSE) %>%
  ungroup()

poc_values <- poc_values %>%
  select(county_code, site_number, poc)

#### HOURLY MERGE ####

# AQS cleaning
AQS_hourly <- as.data.table(AQS_hourly_PM10) # Convert to data.table for faster processing
AQS_hourly[, source := "AQS"] # Add source column
AQS_hourly[, date_time := as.POSIXct(paste(date_local, time_local), format = "%Y-%m-%d %H:%M", tz = "UTC")] # Convert date_time efficiently while preserving HH:MM
AQS_hourly[, priority_flag := fifelse(sample_duration == "1 HOUR", 1, 2)] # Assign priority flag
setorder(AQS_hourly, date_local, time_local, county_code, site_number, poc, priority_flag) # Set key for fast sorting and filtering
AQS_hourly <- AQS_hourly[, .SD[1], by = .(date_local, time_local, county_code, site_number, poc)] # Select the highest priority row per group

# AirNow cleaning
AirNow_filtered <- AirNow_hourly %>%
  filter(parameter == AirNow_parameter_name) %>%
  select(-parameter) %>%
  mutate(
    source = "AirNow",
    date_gmt = as.character(date_gmt),  # Convert POSIXct to character first
    time_gmt = substr(date_gmt, 12, 16), # Extract HH:MM
    date_gmt = substr(date_gmt, 1, 10)  # Extract YYYY-MM-DD
  ) %>%
  rename(date_time = adjusted_time)

AirNow_with_poc <- AirNow_filtered %>%
  left_join(poc_values, by = c("county_code", "site_number")) %>%
  mutate(poc = ifelse(is.na(poc), 999, poc))

# Merge AirNow_with_poc and AQS_hourly
common_columns <- intersect(colnames(AirNow_with_poc), colnames(AQS_hourly))
print(common_columns)

AirNow_with_poc_dt <- as.data.table(AirNow_with_poc) # Convert data frames to data.table for speed
AQS_hourly_dt <- as.data.table(AQS_hourly)

AirNow_with_poc_dt <- AirNow_with_poc_dt[, ..common_columns] # Select common columns while ensuring missing columns in AirNow_filtered get NA
AQS_hourly_dt <- AQS_hourly_dt[, c(common_columns, "sample_duration", "qualifier"), with = FALSE]

merged_dt <- rbindlist(list(AirNow_with_poc_dt, AQS_hourly_dt), fill = TRUE) # Combine the datasets
merged_dt[, date_local := as.Date(date_local, format = "%Y-%m-%d")]
merged_dt[, year := year(date_local)] # Compute time-related variables separately
merged_dt[, month := month(date_local)]
merged_dt[, quarter := fcase( 
  month %in% 1:3, 1,
  month %in% 4:6, 2,
  month %in% 7:9, 3,
  month %in% 10:12, 4
)] 

# Merged cleaning
merged_dt[, priority_flag := fcase( 
  source == "AQS" & !is.na(sample_measurement), 1,
  source == "AirNow" & is.na(sample_duration), 2,
  default = 3
)] 
setorder(merged_dt, county_code, site_number, date_local, poc, time_local, priority_flag) # Fast filtering: order by priority_flag within groups and keep first row
merged_dt <- merged_dt[, .SD[1], by = .(county_code, site_number, date_local, poc, time_local)]
merged <- as.data.frame(merged_dt) %>% # Convert back to data.frame if needed
  select(-priority_flag)

# Add site metadata
merged <- merged %>%
  left_join(site_names, by = c("county_code", "site_number")) 

saveRDS(merged, here("data_processed","merged_hourly", "merged_hourly_PM10.rds"))


#### DAILY MERGE ####

# AQS cleaning
AQS_daily <- AQS_daily_PM10 %>%
  filter(validity_indicator == "Y") %>%
  left_join(wf_flags_PM10 %>%
              mutate(date_local = format(date_local, "%Y-%m-%d"))) %>%
  mutate(source = "AQS",
         date_time = as.POSIXct(date_local, format = "%Y-%m-%d", tz = "UTC")) 

AQS_daily <- as.data.table(AQS_daily) # Convert to data.table for faster processing
AQS_daily[, priority_flag := fifelse(sample_duration == "1 HOUR", 1, 2)] # Assign priority flag
setorder(AQS_daily, date_local, county_code, site_number, poc, priority_flag) # Set key for fast sorting and filtering
AQS_daily <- AQS_daily[, .SD[1], by = .(date_local, county_code, site_number, poc)] # Select the highest priority row per group

# Step 2: clean AirNow_daily data
AirNow_filtered <- AirNow_daily %>%
  filter(parameter == AirNow_parameter_name) %>%
  select(-parameter) %>%
  mutate(
    source = "AirNow",
    date_time = as.POSIXct(date_local, format = "%Y-%m-%d", tz = "UTC")
  )

AirNow_with_poc <- AirNow_filtered %>%
  left_join(poc_values, by = c("county_code", "site_number")) %>%
  mutate(poc = ifelse(is.na(poc), 999, poc))

# Merge AirNow_with_poc and AQS_daily
common_columns <- intersect(colnames(AirNow_with_poc), colnames(AQS_daily))
print(common_columns)

AirNow_with_poc_dt <- as.data.table(AirNow_with_poc) # Convert data frames to data.table for speed
AQS_daily_dt <- as.data.table(AQS_daily)

AirNow_with_poc_dt <- AirNow_with_poc_dt[, ..common_columns] # Select common columns while ensuring missing columns in AirNow_filtered get NA
AQS_daily_dt <- AQS_daily_dt[, c(common_columns, "sample_duration", "qualifier"), with = FALSE]

merged_dt <- rbindlist(list(AirNow_with_poc_dt, AQS_daily_dt), fill = TRUE) # Combine the datasets
merged_dt[, date_local := as.Date(date_local, format = "%Y-%m-%d")]
merged_dt[, year := year(date_local)] # Compute time-related variables separately
merged_dt[, month := month(date_local)]
merged_dt[, quarter := fcase( 
  month %in% 1:3, 1,
  month %in% 4:6, 2,
  month %in% 7:9, 3,
  month %in% 10:12, 4
)] 

# Merged cleaning
merged_dt[, priority_flag := fcase( 
  source == "AQS" & !is.na(arithmetic_mean), 1,
  source == "AirNow" & is.na(sample_duration), 2,
  default = 3
)] 
setorder(merged_dt, county_code, site_number, date_local, poc, priority_flag) # Fast filtering: order by priority_flag within groups and keep first row
merged_dt <- merged_dt[, .SD[1], by = .(county_code, site_number, date_local, poc)]
merged <- as.data.frame(merged_dt) %>% # Convert back to data.frame if needed
  select(-priority_flag)

# Add sample_frequency column for DV completeness calculations (NEXT STEP in the script order)
sample_frequency <- AQS_hourly %>%
  select(county_code, site_number, poc, date_local, sample_frequency) %>%
  mutate(date_local = as.Date(date_local)) %>%
  distinct()

merged <- merged %>%
  left_join(sample_frequency) %>%
  left_join(site_names, by = c("county_code", "site_number")) 

saveRDS(merged, here("data_processed","merged_daily", "merged_daily_PM10.rds"))








