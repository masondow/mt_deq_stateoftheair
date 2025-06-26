# Identify AQS pocs

AQS_daily_PM10 <- readRDS(here("data","PM10","daily_PM10_all_MT.rds")) %>%
  filter(validity_indicator == "Y") %>%
  mutate(
    date_local = as.Date(date_local, format = "%Y-%m-%d"),
    year = year(date_local),       # Extract year from date_local
    month = month(date_local),     # Extract month from date_local
    quarter = case_when(           # Calculate quarter based on month
      month %in% 1:3 ~ 1,
      month %in% 4:6 ~ 2,
      month %in% 7:9 ~ 3,
      month %in% 10:12 ~ 4
    )) 

samples <- AQS_daily_PM10 %>%
  group_by(county_code, site_number, poc, date_local) %>%
  slice(1) %>% #selects only 1 daily average (multiple are listed for each different NAAQS standard)
  ungroup() %>%
  group_by(county_code, site_number, poc, year) %>%
  summarize(annual_samples = sum(!is.na(arithmetic_mean))) %>%
  ungroup() %>%
  filter(year <= year(Sys.Date()) - 1)

# Calculate rolling 3-year period count for each site_name and poc
samples <- samples %>%
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

# Read sites and filter by parameter_code
sites <- readRDS(here("data","sites.rds")) %>%
  filter(parameter_code %in% PM10_codes) %>%
  select(site_name, county_code, site_number, poc, parameter_code, open_date, close_date, naaqs_primary_monitor)

# Merge site history (changes in parameter code or NAAQS primary monitor) by poc; ensures unique site-poc combos
collapsed_sites <- sites %>%
  group_by(site_name, county_code, site_number, poc) %>%
  summarise(
    # Combine unique parameter codes into a sorted, comma-separated string
    parameter_code = paste(
      parameter_code[order(open_date)], collapse = ", "),
    # Paste all unique open dates in chronological order
    open_date = paste(sort(unique(open_date)), collapse = ", "),
    # Paste all unique close dates in chronological order
    close_date = paste(sort(unique(close_date)), collapse = ", "),
    # Paste monitor flags in the order of appearance in time
    naaqs_primary_monitor = paste(
      naaqs_primary_monitor[order(open_date)], collapse = ", "
    ),
    
    .groups = "drop"
  )

poc_count <- collapsed_sites %>%
  filter(!is.na(poc)) %>%  # exclude NA POCs first
  group_by(county_code, site_number) %>%
  summarize(poc_count = n_distinct(poc), .groups = "drop")

samples <- samples %>%
  left_join(collapsed_sites) %>%
  left_join(poc_count)

# Filter and process data
AQS_pocs <- samples %>%
  group_by(county_code, site_number, year) %>%
  arrange(
    desc(naaqs_primary_monitor == "Y"),
    desc(!is.na(DV_period_samples)), 
    desc(DV_period_samples), 
    desc(annual_samples)
  ) %>%
  slice(1) %>%
  ungroup()

saveRDS(AQS_pocs, here("data", "PM10","AQS_PM10_pocs.rds"))

