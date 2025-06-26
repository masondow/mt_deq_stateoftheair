
#test
#DVs_filtered <- load_map_data("PM25","ann",2025)

load_map_data <- function(pollutant, avg_period, year_filter) {
  library(dplyr)
  library(readxl)
  library(here)
  
  # Define file paths
  site_file <- here("data_processed", "site_input_map", paste0(pollutant, "_sites_filtered.rds"))
  dv_file <- here("data_processed", "DVs", paste0(avg_period, "_", pollutant, "_DVs.xlsx"))
  
  # Load site data
  sites_to_map <- readRDS(site_file) %>%
    filter(year == year_filter) %>%
    select(site_name, poc)
  
  # Load site data
  lat_lon <- readRDS(here("data","sites.rds")) %>%
    select(county_code, site_number, latitude, longitude) %>%
    distinct()
  
  # Load and filter DV data
  DVs_filtered <- read_excel(dv_file) %>%
    semi_join(sites_to_map, by = c("site_name", "poc")) %>%  # or change keys if needed
    filter(year == year_filter) %>%
    mutate(pollutant = pollutant) %>%
    left_join(lat_lon)

  return(DVs_filtered)
}


