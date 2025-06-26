# process data
rm(list = ls())

#### Merge AQS+AirNow data, clean, and calculate DVs ####
# load necessary inputs
pollutant_codes <- readRDS(here("data", "pollutant_codes.rds"))
sites <- readRDS(here("data", "sites.rds")) #contains poc and primary naaqs monitor info

PM10_codes <- unlist(pollutant_codes$Parameter_Code[pollutant_codes$Pollutant == "PM10"])
PM25_codes <- unlist(pollutant_codes$Parameter_Code[pollutant_codes$Pollutant == "PM25"])

source_pollutant_scripts <- function(pollutants_to_source) {
  # Loop over each pollutant in the pollutants_to_source list
  for (pollutant in pollutants_to_source) {
    
    # Define the pollutant-specific file paths
    get_AQS_pocs_script <- here("data_scripts", "processing", pollutant, paste0("get_", pollutant, "_AQS_pocs.R"))
    merge_script <- here("data_scripts", "processing", pollutant, paste0("merge_", pollutant, "_AQS_AirNow.R"))
    dv_24hr_script <- here("data_scripts", "processing", pollutant, paste0("calculate_24hr_", pollutant, "_DVs.R"))
    dv_ann_script <- here("data_scripts", "processing", pollutant, paste0("calculate_ann_", pollutant, "_DVs.R"))
    
    # Source the scripts
    message(paste("Sourcing scripts for", pollutant))
    source(get_AQS_pocs_script)
    source(merge_script)
    source(dv_24hr_script)
    source(dv_ann_script)
  }
  source(here("data_scripts","processing","PM25","cumulative", "calculate_cumulative_PM25.R")) #cleans data; AQS priority and assigns poc to AirNow
  message(paste("Sourcing cumulative PM25 data processing for tracker plot"))
}

pollutants_to_source <- c("PM10", "PM25")
source_pollutant_scripts(pollutants_to_source)

#### Filter and save "most representative" site-poc for a given year for map inputs ####

filter_sites <- function(pollutant) {
  # Construct file path dynamically
  file_name <- paste0("ann_", pollutant, "_DVs.xlsx")
  file_path <- here("data_processed", "DVs", file_name)
  
  # Define the parameter codes based on the pollutant
  parameter_codes <- switch(pollutant,
                            "PM25" = PM25_codes,
                            "PM10" = PM10_codes,
                            stop("Unsupported pollutant type."))  # Error if not recognized

  # Read the data
  DVs <- read_excel(file_path) %>%
    select(site_name,
           local_site_name,
           county_code,
           site_number,
           poc,
           year,
           annual_samples,
           DV_period_samples,
           DV_period_complete_quarters,
           monitoring_agency,
           county_name,
           open_date,
           close_date,
           naaqs_primary_monitor,
           `Complete Dataset?`)
  
  # Filter and process data
  sites_filtered <- DVs %>%
    group_by(county_code, site_number, year) %>%
    arrange(
      desc(naaqs_primary_monitor == "Y"),
      desc(!is.na(DV_period_samples)), 
      desc(DV_period_samples), 
      desc(annual_samples)
    ) %>%
    slice(1) %>%
    ungroup()
  
  # Save the filtered data as an RDS file
  output_file <- here("data_processed", "site_input_map", paste0(pollutant, "_sites_filtered.rds"))
  saveRDS(sites_filtered, output_file)
  
  # Return the filtered dataset
  return(sites_filtered)
}

sites_filtered_PM25 <- filter_sites("PM25")
sites_filtered_PM10 <- filter_sites("PM10")

rm(list = ls())
