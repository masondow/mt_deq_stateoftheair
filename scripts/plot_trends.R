# List all .rds files in the directory
rds_files <- list.files("data/aqs_data", pattern = "\\.rds$", full.names = TRUE)

# Loop over each file and assign to global environment
for (file in rds_files) {
  # Create a valid object name from the filename (e.g., "CO_42101")
  obj_name <- tools::file_path_sans_ext(basename(file))
  
  # Read the RDS file
  data <- readRDS(file)
  
  # Assign the data frame to the global environment with the filename as variable name
  assign(obj_name, data, envir = .GlobalEnv)
}


library(dplyr)
library(lubridate)

clean_and_summarize <- function(df) {
  df %>%
    filter(validity_indicator == "Y") %>%
    mutate(year = year(as.Date(date_local))) %>%
    arrange(county_code, site_number, date_local, sample_duration_code != "1") %>%
    group_by(county_code, site_number, date_local) %>%
    slice(1) %>%
    ungroup() %>%
    group_by(year) %>%
    summarize(avg_concentration = mean(arithmetic_mean, na.rm = TRUE), .groups = "drop")
}

file_paths <- list.files("data/aqs_data", full.names = TRUE, pattern = "\\.rds$")
aqs_data_list <- setNames(lapply(file_paths, readRDS), tools::file_path_sans_ext(basename(file_paths)))

summary_list <- lapply(aqs_data_list, clean_and_summarize)
list2env(summary_list, envir = .GlobalEnv)



library(ggplot2)

ggplot(CO_42101, aes(x = year, y = avg_concentration)) +
  geom_line(color = "steelblue") +
  geom_point(color = "darkred") +
  labs(
    title = "Annual Average PM2.5 Concentration",
    x = "Year",
    y = "Avg Concentration (µg/m³)",
    caption = "Source: AQS data"
  ) +
  theme_minimal()

