# AQ Trends

pollutant <- "PM25"

aqs_data <- readRDS(paste0("data/aqs_data/", pollutant, ".rds"))


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

ggplot(CO, aes(x = year, y = avg_concentration)) +
  geom_line(color = "steelblue") +
  geom_point(color = "darkred") +
  labs(
    title = "Annual Average PM2.5 Concentration",
    x = "Year",
    y = "Avg Concentration (µg/m³)",
    caption = "Source: AQS data"
  ) +
  theme_minimal()

