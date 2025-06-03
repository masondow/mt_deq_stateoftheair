# AQ Trends
library(dplyr)
library(lubridate)

pollutant <- "PM25"
aqs_data <- readRDS(paste0("data/aqs_data/", pollutant, ".rds"))
aqs_data_filtered <- aqs_data %>%
  filter(validity_indicator == "Y") %>%
  mutate(
    year = lubridate::year(as.Date(date_local)),
    # Flag whether this row is the preferred type
    preferred_row = (sample_duration_code == "1")
  ) %>%
  filter(year <= 2024) %>%
  group_by(county_code, site_number, date_local) %>%
  # Prefer the row where sample_duration_code == "1"
  slice_max(preferred_row, with_ties = FALSE) %>%
  ungroup() %>%
  group_by(year) %>%
  summarize(avg_concentration = mean(arithmetic_mean, na.rm = TRUE), .groups = "drop")


library(ggplot2)

ggplot(aqs_data_filtered, aes(x = year, y = avg_concentration)) +
  geom_line(color = "steelblue") +
  geom_point(color = "darkred") +
  labs(
    title = "Annual Average PM2.5 Concentration",
    x = "Year",
    y = "Avg Concentration (µg/m³)",
    caption = "Source: AQS data"
  ) +
  theme_minimal()

