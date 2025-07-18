# AQ Trends
library(dplyr)
library(lubridate)

pollutant <- "SO2"
aqs_data <- readRDS(paste0("data/aqs_data/daily_processed/", pollutant, "_daily_processed.rds"))

aqs_data_filtered <- aqs_data %>%
  filter(year <= 2024) %>%
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

