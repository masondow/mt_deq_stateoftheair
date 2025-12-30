# AQ Trends
library(dplyr)
library(lubridate)

pollutant <- "Pb"
aqs_data <- readRDS(paste0("data/aqs_data/daily_processed/", pollutant, "_daily_processed.rds"))

aqs_data_filtered <- aqs_data %>%
  filter(year <= 2024) %>%
  filter(year >= 2002) %>% #Pb only
  group_by(year) %>%
  summarize(avg_concentration = mean(arithmetic_mean, na.rm = TRUE), .groups = "drop")


library(ggplot2)

ggplot(aqs_data_filtered, aes(x = year, y = avg_concentration)) +
  geom_line(color = "steelblue") +
  geom_point(color = "darkred") +
  labs(
    title = "Annual Average SO2 Concentration",
    x = "Year",
    y = "Avg Concentration",
    caption = "Source: AQS data"
  ) +
  theme_minimal()

