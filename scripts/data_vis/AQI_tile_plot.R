aqi_breaks <- c(0, 9, 35.4, 55.4, 125.4, 225.4, 1000)
aqi_colors <- c(
  "#00E400",  # "Good"
  "#FFFF00",  # "Moderate"
  "#FF7E00",  # "USG"
  "#FF0000",  # "Unhealthy"
  "#8F3F97",  # "Very Unhealthy"
  "#7E0023"   # "Hazardous"
)

pollutant <- "PM2.5"
aqs_data <- readRDS(paste0("data/aqs_data/daily_processed/", pollutant, "_daily_processed.rds"))

aqs_data_daily_avg <- aqs_data %>%
  filter(year <= 2024) %>%
  group_by(date_local) %>%
  summarize(
    avg_daily_concentration = mean(arithmetic_mean, na.rm = TRUE),
    .groups = "drop"
  )

aqs_data_daily_max <- aqs_data %>%
  filter(year <= 2024) %>%
  group_by(date_local) %>%
  summarize(
    max_daily_concentration = max(arithmetic_mean, na.rm = TRUE),
    .groups = "drop"
  )

aqs_data_daily_site_max <- aqs_data %>%
  filter(year <= 2024,
         local_site_name == "Libby Courthouse Annex") %>%
  group_by(date_local) %>%
  summarize(
    max_daily_concentration = max(arithmetic_mean, na.rm = TRUE),
    .groups = "drop"
  )

library(dplyr)
library(lubridate)
library(ggplot2)
aqs_data_tiles_avg <- aqs_data_daily_avg %>%
  mutate(
    date_local = as.Date(date_local),
    year = lubridate::year(date_local),
    day_of_year = lubridate::yday(date_local),
    aqi_cat = cut(
      avg_daily_concentration,
      breaks = aqi_breaks,
      labels = c("Good", "Moderate", "USG", "Unhealthy", "Very Unhealthy", "Hazardous"),
      include.lowest = TRUE,
      right = FALSE
    )
  )

aqs_data_tiles_max <- aqs_data_daily_max %>%
  mutate(
    date_local = as.Date(date_local),
    year = lubridate::year(date_local),
    day_of_year = lubridate::yday(date_local),
    aqi_cat = cut(
      max_daily_concentration,
      breaks = aqi_breaks,
      labels = c("Good", "Moderate", "USG", "Unhealthy", "Very Unhealthy", "Hazardous"),
      include.lowest = TRUE,
      right = FALSE
    )
  )

aqs_data_tiles_site_max <- aqs_data_daily_site_max %>%
  mutate(
    date_local = as.Date(date_local),
    year = lubridate::year(date_local),
    day_of_year = lubridate::yday(date_local),
    aqi_cat = cut(
      max_daily_concentration,
      breaks = aqi_breaks,
      labels = c("Good", "Moderate", "USG", "Unhealthy", "Very Unhealthy", "Hazardous"),
      include.lowest = TRUE,
      right = FALSE
    )
  )

aqi_colors_named <- setNames(aqi_colors, levels(aqs_data_tiles$aqi_cat))

ggplot(aqs_data_tiles_max %>% filter(year >= 2006), aes(x = factor(year), y = day_of_year, fill = aqi_cat)) +
  geom_tile(color = "transparent") +
  scale_y_reverse() +
  scale_fill_manual(values = aqi_colors_named, na.value = "transparent", name = "AQI Category") +
  labs(
    title = "Daily PM2.5 AQI Category by Day of Year",
    x = "Year", y = "Day of Year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


