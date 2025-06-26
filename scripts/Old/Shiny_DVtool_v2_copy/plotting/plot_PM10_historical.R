library(ggplot2)
library(plotly)
library(dplyr)
library(highcharter)

data_d <- readRDS(here("data_processed","merged_daily", "merged_daily_PM10.rds"))
data_h <- readRDS(here("data_processed","merged_hourly", "merged_hourly_PM10.rds"))

site <- "Butte"
year_filter <- 2025
poc_filter <- 4  # Specify the POC number you want to filter for

qualifiers <- c(
  "RT - Wildfire-U. S.",
  "RF - Fire - Canadian.",
  "IF - Fire - Canadian.",
  "IT - Wildfire-U. S.",
  "E - Forest Fire."
)

# Define AQI categories and colors
aqi_breaks <- c(0, 54, 154, 254, 354, 424, 10000)  # AQI threshold values
aqi_colors <- c(
  "green"   = "rgb(0,228,0)",      # 0-50 (Good)
  "yellow"  = "rgb(255,255,0)",    # 51-100 (Moderate)
  "orange"  = "rgb(255,126,0)",    # 101-150 (Unhealthy for Sensitive Groups)
  "red"     = "rgb(255,0,0)",      # 151-200 (Unhealthy)
  "purple"  = "rgb(143,63,151)",   # 201-300 (Very Unhealthy)
  "maroon"  = "rgb(126,0,35)"      # 301-500 (Hazardous)
)

filtered_data_d <- data_d %>%
  filter(site_name == site,
         year == year_filter,
         (poc == poc_filter | (is.na(poc) & source == "AirNow"))) %>%
  mutate(date_time = as.numeric(date_time) * 1000) %>%  # Convert to milliseconds
  group_by(date_local) %>%
  arrange(sample_duration != "1 HOUR") %>%  # Prioritize "1 HOUR" rows
  slice(1) %>%  # Keep only one row per date
  ungroup() %>%
  mutate(arithmetic_mean = trunc(arithmetic_mean * 10) / 10)  # Truncate to 1 decimal place

# Filter for dates where qualifier matches the specified values
wildfire <- data_d %>%
  filter(site_name == site,
         year == year_filter,
         (poc == poc_filter | (is.na(poc) & source == "AirNow")),
         qualifier %in% qualifiers) %>%
  group_by(date_local) %>%
  slice(1) %>%  # Keep only one row per date
  ungroup()
  
hc <- highchart() %>%
  hc_add_series(
    data = filtered_data_d, 
    type = "line", 
    hcaes(x = date_time, y = arithmetic_mean), 
    name = "PM10 Levels",
    zones = list(
      list(value = 54, color = "rgb(0,228,0)"),   # Green
      list(value = 154, color = "rgb(255,255,0)"), # Yellow
      list(value = 254, color = "rgb(255,126,0)"), # Orange
      list(value = 354, color = "rgb(255,0,0)"),   # Red
      list(value = 424, color = "rgb(143,63,151)"), # Purple
      list(value = 10000, color = "rgb(126,0,35)")   # Maroon
    )) %>%
  hc_title(text = paste0("PM10 Levels in ", site)) %>%
  hc_xAxis(
    title = list(text = "Date"),
    type = "datetime",  # Specify that the x-axis is datetime
    labels = list(format = "{value:%b %Y}")  # Customize the date format for the labels
  ) %>%
  hc_xAxis(
    title = list(text = "Date"),
    plotBands = lapply(wildfire$date_time, function(d) list(
      from = as.numeric(d) * 1000,  # Convert to milliseconds
      to = as.numeric(d) * 1000 + 1000 * 60 * 60 * 24,  # Extend band for 1 day
      color = "rgba(255, 0, 0, 0.2)"  # Light red transparent band
    ))
  ) %>%
  hc_yAxis(title = list(text = "24-hr avg. PM10 (µg/m³)")) %>%
  hc_tooltip(pointFormat = "PM10: {point.y} µg/m³")

hc  # Show plot


####

filtered_data_h <- data_h %>%
  filter(site_name == site,
         year == year_filter,
         (poc == poc_filter | (is.na(poc) & source == "AirNow"))) %>%
  mutate(date_time = as.numeric(date_time) * 1000) %>%  # Convert to milliseconds
  mutate(sample_measurement = trunc(sample_measurement * 10) / 10)  # Truncate to 1 decimal place


# Create the Highcharts plot
hc <- highchart() %>%
  hc_add_series(data = filtered_data_h, 
                type = "line", 
                hcaes(x = date_time, y = sample_measurement), 
                name = "PM10 Levels",
                zones = list(
                  list(value = 54, color = "rgb(0,228,0)"),   # Green
                  list(value = 154, color = "rgb(255,255,0)"), # Yellow
                  list(value = 254, color = "rgb(255,126,0)"), # Orange
                  list(value = 354, color = "rgb(255,0,0)"),   # Red
                  list(value = 424, color = "rgb(143,63,151)"), # Purple
                  list(value = 10000, color = "rgb(126,0,35)")   # Maroon
                )) %>%
  hc_title(text = paste0("PM10 Levels in ", site)) %>%
  hc_xAxis(
    title = list(text = "Date (MST)"),
    type = "datetime",  # Specify that the x-axis is datetime
    labels = list(format = "{value:%b %Y}")  # Customize the date format for the labels
  ) %>%
  hc_xAxis(
    title = list(text = "Date (MST)"),
    plotBands = lapply(wildfire$date_time, function(d) list(
      from = as.numeric(d) * 1000,  # Convert to milliseconds
      to = as.numeric(d) * 1000 + 1000 * 60 * 60 * 24,  # Extend band for 1 day
      color = "rgba(255, 0, 0, 0.2)"  # Light red transparent band
    ))
  ) %>%
  hc_xAxis(type = "datetime", title = list(text = "Date (MST)")) %>%
  hc_yAxis(title = list(text = "1-hour PM10 (µg/m³)")) %>%
  hc_tooltip(pointFormat = "PM10: {point.y} µg/m³")

hc  # Show plot
