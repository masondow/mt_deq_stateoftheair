library(ggplot2)
library(plotly)
library(dplyr)
library(highcharter)


pollutant <- "PM25"
year_filter <- year(Sys.Date() - 1)
end <- Sys.Date() - 1
start <- Sys.Date() - 3

sites_to_plot <- readRDS(here("data_processed","site_input_map","PM25_sites_filtered.rds")) %>%
  filter(year == year_filter) %>%
  select(site_name, poc)

data_h <- readRDS(here("data_processed","merged_hourly", "merged_hourly_PM25.rds"))
data_h <- data_h %>%
  semi_join(sites_to_plot) %>%
  filter(date_local <= end & date_local >= start) %>%
  mutate(date_time = as.numeric(date_time) * 1000) %>%  # Convert to milliseconds
  mutate(sample_measurement = trunc(sample_measurement * 10) / 10)  # Truncate to 1 decimal place
  

# Define AQI categories and colors
aqi_breaks <- c(0, 9, 35.4, 55.4, 125.4, 225.4, 10000)  # AQI threshold values
aqi_colors <- c(
  "green"   = "rgb(0,228,0)",      # 0-50 (Good)
  "yellow"  = "rgb(255,255,0)",    # 51-100 (Moderate)
  "orange"  = "rgb(255,126,0)",    # 101-150 (Unhealthy for Sensitive Groups)
  "red"     = "rgb(255,0,0)",      # 151-200 (Unhealthy)
  "purple"  = "rgb(143,63,151)",   # 201-300 (Very Unhealthy)
  "maroon"  = "rgb(126,0,35)"      # 301-500 (Hazardous)
)


# Create a highchart for each site_name in data_h
charts <- data_h %>%
  group_by(site_name) %>%
  group_map(~ {
    hc <- highchart() %>%
      hc_add_series(
        data = .x,  # Use the grouped data
        type = "line",
        hcaes(x = date_time, y = sample_measurement),
        name = "PM2.5 Levels",
        zones = list(
          list(value = 9, color = "rgb(0,228,0)"),   # Green
          list(value = 35.4, color = "rgb(255,255,0)"), # Yellow
          list(value = 55.4, color = "rgb(255,126,0)"), # Orange
          list(value = 125.4, color = "rgb(255,0,0)"),   # Red
          list(value = 225.4, color = "rgb(143,63,151)"), # Purple
          list(value = 10000, color = "rgb(126,0,35)")   # Maroon
        )
      ) %>%
      hc_title(
        text = paste0("PM2.5 Levels in ", .y$site_name),
        style = list(fontSize = "12px")  # Adjust the plot title font size here
      ) %>%
      hc_xAxis(
        title = list(text = NULL),  # Remove the x-axis title
        type = "datetime",
        labels = list(format = "{value:%H:%M %b %e}")
      ) %>%
      hc_yAxis(
        title = list(text = NULL)  # Remove the y-axis title
      ) %>%
      hc_tooltip(pointFormat = "PM2.5: {point.y} µg/m³")
    
    # Return a named list of highcharts
    list(site = .y$site_name, chart = hc)
  }) %>%
  setNames(map(., "site")) %>%  # Set site_name as list names
  map("chart")  # Extract just the chart objects

# test
#charts[["Helena"]]



