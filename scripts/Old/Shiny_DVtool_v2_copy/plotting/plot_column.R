
year_filter <- year(Sys.Date() - 1)

sites_to_plot <- readRDS(here("data_processed","site_input_map","ann_PM25_sites_filtered.rds")) %>%
  filter(year == year_filter) %>%
  select(site_name, poc)

DVs <- read_excel(here("data_processed","DVs","ann_PM25_DVs.xlsx"))
DVs <- DVs %>%
  filter(year == year_filter) %>%
  semi_join(sites_to_plot) %>%
  arrange(design_value)

# Define AQI categories and colors
aqi_breaks <- c(0, 9.1, 35.4, 55.4, 125.4, 225.4, 10000)  # AQI threshold values
aqi_colors <- c(
  "green"   = "rgb(0,228,0)",      # 0-50 (Good)
  "yellow"  = "rgb(255,255,0)",    # 51-100 (Moderate)
  "orange"  = "rgb(255,126,0)",    # 101-150 (Unhealthy for Sensitive Groups)
  "red"     = "rgb(255,0,0)",      # 151-200 (Unhealthy)
  "purple"  = "rgb(143,63,151)",   # 201-300 (Very Unhealthy)
  "maroon"  = "rgb(126,0,35)"      # 301-500 (Hazardous)
)

hc <- highchart() %>%
  hc_chart(type = "column") %>%
  hc_add_series(
    data = DVs, 
    type = "column",
    hcaes(x = site_name, y = design_value),  # Use mapped colors
    name = "Design Value",
    zones = list(
      list(value = 9.1, color = "rgb(0,228,0)"),   # Green
      list(value = 35.4, color = "rgb(255,255,0)"), # Yellow
      list(value = 55.4, color = "rgb(255,126,0)"), # Orange
      list(value = 125.4, color = "rgb(255,0,0)"),   # Red
      list(value = 225.4, color = "rgb(143,63,151)"), # Purple
      list(value = 10000, color = "rgb(126,0,35)")   # Maroon
    )
  ) %>%
  hc_xAxis(title = list(text = "Site Name"), type = "category") %>%
  hc_yAxis(title = list(text = "Design Value (µg/m³)")) %>%
  hc_title(text = "Design Values by Site") %>%
  hc_tooltip(pointFormat = "Design Value: {point.y} µg/m³")

hc  # Print the chart





