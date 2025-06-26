
cumulative_PM25 <- readRDS(here("data_processed","cumulative","cumulative_PM25.rds"))
DV_tracker <- readRDS(here("data_processed","cumulative","DV_tracker.rds"))

site_selection <- "Seeley Lake"

common_year <- 1970  # Set the reference year

# Filter the data for Libby site and relevant columns for filtered_DV
filtered_DV <- DV_tracker %>%
  filter(site_name == site_selection) %>%
  group_by(year) %>%
  arrange(date_time) %>%
  mutate(date_time = as.POSIXct(paste0(common_year, "-", format(date_time, "%m-%d")),
                                format = "%Y-%m-%d", tz = "MST")) %>%
  mutate(date_time = as.numeric(as.POSIXct(date_time, tz = "MST")) * 1000) %>%
  mutate(rolling_DV = round(rolling_DV, 2)) %>%  # Truncate rolling_DV to 2 decimal places
  ungroup()

# Filter the data for Libby site and relevant columns for filtered_cumulative_PM25
filtered_cumulative_PM25 <- cumulative_PM25 %>%
  filter(site_name == site_selection) %>%
  group_by(year) %>%
  arrange(date_time) %>%
  mutate(date_time = as.POSIXct(paste0(common_year, "-", format(date_time, "%m-%d")),
                                format = "%Y-%m-%d", tz = "MST"))%>%
  mutate(date_time = as.numeric(as.POSIXct(date_time, tz = "MST")) * 1000) %>%
  mutate(cumulative_avg_flat = round(cumulative_avg_flat, 2)) %>%  # Truncate rolling_DV to 2 decimal places
  ungroup()

DV_yr_3 <- filtered_cumulative_PM25 %>%
  filter(color_label == "DV yr 3")

DV_yr_2 <- filtered_cumulative_PM25 %>%
  filter(color_label == "DV yr 2")

DV_yr_1 <- filtered_cumulative_PM25 %>%
  filter(color_label == "DV yr 1")

Others <- filtered_cumulative_PM25 %>%
  filter(!(color_label %in% c("DV yr 3", "DV yr 2", "DV yr 1")))

library(highcharter)
# Plot using highcharts
hc <- highchart() %>%
  # Cumulative Average Flat Series (Others)
  hc_add_series(
    data = Others,
    type = "line",
    hcaes(x = date_time, y = cumulative_avg_flat, group = year),
    name = "Cumulative Avg PM25",
    color = "#dedede"  # Black color
  ) %>%
  # DV yr 1 Series
  hc_add_series(
    data = DV_yr_1,
    type = "line",
    hcaes(x = date_time, y = cumulative_avg_flat, group = year),
    name = "Cumulative Avg PM25",
    color = "#48C9B0"  # Black color
  ) %>%
  # DV yr 2 Series
  hc_add_series(
    data = DV_yr_2,
    type = "line",
    hcaes(x = date_time, y = cumulative_avg_flat, group = year),
    name = "Cumulative Avg PM25",
    color = "#009ADE"  # Black color
  ) %>%
  # DV yr 3 Series
  hc_add_series(
    data = DV_yr_3,
    type = "line",
    hcaes(x = date_time, y = cumulative_avg_flat, group = year),
    name = "Cumulative Avg PM25",
    color = "#004A98"  # Black color
  ) %>%
  # NAAQS Series
  hc_add_series(
    data = filtered_DV,
    type = "line",
    hcaes(x = date_time, y = NAAQS, group = year),
    name = "NAAQS",
    color = "#34495E",  # Black color
    dashStyle = "Dash"  # Dashed line
  ) %>%
  # Rolling DV Series
  hc_add_series(
    data = filtered_DV,
    type = "line",
    hcaes(x = date_time, y = rolling_DV, group = year),
    name = "Design Value Tracker",
    color = "#F54D28"  # Green
  ) %>%
  hc_title(text = paste0(site_selection," DV Tracker")) %>%
  hc_xAxis(
    title = list(text = "Date"),
    type = "datetime",  # X-axis as datetime
    labels = list(format = "{value:%b}")  # Format date labels
  ) %>%
  hc_yAxis(title = list(text = "Value")) %>%
  hc_tooltip(
    useHTML = TRUE,  # Enables custom formatting
    headerFormat = "<b>{series.name}</b><br>",
    pointFormat = "Date: {point.x:%b %e} <br> Year: {point.series.options.group} <br> Conc.: {point.y}"
  ) %>%
  hc_legend(
    enabled = TRUE,
    labelFormatter = JS("function () {
      return this.options.group;  // Uses 'group' (year) as legend label
    }")
  )

print(hc)



