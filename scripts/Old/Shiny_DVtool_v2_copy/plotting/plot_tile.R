library(highcharter)
library(lubridate)
library(dplyr)

pollutant <- "PM25"
year_filter <- 2025
site_filter <- "Libby"

sites_to_plot <- readRDS(here("data_processed","site_input_map","PM25_sites_filtered.rds")) %>%
  filter(year == year_filter) %>%
  select(site_name, poc)

data_d <- readRDS(here("data_processed","merged_daily", "merged_daily_PM25.rds"))

aqi_breaks <- c(0, 9, 35.4, 55.4, 125.4, 225.4, 250)
aqi_colors <- c(
  "rgb(0,228,0)",     # green
  "rgb(255,255,0)",   # yellow
  "rgb(255,126,0)",   # orange
  "rgb(255,0,0)",     # red
  "rgb(143,63,151)",  # purple
  "rgb(126,0,35)"     # maroon
)

# Only use the lower bounds for stops (length 6)
breaks_for_stops <- aqi_breaks[1:6]
normalized_stops <- (breaks_for_stops - min(breaks_for_stops)) / (max(aqi_breaks) - min(aqi_breaks))

# Now both are length 6
color_stops <- purrr::map2(normalized_stops, aqi_colors, ~ list(.x, .y))

# Step 1: Create a full year grid for the selected year
# Redefine months and days to avoid conflicts with functions
months <- month.abb  # Built-in month abbreviations
days <- 1:31  # Numeric vector representing days of the month

# Now you can create the full year grid
full_year_grid <- expand.grid(month = months, day = days) %>%
  mutate(
    year = year_filter,  # Directly use the value of year_filter
    # Convert month from character to numeric
    month_num = match(month, month.abb),  # Convert month abbreviation to numeric
    date_time = as.Date(paste(year, month_num, day, sep = "-")),  # Create a Date object
    x = day - 1,
    y = as.numeric(month_num) - 1,
    value = NA  # Initialize all values to NA
  )

# Step 2: Filter and prepare your actual data
data_d <- data_d %>%
  semi_join(sites_to_plot, by = join_by(poc, site_name)) %>%
  filter(year(date_time) == year_filter,
         site_name == site_filter) %>%
  select(site_name, local_site_name, poc, source, arithmetic_mean, date_time) %>%
  mutate(
    year = year(date_time),
    month = lubridate::month(date_time, label = TRUE, abbr = TRUE),
    day = day(date_time),
    month_num = as.numeric(month)  # Convert ordered factor to numeric
  )

# Now perform the join
merged_data <- full_year_grid %>%
  left_join(data_d, by = c("year", "month_num", "day")) %>%
  mutate(value = ifelse(is.na(arithmetic_mean), NA, arithmetic_mean))  # Fill value with actual data

# Step 4: Prepare the data for plotting
plot_data <- merged_data %>%
  transmute(
    x = day - 1,
    y = as.numeric(month_num) - 1,
    value = value,
    name = paste(month.abb[as.numeric(month_num)], day),
    source = source  # include this in each point
  )

hc <- highchart() %>%
  hc_chart(type = "heatmap") %>%
  hc_title(text = paste0(site_filter, " Daily ", pollutant, " - ", year_filter)) %>%
  hc_xAxis(
    categories = sort(unique(plot_data$day)),
    title = list(text = "Day")
  ) %>%
  hc_yAxis(
    categories = month.abb,  # reverse to match calendar-style
    reversed = TRUE,
    title = list(text = "Month")
  ) %>%
  hc_colorAxis(
    stops = color_stops,
    min = min(aqi_breaks),
    max = max(aqi_breaks),
    # Explicitly handle NA values by setting a null color
    nullColor = "#e0e0e0"  # Color for NA values (you can change this to any color)
  ) %>%
  hc_add_series(
    data = plot_data,  # Use plot_data to include NA values
    name = paste(pollutant),
    dataLabels = list(enabled = FALSE)
  ) %>%
  hc_tooltip(
    useHTML = TRUE,
    pointFormat = "<b>{point.name}</b><br><b>{point.value:.2f}</b> µg/m<sup>3</sup><br>Source: {point.source}"
  )

# Show the plot
print(hc)


