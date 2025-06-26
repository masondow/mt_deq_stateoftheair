library(readxl)

pollutant <- "PM25"
avg_period <- "ann"
year_filter <- 2025
site_filter <- "Missoula" #defined by sites_to_plot

sites_to_plot <- readRDS(here("data_processed","site_input_map","PM25_sites_filtered.rds")) %>%
  filter(year == year_filter) %>%
  select(site_name, poc)

DVs <- read_excel(here("data_processed","DVs","ann_PM25_DVs.xlsx"))
DVs <- DVs %>%
  semi_join(sites_to_plot) %>%
  filter(site_name == site_filter) %>%
  select(site_name, year, design_value, design_value_no_flags)

# Create the highchart plot
hc <- highchart() %>%
  hc_title(text = "Design Value History") %>%
  hc_xAxis(categories = DVs$year) %>%
  hc_yAxis(title = list(text = "Design Value")) %>%
  hc_add_series(
    name = "Design Value", 
    data = DVs$design_value, 
    type = "line", 
    color = "#F54D28"
  ) %>%
  hc_add_series(
    name = "Design Value (No Wildfire)", 
    data = DVs$design_value_no_flags, 
    type = "line", 
    color = "#004A98"
  ) %>%
  hc_legend(enabled = TRUE)
print(hc)

