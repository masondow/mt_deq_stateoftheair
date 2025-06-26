library(dplyr)

#### Map DVs ####

DV_list <- read_excel("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/DATA/PM25/2000_2024_all_sites_PM25_DVs_ann.xlsx")

pollutant <- "PM25"
NAAQS <- 9
year <- 2024

# Filter for the most recent year per site
DV_list_filtered <- DV_list %>%
  mutate(pollutant = pollutant) %>%
  filter(year == !!year) %>%
  group_by(county_code, site_number) %>%
  arrange(poc != 3) %>%  # Prioritize POC 3
  slice(1) %>%  # Select the first (POC 3 if available)
  ungroup()

# Define a color palette based on design_value
pal <- colorNumeric(
  palette = "YlOrRd",  # Choose a color scale ("YlOrRd" is Yellow-Orange-Red)
  domain = DV_list_filtered$design_value,  # Define the range of values
  na.color = "lightgray"  # Color for NA values
)

library(leaflet)
leaflet(DV_list_filtered) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    popup = ~paste0("<b>Site Name:</b> ", local_site_name, "<br>",
                    "<b>County:</b> ", county, "<br>",
                    "<b>Pollutant (period):</b> ", pollutant, " (24-hour)<br>",
                    "<b>DV Form:</b> 3-year average<br>",
                    "<b>Year:</b> ", year, "<br>",
                    "<b>Annual Average:</b> ", annual_average, "<br>",
                    "<b>Annual Average (No Wildfire):</b> ", annual_average_no_flags, "<br>",
                    "<b>Design Value:</b> ", design_value, "<br>",
                    "<b>Design Value (No Wildfire):</b> ", design_value_no_flags, "<br>",
                    "<b>Complete Dataset?:</b> ", `Complete Dataset?`),
    radius = ~ifelse(is.na(design_value), 5,
                     ifelse(design_value > NAAQS, 9, 7)),
    color = ~ifelse(is.na(design_value), "gray",  # Gray border if NA
                    ifelse(design_value > NAAQS, "red", "black")),  # Red if >35, black otherwise
    weight = ~ifelse(is.na(design_value), 1,  # Thinner border if NA
                     ifelse(design_value > NAAQS, 4, 2)),  # Thicker border if >35
    opacity = 1,
    fillColor = ~ifelse(is.na(design_value), "lightgray", pal(design_value)),  # Light gray fill if NA
    fillOpacity = 1
  ) %>%
  addLegend(
    position = "bottomleft",
    pal = pal,
    values = DV_list_filtered$design_value,
    title = paste0(year, " DV"),
    opacity = 1
  )





