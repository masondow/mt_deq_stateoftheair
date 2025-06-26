library(dplyr)
library(readxl)
library(leaflet)
#### Map DVs ####
DV_list <- read_excel("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/DATA/PM10/2000_2024_5yr_PM10_DVs_24h_wf98.xlsx")

pollutant <- "PM10"
NAAQS <- 1
year <- 2024

# Filter for the most recent year per site
DV_list_filtered <- DV_list %>%
  mutate(pollutant = pollutant) %>%
  filter(year == !!year)

# Define a color palette based on design_value
pal <- colorNumeric(
  palette = "YlOrRd",  # Choose a color scale ("YlOrRd" is Yellow-Orange-Red)
  domain = DV_list_filtered$DV_3yr_avg_exceedance,  # Define the range of values
  na.color = "lightgray"  # Color for NA values
)

leaflet(DV_list_filtered) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    popup = ~paste0("<b>Site Name:</b> ", local_site_name, "<br>",
                    "<b>County:</b> ", county, "<br>",
                    "<b>Pollutant (period):</b> ", pollutant, " (24-hour)<br>",
                    "<b>DV Form:</b> 3-yr avg. ann. exceedances<br>",
                    "<b>DV (LMP) Form:</b> 5-yr n-th highest (look-up method)<br>",
                    "<b>Year:</b> ", year, "<br>",
                    "<b>DV:</b> ", DV_3yr_avg_exceedance, "<br>",
                    "<b>DV (No Wildfire):</b> ", DV_3yr_avg_exceedance_no_wildfire, "<br>",
                    "<b>DV (LMP):</b> ", CDV_5yr, "<br>",
                    "<b>DV (LMP, No Wildfire):</b> ", CDV_5yr_no_wildfire, "<br>",
                    "<b>Complete Dataset?:</b> ", `Complete Dataset?`),
    radius = ~ifelse(is.na(DV_3yr_avg_exceedance), 5,
                     ifelse(DV_3yr_avg_exceedance > NAAQS, 9, 7)),
    color = ~ifelse(is.na(DV_3yr_avg_exceedance), "gray",  # Gray border if NA
                    ifelse(DV_3yr_avg_exceedance > NAAQS, "red", "black")),  # Red if >35, black otherwise
    weight = ~ifelse(is.na(DV_3yr_avg_exceedance), 1,  # Thinner border if NA
                     ifelse(DV_3yr_avg_exceedance > NAAQS, 4, 2)),  # Thicker border if >35
    opacity = 1,
    fillColor = ~ifelse(is.na(DV_3yr_avg_exceedance), "lightgray", pal(DV_3yr_avg_exceedance)),  # Light gray fill if NA
    fillOpacity = 1
  ) %>%
  addLegend(
    position = "bottomleft",
    pal = pal,
    values = DV_list_filtered$DV_3yr_avg_exceedance,
    title = paste0(year, " DV"),
    opacity = 1
  )



