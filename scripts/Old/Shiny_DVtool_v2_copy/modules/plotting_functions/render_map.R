# render_map.R

#test
#map_obj <- render_map(DVs_filtered, 2021, NAAQS, "PM10", "24hr")

render_map <- function(DVs_filtered, year_filter, NAAQS, pollutant, avg_period, leaflet_proxy_id = NULL) {
  library(dplyr)
  library(leaflet)
  
  # Default map object in case no condition is met
  map_obj <- NULL
  
  # PM25 Annual Average DV Map
  if (pollutant == "PM25" & avg_period == "ann") {
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = DVs_filtered$design_value,
      na.color = "lightgray"
    )
    
    map_obj <- leaflet(DVs_filtered) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        layerId = ~paste(site_name, county_code, site_number, poc, sep = "_"),  # something unique
        popup = ~paste0("<b>Site Name:</b> ", site_name, "<br>",
                        "<b>Secondary Name:</b> ", local_site_name, "<br>",
                        "<b>POC:</b> ", poc, "<br>",
                        "<b>County:</b> ", county_name, "<br>",
                        "<b>Pollutant (period):</b> ", pollutant, " (Annual Average)<br>",
                        "<b>DV Form:</b> 3-year average<br>",
                        "<b>Year:</b> ", year_filter, "<br>",
                        "<b>Max. Daily Conc.:</b> ", max_daily_value, "<br>",
                        "<b>Annual Average:</b> ", annual_average, "<br>",
                        "<b>Annual Average (No Wildfire):</b> ", annual_average_no_flags, "<br>",
                        "<b>Design Value:</b> ", design_value, "<br>",
                        "<b>Design Value (No Wildfire):</b> ", design_value_no_wildfire, "<br>",
                        "<b>Complete Dataset?:</b> ", `Complete Dataset?`),
        radius = ~ifelse(is.na(design_value), 5,
                         ifelse(design_value > NAAQS, 9, 7)),
        color = ~ifelse(is.na(design_value), "gray",
                        ifelse(design_value > NAAQS, "red", "black")),
        weight = ~ifelse(is.na(design_value), 1,
                         ifelse(design_value > NAAQS, 4, 2)),
        opacity = 1,
        fillColor = ~ifelse(is.na(design_value), "lightgray", pal(design_value)),
        fillOpacity = 1
      ) %>%
      addLegend(
        position = "bottomleft",
        pal = pal,
        values = DVs_filtered$design_value,
        title = paste0(year_filter, " DV"),
        opacity = 1
      )
  }
  
  # PM25 24-Hour 98th %ile DV Map
  if (pollutant == "PM25" & avg_period == "24hr") {
    pal <- colorNumeric(
      palette = "YlOrRd",  # Choose a color scale
      domain = DVs_filtered$design_value,  # Define the range of values
      na.color = "lightgray"  # Color for NA values
    )
    
    map_obj <- leaflet(DVs_filtered) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        layerId = ~paste(site_name, county_code, site_number, poc, sep = "_"),  # something unique
        popup = ~paste0("<b>Site Name:</b> ", site_name, "<br>",
                        "<b>Secondary Name:</b> ", local_site_name, "<br>",
                        "<b>POC:</b> ", poc, "<br>",
                        "<b>County:</b> ", county_name, "<br>",
                        "<b>Pollutant (period):</b> ", pollutant, " (24-hour)<br>",
                        "<b>DV Form:</b> 98%-ile of 24-hr values (3-yr avg)<br>",
                        "<b>Year:</b> ", year_filter, "<br>",
                        "<b>Max. Daily Conc.:</b> ", max_daily_value, "<br>",
                        "<b>98th Percentile:</b> ", annual_98th_percentile, "<br>",
                        "<b>98th Percentile (No Wildfire):</b> ", annual_98th_percentile_no_wildfire, "<br>",
                        "<b>Design Value:</b> ", design_value, "<br>",
                        "<b>Design Value (No Wildfire):</b> ", design_value_no_wildfire, "<br>",
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
        values = DVs_filtered$design_value,
        title = paste0(year_filter, " DV"),
        opacity = 1
      )
  }
  
  # PM10 Annual Average DV Map
  if (pollutant == "PM10" & avg_period == "ann") {
    # Define a color palette based on design_value
    pal <- colorNumeric(
      palette = "YlOrRd",  # Choose a color scale ("YlOrRd" is Yellow-Orange-Red)
      domain = DVs_filtered$design_value,  # Define the range of values
      na.color = "lightgray"  # Color for NA values
    )
    
    map_obj <- leaflet(DVs_filtered) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        layerId = ~paste(site_name, county_code, site_number, poc, sep = "_"),  # something unique
        popup = ~paste0("<b>Site Name:</b> ", site_name, "<br>",
                        "<b>Secondary Name:</b> ", local_site_name, "<br>",
                        "<b>POC:</b> ", poc, "<br>",
                        "<b>County:</b> ", county_name, "<br>",
                        "<b>Pollutant (period):</b> ", pollutant, " (annual)<br>",
                        "<b>DV Form:</b> 3-year average<br>",
                        "<b>Year:</b> ", year_filter, "<br>",
                        "<b>Max. Daily Conc.:</b> ", max_daily_value, "<br>",
                        "<b>Annual Avg:</b> ", annual_avg, "<br>",
                        "<b>Annual Avg (No Wildfire):</b> ", annual_avg_no_wildfire, "<br>",
                        "<b>Design Value:</b> ", design_value, "<br>",
                        "<b>Design Value (No Wildfire):</b> ", design_value_no_wildfire, "<br>",
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
        values = DVs_filtered$design_value,
        title = paste0(year_filter, " DV"),
        opacity = 1
      )
  }
  
  # PM10 24-hr Exceedances DV Map
  if (pollutant == "PM10" & avg_period == "24hr") {
    # Define a color palette based on design_value
    pal <- colorNumeric(
      palette = "YlOrRd",  # Choose a color scale ("YlOrRd" is Yellow-Orange-Red)
      domain = DVs_filtered$DV_3yr_avg_exceedance,  # Define the range of values
      na.color = "lightgray"  # Color for NA values
    )
    
    map_obj <- leaflet(DVs_filtered) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        layerId = ~paste(site_name, county_code, site_number, poc, sep = "_"),  # something unique
        popup = ~paste0("<b>Site Name:</b> ", site_name, "<br>",
                        "<b>Secondary Name:</b> ", local_site_name, "<br>",
                        "<b>POC:</b> ", poc, "<br>",
                        "<b>County:</b> ", county_name, "<br>",
                        "<b>Pollutant (period):</b> ", pollutant, " (24-hour)<br>",
                        "<b>DV Form:</b> 3-yr avg. ann. exceedances<br>",
                        "<b>DV (LMP) Form:</b> 5-yr (look-up method)<br>",
                        "<b>Year:</b> ", year_filter, "<br>",
                        "<b>Max. Daily Conc.:</b> ", max_daily_value, "<br>",
                        "<b>DV:</b> ", DV_3yr_avg_exceedance, "<br>",
                        "<b>DV (No Wildfire):</b> ", DV_3yr_avg_exceedance_no_wildfire, "<br>",
                        "<b>DV (LMP):</b> ", LMP_DV, "<br>",
                        "<b>DV (LMP, No Wildfire):</b> ", LMP_DV_no_wildfire, "<br>",
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
        values = DVs_filtered$DV_3yr_avg_exceedance,
        title = paste0(year_filter, " DV"),
        opacity = 1
      )
  }
  
  # Return the generated map
  return(map_obj)
}
