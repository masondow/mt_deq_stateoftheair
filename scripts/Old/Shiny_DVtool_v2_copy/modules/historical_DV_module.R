# modules/historical_DV.R

library(shiny)
library(highcharter)
library(dplyr)
library(readxl)
library(here)

# UI Function
historical_DVUI <- function(id) {
  ns <- NS(id)
  tagList(
    textOutput(ns("site_message")),
    highchartOutput(ns("DV_plot"))
  )
}

# Server Function
historical_DVServer <- function(id, selected_site, pollutant, avg_period, year_filter, dv_type) {
  moduleServer(id, function(input, output, session) {
    
    output$site_message <- renderText({
      site_info <- selected_site()
      if (is.null(site_info)) {
        return("Select a site from the map")
      }
      return("")  # Blank if site is selected
    })
    
    output$DV_plot <- renderHighchart({
      site_info <- selected_site()
      if (is.null(site_info)) return(NULL)
      
      # Use year_filter() here — works now!
      sites_to_plot <- readRDS(here("data_processed", "site_input_map", paste0(pollutant(), "_sites_filtered.rds"))) %>%
        filter(year == year_filter()) %>%
        select(site_name, poc)
      
      file_path <- here("data_processed", "DVs", paste0(avg_period(), "_", pollutant(), "_DVs.xlsx"))
      if (!file.exists(file_path)) return(NULL)
      
      NAAQS <-  get_NAAQS(pollutant(), avg_period())
      
      DVs <- read_excel(file_path)
      
      # PM25 Annual Average DV Map
      if (pollutant() == "PM25" & avg_period() == "ann") {
        DVs <- DVs %>%
          semi_join(sites_to_plot) %>%
          filter(site_name == site_info$site_name) %>%
          select(site_name, year, design_value, design_value_no_wildfire) %>%
          mutate(standard = NAAQS)
        
        if (nrow(DVs) == 0) return(NULL)
        
        # Build highchart
        map_obj <- highchart() %>%
          hc_title(text = paste("Design Value History for", site_info$site_name)) %>%
          hc_xAxis(categories = DVs$year) %>%
          hc_yAxis(
            title = list(text = "Design Value (µg/m<sup>3</sup>)",
                         useHTML = TRUE)) %>%
          hc_add_series(
            name = "Design Value", 
            data = DVs$design_value, 
            type = "line", 
            color = "#F54D28"
          ) %>%
          hc_add_series(
            name = "Design Value (No Wildfire)", 
            data = DVs$design_value_no_wildfire, 
            type = "line", 
            color = "#004A98"
          ) %>%
          hc_add_series(
            name = "NAAQS",
            data = DVs$standard, 
            type = "line", 
            color = "#34495E", 
            dashStyle = "Dash",
            marker = list(enabled = FALSE)) %>%
          hc_legend(enabled = TRUE)
      }
      
      # PM25 24hr DV Map
      if (pollutant() == "PM25" & avg_period() == "24hr") {
        DVs <- DVs %>%
          semi_join(sites_to_plot) %>%
          filter(site_name == site_info$site_name) %>%
          select(site_name, year, design_value, design_value_no_wildfire) %>%
          mutate(standard = NAAQS)
        
        if (nrow(DVs) == 0) return(NULL)
        
        # Build highchart
        map_obj <- highchart() %>%
          hc_title(text = paste("Design Value History for", site_info$site_name)) %>%
          hc_xAxis(categories = DVs$year) %>%
          hc_yAxis(
            title = list(text = "Design Value (µg/m<sup>3</sup>)",
                         useHTML = TRUE)) %>%
          hc_add_series(
            name = "Design Value", 
            data = DVs$design_value, 
            type = "line", 
            color = "#F54D28"
          ) %>%
          hc_add_series(
            name = "Design Value (No Wildfire)", 
            data = DVs$design_value_no_wildfire, 
            type = "line", 
            color = "#004A98"
          ) %>%
          hc_add_series(
            name = "NAAQS",
            data = DVs$standard, 
            type = "line", 
            color = "#34495E", 
            dashStyle = "Dash",
            marker = list(enabled = FALSE)) %>%
          hc_legend(enabled = TRUE)
      }
      
      # PM10 Annual Average DV Map
      if (pollutant() == "PM10" & avg_period() == "ann") {
        DVs <- DVs %>%
          semi_join(sites_to_plot) %>%
          filter(site_name == site_info$site_name) %>%
          select(site_name, year, design_value, design_value_no_wildfire) %>%
          mutate(standard = NAAQS)
        
        if (nrow(DVs) == 0) return(NULL)
        
        # Build highchart
        map_obj <- highchart() %>%
          hc_title(text = paste("Design Value History for", site_info$site_name)) %>%
          hc_xAxis(categories = DVs$year) %>%
          hc_yAxis(
            title = list(text = "Design Value (µg/m<sup>3</sup>)",
                         useHTML = TRUE)) %>%
          hc_add_series(
            name = "Design Value", 
            data = DVs$design_value, 
            type = "line", 
            color = "#F54D28"
          ) %>%
          hc_add_series(
            name = "Design Value (No Wildfire)", 
            data = DVs$design_value_no_wildfire, 
            type = "line", 
            color = "#004A98"
          ) %>%
          hc_add_series(
            name = "NAAQS",
            data = DVs$standard, 
            type = "line", 
            color = "#34495E", 
            dashStyle = "Dash",
            marker = list(enabled = FALSE)) %>%
          hc_legend(enabled = TRUE)
      }
      
      # PM10 24hr DV Map
      if (pollutant() == "PM10" & avg_period() == "24hr" & dv_type() == "NAAQS") {
        DVs <- DVs %>%
          semi_join(sites_to_plot) %>%
          filter(site_name == site_info$site_name) %>%
          select(site_name, year, DV_3yr_avg_exceedance, DV_3yr_avg_exceedance_no_wildfire) %>%
          mutate(standard = NAAQS)
        
        if (nrow(DVs) == 0) return(NULL)
        
        # Build highchart
        map_obj <- highchart() %>%
          hc_title(text = paste("Design Value History for", site_info$site_name)) %>%
          hc_xAxis(categories = DVs$year) %>%
          hc_yAxis(
            title = list(text = "Design Value - Est. # Exceedances",
                         useHTML = TRUE)) %>%
          hc_add_series(
            name = "Design Value", 
            data = DVs$DV_3yr_avg_exceedance, 
            type = "line", 
            color = "#F54D28"
          ) %>%
          hc_add_series(
            name = "Design Value (No Wildfire)", 
            data = DVs$DV_3yr_avg_exceedance_no_wildfire, 
            type = "line", 
            color = "#004A98"
          ) %>%
          hc_add_series(
            name = "NAAQS",
            data = DVs$standard, 
            type = "line", 
            color = "#34495E", 
            dashStyle = "Dash",
            marker = list(enabled = FALSE)) %>%
          hc_legend(enabled = TRUE)
      }
      
      # PM10 24hr LMP DV Map
      if (pollutant() == "PM10" & avg_period() == "24hr" & dv_type() == "LMP") {
        DVs <- DVs %>%
          semi_join(sites_to_plot) %>%
          filter(site_name == site_info$site_name) %>%
          select(site_name, year, LMP_DV, LMP_DV_no_wildfire, CDV_no_wildfire) %>%
          mutate(standard = NAAQS)
        
        if (nrow(DVs) == 0) return(NULL)
        
        # Build highchart
        map_obj <- highchart() %>%
          hc_title(text = paste("LMP DV History for", site_info$site_name)) %>%
          hc_xAxis(categories = DVs$year) %>%
          hc_yAxis(
            title = list(text = "Design Value (µg/m<sup>3</sup>)",
                         useHTML = TRUE)) %>%
          hc_add_series(
            name = "LMP DV", 
            data = DVs$LMP_DV, 
            type = "line", 
            color = "#F54D28"
          ) %>%
          hc_add_series(
            name = "LMP DV (No Wildfire >98 µg/m<sup>3</sup>)", 
            data = DVs$LMP_DV_no_wildfire, 
            type = "line", 
            color = "#004A98"
          ) %>%
          hc_add_series(
            name = "Critical Design Value (No Wildfire >98 µg/m<sup>3</sup>)",
            data = DVs$CDV_no_wildfire, 
            type = "line", 
            color = "#34495E", 
            dashStyle = "Dash",
            marker = list(enabled = FALSE)) %>%
          hc_legend(
            enabled = TRUE,
            useHTML = TRUE
          )
      }
      
      # Return the generated map
      return(map_obj)
  
    })
  })
}