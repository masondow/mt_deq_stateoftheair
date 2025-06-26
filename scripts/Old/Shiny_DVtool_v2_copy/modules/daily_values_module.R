library(ggplot2)
library(plotly)
library(dplyr)
library(highcharter)

# UI Function
historical_daily_UI <- function(id) {
  ns <- NS(id)
  tagList(
    highchartOutput(ns("daily_value_plot"))
  )
}

# Server Function
historical_daily_Server <- function(id, selected_site, pollutant, year_filter) {
  moduleServer(id, function(input, output, session) {
    
    output$daily_value_plot <- renderHighchart({
      site_info <- selected_site()
      if (is.null(site_info)) return(NULL)
      
      qualifiers <- c(
        "RT - Wildfire-U. S.",
        "RF - Fire - Canadian.",
        "IF - Fire - Canadian.",
        "IT - Wildfire-U. S.",
        "E - Forest Fire."
      )
      
      get_aqi_scale(pollutant())
      
      aqi_scale <- get_aqi_scale(pollutant())
      aqi_breaks <- aqi_scale$breaks
      aqi_colors <- aqi_scale$colors
      
      sites_to_plot <- readRDS(here("data_processed","site_input_map",paste0(pollutant(), "_sites_filtered.rds"))) %>%
        filter(year == year_filter()) %>%
        select(site_name, poc)
      
      data_d <- readRDS(here("data_processed", "merged_daily", paste0("merged_daily_", pollutant(), ".rds")))  # Fixing this line
      
      filtered_data_d <- data_d %>%
        semi_join(sites_to_plot, by = join_by(poc, site_name)) %>%
        filter(year(date_time) == year_filter(),
               site_name == site_info$site_name) %>%
        mutate(date_time = as.numeric(date_time) * 1000) %>%  # Convert to milliseconds
        mutate(arithmetic_mean = trunc(arithmetic_mean * 10) / 10)  # Truncate to 1 decimal place
      
      # Return NULL if there's no data to plot
      if (nrow(filtered_data_d) == 0) return(NULL)
      
      # Filter for dates where qualifier matches the specified values
      wildfire <- data_d %>%
        filter(site_name == site_info$site_name,
               year == year_filter(),
               (poc == site_info$poc | (is.na(poc) & source == "AirNow")),
               qualifier %in% qualifiers) %>%
        group_by(date_local) %>%
        slice(1) %>%  # Keep only one row per date
        ungroup()
      
      # PM25 Annual Average DV Map
      if (pollutant() == "PM25") {
        plot_obj <- highchart() %>%
          hc_add_series(
            data = filtered_data_d, 
            type = "line", 
            hcaes(x = date_time, y = arithmetic_mean), 
            marker = list(enabled = FALSE),
            name = "PM2.5 Concentration",
            zones = list(
              list(value = 9, color = "rgb(0,228,0)"),   # Green
              list(value = 35.4, color = "rgb(255,255,0)"), # Yellow
              list(value = 55.4, color = "rgb(255,126,0)"), # Orange
              list(value = 125.4, color = "rgb(255,0,0)"),   # Red
              list(value = 225.4, color = "rgb(143,63,151)"), # Purple
              list(value = 10000, color = "rgb(126,0,35)")   # Maroon
            )) %>%
          hc_title(text = paste0(site_info$site_name, " Daily PM2.5 Concentration - ", year_filter())) %>%  # Corrected site name reference
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
          hc_yAxis(title = list(text = "24-hr avg. PM2.5 (µg/m³)")) %>%
          hc_tooltip(pointFormat = "PM2.5: {point.y} µg/m³") %>%
          hc_legend(enabled = FALSE)
      }
      
      # PM10 Annual Average DV Map
      if (pollutant() == "PM10") {
        plot_obj <- highchart() %>%
          hc_add_series(
            data = filtered_data_d, 
            type = "line", 
            hcaes(x = date_time, y = arithmetic_mean), 
            marker = list(enabled = FALSE),
            name = "PM10 Concentration",
            zones = list(
              list(value = 54, color = "rgb(0,228,0)"),   # Green
              list(value = 154, color = "rgb(255,255,0)"), # Yellow
              list(value = 254, color = "rgb(255,126,0)"), # Orange
              list(value = 354, color = "rgb(255,0,0)"),   # Red
              list(value = 424, color = "rgb(143,63,151)"), # Purple
              list(value = 10000, color = "rgb(126,0,35)")   # Maroon
            )) %>%
          hc_title(text = paste0(site_info$site_name, " Daily PM10 Concentration - ", year_filter())) %>%  # Corrected site name reference
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
          hc_tooltip(pointFormat = "PM10: {point.y} µg/m³") %>%
          hc_legend(enabled = FALSE)
      }
      return(plot_obj)  # Show plot
    })
  })
}