
# UI Function
DV_bar_UI <- function(id) {
  ns <- NS(id)
  tagList(
    highchartOutput(ns("dv_bar"))
  )
}


designValueBarServer <- function(id, pollutant, avg_period, year_filter, dv_type) {
  moduleServer(id, function(input, output, session) {
    
    output$dv_bar <- renderHighchart({
      req(pollutant(), avg_period(), year_filter())  # Ensure reactives are available
      print(paste("pollutant:", pollutant()))
      print(paste("avg_period:", avg_period()))
      print(paste("dv_type:", dv_type()))
      print(paste("Rendering plot for pollutant:", pollutant(), "avg_period:", avg_period(), "year_filter:", year_filter()))
      
      # Get the appropriate site list
      site_file <- here("data_processed", "site_input_map", paste0(pollutant(), "_sites_filtered.rds"))
      if (!file.exists(site_file)) {
        print(paste("File not found:", site_file))
        return(NULL)
      }
      
      sites_to_plot <- readRDS(site_file) %>%
        filter(year == year_filter()) %>%
        select(site_name, poc)
      
      NAAQS <- get_NAAQS(pollutant(), avg_period())
      
      # Get the appropriate DV file
      dv_file <- here("data_processed", "DVs", paste0(avg_period(), "_", pollutant(), "_DVs.xlsx"))
      if (!file.exists(dv_file)) {
        print(paste("File not found:", dv_file))
        return(NULL)
      }
      
      if (pollutant() == "PM10" && avg_period() == "24hr" && dv_type() == "NAAQS") {
        DVs <- read_excel(dv_file) %>%
          filter(year == year_filter()) %>%
          semi_join(sites_to_plot, by = c("site_name", "poc")) %>%
          arrange(DV_3yr_avg_exceedance) %>%
          mutate(wildfire_contribution = round(DV_3yr_avg_exceedance - DV_3yr_avg_exceedance_no_wildfire, 1))
        
        # Get NAAQS series data using numeric indices for the categorical axis
        NAAQS_line_data <- lapply(seq_along(DVs$site_name), function(i) list(x = i - 1, y = NAAQS))
        
        if (nrow(DVs) == 0) {
          print("No data found for the selected filters.")
          return(NULL)
        }
        
        #print(str(DVs))  # Debug print for the loaded DVs data
        
        # Plot
        plot_obj <- highchart() %>%
          hc_chart(type = "column") %>%
          
          # First series: DV_3yr_avg_exceedance
          hc_add_series(
            data = DVs, 
            type = "column",
            hcaes(x = site_name, y = wildfire_contribution), 
            name = "Wildfire Contribution",
            color = "#F54D28",  # Color for the first set of bars (customizable)
            stacking = "normal",    # Ensure bars overlap
            pointPadding = 0,   # Remove padding between bars for perfect overlap
            tooltip = list(
              pointFormat = paste0(
                "Wildfire Contribution: {point.y} exceedances"  # Display Wildfire Contribution
              )
            )
          ) %>%
          
          # Second series: DV_3yr_avg_exceedance_no_wildfire
          hc_add_series(
            data = DVs, 
            type = "column",
            hcaes(x = site_name, y = DV_3yr_avg_exceedance_no_wildfire, additional_series_value = DV_3yr_avg_exceedance), 
            name = "DV 3yr Avg Exceedance (No Wildfire)",
            color = "#004A98",  # Color for the second set of bars (customizable)
            stacking = "normal",    # Ensure bars overlap
            pointPadding = 0,   # Remove padding between bars for perfect overlap
            tooltip = list(
              pointFormat = paste0(
                "Design Value: {point.additional_series_value} exceedances<br/>",  # Access the new field for the second series value
                "Design Value (no wildfire): {point.y} exceedances<br/>"  # Display Wildfire Contribution
              )
            )
          ) %>%
          # Third series: NAAQS line as a constant series
          hc_add_series(
            data = NAAQS_line_data,
            type = "line",
            name = "NAAQS",
            color = "#34495E",
            dashStyle = "Dash",
            lineWidth = 2,
            marker = list(enabled = FALSE),
            enableMouseTracking = TRUE,
            showInLegend = FALSE,
            tooltip = list(
              pointFormat = paste0(
                "Current NAAQS: {point.y} exceedance<br/>"  # Display Wildfire Contribution
                )
              )
          ) %>%
          
          hc_xAxis(type = "category") %>%
          
          # Y-axis and plot line for NAAQS
          hc_yAxis(
            title = list(text = "Design Value - Est. # Exceedances", useHTML = TRUE)
          ) %>%
          
          hc_title(text = paste("Design Values by Site –", year_filter())) %>%
          hc_legend(enabled = TRUE)  # Show legend for both series
      }
      
      else if (pollutant() == "PM10" && avg_period() == "24hr" && dv_type() == "LMP") {
        DVs <- read_excel(dv_file) %>%
          filter(year == year_filter()) %>%
          semi_join(sites_to_plot, by = c("site_name", "poc")) %>%
          arrange(LMP_DV) %>%
          mutate(wildfire_contribution = round(LMP_DV - LMP_DV_no_wildfire, 1))
        
        # Get default CDV series data using numeric indices for the categorical axis
        CDV_line_data <- lapply(seq_along(DVs$site_name), function(i) list(x = i - 1, y = 98))
        
        if (nrow(DVs) == 0) {
          print("No data found for the selected filters.")
          return(NULL)
        }
        
        #print(str(DVs))  # Debug print for the loaded DVs data
        
        # Plot
        plot_obj <- highchart() %>%
          hc_chart(type = "column") %>%
          
          # First series
          hc_add_series(
            data = DVs, 
            type = "column",
            hcaes(x = site_name, y = wildfire_contribution), 
            name = "Wildfire Contribution",
            color = "#F54D28",  # Color for the first set of bars (customizable)
            stacking = "normal",    # Ensure bars overlap
            pointPadding = 0,   # Remove padding between bars for perfect overlap
            tooltip = list(
              pointFormat = paste0(
                "Wildfire Contribution: {point.y} µg/m³<br/>"  # Display Wildfire Contribution
              )
            )
          ) %>%
          
          # Second series
          hc_add_series(
            data = DVs, 
            type = "column",
            hcaes(x = site_name, y = LMP_DV_no_wildfire, additional_series_value = LMP_DV), 
            name = "LMP Design Value",
            color = "#004A98",  # Color for the second set of bars (customizable)
            stacking = "normal",    # Ensure bars overlap
            pointPadding = 0,   # Remove padding between bars for perfect overlap
            tooltip = list(
              pointFormat = paste0(
                "Design Value: {point.additional_series_value} µg/m³<br/>",  # Access the new field for the second series value
                "Design Value (no wildfire): {point.y} µg/m³<br/>"  # Display Wildfire Contribution
              )
            )
          ) %>%
          # Third series
          hc_add_series(
            data = CDV_line_data,
            type = "line",
            name = "Critical Design Value (default)",
            color = "#34495E",
            dashStyle = "Dash",
            lineWidth = 2,
            marker = list(enabled = FALSE),
            enableMouseTracking = TRUE,
            showInLegend = FALSE,
            tooltip = list(
              pointFormat = paste0(
                "Critical DV (default): {point.y} µg/m³<br/>"  # Display Wildfire Contribution
              )
            )
          ) %>%
          
          hc_xAxis(type = "category") %>%
          
          # Y-axis and plot line for NAAQS
          hc_yAxis(
            title = list(text = "Design Value - µg/m³<br/>", useHTML = TRUE)
          ) %>%
          
          hc_title(text = paste("LMP Design Values by Site –", year_filter())) %>%
          hc_legend(enabled = TRUE)  # Show legend for both series
      }
      
      else {
        DVs <- read_excel(dv_file) %>%
          filter(year == year_filter()) %>%
          semi_join(sites_to_plot, by = c("site_name", "poc")) %>%
          arrange(design_value) %>%
          mutate(wildfire_contribution = round(design_value - design_value_no_wildfire, 1))
        
        # Get NAAQS series data using numeric indices for the categorical axis
        NAAQS_line_data <- lapply(seq_along(DVs$site_name), function(i) list(x = i - 1, y = NAAQS))
        
        if (nrow(DVs) == 0) {
          print("No data found for the selected filters.")
          return(NULL)
        }
        
        #print(str(DVs))  # Debug print for the loaded DVs data
        
        # Plot
        plot_obj <- highchart() %>%
          hc_chart(type = "column") %>%
          
          # First series: DV_3yr_avg_exceedance
          hc_add_series(
            data = DVs, 
            type = "column",
            hcaes(x = site_name, y = wildfire_contribution), 
            name = "Wildfire Contribution",
            color = "#F54D28",  # Color for the first set of bars (customizable)
            stacking = "normal",    # Ensure bars overlap
            pointPadding = 0,   # Remove padding between bars for perfect overlap
            tooltip = list(
              pointFormat = paste0(
                "Wildfire Contribution: {point.y} µg/m³<br/>"  # Display Wildfire Contribution
              )
            )
          ) %>%
          
          # Second series: DV_3yr_avg_exceedance_no_wildfire
          hc_add_series(
            data = DVs, 
            type = "column",
            hcaes(x = site_name, y = design_value_no_wildfire, additional_series_value = design_value), 
            name = "Design Value (No Wildfire)",
            color = "#004A98",  # Color for the second set of bars (customizable)
            stacking = "normal",    # Ensure bars overlap
            pointPadding = 0,   # Remove padding between bars for perfect overlap
            tooltip = list(
              pointFormat = paste0(
                "Design Value: {point.additional_series_value} µg/m³<br/>",  # Access the new field for the second series value
                "Design Value (no wildfire): {point.y} µg/m³<br/>"  # Display Wildfire Contribution
              )
            )
          ) %>%
          # Third series: NAAQS line as a constant series
          hc_add_series(
            data = NAAQS_line_data,
            type = "line",
            name = "NAAQS",
            color = "#34495E",
            dashStyle = "Dash",
            lineWidth = 2,
            marker = list(enabled = FALSE),
            enableMouseTracking = TRUE,
            showInLegend = TRUE,
            tooltip = list(
              pointFormat = paste0(
                "Current NAAQS: {point.y} µg/m³<br/>"  # Display Wildfire Contribution
                )
              )
          ) %>%
        
        hc_xAxis(type = "category") %>%
          
          # Y-axis and plot line for NAAQS
          hc_yAxis(
            title = list(text = "Design Value - µg/m³", useHTML = TRUE)
          ) %>%
          
          hc_title(text = paste("Design Values by Site –", year_filter())) %>%
          hc_legend(enabled = TRUE)  # Show legend for both series
      }
        
      
      return(plot_obj)
      
    })
  })
}