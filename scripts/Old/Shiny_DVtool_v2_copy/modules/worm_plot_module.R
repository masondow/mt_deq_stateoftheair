# Module UI
worm_plot_UI <- function(id) {
  ns <- NS(id)
  
  # Input for pollutant selection
  tagList(
    selectInput(ns("pollutant"), "Pollutant", 
                choices = c("PM2.5" = "PM25", "PM10" = "PM10"), 
                selected = "PM25"),
    
    # Placeholder for the charts, to be populated dynamically
    fluidRow(
      uiOutput(ns("chart_grid"))
    )
  )
}

# Server
worm_plot_Server <- function(id, year_filter, start, end) {
  moduleServer(id, function(input, output, session) {
    
    # Make data filtering and chart generation reactive based on pollutant selection
    data_h <- reactive({
      req(input$pollutant)  # Ensure the pollutant input is available
      
      # Read the necessary files each time the module is triggered
      sites_to_plot <- readRDS(here("data_processed", "site_input_map", paste0(input$pollutant,"_sites_filtered.rds"))) %>%
        filter(year == year_filter) %>%
        select(site_name, poc)
      #print(paste("Sites to plot for", input$pollutant, ":", nrow(sites_to_plot)))
      
      # Filter data based on the selected pollutant
      data <- readRDS(here("data_processed", "merged_hourly", "recent_hourly", paste0("merged_short_hourly_", input$pollutant, ".rds")))
      
      data %>%
        semi_join(sites_to_plot) %>%
        filter(date_local <= end & date_local >= start) %>%
        mutate(date_time = as.numeric(date_time) * 1000) %>%  # Convert to milliseconds
        mutate(sample_measurement = trunc(sample_measurement * 10) / 10)  # Truncate to 1 decimal place
    })
    
    # Generate the highcharts for each site
    charts <- reactive({
      req(data_h())  # Make sure data is available before processing
      
      # Get the AQI scale based on the selected pollutant
      aqi_scale <- get_aqi_scale(input$pollutant)
      
      # Generate the charts for each site
      data_h() %>%
        group_by(site_name) %>%
        group_map(~ {
          hc <- highchart() %>%
            hc_add_series(
              data = .x,  # Use the grouped data
              type = "line",
              hcaes(x = date_time, y = sample_measurement),
              name = "Pollutant Levels",
              zones = lapply(1:length(aqi_scale$breaks) - 1, function(i) {
                list(
                  value = aqi_scale$breaks[i + 1],
                  color = aqi_scale$colors[i]
                )
              }),
              lineWidth = 5
            ) %>%
            hc_chart(margin = c(30, 0, 30, 50)) %>%  # Reduce the left and right margins (in pixels)
            hc_title(
              text = paste0(.y$site_name),
              style = list(fontSize = "10px", fontWeight = "bold")  # Make the title bold
            ) %>%
            hc_xAxis(
              title = list(text = NULL),  # Remove the x-axis title
              type = "datetime",
              labels = list(enabled = TRUE)  # Remove x-axis labels
            ) %>%
            hc_yAxis(
              title = list(text = ""),  # Remove the y-axis title
              labels = list(enabled = TRUE)  # Clear the labels and format
            ) %>%
            hc_tooltip(pointFormat = "{point.y} µg/m³") %>%
            hc_legend(enabled = FALSE)  # Remove the legend button
          
          # Return a named list of highcharts
          list(site = .y$site_name, chart = hc)
        }) %>%
        setNames(map(., "site")) %>%  # Set site_name as list names
        map("chart")  # Extract just the chart objects
    })
    
    # Render the grid layout for the charts dynamically
    output$chart_grid <- renderUI({
      req(charts())  # Ensure charts are ready
      
      # Create a column for each chart
      fluidRow(
        lapply(1:length(charts()), function(i) {
          column(
            width = 4,  # Set the column width to display 3 charts per row
            style = "padding: 10px;",  # Add padding to space out the charts
            highchartOutput(session$ns(paste0("chart_", i)), height = "200px")  # Dynamic highchart output
          )
        })
      )
    })
    
    # Render highcharts dynamically for each site
    observe({
      req(charts())  # Make sure charts are ready
      
      lapply(1:length(charts()), function(i) {
        site_name <- names(charts())[i]
        output[[paste0("chart_", i)]] <- renderHighchart({
          charts()[[site_name]]  # Render the chart for the corresponding site
        })
      })
    })
    
  })
}