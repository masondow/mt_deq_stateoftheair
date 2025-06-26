# cumulative_PM25.R plotting module

cumulative_PM25UI <- function(id) {
  ns <- NS(id)
  
  # Load DV_tracker inside the UI
  DV_tracker <- readRDS(here("data_processed", "cumulative", "DV_tracker.rds"))
  
  tagList(
    selectInput(ns("site_name"), "Select Site", choices = unique(DV_tracker$site_name), selected = unique(DV_tracker$site_name)[1]),
    highchartOutput(ns("cumulative_plot"), height = "600px")
  )
}

cumulative_PM25Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    cumulative_PM25 <- readRDS(here("data_processed", "cumulative", "cumulative_PM25.rds"))
    DV_tracker <- readRDS(here("data_processed", "cumulative", "DV_tracker.rds"))
    
    output$cumulative_plot <- renderHighchart({
      req(input$site_name)  # Ensure something is selected
      
      site_selection <- input$site_name
      common_year <- 1970
      
      filtered_DV <- DV_tracker %>%
        filter(site_name == site_selection) %>%
        group_by(year) %>%
        arrange(date_time) %>%
        mutate(date_time = as.POSIXct(paste0(common_year, "-", format(date_time, "%m-%d")), format = "%Y-%m-%d", tz = "MST")) %>%
        mutate(date_time = as.numeric(as.POSIXct(date_time, tz = "MST")) * 1000) %>%
        mutate(rolling_DV = round(rolling_DV, 2)) %>%
        ungroup()
      
      filtered_cumulative_PM25 <- cumulative_PM25 %>%
        filter(site_name == site_selection) %>%
        group_by(year) %>%
        arrange(date_time) %>%
        mutate(date_time = as.POSIXct(paste0(common_year, "-", format(date_time, "%m-%d")), format = "%Y-%m-%d", tz = "MST")) %>%
        mutate(date_time = as.numeric(as.POSIXct(date_time, tz = "MST")) * 1000) %>%
        mutate(cumulative_avg_flat = round(cumulative_avg_flat, 2)) %>%
        ungroup()
      
      DV_yr_3 <- filtered_cumulative_PM25 %>% filter(color_label == "DV yr 3")
      DV_yr_2 <- filtered_cumulative_PM25 %>% filter(color_label == "DV yr 2")
      DV_yr_1 <- filtered_cumulative_PM25 %>% filter(color_label == "DV yr 1")
      Others   <- filtered_cumulative_PM25 %>% filter(!(color_label %in% c("DV yr 3", "DV yr 2", "DV yr 1")))
      
      highchart() %>%
        hc_add_series(data = Others, 
                      type = "line", 
                      hcaes(x = date_time, y = cumulative_avg_flat, group = year), 
                      name = "Cumulative Avg PM25", 
                      color = "#dedede",
                      marker = list(enabled = FALSE)) %>%
        hc_add_series(data = DV_yr_1, 
                      type = "line", 
                      hcaes(x = date_time, y = cumulative_avg_flat, group = year), 
                      name = "DV yr 1", 
                      color = "#48C9B0",
                      marker = list(enabled = FALSE)) %>%
        hc_add_series(data = DV_yr_2, 
                      type = "line", 
                      hcaes(x = date_time, y = cumulative_avg_flat, group = year), 
                      name = "DV yr 2", 
                      color = "#009ADE",
                      marker = list(enabled = FALSE)) %>%
        hc_add_series(data = DV_yr_3, 
                      type = "line", 
                      hcaes(x = date_time, y = cumulative_avg_flat, group = year),
                      name = "DV yr 3", 
                      color = "#004A98",
                      marker = list(enabled = FALSE)) %>%
        hc_add_series(data = filtered_DV, 
                      type = "line", 
                      hcaes(x = date_time, y = NAAQS, group = year), 
                      name = "NAAQS", 
                      color = "#34495E", 
                      dashStyle = "Dash",
                      marker = list(enabled = FALSE)) %>%
        hc_add_series(data = filtered_DV, 
                      type = "line", 
                      hcaes(x = date_time, y = rolling_DV, group = year), 
                      name = "Design Value Tracker", 
                      color = "#F54D28",
                      lineWidth = 3,  # ← Increase this number for a thicker line
                      marker = list(enabled = FALSE)) %>%
        hc_title(text = paste0(site_selection, " Annual PM2.5 DV Tracker")) %>%
        hc_xAxis(title = list(text = "Date"), type = "datetime", labels = list(format = "{value:%b}")) %>%
        hc_yAxis(title = list(text = "Cumulative Concentration")) %>%
        hc_tooltip(
          useHTML = TRUE,
          headerFormat = "<b>{series.name}</b><br>",
          pointFormat = "Date: {point.x:%b %e} <br> Year: {point.series.options.group} <br> Conc.: {point.y}"
        ) %>%
        hc_legend(
          enabled = TRUE,
          labelFormatter = JS("function () {
            return this.options.group;
          }")
        )
    })
  })
}