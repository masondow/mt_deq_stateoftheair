# design_value_module_ui.R
design_value_module_ui <- function(id) {
  ns <- NS(id)  # Namespace for module UI
  
  tagList(
    fluidRow(
      # First row: Pollutant and Averaging Period inputs
      column(3, selectInput(ns("pollutant"), "Pollutant", 
                            choices = c("PM2.5" = "PM25", "PM10" = "PM10"), 
                            selected = "PM25")),
      
      column(3, selectInput(ns("avg_period"), "Averaging Period", 
                            choices = c("24 Hour" = "24hr", "Annual" = "ann", "24 Hour LMP (PM10 only)" = "24hr_PM10_LMP"),
                            selected = "ann")),
  
      column(3, numericInput(ns("year_filter"), "DV Year",
                             value = as.numeric(format(Sys.Date()-1, "%Y")),
                             min = 2000, max = as.numeric(format(Sys.Date()-1, "%Y")), step = 1)),
      
      column(3, uiOutput(ns("site_selector")))
    ),
    # Add title for Wildfire Table
    h4("Choose Data to Exclude:"),
    DTOutput(ns("wildfire_table")),
    
    # Line break after the first table
    br(),
    
    # Add title for Exclusion Tracker Table
    h4("Design Value Result (cumulative):"),
    DTOutput(ns("exclusion_tracker")),
    br(),
    fluidRow(
      # Third row: Clear Selection Button and Download Button
      column(4, actionButton(ns("clear_selection"), "Clear Selection", icon = icon("eraser"))),
      column(4, downloadButton(ns("download_results"), "Download Results"))
    )
  )
}

design_value_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Namespace for server-side handling
    
    filtered_sites <- reactive({
      req(input$pollutant, input$avg_period, input$year_filter)
      
      # Debugging statement to check the inputs
      #print(paste("Pollutant:", input$pollutant, "Averaging Period:", input$avg_period))
      
      # Call get_sites with the selected pollutant, avg_period, and year_filter
      sites <- get_sites(input$pollutant, input$avg_period, input$year_filter)
      
      # Debugging the result
      #print(sites)
      
      # If sites is NULL (no valid sites for the selected combination), return NULL
      if (is.null(sites) || nrow(sites) == 0) {
        return(NULL)
      }
      
      return(sites)
    })
    
    output$site_selector <- renderUI({
      req(filtered_sites())  # Make sure filtered_sites is valid
      
      # If filtered_sites is NULL or empty, return NULL to clear site input options
      if (is.null(filtered_sites()) || nrow(filtered_sites()) == 0) {
        return(NULL)  # No sites to show
      }
      
      # If there are valid sites, show the selectInput
      selectInput(ns("site_filter"), "Site", choices = unique(filtered_sites()$site_name))
    })
    
    # Define filtered_data as a reactive expression that calls get_filtered_data
    filtered_data <- reactive({
      req(input$pollutant, input$avg_period, input$site_filter, input$year_filter)
      
      # Call get_filtered_data with the inputs to get the daily and wildfire data
      get_filtered_data(
        pollutant = input$pollutant,
        avg_period = input$avg_period,
        site_filter = input$site_filter,  # Assuming site_filter is provided as input
        year_filter = input$year_filter
      )
    })
    
    # Reactive value to store selected wildfire rows
    selected_wildfire_rows <- reactiveVal(NULL)
    
    output$wildfire_table <- renderDT({
      req(filtered_data())
      
      datatable(
        filtered_data()$wf_data %>%
          arrange(desc(arithmetic_mean)) %>%
          select(date_local, arithmetic_mean, qualifier) %>%
          mutate(arithmetic_mean = round(arithmetic_mean, 1)), 
        selection = "multiple",
        options = list(
          scrollY = "200px",  # Adjust the height for scrollable area
          scroller = TRUE,    # Enable scrolling functionality
          paging = FALSE,     # Disable page selection (use scrolling instead)
          dom = "t",          # Hide table controls (optional)
          scrollCollapse = TRUE  # Collapse empty rows when scrolling
        )
      ) %>%
        # Adjust font size for all columns
        formatStyle(
          columns = c("date_local", "arithmetic_mean", "qualifier"),
          target = "cell",
          fontSize = "14px"  # Set the font size for the columns
        ) %>%
        # Adjust font size for index column (row numbers)
        formatStyle(
          columns = 0,  # First column (index column)
          fontSize = "14px"  # Set the font size for the index column
        )
    })
    
    observeEvent(input$wildfire_table_rows_selected, {
      selected_wildfire_rows(input$wildfire_table_rows_selected)
    })
    
    observeEvent({
      input$pollutant
      input$avg_period
      input$site_filter
      input$year_filter
    }, {
      selected_wildfire_rows(NULL)       # Reset the selected rows reactive
      selectRows(dataTableProxy("wildfire_table"), NULL)  # Reset table selection
    })
    
    observeEvent(input$clear_selection, {
      selected_wildfire_rows(NULL)  # Clear stored selection
      selectRows(dataTableProxy("wildfire_table"), NULL)  # Clear visible selection
    })
    
    # Create a reactive value to store the results table
    results_reactive <- reactiveVal()
    
    # Track exclusions and resulting DVs
    output$exclusion_tracker <- renderDT({
      req(filtered_data())
      
      selected_rows <- selected_wildfire_rows()  # <- use the reactiveVal
      
      # If no rows selected, return empty table
      if (is.null(selected_rows) || length(selected_rows) == 0) {
        return(datatable(data.frame(
          `Excluded Date` = character(),
          `Concentration` = numeric(),
          `DV After Exclusion` = numeric()
        ), rownames = FALSE))
      }
      
      # Continue your existing logic
      wf_data_sorted <- filtered_data()$wf_data %>% 
        arrange(desc(arithmetic_mean)) %>%
        mutate(arithmetic_mean = round(arithmetic_mean, 1))
      wf_data_selected <- wf_data_sorted[selected_rows, , drop = FALSE]
      
      #print(wf_data_selected)
      
      # Initialize results data frame
      results <- data.frame(
        excluded_date = character(),
        concentration = numeric(),
        DV_after_exclusion = numeric()
      )
      
      # Initialize excluded_dates as an empty vector
      excluded_dates <- c()
      
      # Calculate the baseline design value with NO exclusions
      baseline_filtered <- filtered_data()$daily_data  # Get the full dataset without exclusions
      
      # Calculate baseline DV
      baseline_dv <- calculate_DV(
        pollutant = input$pollutant,
        avg_period = input$avg_period,
        year_filter = input$year_filter,
        daily_data = baseline_filtered
      )
      
      # Add the baseline DV as the first row of the results
      results <- bind_rows(
        tibble(
          excluded_date = "Baseline (No Exclusion)",  # Label for the baseline row
          concentration = NA,  # No concentration for baseline
          DV_after_exclusion = round(as.numeric(baseline_dv), 2)  # Round the baseline DV
        ),
        results  # Add the baseline row before any exclusions
      )
      
      for (i in seq_len(nrow(wf_data_selected))) {
        # Ensure the excluded date is in Date format (convert if necessary)
        excluded_date <- as.Date(wf_data_selected$date_local[i])  # Convert to Date
        
        # Print for debugging: Check the value of excluded_date before appending
        #print(paste("Excluding date:", excluded_date))
        
        # Append the date to excluded_dates (ensure it's a Date)
        excluded_dates <- c(as.Date(excluded_dates), excluded_date)
        
        #print(excluded_dates)
        #print(as.Date(excluded_dates))
        
        filtered <- filtered_data()$daily_data %>%
          filter(!date_local %in% excluded_dates)  # Ensure date_local is a Date for comparison
        
        # Calculate the design value with the filtered data
        dv <- calculate_DV(
          pollutant = input$pollutant,
          avg_period = input$avg_period,
          year_filter = input$year_filter,
          daily_data = filtered
        )
        
        #print("Value before coercion:")
        #print(dv)
        #str(dv)
        
        dv_value <- round(as.numeric(dv), 2)
        
        # Store the results using bind_rows for better handling
        results <- bind_rows(results, tibble(
          excluded_date = as.character(excluded_date),
          concentration = wf_data_selected$arithmetic_mean[i],
          DV_after_exclusion = dv_value
        ))
      }
      
      # Update the reactive value with the results table
      results_reactive(results)
      # Assign the desired names with spaces
      colnames(results) <- c("Excluded Date", "Concentration", "DV After Exclusion")
      
      # Render the results table
      # Render the results table with scroll and font size adjustments
      datatable(
        results, 
        rownames = FALSE, 
        options = list(
          scrollY = "200px",       # Make the table scrollable with a height of 200px
          scroller = TRUE,         # Enable scrolling functionality
          paging = FALSE,          # Disable paging (use scroll instead)
          dom = "t",               # Hide table controls (optional)
          scrollCollapse = TRUE    # Collapse empty rows when scrolling
        )
      ) %>%
        # Adjust font size for all columns
        formatStyle(
          columns = c("Excluded Date", "Concentration", "DV After Exclusion"),
          target = "cell",
          fontSize = "14px"  # Set the font size for the columns
        ) %>%
        # Adjust font size for the index column (row numbers)
        formatStyle(
          columns = 0,  # First column (index column)
          fontSize = "14px"  # Set the font size for the index column
        )
      
      
    })
    
    # Download handler
    output$download_results <- downloadHandler(
      filename = function() {
        paste("exclusion_results_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        # Add the columns Site, Pollutant, Avg Period, and DV Year to the results
        final_results <- results_reactive() %>%
          mutate(
            Site = input$site_filter,
            Pollutant = input$pollutant,
            Avg_Period = input$avg_period,
            DV_Year = input$year_filter
          )
        
        # Write the final results to CSV with added columns at the beginning
        write.csv(final_results, file, row.names = FALSE)
      }
    )
  })  # <- closing parenthesis for moduleServer()
}    # <- closing parenthesis for design_value_module_server()