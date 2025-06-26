# map_module.R

mapUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    leafletOutput(ns("map"), height = 600)
  )
}

mapServer <- function(id, pollutant, avg_period, year_filter, selected_site) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    map_data <- reactive({
      load_map_data(pollutant(), avg_period(), year_filter())
    })
    
    output$map <- renderLeaflet({
      req(map_data())
      render_map(
        DVs_filtered = map_data(),
        year_filter = year_filter(),
        NAAQS = get_NAAQS(pollutant(), avg_period()),
        pollutant = pollutant(),
        avg_period = avg_period()
      )
    })
    
    # Extract metadata when a marker is clicked
    observeEvent(input$map_marker_click, {
      click_id <- input$map_marker_click$id
      df <- map_data()
      
      id_parts <- strsplit(click_id, "_")[[1]]
      
      site_row <- df %>%
        filter(site_name == id_parts[1],
               county_code == id_parts[2],
               site_number == id_parts[3],
               poc == id_parts[4])
      
      # ✅ Update shared selected_site from main server
      selected_site(site_row)
    })
    
    # Return whatever you need — could be NULL or other outputs
    return(invisible(NULL))
  })
}