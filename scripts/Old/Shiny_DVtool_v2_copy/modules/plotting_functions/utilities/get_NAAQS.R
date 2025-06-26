# Helper function to define NAAQS based on pollutant, averaging period, and year

#test 
#NAAQS <- get_NAAQS("PM10","24hr")

get_NAAQS <- function(pollutant, avg_period) {
  # Define a NAAQS threshold for each pollutant and averaging period
  # You can adjust this logic as needed, e.g., based on specific regulations for each year.
  
  if (pollutant == "PM25") {
    if (avg_period == "ann") {
      # Example: 12 is the NAAQS for PM2.5 annual average
      return(9)
    } else if (avg_period == "24hr") {
      # Example: 35 is the NAAQS for PM2.5 24-hour average
      return(35)
    }
  } else if (pollutant == "PM10") {
    if (avg_period == "ann") {
      # Example: 150 is the NAAQS for PM10 annual average
      return(50)
    } else if (avg_period == "24hr") {
      # Example: 150 is the NAAQS for PM10 24-hour average
      return(1)
    }
  }
  
  # Add more pollutants and averaging periods as needed
  return(NA)  # Return NA if no match is found
}


