library(readxl)
library(dplyr)

# Read the data from your Excel file
emis_all <- read_excel("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/Legislative/wildfire_emission_contribution/2023_emis.xlsx")

# Extract unique pollutants
unique_pollutants <- unique(emis_all$Pollutant)

# Loop over each pollutant
for (pollutant in unique_pollutants) {
  
  # Filter the data for the current pollutant (directly reference Pollutant column)
  emis <- emis_all %>%
    filter(State == "MT" & Pollutant == pollutant)  # No need for `!!` here
  
  # Extract specific sectors
  fires_wild <- emis %>% filter(Sector == "Fires - Wildfires")
  fires_pres <- emis %>% filter(Sector == "Fires - Prescribed Fires")
  
  # Find the sector with the greatest emissions (excluding the two fire categories)
  top_sector <- emis %>%
    filter(!(Sector %in% c("Fires - Wildfires", "Fires - Prescribed Fires"))) %>%
    arrange(desc(emissions2023)) %>%
    slice(1)  # Select the top sector
  
  # Sum remaining emissions into "Other"
  other_emissions <- emis %>%
    filter(!(Sector %in% c("Fires - Wildfires", "Fires - Prescribed Fires", top_sector$Sector))) %>%
    summarise(Sector = "All Other", emissions2023 = sum(emissions2023, na.rm = TRUE))
  
  # Combine into a final dataset
  pie_data <- bind_rows(fires_wild, fires_pres, top_sector, other_emissions)
  
  library(ggplot2)
  library(ggrepel)
  # Define colors for fixed categories
  color_map <- c(
    "Fires - Wildfires" = "#e53935",   # Red
    "Fires - Prescribed Fires" = "#fb8c00",  # Blue
    "All Other" = "#e8eaf6"  # Gray
  )
  
  # Dynamically assign a color for the top_sector
  if (nrow(top_sector) > 0) {
    top_sector_color <- "#e8f5e9"  # Green (or pick dynamically)
    color_map[top_sector$Sector] <- top_sector_color
  }
  
  # Define priority order
  priority_sectors <- c("Fires - Wildfires", "Fires - Prescribed Fires")
  
  # Identify top_sector dynamically (if it exists)
  if (nrow(top_sector) > 0) {
    top_sector_name <- top_sector$Sector
  } else {
    top_sector_name <- NULL
  }
  
  # Define full sector order (ensuring Wildfires, Prescribed Fires, then Top Sector, then Other)
  ordered_sectors <- c(priority_sectors, top_sector_name, setdiff(pie_data$Sector, c(priority_sectors, top_sector_name)))
  
  # Apply ordering to pie_data
  pie_data <- pie_data %>%
    mutate(Sector = factor(Sector, levels = ordered_sectors))
  
  # Ensure the color order matches pie_data
  pie_colors <- color_map[levels(pie_data$Sector)]
  
  library(stringr)
  # Create formatted legend labels with commas
  formatted_labels <- paste0(pie_data$Sector, " (", formatC(pie_data$emissions2023, format = "f", big.mark = ",", digits = 0), ")")
  # Wrap long legend labels (set width to 20 characters, adjust as needed)
  formatted_labels <- str_wrap(formatted_labels, width = 33)
  
  # Create pie chart
  p <- ggplot(pie_data, aes(x = "", y = emissions2023, fill = Sector)) +
    geom_bar(stat = "identity", width = 1, color = "white", size = 1) + 
    coord_polar("y", start = 0) +
    scale_fill_manual(values = pie_colors, labels = formatted_labels, name = "Sector (tpy emissions)") +  # Add legend title
    labs(title = paste0(pollutant)) +  # Title of the chart
    theme_void() +
    theme(
      legend.title = element_text(face = "bold"),  # Make the legend title bold
      plot.title = element_text(
        face = "bold",           # Make the title bold
        size = 16,              # Increase title size
        hjust = .5,           # Position title slightly left (negative value moves it left)
        vjust = -4,              # Align title vertically
        margin = margin(t = 30) # Add space above the title
      ),
      plot.margin = margin(10, 10, 10, 10),  # Add space around the plot to avoid clipping
      axis.text = element_blank(),  # Hide axis labels
      axis.ticks = element_blank()  # Hide axis ticks
    )
  
  
  print(p)
  
  ggsave(paste0("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/Legislative/wildfire_emission_contribution/pies/",pollutant,"_pie.png"), plot = p, width = 6, height = 6, dpi = 300)
}

