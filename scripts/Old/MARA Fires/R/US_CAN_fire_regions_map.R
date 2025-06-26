# Load necessary libraries
library(ggplot2)
library(rnaturalearth)
library(sf)

# Get the US and Canada map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Get the state and province level boundaries for US and Canada
states_provinces <- ne_states(country = c("united states of america", "canada"), returnclass = "sf")

# Define the regions to highlight
canadian_provinces <- c("Yukon", "Northwest Territories", "British Columbia", "Alberta", "Saskatchewan")
us_states <- c("Washington", "Oregon", "California", "Idaho", "Nevada", "Utah", "Wyoming", "Montana")

# Filter the data to only include the desired regions
highlighted_regions <- states_provinces %>%
  filter(name %in% c(canadian_provinces, us_states))

# Create a new variable for color grouping
highlighted_regions$color_group <- ifelse(highlighted_regions$name %in% canadian_provinces, "canadian_provinces", "us_states")

# Create the map plot
p <- ggplot(data = highlighted_regions) +
  geom_sf(aes(fill = color_group), color = "white", size = 0.3) +  # White borders between regions
  scale_fill_manual(
    values = c("canadian_provinces" = "#009ADE",  # Color for Canadian provinces
               "us_states" = "#004A98")           # Color for US states
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.text = element_blank(),  # Remove axis text
    axis.ticks = element_blank(),  # Remove axis ticks
    legend.position = "none",  # Remove the legend
    panel.grid.major = element_blank(),  # Remove major grid lines (lat/lon lines)
    panel.grid.minor = element_blank()   # Remove minor grid lines (lat/lon lines)
  ) +
  coord_sf(crs = st_crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=46.8797 +lon_0=-110.3626"))  # Albers Equal Area projection centered on Montana

# Print the plot to check
print(p)

# Save the plot as a PNG with transparent background
ggsave("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/MARA Fires/Final Figures/US_CAN_fire_region_map.png", plot = p, bg = "transparent", width = 10, height = 6, dpi = 300)






