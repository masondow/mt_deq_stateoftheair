##### READ ME #####
# Run 'libby_topo_and_elev_transect.R' and 'Mixing height averages.R' first
# Then run this script to add mixing height to transect profile

# Convert Distance to miles and Elevation to feet
elevation_profile <- elevation_profile %>%
  mutate(
    Distance = Distance * 0.621371,    # 1 km = 0.621371 miles (converting to miles)
    Elevation = Elevation * 3.28084     # 1 m = 3.28084 feet (converting to feet)
  )

seasonal_MHT <- seasonal_MHT %>%
  mutate(avg_max_MHT = avg_max_MHT + 2070) # add elevation of LBBM8 (Libby NBM point) to MHT feet AGL for true elevation

# Create a copy of seasonal_MHT where you pivot the 'avg_max_MHT' values to separate columns
seasonal_MHT_wide <- seasonal_MHT %>%
  pivot_wider(names_from = season, values_from = avg_max_MHT)

# Join seasonal_MHT data with elevation_profile
elevation_profile <- elevation_profile %>%
  # Add the seasonal MHT values as separate columns (Cool and Warm)
  mutate(
    Cool = seasonal_MHT_wide$Cool,
    Warm = seasonal_MHT_wide$Warm
  )

# View the updated data
head(elevation_profile)

# Plot the elevation profile with a line and filled area below
ggplot(elevation_profile, aes(x = Distance, y = Elevation)) +
  # Elevation profile layer with area fill in dark blue
  geom_line(aes(x = Distance, y = Warm), 
            color = "#F54D28", size = 3) +
  geom_ribbon(aes(ymin = Cool, ymax = Warm), fill = "#F54D28", alpha = .3) +
  # Elevation profile layer with area fill in dark blue
  geom_line(aes(x = Distance, y = Cool), 
            color = "#009ADE", size = 3) +
  geom_ribbon(aes(ymin = 0, ymax = Cool), fill = "#009ADE", alpha = .3) +
  # Fill the area below the line in black
  geom_ribbon(aes(ymin = 0, ymax = Elevation), fill = "#004A98", alpha = 1) +
  # Add the line on top of the filled area
  geom_line(color = "black", size = 1) +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(expand=c(0,0),
                     breaks=seq(0,40, by=5)) +
  labs(
    x = "Distance Along Transect (miles)",
    y = "Elevation (feet)",
    title = ""
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 14),  # Adjust x-axis text size
    axis.text.y = element_text(size = 14),  # Adjust y-axis text size
    axis.title.x = element_text(size = 16),  # Adjust x-axis title size
    axis.title.y = element_text(size = 16),  # Adjust y-axis title size
    plot.title = element_text(size = 16, hjust = 0.5)  # Adjust title size and center it
  )



# Plot elevation profile and seasonal MHT with facet wrapping
ggplot() +
  # Elevation profile layer with area fill in dark blue
  geom_line(data = elevation_profile, aes(x = Distance, y = Warm), 
            color = "#F54D28", size = 3) +
  # Elevation profile layer with area fill in dark blue
  geom_line(data = elevation_profile, aes(x = Distance, y = Cool), 
              color = "#009ADE", size = 3) +
  # Elevation profile layer with area fill in dark blue
  geom_ribbon(data = elevation_profile, aes(x = Distance, ymin = 0, ymax = Elevation), 
              fill = "#004A98", alpha = 1) +
  labs(
    x = "Distance (miles) / Season",
    y = "Elevation (feet) / Average Max MHT",
    title = "Elevation Profile and Seasonal Average Maximum MHT"
  ) +
  theme_minimal()
