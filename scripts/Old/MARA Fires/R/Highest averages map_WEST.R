library(ggplot2)
library(rnaturalearth)
library(sf)
library(viridis)

# Download US states as sf object
us_states <- ne_states(country = "United States of America", returnclass = "sf")

# Filter Montana from the US states data
montana <- us_states[us_states$name == "Montana", ]

# Define slope (m) and intercept (b); actual intercept is 64; adjusted to not overlap points so much
m <- -1
b <- -64.3

# Calculate x-values for y = 49 and y = 45
x_upper <- (49.05 - b) / m
x_lower <- (44.95 - b) / m

# Plot with point data on top
ggplot(west_highest_averages, aes(x = longitude, y = latitude, fill = average_arithmetic_mean)) +
  geom_sf(data = montana, fill = "black", color = "black", size = 1) +  # Add Montana outline
  geom_segment(aes(x = x_lower, xend = x_upper, y = 44.95, yend = 49.05), color = "white", size = 5) +  # Add the line segment
  geom_point(shape = 21, size = 20, color = "black") +  # Shape 21 allows for filling, move this after other layers
  scale_fill_viridis_c(option = "inferno", name = bquote("PM"[2.5] ~ " (µg/m"^3*")")) +  # Use a color scale with proper units
  labs(title = bquote(atop(bold("Western Montana Smokiest Days (2003-2023)"), 
                           "Daily Averages - Highest 1% PM"[2.5] ~ "Days")),
       x = "Longitude",
       y = "Latitude") +
  theme_minimal() +
  theme(legend.position = "bottom",  # Move legend to the bottom
        legend.margin = margin(t = -30),  # Adjust space above the legend
        plot.title = element_text(hjust = 0.5, margin = margin(b = -15)),  # Center title and adjust space below it
        axis.text = element_blank(),  # Remove axis text
        axis.title = element_blank(),  # Remove axis titles
        axis.ticks = element_blank(),  # Remove axis ticks
        panel.grid = element_blank())  # Remove panel grid
