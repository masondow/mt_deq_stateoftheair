# USES DATA FROM Smoke Maps.R
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthhires)  # For high-resolution borders
library(sf)
library(ggplot2)
library(viridis)
library(stars)
library(grid)

##### Rasterize Smoke #####

# Define a bounding box in lat/lon (WGS 84, EPSG:4326)
# Coordinates (min lon, min lat, max lon, max lat)
bbox_latlon <- st_bbox(c(xmin = -125, ymin = 40, xmax = -95, ymax = 55), 
                       crs = st_crs(4326))

# Convert the bounding box to an sf object
bbox_latlon_sf <- st_as_sfc(bbox_latlon)

# Reproject the bounding box to UTM Zone 12N (or the desired projection)
bbox_projected <- st_transform(bbox_latlon_sf, crs = 32612)

# Create a grid that fits the bounding box and set the cell size in projected units (e.g., 10km)
grid <- st_make_grid(bbox_projected, cellsize = 10000)  # Cell size in meters (e.g., 10km)

# Convert grid to sf object
grid_sf <- st_sf(geometry = grid)

# Make sure `smoke_east` is projected correctly
smoke_east_projected <- st_transform(combined_shapefiles_east, crs = 32612)

# Count the number of overlaps for each grid cell
grid_sf$overlap_count <- lengths(st_intersects(grid_sf, smoke_east_projected))

cell_size <- 10000

# Rasterize the grid based on overlap counts
raster_grid <- st_rasterize(grid_sf, dx = cell_size, dy = cell_size)

# Plot the raster heat map
ggplot() +
  geom_stars(data = raster_grid) +
  scale_fill_viridis_c(option = "inferno", name = "Smoke Density") +
  labs(title = "Wildfire Smoke Density Heat Map", x = "Longitude", y = "Latitude") +
  theme_minimal()

##### Process Fire Perimeters #####

# Ensure all datasets have the same columns
common_cols <- intersect(intersect(names(CAN_fires_east), names(US_fires_east)), names(US_fires_east_2023))

# Transform coordinates of each dataset to EPSG:32612
CAN_fires_east <- st_transform(CAN_fires_east, crs = 32612)
US_fires_east <- st_transform(US_fires_east, crs = 32612)
US_fires_east_2023 <- st_transform(US_fires_east_2023, crs = 32612)

# Subset and rename columns if necessary
CAN_fires_east <- CAN_fires_east[, common_cols]
US_fires_east <- US_fires_east[, common_cols]
US_fires_east_2023 <- US_fires_east_2023[, common_cols]

# Combine all datasets into one
combined_fires_east <- rbind(CAN_fires_east, US_fires_east, US_fires_east_2023)

# Check for invalid geometries
invalid_geom <- st_is_valid(combined_fires_east)
invalid_count <- sum(!invalid_geom)

# Print the count of invalid geometries
print(paste("Number of invalid geometries:", invalid_count))

# Repair invalid geometries
combined_fires_east <- st_make_valid(combined_fires_east)

# Simplify geometries
combined_fires_east_simplified <- st_simplify(combined_fires_east, dTolerance = 1000)  # Adjust tolerance as needed

##### MAPPING #####

# Download US states and Canadian provinces as sf objects
us_states <- ne_states(country = "United States of America", returnclass = "sf")
can_provinces <- ne_states(country = "Canada", returnclass = "sf")

# Transform to the same CRS as your grid_sf (assuming grid_sf has a CRS set)
us_states <- st_transform(us_states, st_crs(grid_sf))
can_provinces <- st_transform(can_provinces, st_crs(grid_sf))

# Filter Montana from the US states data
montana <- us_states[us_states$name == "Montana", ]
other_states <- us_states[us_states$name != "Montana", ]

# Obtain the bounding box of the raster grid
raster_bbox <- st_bbox(raster_grid)

# Filter the fire data to only include features within the bounding box of the raster grid
combined_fires_east_simplified <- st_crop(combined_fires_east_simplified, raster_bbox)

# Create dummy data for the legend
legend_data <- data.frame(
  id = factor(c("Fire Area")),
  x = c(1),
  y = c(1)
)

ggplot() + 
  # Plot the raster heatmap
  geom_stars(data = raster_grid) + 
  scale_fill_viridis_c(option = "inferno", 
                       name = "Satellite Smoke Detects", 
                       guide = guide_colorbar(title.position = "top", 
                                              title.hjust = 0.5)) + 
  # Add other US state borders in white
  geom_sf(data = other_states, fill = NA, color = "white", size = 0.3) + 
  # Add Canadian province borders in white
  geom_sf(data = can_provinces, fill = NA, color = "white", size = 0.3) + 
  # Add Montana border in black
  geom_sf(data = montana, fill = NA, color = "white", size = 3) + 
  # Add the simplified fire data
  geom_sf(data = combined_fires_east_simplified, fill = "black", color = "black", size = 0.5, alpha = 1) + 
  # Add a dummy layer to create a legend for fire areas
  geom_point(data = legend_data, aes(x = x, y = y, color = id), size = 5, shape = 15, show.legend = TRUE) + 
  scale_color_manual(values = c("black"), name = "", labels = c("Fire Area")) + 
  # Customize plot appearance
  labs(title = bquote(atop(
    bold("EASTERN MT 2003-2023 Highest 1% PM"[2.5]*" Days"), 
    "Aggregated Smoke and Fires"
  ))) + 
  theme_minimal() + 
  # Adjust margins and spacing
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(
      hjust = 0.5, 
      margin = margin(b = -10)  # Reduce space below the title
    ),
    legend.position = "bottom",  # Move legend to the bottom
    legend.margin = margin(t = -11),  # Reduce space above the legend
    legend.box.margin = margin(t = -10),  # Reduce space above the legend box
    legend.key.size = unit(0.5, "cm"),  # Adjust legend key size if needed
    legend.text = element_text(size = 8),  # Adjust legend text size if needed
    legend.title = element_text(size = 10)  # Adjust legend title size if needed
  ) + 
  # Zoom to the extent of the raster grid
  coord_sf(xlim = c(st_bbox(raster_grid)[1], st_bbox(raster_grid)[3]),
           ylim = c(st_bbox(raster_grid)[2], st_bbox(raster_grid)[4]))





