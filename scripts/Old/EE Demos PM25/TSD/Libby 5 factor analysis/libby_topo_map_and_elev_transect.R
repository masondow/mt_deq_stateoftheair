library(readxl)
library(dplyr)

##### Add Site Names and Metadata #####
sites <- read_excel("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/EE Demos/AQS sites.xlsx")
sites_simple <- sites %>%
  dplyr::select(
    site,
    parameter_code,
    measurement_scale,
    measurement_scale_def,
    monitoring_objective,
    naaqs_primary_monitor,
    monitor_type,
    networks,
    latitude,
    longitude,
    datum,
  )

# Assuming sites_simple is your data frame
sites_simple <- sites_simple %>%
  mutate(radius = case_when(
    measurement_scale == "NEIGHBORHOOD" ~ 4000,    # 4 km in meters
    measurement_scale == "REGIONAL SCALE" ~ 300000, # 300 km in meters
    measurement_scale == "MIDDLE SCALE" ~ 500,      # 0.5 km in meters
    measurement_scale == "MICROSCALE" ~ 100,        # 0.1 km in meters
    TRUE ~ NA_real_                                 # Handle any other cases, if needed
  ))

##### IMPORT MAP DATA #####

# Load necessary libraries
library(ggplot2)
library(elevatr)
library(raster)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(ggnewscale)
library(maps)
library(terra)
library(ggforce)  # Needed for geom_circle

# get other state and Canadian borders
world <- ne_countries(scale = "medium", returnclass = "sf")
states_provinces <- ne_states(country = c("united states of america", "canada"), returnclass = "sf")
# Filter for Montana (used for masking)
montana_sf <- states_provinces %>% 
  filter(name == "Montana")

# get Montana border (with size-adjustable border line)
states <- map_data("state")
montana_border <- subset(states, region == "montana")


# Specify the site name and the buffer distance (degrees)
site_name <- "Libby"
buffer <- .25  # Define how many degrees to expand in each direction

# Filter to get the latitude and longitude for "Libby"
libby_coords <- sites_simple %>%
  filter(site == site_name) %>%
  dplyr::select(latitude, longitude) %>%
  slice(1)  # Ensures only one row is dplyr::selected if there are duplicates

# Calculate the bounding box based on the buffer distance
bbox <- c(
  left = libby_coords$longitude - buffer,
  bottom = libby_coords$latitude - buffer,
  right = libby_coords$longitude + buffer,
  top = libby_coords$latitude + buffer
)

# Print bounding box
print(bbox)

pts <- data.frame(x = c(bbox[1], bbox[3]), y = c(bbox[2], bbox[4]))
pts_sf <- st_as_sf(pts, coords = c("x", "y"), crs = 4326)

# Retrieve elevation data if not already defined
if (!exists("elevation")) {
  elevation <- get_elev_raster(pts_sf, z = 10, clip = "bbox")
  print(elevation)  # Inspect the elevation object
}

# Convert RasterLayer to SpatRaster
elevation_terra <- rast(elevation)

# Exaggerate the elevation
exaggeration_factor <- 40
exaggerated_elevation <- elevation_terra * exaggeration_factor

# Create slope and aspect layers
slope <- terrain(exaggerated_elevation, v = "slope", unit = "radians")
aspect <- terrain(exaggerated_elevation, v = "aspect", unit = "radians")

# Create hillshade layer
hillshade <- shade(slope, aspect, angle = 45, direction = 315)

# Mask your elevation data outside of Montana using the montana_sf object
elevation_masked <- mask(elevation_terra, montana_sf)

# Convert masked elevation to a data frame for ggplot
elevation_masked_df <- as.data.frame(elevation_masked, xy = TRUE)
colnames(elevation_masked_df) <- c("x", "y", "elevation")

# Mask your hillshade data outside of Montana using the montana_sf object
hillshade_masked <- mask(hillshade, montana_sf)

# Convert masked hillshade to a data frame for ggplot
hillshade_masked_df <- as.data.frame(hillshade_masked, xy = TRUE)
colnames(hillshade_masked_df) <- c("x", "y", "hillshade")

##### GET TRANSECT #####

# Define your start and end coordinates for the transect
# Example coordinates (replace with your actual coordinates)
# Define start and end coordinates for the transect
start_coords <- c(-115.69164966623086, 48.22245539987944)
end_coords <- c(-115.42066511148008, 48.55364405598448) 

# Step 1: Create an sf line for the transect with correct CRS
transect <- st_sfc(st_linestring(rbind(start_coords, end_coords)), crs = st_crs(elevation))

# Convert to an sf object
transect_sf <- st_sf(geometry = transect)

# Verify the transect line
transect_sf



# Step 1: Reproject the line to a projected CRS (e.g., UTM zone 11N for the western U.S.)
transect_proj <- st_transform(transect_sf, crs = crs(elevation)) 

# Step 2: Use st_segmentize to create points every 50 meters along the line
distance_interval <- 100  # Set the distance in meters between points
transect_densified <- st_segmentize(transect_proj, dfMaxLength = distance_interval)

# Step 3: Extract the vertices as points
sampled_points <- st_cast(transect_densified, "POINT")

# Step 4: Transform the points back to the original CRS to match the elevation raster
sampled_points <- st_transform(sampled_points, crs = st_crs(elevation))

# Convert points to SpatialPoints for compatibility with raster::extract
sampled_points_sp <- as(sampled_points, "Spatial")

# Step 5: Extract elevation values from the raster at each point
elevation_values <- raster::extract(elevation, sampled_points_sp)

# Step 6: Calculate distances along the transect for plotting
coords <- st_coordinates(sampled_points)
distances <- c(0, cumsum(sqrt(diff(coords[,1])^2 + diff(coords[,2])^2))*100)

# Step 7: Combine distances and elevation data for plotting
elevation_profile <- data.frame(
  Distance = distances,
  Elevation = elevation_values
)

# Plot the elevation profile
plot(elevation_profile$Distance, elevation_profile$Elevation, type = "l",
     xlab = "Distance along Transect (m)", ylab = "Elevation (km)",
     main = "Elevation Profile Along Transect")

# Plot the elevation profile with a line and filled area below
ggplot(elevation_profile, aes(x = Distance, y = Elevation)) +
  # Fill the area below the line in black
  geom_ribbon(aes(ymin = 0, ymax = Elevation), fill = "#004A98", alpha = 1) +
  # Add the line on top of the filled area
  geom_line(color = "black", size = 1) +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(expand=c(0,0),
                     breaks=seq(0,40, by=5)) +
  labs(
    x = "Distance Along Transect (km)",
    y = "Elevation (m)",
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

# Create a data frame from the coords matrix
coords_df <- data.frame(
  longitude = coords[, 1],
  latitude = coords[, 2]
)

##### PLOT MAP ######

# Define elevation limits and color breaks
elevation_limits <- c(-2000, 4000)
elevation_breaks <- c(-2000, 0, 10, 500, 1500, 2000, 2500, 4000)
elevation_labels <- seq(from = 0, to = 4000, by = 1000)

# Using the bbox to define x and y limits dynamically
x_limits <- c(bbox["left"], bbox["right"])  # Longitude limits
y_limits <- c(bbox["bottom"], bbox["top"])  # Latitude limits

# Plot with hillshade and elevation overlay
ggplot() + 
  # Hillshade layer for relief
  geom_raster(data = hillshade_masked_df, aes(x = x, y = y, fill = hillshade), alpha = .8) +
  scale_fill_gradient(low = "black", high = "transparent", guide = FALSE) +
  
  new_scale_fill() +
  # Add elevation layer for terrain shading
  geom_raster(data = elevation_masked_df, aes(x = x, y = y, fill = elevation), alpha = .7) +
  scale_fill_gradientn(
    colours = c("#394A66", "#90BBD0", "#769C92", "#D1B792", "#2B7A5F", "#9E7455", "gray", "white"),
    values = scales::rescale(elevation_breaks),
    limits = elevation_limits,
    breaks = elevation_labels,
    labels = scales::label_comma(),
    guide = FALSE
  ) +
    new_scale_fill() +
  
  # Add the transect line using geom_line
  geom_line(data = coords_df, aes(x = longitude, y = latitude), color = "white", size = 2, alpha= .7) +
  
  geom_point(
    data = sites_simple %>% filter(site %in% c("Frenchtown", "Libby")),
    aes(x = longitude, y = latitude, color = monitor_type),
    size = 3
  ) +  scale_color_manual(values = c("NON-EPA FEDERAL" = "#f54D28", 
                                     "SLAMS" = "#004A98",
                                     "SPM" = "#009ADE")) +
  
  geom_sf(data = montana_sf, fill = NA, color = "blue") +
  # Add the transect line
  # Add map labels and scales
  labs(
    title = "Montana Shaded Relief Map",
    x = "Longitude",
    y = "Latitude"
  ) + 
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    plot.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",  # Adjusts legend position (x, y)
    legend.title = element_blank() # Ensures legend title is removed
  ) +
  
  # Set the x and y limits
  xlim(x_limits) +
  ylim(y_limits) +
  coord_sf(expand = FALSE) +
  annotation_scale(location = "br", width_hint = 0.8, text_col = "white") +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         style = north_arrow_fancy_orienteering())



