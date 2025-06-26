library(readxl)
library(dplyr)

##### Add Site Names and Metadata #####
sites <- read_excel("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/EE Demos/AQS sites.xlsx")
sites_simple <- sites %>%
  select(
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


# Define area of interest with bounding box
bbox <- c(left = -117, bottom = 44, right = -103, top = 50)
pts <- data.frame(x = c(bbox[1], bbox[3]), y = c(bbox[2], bbox[4]))
pts_sf <- st_as_sf(pts, coords = c("x", "y"), crs = 4326)

# Retrieve elevation data if not already defined
if (!exists("elevation")) {
  elevation <- get_elev_raster(pts_sf, z = 7, clip = "bbox")
  print(elevation)  # Inspect the elevation object
}

# Convert RasterLayer to SpatRaster
elevation_terra <- rast(elevation)

# Exaggerate the elevation
exaggeration_factor <- 10
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

##### MAPPING Inset Areas #####

# LIBBY

# Specify the site name and the buffer distance (degrees)
site_name <- "Libby"
buffer <- 0.25  # Define how many degrees to expand in each direction

# Filter to get the latitude and longitude for "Libby"
libby_coords <- sites_simple %>%
  filter(site == site_name) %>%
  select(latitude, longitude) %>%
  slice(1)  # Ensures only one row is selected if there are duplicates

# Calculate the bounding box based on the buffer distance
bbox_libby <- c(
  left = libby_coords$longitude - buffer,
  bottom = libby_coords$latitude - buffer,
  right = libby_coords$longitude + buffer,
  top = libby_coords$latitude + buffer
)

# Create a polygon from the bounding box coordinates
bbox_libby_polygon <- st_polygon(list(rbind(
  c(bbox_libby["left"], bbox_libby["bottom"]),  # Bottom-left corner
  c(bbox_libby["right"], bbox_libby["bottom"]), # Bottom-right corner
  c(bbox_libby["right"], bbox_libby["top"]),    # Top-right corner
  c(bbox_libby["left"], bbox_libby["top"]),     # Top-left corner
  c(bbox_libby["left"], bbox_libby["bottom"])   # Closing the polygon
))) %>% 
  st_sfc(crs = 4326)  # Set coordinate reference system to WGS84 (lat/lon)

# Convert to a data frame for plotting in ggplot
bbox_libby_polygon_df <- st_as_sf(bbox_libby_polygon)

##### MAPPING Setup #####

# Define elevation limits and color breaks
elevation_limits <- c(-2000, 4000)
elevation_breaks <- c(-2000, 0, 10, 500, 1500, 2000, 2500, 4000)
elevation_labels <- seq(from = 0, to = 4000, by = 1000)

# Define your desired x and y limits
x_limits <- c(-117, -103)  # Longitude limits
y_limits <- c(44, 50)      # Latitude limits

##### MAP #####

# Plot with hillshade and elevation overlay
ggplot() + 
  # Hillshade layer for relief
  geom_raster(data = hillshade_masked_df, aes(x = x, y = y, fill = hillshade), alpha = .5) +
  scale_fill_gradient(low = "black", high = "transparent", guide = FALSE) +
  
  new_scale_fill() +
  # Add elevation layer for terrain shading
  geom_raster(data = elevation_masked_df, aes(x = x, y = y, fill = elevation), alpha = .4) +
  scale_fill_gradientn(
    colours = c("#394A66", "#90BBD0", "#769C92", "#D1B792", "#2B7A5F", "#9E7455", "gray", "white"),
    values = scales::rescale(elevation_breaks),
    limits = elevation_limits,
    breaks = seq(from = 0, to = 4000, by = 1000),  # Only show breaks from 0 and above
    labels = c("0", "1000", "2000", "3000","4000")  # Custom labels
  ) +
  
  # Other map layers here, like hillshade, elevation, points
  geom_sf(data = bbox_libby_polygon_df, fill = "white", color = "white", size = 1.5, alpha = 0.4) +

  geom_polygon(data = montana_border, aes(x = long, y = lat, group = group), 
               fill = NA, color = "black", size = 1) +
  
  geom_point(
    data = sites_simple %>% filter(site %in% c("Libby")),
    aes(x = longitude, y = latitude, color = monitor_type),
    size = 3
  ) +  scale_color_manual(values = c("NON-EPA FEDERAL" = "#f54D28", 
                                "SLAMS" = "#004A98",
                                "SPM" = "#009ADE")) +
  
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
    legend.title = element_blank()  # Ensures legend title is removed
  ) +

  # Use coord_sf for spatial reference
  coord_sf(
    xlim = x_limits, 
    ylim = y_limits,
    expand = FALSE  # Avoids padding around the edges
  )
