library(readxl)
library(dplyr)

##### Add Site Names and Metadata #####
sites <- read_excel("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/EE Demos/AQS sites.xlsx")
sites <- sites %>%
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

completeness <- read_excel("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/EE Demos/TSD/21_23_DV_and_completeness.xlsx") %>%
  select(site, poc, total_complete_quarters, design_value, design_value_no_flags)

# Perform the inner join (merging will only keep relevant 2021-2023 monitors present in "completeness"; will exclude new 2024 monitors like Choteau and Glendive)
merged_sites <- inner_join(sites, completeness, by = "site")

sites_simple <- merged_sites %>%
  filter(
    naaqs_primary_monitor == "Y",
    parameter_code == "88101",
    monitor_type != "NON-EPA FEDERAL"           # Exclude SPM (non-regulatory) rows
  ) %>%
  mutate(monitor_label = ifelse(total_complete_quarters == 12, 
                                "Regulatory Monitor", 
                                "Regulatory Monitor\n(does not meet data completeness requirements)"))


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

library(tigris)
# Load county boundaries from tigris
montana_counties <- counties(state = "MT", class = "sf")


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

# Define elevation limits and color breaks
elevation_limits <- c(-2000, 4000)
elevation_breaks <- c(-2000, 0, 10, 500, 1500, 2000, 2500, 4000)
elevation_labels <- seq(from = 0, to = 4000, by = 1000)

# Define your desired x and y limits
x_limits <- c(-117, -103)  # Longitude limits
y_limits <- c(44, 50)      # Latitude limits

# Plot with hillshade and elevation overlay
map <- ggplot() + 
  # Hillshade layer for relief
  geom_raster(data = hillshade_masked_df, aes(x = x, y = y, fill = hillshade), alpha = .3) +
  scale_fill_gradient(low = "black", high = "transparent", guide = FALSE) +
  
  new_scale_fill() +
  # Add elevation layer for terrain shading
  geom_raster(data = elevation_masked_df, aes(x = x, y = y, fill = elevation), alpha = .15) +
  scale_fill_gradientn(
    colours = c("#394A66", "#90BBD0", "#769C92", "#D1B792", "#2B7A5F", "#9E7455", "gray", "white"),
    values = scales::rescale(elevation_breaks),
    limits = elevation_limits,
    breaks = elevation_labels,
    labels = scales::label_comma(),
    guide = FALSE
  ) +
  geom_sf(data = montana_counties, fill = NA, color = "gray50", size = 0.5) +
  
  geom_polygon(data = montana_border, aes(x = long, y = lat, group = group), 
               fill = NA, color = "black", size = 1) +
  new_scale_fill() +
  geom_point(data = sites_simple, aes(x = longitude, y = latitude, color = monitor_label), size = 6) +
  scale_color_manual(values = c("Regulatory Monitor" = "#004A98",
                                "Regulatory Monitor\n(does not meet data completeness requirements)" = "#009ADE" 
                                )) +
  
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
    legend.position = c(0.7, 0.13),  # Adjusts legend position (x, y)
    legend.title = element_blank()  # Ensures legend title is removed
  ) +
  
  # Set the x and y limits
  xlim(x_limits) +
  ylim(y_limits) +
  coord_sf()  # You can adjust the ratio if needed

print(map)

ggsave(filename = "/Users/cameronnealy/Downloads/TSD Fig 1_reg.png", plot = map, width = 11, height = 7, dpi = 300)




