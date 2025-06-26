library(httr)
library(jsonlite)
library(sf)

# URL for the ArcGIS REST service
url <- "https://apps.fs.usda.gov/arcx/rest/services/EDW/EDW_FireOccurrenceAndPerimeter_01/MapServer/29/query"

# Query parameters
query_params <- list(
  where = "1=1",  # Fetch all data
  outFields = "*", # Fetch all fields
  f = "geojson"    # Format the output as GeoJSON
)

# Make the GET request
response <- GET(url, query = query_params)

# Parse the GeoJSON response
geojson_data <- content(response, as = "text")
geojson_sf <- st_read(geojson_data, quiet = FALSE)

# View the first few rows of the data
print(geojson_sf)  

# Save the sf object to a GeoJSON file
st_write(geojson_sf, "/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/MARA Fires/R/2023 US perimeters/2023_US_fire_perimeter_data.geojson", driver = "GeoJSON")
