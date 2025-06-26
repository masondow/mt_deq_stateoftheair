library(readxl)
library(geosphere)

##### Q/d PM2.5 #####
pt_emissions_pm25 <- read_excel("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/EE Demos/TSD/Libby 5 factor analysis/emissions data/WA_ID_Libby_Facility emissions PM25.xlsx")

# Reference point (e.g., Libby, MT coordinates)
ref_lat <- 48.3926  # Replace with your reference latitude
ref_lon <- -115.5564  # Replace with your reference longitude

# Add distance columns (in km and miles) to the data frame
pt_emissions_pm25 <- pt_emissions_pm25 %>%
  mutate(
    Distance_km = distHaversine(
      cbind(Lon, Lat),  # Columns of facility longitude and latitude
      c(ref_lon, ref_lat)  # Reference point as a vector
    ) / 1000,  # Convert meters to kilometers
    Distance_mi = Distance_km * 0.621371  # Convert kilometers to miles
  )

pt_emissions_pm25 <- pt_emissions_pm25 %>%
  mutate(Qd = `2022 Emissions (Tons)` / Distance_km)

# View updated data frame
head(pt_emissions_pm25)

##### Q/d NOx + SO2 #####
pt_emissions_NoxSo2 <- read_excel("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/EE Demos/TSD/Libby 5 factor analysis/emissions data/WA_ID_Libby_Facility emissions Nox_SO2.xlsx")

pt_emissions_NoxSo2 <- pt_emissions_NoxSo2 %>%
  mutate(
    `2022 Emissions (Tons)` = as.numeric(na_if(as.character(`2022 Emissions (Tons)`), "-")),
    `2026 Emissions (Tons)` = as.numeric(na_if(as.character(`2026 Emissions (Tons)`), "-")),
    `2032 Emissions (Tons)` = as.numeric(na_if(as.character(`2032 Emissions (Tons)`), "-")),
    `2038 Emissions (Tons)` = as.numeric(na_if(as.character(`2038 Emissions (Tons)`), "-"))
  ) %>%
  group_by(`EIS Facility ID`, `EPA Region`, State, `State-County`, `Facility Name`, `Facility Type`, `NAICS Description`, Lat, Lon) %>%
  summarise(
    Pollutant = "NOx+SO2",
    `2022 Emissions (Tons)` = sum(`2022 Emissions (Tons)`, na.rm = TRUE),
    `2026 Emissions (Tons)` = sum(`2026 Emissions (Tons)`, na.rm = TRUE),
    `2032 Emissions (Tons)` = sum(`2032 Emissions (Tons)`, na.rm = TRUE),
    `2038 Emissions (Tons)` = sum(`2038 Emissions (Tons)`, na.rm = TRUE),
    .groups = "drop"
  )

# Add distance columns (in km and miles) to the data frame
pt_emissions_NoxSo2 <- pt_emissions_NoxSo2 %>%
  mutate(
    Distance_km = distHaversine(
      cbind(Lon, Lat),  # Columns of facility longitude and latitude
      c(ref_lon, ref_lat)  # Reference point as a vector
    ) / 1000,  # Convert meters to kilometers
    Distance_mi = Distance_km * 0.621371  # Convert kilometers to miles
  )

pt_emissions_NoxSo2 <- pt_emissions_NoxSo2 %>%
  mutate(Qd = `2022 Emissions (Tons)` / Distance_km)

# View updated data frame
head(pt_emissions_NoxSo2)
