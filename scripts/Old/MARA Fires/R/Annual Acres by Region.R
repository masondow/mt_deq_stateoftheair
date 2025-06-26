# Load necessary libraries
library(sf)
library(dplyr)
library(lubridate)
library(stringr)

# Read shapefile
US_shp_data <- st_read("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/MARA Fires/R/Data/mtbs_perimeter_data (1)/mtbs_perims_DD.shp")
CAN_shp_data <- st_read("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/MARA Fires/R/Data/nbac_1972_2023_20240530_shp/nbac_1972_2023_20240530.shp")
US_shp_data_2023 <- st_read("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/MARA Fires/R/Data/2023 US perimeters/2023_US_fire_perimeter_data.geojson")

##### US Fire Data #####

# US data
US_shp_data_df <- US_shp_data %>%
  as.data.frame() %>%
  select(Event_ID, Incid_Type, BurnBndAc, Ig_Date) %>%
  rename(
    ID = Event_ID,
    type = Incid_Type,
    acres = BurnBndAc,
    sdate = Ig_Date
  )

# Create a regex pattern for state codes
state_pattern <- "^WA|^OR|^ID|^CA|^NV|^MT|^WY|^UT"

# Filter
US_shp_data_df <- US_shp_data_df %>%
  filter(str_detect(ID, state_pattern) & 
           type == "Wildfire") %>%
  mutate(sdate = as.Date(sdate)) %>%
  mutate(Year = format(sdate, "%Y"))

##### US 2023 Fire Data (gap fill) #####

# US 2023 supplement
US_shp_data_2023_df <- US_shp_data_2023 %>%
  as.data.frame() %>%
  mutate(type = "Wildfire") %>%
  select(UNIQFIREID, type, GISACRES, DISCOVERYDATETIME) %>%
  rename(
    ID = UNIQFIREID,
    type = type,
    acres = GISACRES,
    sdate = DISCOVERYDATETIME
  )

# Create a regex pattern to match state codes in the 6th and 7th position
state_pattern_2023 <- "^.{5}(WA|OR|ID|CA|NV|MT|WY|UT)"

# Filter the data frame based on the ID column
US_shp_data_2023_df <- US_shp_data_2023_df %>%
  filter(str_detect(ID, state_pattern_2023) &
           acres > 1000)

# Convert sdate from milliseconds since Unix epoch to YYYY-MM-DD in MST
US_shp_data_2023_df <- US_shp_data_2023_df %>%
  mutate(
    # Convert milliseconds to seconds
    sdate_seconds = sdate / 1000,
    # Convert to POSIXct in UTC
    sdate_utc = as.POSIXct(sdate_seconds, origin = "1970-01-01", tz = "UTC"),
    # Convert to MST (UTC-7)
    sdate_mst = with_tz(sdate_utc, tzone = "MST"),
    # Format to YYYY-MM-DD
    sdate = format(sdate_mst, "%Y-%m-%d"),
    sdate = as.Date(sdate),
    Year = format(sdate, "%Y"),
    Year = format(sdate, "%Y")
  )

# Drop intermediate columns
US_shp_data_2023_df <- US_shp_data_2023_df %>%
  select(-sdate_seconds, -sdate_utc, -sdate_mst)

# Combine data frames by stacking rows
US_shp_data_df <- bind_rows(US_shp_data_2023_df, US_shp_data_df)


##### CAN Fire Data #####
CAN_shp_data_df <- CAN_shp_data %>%
  as.data.frame() %>%
  filter(is.na(PRESCRIBED) | PRESCRIBED != "true") %>%  # Exclude rows where PRESCRIBED is "true"
  mutate(
    acres = ADJ_HA * 2.47105,  # Convert hectares to acres
    sdate = pmin(HS_SDATE, AG_SDATE, na.rm = TRUE),  # Get the earlier of the two dates, ignoring NA
    PRESCRIBED = "Wildfire"
  ) %>%
  filter(ADMIN_AREA %in% c("AB", "BC", "SK", "YT", "NT") &
           acres > 1000) %>%  # Filter by ADMIN_AREA
  select(
    ID = NFIREID, 
    type = PRESCRIBED,  # Set type to "Wildfire" for all rows
    acres, 
    sdate
  ) %>%
  mutate(Year = format(sdate, "%Y"))

##### Annual Data Calculations #####

CAN_annual_acres <- CAN_shp_data_df %>%
  group_by(Year) %>%
  summarise(total_acres = sum(acres, na.rm = TRUE)) %>%
  mutate(Region = "Canada")

US_annual_acres <- US_shp_data_df %>%
  group_by(Year) %>%
  summarise(total_acres = sum(acres, na.rm = TRUE)) %>%
  mutate(Region = "US")

# Combine data frames by stacking rows
annual_acres <- bind_rows(CAN_annual_acres, US_annual_acres)

##### PLOTTING #####

# Convert Year column to numeric and filter for years 1984-2023
annual_acres <- annual_acres %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year >= 1984 & Year <= 2023)

# Plot the combined data with custom legend title
p <- ggplot(annual_acres, aes(x = Year, y = total_acres, fill = Region)) +
  geom_bar(stat = "identity", position = "dodge") +  # Use dodge to separate bars by Region
  geom_smooth(aes(group = Region, color = Region), method = "lm", se = FALSE, linetype = "dashed", size = .75) +  # Add trendlines for each region
  labs(
    title = "Wildfire Acres Burned",
    x = "",
    y = "Acres Burned",
    fill = "Region (acres and trend line)",  # Update the legend title for fill
    color = "Region (acres and trend line)"  # Update the legend title for color
  ) +
  scale_x_continuous(
    limits = c(1983, 2024),
    breaks = seq(1985, 2023, by = 5),
    expand = c(0, 0)  # Remove extra space around the plot limits
  ) +
  scale_y_continuous(
    labels = scales::comma,
    expand = c(.01, 0)  # Format y-axis labels with commas
  ) +
  scale_fill_manual(values = c("Canada" = "#009ADE", "US" = "#004A98")) +  # Manually set custom colors
  scale_color_manual(values = c("Canada" = "#009ADE", "US" = "#004A98")) +  # Set the same colors for the trendlines
  theme_minimal() +
  theme(
    plot.margin = margin(1, 1, 1, 1),  # Remove margins around the plot
    legend.position = c(0.4, 0.9),     # Position legend at the top right
    legend.justification = c(1, 1),    # Anchor the legend at the top right
    
    # Adjust text sizes
    plot.title = element_text(size = 16, face = "bold"),  # Title text size
    axis.title.x = element_text(size = 14),  # X-axis title text size
    axis.title.y = element_text(size = 14),  # Y-axis title text size
    axis.text.x = element_text(size = 12),   # X-axis tick text size
    axis.text.y = element_text(size = 12),   # Y-axis tick text size
    legend.text = element_text(size = 12),   # Legend text size
    legend.title = element_text(size = 14),  # Legend title text size
    
    # Set transparent background
    panel.background = element_rect(fill = "transparent", color = NA), # Background of the plotting area
    plot.background = element_rect(fill = "transparent", color = NA),  # Background of the entire plot
    legend.background = element_rect(fill = "transparent", color = NA), # Background of the legend
    
    # Vertical grid lines
    panel.grid.major.x = element_line(color = alpha("gray", 0.2), size = 0.5),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = alpha("gray", 0.2), size = 0.5),
    panel.grid.minor.y = element_line(color = alpha("gray", 0.2), size = 0.5)
  )

# Print the plot to check
print(p)

# Save the plot as a PNG with transparent background
ggsave("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/MARA Fires/Final Figures/annual_wildfire_acres.png", plot = p, bg = "transparent", width = 10, height = 6, dpi = 300)



