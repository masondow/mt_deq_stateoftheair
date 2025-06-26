##### 2023 - 2023 NBM Met Data #####
NBM <- readRDS("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/EE Demos/TSD/Libby 5 factor analysis/met_analysis/LBBM8_2021_2023_NBM_data.rds")

# Calculate the average of daily maximum MHT for each season
seasonal_MHT <- NBM %>%
  filter(YEAR_MST >= 2021 & YEAR_MST <= 2023) %>%
  # Create a column to categorize months into "Warm" and "Cool" seasons
  mutate(season = ifelse(MONTH_MST %in% 5:10, "Warm", "Cool")) %>%
  
  # Group by year, season, and each day to find the daily max MHT
  group_by(YEAR_MST, season, MONTH_MST, DAY_MST) %>%
  summarise(daily_max_MHT = max(MHT, na.rm = TRUE), .groups = "drop") %>%
  
  # Now group by year and season to calculate the average of daily maximum MHT values
  group_by(season) %>%
  summarise(avg_max_MHT = mean(daily_max_MHT, na.rm = TRUE)*100, .groups = "drop")

# View the results
seasonal_MHT
