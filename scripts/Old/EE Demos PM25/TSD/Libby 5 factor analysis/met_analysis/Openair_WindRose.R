library(dplyr)
library(lubridate)
library(stringr)
library(readxl)
library(RAQSAPI)
library(magrittr)
library(ggplot2)

##### 2021 - 2023 AQS Data #####

daily_pm25 <- readRDS("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/DATA/2003_2023_daily_pm25_88101_88502_all_MT.rds")
hourly_pm25 <- readRDS("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/DATA/2003_2023_hourly_pm25_88101_88502_all_MT.rds")

##### 2023 - 2023 NBM Met Data #####
NBM <- readRDS("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/EE Demos/TSD/Libby 5 factor analysis/met_analysis/LBBM8_2021_2023_NBM_data.rds")

# Define qualifiers (for filtering later)
qualifiers <- c(
  "RT - Wildfire-U. S.",
  "RF - Fire - Canadian.",
  "IF - Fire - Canadian.",
  "IT - Wildfire-U. S.",
  "E - Forest Fire."
)

Libby_hourly <- hourly_pm25 %>% 
  filter(county_code == "053" & 
           site_number == "0018" & 
           poc == "3" & 
           parameter_code == "88101") %>% 
  mutate(
    YEAR_MST = year(ymd(date_local)),
    MONTH_MST = month(ymd(date_local)),
    DAY_MST = day(ymd(date_local))
  ) %>% 
  # Rename time_local to HOUR_MST and extract the hour as integer
  rename(HOUR_MST = time_local) %>% 
  mutate(
    # Extract the hour part and convert it to integer
    HOUR_MST = as.integer(str_extract(HOUR_MST, "^\\d{2}"))
  ) %>% 
  filter(YEAR_MST >= 2021 & YEAR_MST <= 2023 &
           !is.na(sample_measurement)) %>% 
  # Reorder columns to put YEAR, MONTH, DAY, and MST first
  dplyr::select(YEAR_MST, MONTH_MST, DAY_MST, HOUR_MST, sample_measurement, qualifier)

##### Merge #####

Libby_NBM_rose <- Libby_hourly %>%
  inner_join(NBM, by = c("YEAR_MST", "MONTH_MST", "DAY_MST", "HOUR_MST")) %>%
  dplyr::select(YEAR_MST, MONTH_MST, DAY_MST, HOUR_MST, sample_measurement, qualifier, WDR, WSP, MHT) %>%
  mutate(
    pm2.5 = sample_measurement,
    wd = WDR * 10,
    ws = WSP * 0.514444,  # Convert wind speed from knots to m/s
    Feet = MHT*100,
    date = make_datetime(YEAR_MST, MONTH_MST, DAY_MST, HOUR_MST)  # Create the date-time column
  )

Libby_NBM_rose_warm <- Libby_NBM_rose %>%
  filter(MONTH_MST >= 5 & MONTH_MST <= 10)
Libby_NBM_rose_cool <- Libby_NBM_rose %>%
  filter(!(MONTH_MST %in% 5:10))
Libby_NBM_fire <- Libby_NBM_rose %>% 
  filter(!is.na(qualifier))

##### Pollution Rose #####

library(openair)
library(tidyverse)

trendLevel(Libby_NBM_rose, pollutant = "pm2.5")
trendLevel(Libby_NBM_rose, pollutant = "Feet")

pollutionRose(Libby_NBM_rose, pollutant = "pm2.5")
pollutionRose(Libby_NBM_rose_warm, pollutant = "pm2.5")
pollutionRose(Libby_NBM_rose_cool, pollutant = "pm2.5")
pollutionRose(Libby_NBM_fire, pollutant = "pm2.5")

polarAnnulus(Libby_NBM_rose, 
             pollutant = "pm2.5", 
             period = "hour", 
             #type = "season",
             main = "hour")

polarAnnulus(Libby_NBM_rose_warm, 
             pollutant = "pm2.5", 
             period = "hour", 
             #type = "season",
             main = "hour")

polarAnnulus(Libby_NBM_rose_cool, 
             pollutant = "pm2.5", 
             period = "hour", 
             #type = "season",
             main = "hour")

polarAnnulus(Libby_NBM_fire, 
             pollutant = "pm2.5", 
             period = "hour", 
             #type = "season",
             main = "hour")

polarPlot(Libby_NBM_rose, 
          pollutant = "pm2.5",
          limits = c(0,30))

polarPlot(Libby_NBM_rose_warm, 
          pollutant = "pm2.5",
          limits = c(0,30))

polarPlot(Libby_NBM_rose_cool, 
          pollutant = "pm2.5",
          limits = c(0,30))

polarPlot(Libby_NBM_fire, 
             pollutant = "pm2.5",
          limits = c(0,30))




