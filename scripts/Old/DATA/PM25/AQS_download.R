library(dplyr)
library(RAQSAPI)

# Set AQS credentials
aqs_credentials(username = "Cameron.Nealy@mt.gov", key = "copperhawk98")

byear <- 2000
eyear <- 2002

daily_88101 <- aqs_dailysummary_by_state(
  parameter = "88101",
  bdate = as.Date(paste0(byear,"0101"), format = "%Y%m%d"),
  edate = as.Date(paste0(eyear,"1231"), format = "%Y%m%d"),
  stateFIPS = 30
)

daily_88502 <- aqs_dailysummary_by_state(
  parameter = "88502",
  bdate = as.Date(paste0(byear,"0101"), format = "%Y%m%d"),
  edate = as.Date(paste0(eyear,"1231"), format = "%Y%m%d"),
  stateFIPS = 30
)

hourly_88101 <- aqs_sampledata_by_state(
  parameter = "88101",
  bdate = as.Date(paste0(byear,"0101"), format = "%Y%m%d"),
  edate = as.Date(paste0(eyear,"1231"), format = "%Y%m%d"),
  stateFIPS = 30
)

hourly_88502 <- aqs_sampledata_by_state(
  parameter = "88502",
  bdate = as.Date(paste0(byear,"0101"), format = "%Y%m%d"),
  edate = as.Date(paste0(eyear,"1231"), format = "%Y%m%d"),
  stateFIPS = 30
)

daily_merge <- rbind(daily_88101, daily_88502)
hourly_merge <- rbind(hourly_88101, hourly_88502)

daily_historical <- readRDS("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/DATA/PM25/2003_2024_daily_pm25_88101_88502_all_MT.rds")
hourly_historical <- readRDS("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/DATA/PM25/2003_2024_hourly_pm25_88101_88502_all_MT.rds")

daily_historical_merge <- rbind(daily_historical, daily_merge)
hourly_historical_merge <- rbind(hourly_historical, hourly_merge)

saveRDS(daily_historical_merge, "/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/DATA/PM25/2000_2024_daily_pm25_88101_88502_all_MT.rds")
saveRDS(hourly_historical_merge, "/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/DATA/PM25/2000_2024_hourly_pm25_88101_88502_all_MT.rds")







