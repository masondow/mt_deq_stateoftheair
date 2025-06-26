library(dplyr)
library(RAQSAPI)

# Set AQS credentials
aqs_credentials(username = "Cameron.Nealy@mt.gov", key = "copperhawk98")

byear <- 2000
eyear <- 2024

daily_81102 <- aqs_dailysummary_by_state(
  parameter = "81102",
  bdate = as.Date(paste0(byear,"0101"), format = "%Y%m%d"),
  edate = as.Date(paste0(eyear,"1231"), format = "%Y%m%d"),
  stateFIPS = 30
)


hourly_81102 <- aqs_sampledata_by_state(
  parameter = "81102",
  bdate = as.Date(paste0(byear,"0101"), format = "%Y%m%d"),
  edate = as.Date(paste0(eyear,"1231"), format = "%Y%m%d"),
  stateFIPS = 30
)


#daily_historical <- readRDS("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/DATA/2003_2023_daily_pm25_88101_88502_all_MT.rds")
#hourly_historical <- readRDS("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/DATA/2003_2023_hourly_pm25_88101_88502_all_MT.rds")

#daily_historical_merge <- rbind(daily_historical, daily_merge)
#hourly_historical_merge <- rbind(hourly_historical, hourly_merge)

saveRDS(daily_81102, "/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/DATA/PM10/2000_2024_daily_pm10_81102_all_MT.rds")
saveRDS(hourly_81102, "/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/DATA/PM10/2000_2024_hourly_pm10_81102_all_MT.rds")







