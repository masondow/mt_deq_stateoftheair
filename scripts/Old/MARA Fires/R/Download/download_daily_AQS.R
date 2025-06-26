# Set AQS credentials
aqs_credentials(username = "Cameron.Nealy@mt.gov", key = "copperhawk98")


"data <- aqs_dailysummary_by_state(parameter="88101",
                                   bdate=as.Date("20030101",
                                                 format="%Y%m%d"),
                                   edate=as.Date("20231231",
                                                 format="%Y%m%d"),
                                   stateFIPS="30")"

"data2 <- aqs_dailysummary_by_state(parameter="88502",
                                   bdate=as.Date("20030101",
                                                 format="%Y%m%d"),
                                   edate=as.Date("20231231",
                                                 format="%Y%m%d"),
                                   stateFIPS="30")"
# Load the dplyr package
library(dplyr)

# Merge the two data frames
pm25_samples <- bind_rows(data, data2)

# Filter the pm25_samples data frame
pm25_samples <- pm25_samples %>%
  filter(method_code %in% c("170", "183", "209", "731") 
         & sample_duration == "1 HOUR"
         & validity_indicator == "Y")

# Save the data frame as an RDS file
saveRDS(pm25_samples, "/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/MARA Fires/R/pm25_samples_all_continuous.rds")
