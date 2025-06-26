
# Set AQS credentials
aqs_credentials(username = "Cameron.Nealy@mt.gov", key = "copperhawk98")


data88101 <- aqs_monitors_by_state(parameter="88101",
                      bdate=as.Date("19000101",
                                    format="%Y%m%d"),
                      edate=as.Date("20241231",
                                    format="%Y%m%d"),
                      stateFIPS="30")


data88502 <- aqs_monitors_by_state(parameter="88502",
                                   bdate=as.Date("19000101",
                                                 format="%Y%m%d"),
                                   edate=as.Date("20241231",
                                                 format="%Y%m%d"),
                                   stateFIPS="30")

# Load necessary libraries
library(dplyr)
library(writexl)

# Merge the two data frames
merged_data <- bind_rows(data88101, data88502)

# Export the merged data frame to an Excel file
write_xlsx(merged_data, "/Users/cameronnealy/Downloads/88101_88502_sites.xlsx")

# Optional: Print the first few rows of the merged data to verify
print(head(merged_data))
