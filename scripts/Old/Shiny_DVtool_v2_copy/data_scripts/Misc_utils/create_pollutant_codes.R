# Create a data frame
pollutant_codes <- data.frame(
  Pollutant = c("PM10", "PM25"),
  Parameter_Code = I(list(81102, c(88101, 88502)))  # Using list to store multiple values for PM25
)

# Save the data frame as an .rds file
saveRDS(pollutant_codes, here("data", "pollutant_codes.rds"))
