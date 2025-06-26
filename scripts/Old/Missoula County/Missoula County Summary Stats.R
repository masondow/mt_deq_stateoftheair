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

##### Add Flags #####

# Define qualifiers (for exclusion later)
qualifiers <- c(
  "RT - Wildfire-U. S.",
  "RF - Fire - Canadian.",
  "IF - Fire - Canadian.",
  "IT - Wildfire-U. S.",
  "E - Forest Fire."
)

# Convert date_local to Date and summarize only the specified qualifiers
flags <- hourly_pm25 %>%
  mutate(date_local = as.Date(date_local, format = "%Y-%m-%d")) %>%
  group_by(date_local, state_code, county_code, site_number, poc) %>%
  summarize(qualifier = paste(
    unique(na.omit(qualifier[qualifier %in% qualifiers])), 
    collapse = ", "
  )) %>%
  ungroup()

daily_pm25_flags <- daily_pm25 %>%
  filter(validity_indicator == "Y") %>% #typically I filter for 1-HR sampling, but I'm including 24-HR samples because these are annual summaries
  mutate(
    date_local = as.Date(date_local, format = "%Y-%m-%d"),
    year = year(date_local),       # Extract year from date_local
    month = month(date_local),     # Extract month from date_local
    quarter = case_when(           # Calculate quarter based on month
      month %in% 1:3 ~ 1,
      month %in% 4:6 ~ 2,
      month %in% 7:9 ~ 3,
      month %in% 10:12 ~ 4
    )
  ) %>%
  distinct(date_local, local_site_name, poc, .keep_all = TRUE)

# Merge flags with daily_pm25 by multiple columns
daily_pm25_flags <- daily_pm25_flags %>% 
  left_join(flags, by = c("date_local", "state_code", "county_code", "site_number", "poc"))

##### Annual Averages #####

# Calculate the annual average of arithmetic_mean by year
MT_annual <- daily_pm25_flags %>%
  group_by(year) %>%
  summarize(annual_average = mean(arithmetic_mean, na.rm = TRUE)) %>%
  mutate(local_site_name = "Montana Average")

MT_annual_no_wildfires <- daily_pm25_flags %>%
  filter(qualifier == "") %>%  # Include only rows where qualifier is empty
  group_by(year) %>%
  summarize(annual_average_without_fire = mean(arithmetic_mean, na.rm = TRUE)) %>%
  mutate(local_site_name = "Montana Average")

# Missoula County
# Calculate the annual average of arithmetic_mean by year
Missoula_county_annual <- daily_pm25_flags %>%
  filter(local_site_name %in% c("MSLA Boyd Park", "French Town - Beckwith", "Seeley Elementary School") & poc == 3) %>%
  group_by(local_site_name, year) %>%
  summarize(annual_average = mean(arithmetic_mean, na.rm = TRUE))

Missoula_county_annual_no_wildfires <- daily_pm25_flags %>%
  filter(local_site_name %in% c("MSLA Boyd Park", "French Town - Beckwith", "Seeley Elementary School") & poc == 3) %>%
  filter(qualifier == "") %>%  # Include only rows where qualifier is empty
  group_by(local_site_name, year) %>%
  summarize(annual_average_without_fire = mean(arithmetic_mean, na.rm = TRUE))
  
##### Number of Exceedances #####

# state
MT_exceedances <- daily_pm25_flags %>%
  group_by(state_code, county_code, site_number, poc, year) %>%
  summarize(
    days_above_9 = sum(arithmetic_mean > 9, na.rm = TRUE),
    days_above_35 = sum(arithmetic_mean > 35, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(year) %>%
  summarize(
    days_above_9 = mean(days_above_9, na.rm = TRUE),
    days_above_35 = mean(days_above_35, na.rm = TRUE)
  ) %>%
  mutate(local_site_name = "Montana Average")

# Missoula County
Missoula_county_exceedances <- daily_pm25_flags %>%
  filter(local_site_name %in% c("MSLA Boyd Park", "French Town - Beckwith", "Seeley Elementary School") & poc == 3) %>%
  group_by(local_site_name, year) %>%
  summarize(
    days_above_9 = sum(arithmetic_mean > 9, na.rm = TRUE),
    days_above_35 = sum(arithmetic_mean > 35, na.rm = TRUE)
  )

##### Number of Wildfire Smoke Days #####

# state
MT_fire_days <- daily_pm25_flags %>%
  filter(qualifier != "") %>%  # Only include rows with a non-empty qualifier
  group_by(state_code, county_code, site_number, poc, year) %>%
  summarize(wildfire_smoke_days = n(), .groups = "drop") %>%
  group_by(year) %>%
  summarize(wildfire_smoke_days = mean(wildfire_smoke_days, na.rm = TRUE)) %>%
  mutate(local_site_name = "Montana Average")

# Missoula County
Missoula_county_qualifier_days <- daily_pm25_flags %>%
  filter(local_site_name %in% c("MSLA Boyd Park", "French Town - Beckwith", "Seeley Elementary School") & poc == 3) %>%
  filter(qualifier != "") %>%  # Only include rows with a non-empty qualifier
  group_by(local_site_name, year) %>%
  summarize(wildfire_smoke_days = n(), .groups = "drop")

##### Summaries #####

# Combine MT_annual with summary
annual_summary <- bind_rows(MT_annual, Missoula_county_annual)

annual_summary_no_fires <- bind_rows(MT_annual_no_wildfires, Missoula_county_annual_no_wildfires)

exceedance_summary <- bind_rows(MT_exceedances, Missoula_county_exceedances)

fire_day_summary <- bind_rows(MT_fire_days, Missoula_county_qualifier_days)  

summary <- annual_summary %>%
  left_join(annual_summary_no_fires, by = c("local_site_name", "year")) %>%
  left_join(exceedance_summary, by = c("local_site_name", "year")) %>%
  left_join(fire_day_summary, by = c("local_site_name", "year"))

##### Trends #####

# Calculate slopes for each variable of interest
slope_data <- summary %>%
  group_by(local_site_name) %>%
  summarize(
    PM25_trend = coef(lm(annual_average ~ year))[2],                     # Slope for annual_average
    PM25_trend_without_fire = coef(lm(annual_average_without_fire ~ year))[2],  # Slope for annual_average_without_fire
    annual_NAAQS_exceedance_trend = coef(lm(days_above_9 ~ year))[2],    # Slope for days_above_9
    `24hr_NAAQS_exceedance_trend` = coef(lm(days_above_35 ~ year))[2],   # Slope for days_above_35
    wildfire_smoke_day_trend = coef(lm(wildfire_smoke_days ~ year))[2]   # Slope for wildfire_smoke_days
  )

##### PLOTTING #####

# Reorder the factor levels for local_site_name to control the legend order
summary$local_site_name <- factor(summary$local_site_name, 
                                  levels = c("Seeley Elementary School", "French Town - Beckwith", 
                                             "MSLA Boyd Park", "Montana Average"))

# Create the plot with regression lines and custom colors defined inline
summary1 <- summary %>%
  #filter(local_site_name != "Montana Average") %>%
  ggplot(aes(x = year, y = annual_average, color = local_site_name)) +
  geom_line(size = 2, alpha = 0.2) +                    # Add lines
  geom_point(size = 2, alpha = 0) +                   # Add points
  geom_smooth(method = "lm", se = FALSE, size = 1.5) + # Add linear regression lines
  labs(title = "Annual Average PM2.5",
       x = "Year",
       y = expression("Annual Average PM"["2.5"] ~ "(µg/m"^3 ~ ")"),
       color = "Site") + 
  scale_color_manual(values = c(                        # Define custom colors
    "French Town - Beckwith" = "#004A98",
    "Montana Average" = "gray80",
    "MSLA Boyd Park" = "#F54D28",
    "Seeley Elementary School" = "#009ADE"
  ),
  labels = c("Seeley Lake", "Frenchtown", "Missoula", "Montana Average")) + # Change legend labels 
  scale_x_continuous(limits = c(2010, 2023),
                     breaks = seq(2010, 2023, by = 2)) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14),  # Adjust x-axis text size
    axis.text.y = element_text(size = 12),  # Adjust y-axis text size
    axis.title.x = element_text(size = 14),  # Adjust x-axis title size
    axis.title.y = element_text(size = 14),  # Adjust y-axis title size
    plot.title = element_text(size = 20, face = "bold", margin = margin(b = 10)),  # Adjust title size and margin
    legend.position = c(0.2, 0.9),  # Move the legend to the top left (x, y format)
    legend.title = element_text(size = 12),  # Adjust legend title size
    legend.text = element_text(size = 14),  # Adjust legend text size
    panel.background = element_rect(fill = "transparent"),  # Set plot background to transparent
    plot.background = element_rect(fill = "transparent", color = NA),  # Set overall plot background to transparent with no border
    legend.background = element_rect(fill = "transparent", color = NA),  # Set legend background to transparent with no border
    panel.border = element_blank()  # Remove any panel border
  )

print(summary1)






# Create the plot with regression lines and custom colors defined inline
summary2 <- summary %>%
  #filter(local_site_name != "Montana Average") %>%
  ggplot(aes(x = year, y = annual_average_without_fire, color = local_site_name)) +
  geom_line(size = 2, alpha = 0.2) +                    # Add lines
  geom_point(size = 2, alpha = 0) +                   # Add points
  geom_smooth(method = "lm", se = FALSE, size = 1.5) + # Add linear regression lines
  labs(title = "Annual Average PM2.5 without Wildfire Smoke Days",
       x = "Year",
       y = expression("Annual Average PM"["2.5"] ~ "(µg/m"^3 ~ ")"),
       color = "Site") + 
  scale_color_manual(values = c(                        # Define custom colors
    "French Town - Beckwith" = "#004A98",
    "Montana Average" = "gray80",
    "MSLA Boyd Park" = "#F54D28",
    "Seeley Elementary School" = "#009ADE"
  ),
  labels = c("Seeley Lake", "Frenchtown", "Missoula", "Montana Average")) + # Change legend labels  
  scale_x_continuous(limits = c(2010, 2023),
                     breaks = seq(2010, 2023, by = 2)) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14),  # Adjust x-axis text size
    axis.text.y = element_text(size = 12),  # Adjust y-axis text size
    axis.title.x = element_text(size = 14),  # Adjust x-axis title size
    axis.title.y = element_text(size = 14),  # Adjust y-axis title size
    plot.title = element_text(size = 20, face = "bold", margin = margin(b = 10)),  # Adjust title size and margin
    legend.position = c(0.2, 0.9),  # Move the legend to the top left (x, y format)
    legend.title = element_text(size = 12),  # Adjust legend title size
    legend.text = element_text(size = 14),  # Adjust legend text size
    panel.background = element_rect(fill = "transparent"),  # Set plot background to transparent
    plot.background = element_rect(fill = "transparent", color = NA),  # Set overall plot background to transparent with no border
    legend.background = element_rect(fill = "transparent", color = NA),  # Set legend background to transparent with no border
    panel.border = element_blank()  # Remove any panel border
  )

print(summary2)



# Create the plot with regression lines and custom colors defined inline
summary3 <- summary %>%
  #filter(local_site_name != "Montana Average") %>%
  ggplot(aes(x = year, y = wildfire_smoke_days, color = local_site_name)) +
  geom_line(size = 2, alpha = 0.2) +                    # Add lines
  geom_point(size = 2, alpha = 0) +                   # Add points
  geom_smooth(method = "lm", se = FALSE, size = 1.5) + # Add linear regression lines
  labs(title = "Wildfire Smoke Days",
       x = "Year",
       y = "Days",
       color = "Site") + 
  scale_color_manual(values = c(                        # Define custom colors
    "French Town - Beckwith" = "#004A98",
    "Montana Average" = "gray80",
    "MSLA Boyd Park" = "#F54D28",
    "Seeley Elementary School" = "#009ADE"
  ),
  labels = c("Seeley Lake", "Frenchtown", "Missoula", "Montana Average")) + # Change legend labels  
  scale_x_continuous(limits = c(2010, 2023),
                     breaks = seq(2010, 2023, by = 2)) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14),  # Adjust x-axis text size
    axis.text.y = element_text(size = 12),  # Adjust y-axis text size
    axis.title.x = element_text(size = 14),  # Adjust x-axis title size
    axis.title.y = element_text(size = 14),  # Adjust y-axis title size
    plot.title = element_text(size = 20, face = "bold", margin = margin(b = 10)),  # Adjust title size and margin
    legend.position = c(0.35, 0.9),  # Move the legend to the top left (x, y format)
    legend.title = element_text(size = 12),  # Adjust legend title size
    legend.text = element_text(size = 14),  # Adjust legend text size
    panel.background = element_rect(fill = "transparent"),  # Set plot background to transparent
    plot.background = element_rect(fill = "transparent", color = NA),  # Set overall plot background to transparent with no border
    legend.background = element_rect(fill = "transparent", color = NA),  # Set legend background to transparent with no border
    panel.border = element_blank()  # Remove any panel border
  )

print(summary3)



# Create the plot with regression lines and custom colors defined inline
summary4 <- summary %>%
  #filter(local_site_name != "Montana Average") %>%
  ggplot(aes(x = year, y = days_above_9, color = local_site_name)) +
  geom_line(size = 2, alpha = 0.2) +                    # Add lines
  geom_point(size = 2, alpha = 0) +                   # Add points
  geom_smooth(method = "lm", se = FALSE, size = 1.5) + # Add linear regression lines
  labs(title = "Exceedances of the Annual PM2.5 NAAQS",
       x = "Year",
       y = "Days",
       color = "Site") + 
  scale_color_manual(values = c(                        # Define custom colors
    "French Town - Beckwith" = "#004A98",
    "Montana Average" = "gray80",
    "MSLA Boyd Park" = "#F54D28",
    "Seeley Elementary School" = "#009ADE"
  ),
  labels = c("Seeley Lake", "Frenchtown", "Missoula", "Montana Average")) + # Change legend labels  
  scale_x_continuous(limits = c(2010, 2023),
                     breaks = seq(2010, 2023, by = 2)) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14),  # Adjust x-axis text size
    axis.text.y = element_text(size = 12),  # Adjust y-axis text size
    axis.title.x = element_text(size = 14),  # Adjust x-axis title size
    axis.title.y = element_text(size = 14),  # Adjust y-axis title size
    plot.title = element_text(size = 20, face = "bold", margin = margin(b = 10)),  # Adjust title size and margin
    legend.position = c(0.8, 0.9),  # Move the legend to the top left (x, y format)
    legend.title = element_text(size = 12),  # Adjust legend title size
    legend.text = element_text(size = 14),  # Adjust legend text size
    panel.background = element_rect(fill = "transparent"),  # Set plot background to transparent
    plot.background = element_rect(fill = "transparent", color = NA),  # Set overall plot background to transparent with no border
    legend.background = element_rect(fill = "transparent", color = NA),  # Set legend background to transparent with no border
    panel.border = element_blank()  # Remove any panel border
  )

print(summary4)




# Create the plot with regression lines and custom colors defined inline
summary5 <- summary %>%
  #filter(local_site_name != "Montana Average") %>%
  ggplot(aes(x = year, y = days_above_35, color = local_site_name)) +
  geom_line(size = 2, alpha = 0.2) +                    # Add lines
  geom_point(size = 2, alpha = 0) +                   # Add points
  geom_smooth(method = "lm", se = FALSE, size = 1.5) + # Add linear regression lines
  labs(title = "Exceedances of the 24-hr PM2.5 NAAQS",
       x = "Year",
       y = "Days",
       color = "Site") + 
  scale_color_manual(values = c(                        # Define custom colors
    "French Town - Beckwith" = "#004A98",
    "Montana Average" = "gray80",
    "MSLA Boyd Park" = "#F54D28",
    "Seeley Elementary School" = "#009ADE"
  ),
  labels = c("Seeley Lake", "Frenchtown", "Missoula", "Montana Average")) + # Change legend labels  
  scale_x_continuous(limits = c(2010, 2023),
                     breaks = seq(2010, 2023, by = 2)) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14),  # Adjust x-axis text size
    axis.text.y = element_text(size = 12),  # Adjust y-axis text size
    axis.title.x = element_text(size = 14),  # Adjust x-axis title size
    axis.title.y = element_text(size = 14),  # Adjust y-axis title size
    plot.title = element_text(size = 20, face = "bold", margin = margin(b = 10)),  # Adjust title size and margin
    legend.position = c(0.8, 0.9),  # Move the legend to the top left (x, y format)
    legend.title = element_text(size = 12),  # Adjust legend title size
    legend.text = element_text(size = 14),  # Adjust legend text size
    panel.background = element_rect(fill = "transparent"),  # Set plot background to transparent
    plot.background = element_rect(fill = "transparent", color = NA),  # Set overall plot background to transparent with no border
    legend.background = element_rect(fill = "transparent", color = NA),  # Set legend background to transparent with no border
    panel.border = element_blank()  # Remove any panel border
  )

print(summary5)

