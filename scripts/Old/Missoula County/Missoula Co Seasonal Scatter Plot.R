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
  filter(validity_indicator == "Y") %>% # Filter for valid samples
  mutate(
    date_local = as.Date(date_local, format = "%Y-%m-%d"),  # Convert to Date
    year = year(date_local),                                # Extract year
    month = month(date_local),                              # Extract month
    day_of_year = yday(date_local),                        # Add Day of Year column
    quarter = case_when(                                    # Calculate quarter
      month %in% 1:3 ~ 1,
      month %in% 4:6 ~ 2,
      month %in% 7:9 ~ 3,
      month %in% 10:12 ~ 4
    )
  ) %>%
  distinct(date_local, local_site_name, poc, .keep_all = TRUE)  # Ensure distinct rows

# Merge flags with daily_pm25 by multiple columns
daily_pm25_flags <- daily_pm25_flags %>% 
  left_join(flags, by = c("date_local", "state_code", "county_code", "site_number", "poc"))

##### Daily Averages #####

Missoula_county_daily_avg <- daily_pm25_flags %>%
  filter(local_site_name %in% c("MSLA Boyd Park", "French Town - Beckwith", "Seeley Elementary School") & poc == 3) %>%
  group_by(local_site_name, day_of_year) %>%
  summarize(daily_avg = mean(arithmetic_mean, na.rm = TRUE))

Montana_daily_avg <- daily_pm25_flags %>%
  group_by(day_of_year) %>%
  summarize(daily_avg = mean(arithmetic_mean, na.rm = TRUE)) %>%
  mutate(local_site_name = "Montana Average")

##### Daily Averages no fire #####

Missoula_county_daily_avg_without_fire <- daily_pm25_flags %>%
  filter(local_site_name %in% c("MSLA Boyd Park", "French Town - Beckwith", "Seeley Elementary School") & poc == 3) %>%
  filter(is.na(qualifier) | qualifier == "") %>%
  group_by(local_site_name, day_of_year) %>%
  summarize(daily_avg_no_fire = mean(arithmetic_mean, na.rm = TRUE))

Montana_daily_avg_without_fire <- daily_pm25_flags %>%
  filter(is.na(qualifier) | qualifier == "") %>%
  group_by(day_of_year) %>%
  summarize(daily_avg_no_fire = mean(arithmetic_mean, na.rm = TRUE)) %>%
  mutate(local_site_name = "Montana Average")


##### Scatter #####

Missoula_county_scatter_no_wildfire <- daily_pm25_flags %>%
  filter(local_site_name %in% c("MSLA Boyd Park", "French Town - Beckwith", "Seeley Elementary School") & poc == 3) %>%
  filter(is.na(qualifier) | qualifier == "") %>%
  select(local_site_name, day_of_year, arithmetic_mean)

Missoula_county_scatter <- daily_pm25_flags %>%
  filter(local_site_name %in% c("MSLA Boyd Park", "French Town - Beckwith", "Seeley Elementary School") & poc == 3) %>%
  select(local_site_name, day_of_year, arithmetic_mean)

##### Plotting #####

# Reorder the local_site_name factor
Missoula_county_scatter_no_wildfire <- Missoula_county_scatter_no_wildfire %>%
  mutate(local_site_name = factor(local_site_name, 
                                  levels = c("Seeley Elementary School", "MSLA Boyd Park", "French Town - Beckwith"))) %>% # reorder for correct legend mapping 
  mutate(date = as.Date(day_of_year - 1, origin = "2024-01-01"))  # place day of year on standard year scale for monthly x-axis plotting

Missoula_county_daily_avg_without_fire <- Missoula_county_daily_avg_without_fire %>%
  mutate(date = as.Date(day_of_year - 1, origin = "2024-01-01")) # place day of year on standard year scale for monthly x-axis plotting

Montana_daily_avg_without_fire <- Montana_daily_avg_without_fire %>%
  mutate(date = as.Date(day_of_year - 1, origin = "2024-01-01")) # place day of year on standard year scale for monthly x-axis plotting


ggplot() + 
  # Plot the data points, keeping Seeley Elementary School in the background
  geom_point(data = subset(Missoula_county_scatter_no_wildfire, local_site_name == "Seeley Elementary School"), 
             aes(x = date, y = arithmetic_mean, color = local_site_name), 
             size = 2, alpha = 0.05) +
  geom_point(data = subset(Missoula_county_scatter_no_wildfire, local_site_name == "French Town - Beckwith"), 
             aes(x = date, y = arithmetic_mean, color = local_site_name), 
             size = 2, alpha = 0.05) +
  geom_point(data = subset(Missoula_county_scatter_no_wildfire, local_site_name == "MSLA Boyd Park"), 
             aes(x = date, y = arithmetic_mean, color = local_site_name), 
             size = 2, alpha = 0.05) +
  # Add horizontal lines with legend labels
  geom_hline(aes(yintercept = 35, color = "24-hr PM2.5 NAAQS 35 µg/m³"), linetype = "dotted", size = 0.8) +
  geom_hline(aes(yintercept = 9, color = "Annual PM2.5 NAAQS 9 µg/m³"), linetype = "dashed", size = 0.8) +
  geom_line(data = Montana_daily_avg_without_fire, 
            aes(x = date, y = daily_avg_no_fire, color = "Montana average"), 
            size = 2.5, alpha = 0.25) +
  # Add lines for each site with defined colors and sizes
  geom_line(data = subset(Missoula_county_daily_avg_without_fire, local_site_name == "Seeley Elementary School"), 
            aes(x = date, y = daily_avg_no_fire, color = local_site_name), 
            size = 1) +
  geom_line(data = subset(Missoula_county_daily_avg_without_fire, local_site_name == "French Town - Beckwith"), 
            aes(x = date, y = daily_avg_no_fire, color = local_site_name), 
            size = 1) +
  geom_line(data = subset(Missoula_county_daily_avg_without_fire, local_site_name == "MSLA Boyd Park"), 
            aes(x = date, y = daily_avg_no_fire, color = local_site_name), 
            size = 1) +
  labs(title = "Average Daily PM2.5", 
       subtitle = "Without Wildfire Smoke Days",
       x = "Month", 
       y = expression("PM"["2.5"] ~ "(µg/m"^3 ~ ")"), 
       color = "") +
  scale_color_manual(values = c("Seeley Elementary School" = "#009ADE", 
                                "MSLA Boyd Park" = "#F54D28", 
                                "French Town - Beckwith" = "#004A98",
                                "Montana average" = "black",
                                "Annual PM2.5 NAAQS 9 µg/m³" ="black",
                                "24-hr PM2.5 NAAQS 35 µg/m³" = "black"),
                     label = c("Seeley Lake", "Frenchtown", "Missoula", "24-hr PM2.5 NAAQS 35 µg/m³", "Annual PM2.5 NAAQS 9 µg/m³", "Montana average")) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = c(0, 0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(0, 40)) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = -.5, size = 14),  # Adjust x-axis text size
    axis.text.y = element_text(size = 14),  # Adjust y-axis text size
    axis.title.x = element_text(size = 16),  # Adjust x-axis title size
    axis.title.y = element_text(size = 16),  # Adjust y-axis title size
    plot.title = element_text(size = 22, face = "bold", margin = margin(b = 10), vjust = -.3),  # Adjust title size and margin
    plot.subtitle = element_text(size = 17),
    #legend.position = c(0.2, 0.9),  # Move the legend to the top left (x, y format)
    legend.title = element_text(size = 12),  # Adjust legend title size
    legend.text = element_text(size = 14),  # Adjust legend text size
    legend.key.width = unit(.75, "cm"),
    legend.key.height = unit(0.6, "cm"),    
    panel.background = element_rect(fill = "transparent"),  # Set plot background to transparent
    plot.background = element_rect(fill = "transparent", color = NA),  # Set overall plot background to transparent with no border
    legend.background = element_rect(fill = "transparent", color = NA),  # Set legend background to transparent with no border
    panel.border = element_blank()  # Remove any panel border
  )
  



# Reorder the local_site_name factor
Missoula_county_scatter <- Missoula_county_scatter %>%
  mutate(local_site_name = factor(local_site_name, 
                                  levels = c("Seeley Elementary School", "MSLA Boyd Park", "French Town - Beckwith"))) %>% # reorder for correct legend mapping 
  mutate(date = as.Date(day_of_year - 1, origin = "2024-01-01"))  # place day of year on standard year scale for monthly x-axis plotting

Missoula_county_daily_avg <- Missoula_county_daily_avg %>%
  mutate(date = as.Date(day_of_year - 1, origin = "2024-01-01")) # place day of year on standard year scale for monthly x-axis plotting

Montana_daily_avg <- Montana_daily_avg %>%
  mutate(date = as.Date(day_of_year - 1, origin = "2024-01-01")) # place day of year on standard year scale for monthly x-axis plotting


ggplot() + 
  # Plot the data points, keeping Seeley Elementary School in the background
  geom_point(data = subset(Missoula_county_scatter, local_site_name == "Seeley Elementary School"), 
             aes(x = date, y = arithmetic_mean, color = local_site_name), 
             size = 2, alpha = 0.05) +
  geom_point(data = subset(Missoula_county_scatter, local_site_name == "French Town - Beckwith"), 
             aes(x = date, y = arithmetic_mean, color = local_site_name), 
             size = 2, alpha = 0.05) +
  geom_point(data = subset(Missoula_county_scatter, local_site_name == "MSLA Boyd Park"), 
             aes(x = date, y = arithmetic_mean, color = local_site_name), 
             size = 2, alpha = 0.05) +
  # Add horizontal lines with legend labels
  geom_hline(aes(yintercept = 35, color = "24-hr PM2.5 NAAQS 35 µg/m³"), linetype = "dotted", size = 0.8) +
  geom_hline(aes(yintercept = 9, color = "Annual PM2.5 NAAQS 9 µg/m³"), linetype = "dashed", size = 0.8) +
  geom_line(data = Montana_daily_avg, 
            aes(x = date, y = daily_avg, color = "Montana average"), 
            size = 2.5, alpha = 0.25) +
  # Add lines for each site with defined colors and sizes
  geom_line(data = subset(Missoula_county_daily_avg, local_site_name == "Seeley Elementary School"), 
            aes(x = date, y = daily_avg, color = local_site_name), 
            size = 1) +
  geom_line(data = subset(Missoula_county_daily_avg, local_site_name == "French Town - Beckwith"), 
            aes(x = date, y = daily_avg, color = local_site_name), 
            size = 1) +
  geom_line(data = subset(Missoula_county_daily_avg, local_site_name == "MSLA Boyd Park"), 
            aes(x = date, y = daily_avg, color = local_site_name), 
            size = 1) +
  labs(title = "Average Daily PM2.5", 
       subtitle = "Including Wildfire Smoke Days",
       x = "Month", 
       y = expression("PM"["2.5"] ~ "(µg/m"^3 ~ ")"), 
       color = "") +
  scale_color_manual(values = c("Seeley Elementary School" = "#009ADE", 
                                "MSLA Boyd Park" = "#F54D28", 
                                "French Town - Beckwith" = "#004A98",
                                "Montana average" = "black",
                                "Annual PM2.5 NAAQS 9 µg/m³" ="black",
                                "24-hr PM2.5 NAAQS 35 µg/m³" = "black"),
                     label = c("Seeley Lake", "Frenchtown", "Missoula", "24-hr PM2.5 NAAQS 35 µg/m³", "Annual PM2.5 NAAQS 9 µg/m³", "Montana average")) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = c(0, 0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(0, 60)) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = -.5, size = 14),  # Adjust x-axis text size
    axis.text.y = element_text(size = 14),  # Adjust y-axis text size
    axis.title.x = element_text(size = 16),  # Adjust x-axis title size
    axis.title.y = element_text(size = 16),  # Adjust y-axis title size
    plot.title = element_text(size = 22, face = "bold", margin = margin(b = 10), vjust = -.3),  # Adjust title size and margin
    plot.subtitle = element_text(size = 17),
    #legend.position = c(0.2, 0.9),  # Move the legend to the top left (x, y format)
    legend.title = element_text(size = 12),  # Adjust legend title size
    legend.text = element_text(size = 14),  # Adjust legend text size
    legend.key.width = unit(.75, "cm"),
    legend.key.height = unit(0.6, "cm"),    
    panel.background = element_rect(fill = "transparent"),  # Set plot background to transparent
    plot.background = element_rect(fill = "transparent", color = NA),  # Set overall plot background to transparent with no border
    legend.background = element_rect(fill = "transparent", color = NA),  # Set legend background to transparent with no border
    panel.border = element_blank()  # Remove any panel border
  )
  