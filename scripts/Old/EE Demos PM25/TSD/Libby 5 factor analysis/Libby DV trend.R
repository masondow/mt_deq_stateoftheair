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

Libby_pm25_flags <- daily_pm25 %>%
  filter(local_site_name == "Libby Courthouse Annex" & 
           validity_indicator == "Y" &
           pollutant_standard == "PM25 Annual 2024") %>%
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
Libby_pm25_flags <- Libby_pm25_flags %>% 
  left_join(flags, by = c("date_local", "state_code", "county_code", "site_number", "poc"))

Libby_pm25_flags <- Libby_pm25_flags %>%
  select(local_site_name, date_local, sample_duration, year, month, quarter, arithmetic_mean, qualifier) 

Libby_pm25_flags <- Libby_pm25_flags %>%
  group_by(date_local) %>% 
  summarize(
    local_site_name = first(local_site_name),  # Retain the first site name (or adjust logic if needed)
    sample_duration = first(sample_duration),
    year = first(year),                        # Retain the first year (assuming it's the same for each date)
    month = first(month),                      # Retain the first month
    quarter = first(quarter),                  # Retain the first quarter
    arithmetic_mean = mean(arithmetic_mean, na.rm = TRUE),  # Average `arithmetic_mean`
    qualifier = first(qualifier)               # Retain the first qualifier
  ) %>%
  ungroup()  # Remove grouping


##### Quarterly Completeness #####

# Define a function to get total days for a given year and quarter
get_total_days <- function(year, quarter) {
  if (quarter == 1) {
    return(as.numeric(as.Date(paste0(year, "-03-31")) - as.Date(paste0(year, "-01-01")) + 1))
  } else if (quarter == 2) {
    return(as.numeric(as.Date(paste0(year, "-06-30")) - as.Date(paste0(year, "-04-01")) + 1))
  } else if (quarter == 3) {
    return(as.numeric(as.Date(paste0(year, "-09-30")) - as.Date(paste0(year, "-07-01")) + 1))
  } else if (quarter == 4) {
    return(as.numeric(as.Date(paste0(year, "-12-31")) - as.Date(paste0(year, "-10-01")) + 1))
  }
}

# Calculate data completeness by year, and quarter
quarterly_completeness_summary <- Libby_pm25_flags %>%
  group_by(local_site_name, year, quarter) %>%
  summarise(
    # Adjust data_days to count "24 HOUR" samples as 3 days
    data_days = sum(ifelse(sample_duration == "24 HOUR", 3, 1)),  
    total_days = get_total_days(first(year), first(quarter)),  # Total days in the quarter
    quarter_completeness = data_days / total_days * 100  # Calculate completeness percentage
  ) %>%
  ungroup()

##### Annual Completeness #####

# Create the complete_quarters column with unique quarters per year and site
annual_completeness <- quarterly_completeness_summary %>%
  group_by(year, local_site_name) %>%
  summarize(
    complete_quarters = n_distinct(quarter[quarter_completeness >= 75], na.rm = TRUE)
  ) %>%
  ungroup()

##### Design Values #####

# quarterly averages
Libby_DV <- Libby_pm25_flags %>%
  group_by(local_site_name, year, quarter) %>%
  summarise(  # Use summarise to collapse data by group
    quarterly_average = mean(arithmetic_mean, na.rm = TRUE)  # Calculate the mean for the group
  ) %>%
  ungroup()  # Ungroup the data frame

# annual averages
Libby_DV <- Libby_DV %>%
  group_by(local_site_name, year) %>%
  summarise(annual_average = mean(quarterly_average, na.rm = TRUE)) %>%
  ungroup()

# Calculate 3-year rolling averages
Libby_DV <- Libby_DV %>%
  group_by(local_site_name) %>%  # Group by site and POC
  arrange(year) %>%  # Ensure data is ordered by year
  mutate(
    DV = zoo::rollmean(annual_average, 3, fill = NA, align = "right")  # Calculate 3-year rolling average
  ) %>%
  ungroup()

##### Design Values no wildfire flags #####
# filter for active sites, remove redundant POCs, and remove wildfire flagged data 
Libby_pm25_no_flags <- Libby_pm25_flags %>%
  filter((is.na(qualifier) | qualifier == "")) 

# quarterly averages
Libby_DV_no_flags <- Libby_pm25_no_flags %>%
  group_by(local_site_name, year, quarter) %>%
  summarise(  # Use summarise to collapse data by group
    quarterly_average = mean(arithmetic_mean, na.rm = TRUE)  # Calculate the mean for the group
  ) %>%
  ungroup()  # Ungroup the data frame

# annual averages
Libby_DV_no_flags <- Libby_DV_no_flags %>%
  group_by(local_site_name, year) %>%
  summarise(annual_average_noEE = mean(quarterly_average, na.rm = TRUE)) %>%
  ungroup()

# Calculate 3-year rolling averages
Libby_DV_no_flags <- Libby_DV_no_flags %>%
  group_by(local_site_name) %>%  # Group by site and POC
  arrange(year) %>%  # Ensure data is ordered by year
  mutate(
    DV_no_EE = zoo::rollmean(annual_average_noEE, 3, fill = NA, align = "right")  # Calculate 3-year rolling average
  ) %>%
  ungroup()

# Join the two data frames
Libby_DV <- Libby_DV %>%
  left_join(Libby_DV_no_flags %>% select(local_site_name, year, annual_average_noEE, DV_no_EE),
            by = c("local_site_name", "year"))
##### Plot (with wildfire contribution) #####
plot_show_fire <- ggplot() +
  # Add a ribbon between the two lines
  geom_ribbon(
    data = Libby_DV,
    aes(x = year, ymin = DV_no_EE, ymax = DV, fill = "Wildfire Smoke Contribution"),  # ymin and ymax define the area
    alpha = 0.3  # Adjust transparency of the fill
  ) +
  geom_line(data = Libby_DV,
            aes(x = year, y = DV, group = local_site_name, color = "Design Value"),
            linewidth = 1.5, alpha = 1) +
  geom_line(data = Libby_DV, 
            aes(x = year, y = DV_no_EE, color = "Design Value (wildfire smoke days removed)"), 
            linewidth = 1.5) +
  geom_hline(yintercept = 9, color = "black", linetype = "dashed", size = 1) +  # Add dashed line at y = 9
  scale_color_manual(
    values = c("Design Value (wildfire smoke days removed)" = "#004A98", 
               "Design Value" = "#F54D28")) + 
  scale_fill_manual(values = c("Wildfire Smoke Contribution" = "#F54D28")) +
  labs(
    title = bquote(bold("Libby PM"[2.5] ~ "Annual Design Value Trend")),  # Title with subscript
    subtitle = "Since 2005",  # Subtitle not bold
    x = "", 
    y = expression("Design Value PM"[2.5] * " (µg/m³)"),  # Subscript in y-axis label
    color = "",  # Custom legend title
    fill = ""
  ) +
  theme_classic() +  # Classic theme for clean appearance
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14),  # Adjust x-axis text size
    axis.text.y = element_text(size = 14),  # Adjust y-axis text size
    axis.title.x = element_text(size = 14),  # Adjust x-axis title size
    axis.title.y = element_text(size = 15),  # Adjust y-axis title size
    plot.title = element_text(size = 20, face = "bold", margin = margin(b = 10)),  # Adjust title size and margin
    plot.subtitle = element_text(size = 18, face = "plain", vjust = 3),  # Subtitle not bold
    legend.position = c(0.5, 0.9),  # Move the legend to the top left (x, y format)
    legend.title = element_text(size = 12),  # Adjust legend title size
    legend.text = element_text(size = 14),  # Adjust legend text size
    legend.spacing.y = unit(-.5, "cm"),  # Reduce vertical spacing between legend items
    panel.background = element_rect(fill = "transparent"),  # Set plot background to transparent
    plot.background = element_rect(fill = "transparent", color = NA),  # Set overall plot background to transparent with no border
    legend.background = element_rect(fill = "transparent", color = NA),  # Set legend background to transparent with no border
    panel.border = element_blank(),  # Remove any panel border
    panel.grid.major.y = element_line(color = "gray90", linetype = "solid", size = 0.5),
    plot.margin = margin(t = 10, r = 20, b = 10, l = 10)
  ) +
  scale_y_continuous(
    breaks = seq(8, 16, by = 1)  # Set y-axis ticks from 7 to 15 by 1
    #expand = c(0, 0)
  ) +
  scale_x_continuous(
    breaks = seq(2005, 2023, by = 2),  # Tick marks every 2 years
    expand = c(0,0)
  ) +
  coord_cartesian(xlim = c(2005, 2023), ylim = c(8, 16))

print(plot_show_fire)

ggsave(filename = "/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/EE Demos/TSD/TSD Figures/TSD Fig 8_show_fire.png", plot = plot_show_fire, width = 8, height = 4.75, dpi = 300)


##### Plot (with wildfire contribution) #####
plot_no_fire <- ggplot() +
  geom_line(data = Libby_DV,
            aes(x = year, y = DV, group = local_site_name, color = "Design Value"),
            linewidth = 1.5, alpha = 1) +
  geom_hline(yintercept = 9, color = "black", linetype = "dashed", size = 1) +  # Add dashed line at y = 9
  scale_color_manual(
    values = c("Design Value (wildfire smoke days removed)" = "#004A98", 
               "Design Value" = "#F54D28")) + 
  scale_fill_manual(values = c("Wildfire Smoke Contribution" = "#F54D28")) +
  labs(
    title = bquote(bold("Libby PM"[2.5] ~ "Annual Design Value Trend")),  # Title with subscript
    subtitle = "Since 2005",  # Subtitle not bold
    x = "", 
    y = expression("Design Value PM"[2.5] * " (µg/m³)"),  # Subscript in y-axis label
    color = "",  # Custom legend title
    fill = ""
  ) +
  theme_classic() +  # Classic theme for clean appearance
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14),  # Adjust x-axis text size
    axis.text.y = element_text(size = 14),  # Adjust y-axis text size
    axis.title.x = element_text(size = 14),  # Adjust x-axis title size
    axis.title.y = element_text(size = 15),  # Adjust y-axis title size
    plot.title = element_text(size = 20, face = "bold", margin = margin(b = 10)),  # Adjust title size and margin
    plot.subtitle = element_text(size = 18, face = "plain", vjust = 3),  # Subtitle not bold
    legend.position = c(0.32, 0.9),  # Move the legend to the top left (x, y format)
    legend.title = element_text(size = 12),  # Adjust legend title size
    legend.text = element_text(size = 14),  # Adjust legend text size
    legend.spacing.y = unit(-.5, "cm"),  # Reduce vertical spacing between legend items
    panel.background = element_rect(fill = "transparent"),  # Set plot background to transparent
    plot.background = element_rect(fill = "transparent", color = NA),  # Set overall plot background to transparent with no border
    legend.background = element_rect(fill = "transparent", color = NA),  # Set legend background to transparent with no border
    panel.border = element_blank(),  # Remove any panel border
    panel.grid.major.y = element_line(color = "gray90", linetype = "solid", size = 0.5),
    plot.margin = margin(t = 10, r = 20, b = 10, l = 10)
  ) +
  scale_y_continuous(
    breaks = seq(8, 16, by = 1)  # Set y-axis ticks from 7 to 15 by 1
    #expand = c(0, 0)
  ) +
  scale_x_continuous(
    breaks = seq(2005, 2023, by = 2),  # Tick marks every 2 years
    expand = c(0,0)
  ) +
  coord_cartesian(xlim = c(2005, 2023), ylim = c(8, 16))

print(plot_no_fire)

ggsave(filename = "/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/EE Demos/TSD/TSD Figures/TSD Fig 8_no_fire.png", plot = plot_no_fire, width = 8, height = 4.75, dpi = 300)
