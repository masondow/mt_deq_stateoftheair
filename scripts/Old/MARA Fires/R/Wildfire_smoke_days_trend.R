library(dplyr)
library(lubridate)
library(stringr)
library(readxl)
library(magrittr)
library(ggplot2)

##### Download AQS Data #####

daily_pm25 <- readRDS("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/DATA/2003_2023_daily_pm25_88101_88502_all_MT.rds")
hourly_pm25 <- readRDS("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/DATA/2003_2023_hourly_pm25_88101_88502_all_MT.rds")

##### Add Flags #####

# Define qualifiers (for exclusion later)
qualifiers <- c(
  "RT - Wildfire-U. S.",
  "IT - Wildfire-U. S.",
  "RF - Fire - Canadian.",
  "IF - Fire - Canadian.",
  "E - Forest Fire."
)

# Convert date_local to Date and summarize only the specified qualifiers
flags <- hourly_pm25 %>%
  mutate(date_local = as.Date(date_local, format = "%Y-%m-%d")) %>%
  group_by(date_local, state_code, county_code, site_number) %>%
  summarize(qualifier = paste(
    unique(na.omit(qualifier[qualifier %in% qualifiers])), 
    collapse = ", "
  )) %>%
  ungroup()

# Filter, convert the date_local column, and add a year column
daily_pm25 <- daily_pm25 %>%
  filter(sample_duration == "1 HOUR" & validity_indicator == "Y") %>%
  mutate(date_local = as.Date(date_local, format = "%Y-%m-%d"),  # Convert date_local to Date type
         year = format(date_local, "%Y")) %>%  # Extract the year from date_local
  distinct(date_local, local_site_name, .keep_all = TRUE)  # Ensure uniqueness

# Merge flags with daily_pm25 by multiple columns
daily_pm25 <- daily_pm25 %>% 
  left_join(flags, by = c("date_local", "state_code", "county_code", "site_number"))

# Select columns
daily_pm25_filtered <- daily_pm25 %>%
  select(local_site_name, date_local, year, arithmetic_mean, qualifier)

##### All Sites Trend - Wildfire Days #####

all_sites <- daily_pm25_filtered %>%
  filter(qualifier %in% qualifiers)

# Group by local_site_name and year, then calculate the annual average arithmetic_mean
annual_pm25_average <- all_sites %>%
  group_by(local_site_name, year) %>%
  summarize(average_arithmetic_mean = mean(arithmetic_mean, na.rm = TRUE)) %>%
  ungroup()  # Ungroup after summarizing

montana_annual_pm25_average <- all_sites %>%
  group_by(year) %>%
  summarize(average_arithmetic_mean = mean(arithmetic_mean, na.rm = TRUE)) %>%
  ungroup()  # Ungroup after summarizing

# Ensure the year column is numeric for proper plotting
annual_pm25_average$year <- as.numeric(annual_pm25_average$year)
montana_annual_pm25_average$year <- as.numeric(montana_annual_pm25_average$year)

# Create the line plot with dummy legends and formatted text
p <- ggplot() +
#  geom_line(data = annual_pm25_average, 
#            aes(x = year, y = average_arithmetic_mean, group = local_site_name, color = "Individual Monitor Averages"), 
#            linewidth = 0.75) +  # Gray lines for each site
  geom_line(data = montana_annual_pm25_average, 
            aes(x = year, y = average_arithmetic_mean, color = "Montana Average"), 
            linewidth = 1.25) +  # Black line for Montana average
  geom_smooth(data = annual_pm25_average, 
              aes(x = year, y = average_arithmetic_mean, color = "Wildfire Smoke Trend"), 
              method = "lm", linetype = "solid", se = FALSE, linewidth = 1.25) +  # Global trend line
  scale_y_continuous(limits = c(10, 45), expand = c(0, 0)) +  # Y-axis limits
  scale_x_continuous(
    breaks = seq(2005, 2023, by = 5)  # Tick marks every year
  ) +
  scale_color_manual(values = c(#"Individual Monitor Averages" = "gray80", 
                                "Montana Average" = "black", 
                                "Wildfire Smoke Trend" = "#F54D28")) +  # Custom color for legend
  labs(title = "Increasing Severity of Wildfire Smoke Days", 
       x = expression(""), 
       y = expression("Average PM"[2.5] * " (µg/m³)"),  # Subscript and superscript in y-axis label
       color = "") +  # Custom legend title
  theme_classic() +  # Classic theme for clean appearance
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

print(p)

# Define the output directory
output_dir <- "/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/MARA Fires/Final Figures"


filename <- file.path(output_dir, paste0("concentration_on_smoke_days.png"))

# Save the plot with a transparent background
ggsave(
  filename,             # Output file path
  plot = p,  # Plot object
  bg = "transparent",   # Background color
  width = 8,           # Width in inches (adjust as needed)
  height = 5,           # Height in inches (adjust as needed)
  dpi = 300             # Resolution (dots per inch)
)  
