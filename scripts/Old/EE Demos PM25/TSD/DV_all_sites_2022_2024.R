library(dplyr)
library(lubridate)
library(stringr)
library(readxl)
library(RAQSAPI)
library(magrittr)
library(ggplot2)

##### 2022 - 2024 AQS Data #####

daily_pm25 <- readRDS("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/DATA/2003_2024_daily_pm25_88101_88502_all_MT.rds")
hourly_pm25 <- readRDS("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/DATA/2003_2024_hourly_pm25_88101_88502_all_MT.rds")

##### Add Flags #####

# Define qualifiers (for exclusion later)
qualifiers <- c(
  "RT - Wildfire-U. S.",
  "RF - Fire - Canadian.",
  "IF - Fire - Canadian.",
  "IT - Wildfire-U. S."
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
  filter(sample_duration == "1 HOUR" & validity_indicator == "Y") %>%
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

daily_pm25_flags <- daily_pm25_flags %>%
  filter(year >= 2022 & year <= 2024)

##### Add Site Names and Metadata #####
sites <- read_excel("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/EE Demos/AQS sites.xlsx")
sites_merge <- sites %>%
  dplyr::select(
    site,
    state_code,
    county_code,
    site_number,
    parameter_code,
    poc,
    monitor_type
  )

# Perform the left join
pm25_DV <- daily_pm25_flags %>%
  left_join(sites_merge, by = c("state_code", "county_code", "site_number", "parameter_code", "poc"))

##### Tiering #####

# Define the path to the Excel file
epa_tiers_path <- "/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/EE Demos/Tiering/tiering_2019_2023.xlsx"
tiering_data <- read_excel(epa_tiers_path)

# Split SITE_ID into state_code, county_code, and site_number
tiering_data <- tiering_data %>%
  mutate(
    state_code = substr(SITE_ID, 1, 2),       # First two digits
    county_code = substr(SITE_ID, 3, 5),      # Next three digits
    site_number = substr(SITE_ID, 6, 9)       # Last four digits
  )
tiering_merge <- tiering_data %>%
  dplyr::select(`Tier 1`, `Tier 2`, state_code, county_code, site_number, month)

pm25_DV <- pm25_DV %>%
  left_join(tiering_merge, by = c("state_code", "county_code", "site_number", "month"))

# Add the EE_tier column based on the specified conditions
pm25_DV <- pm25_DV %>%
  mutate(
    EE_tier = case_when(
      !is.na(qualifier) & qualifier != "" & arithmetic_mean >= `Tier 1` ~ 1,
      !is.na(qualifier) & qualifier != "" & arithmetic_mean < `Tier 1` & arithmetic_mean >= `Tier 2` ~ 2,
      !is.na(qualifier) & qualifier != "" & arithmetic_mean < `Tier 2` ~ 3,
      TRUE ~ NA_real_  # If none of the conditions match, set to NA
    )
  )

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

# Calculate data completeness by site, year, and quarter
quarterly_completeness_summary <- pm25_DV %>%
  group_by(site, poc, year, quarter) %>%
  summarise(
    data_days = n_distinct(date_local),  # Count unique days with data
    total_days = get_total_days(first(year), first(quarter)),  # Get total days for each quarter
    quarter_completeness = data_days / total_days * 100  # Calculate completeness percentage
  ) %>%
  ungroup()

quarter_merge <- quarterly_completeness_summary %>%
  dplyr::select(site, poc, year, quarter, quarter_completeness)

# View the summary
pm25_DV <- pm25_DV %>%
  left_join(quarter_merge, by = c("site", "poc", "year", "quarter"))

##### Annual Completeness #####

# Create the complete_quarters column with unique quarters per year and site
annual_completeness <- pm25_DV %>%
  group_by(year, poc, site, local_site_name) %>%
  summarize(
    complete_quarters = n_distinct(quarter[quarter_completeness >= 75], na.rm = TRUE)
  ) %>%
  ungroup()

# View the summary
pm25_DV <- pm25_DV %>%
  left_join(annual_completeness, by = c("site", "poc", "year", "local_site_name"))

##### Site Completeness 2021-2023 #####

# Count the number of complete quarters per site and add an "active" column
# sites are considered active if they had at least 1 complete quarter in 2023
site_completeness <- annual_completeness %>%
  group_by(site, poc, local_site_name) %>%
  summarize(
    total_complete_quarters = sum(complete_quarters),
    active = ifelse(any(year == 2023 & complete_quarters > 0), "yes", "no"),
    .groups = "drop"
  )

# View the summary
pm25_DV <- pm25_DV %>%
  left_join(site_completeness, by = c("site", "poc", "local_site_name"))

##### Design Values #####
# filter for active sites and remove redundant POCs
pm25_DV_all <- pm25_DV %>%
  filter(active == "yes" &
           !(site == "Helena" & poc == 4)) %>%
  mutate(arithmetic_mean = floor(arithmetic_mean*10)/10)

# quarterly averages
pm25_DV_all <- pm25_DV_all %>%
  group_by(site, poc, year, quarter, parameter_code, monitor_type, total_complete_quarters) %>%
  summarize(quarterly_average = mean(arithmetic_mean, na.rm = TRUE), .groups = 'drop') 

# annual averages
pm25_DV_all <- pm25_DV_all %>%
  group_by(site, poc, year, parameter_code, monitor_type, total_complete_quarters) %>%
  summarize(annual_average = mean(quarterly_average, na.rm = TRUE), .groups = 'drop')

# design value
pm25_DV_all <- pm25_DV_all %>%
  group_by(site, poc, parameter_code, monitor_type, total_complete_quarters) %>%
  summarize(design_value = mean(annual_average, na.rm = TRUE), .groups = 'drop') 

##### Design Values no wildfire flags #####
# filter for active sites, remove redundant POCs, and remove wildfire flagged data 
pm25_DV_no_flags <- pm25_DV %>%
  filter(active == "yes" & 
           !(site == "Helena" & poc == 4) & 
           (is.na(qualifier) | qualifier == "")) %>%  # Exclude rows where qualifier is not empty or NA
  mutate(arithmetic_mean = floor(arithmetic_mean * 10) / 10) # truncate to 1st decimal place (this is how data is presented by EPA from daily data download)

# quarterly averages without wildfire
pm25_DV_no_flags <- pm25_DV_no_flags %>%
  group_by(site, poc, year, quarter, parameter_code, monitor_type, total_complete_quarters) %>%
  summarize(quarterly_average_no_flags = mean(arithmetic_mean, na.rm = TRUE), .groups = 'drop') 

# annual averages without wildfire
pm25_DV_no_flags <- pm25_DV_no_flags %>%
  group_by(site, poc, year, parameter_code, monitor_type, total_complete_quarters) %>%
  summarize(annual_average_no_flags = mean(quarterly_average_no_flags, na.rm = TRUE), .groups = 'drop') 

# design values without wildfire
pm25_DV_no_flags <- pm25_DV_no_flags %>%
  group_by(site, poc, parameter_code, monitor_type, total_complete_quarters) %>%
  summarize(design_value_no_flags = mean(annual_average_no_flags, na.rm = TRUE), .groups = 'drop') 

##### Summaries #####

summary <- pm25_DV_all %>%
  left_join(pm25_DV_no_flags, by = c("site", "poc", "parameter_code", "monitor_type", "total_complete_quarters")) %>%
  mutate(monitor_type = if_else(parameter_code == "88502" & monitor_type == "SPM",
                                "SPM (non-regulatory)",
                                monitor_type),
         monitor_type = if_else(monitor_type == "NON-EPA FEDERAL", 
                                "NPS", 
                                monitor_type)) %>%
  filter(monitor_type != "NPS") %>%
  mutate(monitor_label = ifelse(total_complete_quarters == 12, 
                                "Meets data completeness requirements", 
                                "Does not meet data completeness requirements"))

library(writexl)
write_xlsx(summary, path = "/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/EE Demos/TSD/22_24_DV_and_completeness.xlsx")


##### Plot #1 #####

# Define your custom colors for each parameter_code
custom_colors <- c("Meets data completeness requirements" = "#004A98", "Does not meet data completeness requirements" = "#009ADE")

summary1 <- summary %>%
  filter(parameter_code != "88502" &
           monitor_label == "Meets data completeness requirements" &
           site != "Libby") %>%
  mutate(custom_fill = ifelse(site == "Frenchtown", "Wildfire Smoke Contribution", monitor_label))

# Create the plot
plot1 <- ggplot(summary1, aes(x = reorder(site, desc(design_value)), y = design_value, fill = custom_fill)) + 
  geom_col() +  # Create the column plot for design_value
  geom_col(data = summary1 %>% filter(site == "Frenchtown"), 
           aes(y = design_value_no_flags), 
           fill = "#004A98", alpha = 1) +  # Overlay Frenchtown's design_value_no_flags
  geom_hline(yintercept = 9, linetype = "dashed", color = "black", linewidth = 1.25) +
  labs(title = "Regulatory PM2.5 Monitors",
       x = "",
       y = expression("2021-2023 Design Value - PM"["2.5"] ~ "(µg/m"^3 ~ ")")) +
  theme_minimal() +  # Use a minimal theme
  scale_fill_manual(values = c("Wildfire Smoke Contribution" = "#F54D28",
                               "Meets data completeness requirements" = "#004A98")) +  # Define custom colors
  scale_y_continuous(limits = c(0, 13),
                     expand = c(0, 0),
                     breaks = seq(0, 15, by = 2)) +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # Adjust x-axis text size
    axis.text.y = element_text(size = 12),  # Adjust y-axis text size
    axis.title.x = element_text(size = 14),  # Adjust x-axis title size
    axis.title.y = element_text(size = 14),  # Adjust y-axis title size
    plot.title = element_text(size = 15, face = "bold"),  # Adjust plot title size
    legend.text = element_text(size = 12),  # Adjust legend text size
    legend.title = element_blank(),  # Remove legend title
    legend.position = c(0.7, 0.9),  # Position legend in the top-left corner
    legend.background = element_rect(fill = "white", color = NA),  # Add white background to legend
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),  # Adjust margin around legend items
    legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0),  # Adjust margin around the legend box
    
    # Grid line settings
    panel.grid.major.x = element_blank(),  # Remove vertical grid lines
    panel.grid.minor.x = element_blank(),  # Remove minor vertical grid lines
    panel.grid.major.y = element_line(color = "gray90"),  # Keep major horizontal grid lines
    panel.grid.minor.y = element_blank()  # Remove minor horizontal grid lines
  )

print(plot1)

ggsave(filename = "/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/EE Demos/TSD/TSD Figures/TSD Fig 2.png", plot = plot1, width = 8, height = 6, dpi = 300)

##### Plot #2 #####

summary2 <- summary %>%
  filter(parameter_code != "88502" &
           monitor_label == "Does not meet data completeness requirements")

# Define your custom colors for each parameter_code
custom_colors2 <- c("Meets data completeness requirements" = "#004A98", "Does not meet data completeness requirements" = "#009ADE")

# Create the column plot with manual colors
plot2 <- ggplot(summary2, aes(x = reorder(site, desc(design_value)), y = design_value, fill = monitor_label)) + 
  geom_col() +  # Create the column plot
  geom_hline(yintercept = 9, linetype = "dashed", color = "black", linewidth = 1.25) +
  labs(title = "Regulatory PM2.5 Monitors",
       x = "",
       y = expression("2021-2023 Design Value - PM"["2.5"] ~ "(µg/m"^3 ~ ")")) +
  theme_minimal() +  # Use a minimal theme
  scale_fill_manual(values = custom_colors2) +
  scale_y_continuous(limits = c(0,13),
                     expand = c(0,0),
                     breaks = seq(0,14, by = 2)) +
  theme_classic() + # Use a minimal theme
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # Adjust x-axis text size
    axis.text.y = element_text(size = 12),  # Adjust y-axis text size
    axis.title.x = element_text(size = 14),  # Adjust x-axis title size
    axis.title.y = element_text(size = 14),  # Adjust y-axis title size
    plot.title = element_text(size = 15, face = "bold"),  # Adjust plot title size
    legend.text = element_text(size = 12),  # Adjust legend text size
    legend.title = element_blank(),  # Remove legend title
    legend.position = c(0.7, 0.9),  # Position legend in the top-left corner
    legend.background = element_rect(fill = "white", color = NA),  # Add white background to legend
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),  # Adjust margin around legend items
    legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0),  # Adjust margin around the legend box
    
    # Grid line settings
    panel.grid.major.x = element_blank(),  # Remove vertical grid lines
    panel.grid.minor.x = element_blank(),  # Remove minor vertical grid lines
    panel.grid.major.y = element_line(color = "gray90"),  # Keep major horizontal grid lines
    panel.grid.minor.y = element_blank()  # Remove minor horizontal grid lines
  )

print(plot2)

ggsave(filename = "/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/EE Demos/TSD/TSD Figures/TSD Fig 3.png", plot = plot2, width = 8, height = 6, dpi = 300)

##### Plot #3 #####

summary3 <- summary %>%
  filter(total_complete_quarters == 12 & monitor_type %in% c("SLAMS", "SPM"))

# Define your custom colors for each parameter_code
custom_colors3 <- c("SLAMS" = "#004A98", "SPM" = "#009ADE", "SPM (non-regulatory)" = "#b2d3eb", "NPS" = "#59792E")

# Create the column plot with manual colors
plot3 <- ggplot(summary3, aes(x = reorder(site, desc(design_value)), y = design_value, fill = monitor_type)) + 
  geom_col() +  # Create the column plot
  geom_hline(yintercept = 9, linetype = "dashed", color = "black", linewidth = 1.25) +
  labs(title = bquote(bold("Regulatory DEQ Monitors") ~ "Meeting Data Completeness Requirements"),
       x = "",
       y = expression("2021-2023 Design Value - PM"["2.5"] ~ "(µg/m"^3 ~ ")")) +
  theme_minimal() +  # Use a minimal theme
  scale_fill_manual(values = custom_colors3, 
                    labels = c("SLAMS" = "SLAMS", "NON-EPA FEDERAL" = "Non-EPA Federal")) +
  scale_y_continuous(limits = c(0,13),
                     expand = c(0,0),
                     breaks = seq(0,14, by = 2)) +
  theme_classic() + # Use a minimal theme
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # Adjust x-axis text size
    axis.text.y = element_text(size = 12),  # Adjust y-axis text size
    axis.title.x = element_text(size = 14),  # Adjust x-axis title size
    axis.title.y = element_text(size = 14),  # Adjust y-axis title size
    plot.title = element_text(size = 15, face = "bold"),  # Adjust plot title size
    legend.text = element_text(size = 12),  # Adjust legend text size
    legend.title = element_blank(),  # Remove legend title
    legend.position = c(0.85, 0.9),  # Position legend in the top-left corner
    legend.background = element_rect(fill = "white", color = NA),  # Add white background to legend
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),  # Adjust margin around legend items
    legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0),  # Adjust margin around the legend box
    
    # Grid line settings
    panel.grid.major.x = element_blank(),  # Remove vertical grid lines
    panel.grid.minor.x = element_blank(),  # Remove minor vertical grid lines
    panel.grid.major.y = element_line(color = "gray90"),  # Keep major horizontal grid lines
    panel.grid.minor.y = element_blank()  # Remove minor horizontal grid lines
  )

print(plot3)

ggsave(filename = "/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/EE Demos/TSD/TSD Figures/TSD Fig 4.png", plot = plot3, width = 8, height = 6, dpi = 300)

##### Plot #4 #####

summary4 <- summary %>%
  filter(total_complete_quarters == 12 & monitor_type %in% c("SLAMS", "SPM"))

plot4 <- ggplot(summary4, aes(x = reorder(site, desc(design_value_no_flags)))) + 
  # Plot design_value with custom color
  geom_col(aes(y = design_value, fill = "Wildfire Smoke Contribution"), position = "identity", alpha = .25, color = "gray40") +
  # Plot design_value_no_flags with custom color
  geom_col(aes(y = design_value_no_flags, fill = "DV w/o Wildfire Smoke Days"), position = "identity", alpha = 1) +
  # Add dashed reference line at y = 9
  geom_hline(yintercept = 9, linetype = "dashed", color = "black", linewidth = 1.25) +
  # Labels and theme settings
  labs(title = bquote(bold("Wildfire Smoke Contribution to Regulatory DEQ Monitors")),
       subtitle = "Meeting Data Completeness Requirements",
       x = "",
       y = expression("2021-2023 Design Value - PM"["2.5"] ~ "(µg/m"^3 ~ ")")) +
  scale_y_continuous(limits = c(0, 13),
                     expand = c(0, 0),
                     breaks = seq(0, 14, by = 2)) +
  # Define custom legend colors and labels
  scale_fill_manual(values = c("Wildfire Smoke Contribution" = "#F54D28", "DV w/o Wildfire Smoke Days" = "#004A98")) +
  # Set theme
  theme_classic() + # Use a minimal theme
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # Adjust x-axis text size
    axis.text.y = element_text(size = 12),  # Adjust y-axis text size
    axis.title.x = element_text(size = 14),  # Adjust x-axis title size
    axis.title.y = element_text(size = 14),  # Adjust y-axis title size
    plot.title = element_text(size = 15, face = "bold", vjust = -.5),  # Adjust plot title size
    plot.subtitle = element_text(size = 15),
    legend.text = element_text(size = 12),  # Adjust legend text size
    legend.title = element_blank(),  # Remove legend title
    legend.position = c(0.8, 0.9),  # Position legend in the top-left corner
    legend.background = element_rect(fill = "white", color = NA),  # Add white background to legend
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),  # Adjust margin around legend items
    legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0),  # Adjust margin around the legend box
    
    # Grid line settings
    panel.grid.major.x = element_blank(),  # Remove vertical grid lines
    panel.grid.minor.x = element_blank(),  # Remove minor vertical grid lines
    panel.grid.major.y = element_line(color = "gray90"),  # Keep major horizontal grid lines
    panel.grid.minor.y = element_blank()  # Remove minor horizontal grid lines
  )

print(plot4)

ggsave(filename = "/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/EE Demos/TSD/TSD Figures/TSD Fig 5.png", plot = plot4, width = 8, height = 6, dpi = 300)


##### Plot #5 #####

summary5 <- summary %>%
  filter(total_complete_quarters == 12 & monitor_type %in% c("SLAMS", "SPM")) %>%
  mutate(designation_no_fire = if_else(design_value_no_flags <= 9, "Attainment", "Nonattainment"))

# Define your custom colors for each parameter_code
custom_colors5 <- c("Attainment" = "#004A98", "Nonattainment" = "#F54D28")

# Create the column plot with manual colors
plot5 <- ggplot(summary5, aes(x = reorder(site, desc(design_value_no_flags)), y = design_value_no_flags, fill = designation_no_fire)) + 
  geom_col() +  # Create the column plot
  geom_hline(yintercept = 9, linetype = "dashed", color = "black", linewidth = 1.25) +
  labs(title = bquote(bold("Regulatory DEQ Monitors") ~ "Meeting Data Completeness Requirements"),
       subtitle = "Attainment Status after Excluding Wildfire Smoke Days ",
       x = "",
       y = expression("2021-2023 Design Value - PM"["2.5"] ~ "(µg/m"^3 ~ ")")) +
  theme_minimal() +  # Use a minimal theme
  scale_fill_manual(values = custom_colors5, 
                    labels = c("SLAMS" = "SLAMS", "NON-EPA FEDERAL" = "Non-EPA Federal")) +
  scale_y_continuous(limits = c(0,13),
                     expand = c(0,0),
                     breaks = seq(0,14, by = 2)) +
  theme_classic() + # Use a minimal theme
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # Adjust x-axis text size
    axis.text.y = element_text(size = 12),  # Adjust y-axis text size
    axis.title.x = element_text(size = 14),  # Adjust x-axis title size
    axis.title.y = element_text(size = 14),  # Adjust y-axis title size
    plot.title = element_text(size = 15, face = "bold"),  # Adjust plot title size
    plot.subtitle = element_text(size = 13),
    legend.text = element_text(size = 12),  # Adjust legend text size
    legend.title = element_blank(),  # Remove legend title
    legend.position = c(0.85, 0.9),  # Position legend in the top-left corner
    legend.background = element_rect(fill = "white", color = NA),  # Add white background to legend
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),  # Adjust margin around legend items
    legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0),  # Adjust margin around the legend box
    
    # Grid line settings
    panel.grid.major.x = element_blank(),  # Remove vertical grid lines
    panel.grid.minor.x = element_blank(),  # Remove minor vertical grid lines
    panel.grid.major.y = element_line(color = "gray90"),  # Keep major horizontal grid lines
    panel.grid.minor.y = element_blank()  # Remove minor horizontal grid lines
  )

print(plot5)

ggsave(filename = "/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/EE Demos/TSD/TSD Figures/TSD Fig 6.png", plot = plot5, width = 8, height = 6, dpi = 300)




