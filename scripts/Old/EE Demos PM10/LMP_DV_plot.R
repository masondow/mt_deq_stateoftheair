library(dplyr)
DV_list <- read_excel("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/DATA/PM10/2000_2024_5yr_PM10_DVs_24h_wf98.xlsx")

LMP_DVs <- DV_list %>%
  filter(local_site_name %in% c("Butte Greeley School",
                                "Flathead Valley",
                                "Kalispell Flathead Electric",
                                "Libby Courthouse Annex",
                                "MSLA Boyd Park",
                                "THOMPSON FALLS HIGH SCHOOL",
                                "White Fish Dead End") &
           year >= 2019) %>%
  select(local_site_name,
         poc,
         year,
         DV_3yr_avg_exceedance,
         DV_3yr_avg_exceedance_no_wildfire)

# plot with EEs
with_wildfire <- ggplot(LMP_DVs, aes(x = year, y = DV_3yr_avg_exceedance, color = local_site_name, group = local_site_name)) +
  geom_line(size = .8) +  # Adjust line thickness
  geom_point(size = 1.25) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = .5) +  # Add dashed line at 98
  scale_color_manual(values = c("#c62828", "#e91e63", "#3949ab", "#039be5", "#43a047", "#c0ca33", "#ffb300")) +  # Adjust colors
  labs(
    title = "LMP Sites 24-hr PM10 DV",
    subtitle = "Exceptional Events Included",
    x = "Year",
    y = "Estimated Exceedances",
    color = "Site Name"
  ) +
  scale_y_continuous(breaks = seq(0, 2, by = .5),
                     limits = c(0, 2)) +  # Y-axis labels every 20
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),   # Bold title
    plot.subtitle = element_text(face = "plain", size = 12), # Regular subtitle
    panel.grid.major.y = element_line(color = "gray95", size = 0.5),  # Major horizontal grid lines every 10
    panel.grid.minor.y = element_line(color = "gray95", size = 0.5),  # No minor horizontal grid lines
    panel.grid.major.x = element_line(color = "gray95", size = 0.5),  # Major vertical grid lines
    panel.grid.minor.x = element_blank()   # No minor vertical grid lines
  )
print(with_wildfire)

# Save the plot as a PNG
ggsave(
  "/Users/cameronnealy/Downloads/LMP_DV.png",         # File name
  plot = with_wildfire,         # ggplot object
  width = 7,             # Width in inches (can adjust to pixels)
  height = 3.5,             # Height in inches (can adjust to pixels)
  dpi = 300,              # Resolution (dots per inch)
  units = "in",           # Units for width and height ('in' for inches)
  device = "png"          # File type
)

# plot w/o wildfire EEs >98
no_wildfire <- ggplot(LMP_DVs, aes(x = year, y = DV_3yr_avg_exceedance_no_wildfire, color = local_site_name, group = local_site_name)) +
  geom_line(size = .8) +  # Adjust line thickness
  geom_point(size = 1.25) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = .5) +  # Add dashed line at 98
  scale_color_manual(values = c("#c62828", "#e91e63", "#3949ab", "#039be5", "#43a047", "#c0ca33", "#ffb300")) +  # Adjust colors
  labs(
    title = "LMP Sites 24-hr PM10 DV",
    subtitle = "Wildfire Flags >150 ug/m3 Excluded",
    x = "Year",
    y = "Estimated Exceedances",
    color = "Site Name"
  ) +
  scale_y_continuous(breaks = seq(0, 2, by = .5),
                     limits = c(0, 2)) +  # Y-axis labels every 20
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),   # Bold title
    plot.subtitle = element_text(face = "plain", size = 12), # Regular subtitle
    panel.grid.major.y = element_line(color = "gray95", size = 0.5),  # Major horizontal grid lines every 10
    panel.grid.minor.y = element_line(color = "gray95", size = 0.5),  # No minor horizontal grid lines
    panel.grid.major.x = element_line(color = "gray95", size = 0.5),  # Major vertical grid lines
    panel.grid.minor.x = element_blank()   # No minor vertical grid lines
  )
print(no_wildfire)

# Save the plot as a PNG
ggsave(
  "/Users/cameronnealy/Downloads/LMP_DV_no_wildfire_over_150.png",         # File name
  plot = no_wildfire,         # ggplot object
  width = 7,             # Width in inches (can adjust to pixels)
  height = 3.5,             # Height in inches (can adjust to pixels)
  dpi = 300,              # Resolution (dots per inch)
  units = "in",           # Units for width and height ('in' for inches)
  device = "png"          # File type
)



