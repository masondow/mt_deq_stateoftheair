# aqi color scale plotting utility

get_aqi_scale <- function(pollutant) {
  if (pollutant == "PM25") {
    list(
      breaks = c(0, 9, 35.4, 55.4, 125.4, 225.4, 250),
      colors = c("rgb(0,228,0)", "rgb(255,255,0)", "rgb(255,126,0)",
                 "rgb(255,0,0)", "rgb(143,63,151)", "rgb(126,0,35)")
    )
  } else {
    list(
      breaks = c(0, 54, 154, 254, 354, 424, 440),
      colors = c("rgb(0,228,0)", "rgb(255,255,0)", "rgb(255,126,0)",
                 "rgb(255,0,0)", "rgb(143,63,151)", "rgb(126,0,35)")
    )
  }
}