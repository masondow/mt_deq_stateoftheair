setwd("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/Shiny_DVtool_v2_copy")
here::i_am(".here")
here()

# Set AQS credentials
aqs_credentials(username = "Cameron.Nealy@mt.gov", key = "copperhawk98")

# Functions
source(here("functions", "data_update_funcs.R"))



#### Sites ####
check_and_update_sites()

#### AQS ####
update_AQS_for_each_parameter()

#### AirNow ####
source(here("data_scripts", "update_AirNow.R"))




