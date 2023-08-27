# This is the script used to filter the earthquake data that only included last 12 month

#==================================================================
# For reproduce a different month report date, only need to change the global_variables.R file
#==================================================================

library(tidyverse)
library(lubridate)

here::i_am("scr/Monthly_report/Mon_rep_data_preprocess.R")

library(here)

# Read the global variables into the environment
source(here::here("scr","global_variables.R"))

# Read the induced earthquake csv file and save it
earthquake_tbl <- readr::read_csv(here::here("data","raw", file_name))

# Filter the earthquake dataset to only include the last 12 months of data
end_date <- ymd(gsub("_induced-earthquakes.csv", "", file_name))
start_date <- end_date %m-% months(12)
earthquake_tbl_filtered <- earthquake_tbl %>%
  filter(date >= start_date & date < end_date)

# Save the filtered dataset to the derived folder in data folder
filter_data_name <- paste0("last_12_months_", file_name)
write_csv(earthquake_tbl_filtered, here::here("data","derived", filter_data_name))
