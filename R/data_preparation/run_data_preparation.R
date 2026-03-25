source("R/data_preparation/get_annual_report_data.R")
source("R/data_preparation/utils.R")

config <- yaml::read_yaml("_config.yml")

get_annual_report_data(config)
