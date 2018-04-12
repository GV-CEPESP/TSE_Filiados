#Test Script

rm(list = ls())

source("FUN.R")

get_affiliates("mt", "pt")

unzip_affiliates("mt", "pt")

data <- build_data("mt", "pt")
