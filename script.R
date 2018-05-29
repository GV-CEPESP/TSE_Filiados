# Original Repo: https://github.com/nataliabueno/filiados_TSE/blob/master/filiados_TSE.R

# SUMMARY

# 1. Downloads Data
# 2. Bind Data
# 3. Adds TSE and IBGE codes 

rm(list=ls())

library(tidyverse)
source("FUN.R")

# 1. Downloading ----------------------------------------------------------

## 1.1. Download 
args <- get_args()

log_downloads <- purrr::pmap(args, get_affiliates)

log_downloads %>% 
  transpose() %>% 
  compact()

##1.1.1. Consistência dos Downloads
ufs <- list.files("./data")

length(ufs)

for(uf in ufs){
  print(length(list.files(sprintf("./data/%s", uf))))
}

##1.2. Unzip

log_unzip <- purrr::pmap(args, unzip_affiliates)

log_unzip %>% 
  transpose() %>% 
  .$error %>% 
  compact()

data <- build_data(args$uf, args$party)