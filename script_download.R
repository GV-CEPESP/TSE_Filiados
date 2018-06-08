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

##1.3. Build Data

data <- build_data(args$uf, args$party)

data <- transpose(data)

for(i  in seq_along(data$result)){
  if(!is.null(data$result[[i]])){
    data$result[[i]] <- data$result[[i]] %>% 
      mutate(`NUMERO DA INSCRICAO` = as.character(`NUMERO DA INSCRICAO`))
  }
}

data <- bind_rows(data$result)

# write_rds(data, "filiados.rds")
# write_csv(data, "filiados.csv")
