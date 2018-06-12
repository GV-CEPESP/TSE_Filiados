library(tidyverse)

data <- read_rds("filiados.rds")

glimpse(data)

data %>% 
  mutate(`NUMERO DA INSCRICAO` == as.numeric(`NUMERO DA INSCRICAO`)) %>% 
  filter(`NUMERO DA INSCRICAO` == 9288940752)
