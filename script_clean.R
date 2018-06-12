library(tidyverse)

filiados <- read_rds("filiados_test.rds")


# 1. Pré-Processamento ----------------------------------------------------

## 1.1. Renomenado as variáveis
## 
names(filiados) <- c("DATA_EXTRACAO", "HORA_EXTRACAO", "NUMERO_INSCRICAO", "NOME_FILIADO", "SIGLA_PARTIDO", "NOME_PARTIDO",
                     "UF", "CODIGO_MUNICIPIO", "NOME_MUNICIPIO", "ZONA_ELEITORAL", "SECAO_ELEITORAL", "DATA_FILIACAO",
                     "SIT_REGISTRO", "TIPO_REGISTRO", "DATA_PROCESSAMENTO", "DATA_DESFILIACAO", "DATA_CANCELAMENTO",
                     "DATA_REGULARIZACAO", "MOTIVO_CANCELAMENTO")