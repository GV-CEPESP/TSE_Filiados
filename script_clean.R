library(tidyverse)

filiados <- read_rds("filiados_test.rds")

# 1. Pré-Processamento ----------------------------------------------------

## 1.1. Renomenado as variáveis

names(filiados) <- c("DATA_EXTRACAO", "HORA_EXTRACAO", "NUMERO_INSCRICAO", "NOME_FILIADO", "SIGLA_PARTIDO", "NOME_PARTIDO",
                     "UF", "CODIGO_MUNICIPIO", "NOME_MUNICIPIO", "ZONA_ELEITORAL", "SECAO_ELEITORAL", "DATA_FILIACAO",
                     "SIT_REGISTRO", "TIPO_REGISTRO", "DATA_PROCESSAMENTO", "DATA_DESFILIACAO", "DATA_CANCELAMENTO",
                     "DATA_REGULARIZACAO", "MOTIVO_CANCELAMENTO")

## 1.2. Transformação das Variáveis 

filiados <- filiados %>% 
  mutate(DATA_ENTRADA = parse_date(DATA_FILIACAO, format = "%d/%m/%Y"),
         DATA_SAIDA   = case_when(!is.na(DATA_DESFILIACAO) ~ DATA_DESFILIACAO,
                                  !is.na(DATA_CANCELAMENTO)~ DATA_CANCELAMENTO,
                                  T                        ~ "19/07/2018"),
         DATA_SAIDA   = parse_date(DATA_SAIDA, format = "%d/%m/%Y"))

filiados <- filiados %>% 
  select(NUMERO_INSCRICAO, NOME_FILIADO, SIGLA_PARTIDO, NOME_PARTIDO, UF, CODIGO_MUNICIPIO, NOME_MUNICIPIO, DATA_ENTRADA, DATA_SAIDA)

## 1.3. Consistência

### 1.3.1. Teste de Repetições

# Nesta seção, o objetivo é testar a existência de mais de uma filiação para uma pessoa
# em um mesmo período de tempo.