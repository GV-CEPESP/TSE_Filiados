# Script de Limpeza dos Bancos de Dados

rm(list = ls())

library(tidyverse)

filiados <- read_rds("filiados.rds")

# 1. Pré-Processamento ----------------------------------------------------

## 1.1. Renomenado as variáveis

names(filiados) <- c("DATA_EXTRACAO", "HORA_EXTRACAO", "NUMERO_INSCRICAO", "NOME_FILIADO", "SIGLA_PARTIDO", "NOME_PARTIDO",
                     "UF", "CODIGO_MUNICIPIO", "NOME_MUNICIPIO", "ZONA_ELEITORAL", "SECAO_ELEITORAL", "DATA_FILIACAO",
                     "SIT_REGISTRO", "TIPO_REGISTRO", "DATA_PROCESSAMENTO", "DATA_DESFILIACAO", "DATA_CANCELAMENTO",
                     "DATA_REGULARIZACAO", "MOTIVO_CANCELAMENTO")

glimpse(filiados)

## 1.2. Transformação das Variáveis 

filiados <- filiados %>% 
  mutate(DATA_ENTRADA = parse_date(DATA_FILIACAO, format = "%d/%m/%Y"))

filiados <- filiados %>% 
  mutate(DATA_SAIDA = case_when(!is.na(DATA_DESFILIACAO)  ~ DATA_DESFILIACAO,
                                !is.na(DATA_CANCELAMENTO) ~ DATA_CANCELAMENTO,
                                T                         ~ NA_character_),
         DATA_SAIDA = parse_date(DATA_SAIDA, format = "%d/%m/%Y"))

filiados <- filiados %>% 
  select(NUMERO_INSCRICAO, NOME_FILIADO, SIGLA_PARTIDO, NOME_PARTIDO, UF, CODIGO_MUNICIPIO, NOME_MUNICIPIO, DATA_ENTRADA, DATA_SAIDA)

# 2. Consistência ---------------------------------------------------------

## 2.1. Teste de Repetições

filiados %>% 
  count(NUMERO_INSCRICAO)

# Nesta seção, o objetivo é testar a existência de mais de uma filiação para uma pessoa
# em um mesmo período de tempo.

# 3. Consertos ------------------------------------------------------------

## 3.1. Atribuinda novas datas de saída a partir de outras filiações

filiados <- filiados %>% 
  group_by(NUMERO_INSCRICAO) %>% 
  arrange(NUMERO_INSCRICAO, DATA_ENTRADA) %>% 
  mutate(DATA_SAIDA = lead(DATA_ENTRADA) - 1,
         DATA_SAIDA = ifelse(!is.na(DATA_SAIDA), DATA_SAIDA, as.Date("2018-07-05")))

write_rds(filiados[1:10000,], "filiados_test.rds")

write_rds(filiados, "filiados_new.rds")
