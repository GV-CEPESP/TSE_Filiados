library(magrittr)

# 1. Download dos Bancos de Dados -----------------------------------------

get_args <- function(){
  parties <- xml2::read_html("http://filiaweb.tse.jus.br/filiaweb/portal/relacoesFiliados.xhtml") %>% 
    rvest::html_nodes(xpath = "//select[@id = 'partido']/option")
  
  parties_df <- tibble::tibble(partido        = rvest::html_text(parties),
                               partido_value  = rvest::html_attr(parties, "value"))
  
  ufs <- xml2::read_html("http://filiaweb.tse.jus.br/filiaweb/portal/relacoesFiliados.xhtml") %>% 
    rvest::html_nodes(xpath = "//select[@id = 'uf']/option")
  
  ufs_df <- tibble::tibble(uf       = rvest::html_text(ufs),
                           uf_value = rvest::html_attr(ufs, "value"))
  
  args <- expand.grid(party = as.character(parties_df$partido_value),
                      uf    = as.character(ufs_df$uf_value))
  
  args <- purrr::modify(args, as.character)
}

get_affiliates <- function(uf, party){
  
  url_base <- "http://agencia.tse.jus.br/estatistica/sead/eleitorado/filiados/uf/filiados_%s_%s.zip"
  
  url_use <- sprintf(url_base,party, uf)
  
  safe_download <- purrr::safely(download.file)
  
  dir.create(sprintf("./data/%s/%s",uf, party), recursive = T)
  
  safe_download(url      = url_use,
                destfile = sprintf("./data/%s/%s/file.zip",uf, party))
}

unzip_affiliates <- function(uf, party, dir = "./data"){
  
  safe_unzip <- purrr::safely(unzip)
  
  zip_file <- sprintf("%s/%s/%s/file.zip", dir, uf, party)
  
  suppressWarnings(dir.create(sprintf("./data_unziped/%s/%s/", uf, party), recursive = T))
  
  safe_unzip(zipfile = zip_file,
             exdir   = sprintf("./data_unziped/%s/%s", uf, party))
}

build_data <- function(uf_vec, party_vec){
  
  filiados_files <- stringr::str_c("./data_unziped/", uf_vec, "/", party_vec, "/aplic/sead/lista_filiados/uf/filiados_", party_vec, "_", uf_vec, ".csv")
  
  sub_jud_files <- stringr::str_c("./data_unziped/", uf_vec, "/", party_vec, "/aplic/sead/lista_filiados/uf/fil_sub_jud_", party_vec, "_", uf_vec, ".csv")
  
  safe_read <- purrr::safely(readr::read_delim)
  
  locale <-  readr::locale(encoding = "ISO-8859-1")
  
  delim <- ";"
  
  filiados_ls <- purrr::map(filiados_files, safe_read, delim = delim, locale = locale)
  
  filiados_dfs <- filiados_ls %>% 
    transpose() %>% 
    first() %>%
    compact() %>% 
    purrr::map(dplyr::mutate, TIPO = "filiados")
  
  sub_jud_ls <- purrr::map(sub_jud_files, safe_read, delim =  delim,  locale = locale ) 
  
  sub_jud_dfs <- sub_jud_ls %>% 
    transpose() %>% 
    first() %>%
    compact() %>% 
    purrr::map(dplyr::mutate, TIPO = "sub_jud")
  
  full_list <- append(filiados_ls, sub_jud_ls)
}

# 2. Limpeza dos Bancos de Dados ------------------------------------------

entrada1 = as.Date("2010-06-01")
saida1   = as.Date("2010-08-01")
entrada2 = as.Date("2010-07-01")
saida2   = as.Date("2010-10-01")
entrada2 = as.Date("2010-04-01")
saida2   = as.Date("2010-07-01")

overlap_test <- function(entrada1,saida1,entrada2,saida2){
  # Essa função testa se dois pares de datas de entrada e saída possuem
  # uma sobreposição.
  overlap_test <- (entrada1 < entrada2 & saida1 > entrada2) | (entrada2 < entrada1 & saida2 > entrada1)
  
  return(overlap_test)
}

overlap_count <- function(banco){
  # Essa função faz uma contagem de sobreposições de datas de filiação para
  # cada numero de inscrição
  
  ids <- banco$NUMERO_INSCRICAO_INT
  
  ids <- sort(unique(NUM))
  
  for(i in seq_alon(ids)){
    banco_i <- banco[banco$NUMERO_INSCRICAO_INT == ids[[i]],]
    arrange(banco_i, DATA_ENTRADA)
  }
}
