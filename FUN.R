library(magrittr)

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

## 1.2. Binding function

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
  
  dplyr::bind_rows(full_list)
}


