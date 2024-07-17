# Função para resumir arquivos por dia
rs_arquivos_dia <- function(pasta_data, data_i, data_f) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(lubridate)
    library(tibble)
    library(purrr)
  })
  
  # Definir as pastas base
  pasta_campo <- file.path(pasta_data, "01_CAMPO")
  pasta_scan <- file.path(pasta_campo, "01_SCAN")
  pasta_gps <- file.path(pasta_campo, "03_GPS")
  pasta_sonda <- file.path(pasta_campo, "04_SONDA")
  pasta_evidencias <- file.path(pasta_campo, "05_EVIDENCIAS")
  
  # Carregar e processar informações de arquivos scan
  info_scan <- tibble(arquivo_scan = list.files(pasta_scan, pattern = "^.{14}.pdf")) %>%
    mutate(data = ymd(str_sub(arquivo_scan, 5, 14)),
           saida = str_sub(arquivo_scan, 1, 3)) %>%
    filter(data > data_i, data < data_f) %>%
    select(saida, data, arquivo_scan)
  
  # Carregar e processar informações de arquivos GPS
  info_gps <- tibble(pasta = list.files(pasta_gps, pattern = ".{14}"),
                     caminho = list.files(pasta_gps, pattern = ".{14}", full.names = TRUE)) %>%
    mutate(data = ymd(str_sub(pasta, 5, 14)) %>% suppressWarnings(),
           saida = str_sub(pasta, 1, 3)) %>%
    filter(data > data_i, data < data_f) %>%
    rowwise() %>%
    mutate(arquivo_trajecto = list.files(caminho, pattern = "Trajecto_\\d{4}-\\d{2}-\\d{2} \\d{6}.gpx"),
           arquivo_wp = list.files(caminho, pattern = "Pontos de passagem_\\d{2}-[A-Z]{3}-\\d{2}.gpx")) %>%
    select(saida, data, arquivo_trajecto, arquivo_wp)
  
  # Carregar e processar informações de arquivos sonda
  info_sonda <- tibble(pasta = list.files(pasta_sonda, pattern = ".{14}"),
                       caminho = list.files(pasta_sonda, pattern = ".{14}", full.names = TRUE)) %>%
    mutate(data = ymd(str_sub(pasta, 5, 14)),
           saida = str_sub(pasta, 1, 3)) %>%
    filter(data > data_i, data < data_f) %>%
    rowwise() %>%
    mutate(arquivo_sonda = list.files(caminho, pattern = ".xls")) %>%
    select(saida, data, arquivo_sonda)
  
  # Carregar e processar informações de evidências
  info_evidencias <- tibble(pasta = list.files(pasta_evidencias, pattern = ".{14}"),
                            caminho = list.files(pasta_evidencias, pattern = ".{14}", full.names = TRUE)) %>%
    mutate(data = ymd(str_sub(pasta, 5, 14)),
           saida = str_sub(pasta, 1, 3)) %>%
    filter(data > data_i, data < data_f) %>%
    rowwise() %>%
    mutate(n_evidencias = length(list.files(caminho, pattern = "JPG|jpg|JPEG|jpeg"))) %>%
    select(saida, data, n_evidencias)
  
  # Combinar todas as informações processadas
  res_arq <- info_scan %>%
    full_join(info_sonda, by = c("saida", "data")) %>%
    full_join(info_gps, by = c("saida", "data")) %>%
    full_join(info_evidencias, by = c("saida", "data"))
  
  return(res_arq)
}