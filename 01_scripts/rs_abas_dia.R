# Pegar os dados de cada aba do Excel, filtrando entre data_i e data_F
rs_abas_dia <- function(pasta_data, data_i, data_f) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(lubridate)
    library(readxl)
  })
  
  arquivo_excel <- file.path(pasta_data, "01_CAMPO", "02_EXCEL", "populacional_PBC.xlsx")
  
  # Aba "saidas"
  datas_saidas <- 
    read_excel(arquivo_excel, sheet = "saidas", col_types = c("text", "date", "text", "text", "text", "text", "text", "text", "text", "text")) %>%
    mutate(data = ymd(data)) %>%
    filter(data > data_i, data < data_f) %>%
    group_by(data) %>%
    summarise(saidas = n())
  
  # Aba "amostragens"
  datas_amostragens <- 
    read_excel(arquivo_excel, sheet = "amostragens", col_types = c("text", "date", "text", "text", "text", "text", "text")) %>%
    mutate(data = ymd(data)) %>%
    filter(data > data_i, data < data_f) %>%
    group_by(data) %>%
    summarise(amostragens = n())
  
  # Aba "climas"
  datas_clima <- 
    read_excel(arquivo_excel, sheet = "climas", col_types = c("text", "date", "text", "text", "text", "text", "text", "text", "text", "text", "text")) %>%
    mutate(data = ymd(data)) %>%
    filter(data > data_i, data < data_f) %>%
    group_by(data) %>%
    summarise(climas = n())
  
  # Aba "avistagens"
  datas_avistagens <- 
    read_excel(arquivo_excel, sheet = "avistagens", col_types = c("text", "date", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text")) %>%
    mutate(data = ymd(data)) %>%
    filter(data > data_i, data < data_f) %>%
    group_by(data) %>%
    summarise(avistagens = n())
  
  # Aba "pausas"
  datas_pausa <- 
    read_excel(arquivo_excel, sheet = "pausas", col_types = c("text", "date", "text", "text", "text")) %>%
    mutate(data = ymd(data)) %>%
    filter(data > data_i, data < data_f) %>%
    group_by(data) %>%
    summarise(pausas = n())
  
  # Aba "wps_extra"
  datas_wpextra <- 
    read_excel(arquivo_excel, sheet = "wps_extra", col_types = c("text", "date", "date", "text", "text", "text", "text")) %>%
    mutate(data = ymd(data)) %>%
    filter(data > data_i, data < data_f) %>%
    group_by(data) %>%
    summarise(wps_extra = n())
  
  # Aba "identificacoes"
  datas_ids <- 
    read_excel(arquivo_excel, sheet = "identificacoes", col_types = c("text", "date", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text")) %>%
    mutate(data = ymd(data)) %>%
    filter(data > data_i, data < data_f) %>%
    group_by(data) %>%
    summarise(ids = n())
  
  # Combinar os dados de todas as abas
  res_abas <- datas_saidas %>%
    left_join(datas_amostragens, by = "data") %>%
    left_join(datas_clima, by = "data") %>%
    left_join(datas_avistagens, by = "data") %>%
    left_join(datas_pausa, by = "data") %>%
    left_join(datas_wpextra, by = "data") %>%
    left_join(datas_ids, by = "data") %>%
    replace(is.na(.), 0) 
  
  return(res_abas)
}