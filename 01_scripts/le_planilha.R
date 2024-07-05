le_planilha <- function(pasta_data) {
  suppressPackageStartupMessages({
    library(readxl)
    library(dplyr)
    library(lubridate)
    library(stringr)
  })
  
  arquivo_excel <- file.path(pasta_data, "01_CAMPO", "02_EXCEL", "populacional_PBC.xlsx")
  
  sheets <- list(
    saidas = list(sheet = "saidas", cols = c("text", "date", "text", "text", "text", "text", "text", "text", "text", "text")),
    amostragens = list(sheet = "amostragens", cols = c("text", "date", "text", "text", "text", "text", "text")),
    climas = list(sheet = "climas", cols = c("text", "date", "text", "text", "text", "text", "text", "text", "text", "text", "text")),
    avistagens = list(sheet = "avistagens", cols = c("text", "date", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text")),
    pausas = list(sheet = "pausas", cols = c("text", "date", "text", "text", "text")),
    wps_extra = list(sheet = "wps_extra", cols = c("text", "date", "date", "text", "text", "text", "text")),
    identificacoes = list(sheet = "identificacoes", cols = c("text", "date", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text"), skip = 4),
    individuos = list(sheet = "individuos", cols = c("text", "text", "text", "text", "text", "text"))
  )
  
  read_and_process <- function(sheet_name, sheet_info) {
    data <- read_excel(arquivo_excel, sheet = sheet_info$sheet, col_types = sheet_info$cols, skip = sheet_info$skip %||% 0)
    data <- data %>%
      mutate(across(where(is.character), as.character),
             across(where(is.Date), as_date),
             across(matches("^wp_"), ~ str_pad(., 3, pad = "0")),
             across(matches("^n_"), ~ as.integer(.), .names = "clean_{col}") %>% suppressWarnings())
    
    if (sheet_name == "wps_extra") {
      data <- data %>%
        mutate(datahora_extra = ymd_hm(paste(data, str_sub(hora_extra, 12, 16)), tz = Sys.timezone())) %>%
        select(-hora_extra)
    }
    
    if (sheet_name == "identificacoes") {
      data <- data %>%
        mutate(filhote_ac = filhote_ac == "V",
               catalogo_atualizado = catalogo_atualizado == "V",
               lado_novo = lado_novo == "V")
    }
    
    return(data)
  }
  
  lista <- lapply(names(sheets), function(sheet_name) {
    sheet_info <- sheets[[sheet_name]]
    read_and_process(sheet_name, sheet_info)
  })
  
  names(lista) <- names(sheets)
  cat("-> planilha ok\n")
  
  return(lista)
}