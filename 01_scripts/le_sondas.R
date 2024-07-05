# Função geral de leitura de arquivos múltiplos de sonda
le_sondas <- function(pasta_data) {
  suppressPackageStartupMessages({
    library(purrr)
  })
  
  # Função de leitura de arquivo individual *.xls da sonda
  le_sonda_arquivo <- function(arquivo_sonda) {
    suppressPackageStartupMessages({
      library(readxl)
      library(magrittr)
      library(dplyr)
      library(lubridate)
      library(stringr)
    })
    
    tem_dados_sonda <- ncol(read_xls(arquivo_sonda, sheet = 2, range = "H1:H1")) != 0
    
    if (tem_dados_sonda) {
      
      sonda_bruto <- as.data.frame(read_xls(arquivo_sonda,
                                            sheet = excel_sheets(arquivo_sonda)[2],
                                            skip = 1,
                                            col_types = c("date", "date", "text", "text", "skip",
                                                          "skip", "skip", "skip", "skip", "skip", 
                                                          "text", "skip", "text", "skip", "text",
                                                          "text", "text", "text", "skip", "skip"),
                                            col_names = c("Date", "Time", "Temp", "pH", "Sal",
                                                          "Pres", "OD", "Turb", "lat", "lng")))
      sonda_sel <- sonda_bruto %>%
        mutate(
          Date = ymd(Date),
          Time = str_sub(as.character(Time), 12, 19),
          datahora_SONDA = ymd_hms(paste(Date, Time)) %>% suppressWarnings(),
          Temp = round(as.double(Temp), 2) %>% suppressWarnings(),
          Sal = round(as.double(Sal), 2) %>% suppressWarnings(),
          OD = round(as.double(OD), 2) %>% suppressWarnings(),
          Turb = round(as.double(Turb), 2) %>% suppressWarnings(),
          pH = round(as.double(pH), 2) %>% suppressWarnings(),
          Pres = round(as.double(Pres), 3) %>% suppressWarnings(),
          lng = as.double(str_c("-", str_sub(lng, 1, 8), sep = "")) %>% suppressWarnings(),
          lat = as.double(str_c("-", str_sub(lat, 1, 8), sep = "")) %>% suppressWarnings(),
          saida = as.character(as.integer(str_sub(arquivo_sonda, -36, -34))),
          arquivo = arquivo_sonda
        ) %>%
        select(datahora_SONDA, saida, Temp, Sal, OD, Turb, pH, Pres, lng, lat, arquivo)
      
    } else {
      sonda_bruto <- as.data.frame(read_xls(arquivo_sonda,
                                            sheet = excel_sheets(arquivo_sonda)[2],
                                            col_types = c("date", "date", "text", "text", "text",
                                                          "text", "text")))
      sonda_sel <- sonda_bruto %>%
        rename(
          Pres = "Press.[psi]",
          lng = "GPS Lat.",
          lat = "GPS Long."
        ) %>%
        mutate(
          Date = ymd(Date),
          Time = str_sub(as.character(Time), 12, 19),
          datahora_SONDA = ymd_hms(paste(Date, Time)),
          Temp = as.double(NA) %>% suppressWarnings(),
          Sal = as.double(NA) %>% suppressWarnings(),
          OD = as.double(NA) %>% suppressWarnings(),
          Turb = as.double(NA) %>% suppressWarnings(),
          pH = as.double(NA) %>% suppressWarnings(),
          Pres = round(as.double(Pres), 3) %>% suppressWarnings(),
          lng = as.double(str_c("-", str_sub(lng, 1, 8), sep = "")) %>% suppressWarnings(),
          lat = as.double(str_c("-", str_sub(lat, 1, 8), sep = "")) %>% suppressWarnings(),
          saida = as.character(as.integer(str_sub(arquivo_sonda, -36, -34))),
          arquivo = arquivo_sonda
        ) %>%
        select(datahora_SONDA, saida, Temp, Sal, OD, Turb, pH, Pres, lng, lat, arquivo)
    }
    
    sonda_sel <- sonda_sel %>%
      filter(!is.na(lat), !is.na(lng))
    
    return(sonda_sel)
  }
  
  lista_arquivos_sonda <- list.files(
    path = file.path(pasta_data, "01_CAMPO", "04_SONDA"),
    full.names = TRUE,
    recursive = TRUE,
    pattern = "xls$"
  )
  
  dados_sonda <- lista_arquivos_sonda %>% map_dfr(le_sonda_arquivo) %>% as_tibble()
  
  return(dados_sonda)
}