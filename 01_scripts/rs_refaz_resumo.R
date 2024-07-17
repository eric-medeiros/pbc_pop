# Função para refazer o resumo
rs_refaz_resumo <- function(pasta_data, data_i, data_f, bd) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(stringr)
    library(lubridate)
  })
  
  # Carregar scripts necessários
  source("01_scripts/rs_arquivos_dia.R")
  source("01_scripts/rs_abas_dia.R")
  source("01_scripts/rs_banco_dia.R")
  source("01_scripts/rs_geo_dia.R")
  source("01_scripts/rs_leaflet_dia.R")
  
  # Executar funções de resumo
  res_arq <- rs_arquivos_dia(pasta_data, data_i, data_f)
  res_abas <- rs_abas_dia(pasta_data, data_i, data_f)
  res_bd <- rs_banco_dia(bd, data_i, data_f)
  res_geo <- rs_geo_dia(bd, data_i, data_f)
  res_lfl <- rs_leaflet_dia(bd, data_i, data_f)
  
  # Combinar resultados
  resumo_especifico <- res_bd %>%
    full_join(res_abas, by = "data") %>%
    full_join(res_arq, by = c("saida", "data")) %>%
    select(-data)
  
  # Resumir resultados finais
  resumo_geral <- resumo_especifico %>%
    mutate(T_BARCO = period_to_seconds(hms(T_BARCO)),
           T_BOTO = period_to_seconds(hms(T_BOTO))) %>%
    summarise(
      Saidas = n(),
      km = format(sum(as.numeric(str_replace(KM, ",", "."))), nsmall = 1, decimal.mark = ","),
      Tempo_barco = {
        sum_barco <- sum(T_BARCO)
        hours <- sum_barco %/% 3600
        mins <- (sum_barco %% 3600) %/% 60
        secs <- sum_barco %% 60
        paste0(str_pad(hours, 2, pad = "0"), ":",
               str_pad(mins, 2, pad = "0"), ":",
               str_pad(secs, 2, pad = "0"))
      },
      Tempo_boto = {
        sum_boto <- sum(T_BOTO, na.rm = TRUE)
        hours <- sum_boto %/% 3600
        mins <- (sum_boto %% 3600) %/% 60
        secs <- sum_boto %% 60
        paste0(str_pad(hours, 2, pad = "0"), ":",
               str_pad(mins, 2, pad = "0"), ":",
               str_pad(secs, 2, pad = "0"))
      },
      Grupos = sum(as.numeric(GRUPOS), na.rm = TRUE),
      Animais = sum(as.numeric(BOTOS), na.rm = TRUE),
      Fotos = sum(as.numeric(FOTOS), na.rm = TRUE)
    )
  
  results <- list(resumo_especifico = resumo_especifico,
                  resumo_geral = resumo_geral,
                  resumo_geopackage = res_geo,
                  resumo_leaflet = res_lfl)
  return(results)
}
