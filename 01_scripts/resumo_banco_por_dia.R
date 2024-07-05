resumo_banco_por_dia <- function(bd, data_I, data_F) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(lubridate)
    library(sf)
    library(stringr)
    library(data.table)
  })
  
  # Resumo das rotas
  res_rotas <-
    bd$rota %>%
    filter(data_rota >= data_I, data_rota <= data_F) %>%
    st_as_sf(coords = c("lng", "lat")) %>%
    group_by(saida) %>%
    st_set_crs(4326) %>%
    summarise(
      T_BARCO = {
        # Cálculo do período total de navegação
        periodo <- as.period(last(datahora_ROTA) - first(datahora_ROTA))
        periodo@.Data <- floor(as.numeric(periodo@.Data))
        # Formato HH:MM:SS
        paste0(str_pad(lubridate::hour(periodo), 2, pad = "0"), ":",
               str_pad(lubridate::minute(periodo), 2, pad = "0"), ":",
               str_pad(lubridate::second(periodo), 2, pad = "0")) %>% suppressWarnings()
        
      },
      KM = {
        # Cálculo da distância total percorrida em quilômetros
        km_val <- geometry %>%
          st_combine() %>%
          st_cast("MULTILINESTRING") %>%
          st_length() %>%
          {as.double(.)/1000} %>%
          round(1)
        # Formato com vírgula e uma casa decimal
        format(km_val, nsmall = 1, decimal.mark = ",")
      }
    ) %>%
    as_tibble() %>%
    select(-geometry)
  
  # Resumo das avistagens
  res_avis <- 
    bd$avistagens %>%
    filter(datahora_I >= data_I, datahora_F <= data_F) %>%
    mutate(
      tempo_grupo = as.numeric(difftime(datahora_F, datahora_I, units = "secs")),
      tam_grupo = nafill(as.integer(tam_grupo), fill = 0L) %>% suppressWarnings(),
      tam_min = nafill(as.integer(tam_min), fill = 0L) %>% suppressWarnings(),
      tam_max = nafill(as.integer(tam_max), fill = 0L) %>% suppressWarnings()
    ) %>%
    rowwise() %>%
    mutate(tam_est = sum(tam_grupo, mean(c(tam_min, tam_max)))) %>%
    group_by(saida) %>%
    summarise(
      .groups = "keep",
      BOTOS = as.integer(sum(tam_est)),  # Número total de animais avistados
      T_BOTO = {
        # Cálculo do tempo total de avistagem
        total_seconds <- sum(tempo_grupo, na.rm = TRUE)
        periodo <- seconds_to_period(total_seconds)
        # Formato HH:MM:SS
        paste0(str_pad(lubridate::hour(periodo), 2, pad = "0"), ":",
               str_pad(lubridate::minute(periodo), 2, pad = "0"), ":",
               str_pad(lubridate::second(periodo), 2, pad = "0"))
      },
      GRUPOS = n(),  # Número total de grupos avistados
      FOTOS = sum(as.integer(num_fotos), na.rm = TRUE)  # Número total de fotos registradas
    ) %>%
    arrange(as.numeric(saida))
  
  # Resumo das identificações únicas por saída
  res_id <- 
    bd$identificacoes %>%
    filter(datahora >= data_I, datahora <= data_F) %>%
    group_by(saida) %>%
    summarise(
      IDs = n_distinct(ID)  # Número de identificações únicas
    )
  
  # Resumo final combinando todas as informações
  res_bd <- 
    bd$saidas %>%
    filter(data >= data_I, data <= data_F) %>%
    left_join(res_rotas, by = "saida") %>%
    left_join(res_avis, by = "saida") %>%
    left_join(res_id, by = "saida") %>%
    ungroup() %>%
    mutate(
      SAIDA = as.numeric(saida) - 59,  # Ajuste arbitrário para SAIDA
      DATA = {format(data, "%d/%m/%Y")},  # Formato de data dd/mm/aaaa
      data = as_date(data)  # Conversão da coluna data para formato de data
    ) %>%
    select(
      saida,
      data,
      SAIDA,
      DATA,
      KM,
      T_BARCO,
      T_BOTO,
      GRUPOS,
      BOTOS,
      FOTOS,
      LITROS = litros_consumidos,
      IDs
    ) %>%
    arrange(SAIDA)
  
  return(res_bd)  # Retorno do resultado final
}