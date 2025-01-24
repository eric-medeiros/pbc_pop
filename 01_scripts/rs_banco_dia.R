rs_banco_dia <- function(bd, data_i, data_f) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(lubridate)
    library(sf)
    library(stringr)
    library(data.table)
  })
  
  # Função para calcular o período em formato HH:MM:SS
  calcular_periodo <- function(inicio, fim) {
    periodo <- as.period(last(fim) - first(inicio))
    periodo@.Data <- floor(as.numeric(periodo@.Data))
    paste0(str_pad(lubridate::hour(periodo), 2, pad = "0"), ":",
           str_pad(lubridate::minute(periodo), 2, pad = "0"), ":",
           str_pad(lubridate::second(periodo), 2, pad = "0")) %>% 
      suppressWarnings()
  }
  
  # Função para calcular a distância total percorrida em quilômetros
  calcular_distancia <- function(geometry) {
    km_val <- geometry %>%
      st_combine() %>%
      st_cast("MULTILINESTRING") %>%
      st_length() %>%
      {as.double(.)/1000} %>%
      round(1)
    format(km_val, nsmall = 1, decimal.mark = ",")
  }
  
  # Resumo das rotas
  res_rotas <- bd$rota %>%
    filter(data_rota >= data_i, data_rota <= data_f) %>%
    st_as_sf(coords = c("lng", "lat")) %>%
    group_by(saida) %>%
    st_set_crs(4326) %>%
    summarise(
      T_BARCO = calcular_periodo(datahora_ROTA, datahora_ROTA),
      KM = calcular_distancia(geometry)
    ) %>%
    as_tibble() %>%
    select(-geometry)
  
  # Resumo das avistagens
  res_avis <- bd$avistagens %>%
    filter(datahora_I >= data_i, datahora_F <= data_f) %>%
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
      BOTOS = as.integer(sum(tam_est)),
      T_BOTO = {
        total_seconds <- sum(tempo_grupo, na.rm = TRUE)
        periodo <- seconds_to_period(total_seconds)
        paste0(str_pad(lubridate::hour(periodo), 2, pad = "0"), ":",
               str_pad(lubridate::minute(periodo), 2, pad = "0"), ":",
               str_pad(lubridate::second(periodo), 2, pad = "0"))
      },
      GRUPOS = n(),
      FOTOS = sum(as.integer(num_fotos), na.rm = TRUE)
    ) %>%
    arrange(as.numeric(saida))
  
  # Resumo das identificações únicas por saída
  res_id <- bd$identificacoes %>%
    filter(datahora >= data_i, datahora <= data_f) %>%
    group_by(saida) %>%
    summarise(
      IDs = n_distinct(ID)
    )
  
  # Resumo final combinando todas as informações
  res_bd <- bd$saidas %>%
    filter(data >= data_i, data <= data_f) %>%
    left_join(res_rotas, by = "saida") %>%
    left_join(res_avis, by = "saida") %>%
    left_join(res_id, by = "saida") %>%
    ungroup() %>%
    mutate(
      SAIDA = as.numeric(saida) - 59,
      DATA = format(data, "%d/%m/%Y"),
      IDs = nafill(IDs, "const", 0L),
      data = as_date(data)
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
  
  return(res_bd)
}
