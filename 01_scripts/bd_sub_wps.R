# Função para juntar os dados
bd_sub_wps <- function(dados_excel, dados_wps) {
  suppressPackageStartupMessages({
    library(tidyr)
  })
  
  # Juntando coordenadas inseridas manualmente na planilha com pontos dos GPS
  # Preparando os dados wp_sub
  dados_wps_tudo <-
    dados_excel$wps_extra %>%
    transmute(
      saida = saida,
      datahora = datahora_extra,
      wp = wp_extra,
      lng = as.double(lng_extra),
      lat = as.double(lat_extra)
    ) %>%
    bind_rows(dados_wps) %>%
    arrange(datahora)
  
  wp_sub <-
    dados_wps_tudo %>%
    select(saida, datahora, wp, lng, lat)
  
  # Função para atualizar coordenadas genérica
  atualiza_coordenadas_generico <- function(dados, wp_sub, tipo) {
    dados[[tipo]] <-
      dados[[tipo]] %>%
      left_join(wp_sub, by = c("saida", "wp_i" = "wp")) %>%
      rename(wp_I = wp_i) %>%
      dplyr::select(
        1:wp_I,
        datahora_I = datahora,
        lng_I = lng,
        lat_I = lat,
        wp_I:last_col()
      ) %>%
      left_join(wp_sub, by = c("saida", "wp_f" = "wp")) %>%
      rename(wp_F = wp_f) %>%
      dplyr::select(
        1:wp_F,
        datahora_F = datahora,
        lng_F = lng,
        lat_F = lat,
        wp_I:last_col()
      ) %>%
      mutate(
        lng_I = round(lng_I, 5),
        lat_I = round(lat_I, 5),
        lng_F = round(lng_F, 5),
        lat_F = round(lat_F, 5)
      )
    
    return(dados)
  }
  
  # Aplicando a função para cada seção em dados_excel
  tipos <- c("saidas", "amostragens", "climas", "avistagens", "pausas")
  for (tipo in tipos) {
    dados_excel <- atualiza_coordenadas_generico(dados_excel, wp_sub, tipo)
  }
  
  # Atualizar dados de caminhos
  dados_excel$caminhos <-
    dados_wps_tudo %>%
    select(-lng, -lat, -datahora) %>%
    filter(arquivo_wp != "NA") %>%
    nest(id = c(saida, wp)) %>%
    transmute(
      tipo = "gps_wp",
      id = id,
      arquivo = basename(arquivo_wp),
      pasta = dirname(arquivo_wp)
    ) %>%
    bind_rows(dados_excel$caminhos)
  
  #Adicionar wps ao banco
  dados_excel$wps <- wp_sub
  
  # Remover dados extras de wps
  dados_excel$wps_extra <- NULL
  
  cat("-> OK - substituição dos wps \n")
  
  return(dados_excel)
}



