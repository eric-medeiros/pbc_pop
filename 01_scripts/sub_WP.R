sub_WP <- function(pasta_data) {
  
  source("01_scripts/le_planilha.R")
  source("01_scripts/le_WPs.R")
  
  lista <- le_planilha(pasta_data)
  dados_WP <- le_WPs(pasta_data)
  
  # juntando coordenadas inseridas manualmente na planilha com pontos dos GPS
  WP_sub <- lista$WP_extras %>%
    transmute("saida" = saida,
              "datahora" = datahora_extra,
              "WP" = WP_extra,
              "lng" = lng_extra,
              "lat" = lat_extra) %>%
    bind_rows(dados_WP)
  
  # Substituindo onde necessário
  # saidas ----
  lista$saidas <- 
    lista$saidas %>%
    left_join(WP_sub, by = c(saida = "saida", WP_I = "WP")) %>%
    dplyr::select(1:4,
                  "datahora_I" = datahora,
                  "lng_I" = lng,
                  "lat_I" = lat,
                  5:10) %>%
    left_join(WP_sub, by = c(saida = "saida", WP_F = "WP")) %>%
    dplyr::select(1:8,
                  "datahora_F" = datahora,
                  "lng_F" = lng,
                  "lat_F" = lat,
                  9:13)
  
  # amostragens ----
  lista$amostragens <-
    lista$amostragens %>%
    left_join(WP_sub, by = c(saida = "saida", WP_I = "WP")) %>%
    dplyr::select(1:5,
                  "datahora_I" = datahora,
                  "lng_I" = lng,
                  "lat_I" = lat,
                  6:7) %>%
    left_join(WP_sub, by = c(saida = "saida", WP_F = "WP")) %>%
    dplyr::select(1:9,
                  "datahora_F" = datahora,
                  "lng_F" = lng,
                  "lat_F" = lat,
                  10:11)
  
  # clima ----
  lista$clima <-
    lista$clima %>%
    left_join(WP_sub, by = c(saida = "saida", WP_I = "WP")) %>%
    dplyr::select(1:3,
                  "datahora_I" = datahora,
                  "lng_I" = lng,
                  "lat_I" = lat,
                  4:11) %>%
    left_join(WP_sub, by = c(saida = "saida", WP_F = "WP")) %>%
    dplyr::select(1:7,
                  "datahora_F" = datahora,
                  "lng_F" = lng,
                  "lat_F" = lat,
                  8:14)
  
  # avistagens ----
  lista$avistagens <-
    lista$avistagens %>%
    left_join(WP_sub, by = c(saida = "saida", WP_I = "WP")) %>%
    dplyr::select(1:5,
                  "datahora_I" = datahora,
                  "lng_I" = lng,
                  "lat_I" = lat,
                  6:18) %>%
    left_join(WP_sub, by = c(saida = "saida", WP_F = "WP")) %>%
    dplyr::select(1:9,
                  "datahora_F" = datahora,
                  "lng_F" = lng,
                  "lat_F" = lat,
                  10:21)
  
  # pausas ----
  lista$pausas <- 
    lista$pausas %>%
    left_join(WP_sub, by = c(saida = "saida", WP_I = "WP")) %>%
    dplyr::select(1:3, 
                  "datahora_I" = datahora,
                  "lng_I" = lng,
                  "lat_I" = lat,
                  4:5) %>%
    left_join(WP_sub, by = c(saida = "saida", WP_F = "WP")) %>%
    dplyr::select(1:7,
                  "datahora_F" = datahora,
                  "lng_F" = lng,
                  "lat_F" = lat,
                  8)
  # ----
  lista$WP_extras <- NULL
  lista$WPs <- WP_sub
  
  invisible(lista)
  
  return(lista)  
}
