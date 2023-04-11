# Funcao geral de leitura de arquivos múltiplos de GPS 

le_rota <- function(pasta_data) {
  library(purrr)
  library(abind)
  library(data.table)
  library(lubridate)
  library(stringr)
  library(tidyr)
  library(stringr)
  library(dplyr)

  # Funcao de leitura de arquivo indivudual de *.gpx do tipo track
  le_rota_arquivo <- function(arquivo_rota) {
    library(sf)
    library(dplyr)
    library(lubridate)
    library(sp)
    
    rota_pontos <- st_read(arquivo_rota, layer = "track_points", quiet = TRUE)
    
    saida <- as.character(as.integer(suppressWarnings(str_extract(str_split(arquivo_rota, "/"), "\\d{3}"))))
    data <- as.character.Date(suppressWarnings(str_extract(str_split(arquivo_rota, "/"), "\\d{4}_\\d{2}_\\d{2}")))
    lng <- st_coordinates(rota_pontos)[,"X"]
    lat <- st_coordinates(rota_pontos)[,"Y"]
    dist_p_prox <- st_distance(rota_pontos$geometry, lead(rota_pontos$geometry), by_element = TRUE)
    datahora_prox <- lead(rota_pontos$time)
    tempo_p_prox <- datahora_prox - rota_pontos$time
    
    
    rota_pontos <- rota_pontos %>%
      as_tibble() %>%
      transmute("saida" = saida,
                "datahora_ROTA" = as.character(time),
                "lng" = lng,
                "lat" = lat,
                "dist_p_prox" = dist_p_prox,
                "tempo_p_prox" = tempo_p_prox,
                "data_rota" = data,
                "arquivo" = arquivo_rota
      )
    
    
    return(rota_pontos)
  }
  
  # Listagem dos arquivos dentro da pasta_GPS
  lista_arquivos_rota <- list.files(paste(pasta_data, "/01_CAMPO/03_GPS", sep = ""),
                                   full.names = TRUE,
                                   pattern = "Trajecto",
                                   recursive = TRUE)
  
  # Aplicando a funcao le_rota_pontos_arquivo para cada arquivo na lista de arquivos da pasta_GPS
  
  dados_rota <- map_dfr(lista_arquivos_rota, le_rota_arquivo, .id = "registro_ROTA") %>%
    group_by(registro_ROTA)
  

  return(dados_rota)
}
