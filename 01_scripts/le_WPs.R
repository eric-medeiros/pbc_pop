# Função de leitura de arquivos múltiplos de WPs
le_WPs <- function(pasta_data) {
  library(data.table)
  library(purrr)
  library(abind)
  
  # Função de leitura de arquivo indivudual de *.gpx do tipo track
  le_WP <- function(arquivo_WP) {
    library(sf)
    library(dplyr)
    library(data.table)
    library(lubridate)
    
    WP <- st_read(arquivo_WP, layer = "waypoints", quiet = TRUE)
    
    saida <- suppressWarnings(as.character(as.integer(str_extract(str_split(arquivo_WP, "/"), "\\d{3}"))))
    lng <- st_coordinates(WP)[,"X"]
    lat <- st_coordinates(WP)[,"Y"]
    
    WP <- WP %>%
      as_tibble() %>%
      transmute("saida" = saida,
                "datahora" = as.character(time),
                "WP" = name,
                "lng" = lng,
                "lat" = lat)
    
    return(WP)
  }
  
  # Listagem dos arquivos *.gpx da pasta_GPS
  lista_arquivos_WP <- list.files (paste(pasta_data, "/01_CAMPO/03_GPS", sep = ""),
                                   recursive = TRUE,
                                   full.names = TRUE,
                                   pattern = "Pontos")
  
  # Aplicando a função le_WP para todos arquivos da lista lista_arquivos_WP
  dados_WP <- map_dfr(lista_arquivos_WP, le_WP, .id="registro_PONTOS") %>%
    mutate(datahora = ymd_hms(datahora))
    
  invisible(dados_WP)
  
  return(dados_WP)
}
