# Funcao geral de leitura de arquivos múltiplos de GPS 

le_rotas <- function(pasta_data) {
  library(purrr)
  library(data.table)
  library(lubridate)
  library(stringr)
  library(tidyr)
  library(dplyr)
  
  # Funcao de leitura de arquivo indivudual de *.gpx do tipo track
  le_rota_arquivo <- function(arquivo_rota) {
    library(sf)
    library(dplyr)
    library(lubridate)
    library(stringr)
    library(sp)
    
    rota_pontos <-
      st_read(arquivo_rota, layer = "track_points", quiet = TRUE) %>%
      transmute(saida = str_extract(arquivo_rota, "(?<=/)\\d{3}(?=_)") %>% as.integer() %>% as.character(),
                datahora_ROTA = as_datetime(time, tz = Sys.timezone()),
                data_rota = ymd(str_extract(arquivo_rota, "\\d{4}_\\d{2}_\\d{2}")),
                lng = st_coordinates(.)[,"X"],
                lat = st_coordinates(.)[,"Y"],
                arquivo = arquivo_rota) %>%
      as_tibble() %>%
      select(-geometry)
    
    return(rota_pontos)
  }
  # Listagem dos arquivos dentro da pasta_GPS
  caminho_pasta_gps <- paste(pasta_data, "/01_CAMPO/03_GPS", sep = "")
  
  lista_arquivos_rota <- list.files(list.dirs(path = caminho_pasta_gps,
                                              recursive = FALSE,
                                              full.names = TRUE),
                                    full.names = TRUE,
                                    pattern = "Trajecto",
                                    recursive = FALSE)
  
  # Aplicando a funcao le_rota_pontos_arquivo para cada arquivo na lista de arquivos da pasta_GPS
  
  dados_rota <- lista_arquivos_rota %>% map_dfr(le_rota_arquivo)
  
  return(dados_rota)
  }
