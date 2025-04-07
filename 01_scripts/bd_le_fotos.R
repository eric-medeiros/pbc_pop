# Função para leitura dos EXIF nas fotos
bd_le_fotos <- function(pasta_data) {
  suppressPackageStartupMessages({
    library(stringr)
    library(exiftoolr)  # Alterado para exiftoolr
    library(dplyr)
    library(lubridate)
    library(purrr)
    library(tibble)
    library(tidyr)
  })
  
  le_pasta_ID <- function(pasta_ID) {
    
    extrair_exif <- function(caminho_arquivos) {
      # Extraindo ID e outros metadados do caminho do arquivo
      ID <- str_extract(caminho_arquivos, "\\bCN\\w+")
      arquivo_sub <- str_extract(caminho_arquivos, "\\bE.+\\.(jpg|JPG|jpeg|JPEG)\\b")
      arquivo <- str_extract(arquivo_sub, "E\\d+S\\d+A\\d+ \\(\\d+\\)")
      exp <- str_extract(arquivo, "(?<=E)\\d+(?=S)") %>% as.integer()
      saida_exp <- str_extract(arquivo, "(?<=S)\\d+(?=A)")
      grupo <- str_extract(arquivo, "(?<=A)\\d+(?=\\s)") %>% as.integer()
      
      # Definindo as tags EXIF a serem extraídas
      tags = c("GPSLatitude", "GPSLongitude", "CreateDate")
      
      # Lendo as informações EXIF das fotos (função do exiftoolr)
      exif_info <- exif_read(caminho_arquivos, tags = tags) %>%  # Alterado para exif_read
        transmute("lng" = GPSLongitude,
                  "lat" = GPSLatitude,
                  "datahora" = ymd_hms(CreateDate, tz = Sys.timezone()) %>% as.character.Date()) %>%
        bind_cols("exp" = exp,
                  "grupo" = grupo,
                  "ID" = ID,
                  "arquivo" = arquivo,
                  "arquivo_sub" = arquivo_sub,
                  "caminho" = caminho_arquivos) %>%
        select(ID, exp, grupo, datahora, lng, lat, arquivo, arquivo_sub, caminho) 
      
      return(exif_info)
    }
    
    ID <- str_sub(pasta_ID, -5, -1)
    caminho_arquivos <- list.files(pasta_ID, ".(jpg|JPG|jpeg|JPEG)", full.names = TRUE)
    caminho_cache <- paste0(pasta_ID, "/CacheFotos_", ID, ".csv")
    
    if (length(caminho_arquivos) == 0) {
      # Caso não existam arquivos na pasta
      dados_ID <- tibble(ID = ID,
                         data = list(tibble(exp = integer(),
                                            grupo = integer(),
                                            datahora = character(),
                                            lng = double(),
                                            lat = double(),
                                            arquivo = character(),
                                            arquivo_sub = character(),
                                            caminho = character())),
                         status = "ID livre") %>%
        group_by(ID)
      if (file.exists(caminho_cache)) {
        file.remove(caminho_cache)
      }
    } else if (!file.exists(caminho_cache) & length(caminho_arquivos) > 0) {
      # Caso não exista cache e existam arquivos, extrair EXIF e salvar cache
      dados_ID <- extrair_exif(caminho_arquivos)
      write.csv(dados_ID, caminho_cache, row.names = FALSE)
      dados_ID <- dados_ID %>%
        group_by(ID) %>%
        nest() %>%
        mutate(status = "exif lido e cache salvo")
    } else if (file.exists(caminho_cache) & isTRUE(all.equal.character(read.csv(caminho_cache)$caminho, caminho_arquivos))) {
      # Caso exista cache e os arquivos não mudaram, ler do cache
      dados_ID <- read.csv(caminho_cache) %>% 
        group_by(ID) %>%
        nest() %>%
        mutate(status = "cache lido")
    } else if (file.exists(caminho_cache) & !isTRUE(all.equal.character(read.csv(caminho_cache)$caminho, caminho_arquivos))) {
      # Caso exista cache mas os arquivos mudaram, extrair EXIF e atualizar cache
      dados_ID <- extrair_exif(caminho_arquivos)
      write.csv(dados_ID, caminho_cache, row.names = FALSE)
      dados_ID <- dados_ID %>%
        group_by(ID) %>%
        nest() %>%
        mutate(status = "exif lido e cache salvo por cima do antigo")
    }
    
    return(dados_ID)
  }
  
  pasta_hist <- paste0(pasta_data,"/02_ANALISE/01_HIST_ID")
  lista_caminhos_pastas_ID <- list.dirs(pasta_hist, recursive = FALSE)
  
  # Aplicando a função le_pasta_ID para cada pasta de ID
  dados_IDs <- lista_caminhos_pastas_ID %>% map_dfr(le_pasta_ID)
  tbl_msg <- dados_IDs %>% ungroup %>% count(status)
  
  # Transformando os dados extraídos em um formato tabular
  dados_fotos <- dados_IDs %>%
    select(ID, data) %>%
    unnest(cols = c(data)) %>%
    mutate(exp = as.character(exp),
           grupo = as.character(grupo),
           datahora = ymd_hms(datahora, tz = Sys.timezone()))
  
  cat("-> OK - leitura das fotos\n")
  
  return(dados_fotos)
}




# # Função para leitura dos EXIF nas fotos
# bd_le_fotos <- function(pasta_data) {
#   suppressPackageStartupMessages({
#     library(stringr)
#     library(exifr)
#     library(dplyr)
#     library(lubridate)
#     library(purrr)
#     library(tibble)
#     library(tidyr)
#   })
#   
#   le_pasta_ID <- function(pasta_ID) {
#     
#     extrair_exif <- function(caminho_arquivos) {
#       # Extraindo ID e outros metadados do caminho do arquivo
#       ID <- str_extract(caminho_arquivos, "\\bCN\\w+")
#       arquivo_sub <- str_extract(caminho_arquivos, "\\bE.+\\.(jpg|JPG|jpeg|JPEG)\\b")
#       arquivo <- str_extract(arquivo_sub, "E\\d+S\\d+A\\d+ \\(\\d+\\)")
#       exp <- str_extract(arquivo, "(?<=E)\\d+(?=S)") %>% as.integer()
#       saida_exp <- str_extract(arquivo, "(?<=S)\\d+(?=A)")
#       grupo <- str_extract(arquivo, "(?<=A)\\d+(?=\\s)") %>% as.integer()
#       
#       # Definindo as tags EXIF a serem extraídas
#       tags = c("GPSLatitude", "GPSLongitude", "CreateDate")
#       
#       # Lendo as informações EXIF das fotos e organizando em um data frame
#       exif_info <- read_exif(caminho_arquivos, tags = tags) %>%
#         transmute("lng" = GPSLongitude,
#                   "lat" = GPSLatitude,
#                   "datahora" = ymd_hms(CreateDate, tz = Sys.timezone()) %>% as.character.Date()) %>%
#         bind_cols("exp" = exp,
#                   "grupo" = grupo,
#                   "ID" = ID,
#                   "arquivo" = arquivo,
#                   "arquivo_sub" = arquivo_sub,
#                   "caminho" = caminho_arquivos) %>%
#         select(ID, exp, grupo, datahora, lng, lat, arquivo, arquivo_sub, caminho) 
#       
#       return(exif_info)
#     }
#     
#     ID <- str_sub(pasta_ID, -5, -1)
#     caminho_arquivos <- list.files(pasta_ID, ".(jpg|JPG|jpeg|JPEG)", full.names = TRUE)
#     caminho_cache <- paste0(pasta_ID, "/CacheFotos_", ID, ".csv")
#     
#     if (length(caminho_arquivos) == 0) {
#       # Caso não existam arquivos na pasta
#       dados_ID <- tibble(ID = ID,
#                          data = list(tibble(exp = integer(),
#                                             grupo = integer(),
#                                             datahora = character(),
#                                             lng = double(),
#                                             lat = double(),
#                                             arquivo = character(),
#                                             arquivo_sub = character(),
#                                             caminho = character())),
#                          status = "ID livre") %>%
#         group_by(ID)
#       if (file.exists(caminho_cache)) {
#         file.remove(caminho_cache)
#       }
#     } else if (!file.exists(caminho_cache) & length(caminho_arquivos) > 0) {
#       # Caso não exista cache e existam arquivos, extrair EXIF e salvar cache
#       dados_ID <- extrair_exif(caminho_arquivos)
#       write.csv(dados_ID, caminho_cache, row.names = FALSE)
#       dados_ID <- dados_ID %>%
#         group_by(ID) %>%
#         nest() %>%
#         mutate(status = "exif lido e cache salvo")
#     } else if (file.exists(caminho_cache) & isTRUE(all.equal.character(read.csv(caminho_cache)$caminho, caminho_arquivos))) {
#       # Caso exista cache e os arquivos não mudaram, ler do cache
#       dados_ID <- read.csv(caminho_cache) %>% 
#         group_by(ID) %>%
#         nest() %>%
#         mutate(status = "cache lido")
#     } else if (file.exists(caminho_cache) & !isTRUE(all.equal.character(read.csv(caminho_cache)$caminho, caminho_arquivos))) {
#       # Caso exista cache mas os arquivos mudaram, extrair EXIF e atualizar cache
#       dados_ID <- extrair_exif(caminho_arquivos)
#       write.csv(dados_ID, caminho_cache, row.names = FALSE)
#       dados_ID <- dados_ID %>%
#         group_by(ID) %>%
#         nest() %>%
#         mutate(status = "exif lido e cache salvo por cima do antigo")
#     }
#     
#     return(dados_ID)
#   }
#   
#   pasta_hist <- paste0(pasta_data,"/02_ANALISE/01_HIST_ID")
#   lista_caminhos_pastas_ID <- list.dirs(pasta_hist, recursive = FALSE)
#   
#   # Aplicando a função le_pasta_ID para cada pasta de ID
#   dados_IDs <- lista_caminhos_pastas_ID %>% map_dfr(le_pasta_ID)
#   tbl_msg <- dados_IDs %>% ungroup %>% count(status)
#   
#   # Transformando os dados extraídos em um formato tabular
#   dados_fotos <- dados_IDs %>%
#     select(ID, data) %>%
#     unnest(cols = c(data)) %>%
#     mutate(exp = as.character(exp),
#            grupo = as.character(grupo),
#            datahora = ymd_hms(datahora, tz = Sys.timezone()))
#   
#   cat("-> OK - leitura das fotos\n")
#   
#   return(dados_fotos)
# }