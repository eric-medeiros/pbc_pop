# função para leitura dos EXIF nas fotos

le_fotos <- function ( pasta_data ) {
  library(stringr)
  library(exifr)
  library(dplyr)
  library(lubridate)
  library(purrr)
  library(tibble)
  library(tidyr)
  
  le_pasta_ID <- function( pasta_ID ) {
    
    extrair_exif <- function( caminho_arquivos ) {
      
      ID <- str_extract(caminho_arquivos, "\\bCN\\w+")
      arquivo_sub <- str_extract(caminho_arquivos, "\\bE.+\\.(jpg|JPG|jpeg|JPEG)\\b")
      arquivo <- str_extract(arquivo_sub, "E\\d+S\\d+A\\d+ \\(\\d+\\)")
      exp <- str_extract(arquivo, "(?<=E)\\d+(?=S)") %>% as.integer()
      saida_exp <- str_extract(arquivo, "(?<=S)\\d+(?=A)")
      grupo <- str_extract(arquivo, "(?<=A)\\d+(?=\\s)") %>% as.integer()
      
      tags = c("GPSLatitude", "GPSLongitude", "CreateDate")
      
      exif_info <- read_exif(caminho_arquivos, tags = tags) %>%
        transmute("lng" = GPSLongitude,
                  "lat" = GPSLatitude,
                  "datahora" = ymd_hms(CreateDate)) %>%
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
    
    if ( length(caminho_arquivos) == 0 ) {
      dados_ID <- 
        tibble(ID = ID,
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
      if (file.exists(caminho_cache) ) {file.remove(caminho_cache)}
      
    }
    else if ( !file.exists(caminho_cache) & length(caminho_arquivos) > 0) {
      dados_ID <- extrair_exif(caminho_arquivos)
      write.csv(dados_ID, caminho_cache, row.names = FALSE)
      dados_ID <- 
        dados_ID %>%
        group_by(ID) %>%
        nest() %>%
        mutate(status = "exif lido e cache salvo")
    }
    else if ( file.exists(caminho_cache) & isTRUE(all.equal.character(read.csv(caminho_cache)$caminho, caminho_arquivos)) ) {
      dados_ID <-
        read.csv(caminho_cache) %>% 
        group_by(ID) %>%
        nest() %>%
        mutate(status = "cache lido")
    }
    else if ( file.exists(caminho_cache) & !isTRUE(all.equal.character(read.csv(caminho_cache)$caminho, caminho_arquivos)) ) {
      dados_ID <- extrair_exif(caminho_arquivos)
      write.csv(dados_ID, caminho_cache, row.names = FALSE)
      dados_ID <- 
        dados_ID %>%
        group_by(ID) %>%
        nest() %>%
        mutate(status = "exif lido e cache salvo por cima do antigo")
    } 
    return(dados_ID)
  }
  
  pasta_hist <- paste0(pasta_data,"/02_ANALISE/01_HIST_ID")
  lista_caminhos_pastas_ID <- list.dirs(pasta_hist, recursive = FALSE)
  
  dados_IDs <- lista_caminhos_pastas_ID %>% map_dfr(le_pasta_ID)
  tbl_msg <- dados_IDs %>% ungroup %>% count(status)
  
  cat("-> ", nrow(dados_IDs), " IDs (", nrow(unnest(dados_IDs, cols = data))," identificações)\n",
      "-------------------------------------\n",
      tbl_msg[[1,2]], "\tcom o ", tbl_msg[[1,1]], "\n",
      if (nrow(tbl_msg)>1) {paste0(tbl_msg[[2,2]], "\tcom o ", tbl_msg[[2,1]], "\n")},
      if (nrow(tbl_msg)>2) {paste0(tbl_msg[[3,2]], "\tcom o ", tbl_msg[[3,1]], "\n")},
      if (nrow(tbl_msg)>3) {paste0(tbl_msg[[4,2]], "\tcom o ", tbl_msg[[4,1]], "\n")},
      sep = "")
  
  dados_fotos <- dados_IDs %>%
    select(ID, data) %>%
    unnest(cols = c(data)) %>%
    mutate(exp = as.character(exp),
           grupo = as.character(grupo))
  
  invisible(dados_fotos)
  
  return(dados_fotos)
}


# Sem uso - apagar no futuro se não usar em nenhum outro lugar
#
# pasta_label <- paste0(pasta_data,"/02_ANALISE/03_CATALOGOS/CAT_LABELS")
# labels_caminhos = list.dirs(pasta_labels, recursive = FALSE)
# 
# 
# dados_labels <- 
#   tibble(fotos_caminhos = list.files(labels_caminhos, full.names = TRUE, pattern = ".(jpg|JPG|jpeg|JPEG)"),
#          labels_nome = str_extract(fotos_caminhos, "(?<=_LABELS/).+(?= )"),
#          labels_sigla = str_extract(fotos_caminhos, "(?<=\\().+(?=\\))"),
#          ID = str_extract(fotos_caminhos, "(?<=\\)/CN).+(?=_)"),
#          lado = str_extract(fotos_caminhos, "(?<=CN\\d{3}_).(?=\\.)")) %>% 
#   group_by(ID, labels_nome, labels_sigla) %>%
#   nest() %>%
#   rowwise() %>%
#   mutate(E = "E" %in% data$lado,
#          D = "D" %in% data$lado) %>%
#   arrange(ID) %>%
#   select(ID, labels_sigla, labels_nome, D, E, data)