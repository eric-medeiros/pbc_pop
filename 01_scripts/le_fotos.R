# função para leitura dos EXIF nas fotos

le_fotos <- function (pasta_data) {
  library(stringr)
  library(exifr)
  library(dplyr)
  library(lubridate)
  
  le_pasta_ID <- function( pasta_ID ) {
    ID <- str_sub(pasta_ID, -5, -1)
    caminho <- list.files(pasta_ID,".JPG", full.names = TRUE)
    arquivo_sub <- list.files(pasta_ID, ".JPG")
    arquivo <- str_remove(str_replace(arquivo_sub, "\\).*?.JPG", ").JPG"), ".JPG")
    exp <- str_subset(unlist(str_split(arquivo, "E|S")), "^\\d{1,}$")
    saida_exp <- str_subset(unlist(str_split(arquivo, "S|A")), "^\\d{1,}$")
    grupo <- str_subset(unlist(str_split(arquivo, "A| ")), "^\\d{1,}$")
    
    dados_pasta_ID <- tibble("exp" = exp,
                             "grupo" = grupo,
                             "ID" = ID,
                             "arquivo" = arquivo,
                             "arquivo_sub" = arquivo_sub,
                             "caminho" = caminho)
    return(dados_pasta_ID)
  }
  
  pasta_fotos <- paste0(pasta_data,"/02_ANALISE/01_HIST_ID")
  lista_pastas_IDs <- list.dirs(pasta_fotos, recursive = FALSE)
  
  # Dados vindo da nomenclatura de pastas e arquivos
  dados_nome <- map_dfr(lista_pastas_IDs, le_pasta_ID, .id = "registro_ID") %>%
    group_by(registro_ID)
  
  # Dados vindo dos metadados do arquivo
  dados_exif <- read_exif(dados_nome$caminho) %>%
    transmute("arquivo_sub" = FileName,
              "lng" = GPSLongitude,
              "lat" = GPSLatitude,
              "datahora" = CreateDate)
  
  dados_IDs <- dados_nome %>%
    left_join(dados_exif, by = "arquivo_sub", multiple = "all") %>%
    dplyr::select(2:4,8:10,5:7,1)
  
  invisible(dados_IDs)
  
  return(dados_IDs)
  
}