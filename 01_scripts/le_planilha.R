# Função para abrir todas abas do arquivo *.xls de campo

le_planilha <- function(pasta_data) {
  library(readxl)
  library(dplyr)
  library(stringr)
  library(lubridate)
  
  arquivo_excel <- paste(pasta_data, "/01_CAMPO/02_EXCEL/populacional_PBC.xlsx", sep = "")
  
  saidas <- read_excel(arquivo_excel, sheet = 1, 
                       col_types = c("text", "text", "text", "text", "text",
                                     "text", "text", "text", "text", "text"))
  
  amostragens <- read_excel(arquivo_excel, sheet = 2, 
                            col_types = c("text", "text", "text", "text", "text", 
                                          "text", "text"))
  
  clima <- read_excel(arquivo_excel, sheet = 3, 
                      col_types = c("text", "date", "text", "text", "text", 
                                    "text", "text", "text", "text", "text", 
                                    "text"))
  
  avistagens <- read_excel(arquivo_excel, sheet = 4, 
                           col_types = c("text", "date", "text", "text", "text", 
                                         "text", "text", "text", "text", "text", 
                                         "text", "text", "text", "text", "text", 
                                         "text", "text", "text"))
  
  pausas <- read_excel(arquivo_excel, sheet = 5,
                       col_types = c("text", "date", "text", "text", "text"))
  
  WP_extras <- read_excel(arquivo_excel, sheet = 6,
                          col_types = c("text", "date", "text", "date", "text",
                                        "text", "text", "text"))
  
 identificacoes <- read_excel(arquivo_excel, sheet = 7, 
                      col_types = c("text", "date", "text", "text", "text", 
                                    "text", "text", "text", "text", "text",
                                    "text")
  )
  
  saidas$saida <- as.character(saidas$saida)
  saidas$data <- dmy(saidas$data)
  saidas$barco <- as.character(saidas$barco)
  saidas$WP_I <- str_pad(saidas$WP_I, 3, "left", "0")
  saidas$WP_F <- str_pad(saidas$WP_F, 3, "left", "0")
  saidas$rota <- as.character(saidas$rota)
  saidas$equipe <- as.character(saidas$equipe)
  saidas$barqueiro <- as.character(saidas$barqueiro)
  suppressWarnings(saidas$litros_consumidos <- as.numeric(saidas$litros_consumidos, na.rm=TRUE))
  
  amostragens$saida <- as.character(amostragens$saida)
  amostragens$data <- dmy(amostragens$data)
  amostragens$exp <- as.character(amostragens$exp)
  amostragens$rota <- as.character(amostragens$rota)
  amostragens$WP_I <- str_pad(amostragens$WP_I, 3, "left", "0")
  amostragens$WP_F <- str_pad(amostragens$WP_F, 3, "left", "0")
  
  clima$saida <- as.character(clima$saida)
  clima$data <- ymd(clima$data)
  clima$WP_I <- str_pad(clima$WP_I, 3, "left", "0")
  clima$WP_F <- str_pad(clima$WP_F, 3, "left", "0")
  clima$dir_vento <- as.character(clima$dir_vento)
  suppressWarnings(clima$veloc_vento <- as.numeric(clima$veloc_vento))
  clima$beaufort <- as.numeric(clima$beaufort)
  clima$cobert_nuvens <- as.numeric(clima$cobert_nuvens)
  clima$visibilidade <- as.numeric(clima$visibilidade)
  clima$reflexo <- as.numeric(clima$reflexo)
  
  avistagens$saida <- as.character(avistagens$saida)
  avistagens$data <- ymd(avistagens$data)
  avistagens$grupo <- as.character(avistagens$grupo)
  avistagens$num_fotos <- as.integer(avistagens$num_fotos)
  avistagens$WP_I <- str_pad(avistagens$WP_I, 3, "left", "0")
  avistagens$WP_F <- str_pad(avistagens$WP_F, 3, "left", "0")
  avistagens$coesao <- as.character(avistagens$coesao)
  avistagens$estado <- as.character(avistagens$estado)
  suppressWarnings(avistagens$tam_grupo <- as.integer(avistagens$tam_grupo))
  suppressWarnings(avistagens$tam_min <- as.integer(avistagens$tam_min))
  suppressWarnings(avistagens$tam_max <- as.integer(avistagens$tam_max))
  suppressWarnings(avistagens$n_marcados <- as.integer(avistagens$n_marcados))
  suppressWarnings(avistagens$n_lisos <- as.integer(avistagens$n_lisos))
  suppressWarnings(avistagens$n_neonatos <- as.integer(avistagens$n_neonatos))
  suppressWarnings(avistagens$n_infantes <- as.integer(avistagens$n_infantes))
  suppressWarnings(avistagens$n_juvenis <- as.integer(avistagens$n_juvenis))
  suppressWarnings(avistagens$n_adultos <- as.integer(avistagens$n_adultos))
  
  pausas$saida <- as.character(pausas$saida)
  pausas$data <- ymd(pausas$data)
  pausas$WP_I <- str_pad(pausas$WP_I, 3, "left", "0")
  pausas$WP_F <- str_pad(pausas$WP_F, 3, "left", "0")
  
  WP_extras$saida <- as.character(WP_extras$saida)
  WP_extras$data <- ymd(WP_extras$data)
  WP_extras$aba <- as.character(WP_extras$aba)
  WP_extras$datahora_extra <- ymd_hm(paste(WP_extras$data, str_sub(WP_extras$hora_extra, 12, 16))) + hours(3)
  WP_extras$hora_extra <- NULL
  WP_extras$WP_extra <- str_pad(WP_extras$WP_extra, 3, "left", "0")
  WP_extras$lng_extra <- as.numeric(WP_extras$lng_extra)
  WP_extras$lat_extra <- as.numeric(WP_extras$lat_extra)
  WP_extras <- WP_extras %>%
    dplyr::select(c(1:4, 8, 5:7))
  
  identificacoes$saida <- as.character(identificacoes$saida)
  identificacoes$data <- ymd(identificacoes$data)
  identificacoes$grupo <- as.character(identificacoes$grupo)
  identificacoes$arquivo <- as.character(identificacoes$arquivo)
  identificacoes$ID <- as.character(identificacoes$ID)
  identificacoes$quali_F <- as.character(identificacoes$quali_F)
  identificacoes$quali_M <- as.character(identificacoes$quali_M)
  identificacoes$lado <- as.character(identificacoes$lado)
  identificacoes$seq <- as.character(identificacoes$seq)
  identificacoes$filhote_ac <- as.character(identificacoes$filhote_ac)
  
  
  #Junção de todas tabelas em uma lista
  lista_L1 <- list(saidas = saidas,
                   amostragens = amostragens,
                   clima = clima,
                   avistagens = avistagens,
                   pausas = pausas,
                   WP_extras = WP_extras,
                   identificacoes = identificacoes)
  
  invisible(lista_L1)
  
  return(lista_L1)
  
  
}

