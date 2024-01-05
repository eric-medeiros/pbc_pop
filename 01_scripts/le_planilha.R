le_planilha <- function(pasta_data) {
  library(readxl)
  library(dplyr)
  library(lubridate)
  library(stringr)
  
  arquivo_excel <- paste(pasta_data, "/01_CAMPO/02_EXCEL/populacional_PBC.xlsx", sep = "")
  
  saidas <-
    read_excel(arquivo_excel,
               sheet = "saidas",
               col_types = c("text", "date", "text", "text", "text", "text", "text", "text", "text", "text"))
  
  amostragens <-
    read_excel(arquivo_excel,
               sheet = "amostragens",
               col_types = c("text", "date", "text", "text", "text", "text", "text"))
  
  climas <-
    read_excel(arquivo_excel,
               sheet = "climas",
               col_types = c("text", "date", "text", "text", "text", "text", "text", "text", "text", "text", "text"))
  
  avistagens <-
    read_excel(arquivo_excel,
               sheet = "avistagens",
               col_types = c("text", "date", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text"))
  
  pausas <-
    read_excel(arquivo_excel,
               sheet = "pausas",
               col_types = c("text", "date", "text", "text", "text"))
  
  wps_extra <-
    read_excel(arquivo_excel,
               sheet = "wps_extra",
               col_types = c("text", "date", "text", "date", "text", "text", "text", "text"))
  
  identificacoes <-
    read_excel(arquivo_excel,
               sheet = "identificacoes",
               col_types = c("skip", "text", "date", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text"),
               skip = 4)
  
  # individuos <- read_excel(arquivo_excel, sheet = "individuos", col_types = c("text", "text", "text", "text", "text", "text"), skip = 18)
  
  saidas <- 
    saidas %>%
    rename(wp_I = "wp_i",
           wp_F = "wp_f") %>%
    mutate(saida = as.character(saida),
           data = as_date(data),
           barco = as.character(barco),
           wp_I = str_pad(wp_I, 3, "left", "0"),
           wp_F = str_pad(wp_F, 3, "left", "0"),
           rota = as.character(rota),
           equipe = as.character(equipe),
           barqueiro = as.character(barqueiro),
           litros_consumidos = as.numeric(litros_consumidos, na.rm = TRUE) %>% suppressWarnings())
  
  amostragens <- 
    amostragens %>%
    rename(wp_I = "wp_i",
           wp_F = "wp_f") %>%
    mutate(saida = as.character(saida),
           data = as_date(data),
           exp = as.character(exp),
           rota = as.character(rota),
           wp_I = str_pad(wp_I, 3, "left", "0"),
           wp_F = str_pad(wp_F, 3, "left", "0"))
  
  climas <- 
    climas %>%
    rename(wp_I = "wp_i",
           wp_F = "wp_f") %>%
    mutate(saida = as.character(saida),
           data = as_date(data),
           wp_I = str_pad(wp_I, 3, "left", "0"),
           wp_F = str_pad(wp_F, 3, "left", "0"),
           dir_vento = as.character(dir_vento),
           veloc_vento = as.numeric(veloc_vento) %>% suppressWarnings(),
           beaufort = as.numeric(beaufort),
           cobert_nuvens = as.numeric(cobert_nuvens),
           visibilidade = as.numeric(visibilidade),
           reflexo = as.numeric(reflexo))
  
  avistagens <- 
    avistagens %>%
    rename(wp_I = "wp_i",
           wp_F = "wp_f") %>%
    mutate(saida = as.character(saida),
           data = as_date(data),
           oc_id = as.factor(oc_id),
           grupo = as.character(grupo),
           num_fotos = as.integer(num_fotos),
           wp_I = str_pad(wp_I, 3, "left", "0"),
           wp_F = str_pad(wp_F, 3, "left", "0"),
           coesao = as.character(coesao),
           estado = as.character(estado),
           tam_grupo = as.integer(tam_grupo) %>% suppressWarnings(),
           tam_min = as.integer(tam_min) %>% suppressWarnings(),
           tam_max = as.integer(tam_max) %>% suppressWarnings(),
           n_marcados = as.integer(n_marcados) %>% suppressWarnings(),
           n_lisos = as.integer(n_lisos) %>% suppressWarnings(),
           n_neonatos = as.integer(n_neonatos) %>% suppressWarnings(),
           n_infantes = as.integer(n_infantes) %>% suppressWarnings(),
           n_juvenis = as.integer(n_juvenis) %>% suppressWarnings(),
           n_adultos = as.integer(n_adultos) %>% suppressWarnings())
  
  pausas <- 
    pausas %>% 
    rename(wp_I = "wp_i",
           wp_F = "wp_f") %>%
    mutate(saida = as.character(saida),
           data = as_date(data),
           wp_I = str_pad(wp_I, 3, "left", "0"),
           wp_F = str_pad(wp_F, 3, "left", "0"))
  
  wps_extra <- 
    wps_extra %>%
    rename(wp_extra = "wp_extra") %>%
    mutate(saida = as.character(saida),
           data = as_date(data),
           aba = as.character(aba),
           datahora_extra = ymd_hm(paste(data, str_sub(hora_extra, 12, 16)), tz = Sys.timezone()),
           hora_extra = NULL,
           wp_extra = str_pad(wp_extra, 3, "left", "0"),
           lng_extra = as.numeric(lng_extra),
           lat_extra = as.numeric(lat_extra)) %>%
    dplyr::select(c(1:4, 8, 5:7))
  
  identificacoes <- 
    identificacoes %>%
    rename(ID = "id",
           quali_F = "quali_f",
           quali_M = "quali_m") %>%
    mutate(saida = as.character(saida),
           data = as_date(data),
           grupo = as.character(grupo),
           ID = as.character(ID),
           arquivo = as.character(arquivo),
           quali_F = as.character(quali_F),
           quali_M = as.character(quali_M),
           lado = as.character(lado),
           filhote_ac = filhote_ac == "V",
           id_reid = as.factor(id_reid),
           catalogo_atualizado = catalogo_atualizado == "V",
           lado_novo = lado_novo == "V",
           identificador = as.character(identificador),
           fotografa = as.character(fotografa))
  
  #Junção de todas tabelas em uma lista
  lista <- list(saidas = saidas,
                amostragens = amostragens,
                climas = climas,
                avistagens = avistagens,
                pausas = pausas,
                wps_extra = wps_extra,
                identificacoes = identificacoes
                # , individuos = individuos
  )
  
  return(lista)
}
