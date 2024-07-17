bd_refaz_banco <- function(pasta_data) {
  
  # Carrega os scripts de leitura e junção dos dados
  source("01_scripts/bd_le_planilha.R")
  source("01_scripts/bd_le_wps.R")
  source("01_scripts/bd_le_rotas.R")
  source("01_scripts/bd_le_sondas.R")
  source("01_scripts/bd_le_fotos.R")
  source("01_scripts/bd_sub_wps.R")
  source("01_scripts/bd_junta_rotas.R")
  source("01_scripts/bd_junta_sondas.R")
  source("01_scripts/bd_junta_fotos.R")
  
  # source("01_scripts/junta_tudo.R")
  
  suppressPackageStartupMessages({
    library(readr)
  })
  
  # Registra o tempo inicial geral
  inicio_geral <- Sys.time()
  
  # Lê e processa os dados da planilha
  inicio_excel <- Sys.time()
  dados_excel <- bd_le_planilha(pasta_data)
  tempo_excel <- as.numeric(difftime(Sys.time(), inicio_excel, units = "secs"))
  
  # Lê e processa os dados dos waypoints (wps)
  inicio_wps <- Sys.time()
  dados_wps <- bd_le_wps(pasta_data)
  tempo_wps <- as.numeric(difftime(Sys.time(), inicio_wps, units = "secs"))
  
  # Lê e processa os dados das rotas
  inicio_rotas <- Sys.time()
  dados_rotas <- bd_le_rotas(pasta_data)
  tempo_rotas <- as.numeric(difftime(Sys.time(), inicio_rotas, units = "secs"))
  
  # Lê e processa os dados das sondas
  inicio_sondas <- Sys.time()
  dados_sondas <- bd_le_sondas(pasta_data)
  tempo_sondas <- as.numeric(difftime(Sys.time(), inicio_sondas, units = "secs"))
  
  # Lê e processa os dados das fotos
  inicio_fotos <- Sys.time()
  dados_fotos <- bd_le_fotos(pasta_data)
  tempo_fotos <- as.numeric(difftime(Sys.time(), inicio_fotos, units = "secs"))
  
  # Substitui os wps nas abas apropriadas por lng e lat
  inicio_sub <- Sys.time()
  dados_excel_sub <- bd_sub_wps(dados_excel, dados_wps)
  tempo_sub <- as.numeric(difftime(Sys.time(), inicio_sub, units = "secs"))
  
  # Junção de rotas, nomeando os trecho como amostragem(A), deslocamento(D), avistagem(G) ou fora(F)
  inicio_junta_rotas <- Sys.time()
  dados_excel_rotas <- bd_junta_rotas(dados_excel_sub, dados_rotas)
  tempo_junta_rotas <- as.numeric(difftime(Sys.time(), inicio_junta_rotas, units = "secs"))
  
  # Junção de sondas com dados_excel_rotas
  inicio_junta_sondas <- Sys.time()
  dados_excel_sondas <- bd_junta_sondas(dados_excel_rotas, dados_sondas)
  tempo_junta_sondas <- as.numeric(difftime(Sys.time(), inicio_junta_sondas, units = "secs"))
  
  # Junção de dados das fotos com dados_excel_sub
  inicio_junta_fotos <- Sys.time()
  dados_excel_fotos <- bd_junta_fotos(dados_excel_sondas, dados_fotos)
  tempo_junta_fotos <- as.numeric(difftime(Sys.time(), inicio_junta_fotos, units = "secs"))
  
  # Dados do bando de dados
  bd <- dados_excel_fotos
  
  # Cria lista com tempos de execução de cada etapa
  tempos <- tibble(
    datahora_banco = inicio_geral,
    tempo_excel = tempo_excel,
    tempo_wps = tempo_wps,
    tempo_rotas = tempo_rotas,
    tempo_sonda = tempo_sondas,
    tempo_fotos = tempo_fotos,
    tempo_junta_rotas = tempo_junta_rotas,
    tempo_junta_sondas = tempo_junta_sondas,
    tempo_junta_fotos = tempo_junta_fotos,
    tempo_geral = as.numeric(difftime(Sys.time(), inicio_geral, units = "secs"))
  ) %>%
    mutate(across(2:ncol(.), ~ round(.x, 3)))
  
  # Banco completo com tempos
  bd$tempos <- tempos
  
  return(bd)
}