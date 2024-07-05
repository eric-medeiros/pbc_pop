# Função para ler dados de várias fontes, processá-los e salvar em arquivos .rds
salva_rds <- function(pasta_data = "//nas_ipec/PBC-Pesquisa/PROJETOS/ANDAMENTO/01_FOTOID") {
  
  # Carrega os scripts de leitura e junção dos dados
  source("01_scripts/le_planilha.R")
  source("01_scripts/le_wps.R")
  source("01_scripts/le_rotas.R")
  source("01_scripts/le_sondas.R")
  source("01_scripts/le_fotos.R")
  source("01_scripts/junta_tudo.R")
  
  suppressPackageStartupMessages({
    library(readr)
  })
  
  # Registra o tempo inicial geral
  inicio_geral <- Sys.time()
  
  # Lê e processa os dados da planilha
  inicio_excel <- Sys.time()
  dados_excel <- le_planilha(pasta_data)
  tempo_excel <- (Sys.time() - inicio_excel) %>% as.numeric %>% round(3)
  
  # Lê e processa os dados dos waypoints (wps)
  inicio_wps <- Sys.time()
  dados_wps <- le_wps(pasta_data)
  tempo_wps <- (Sys.time() - inicio_wps) %>% as.numeric %>% round(3)
  
  # Lê e processa os dados das rotas
  inicio_rotas <- Sys.time()
  dados_rotas <- le_rotas(pasta_data)
  tempo_rotas <- (Sys.time() - inicio_rotas) %>% as.numeric %>% round(3)
  
  # Lê e processa os dados das sondas
  inicio_sonda <- Sys.time()
  dados_sondas <- le_sondas(pasta_data)
  tempo_sonda <- (Sys.time() - inicio_sonda) %>% as.numeric %>% round(3)
  
  # Lê e processa os dados das fotos
  inicio_fotos <- Sys.time()
  dados_fotos <- le_fotos(pasta_data)
  tempo_fotos <- (Sys.time() - inicio_fotos) %>% as.numeric %>% round(3)
  
  # Junta todos os dados processados
  inicio_junta <- Sys.time()
  dados_banco <- junta_tudo(
    dados_excel = dados_excel,
    dados_wps = dados_wps,
    dados_rotas = dados_rotas,
    dados_sondas = dados_sondas,
    dados_fotos = dados_fotos
  )
  tempo_junta <- (Sys.time() - inicio_junta) %>% as.numeric %>% round(3)
  
  # Calcula o tempo total decorrido
  tempo_geral <- (Sys.time() - inicio_geral) %>% as.numeric %>% round(3)
  
  # Cria lista com tempos de execução de cada etapa
  tempos <- list(
    datahora_banco = inicio_geral,
    tempo_excel = tempo_excel,
    tempo_wps = tempo_wps,
    tempo_rotas = tempo_rotas,
    tempo_sonda = tempo_sonda,
    tempo_fotos = tempo_fotos,
    tempo_junta = tempo_junta,
    tempo_geral = tempo_geral
  )
  
  # Define pasta e arquivos para salvar banco de dados e tempos
  pasta_banco <- file.path(pasta_data, "02_ANALISE", "04_BANCO_DE_DADOS")
  arquivo_banco <- file.path(pasta_banco, "banco_fotoID.rds")
  arquivo_tempo <- file.path(pasta_banco, "tempo_fotoID.rds")
  
  # Salva banco de dados em arquivo .rds
  write_rds(x = dados_banco, file = arquivo_banco)
  
  # Salva tempos de execução em arquivo .rds
  write_rds(x = tempos, file = arquivo_tempo)
}
