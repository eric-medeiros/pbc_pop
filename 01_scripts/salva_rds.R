# Puxa todas as funções de leitura e o de junção dos dados. Em *.rds, cria um banco de dados e um arquivos de controle de tempo gasto em cada etapa

salva_rds <- function(pasta_data = "//nas_ipec/PBC-Pesquisa/PROJETOS/ANDAMENTO/01_FOTOID"){
  
  source("01_scripts/le_planilha.R")
  source("01_scripts/le_wps.R")
  source("01_scripts/le_rotas.R")
  source("01_scripts/le_sondas.R")
  source("01_scripts/le_fotos.R")
  source("01_scripts/junta_tudo.R")
  
  library(readr)
  
  inicio_geral <- Sys.time()
  
  inicio_excel <- Sys.time()
  dados_excel <- le_planilha(pasta_data)
  tempo_excel <- (Sys.time() - inicio_excel) %>% as.numeric %>% round(3)
  
  inicio_wps <- Sys.time()
  dados_wps <- le_wps(pasta_data)
  tempo_wps <- (Sys.time() - inicio_wps) %>% as.numeric %>% round(3)
  
  inicio_rotas <- Sys.time()
  dados_rotas <- le_rotas(pasta_data)
  tempo_rotas <- (Sys.time() - inicio_rotas) %>% as.numeric %>% round(3)
  
  inicio_sonda <- Sys.time()
  dados_sondas <- le_sondas(pasta_data)
  tempo_sonda <- (Sys.time() - inicio_sonda) %>% as.numeric %>% round(3)
  
  inicio_fotos <- Sys.time()
  dados_fotos <- le_fotos(pasta_data)
  tempo_fotos <- (Sys.time() - inicio_fotos) %>% as.numeric %>% round(3)
  
  inicio_junta <- Sys.time()
  dados_banco <- junta_tudo(dados_excel = dados_excel,
                            dados_wps = dados_wps,
                            dados_rotas = dados_rotas,
                            dados_sondas = dados_sondas,
                            dados_fotos = dados_fotos)
  tempo_junta <- (Sys.time() - inicio_junta) %>% as.numeric %>% round(3)
  
  tempo_geral <- (Sys.time() - inicio_geral) %>% as.numeric %>% round(3)
  
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
  
  pasta_banco <- paste0(pasta_data, "/02_ANALISE/04_BANCO_DE_DADOS")
  arquivo_banco <- paste0(pasta_banco, "/banco_fotoID.rds")
  arquivo_tempo <- paste0(pasta_banco, "/tempo_fotoID.rds")
  
  write_rds(x = dados_banco,
            file = arquivo_banco)
  
  write_rds(x = tempos,
            file = arquivo_tempo)
}