bd_banco <- function(pasta_outputs, update, pasta_data) {
  # Função para atualizar ou ler o banco de dados
  library(readr)
  
  caminho_rds <- file.path(pasta_outputs, "01_BANCO", "bd.rds")
  
  if(update) {
    # Condição para atualizar o banco de dados
    source("01_scripts/bd_refaz_banco.R")
    bd <- bd_refaz_banco(pasta_data)
    if(!dir.exists(dirname(caminho_rds))) { dir.create(dirname(caminho_rds)) }
    write_rds(bd, caminho_rds)
    cat("-> OK - Banco de Dados atualizado\n")
  } else {
    # Condição para leitura do banco de dados
    bd <- read_rds(caminho_rds)
    cat("-> OK - Banco de Dados lido\n")
  }
  return(bd)
}