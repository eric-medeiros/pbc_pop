rs_resumo <- function(pasta_outputs, data_i, data_f, pasta_data, bd) {
  # Função para atualizar ou ler o banco de dados
  source("01_scripts/rs_refaz_resumo.R")
  library(xlsx)
  library(sf)

  arquivo_xls <- paste0("resumo_", data_i, "_", data_f, ".xls")
  caminho_xls <- file.path(pasta_outputs, "02_RESUMO", arquivo_xls)
  arquivo_gpkg <- paste0("resumo_", data_i, "_", data_f, ".gpkg")
  caminho_gpkg <- file.path(pasta_outputs, "02_RESUMO", arquivo_gpkg)

  if(!dir.exists(dirname(caminho_xls))) { dir.create(dirname(caminho_xls)) }
  
  res <- rs_refaz_resumo(pasta_data, data_i, data_f, bd)
  
  write.xlsx(res$resumo_especifico, caminho_xls, sheetName = "POR DIA")
  write.xlsx(res$resumo_geral, caminho_xls, sheetName = "POR PERIODO")
  st_write(res$resumo_geopackage$rotas_gpkg, dsn = caminho_gpkg, layer = "rotas", append = FALSE, quiet = TRUE)
  st_write(res$resumo_geopackage$pontos_gpkg, dsn = caminho_gpkg, layer = "pontos", append = FALSE, quiet = TRUE)
  
  cat("-> OK - Resumo ", as.character.Date(data_i), " - ", as.character.Date(data_f)," atualizado\n")
  
  return(res)
}