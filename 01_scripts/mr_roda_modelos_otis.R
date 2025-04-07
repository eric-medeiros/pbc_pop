mr_roda_modelos_otis <- function(dados_ch) {
  
  # Definir diretório de trabalho específico para MARK
  setwd(here::here("03_results", "03_MARK"))  # Tudo acontecerá aqui
  
  # Limpar arquivos antigos do MARK antes de iniciar
  arquivos_antigos <- list.files(
    pattern = "\\.(inp|out|res|vcv|tmp)$", 
    full.names = TRUE
  )
  if (length(arquivos_antigos) > 0) {
    invisible(file.remove(arquivos_antigos))
    cat("Arquivos antigos removidos:\n", paste(arquivos_antigos, collapse = "\n"), "\n")
  }
  
  # Verificar e criar coluna 'group' se necessário
  if (!"group" %in% names(dados_ch)) dados_ch$group <- as.factor("Geral")
  
  # Processar dados para o modelo Closed
  dp <- RMark::process.data(dados_ch, model = "Closed", groups = "group")
  ddl <- RMark::make.design.data(dp)
  
  # Definir modelos
  modelos <- list(
    M0 = list(
      p = list(formula = ~1, link = "logit"),
      c = list(formula = ~1, link = "logit"),
      f0 = list(formula = ~1)
    ),
    Mt = list(
      p = list(formula = ~time, link = "logit"),
      c = list(formula = ~1, link = "logit"),
      f0 = list(formula = ~1)
    ),
    Mc = list(
      p = list(formula = ~1, link = "logit"),
      c = list(formula = ~time, link = "logit"),
      f0 = list(formula = ~1)
    ),
    Mtc = list(
      p = list(formula = ~time, link = "logit"),
      c = list(formula = ~time, link = "logit"),
      f0 = list(formula = ~1)
    )
  )
  
  result <- list()
  # Rodar modelos e interpretar resultados
  for (i in seq_along(modelos)) {
    nome_modelo <- names(modelos)[i]
    mark_result <- RMark::mark(
      dp,
      ddl,
      model.parameters = modelos[[i]],
      model.name = nome_modelo,
      silent = TRUE,
      output = FALSE
    )
    
    result[[i]] <- list(
      model = mark_result$model,
      model_name = nome_modelo,
      AICc = mark_result$results$AICc,
      c_hat = mark_result$results$deviance/mark_result$results$deviance.df,
      n_params = mark_result$results$npar,
      N = mark_result$results$derived$`N Population Size`$estimate,
      N_se = mark_result$results$derived$`N Population Size`$se,
      N_lcl = mark_result$results$derived$`N Population Size`$lcl,
      N_ucl = mark_result$results$derived$`N Population Size`$ucl,
      output_file = paste0(mark_result$output, ".out")
    )
   
  }  
  return(result)
}
