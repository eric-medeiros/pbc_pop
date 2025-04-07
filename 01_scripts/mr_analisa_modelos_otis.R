mr_analisa_modelos_otis <- function(lista_resultados_modelos) {
  library(purrr)
  library(tibble)
  library(dplyr)
  
  # Tabela comparativa com AIC, DeltaAICc e peso
  result <- 
    tibble(
      Modelo_Tipo = map_chr(lista_resultados_modelos, ~ .x$model),
      Modelo_Nome = map_chr(lista_resultados_modelos, ~ .x$model_name),
      AICc = map_dbl(lista_resultados_modelos, ~ .x$AICc),
      n_params = map_dbl(lista_resultados_modelos, ~ ifelse(is.null(.x$n_params), NA_real_, .x$n_params)),
      c_hat = map_dbl(lista_resultados_modelos, ~ .x$c_hat),
      N = map_dbl(lista_resultados_modelos, ~ .x$N),
      N_se = map_dbl(lista_resultados_modelos, ~ .x$N_se),
      N_lcl = map_dbl(lista_resultados_modelos, ~ .x$N_lcl),
      N_ucl = map_dbl(lista_resultados_modelos, ~ .x$N_ucl),
      output_file = map_chr(lista_resultados_modelos, ~ .x$output_file)
    ) %>%
    mutate(
      DeltaAICc = AICc - min(AICc, na.rm = TRUE),
      weight    = exp(-0.5 * DeltaAICc) / sum(exp(-0.5 * DeltaAICc))
    ) %>%
    arrange(AICc)
  
  return(result)
}
