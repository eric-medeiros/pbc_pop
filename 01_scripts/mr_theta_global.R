mr_theta_global <- function(bd) {
  library(dplyr)
  
  # Para cada período, calcula a razão: total de capturas (soma de 'avis')
  # dividida pelo número de indivíduos distintos (ID)
  result <-
    bd$avistagens %>%
    select(saida, grupo, tam_grupo, n_marcados, n_lisos) %>%
    filter(tam_grupo == n_marcados + n_lisos) %>%
    mutate(tam_grupo = as.numeric(tam_grupo)) %>%
    transmute(theta_grupo = n_marcados/tam_grupo) %>%
    summarise(theta_geral = mean(theta_grupo, na.rm = TRUE),
              theta_sd = sd(theta_grupo, na.rm = TRUE),
              theta_n = n()) %>%
    mutate(theta_se = theta_sd / sqrt(theta_n))

  return(result)
}