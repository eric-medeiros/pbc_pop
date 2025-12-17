mr_prepara_dados_mr <- function(bd, data_i, data_f) {
  library(tidyverse)
  
  result <- 
    bd$identificacoes %>%
    rename(ID = id) %>%
    filter(between(data, data_i, data_f)) %>%
    group_by(ID) %>%
    mutate(avis = 1L,
           exp_norm = as.numeric(exp) - min(as.numeric(exp)) + 1,
           periodo = exp
           # periodo = as.numeric(saida) - 59
           # periodo = exp_norm
           # periodo = ceiling(exp_norm / 3)
    ) %>%
    group_by(ID, periodo)
  
  return(result)
}