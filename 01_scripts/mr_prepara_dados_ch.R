mr_prepara_dados_ch <- function(bd) {
  library(tidyverse)
  
  result <- 
  dados_mr %>%
    summarise(avis = as.integer(sum(avis) >= 1), .groups = "drop") %>%
    arrange(as.numeric(periodo)) %>%
    pivot_wider(names_from = periodo,
                values_from = avis,
                values_fill = 0) %>%
    unite("ch", 2:tail(names(.), 1), sep = "") %>%
    dplyr::select(ch, ID)
  
  return(result)
}