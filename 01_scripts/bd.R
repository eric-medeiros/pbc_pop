bd <- function ( pasta_data ) {
  library(magrittr)
  
  source("01_scripts/sub_WP.R")
  source("01_scripts/le_sonda.R")
  source("01_scripts/junta_sonda.R")
  source("01_scripts/le_rota.R")
  source("01_scripts/junta_rota.R")
  source("01_scripts/le_fotos.R")
  source("01_scripts/junta_foto.R")

  bd <- sub_WP(pasta_data) %>%
    junta_sonda(dados_sonda = le_sonda(pasta_data)) %>%
    junta_rota(dados_rotas = le_rota(pasta_data)) %>%
    junta_foto(dados_fotos = le_fotos(pasta_data))
  
  invisible(bd)
  
  return(bd)
}