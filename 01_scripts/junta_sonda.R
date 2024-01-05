# Função para inserir sonda no banco de dados
junta_sonda <- function (lista, dados_sonda) {
  library(dplyr)
  
  # Juntando a sonda toda 
  lista$sonda <- 
    lista$avistagens %>%
    select(saida,grupo,datahora_I, datahora_F) %>%
    right_join(dados_sonda,
               by = join_by(saida,
                            datahora_I <= datahora_SONDA,
                            datahora_F >= datahora_SONDA)) %>%
    group_by(saida,grupo) %>%
    arrange(datahora_SONDA) %>%
    ungroup() %>%
    select(saida, grupo, datahora_SONDA, Temp, Sal, OD, Turb, pH, Pres, lng, lat)
  
  invisible(lista)
  
  return(lista)
}
