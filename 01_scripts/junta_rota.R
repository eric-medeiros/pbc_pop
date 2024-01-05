# Função para inserir rota no banco de dados
junta_rota <- function (lista, dados_rotas) {
  library(dplyr)
  
  # Juntando a rota toda  
  lista$rotas <- 
    dados_rotas %>%
    left_join(lista$avistagens %>%
                group_by(saida,grupo) %>%
                select(saida,grupo,datahora_I_avis = datahora_I, datahora_F_avis = datahora_F),
              by = join_by(saida,
                           datahora_ROTA >= datahora_I_avis,
                           datahora_ROTA <= datahora_F_avis)) %>%
    left_join(lista$amostragens %>%
                group_by(saida) %>%
                select(saida,datahora_I_amos = datahora_I, datahora_F_amos = datahora_F),
              by = join_by(saida,
                           datahora_ROTA >= datahora_I_amos,
                           datahora_ROTA <= datahora_F_amos)) %>%
    mutate(amostragem = !is.na(datahora_I_amos)) %>%
    select(-datahora_I_amos, -datahora_F_amos, -datahora_I_avis, -datahora_F_avis) %>%
    arrange(datahora_ROTA) %>%
    select(saida, grupo, amostragem, datahora_ROTA, lng, lat, dist_p_prox, tempo_p_prox, data_rota, arquivo, geometry)
  
  invisible(lista)
  
  return(lista)
}