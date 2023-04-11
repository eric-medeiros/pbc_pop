# Função para inserir dados do raven no banco de dados
junta_foto <- function(lista_dados, dados_fotos){
  library(dplyr)
  library(tidyr)
  

  lista_dados$identificacoes <- lista_dados$identificacoes %>%
    left_join(lista_dados$amostragens[c("exp","saida")], by = "saida") %>%
    full_join(dados_fotos, by = c("exp", "grupo", "ID", "arquivo"), multiple = "all") %>%
    dplyr::select("saida",
                  "exp",
                  "grupo",
                  "ID",
                  "data",
                  "datahora",
                  "lng",
                  "lat",
                  "arquivo",
                  "quali_F",
                  "quali_M",
                  "lado",
                  "filhote_ac",
                  "seq",
                  "OBS",
                  "arquivo_sub",
                  "caminho")
  
  invisible(lista_dados)
  
  return(lista_dados)
}

