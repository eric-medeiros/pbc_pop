# Função para inserir dados do raven no banco de dados
junta_foto <- function(lista_dados, dados_fotos){
  library(dplyr)
  library(tidyr)

  # lista_dados <- bd
  # 
  # excel_s_pasta_n <- 
  #   lista_dados$identificacoes %>%
  #   left_join(lista_dados$amostragens[c("exp","saida")], by = "saida") %>%
  #   full_join(dados_fotos,
  #             by = join_by(exp, grupo, ID, arquivo),
  #             keep = TRUE,
  #             suffix = c("_excel", "_jpeg")) %>%
  #   filter (is.na(grupo_jpeg),
  #           !saida %in% as.character(c(67,69,70,71,75,76,78:83))) %>%
  #   arrange(ID_excel) %>%
  #   select(ID_excel, arquivo_excel)
  # 
  # excel_n_pasta_s <- 
  #   lista_dados$identificacoes %>%
  #   left_join(lista_dados$amostragens[c("exp","saida")], by = "saida") %>%
  #   full_join(dados_fotos,
  #             by = join_by(exp, grupo, ID, arquivo),
  #             keep = TRUE,
  #             suffix = c("_excel", "_jpeg")) %>%
  #   filter (is.na(grupo_excel),
  #           !saida %in% as.character(c(67,69,70,71,75,76,78:83))) %>%
  #   arrange(ID_jpeg) %>%
  #   select(ID_jpeg, arquivo_jpeg)
  

  lista_dados$identificacoes <- lista_dados$identificacoes %>%
    left_join(lista_dados$amostragens[c("exp","saida")],
              by = "saida") %>%
    full_join(dados_fotos,
              join_by(exp, grupo, ID, arquivo),
              multiple = "all",
              keep = FALSE) %>%
    filter(!is.na(grupo),
           !is.na(id_reid),
           !is.na(exp)) %>% 
    dplyr::select(saida,
                  exp,
                  grupo,
                  ID,
                  data,
                  datahora,
                  lng,
                  lat,
                  arquivo,
                  quali_F,
                  quali_M,
                  lado,
                  filhote_ac,
                  id_reid,
                  catalogo_atualizado,
                  lado_novo,
                  identificador,
                  fotografa,
                  obs,
                  arquivo_sub,
                  caminho)
  
  invisible(lista_dados)
  
  return(lista_dados)
}

