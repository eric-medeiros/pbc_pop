# Função junta_identificacoes para combinar dados de identificações com dados de amostragens e fotos
bd_junta_fotos <- function(dados_excel_sub, dados_fotos) {
  
  # Juntando dados de identificações com dados de amostragens
  dados_identificacoes_processadas <- dados_excel_sub$identificacoes %>%
    left_join(dados_excel_sub$amostragens %>% select(exp, saida), by = "saida") %>%
    full_join(dados_fotos, by = join_by("exp", "grupo", "id" == "ID", "arquivo")) %>%
    filter(!is.na(grupo), !is.na(id_reid), !is.na(exp)) %>%
    mutate(data = as_date(data)) %>%
    select(saida, exp, grupo, ID = id, data, datahora, lng, lat, arquivo, quali_F = quali_f, quali_M = quali_m, lado, filhote_ac, id_reid,
           catalogo_atualizado, lado_novo, identificador, fotografa, obs, arquivo_sub, caminho)
  
  # Adicionando os resultados das identificações à lista dados_excel_sub$identificacoes
  dados_excel_sub$identificacoes <- dados_identificacoes_processadas %>% select(-caminho)
  
  # Atualizando $caminhos
  dados_excel_sub$caminhos <- dados_identificacoes_processadas %>%
    select(saida, caminho) %>%
    unique() %>%
    nest(id = saida) %>%
    transmute(tipo = "fotos",
              id = id,
              arquivo = basename(caminho),
              pasta = dirname(caminho)) %>%
    bind_rows(dados_excel_sub$caminhos)
  
  cat("-> OK - junção das fotos\n")
  
  return(dados_excel_sub)
}