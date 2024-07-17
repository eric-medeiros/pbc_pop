# Função junta_sondas para combinar dados de sonda com dados de avistagens
bd_junta_sondas <- function(dados_excel_sub, dados_sondas) {
  
  # Juntando dados da sonda com dados de avistagens
  dados_sondas_processadas <- dados_sondas %>%
    left_join(dados_excel_sub$avistagens %>% select(saida, grupo, datahora_I, datahora_F),
              by = join_by("saida", "datahora_SONDA" >= "datahora_I", "datahora_SONDA" <= "datahora_F")) %>%
    group_by(saida, grupo) %>% # Agrupando por 'saida' e 'grupo'
    arrange(datahora_SONDA) %>% # Ordenando pelas datahora_SONDA
    ungroup() %>% 
    select(saida, grupo, datahora_SONDA, Temp, Sal, OD, Turb, pH, Pres, lng, lat, arquivo) # Selecionando as colunas relevantes
  
  # Adicionando os resultados das sondas à lista dados_excel_sub$sonda
  dados_excel_sub$sonda <- dados_sondas_processadas %>% select(-arquivo)
  
  # Atualizando $caminhos
  dados_excel_sub$caminhos <- dados_sondas_processadas %>%
    select(saida, caminho_arquivo = arquivo) %>%
    unique() %>%
    nest(id = saida) %>%
    transmute(tipo = "sonda",
              id = id,
              arquivo = basename(caminho_arquivo),
              pasta = dirname(caminho_arquivo)) %>%
    bind_rows(dados_excel_sub$caminhos)
  
  cat("-> OK - junção das sonda\n")
  
  return(dados_excel_sub)
}