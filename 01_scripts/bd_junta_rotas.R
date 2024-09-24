bd_junta_rotas <- function(dados_excel_sub, dados_rotas) {
  
  # Juntando dados das rotas com avistagens
  dados_rotas_processadas <- dados_rotas %>%
    left_join(dados_excel_sub$avistagens %>% select(saida, grupo, datahora_I_avis = datahora_I, datahora_F_avis = datahora_F),
              by = join_by("saida",
                           "datahora_ROTA" >= "datahora_I_avis",
                           "datahora_ROTA" <= "datahora_F_avis")) %>%
    # Juntando dados das rotas com amostragens
    left_join(dados_excel_sub$amostragens %>% select(saida, datahora_I_amos = datahora_I, datahora_F_amos = datahora_F),
              by = join_by("saida",
                           "datahora_ROTA" >= "datahora_I_amos",
                           "datahora_ROTA" <= "datahora_F_amos")) %>%
    # Juntando dados das rotas com saídas
    left_join(dados_excel_sub$saidas %>% select(saida, datahora_I_said = datahora_I, datahora_F_said = datahora_F),
              by = join_by("saida",
                           "datahora_ROTA" >= "datahora_I_said",
                           "datahora_ROTA" <= "datahora_F_said")) %>%
    # Criando coluna 'tipo' baseada nas condições específicas
    mutate(tipo = case_when(!is.na(grupo) ~ paste0("G", grupo),
                            !is.na(datahora_I_amos) & datahora_ROTA >= datahora_I_amos & datahora_ROTA <= datahora_F_amos ~ "A",
                            !is.na(datahora_I_said) & datahora_ROTA >= datahora_I_said & datahora_ROTA <= datahora_F_said ~ "D",
                            TRUE ~ "F"))
  
  # Função para reiniciar os contadores e duplicar pontos de encontro para evitar gaps
  determina_tipo <- function(saida, datahora_ROTA, tipo) {
    contador_a <- 1
    contador_d <- 1
    contador_f <- 1
    sequencia_renomeada <- rep("", length(tipo))
    duplicado <- rep(FALSE, length(tipo))
    
    for (i in seq_along(tipo)) {
      if (tipo[i] == "F") {
        sequencia_renomeada[i] <- paste0(tipo[i], contador_f)
        if (i < length(tipo) && tipo[i + 1] != "F") {
          contador_f <- contador_f + 1
        }
      } else if (tipo[i] == "D") {
        sequencia_renomeada[i] <- paste0(tipo[i], contador_d)
        if (i < length(tipo) && tipo[i + 1] != "D") {
          contador_d <- contador_d + 1
        }
      } else if (tipo[i] == "A") {
        sequencia_renomeada[i] <- paste0(tipo[i], contador_a)
        if (i < length(tipo) && tipo[i + 1] != "A") {
          contador_a <- contador_a + 1
        }
      } else {
        sequencia_renomeada[i] <- tipo[i]
      }
      
      # Duplica o ponto de encontro entre dois tipos diferentes
      if (i < length(tipo) && tipo[i] != tipo[i + 1]) {
        duplicado[i + 1] <- TRUE
      }
    }
    
    # Duplicando os pontos de transição
    df <- tibble(datahora_ROTA, tipo = sequencia_renomeada, duplicado)
    df <- df %>%
      mutate(datahora_ROTA = if_else(duplicado, lag(datahora_ROTA), datahora_ROTA)) %>%
      select(-duplicado)
    
    return(df$tipo)
  }
  
  # Aplicando a função determina_tipo para cada saída
  dados_rotas_processadas <- dados_rotas_processadas %>%
    group_by(saida) %>%
    arrange(datahora_ROTA) %>%
    mutate(tipo = determina_tipo(saida, datahora_ROTA, tipo)) %>%
    ungroup()
  
  # Adicionando os resultados das rotas à lista dados_excel_sub$rotas
  dados_excel_sub$rotas <- 
    dados_rotas_processadas %>%
    select(saida, datahora_ROTA, data_rota, tipo, lng, lat)
  
  # Atualizando $caminhos
  dados_excel_sub$caminhos <- 
    dados_rotas_processadas %>%
    select(saida, arquivo) %>%
    unique() %>%
    nest(id = saida) %>%
    rename(caminho_arquivo = arquivo) %>%
    transmute(tipo = "gps_rota",
              id = id,
              arquivo = basename(caminho_arquivo),
              pasta = dirname(caminho_arquivo)) %>%
    bind_rows(dados_excel_sub$caminhos)
  
  cat("-> OK - junção das rotas\n")
  
  return(dados_excel_sub)
}
