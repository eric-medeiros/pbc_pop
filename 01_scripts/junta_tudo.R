# Função para juntar os dados
junta_tudo <- function(dados_excel, dados_wps, dados_rotas, dados_sondas, dados_fotos) {
  
  # Juntando coordenadas inseridas manualmente na planilha com pontos dos GPS
  
  # Preparando os dados wp_sub
  wp_sub <- dados_excel$wps_extra %>%
    transmute(saida = saida,
              datahora = datahora_extra,
              wp = wp_extra,
              lng = as.double(lng_extra),  # Convertendo para double
              lat = as.double(lat_extra)) %>%
    bind_rows(dados_wps) %>%
    arrange(datahora)
  
  # Função para atualizar coordenadas genérica
  atualiza_coordenadas_generico <- function(dados, wp_sub, tipo) {
    dados[[tipo]] <- dados[[tipo]] %>%
      left_join(wp_sub, by = c("saida", "wp_i" = "wp")) %>%
      rename(wp_I = wp_i) %>%
      dplyr::select(1:wp_I,
                    datahora_I = datahora,
                    lng_I = lng,
                    lat_I = lat,
                    wp_I:last_col()) %>%
      left_join(wp_sub, by = c("saida", "wp_f" = "wp")) %>%
      rename(wp_F = wp_f) %>%
      dplyr::select(1:wp_F,
                    datahora_F = datahora,
                    lng_F = lng,
                    lat_F = lat,
                    wp_I:last_col()) %>%
      mutate(lng_I = round(lng_I, 5),
             lat_I = round(lat_I, 5),
             lng_F = round(lng_F, 5),
             lat_F = round(lat_F, 5))
    
    return(dados)
  }
  
  # Aplicando a função para cada seção em dados_excel
  tipos <- c("saidas", "amostragens", "climas", "avistagens", "pausas")
  for (tipo in tipos) {
    dados_excel <- atualiza_coordenadas_generico(dados_excel, wp_sub, tipo)
  }
  
  # Dados extras de wps
  dados_excel$wps_extra <- NULL
  dados_excel$wps <- wp_sub
  
  cat("-> wps ok\n")
  
  # Juntando dados das rotas
  dr <- dados_rotas %>%
    left_join(dados_excel$avistagens %>% select(saida, grupo, datahora_I_avis = datahora_I, datahora_F_avis = datahora_F),
              by = join_by("saida",
                           "datahora_ROTA" >= "datahora_I_avis",
                           "datahora_ROTA" <= "datahora_F_avis")) %>%
    left_join(dados_excel$amostragens %>% select(saida, datahora_I_amos = datahora_I, datahora_F_amos = datahora_F),
              by = join_by("saida",
                           "datahora_ROTA" >= "datahora_I_amos",
                           "datahora_ROTA" <= "datahora_F_amos")) %>%
    left_join(dados_excel$saidas %>% select(saida, datahora_I_said = datahora_I, datahora_F_said = datahora_F),
              by = join_by("saida",
                           "datahora_ROTA" >= "datahora_I_said",
                           "datahora_ROTA" <= "datahora_F_said")) %>%
    mutate(tipo = case_when(!is.na(grupo) ~ paste0("G", grupo),
                            datahora_ROTA >= datahora_I_amos & datahora_ROTA <= datahora_F_amos ~ "A",
                            is.na(datahora_I_amos) & !is.na(datahora_I_said) ~ "D",
                            TRUE ~ "F"))
  
  dr <- dr %>%
    group_by(saida) %>%
    reframe(
      prim_datahora_I_amos = first(na.omit(datahora_I_amos)),
      ulti_datahora_F_amos = last(na.omit(datahora_F_amos))) %>%
    arrange(as.numeric(saida)) %>%
    right_join(dr, by = "saida") %>%
    mutate(transicoes_s = c(0, diff(as.integer(saida))),
           transicoes_f = c(0, diff(tipo == "F")),
           transicoes_d = c(0, diff(tipo == "D")),
           transicoes_a = c(0, diff(tipo == "A")))
  
  contador_a <- 1
  contador_d <- 1
  contador_f <- 1
  sequencia_renomeada <- rep("", nrow(dr))
  
  for (i in seq(nrow(dr))) {
    if (dr$tipo[i] == "F") {
      sequencia_renomeada[i] <- paste0(dr$tipo[i], contador_f)
    } else if (dr$tipo[i] == "D") {
      sequencia_renomeada[i] <- paste0(dr$tipo[i], contador_d)
    } else if (dr$tipo[i] == "A") {
      sequencia_renomeada[i] <- paste0(dr$tipo[i], contador_a)
    } else {
      sequencia_renomeada[i] <- dr$tipo[i]
    }
    
    if (dr$transicoes_a[i] == -1) {
      contador_a <- contador_a + 1
    }
    if (dr$transicoes_d[i] == -1) {
      contador_d <- contador_d + 1
    }
    if (dr$transicoes_f[i] == -1) {
      contador_f <- contador_f + 1
    }
    if (dr$transicoes_s[i] == 1) {
      contador_a <- 1
      contador_d <- 1
      contador_f <- 1
    }
  }
  
  dados_excel$rotas <- 
    dr %>%
    mutate(tipo = sequencia_renomeada) %>%
    select(saida, datahora_ROTA, data_rota, tipo, lng, lat, arquivo)
  
  cat("-> rotas ok\n")
  
  # Juntando dados da sonda
  dados_excel$sonda <- 
    dados_sondas %>%
    left_join(dados_excel$avistagens %>% select(saida, grupo, datahora_I, datahora_F),
              by = join_by("saida", "datahora_SONDA" >= "datahora_I", "datahora_SONDA" <= "datahora_F")) %>%
    group_by(saida, grupo) %>%
    arrange(datahora_SONDA) %>%
    ungroup() %>%
    select(saida, grupo, datahora_SONDA, Temp, Sal, OD, Turb, pH, Pres, lng, lat, arquivo)
  
  cat("-> sonda ok\n")
  
  # Juntando dados das fotos no histórico
  dados_excel$identificacoes <- 
    dados_excel$identificacoes %>%
    left_join(dados_excel$amostragens %>% select(exp, saida),
              by = "saida") %>%
    full_join(dados_fotos,
              by = join_by("exp", "grupo", "id" == "ID", "arquivo")) %>%
    filter(!is.na(grupo),
           !is.na(id_reid),
           !is.na(exp)) %>%
    mutate(data = as_date(data)) %>%
    select(saida, exp, grupo, ID = id, data, datahora, lng, lat, arquivo, quali_F = quali_f, quali_M = quali_m, lado, filhote_ac, id_reid,
           catalogo_atualizado, lado_novo, identificador, fotografa, obs, arquivo_sub, caminho)
  
  cat("-> fotos histórico ok\n")
  
  return(dados_excel)
}