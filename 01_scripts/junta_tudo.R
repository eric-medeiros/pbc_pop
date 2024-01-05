# juntando coordenadas inseridas manualmente na planilha com pontos dos GPS
junta_tudo <- function(dados_excel, dados_wps, dados_rotas, dados_sondas, dados_fotos) {
  
  wp_sub <- dados_excel$wps_extra %>%
    transmute("saida" = saida,
              "datahora" = datahora_extra,
              "wp" = wp_extra,
              "lng" = lng_extra,
              "lat" = lat_extra) %>%
    bind_rows(dados_wps) %>%
    arrange(datahora)

  # Substituindo onde necessário
  # saidas ----
  dados_excel$saidas <- 
    dados_excel$saidas %>%
    left_join(wp_sub, by = c(saida = "saida", wp_I = "wp")) %>%
    dplyr::select(1:4,
                  "datahora_I" = datahora,
                  "lng_I" = lng,
                  "lat_I" = lat,
                  5:10) %>%
    left_join(wp_sub, by = c(saida = "saida", wp_F = "wp")) %>%
    dplyr::select(1:8,
                  "datahora_F" = datahora,
                  "lng_F" = lng,
                  "lat_F" = lat,
                  9:13) %>%
    mutate(lng_I = round(lng_I, 5),
           lat_I = round(lat_I, 5),
           lng_F = round(lng_F, 5),
           lat_F = round(lat_F, 5))
  
  # amostragens ----
  dados_excel$amostragens <-
    dados_excel$amostragens %>%
    left_join(wp_sub, by = c(saida = "saida", wp_I = "wp")) %>%
    dplyr::select(1:5,
                  "datahora_I" = datahora,
                  "lng_I" = lng,
                  "lat_I" = lat,
                  6:7) %>%
    left_join(wp_sub, by = c(saida = "saida", wp_F = "wp")) %>%
    dplyr::select(1:9,
                  "datahora_F" = datahora,
                  "lng_F" = lng,
                  "lat_F" = lat,
                  10:11) %>%
    mutate(lng_I = round(lng_I, 5),
           lat_I = round(lat_I, 5),
           lng_F = round(lng_F, 5),
           lat_F = round(lat_F, 5))
  
  # climas ----
  dados_excel$climas <-
    dados_excel$climas %>%
    group_by(saida) %>%
    mutate(clima = row_number() %>% as.character()) %>%
    ungroup() %>%
    left_join(wp_sub, by = c(saida = "saida", wp_I = "wp")) %>%
    dplyr::select(1,
                  clima,
                  2:3,
                  "datahora_I" = datahora,
                  "lng_I" = lng,
                  "lat_I" = lat,
                  4:11) %>%
    left_join(wp_sub, by = c(saida = "saida", wp_F = "wp")) %>%
    dplyr::select(1:8,
                  "datahora_F" = datahora,
                  "lng_F" = lng,
                  "lat_F" = lat,
                  9:15) %>%
    mutate(lng_I = round(lng_I, 5),
           lat_I = round(lat_I, 5),
           lng_F = round(lng_F, 5),
           lat_F = round(lat_F, 5))
  
  # avistagens ----
  dados_excel$avistagens <-
    dados_excel$avistagens %>%
    left_join(wp_sub, by = c(saida = "saida", wp_I = "wp")) %>%
    dplyr::select(1:6,
                  "datahora_I" = datahora,
                  "lng_I" = lng,
                  "lat_I" = lat,
                  7:19) %>%
    left_join(wp_sub, by = c(saida = "saida", wp_F = "wp")) %>%
    dplyr::select(1:10,
                  "datahora_F" = datahora,
                  "lng_F" = lng,
                  "lat_F" = lat,
                  11:22) %>%
    mutate(lng_I = round(lng_I, 5),
           lat_I = round(lat_I, 5),
           lng_F = round(lng_F, 5),
           lat_F = round(lat_F, 5))
  
  # pausas ----
  dados_excel$pausas <- 
    dados_excel$pausas %>%
    group_by(saida) %>%
    mutate(pausa = as.character(row_number())) %>%
    ungroup() %>%
    left_join(wp_sub, by = c(saida = "saida", wp_I = "wp")) %>%
    dplyr::select(1,
                  pausa,
                  2:3, 
                  "datahora_I" = datahora,
                  "lng_I" = lng,
                  "lat_I" = lat,
                  4:5) %>%
    left_join(wp_sub, by = c(saida = "saida", wp_F = "wp")) %>%
    dplyr::select(1:8,
                  "datahora_F" = datahora,
                  "lng_F" = lng,
                  "lat_F" = lat,
                  9) %>%
    mutate(lng_I = round(lng_I, 5),
           lat_I = round(lat_I, 5),
           lng_F = round(lng_F, 5),
           lat_F = round(lat_F, 5))
  
  
  # dados_excel com wps ----
  dados_excel$wps_extra <- NULL
  dados_excel$wps <- wp_sub
  cat("-> wps ok\n")
  
  # juntando dados das rotas ----
  dr <- 
    dados_rotas %>%
    left_join(
      dados_excel$avistagens %>% select(saida, grupo, datahora_I_avis = datahora_I, datahora_F_avis = datahora_F),
      by = join_by(
        saida,
        datahora_ROTA >= datahora_I_avis,
        datahora_ROTA <= datahora_F_avis)) %>%
    left_join(
      dados_excel$amostragens %>% group_by(saida) %>% select(saida, datahora_I_amos = datahora_I, datahora_F_amos = datahora_F),
      by = join_by(
        saida,
        datahora_ROTA >= datahora_I_amos,
        datahora_ROTA <= datahora_F_amos)) %>%
    left_join(
      dados_excel$saidas %>% select(saida, datahora_I_said = datahora_I, datahora_F_said = datahora_F),
      by = join_by(
        saida,
        datahora_ROTA >= datahora_I_said,
        datahora_ROTA <= datahora_F_said)) %>%
    mutate(tipo = case_when(
      !is.na(grupo) ~ paste0("G", grupo),
      datahora_ROTA >= datahora_I_amos & datahora_ROTA <= datahora_F_amos ~ "A",
      is.na(datahora_I_amos) & !is.na(datahora_I_said) ~ "D",
      TRUE ~ "F"))
  
  dr <- 
    dr %>%
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
    sequencia_renomeada <-  rep("", nrow(dr))
  
  for (i in seq(nrow(dr))) {
    if (dr$tipo[i] == "F") {
      sequencia_renomeada[i] <- paste0(dr$tipo[i], contador_f)
    } else if (dr$tipo[i] == "D") {    
      sequencia_renomeada[i] <- paste0(dr$tipo[i], contador_d)
    } else if (dr$tipo[i] == "A") {
      sequencia_renomeada[i] <- paste0(dr$tipo[i], contador_a)
    } else {sequencia_renomeada[i] <- dr$tipo[i]}
    
    if (dr$transicoes_a[i] == -1) {contador_a <- contador_a + 1}
    if (dr$transicoes_d[i] == -1) {contador_d <- contador_d + 1}
    if (dr$transicoes_f[i] == -1) {contador_f <- contador_f + 1}
    if (dr$transicoes_s[i] == 1) {contador_a <- 1; contador_d <- 1; contador_f <- 1}
    }
  
    dados_excel$rotas <- 
      dr %>%
      mutate(tipo = sequencia_renomeada) %>%
      select(saida, datahora_ROTA, data_rota, tipo, lng, lat, arquivo)
    
  cat("-> rotas ok\n")

  #juntando dados da sonda
  dados_excel$sonda <- 
    dados_sondas %>%
    mutate(datahora_SONDA = datahora_SONDA) %>%
    left_join(
      dados_excel$avistagens %>% select(saida, grupo, datahora_I, datahora_F),
      by = join_by(
        saida,
        datahora_SONDA >= datahora_I,
        datahora_SONDA <= datahora_F)) %>%
    group_by(saida,grupo) %>%
    arrange(datahora_SONDA) %>%
    ungroup() %>%
    select(saida, grupo, datahora_SONDA, Temp, Sal, OD, Turb, pH, Pres, lng, lat, arquivo)
 
  cat("-> sonda ok\n")
  
  # juntando dados das fotos no histórico
  dados_excel$identificacoes <-
    dados_excel$identificacoes %>%
    left_join(dados_excel$amostragens[c("exp","saida")],
              by = "saida") %>%
    full_join(dados_fotos,
              join_by(exp, grupo, ID, arquivo),
              multiple = "all",
              keep = FALSE) %>%
    filter(!is.na(grupo),
           !is.na(id_reid),
           !is.na(exp)) %>% 
    dplyr::select(saida, exp, grupo, ID, data, datahora, lng, lat, arquivo, quali_F, quali_M, lado, filhote_ac, id_reid,
                  catalogo_atualizado, lado_novo, identificador, fotografa, obs, arquivo_sub, caminho) %>%
    mutate(data = data)
  
  
  cat("-> fotos histórico ok\n")
  
  return(dados_excel)  
}