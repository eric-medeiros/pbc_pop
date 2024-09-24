rs_leaflet_dia <- function(bd, data_i, data_f) {
  library(leaflet)
  library(lubridate)
  library(stringr)
  library(dplyr)
  library(sf)
  library(tidyr)

  # Função para transformar e pivotar os dados - EVENTUALMENTE INCLUIR PAUSAS!!
  transformar_dados <- function(dataframe, tipo_dado) {
    dataframe %>%
      pivot_longer(cols = starts_with("wp"), names_to = "wp_type", values_to = "wp") %>%
      rowwise() %>%
      mutate(
        tipo = case_when(
          wp_type == "wp_I" & tipo_dado == "saida" ~ "abertura de saida",
          wp_type == "wp_F" & tipo_dado == "saida" ~ "fechamento de saida",
          wp_type == "wp_I" & tipo_dado == "amostragem" ~ "abertura de amostragem",
          wp_type == "wp_F" & tipo_dado == "amostragem" ~ "fechamento de amostragem",
          wp_type == "wp_I" & tipo_dado == "avistagem" ~ "abertura de grupo",
          wp_type == "wp_F" & tipo_dado == "avistagem" ~ "fechamento de grupo"),
        datahora = ifelse(wp_type == "wp_I", as.character(datahora_I), as.character(datahora_F)) %>% ymd_hms(),
        data = as.Date(data),
        lng = ifelse(wp_type == "wp_I", lng_I, lng_F),
        lat = ifelse(wp_type == "wp_I", lat_I, lat_F)
      ) %>%
      select(-wp_type, -datahora_I, -datahora_F, -lng_I, -lat_I, -lng_F, -lat_F) %>%
      nest(dados = everything(), .by = c(saida, data, wp, tipo, datahora, lng, lat))
  }
  
  # Aplicar a transformação em bd$saidas
  saida_transformada <- bd$saidas %>%
    filter(datahora_I >= data_i, datahora_F <= data_f) %>%
    transformar_dados(tipo_dado = "saida")
  
  # Aplicar a mesma transformação em bd$amostragens
  amostragem_transformada <- bd$amostragens %>%
    filter(datahora_I >= data_i, datahora_F <= data_f) %>%
    transformar_dados(tipo_dado = "amostragem")
  
  # Aplicar a mesma transformação em bd$avistagens
  avistagem_transformada <- bd$avistagens %>%
    filter(datahora_I >= data_i, datahora_F <= data_f) %>%
    transformar_dados(tipo_dado = "avistagem")
  
  pontos <- 
    bind_rows(saida_transformada, amostragem_transformada, avistagem_transformada) %>%
    arrange(datahora)
  
  rotas <- 
    bd$rotas %>%
    filter(
      datahora_ROTA >= data_i,
      datahora_ROTA <= data_f,
      tipo != "F1",
      tipo != "F2") %>%
    mutate(key = str_c(saida,tipo)) %>%
    group_by(key) %>%
    sfheaders::sf_linestring(
      x = "lng",
      y = "lat",
      linestring_id = "key",
      keep = TRUE) %>%
    arrange(datahora_ROTA)
  
  # Adicionar uma nova coluna 'linha' para rotas
  rotas$linha <- rep(as.integer(NA), nrow(rotas))
  lag_tipo <- lag(rotas$tipo)
  
  for (i in seq(nrow(rotas))) {
    if (is.na(lag_tipo[i])) {
      rotas$linha[i] <- 1
    } else if (rotas$tipo[i] == lag_tipo[i]) {
      rotas$linha[i] <- rotas$linha[i - 1]
    } else if (rotas$tipo[i] != lag_tipo[i]) {
      rotas$linha[i] <- rotas$linha[i - 1] + 1
    }
  }
  
  rotas_ref <-
    st_read(file.path("00_data", "rotas_de_referencia.gpkg"), layer = "tracks") %>%
    st_combine() %>%
    st_cast("LINESTRING") %>%
    st_zm()
 
  # Inicializa o mapa
  m <- 
  leaflet() %>%
    addProviderTiles('CartoDB.VoyagerNoLabels', group = "Suave") %>%
    addProviderTiles('Esri.WorldImagery', group = "Satelite") %>%
    setView(-47.899226, -24.979645, zoom = 12) %>%
    addPolylines(
      data = rotas_ref,
      group = "Refs",
      color = "green",
      weight = 1,
      opacity = 1)
  
  # Função para determinar a cor com base no tipo
  get_color <- function(tipo) {
    if (str_detect(tipo, "D"))  return("black")
    else if (str_detect(tipo, "G")) return("orange")
    else if (str_detect(tipo, "A")) return("blue")
    else return("white")
  }
  
  # Defina os ícones específicos
  icons <- list(
    "abertura de saida" = makeIcon(
      iconUrl = file.path("00_data", "simbolos_mapa", "abertura_saida.png"),
      iconWidth = 28.5,
      iconHeight = 30.5),
    "fechamento de saida" = makeIcon(
      iconUrl = file.path("00_data", "simbolos_mapa", "fechamento_saida.png"),
      iconWidth = 28.5,
      iconHeight = 30.5),
    "abertura de amostragem" = makeIcon(
      iconUrl = file.path("00_data", "simbolos_mapa", "abertura_amostragem.png"),
      iconWidth = 26.9,
      iconHeight = 20),
    "fechamento de amostragem" = makeIcon(
      iconUrl = file.path("00_data", "simbolos_mapa", "fechamento_amostragem.png"),
      iconWidth = 26.9,
      iconHeight = 20),
    "abertura de grupo" = makeIcon(
      iconUrl = file.path("00_data", "simbolos_mapa", "abertura_grupo.png"),
      iconWidth = 40,
      iconHeight = 19.9),
    "fechamento de grupo" = makeIcon(
      iconUrl = file.path("00_data", "simbolos_mapa", "fechamento_grupo.png"),
      iconWidth = 40,
      iconHeight = 19.9)
  )
  
  
  # Loop para adicionar as polilinhas no mapa
  for (s in unique(rotas$saida)) {
    rota_dia <- rotas %>% filter(saida == s) %>% arrange(data_rota)
    
    for (l in unique(rota_dia$linha)) {
      rota_linha <- rota_dia %>% filter(linha == l)
      
      # Verifica se existem pelo menos dois pontos para formar uma polilinha
      if (nrow(st_coordinates(rota_linha)) > 1) {
        
        rota <- st_combine(st_geometry(rota_linha)) %>% st_cast("LINESTRING") %>% st_set_crs(4326)
        color <- get_color(first(rota_linha$tipo))
        
        # Adiciona a polilinha ao mapa
        m <-
          m %>%
          addPolylines(
            data = rota,
            group = paste(s, "-", first(rota_linha$data_rota)),
            color = color,
            weight = 3,
            opacity = 0.5)
      }
    }
  }
  
  # Filtra pontos com valores não nulos de longitude e latitude
  pontos <- pontos %>% filter(!is.na(lng) & !is.na(lat))
  
  # Adiciona os pontos ao mapa no mesmo grupo das linhas
  for (i in seq(nrow(pontos))) {
    ponto <- pontos[i, ]
    icon <- icons[[ponto$tipo]]
    
    m <- m %>% addMarkers(
      lng = ponto$lng,
      lat = ponto$lat,
      icon = icon,
      popup = paste(
        "WP:", ponto$wp, "<br>",
        "Tipo:", ponto$tipo, "<br>",
        "Datahora:", ponto$datahora, "<br>",
        "Lng:", ponto$lng, "<br>",
        "Lat:", ponto$lat
      ),
      group = paste(ponto$saida, "-", ponto$data)
    )
  }
  
  # Adiciona a legenda de cores para as polilinhas no grupo "leg_rotas"
  m <- m %>%
    addLegend(
      position = "bottomright",
      colors = c("black", "orange", "blue", "green"),
      labels = c("Deslocamento", "Amostragem com boto", "Amostragem sem boto", "Rotas de ref."),
      title = "Tipos de Eventos",
      opacity = 0.5,   # Sincroniza opacidade com as linhas
      group = "Refs"  # Define grupo para controlar via LayerControl
    )
  
  # Adiciona controle de camadas com legendas separadas
  overlay_groups <- c(unique(paste(rotas$saida, "-", rotas$data_rota)), "Refs")

  # Atualiza o LayerControl para incluir as legendas nos grupos
  m <- m %>%
    addLayersControl(
      baseGroups = c("Suave", "Satelite"),
      overlayGroups = overlay_groups,
      options = layersControlOptions(collapsed = FALSE)
    )
  
  # Oculta todos os grupos inicialmente, incluindo as legendas
  for (group in overlay_groups) {
    m <- m %>% hideGroup(group)
  }
  
  return(m)
}