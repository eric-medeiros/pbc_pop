rs_geo_dia <- function(bd, data_i, data_f) {
  library(sf)
  
  pontos_gpkg <-
    bd$avistagens %>%
    filter(data >= data_i,
           data <= data_f) %>%
    group_by(saida) %>%
    mutate(data = as.Date(data)) %>%
    select(-wp_I, -datahora_I, -wp_F, -datahora_F, -lng_F, -lat_F) %>%
    st_as_sf(coords = c("lng_I", "lat_I")) %>%
    st_set_crs(4326)
  
  rotas_gpkg <-
    bd$rotas %>%
    filter(datahora_ROTA >= data_i,
           datahora_ROTA <= data_f,
           tipo != "F1",
           tipo != "F2") %>%
    group_by(saida) %>%
    filter(!str_detect(tipo, pattern = "^D")) %>%
    sfheaders::sf_linestring(x = "lng",
                             y = "lat",
                             linestring_id = "saida",
                             keep = TRUE) %>%
    select(saida, data_rota) %>%
    st_set_crs(4326)
  
  res_geo <- list(pontos_gpkg = pontos_gpkg,
                  rotas_gpkg = rotas_gpkg)
  
  return(res_geo )
}