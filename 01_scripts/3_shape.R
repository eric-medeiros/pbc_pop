### Criando um SHP das avistagens com dados das avistagens e sonda como tabela de atributo

# Limpando o Global Environment 
rm(list = ls())

library(magrittr)
library(dplyr)
library(purrr)
library(sf)
library(tibble)
library(data.table)
library(lubridate)
library(stringr)
library(tidyr)

pasta_proj <- rprojroot::find_rstudio_root_file()

bd <- readRDS(paste0(pasta_proj,"/03_export/bd.rds"))
data_i <- dmy("01/12/2022")
data_f <- dmy("31/07/2023")

# DEPOIS TENTAR POR SONDA
pontos_sf <- bd$avistagens %>%
  filter(datahora_I >= data_i,
         datahora_F <= data_f) %>%
  mutate(tam_grupo = nafill(tam_grupo, fill = 0L),
         tam_min = nafill(tam_min, fill = 0L),
         tam_max = nafill(tam_max, fill = 0L)) %>%
  rowwise() %>%
  mutate(tam_est = round(sum(tam_grupo, mean(c(tam_min, tam_max))),0)) %>%
  ungroup() %>%
  dplyr::select(saida, data, lng_I, lat_I, estado, coesao, tam_est, n_neonatos, n_infantes, n_juvenis, n_adultos, num_fotos, oc_id) %>%
  st_as_sf(coords = c("lng_I", "lat_I"),
           crs = 4326)

linhas_sf <- 
  bd$rotas %>%
  filter(datahora_ROTA >= data_i,
         # amostragem,
         datahora_ROTA <= data_f) %>%
  mutate(saida = as.integer(saida)) %>%
  left_join(bd$amostragens %>% 
              mutate(saida = as.integer(saida)) %>%
              select(saida, rota),
            by = "saida") %>%
  st_as_sf(coords = c("lng", "lat"),
           crs = 4326) %>%
  group_by(saida, data_rota, rota) %>%
  summarise(.groups = "keep",
            datahora_i = first(datahora_ROTA),
            datahora_f = last(datahora_ROTA),
            do_union = FALSE) %>%
  st_cast("LINESTRING") %>%
  mutate(D_amos = st_length(geometry) %>% units::set_units("km"),
         T_amos = as.numeric(datahora_f - datahora_i),
         datahora_i = as.character.Date(datahora_i),
         datahora_f = as.character.Date(datahora_f)) %>%
  select(1,3,2,4,5,7,8,6)

# Abrindo shape da agua e Tranformando para EPSG 4326 - para fazer o crop
# arquivo_shp_agua <- paste0(pasta_proj, "/00_data/SHAPES_AUX/Agua.shp")
# agua_sf <- st_transform(st_read(arquivo_shp_agua), 4326) 

# Salvando ----

# Atribuindo caminho
novo_dir <- paste0(pasta_proj,"/03_export/shapefiles")

# /fazendo uma pasta nova
dir.create(novo_dir, recursive = TRUE)

# Salvando o objeto grupos_sf como um Shapefile para fazer o mapa no QGIS
st_write(obj = pontos_sf,
         dsn = "03_export/shapefiles",
         layer = "pontos.shp",
         driver = "ESRI Shapefile",
         append = FALSE)


# Salvando o objeto sf como um Shapefile para fazer o mapa no QGIS
try(st_write(obj = linhas_sf,
             dsn = "03_export/shapefiles/rotas.shp",
             layer = "rotas",
             driver = "ESRI Shapefile",
             append = FALSE))

