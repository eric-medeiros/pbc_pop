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

bd <- readRDS(paste0(pasta_proj,"/02_export/bd.rds"))

# Pontos ----
# Pegando os dados para shapefile
dados_pontos <- bd$avistagens %>%
  mutate(tam_grupo = nafill(tam_grupo, fill = 0L),
         tam_min = nafill(tam_min, fill = 0L),
         tam_max = nafill(tam_max, fill = 0L)) %>%
  rowwise() %>%
  mutate(tam_est = round(sum(tam_grupo, mean(c(tam_min, tam_max))),0),
         int_avis = interval(datahora_I, datahora_F)) %>%
  ungroup() %>%
  dplyr::select(1,2,7,8,13,14,25,20:23,26) %>%
  rowid_to_column("n_grupo")

# Craindo uma lista vazia pra receber dados a seguir  
sonda <- list()

# Selecionando os dados da sonda do intervalo
for (i in 1:nrow(dados_pontos)) {
  sonda[[i]] <-  bd$sonda[ymd_hms(bd$sonda$datahora_SONDA) %within% dados_pontos$int_avis[[i]],]
}

# Juntando todos os dados
dados_pontos <- bind_rows(sonda, .id = "n_grupo") %>%
  mutate(n_grupo = as.numeric(n_grupo)) %>%
  group_by(n_grupo) %>%
  summarise(Temp_m = mean(Temp, na.rm = TRUE),
            Sal_m = mean(Sal, na.rm = TRUE),
            OD_m = mean(OD, na.rm = TRUE),
            Turb_m = mean(Turb, na.rm = TRUE),
            pH_m = mean(pH, na.rm = TRUE),
            Pres_m = mean(Pres, na.rm = TRUE),
            num_pts_s = n()) %>%
  right_join(dados_pontos, by = "n_grupo") %>%
  arrange(as.integer(saida)) %>%
  group_by(saida) %>%
  dplyr::select(9:19,2:8)

# Criando um objeto sf
grupos_sf <- st_as_sf(x = dados_pontos,
                      coords = c("lng_I", "lat_I"))

# Atribuindo o EPSG 4326 - geográficas/WGS84/padrão GPS
st_crs(grupos_sf) <- 4326


# Linhas ----
# Lendo dados do resumo para selecionar dados
dados_rotas <- tibble(read.delim("02_export/resumo.txt"))

# Organizando os dados
dados_rotas <- bd$saidas %>%
  group_by(saida = as.integer(saida)) %>%
  dplyr::select(saida, ROTA = rota) %>%
  left_join(dados_rotas, by = "saida") %>%
  ungroup() %>%
  dplyr::select(3:10,2,1)

# Criando o intervalo
dados_rotas <- dados_rotas %>%
  mutate(int_amos = interval(ymd_hms(bd$amostragens$datahora_I),
                             ymd_hms(bd$amostragens$datahora_F)),
         KM = as.integer(str_replace(KM, ",", ".")))

# lista que receberá os dados da rotas a seguir
pontos <- list()

# Selecionando os dados da rota do intervalo
for (i in 1:nrow(dados_rotas)) {
  
  pontos[[i]] <- bd$rotas[ymd_hms(bd$rotas$datahora_ROTA) %within% dados_rotas$int_amos[[i]], c(2,4,5)]
  
}

# Arrumação dos dados
pontos <- drop_na(tibble(saida = abind::abind(pontos, along = 1)[,1],
                         lng = abind::abind(pontos, along = 1)[,2],
                         lat = abind::abind(pontos, along = 1)[,3]))

# Criando um objeto espacial de pontos
pontos_sf <-  st_as_sf(x = pontos,
                       coords = c("lng", "lat"))

# Atribuindo o EPSG 4326 - geográficas/WGS84/padrão GPS
st_crs(pontos_sf) <- 4326

# Abrindo shape da agua e Tranformando para EPSG 4326 - para fazer o crop
arquivo_shp_agua <- paste0(pasta_proj, "/00_data/SHAPES_AUX/Agua.shp")
agua_sf <- st_transform(st_read(arquivo_shp_agua), 4326)

# Criando um objeto espacial de linhas com crop da água
linha_sf <- pontos_sf %>%
  group_by(saida) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING") %>%
  as_tibble() %>%
  left_join(dados_rotas[1:10]%>%
              group_by(saida = as.character(saida)),
            by = "saida") %>%
  dplyr::select(3:11,2) %>%
  st_as_sf()


# Salvando ----

# Atribuindo caminho
novo_dir <- paste0(pasta_proj,"/02_export/shapefiles")

# /fazendo uma pasta nova
dir.create(novo_dir, recursive = TRUE)

# Salvando o objeto grupos_sf como um Shapefile para fazer o mapa no QGIS
st_write(obj = grupos_sf,
         dsn = "02_export/shapefiles",
         layer = "pontos",
         driver = "ESRI Shapefile",
         append = FALSE)


# Salvando o objeto sf como um Shapefile para fazer o mapa no QGIS
st_write(obj = linha_sf,
         dsn = "02_export/shapefiles",
         layer = "rotas",
         driver = "ESRI Shapefile",
         append = FALSE)

