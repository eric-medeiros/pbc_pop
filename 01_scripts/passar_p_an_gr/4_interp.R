# Mapas de Calor -  Kernel

# Limpando o Global Environment 
rm(list = ls())

library(sf)
library(raster)

library(spatstat)
library(sp)
library(data.table)
library(dplyr)

library(spatialEco)
library(rgdal)
library(maptools)


# Achando a pasta do projeto
pasta_proj <- rprojroot::find_rstudio_root_file()

# Abrindo shape da agua (tem que ter feito o "2_Gerando_SHP.R")
agua_sf <- st_read(paste0(pasta_proj, "/00_data/SHAPES_AUX/Agua.shp"))

# tranformando o para EPSG 32723 - projetadas/WGS 84/UTM 23S
agua_proj <- st_transform(agua_sf, 32723)

# criando raster da agua transformada para projetada
agua_ras <- raster(agua_proj)

# definindo resolução de 250m
res(agua_ras) <- 250

# criando novo raster com o raster acima como valor - estranho, mas esse é plotável
agua_ras <- rasterize(agua_proj, agua_ras)

# Abrindo shape dos grupos, tem que ter rodado o 2_Gerando_SHP.R"
grupos_sf <- st_read("4_export/3_shape/PONTOS/L1_grupos.shp")

# tranformando para EPSG 32723 - projetada/WGS 84/UTM zona23S - entre 48W e 42W
grupos_proj <- st_transform(grupos_sf, 32723)

# Transformando de classe "sf" para classe "ppp" com janela de agua - pacote spatstat
grupos_ppp <- as.ppp(grupos_proj)

# Atribuindo como marks
marks(grupos_ppp) <- as.data.frame(grupos_sf)[,-which(names(grupos_sf) == "geometry")]

# Atribuindo o Window da agua com resolução de 250m
Window(grupos_ppp) <- as.owin(as.im(agua1_ras))

# Atribuindo a unidade de metros, já que é UTM  
unitname(grupos_ppp) <- c("metro", "metros")

# Mudando unidade para km para que o kernel seja por km²
grupos_ppp <- rescale.ppp(grupos_ppp, 1000, "km")

# Removendo pontos duplicados
grupos_ppp <- unique.ppp(grupos_ppp)

# Kernel do pacote spatstat
kernel_L1 <- (density.ppp(grupos_ppp,
                          kernel = "gaussian",
                          sigma = bw.ppl(grupos_ppp),
                          edge = TRUE,
                          diggle = TRUE,
                          positive = TRUE,
                          weights = grupos_ppp$marks$tam_est))

# Voltando para m nos eixos X e Y, mas o Z continua como km²
kernel_L1 <- rescale.im(kernel_L1, .001, c("metro", "metros"))

# Rotulando o CRS no raster do kernel como WGS 84 projetada - pacote raster
rast_proj <- raster(kernel_L1)

crs(rast_proj) <- CRS("+init=epsg:32723")

# Fazendo o volume para cada p
p50_1 <- rasterToPolygons(raster.vol(rast_proj, p=0.50), dissolve = TRUE)[2,]
p95_1 <- rasterToPolygons(raster.vol(rast_proj, p=0.95), dissolve = TRUE)[2,]

# Definir o nome da pasta nova com o padrão de sempre
novo_dir <- paste0(pasta_proj,"/4_export/4_interp/LINHA_1")

# Criar a pasta propriamente dito
dir.create(novo_dir, recursive = TRUE)

# Salvando os raster do kernel
writeRaster(rast_proj,
            filename = "4_export/4_interp/LINHA_1/L1_kernel.tif",
            format = "GTiff",
            overwrite = TRUE)

# salvando os shapefiles de 0.50
writeOGR(obj = p50_1,
         dsn = "4_export/4_interp/LINHA_1",
         layer = "L1_p50",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# salvando o shapefile de 0.95
writeOGR(obj = p95_1,
         dsn = "4_export/4_interp/LINHA_1",
         layer = "L1_p95",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)


### Linha 2 (tem que ter rodados o kernel da linha 1) ----

# Achando a pasta do projeto
pasta_proj <- rprojroot::find_rstudio_root_file()

# Abrindo shape da agua (tem que ter feito o "2_Gerando_SHP.R")
agua2_sf <- st_read(paste0(pasta_proj,"/1_data/SHAPES_AUX/Agua_L2.shp"))

# tranformando o para EPSG 32723 - projetadas/WGS 84/UTM 23S
agua2_proj <- st_transform(agua2_sf, 32723)

# criando raster da agua transformada para projetada
agua2_ras <- raster(agua2_proj)

# definindo resolução de 250m
res(agua2_ras) <- 250


# Caminho do arquivo recém salvo pela linha 1
caminho_raster <- paste0(pasta_proj, "/4_export/4_interp/LINHA_1/L1_kernel.tif")

# Lendo o arquivo recém salvo pela linha 1
rast1_proj <- raster(caminho_raster)

# Cortando a imagem do kernel, com o contorno de agua da linha 2
rast2_proj <- crop(rast1_proj, agua2_ras)

# Fazendo o volume para cada p
p50_2 <- rasterToPolygons(raster.vol(rast2_proj, p=0.50), dissolve = TRUE)[2,]
p95_2 <- rasterToPolygons(raster.vol(rast2_proj, p=0.95), dissolve = TRUE)[2,]


# Definir o nome da pasta nova com o padrão de sempre
novo_dir <- paste0(pasta_proj,"/4_export/4_interp/LINHA_2")

# Criar a pasta propriamente dito
dir.create(novo_dir, recursive = TRUE)

# Salvando os raster do kernel
writeRaster(rast2_proj,
            filename = "4_export/4_interp/LINHA_2/L2_kernel.tif",
            format = "GTiff",
            overwrite = TRUE)

# salvando os shapefiles de 0.50
writeOGR(obj = p50_2,
         dsn = "4_export/4_interp/LINHA_2",
         layer = "L2_p50",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

# salvando o shapefile de 0.95
writeOGR(obj = p95_2,
         dsn = "4_export/4_interp/LINHA_2",
         layer = "L2_p95",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)


### Linha 3 ----

# Achando a pasta do projeto
pasta_proj <- rprojroot::find_rstudio_root_file()

# Abrindo shape da agua (tem que ter feito o "2_Gerando_SHP.R")
agua_sf_geral <- st_read(paste0(pasta_proj, "/1_data/SHAPES_AUX/Agua_L3.shp"))
agua_sf_noite <- st_read(paste0(pasta_proj, "/1_data/SHAPES_AUX/Agua_L3_noite.shp"))

# tranformando para EPSG 32723 - projetada/WGS 84/UTM zona23S - entre 48W e 42W
agua_proj_geral <- st_transform(agua_sf_geral, 32723)
agua_proj_noite <- st_transform(agua_sf_noite, 32723)

# criando raster da agua transformada para projetada
agua_ras_geral <- raster(agua_proj_geral)
agua_ras_noite <- raster(agua_proj_noite)

# definindo resolução de 250m
res(agua_ras_geral) <- 250
res(agua_ras_noite) <- 250

# criando novo raster com o raster acima como valor - estranho, mas esse é plotável
agua_ras_geral <- rasterize(agua_proj_geral, agua_ras_geral)
agua_ras_noite <- rasterize(agua_proj_noite, agua_ras_noite)

# Abrindo shape dos grupos, tem que ter rodado o 2_Gerando_SHP.R"
estacoes_sf <- st_read("4_export/3_shape/PONTOS/L3_estacoes.shp")

# GERAL
est_tot_sf <- 
estacoes_sf %>%
  filter(estacao != 13,
         periodo == "D") %>%
  mutate(lng = st_coordinates(geometry)[,"X"],
         lat = st_coordinates(geometry)[,"Y"]) %>%
  group_by(saida, estacao) %>%
  summarise(.groups = "keep", ASS_P_H_M = (sum(num_ass, na.rm = TRUE)/sum(duracao_s/3600, na.rm = TRUE))) %>%
  mutate(ASS_P_H_M = ifelse(is.na(ASS_P_H_M), 0, ASS_P_H_M))

# Noite/Dia
est_dia_sf <- 
  estacoes_sf %>%
  filter(estacao %in% c("1", "2", "3", "10", "11", "12", "14"),
         periodo == "D") %>%
  mutate(lng = st_coordinates(geometry)[,"X"],
         lat = st_coordinates(geometry)[,"Y"]) %>%
  group_by(saida, estacao) %>%
  summarise(.groups = "keep", ASS_P_H_M = (sum(num_ass, na.rm = TRUE)/sum(duracao_s/3600, na.rm = TRUE))) %>%
  mutate(ASS_P_H_M = ifelse(is.na(ASS_P_H_M), 0, ASS_P_H_M))

est_noi_sf <- 
  estacoes_sf %>%
  filter(estacao %in% c("1", "2", "3", "10", "11", "12", "14"),
         periodo == "N") %>%
  mutate(lng = st_coordinates(geometry)[,"X"],
         lat = st_coordinates(geometry)[,"Y"]) %>%
  group_by(saida, estacao) %>%
  summarise(.groups = "keep", ASS_P_H_M = (sum(num_ass, na.rm = TRUE)/sum(duracao_s/3600, na.rm = TRUE))) %>%
  mutate(ASS_P_H_M = ifelse(is.na(ASS_P_H_M), 0, ASS_P_H_M))

# tranformando para EPSG 32723 - projetada/WGS 84/UTM zona23S - entre 48W e 42W
estacoes_tot_proj <- st_transform(est_tot_sf, 32723)
estacoes_dia_proj <- st_transform(est_dia_sf, 32723)
estacoes_noi_proj <- st_transform(est_noi_sf, 32723)

# Transformando de classe "sf" para classe "ppp" com janela de agua - pacote spatstat
estacoes_tot_ppp <- as.ppp(estacoes_tot_proj[3])
estacoes_dia_ppp <- as.ppp(estacoes_dia_proj[3])
estacoes_noi_ppp <- as.ppp(estacoes_noi_proj[3])

# Atribuindo o Window
Window(estacoes_tot_ppp) <- as.owin(as.im(agua_ras_geral))
Window(estacoes_dia_ppp) <- as.owin(as.im(agua_ras_geral))
Window(estacoes_noi_ppp) <- as.owin(as.im(agua_ras_geral))

# Atribuindo a unidade de metros, já que é UTM  
unitname(estacoes_tot_ppp) <- c("metro", "metros")
unitname(estacoes_dia_ppp) <- c("metro", "metros")
unitname(estacoes_noi_ppp) <- c("metro", "metros")

# Mudando unidade para km para que o kernel seja por km²
estacoes_tot_ppp <- rescale.ppp(estacoes_tot_ppp, 1000, "km")
estacoes_dia_ppp <- rescale.ppp(estacoes_dia_ppp, 1000, "km")
estacoes_noi_ppp <- rescale.ppp(estacoes_noi_ppp, 1000, "km")

# Fazendo uma superfície de tendência para os números de assobios
estacoes_tot_im <- Smooth(estacoes_tot_ppp, bw.smoothppp, sigma = c(hmin=0.5, hmax=0.8), kernel = "gaussian")
estacoes_dia_im <- Smooth(estacoes_dia_ppp, bw.smoothppp, sigma = c(hmin=0.5, hmax=0.8), kernel = "gaussian")
estacoes_noi_im <- Smooth(estacoes_noi_ppp, bw.smoothppp, sigma = c(hmin=0.5, hmax=0.8), kernel = "gaussian")

# Voltando para m em X e Y. Z continua como km²; CRS como WGS 84 projetada - pacote raster
assobios_tot_im <- raster(rescale.im(estacoes_tot_im, .001, c("metro", "metros")))
assobios_dia_im <- raster(rescale.im(estacoes_dia_im, .001, c("metro", "metros")))
assobios_noi_im <- raster(rescale.im(estacoes_noi_im, .001, c("metro", "metros")))
crs(assobios_tot_im) <- CRS("+init=epsg:32723")
crs(assobios_dia_im) <- CRS("+init=epsg:32723")
crs(assobios_noi_im) <- CRS("+init=epsg:32723")
names(assobios_tot_im) <- "Assobio/hora"
names(assobios_dia_im) <- "Assobio/hora"
names(assobios_noi_im) <- "Assobio/hora"

# Definir o nome da pasta nova com o padrão de sempre
novo_dir <- paste0(pasta_proj,"/4_export/4_interp/LINHA_3")

# Criar a pasta propriamente dito
dir.create(novo_dir, recursive = TRUE)

# Salvando os raster PRINCIPAL do kernel
writeRaster(assobios_tot_im,
            filename = "4_export/4_interp/LINHA_3/L3_assobios_tot.tif",
            format = "GTiff",
            overwrite = TRUE,
            bylayer = TRUE,
            suffix = "names")
writeRaster(assobios_dia_im,
            filename = "4_export/4_interp/LINHA_3/L3_assobios_dia.tif",
            format = "GTiff",
            overwrite = TRUE,
            bylayer = TRUE,
            suffix = "names")
writeRaster(assobios_noi_im,
            filename = "4_export/4_interp/LINHA_3/L3_assobios_noi.tif",
            format = "GTiff",
            overwrite = TRUE,
            bylayer = TRUE,
            suffix = "names")
