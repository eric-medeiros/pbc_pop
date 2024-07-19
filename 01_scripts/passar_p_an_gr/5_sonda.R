# Criação de um arquivo *.tif para cada um dos parâmetros da sonda (temp, sal, od, ph, turb, press)

# Limpando o Global Environment 
rm(list = ls())

# Linha 1 ----

library(terra)
library(dplyr)
library(tibble)
library(tidyr)
library(lubridate)
library(stringr)


# Achando a pasta do projeto
pasta_proj <- rprojroot::find_rstudio_root_file()

# Abrindo shape da agua transformando o para EPSG 32723 - projetadas/WGS 84/UTM 23S
agua_sv <- vect("1_data/SHAPES_AUX/Agua_L1.shp")

# Mudando a projeção
agua_proj <- agua_sv %>%
  terra::project("epsg:32723")

# criando raster da agua transformada para projetada
agua_im <- rast(agua_proj, resolution = 250)

# criando novo raster com o raster acima como valor - estranho, mas esse é plotável
agua_ras <- rasterize(agua_proj, agua_im)


# Abrindo arquivo RDS
bd_L1 <- readRDS(paste0(pasta_proj,"/4_export/1_banco/bd_L1.rds"))

# Definindo dados da sonda com coordenadas para os pontos
sonda <- bd_L1$sonda %>% group_by(saida) %>% nest

# Definir o nome da pasta nova com o padrão de sempre
novo_dir <- paste0(pasta_proj, "/4_export/5_sonda/LINHA_1")

# Criar uma pasta para cada parâmetro pare receber os arquivos diários
dir.create(novo_dir, recursive = TRUE)


# Para cada saida em que houve coleta de sonda, será feito o seguinte
for (i in 1:nrow(sonda)) {
  
  # Definir do dados com o crs 4326 - geográfica/WGS 84 - padrão GPS
  sonda_sv <- sonda[[2]][[i]] %>%
    vect(c("lng","lat"), crs ="epsg:4326") %>%
    terra::project("epsg:32723") %>%
    terra::buffer(250) %>%
    terra::crop(agua_proj)
  
  sonda_sv <- sonda_sv[,c(-9,-10)]
  
  # Criando os rasters
  ras_temp <- rasterize(x = sonda_sv[,"Temp"],
                        y = agua_ras,
                        field = "Temp",
                        fun = "mean",
                        na.rm = TRUE)
  
  ras_sal <- rasterize(x = sonda_sv[,"Sal"],
                       y = agua_ras,
                       field = "Sal",
                       fun = "mean",
                       na.rm = TRUE)
  
  ras_od <- rasterize(x = sonda_sv[,"OD"],
                      y = agua_ras,
                      field = "OD",
                      fun = "mean",
                      na.rm = TRUE)
  
  ras_turb <- rasterize(x = sonda_sv[,"Turb"],
                        y = agua_ras,
                        field = "Turb",
                        fun = "mean",
                        na.rm = TRUE)
  
  ras_ph <- rasterize(x = sonda_sv[,"pH"],
                      y = agua_ras,
                      field = "pH",
                      fun = "mean",
                      na.rm = TRUE)
  
  ras_pres <- rasterize(x = sonda_sv[,"Pres"],
                        y = agua_ras,
                        field = "Pres",
                        fun = "mean",
                        na.rm = TRUE)
  
  # Definindo o nomes dos arquivos
  arquivo_temp <- paste0(novo_dir, "/TP_", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                         str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10), ".tif")
  arquivo_sal <- paste0(novo_dir, "/SL_", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                        str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10), ".tif")
  arquivo_od <- paste0(novo_dir, "/OD_", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                       str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10), ".tif")
  arquivo_turb <- paste0(novo_dir, "/TB_", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                         str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10), ".tif")
  arquivo_ph <- paste0(novo_dir, "/PH_", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                       str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10), ".tif")
  arquivo_pres <- paste0(novo_dir, "/PA_", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                         str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10), ".tif")
  
  
  # Salvando os arquivos
  writeRaster(x = terra::project(ras_temp,"epsg:4674"),
              filename = arquivo_temp,
              overwrite = TRUE)
  
  writeRaster(x = terra::project(ras_sal,"epsg:4674"),
              filename = arquivo_sal,
              overwrite = TRUE)
  
  writeRaster(x = terra::project(ras_od,"epsg:4674"),
              filename = arquivo_od,
              overwrite = TRUE)
  
  writeRaster(x = terra::project(ras_turb,"epsg:4674"),
              filename = arquivo_turb,
              overwrite = TRUE)
  
  writeRaster(x = terra::project(ras_ph,"epsg:4674"),
              filename = arquivo_ph,
              overwrite = TRUE)
  
  writeRaster(x = terra::project(ras_pres,"epsg:4674"),
              filename = arquivo_pres,
              overwrite = TRUE)
  
  # Barra de progresso
  pb <- txtProgressBar(min = 0, max = nrow(sonda), initial = 0, style =3)
  setTxtProgressBar(pb,i)
  
}




# Linha 2 ----

library(terra)
library(dplyr)
library(tibble)
library(tidyr)
library(lubridate)
library(stringr)


# Achando a pasta do projeto
pasta_proj <- rprojroot::find_rstudio_root_file()

# Abrindo shape da agua transformando o para EPSG 32723 - projetadas/WGS 84/UTM 23S
agua_sv <- vect("1_data/SHAPES_AUX/Agua_L2.shp")

# Mudando a projeção
agua_proj <- agua_sv %>%
  terra::project("epsg:32723")

# criando raster da agua transformada para projetada
agua_im <- rast(agua_proj, resolution = 250)

# criando novo raster com o raster acima como valor - estranho, mas esse é plotável
agua_ras <- rasterize(agua_proj, agua_im)


# Abrindo arquivo RDS
bd_L2 <- readRDS(paste0(pasta_proj,"/4_export/1_banco/bd_L2.rds"))

# Definindo dados da sonda com coordenadas para os pontos
sonda <- bd_L2$sonda %>% group_by(saida) %>% nest

# Definir o nome da pasta nova com o padrão de sempre
novo_dir <- paste0(pasta_proj, "/4_export/5_sonda/LINHA_2")

# Criar uma pasta para cada parâmetro pare receber os arquivos diários
dir.create(novo_dir, recursive = TRUE)


# Para cada saida em que houve coleta de sonda, será feito o seguinte
for (i in 1:nrow(sonda)) {
  
  # Definir do dados com o crs 4326 - geográfica/WGS 84 - padrão GPS
  sonda_sv <- sonda[[2]][[i]] %>%
    vect(c("lng","lat"), crs ="epsg:4326") %>%
    terra::project("epsg:32723") %>%
    terra::buffer(250) %>%
    terra::crop(agua_proj)
  
  sonda_sv <- sonda_sv[,c(-9,-10)]
  
  # Criando os rasters
  ras_temp <- rasterize(x = sonda_sv[,"Temp"],
                        y = agua_ras,
                        field = "Temp",
                        fun = "mean",
                        na.rm = TRUE)
  
  ras_sal <- rasterize(x = sonda_sv[,"Sal"],
                       y = agua_ras,
                       field = "Sal",
                       fun = "mean",
                       na.rm = TRUE)
  
  ras_od <- rasterize(x = sonda_sv[,"OD"],
                      y = agua_ras,
                      field = "OD",
                      fun = "mean",
                      na.rm = TRUE)
  
  ras_turb <- rasterize(x = sonda_sv[,"Turb"],
                        y = agua_ras,
                        field = "Turb",
                        fun = "mean",
                        na.rm = TRUE)
  
  ras_ph <- rasterize(x = sonda_sv[,"pH"],
                      y = agua_ras,
                      field = "pH",
                      fun = "mean",
                      na.rm = TRUE)
  
  ras_pres <- rasterize(x = sonda_sv[,"Pres"],
                        y = agua_ras,
                        field = "Pres",
                        fun = "mean",
                        na.rm = TRUE)
  
  # Definindo o nomes dos arquivos
  arquivo_temp <- paste0(novo_dir, "/TP_", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                         str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10), ".tif")
  arquivo_sal <- paste0(novo_dir, "/SL_", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                        str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10), ".tif")
  arquivo_od <- paste0(novo_dir, "/OD_", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                       str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10), ".tif")
  arquivo_turb <- paste0(novo_dir, "/TB_", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                         str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10), ".tif")
  arquivo_ph <- paste0(novo_dir, "/PH_", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                       str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10), ".tif")
  arquivo_pres <- paste0(novo_dir, "/PA_", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                         str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10), ".tif")
  
  # Salvando os arquivos
  writeRaster(x = terra::project(ras_temp,"epsg:4674"),
              filename = arquivo_temp,
              overwrite = TRUE)
  
  writeRaster(x = terra::project(ras_sal,"epsg:4674"),
              filename = arquivo_sal,
              overwrite = TRUE)
  
  writeRaster(x = terra::project(ras_od,"epsg:4674"),
              filename = arquivo_od,
              overwrite = TRUE)
  
  writeRaster(x = terra::project(ras_turb,"epsg:4674"),
              filename = arquivo_turb,
              overwrite = TRUE)
  
  writeRaster(x = terra::project(ras_ph,"epsg:4674"),
              filename = arquivo_ph,
              overwrite = TRUE)
  
  writeRaster(x = terra::project(ras_pres,"epsg:4674"),
              filename = arquivo_pres,
              overwrite = TRUE)
  
  
  pb <- txtProgressBar(min = 0, max = nrow(sonda), initial = 0, style =3)
  setTxtProgressBar(pb,i)
  
}



# Linha 3 ----

library(terra)
library(dplyr)
library(tibble)
library(tidyr)
library(lubridate)
library(stringr)


# Achando a pasta do projeto
pasta_proj <- rprojroot::find_rstudio_root_file()

# Abrindo shape da agua transformando o para EPSG 32723 - projetadas/WGS 84/UTM 23S
agua_sv <- vect("1_data/SHAPES_AUX/Agua_L3.shp")

# Mudando a projeção
agua_proj <- agua_sv %>%
  terra::project("epsg:32723")

# criando raster da agua transformada para projetada
agua_im <- rast(agua_proj, resolution = 250)

# criando novo raster com o raster acima como valor - estranho, mas esse é plotável
agua_ras <- rasterize(agua_proj, agua_im)


# Abrindo arquivo RDS
bd_L3 <- readRDS(paste0(pasta_proj,"/4_export/1_banco/bd_L3.rds"))

# Definindo dados da sonda com coordenadas para os pontos
sonda <- bd_L3$sonda %>% group_by(saida) %>% nest

# Definir o nome da pasta nova com o padrão de sempre
novo_dir <- paste0(pasta_proj, "/4_export/5_sonda/LINHA_3")

# Criar uma pasta para cada parâmetro pare receber os arquivos diários
dir.create(novo_dir, recursive = TRUE)


# Para cada saida em que houve coleta de sonda, será feito o seguinte
for (i in 1:nrow(sonda)) {
  
  # Definir do dados com o crs 4326 - geográfica/WGS 84 - padrão GPS
  sonda_sv <- sonda[[2]][[i]] %>%
    vect(c("lng","lat"), crs ="epsg:4326") %>%
    terra::project("epsg:32723") %>%
    terra::buffer(1000) %>%
    terra::crop(agua_proj)
  
  sonda_sv <- sonda_sv[,c(-1,-9,-10)]
  
  # Criando os rasters
  ras_temp <- rasterize(x = sonda_sv[,"Temp"],
                        y = agua_ras,
                        field = "Temp",
                        fun = "mean",
                        na.rm = TRUE)
  
  ras_sal <- rasterize(x = sonda_sv[,"Sal"],
                       y = agua_ras,
                       field = "Sal",
                       fun = "mean",
                       na.rm = TRUE)
  
  ras_od <- rasterize(x = sonda_sv[,"OD"],
                      y = agua_ras,
                      field = "OD",
                      fun = "mean",
                      na.rm = TRUE)
  
  ras_turb <- rasterize(x = sonda_sv[,"Turb"],
                        y = agua_ras,
                        field = "Turb",
                        fun = "mean",
                        na.rm = TRUE)
  
  ras_ph <- rasterize(x = sonda_sv[,"pH"],
                      y = agua_ras,
                      field = "pH",
                      fun = "mean",
                      na.rm = TRUE)
  
  ras_pres <- rasterize(x = sonda_sv[,"Pres"],
                        y = agua_ras,
                        field = "Pres",
                        fun = "mean",
                        na.rm = TRUE)
  
  # Definindo o nomes dos arquivos
  arquivo_temp <- paste0(novo_dir, "/TP_", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                         str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10), ".tif")
  arquivo_sal <- paste0(novo_dir, "/SL_", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                        str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10), ".tif")
  arquivo_od <- paste0(novo_dir, "/OD_", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                       str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10), ".tif")
  arquivo_turb <- paste0(novo_dir, "/TB_", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                         str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10), ".tif")
  arquivo_ph <- paste0(novo_dir, "/PH_", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                       str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10), ".tif")
  arquivo_pres <- paste0(novo_dir, "/PA_", str_pad(sonda[[1]][[i]],width = 3, side = "left", pad = "0"), "_",
                         str_sub(str_replace_all(sonda[[2]][[i]]$datahora_SONDA[[1]], "-", "_"),1, 10), ".tif")
  
  
  # Salvando os arquivos
  writeRaster(x = terra::project(ras_temp,"epsg:4674"),
              filename = arquivo_temp,
              overwrite = TRUE)
  
  writeRaster(x = terra::project(ras_sal,"epsg:4674"),
              filename = arquivo_sal,
              overwrite = TRUE)
  
  writeRaster(x = terra::project(ras_od,"epsg:4674"),
              filename = arquivo_od,
              overwrite = TRUE)
  
  writeRaster(x = terra::project(ras_turb,"epsg:4674"),
              filename = arquivo_turb,
              overwrite = TRUE)
  
  writeRaster(x = terra::project(ras_ph,"epsg:4674"),
              filename = arquivo_ph,
              overwrite = TRUE)
  
  writeRaster(x = terra::project(ras_pres,"epsg:4674"),
              filename = arquivo_pres,
              overwrite = TRUE)
  
  
  pb <- txtProgressBar(min = 0, max = nrow(sonda), initial = 0, style =3)
  setTxtProgressBar(pb,i)
  
}
