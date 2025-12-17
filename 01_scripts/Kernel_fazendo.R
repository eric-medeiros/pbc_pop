# Carrega os pacotes necessários
library(sf)
library(spatstat)
library(spatialEco)
library(terra)

# --- Parte 1: Preparando a camada de água ---

# 1.1: Definindo a pasta do projeto
pasta_proj <- rprojroot::find_rstudio_root_file()

# 1.2: Lendo a camada da água (certifique-se que o script "2_Gerando_SHP.R" já foi executado)
agua_sf <- st_read(file.path(pasta_proj, "00_data", "amostragem_area.gpkg"))

# 1.3: Transformando para EPSG 32723 (UTM 23S)
agua_proj <- st_transform(agua_sf, 32723)

# 1.4: Conversão direta para objeto owin (abordagem A)
agua_owin <- as.owin(agua_proj)

# --- Parte 2: Preparando os pontos dos grupos ---

# 2.1: Lendo o arquivo dos grupos (vetores em GPKG)
grupos_sf <- st_read(file.path(pasta_proj, "03_results", "02_RESUMO",
                               "resumo_2020-01-01_2026-01-01.gpkg"),
                     layer = "pontos")

# 2.2: Transformando para EPSG 32723 (UTM 23S)
grupos_proj <- st_transform(grupos_sf, 32723)

# 2.3: Convertendo os pontos para objeto ppp do spatstat
grupos_ppp <- as.ppp(grupos_proj)

# 2.4: Atribuindo os atributos (marks), removendo a coluna de geometria
marks(grupos_ppp) <- as.data.frame(grupos_sf)[, -which(names(grupos_sf) == "geometry")]

# 2.5: Atribuindo a janela definida pela camada de água
Window(grupos_ppp) <- agua_owin

# 2.6: Definindo a unidade (metros, pois estamos em UTM)
unitname(grupos_ppp) <- c("metro", "metros")

# --- Parte 3: Análise de Kernel Density ---

# Opcional: converter as coordenadas para km para a análise e depois voltar para metros
grupos_ppp <- rescale.ppp(grupos_ppp, 1000, "km")
grupos_ppp <- unique.ppp(grupos_ppp)

# Cálculo do kernel density ponderado (usando um kernel gaussiano)
kernel <- density.ppp(grupos_ppp,
                      kernel = "gaussian",
                      sigma = bw.ppl(grupos_ppp),
                      edge = TRUE,
                      diggle = TRUE,
                      positive = TRUE,
                      weights = grupos_ppp$marks$tam_est)

# Reescala a imagem para que os eixos X e Y voltem a estar em metros
kernel <- rescale.im(kernel, .001, unitname = c("metro", "metros"))

# Converter o objeto im para raster (usando o pacote raster)
rast_proj <- rast(kernel)
crs(rast_proj) <- "epsg:32723"

# --- Parte 4: Extração de polígonos de densidade ---

# 4.1: Calcular os limiares CORRETOS para 50% e 95% do volume (não use quantis!)

probs <- c(0.95, 0.50)
limiares <- global(rast_proj, fun = quantile, probs = probs, na.rm = TRUE)

calcular_limiar <- function(raster, prob) {
  valores <- terra::values(raster, na.rm = TRUE)
  valores_ordenados <- sort(valores, decreasing = TRUE)
  soma_acumulada <- cumsum(valores_ordenados)
  total <- sum(valores_ordenados)
  indice_limiar <- which.max(soma_acumulada >= (prob * total))
  
  result <- valores_ordenados[indice_limiar] 
  
  return(result)
}

limiar_95 <- calcular_limiar(rast_proj, 0.95)  # Área que contém 95% da densidade
limiar_50 <- calcular_limiar(rast_proj, 0.50)  # Área que contém 50% da densidade

# 4.2: Criar máscaras CORRETAS (agora com parênteses fechados!)
mascara_95 <- rast_proj >= limiar_95
mascara_50 <- rast_proj >= limiar_50

# 4.3: Converter para polígonos e filtrar
p95 <- terra::as.polygons(mascara_95, dissolve = TRUE) %>% 
  terra::simplifyGeom(tolerance = 10)

p50 <- terra::as.polygons(mascara_50, dissolve = TRUE) %>% 
  terra::simplifyGeom(tolerance = 10)


# --- Parte 5: Exportação dos Resultados (em GPKG) ---

# Define o diretório de exportação
novo_dir <- file.path(pasta_proj, "03_results", "04_INTERP")
dir.create(novo_dir, recursive = TRUE, showWarnings = FALSE)

# Exporta o raster do kernel (GeoTIFF)
writeRaster(rast_proj,
            filename = file.path(novo_dir, "kernel.tif"),
            overwrite = TRUE)

# Converte os objetos polígonos para sf e exporta em GPKG
p50_sf <- st_as_sf(p50)[2,]
p95_sf <- st_as_sf(p95)[2,]

st_write(p50_sf, file.path(novo_dir, "p50.gpkg"), delete_dsn = TRUE)
st_write(p95_sf, file.path(novo_dir, "p95.gpkg"), delete_dsn = TRUE)

cat("Processamento e exportação concluídos.\n")
