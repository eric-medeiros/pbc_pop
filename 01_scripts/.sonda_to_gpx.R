library(readxl)
library(dplyr)
library(sf)
library(stringr)
library(lubridate)


pasta_proj <- rprojroot::find_rstudio_root_file()

# Leitura do arquivo da sonda

arq_sonda <- "//NAS_IPeC/PBC-Pesquisa/PROJETOS/ANDAMENTO/01_FOTOID/01_CAMPO/04_SONDA/074_2023_04_10/LOG001_0410103125.xls"
dados_sonda <- read_xls(arq_sonda, sheet = 2) %>%
  suppressWarnings()

# Arrumando para ter todos os dados de um *.gpx vundo do GPS
dados_GPS_sonda <- dados_sonda %>%
  transmute(lat = -1 * as.numeric(str_sub(dados_sonda$'GPS Lat.', 1, 8)),
            lng = -1 * as.numeric(str_sub(dados_sonda$'GPS Long.', 1, 8)),
            time = ymd_hms(paste(as.character(Date),
                                 str_extract(as.character(Time), "\\d{2}:\\d{2}:\\d{2}")),
                           tz = "America/Sao_Paulo")) %>%
  suppressWarnings() %>%
  na.omit() %>%
  as.data.frame() %>%
  st_as_sf(coords = c("lng","lat"),
           crs = "WGS84") %>%
  mutate(track_fid = 0L,
         track_seg_id = 0L,
         track_seg_point_id = 0:(n()-1),
         ele = 0L,
         time = ymd_hms(time),
         magvar = as.numeric(NA),
         geoidheight = as.numeric(NA),
         name = as.character(NA),
         cmt = as.character(NA),
         desc = as.character(NA),
         src = as.character(NA),
         link1_href = as.character(NA),
         link1_text = as.character(NA),
         link1_type = as.character(NA),
         link2_href = as.character(NA),
         link2_text = as.character(NA),
         link2_type = as.character(NA),
         sym = as.character(NA),
         type = as.character(NA),
         fix = as.character(NA),
         sat = as.numeric(NA),
         hdop = as.numeric(NA),
         vdop = as.numeric(NA),
         pdop = as.numeric(NA),
         ageofdgpsdata = as.numeric(NA),
         dgpsid = as.numeric(NA)) %>%
  select(3:6,1,7:27,2)

# # Ajuste de horário
# dados_GPS_sonda$TIME <- as.character(dados_GPS_sonda$time)
# dados_GPS_sonda$TIME <- dados_GPS_sonda$time - hms("1:12:00")

# Salvando
arquivo_gpx_saida <- paste0(pasta_proj,"/03_export/gpx/Trajecto.gpx")

file.remove(arquivo_gpx_saida)

st_write(obj = dados_GPS_sonda,
         dsn = arquivo_gpx_saida,
         "GPX_USE_EXTENSIONS = YES")

# Checagem
check_gpx <- st_read(arquivo_shp_saida)
check_gpx
plot(check_gpx[1])