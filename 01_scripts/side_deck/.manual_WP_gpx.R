library(dplyr)
library(sf)
library(lubridate)
library(stringr)


dados_WP <- bind_rows(c(WP = 1, lat = -25.00409, lng = -47.90240, datahora = "10/04/2023 09:22:00"),
                      c(WP = 2, lat = -24.96421, lng = -47.86943, datahora = "10/04/2023 09:55:46"),
                      c(WP = 3, lat = -24.96277, lng = -47.88041, datahora = "10/04/2023 10:14:20"),
                      c(WP = 4, lat = -24.96231, lng = -47.88175, datahora = "10/04/2023 10:18:13"),
                      c(WP = 5, lat = -24.96278, lng = -47.86646, datahora = "10/04/2023 10:27:06"),
                      c(WP = 6, lat = -24.96427, lng = -47.87681, datahora = "10/04/2023 10:43:45"),
                      c(WP = 7, lat = -24.94655, lng = -47.86109, datahora = "10/04/2023 10:58:55"),
                      c(WP = 8, lat = -24.94971, lng = -47.86088, datahora = "10/04/2023 11:05:55"),
                      c(WP = 9, lat = -24.93093, lng = -47.85141, datahora = "10/04/2023 11:17:00"),
                      c(WP = 10, lat = -24.93318, lng = -47.85495, datahora = "10/04/2023 11:31:45"),
                      c(WP = 11, lat = -24.91524, lng = -47.84122, datahora = "10/04/2023 11:42:40"),
                      c(WP = 12, lat = -24.91293, lng = -47.84189, datahora = "10/04/2023 11:53:00"),
                      c(WP = 13, lat = -24.91434, lng = -47.89419, datahora = "10/04/2023 13:02:35"),
                      c(WP = 14, lat = -24.91263, lng = -47.88857, datahora = "10/04/2023 13:16:20"),
                      c(WP = 15, lat = -24.99868, lng = -47.95209, datahora = "10/04/2023 13:59:05")) %>%
  as.data.frame() %>%
  st_as_sf(coords = c(x = "lng", y = "lat")) %>%
  transmute(ele = 0,
            time =  dmy_hms(datahora),
            magvar = as.numeric(NA),
            geoidheight = as.numeric(NA),
            name = str_pad(WP, side = "left", pad = "0", width = 3),
            cmt = as.character(NA),
            desc = as.character(NA),
            src = as.character(NA),
            link1_href = as.character(NA),
            link1_text = as.character(NA),
            link1_type = as.character(NA),
            link2_href = as.character(NA),
            link2_text = as.character(NA),
            link2_type = as.character(NA), 
            sym = "Flag, Blue",
            type = as.character(NA),   
            fix = as.character(NA),
            sat = as.integer(NA),
            hdop = as.numeric(NA),
            vdop = as.numeric(NA),
            pdop = as.numeric(NA),
            ageofdgpsdata = as.numeric(NA),
            dgpsid = as.integer(NA),
            geometry = geometry)

# Ajustando o CRS de GPS
st_crs(dados_WP) <- "WGS84"

# Salvando
arquivo_gpx_saida <- "C:/users/Eric/Desktop/074_2023_04_10/gps_R/Pontos de passagem_10-ABR-23.gpx"
st_write(obj = dados_WP,
         dsn = arquivo_gpx_saida,
         layer = "waypoints",
         driver = "GPX",
         append = FALSE,
         dataset_options = "GPX_USE_EXTENSIONS=yes",
         overwrite_layer = TRUE)


# Checagem
check_gpx <- st_read(arquivo_gpx_saida, layer = "waypoints")
plot(check_gpx["name"])
check_gpx
