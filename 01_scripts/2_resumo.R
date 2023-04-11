# Resumão de relatório

# Limpando o Global Environment 
rm(list = ls())

library(magrittr)
library(dplyr)
library(purrr)
library(lubridate)
library(data.table)

pasta_proj <- rprojroot::find_rstudio_root_file()

bd <- readRDS(paste0(pasta_proj,"/02_export/bd.rds"))

res <- bd$avistagens %>%
  mutate(tempo_grupo = bd$avistagens$datahora_F - bd$avistagens$datahora_I,
         tam_grupo = nafill(tam_grupo, fill = 0L),
         tam_min = nafill(tam_min, fill = 0L),
         tam_max = nafill(tam_max, fill = 0L)) %>%
  rowwise() %>%
  mutate(tam_est = sum(tam_grupo, mean(c(tam_min, tam_max)))) %>%
  ungroup() %>%
  dplyr::select(1:4, 25, 26) %>%
  group_by(saida) %>%
  summarise(.groups = "keep",
            SAIDA = first(saida),
            DATA = first(data),
            BOTOS = round(sum(tam_est), 0),
            T_BOTO = seconds_to_period(sum(tempo_grupo, na.rm = TRUE)),
            GRUPOS = n(),
            FOTOS = sum(num_fotos, na.rm = TRUE)) %>%
  right_join(bd$saidas, by = "saida") %>%
  dplyr::select(1:7, litros_consumidos)

res <- bd$rota %>%
  group_by(saida) %>%
  summarise(.groups = "keep",
            KM = round(sum(dist_p_prox, na.rm = TRUE),1),
            T_BARCO = seconds_to_period(sum(tempo_p_prox, na.rm = TRUE))) %>%
  right_join(res, by = "saida") %>%
  rename(LITROS = litros_consumidos) %>%
  ungroup() %>%
  dplyr::select(SAIDA,
                DATA,
                KM,
                T_BARCO,
                T_BOTO,
                GRUPOS,
                BOTOS,
                FOTOS,
                LITROS,
                saida) %>%
  arrange(DATA)

res$DATA <- res$DATA %>% format("%d/%m/%Y")

units(res$KM)$numerator <- "km"
res$KM <- round(res$KM, 1) %>% str_replace_all("\\.", ",")

res$T_BOTO <-  paste0(str_pad(lubridate::hour(res$T_BOTO), 2, pad = "0"), ":",
                      str_pad(lubridate::minute(res$T_BOTO), 2, pad = "0"), ":",
                      str_pad(lubridate::second(res$T_BOTO), 2, pad = "0"))

res$T_BARCO <-  paste0(str_pad(lubridate::hour(res$T_BARCO), 2, pad = "0"), ":",
                       str_pad(lubridate::minute(res$T_BARCO), 2, pad = "0"),":",
                       str_pad(lubridate::second(res$T_BARCO), 2, pad = "0"))

# Definindo caminho para o arquivo TXT
caminho_pasta_resumo_TXT <-  paste0(pasta_proj, "/02_export")
caminho_arquivo_resumo_TXT <-  paste0(caminho_pasta_resumo_TXT, "/resumo.txt")

if (dir.exists(caminho_pasta_resumo_TXT) == FALSE) {
  dir.create(caminho_pasta_resumo_TXT, recursive = TRUE)
}


# Salvando TXT - Confira na pasta /4_export/2_resumo
write.table(x = res,
            file = caminho_arquivo_resumo_TXT,
            sep = "\t",
            row.names = FALSE)
