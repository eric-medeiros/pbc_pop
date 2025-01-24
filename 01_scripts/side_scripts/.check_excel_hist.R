library(xlsx)
library(readxl)
library(stringr)
library(tibble)
library(dplyr)
library(lubridate)

identificacoes <- 
  read_xlsx(
    path = file.path("//NAS_IPeC","PBC-Pesquisa","PROJETOS","ANDAMENTO","01_FOTOID","01_CAMPO","02_EXCEL","populacional_PBC.xlsx"),
    sheet = "identificacoes",
    skip = 3
  ) %>%
  mutate(
    data = paste0( str_pad(day(data), 2, "left", "0"), "/", str_pad(month(data), 2, "left", "0"), "/", year(data)),
    # atipia =  case_when(is.na(atipia) ~ "", TRUE ~ atipia),
    # obs =  case_when(is.na(obs) ~ "", TRUE ~ obs)
  )

tabela_pasta <- tibble(
  caminho_pasta = c(
    list.files(
      file.path(
        "//NAS_IPeC",
        "PBC-Pesquisa",
        "PROJETOS",
        "ANDAMENTO",
        "01_FOTOID",
        "02_ANALISE",
        "01_HIST_ID"),
      full.names = TRUE) %>%
      str_subset("Thumbs\\.db|\\[vagos\\]", negate = TRUE),
    list.files(
      file.path(
        "//NAS_IPeC",
        "PBC-Pesquisa",
        "PROJETOS",
        "ANDAMENTO",
        "01_FOTOID",
        "02_ANALISE",
        "01_HIST_ID",
        "[vagos]"),
      full.names = TRUE)),
  pasta = basename(caminho_pasta),
  id = pasta %>% str_remove("\\[vago\\] ") %>% str_remove(" \\†"),
  status = ifelse(
    str_detect(pasta, "\\[vago\\]"), "vago", 
    ifelse(str_detect(pasta, "†"), "†", "-"))) %>%
  select(pasta, id, status, caminho_pasta)

arquivos_hist=list()
for(c in 1:length(tabela_pasta$caminho_pasta)) {
  arquivos_hist <- bind_rows(
    arquivos_hist,
    tibble(
      arquivos_renome = list.files(tabela_pasta$caminho_pasta[c], "jpg|JPG|jpeg|JPEG"),
      arquivo_original =  arquivos_renome %>% str_replace("\\).*", ")") %>% str_replace(".*?(E\\d{1,2}S)", "\\1"),
      caminho_pasta = tabela_pasta$caminho_pasta[c]
    )
  )
}

jpgs <- 
  arquivos_hist %>%
  full_join(tabela_pasta, by = "caminho_pasta")

excel_e_jpg <- 
  jpgs %>%
  full_join(identificacoes, by = c("arquivo_original" = "arquivo", "id")) %>%
  filter(!is.na(saida)) %>%
  filter(!is.na(pasta)) %>%
  select(saida, data, grupo, id, arquivo = arquivo_original, quali_f, quali_m, lado, filhote_ac, id_reid, catalogo_atualizado, lado_novo, identificador, fotografa, atipia, obs)

excel_sem_jpg <- 
  jpgs %>%
  right_join(identificacoes, by = c("arquivo_original" = "arquivo", "id")) %>%
  filter(is.na(pasta)) %>%
  select(saida, data, grupo, id, arquivo = arquivo_original, quali_f, quali_m, lado, filhote_ac, id_reid, catalogo_atualizado, lado_novo, identificador, fotografa, atipia, obs)

jpg_sem_excel <- 
  jpgs %>%
  left_join(identificacoes, by = c("arquivo_original" = "arquivo", "id")) %>% 
  filter(is.na(saida) & !is.na(arquivo_original)) %>%
  select(pasta, arquivos_renome)

pasta_sem_jpg <- 
  jpgs %>%
  left_join(identificacoes, by = c("arquivo_original" = "arquivo", "id")) %>% 
  filter(is.na(arquivo_original)) %>%
  select(pasta)

jpg_nome_trocado <- 
  jpgs %>%
  filter(str_detect(arquivos_renome, "\\)[a-zA-Z]|\\)\\s*[a-zA-Z]\\s+[a-zA-Z]")) %>%
  select(arquivos_renome, caminho_pasta)

arquivo_output <- file.path("C:", "Users", "PBC-PESQUISA", "Desktop", "auxiliar.xlsx")

write.xlsx(
  x = excel_e_jpg,
  file = arquivo_output,
  sheetName = "excel_e_jpg",
  showNA = FALSE,
  append = FALSE)

write.xlsx(
  x = excel_sem_jpg,
  file = arquivo_output,
  sheetName = "excel_sem_jpg",
  showNA = FALSE,
  append = TRUE)

write.xlsx(
  x = jpg_sem_excel,
  file = arquivo_output,
  sheetName = "jpg_sem_excel",
  showNA = FALSE,
  append = TRUE)

write.xlsx(
  x = pasta_sem_jpg,
  file = arquivo_output,
  sheetName = "pasta_sem_jpg",
  showNA = FALSE,
  append = TRUE)

write.xlsx(
  x = jpg_nome_trocado,
  file = arquivo_output,
  sheetName = "jpg_nome_trocado",
  showNA = FALSE,
  append = TRUE)
