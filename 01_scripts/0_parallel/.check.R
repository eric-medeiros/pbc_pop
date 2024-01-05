
# Pontos para checar:
# - arquivos de :
#   - excel - COMEÇAR
#     ok - Quais os dias com entradas no excel - ABA SAÍDA
#     ok - Desses dias, quais tem as amostragem/clima - quantos de cada?
#     - aba pausa/avistagens/wp extra/identificacoes/indivíduos - CONFIRMAR QUANTIDADE
#   - evidências
#   -scan:
#     -SE TEM ARUIVOS COM O NOME DO DIA
#   - GPS
#     - se existem os arquivos de WP e trajecto
#     - se tem pontos além dos do dia
#   - Sonda
#     - Se tem arquivo da sonda no dia
#     - Se tem na pasta
#     - se as foos que estão na pasta tem a data correta
# ---- 
# - Tempo decorrido desde o campo:
#   - 30 dias:
#     - se fotos foram triadas:
#       - se na pasta do campo - ver padrãoa adotar
#   - 60 dias:
#     - se já rolaram as identificações



# Limpando o Global Environment 
rm(list = ls())
data_i = "01/04/2023"
data_f = "31/07/2023"

check <- function(data_i, data_f) {
  library(readxl)
  library(dplyr)
  library(lubridate)

  # Pastas/arquivos no servidor NAS
  pasta_data <- "//nas_ipec/PBC-Pesquisa/PROJETOS/ANDAMENTO/01_FOTOID"
  pasta_campo <- paste0(pasta_data, "/01_CAMPO")
  arquivo_excel <- paste0(pasta_campo, "/02_EXCEL/populacional_PBC.xlsx")
  
  pasta_fotos <- paste0(pasta_campo, "/00_FOTOS")
  pasta_scan <- paste0(pasta_campo, "/01_SCAN")
  pasta_gps <- paste0(pasta_campo, "/03_GPS")
  pasta_sonda <- paste0(pasta_campo, "/04_SONDA")
  pasta_evidencias <- paste0(pasta_campo, "/05_EVIDENCIAS")
  
  datas_saidas <- 
    read_excel(arquivo_excel, sheet = "Saidas", col_types = c("text", "date", "text", "text", "text", "text", "text", "text", "text", "text")) %>%
    mutate(data = ymd(data)) %>%
    filter(data>dmy(data_i), data<dmy(data_f)) %>%
    select(data)
  
  datas_amostragens <- 
  read_excel(arquivo_excel, sheet = "Amostragem", col_types = c("text", "date", "text", "text", "text", "text", "text")) %>%
    mutate(data = ymd(data)) %>%
    filter(data>dmy(data_i), data<dmy(data_f)) %>%
    group_by(data) %>%
    summarise(n_amos = n())
  
  datas_clima <- 
    read_excel(arquivo_excel, sheet = "Clima", col_types = c("text", "date", "text", "text", "text", "text", "text", "text", "text", "text", "text")) %>%
    mutate(data = ymd(data)) %>%
    filter(data>dmy(data_i), data<dmy(data_f)) %>%
    group_by(data) %>%
    summarise(n_clima = n())
  
  
  
datas_saidas %>%
  left_join(datas_amostragens) %>%
  left_join(datas_clima)
  
  
  }

"ACA" %in% available.packages()
