banco <- function(pasta_data) {

library(magrittr)

rm(list = ls())

# Definindo caminhos
pasta_proj <- rprojroot::find_rstudio_root_file()
lista_R <- list.files(paste0(pasta_proj, "/01_scripts"), full.names = TRUE, pattern = "^[^0-9_]")
caminho_pasta_BD_RDS <-  paste0(pasta_proj, "/03_export")
caminho_arquivo_BD_RDS <-  paste0(caminho_pasta_BD_RDS, "/bd.rds")

# Importando as funções!!
for (func in lista_R) {source(func)}

# Executando - Pode mudar a pasta, mas só com estrutura interna
bd <- sub_WP(pasta_data) %>%
  junta_sonda(dados_sonda = le_sonda(pasta_data)) %>%
  junta_rota(dados_rotas = le_rota(pasta_data)) %>%
  junta_foto(dados_fotos = le_fotos(pasta_data))

if (dir.exists(caminho_pasta_BD_RDS) == FALSE) {dir.create(caminho_pasta_BD_RDS, recursive = TRUE)}

# Salvando RDS - Confira na pasta results/RDS
saveRDS(object = bd, file = caminho_arquivo_BD_RDS)

cat("-> RDS salvo")
}