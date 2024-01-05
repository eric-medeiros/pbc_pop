# Scripts para gerar os bancos de dados em RDS

# Limpando o Global Environment 
rm(list = ls())

# Pegando a pasta do projeto - vai ser diferente em cada PC, mas ele acha sozinho
pasta_proj <- rprojroot::find_rstudio_root_file()

# Importando as funções!!
source(paste0(pasta_proj, "/01_scripts/bd.R"))

# Especificando os caminhos mexer nos "#" para selecionar:
# 1) Dados de exemplo que estão no github
# pasta_data <- paste(pasta_proj, "/1_data/LINHA_1", sep = "")

# 2)Pastas do nosso servidor
pasta_data <- "//nas_ipec/PBC-Pesquisa/PROJETOS/ANDAMENTO/01_FOTOID"

# Executando - Pode mudar a pasta, mas só com estrutura interna
bd <- bd(pasta_data)

# Definindo caminho para o arquivo RDS
caminho_pasta_BD_RDS <-  paste0(pasta_proj, "/03_export")
caminho_arquivo_BD_RDS <-  paste0(caminho_pasta_BD_RDS, "/bd.rds")

if (dir.exists(caminho_pasta_BD_RDS) == FALSE) {
  dir.create(caminho_pasta_BD_RDS, recursive = TRUE)
}

# Salvando RDS - Confira na pasta results/RDS
saveRDS(object = bd, file = caminho_arquivo_BD_RDS)
