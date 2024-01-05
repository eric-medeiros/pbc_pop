library(dplyr)
library(stringr)

pasta_cat <- "//NAS_IPeC/PBC-Pesquisa/PROJETOS/ANDAMENTO/01_FOTOID/02_ANALISE/01_HIST_ID"

caminho_IDs <- list.files(pasta_cat, full.names = TRUE)

tab_ID <- function(caminho_ID){
  tibble(Image = list.files(caminho_ID, ".JPG", ignore.case = TRUE),
         caminho = list.files(caminho_ID, ".JPG", ignore.case = TRUE, full.names = TRUE),
         CatalogID = str_sub(caminho_ID, -5, -1))
}

tab_IDs <- lapply(caminho_IDs, tab_ID) %>% bind_rows()

# Designando os caminhos de saidas
caminho_pasta_saida <- "//NAS_IPeC/PBC-Pesquisa/PROJETOS/ANDAMENTO/01_FOTOID/02_ANALISE/06_FINFINDR/fotos_base"
caminho_csv_saida <- paste0(caminho_pasta_saida, "/IDs.csv")


#Salva CSV
file.copy(tab_IDs$caminho, caminho_pasta_saida, overwrite = FALSE, copy.date = TRUE)
#Salve cópia das fotos 
write.csv(tab_IDs[c("Image", "CatalogID")], caminho_csv_saida, row.names = FALSE)


