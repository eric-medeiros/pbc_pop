# Função para inserir sonda no banco de dados
junta_sonda <- function (lista, dados_sonda) {
  library(dplyr)
  
  # Juntando a rota toda  
  lista$sonda <- dados_sonda
  
  invisible(lista)
  
  return(lista)
}
