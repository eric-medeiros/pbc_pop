# Função para inserir rota no banco de dados
junta_rota <- function (lista, dados_rotas) {
  library(dplyr)
 
  # Juntando a rota toda  
  lista$rotas <- dados_rotas 
  
  invisible(lista)
  
  return(lista)
}
