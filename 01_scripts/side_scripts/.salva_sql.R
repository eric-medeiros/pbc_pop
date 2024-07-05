# Puxa todas as funções de leitura e o de junção dos dados. Depois cria diversas tabelas relacionadas em um banco SQLite

source("01_scripts/le_planilha.R")
source("01_scripts/le_wps.R")
source("01_scripts/le_rotas.R")
source("01_scripts/le_sondas.R")
source("01_scripts/le_fotos.R")
source("01_scripts/junta_tudo.R")

library(DBI)
library(RSQLite)
  
pasta_data <- "//nas_ipec/PBC-Pesquisa/PROJETOS/ANDAMENTO/01_FOTOID"

inicio_geral <- Sys.time()

inicio_excel <- Sys.time()
dados_excel <- le_planilha(pasta_data)
tempo_excel <- (Sys.time() - inicio_excel) %>% as.numeric %>% round(3)

inicio_wps <- Sys.time()
dados_wps <- le_wps(pasta_data)
tempo_wps <- (Sys.time() - inicio_wps) %>% as.numeric %>% round(3)

inicio_rotas <- Sys.time()
dados_rotas <- le_rotas(pasta_data)
tempo_rotas <- (Sys.time() - inicio_rotas) %>% as.numeric %>% round(3)

inicio_sonda <- Sys.time()
dados_sondas <- le_sondas(pasta_data)
tempo_sonda <- (Sys.time() - inicio_sonda) %>% as.numeric %>% round(3)

inicio_fotos <- Sys.time()
dados_fotos <- le_fotos(pasta_data)
tempo_fotos <- (Sys.time() - inicio_fotos) %>% as.numeric %>% round(3)

inicio_junta <- Sys.time()
dados_banco <- junta_tudo(dados_excel = dados_excel,
                          dados_wps = dados_wps,
                          dados_rotas = dados_rotas,
                          dados_sondas = dados_sondas,
                          dados_fotos = dados_fotos)
tempo_junta <- (Sys.time() - inicio_junta) %>% as.numeric %>% round(3)

tempo_geral <- (Sys.time() - inicio_geral) %>% as.numeric %>% round(3)

tempo_excel
tempo_wps
tempo_rotas
tempo_sonda
tempo_fotos
tempo_junta
tempo_geral

pasta_banco <- paste0(pasta_data, "/02_ANALISE/04_BANCO_DE_DADOS")
arquivo_banco <- paste0(pasta_banco, "/sql_pop.db")

con <- dbConnect(RSQLite::SQLite(), arquivo_banco)

criar_inserir_tabela <- function(tabela_nome, dados, estrutura) {
  if (dbExistsTable(con, tabela_nome)) { dbExecute(con, paste("DROP TABLE", tabela_nome)) }
  
  query <- paste("CREATE TABLE", tabela_nome, estrutura)
  dbExecute(con, query)
  
  dbWriteTable(con, tabela_nome, dados, row.names = FALSE, append = TRUE)
}

# estruturas ----

estrutura_saidas <- "(
  saida TEXT PRIMARY KEY,
  data DATE,
  barco TEXT,
  wp_I TEXT,
  datahora_I DATETIME,
  lng_I DECIMAL(7, 5),
  lat_I DECIMAL(7, 5),
  wp_F TEXT,
  datahora_F DATETIME,
  lng_F DECIMAL(7, 5),
  lat_F DECIMAL(7, 5),
  rota TEXT,
  equipe TEXT,
  barqueiro TEXT,
  litros_consumidos DECIMAL(4, 1),
  obs TEXT
  PRIMARY KEY (saida)
)"

estrutura_amostragens <- "(
  saida TEXT PRIMARY KEY,
  data DATE,
  exp TEXT,
  rota TEXT,
  wp_I TEXT,
  datahora_I DATETIME,
  lng_I DECIMAL(7, 5),
  lat_I DECIMAL(7, 5),
  wp_F TEXT,
  datahora_F DATETIME,
  lng_F DECIMAL(7, 5),
  lat_F DECIMAL(7, 5),
  obs TEXT
)"

estrutura_climas <- "(
  saida TEXT,
  clima TEXT,
  data DATE,
  wp_I TEXT,
  datahora_I DATETIME,
  lng_I DECIMAL(7, 5),
  lat_I DECIMAL(7, 5),
  wp_F TEXT,
  datahora_F DATETIME,
  lng_F DECIMAL(7, 5),
  lat_F DECIMAL(7, 5),
  dir_vento TEXT,
  veloc_vento DECIMAL(3, 1),
  beaufort INT,
  cobert_nuvens INT,
  visibilidade INT,
  reflexo INT,
  obs TEXT,
  PRIMARY KEY (saida, clima)
)"

estrutura_avistagens <- "(
  saida TEXT,
  grupo TEXT,
  data DATE,
  oc_id TEXT,
  num_fotos INT,
  wp_I TEXT,
  datahora_I DATETIME,
  lng_I DECIMAL(7, 5),
  lat_I DECIMAL(7, 5),
  wp_F TEXT,
  datahora_F DATETIME,
  lng_F DECIMAL(7, 5),
  lat_F DECIMAL(7, 5),
  estado TEXT,
  coesao TEXT,
  tam_grupo INT,
  tam_min INT,
  tam_max INT,
  n_marcados INT,
  n_lisos INT,
  n_neonatos INT,
  n_infantes INT,
  n_juvenis INT,
  n_adultos INT,
  obs TEXT,
  PRIMARY KEY (saida, grupo)
)"

estrutura_pausas <- "(
  saida TEXT,
  pausa TEXT,
  data DATE,
  wp_I TEXT,
  datahora_I DATETIME,
  lng_I DECIMAL(7, 5),
  lat_I DECIMAL(7, 5),
  wp_F TEXT,
  datahora_F DATETIME,
  lng_F DECIMAL(7, 5),
  lat_F DECIMAL(7, 5),
  obs TEXT,
  PRIMARY KEY (saida, pausa)
)"

estrutura_identificacoes <- "(
  saida TEXT,
  exp TEXT,
  grupo TEXT,
  ID TEXT,
  data DATE,
  datahora DATETIME,
  lng DECIMAL(7, 5),
  lat DECIMAL(7, 5),
  arquivo TEXT,
  quali_F TEXT,
  quali_M TEXT,
  lado TEXT,
  filhote_ac BOOLEAN,
  id_reid TEXT,
  catalogo_atualizado BOOLEAN,
  lado_novo BOOLEAN,
  identificador TEXT,
  fotografa TEXT,
  obs TEXT,
  arquivo_sub TEXT,
  caminho TEXT
)"
  
dados_banco$identificacoes
wps
rotas
sonda

# execução ----

criar_inserir_tabela(tabela_nome = "saidas",
                     dados = dados_banco$saidas,
                     estrutura = estrutura_saidas)

criar_inserir_tabela(tabela_nome = "amostragens",
                     dados = dados_banco$amostragens,
                     estrutura = estrutura_amostragens)

criar_inserir_tabela(tabela_nome = "climas",
                     dados = dados_banco$climas,
                     estrutura = estrutura_climas)

criar_inserir_tabela(tabela_nome = "avistagens",
                     dados = dados_banco$avistagens,
                     estrutura = estrutura_avistagens)

criar_inserir_tabela(tabela_nome = "pausas",
                     dados = dados_banco$pausas,
                     estrutura = estrutura_pausas)

criar_inserir_tabela("identificacoes", dados_banco$identificacoes)
criar_inserir_tabela("wps", dados_banco$wps)
criar_inserir_tabela("rotas", dados_banco$rotas)
criar_inserir_tabela("sonda", dados_banco$sonda)
# criar_inserir_tabela("individuos", dados_banco$individuos)

dbDisconnect(con)
# Função para abrir todas abas do arquivo *.xls de campo e salvar com um banco de dados bruto
le_excel <- function(pasta_data) {

  