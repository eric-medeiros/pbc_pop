library(RSQLite)
library(DBI)

source("01_scripts/sub_WP.R")

# Conecte-se ao banco de dados
con <- dbConnect(RSQLite::SQLite(), dbname = "03_export/BancoFotoID.db")

# Criação do SQL para criar a tabela no banco de dados
sql <- "CREATE TABLE IF NOT EXISTS tabela_exemplo (
  saida VARCHAR(10) PRIMARY KEY,
  data DATE,
  barco VARCHAR(10),
  WP_I VARCHAR(10),
  datahora_I TIMESTAMP,
  lng_I DOUBLE PRECISION,
  lat_I DOUBLE PRECISION,
  WP_F VARCHAR(10),
  datahora_F TIMESTAMP,
  lng_F DOUBLE PRECISION,
  lat_F DOUBLE PRECISION,
  rota VARCHAR(10),
  equipe VARCHAR(50),
  barqueiro VARCHAR(50),
  litros_consumidos NUMERIC,
  obs VARCHAR(100)
);"

# Executar o comando SQL para criar a tabela
dbExecute(con, sql)

# Desconectar do banco de dados
dbDisconnect(con)

src_dbi(con)
