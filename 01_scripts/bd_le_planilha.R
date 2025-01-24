bd_le_planilha <- function(pasta_data) {
  suppressPackageStartupMessages({
    require(readxl)
    require(dplyr)
    require(lubridate)
    require(stringr)
  })
  
  arquivo_excel <- file.path(pasta_data, "01_CAMPO", "02_EXCEL", "populacional_PBC.xlsx")
  
  result <- 
    list(
      "saidas" =
        read_excel(
          arquivo_excel,
          sheet = "saidas",
          col_types = c("text", "date", "text", "text", "text", "text", "text", "text", "text", "text")
        ) %>%
        mutate(
          data = ymd(data),
          across(matches("^wp_"), ~ str_pad(., 3, pad = "0"))),
      
      "amostragens" =
        read_excel(
          arquivo_excel,
          sheet = "amostragens",
          col_types = c("text", "date", "text", "text", "text", "text", "text")
        ) %>%
        mutate(
          data = ymd(data),
          across(matches("^wp_"), ~ str_pad(., 3, pad = "0"))),
      
      "climas" =
        read_excel(
          arquivo_excel,
          sheet = "climas",
          col_types = c("text", "date", "text", "text", "text", "text", "text", "text", "text", "text", "text")
        ) %>%
        mutate(
          data = ymd(data),
          across(matches("^wp_"), ~ str_pad(., 3, pad = "0"))),
      
      "avistagens" =
        read_excel(
          arquivo_excel,
          sheet = "avistagens",
          col_types = c("text", "date", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text")
        ) %>%
        mutate(
          data = ymd(data),
          across(matches("^n_"), ~ as.integer(.)) %>% suppressWarnings(),
          across(matches("^wp_"), ~ str_pad(., 3, pad = "0"))),
      
      "pausas" =
        read_excel(
          arquivo_excel,
          sheet = "pausas",
          col_types = c("text", "date", "text", "text", "text")
        ) %>%
        mutate(
          data = ymd(data),
          across(matches("^wp_"), ~ str_pad(., 3, pad = "0"))),
      
      "wps_extra" =
        read_excel(
          arquivo_excel,
          sheet = "wps_extra",
          col_types = c("text", "date", "date", "text", "text", "text", "text")
        ) %>%
        mutate(
          data = ymd(data),
          datahora_extra = ymd_hm(paste(data, str_sub(hora_extra, 12, 16)), tz = Sys.timezone()),
          hora_extra = NULL,
          across(matches("^wp_"), ~ str_pad(., 3, pad = "0"))),
      
      "identificacoes" =
        read_excel(
          arquivo_excel,
          sheet = "identificacoes",
          col_types = c("text", "date", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text"),
          skip = 3
        ) %>%
        mutate(
          data = ymd(data),
          filhote_ac = filhote_ac == "V"),
      
      "individuos" =
        read_excel(
          arquivo_excel,
          sheet = "individuos",
          col_types = c("text", "text", "text", "text", "text", "text", "skip")),
      
      "caminhos" = tibble(
        tipo = "planilha",
        id = list(1L),
        arquivo = basename(arquivo_excel),
        pasta = dirname(arquivo_excel))
    )
  
  cat("-> OK - leitura da planilha\n")
  
  return(result)
  
}