# Função geral de leitura de arquivos múltiplos de sonda
le_sonda <- function(pasta_data) {
  library(abind)
  library(purrr)
  library(data.table)
  library(tidyr)
  library(dplyr)
  
  # Função de leitura de arquivo indivudual *.xls da sonda
  le_sonda_arquivo <- function(arquivo_sonda) {
    library(readxl)
    library(stringr)
    library(data.table)
    library(lubridate)
    
    tem_dados_sonda <- ncol(read_xls(arquivo_sonda, sheet = 2, range = "H1:H1")) != 0
    
    if (tem_dados_sonda) {
      
      sonda_bruto <- as.data.frame(read_xls(arquivo_sonda,
                                            sheet = excel_sheets(arquivo_sonda)[2],
                                            col_types = c( "date", "date", "text", "text", "text",
                                                           "text", "text", "text", "text", "text", 
                                                           "text", "text", "text", "text", "text",
                                                           "text", "text", "text", "text", "text")))     
      sonda_sel <- sonda_bruto %>%
        select(1, 2, 3, 11, 15, 16, 4, 13, 18, 17) %>%
        rename(Temp = "Temp.[°C]",
               Sal = "Sal.[psu]",
               OD = "D.O.[ppm]",
               Turb = "Turb.FNU",
               Pres = "Press.[psi]",
               lng = "GPS Long.",
               lat = "GPS Lat.") %>%
        mutate(Date = ymd(Date),
               Time = str_sub(as.character(Time), 12, 19),
               datahora_SONDA = as.character.Date(ymd_hms(paste(Date, Time)) + hours(3)) %>% ymd_hms() %>% suppressWarnings(),
               Temp = round(as.double(Temp), 2) %>% suppressWarnings(),
               Sal = round(as.double(Sal), 2) %>% suppressWarnings(),
               OD = round(as.double(OD), 2) %>% suppressWarnings(),
               Turb = round(as.double(Turb), 2) %>% suppressWarnings(),
               pH = round(as.double(pH), 2) %>% suppressWarnings(),
               Pres = round(as.double(Pres), 3) %>% suppressWarnings(),
               lng = as.double(str_c("-",str_sub(lng, 1, 8), sep = "")) %>% suppressWarnings(),
               lat = as.double(str_c("-",str_sub(lat, 1, 8), sep = "")) %>% suppressWarnings(),
               saida = as.character(as.integer(str_sub(arquivo_sonda, -36,-34)))) %>%
        select(12,11,3:10) %>%
        as_tibble()
      

    } else {
      sonda_bruto <- as.data.frame(read_xls(arquivo_sonda,
                                            sheet = excel_sheets(arquivo_sonda)[2],
                                            col_types = c( "date", "date", "text", "text", "text",
                                                           "text", "text")))
      sonda_sel <- sonda_bruto %>%
        select(1:5) %>%
        rename(Pres = "Press.[psi]",
               lng = "GPS Lat.",
               lat = "GPS Long.") %>%
        mutate(Date = ymd(Date),
               Time = str_sub(as.character(Time), 12, 19),
               datahora_SONDA = as.character.Date(ymd_hms(paste(Date, Time)) + hours(3)) %>% ymd_hms() %>% suppressWarnings(),
               Temp = as.double(NA) %>% suppressWarnings(),
               Sal = as.double(NA) %>% suppressWarnings(),
               OD = as.double(NA) %>% suppressWarnings(),
               Turb = as.double(NA) %>% suppressWarnings(),
               pH = as.double(NA) %>% suppressWarnings(),
               Pres = round(as.double(Pres), 3) %>% suppressWarnings(),
               lng = as.double(str_c("-",str_sub(lng, 1, 8), sep = "")) %>% suppressWarnings(),
               lat = as.double(str_c("-",str_sub(lat, 1, 8), sep = "")) %>% suppressWarnings(),
               saida = as.character(as.integer(str_sub(arquivo_sonda, -36,-34)))) %>%
        select(12,6,3,7:11,4,5) %>%
        as_tibble()
    }
         

    # Muito outlier por entrar e sair da agua
    sonda_sel <- sonda_sel %>%
      filter(!Temp %in% boxplot(Temp, plot = FALSE)$out,
             !Sal %in% boxplot(Sal, plot = FALSE)$out,
             !OD %in% boxplot(OD, plot = FALSE)$out,
             !Turb %in% boxplot(Turb, plot = FALSE)$out,
             !pH %in% boxplot(pH, plot = FALSE)$out)
    
#    sonda_sel_dt <- data.table(sonda_sel)
    
    return(sonda_sel)
  }
  
  lista_arquivos_sonda <- list.files(paste(pasta_data, "/01_CAMPO/04_SONDA", sep = ""),
                                     full.names = TRUE, 
                                     recursive = TRUE,
                                     pattern = "xls$")
  
  dados_sonda <- lista_arquivos_sonda %>% map_dfr(le_sonda_arquivo)
  
  invisible(dados_sonda)
  
  return(dados_sonda)
}

