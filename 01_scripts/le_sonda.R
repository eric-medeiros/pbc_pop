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
    
    
    sonda_bruto <- as.data.frame(read_xls(arquivo_sonda,
                                          sheet = excel_sheets(arquivo_sonda)[2],
                                          col_types = c( "date", "date", "text", "text", "text",
                                                         "text", "text", "text", "text", "text", 
                                                         "text", "text", "text", "text", "text",
                                                         "text", "text", "text", "text", "text")
    ))
    
   
    sonda_sel <- sonda_bruto[, c(1, 2, 3, 11, 15, 16, 4, 13, 18, 17)]
    
    names(sonda_sel)[3] <- "Temp"
    names(sonda_sel)[4] <- "Sal"
    names(sonda_sel)[5] <- "OD"
    names(sonda_sel)[6] <- "Turb"
    names(sonda_sel)[8] <- "Pres"
    names(sonda_sel)[9] <- "lng"
    names(sonda_sel)[10] <- "lat"
    
    sonda_sel$Date <- ymd(sonda_sel$Date)
    sonda_sel$Time <- str_sub(as.character(sonda_sel$Time), 12, 19)
    sonda_sel$datahora_SONDA <- as.character.Date(ymd_hms(paste(sonda_sel$Date, sonda_sel$Time)) + hours(3))
    sonda_sel$Temp <- suppressWarnings(round(as.double(sonda_sel$Temp), 2))
    sonda_sel$Sal <- suppressWarnings(round(as.double(sonda_sel$Sal), 2))
    sonda_sel$OD <- suppressWarnings(round(as.double(sonda_sel$OD), 2))
    sonda_sel$Turb <- suppressWarnings(round(as.double(sonda_sel$Turb), 2))
    sonda_sel$pH <- suppressWarnings(round(as.double(sonda_sel$pH), 2))
    sonda_sel$Pres <- suppressWarnings(round(as.double(sonda_sel$Pres), 3))
    sonda_sel$lng <- suppressWarnings(as.double(str_c("-",str_sub(sonda_sel$lng, 1, 8), sep = "")))
    sonda_sel$lat <- suppressWarnings(as.double(str_c("-",str_sub(sonda_sel$lat, 1, 8), sep = "")))
    sonda_sel$saida <- as.character(as.integer(str_sub(arquivo_sonda, -36,-34)))
    
    sonda_sel <- sonda_sel[, c(12, 11, 3:8, 9, 10)]
    
    
    # Muito outlier por entrar e sair da agua
    out_temp <- boxplot(sonda_sel$Temp, plot = FALSE)$out
    sonda_sel <- sonda_sel[!(sonda_sel$Temp %in% out_temp), ]
    
    out_sal <- boxplot(sonda_sel$Sal, plot = FALSE)$out
    sonda_sel <- sonda_sel[!(sonda_sel$Sal %in% out_sal), ]
    sonda_sel$Sal <- round(as.numeric(sonda_sel$Sal), 2)
    
    out_do <- boxplot(sonda_sel$OD, plot = FALSE)$out
    sonda_sel <- sonda_sel[!(sonda_sel$OD %in% out_do), ]
    
    out_turb <- boxplot(sonda_sel$Turb, plot = FALSE)$out
    sonda_sel <- sonda_sel[!(sonda_sel$Turb %in% out_turb), ]
    
    out_ph <- boxplot(sonda_sel$pH, plot = FALSE)$out
    sonda_sel <- sonda_sel[!(sonda_sel$pH %in% out_ph), ]
    
    out_pres <- boxplot(sonda_sel$Pres, plot = FALSE)$out
    sonda_sel <- sonda_sel[!(sonda_sel$Pres %in% out_pres), ]
    
    sonda_sel_dt <- data.table(sonda_sel)
    
    return(sonda_sel_dt)
  }
  
  lista_arquivos_sonda <- list.files(paste(pasta_data, "/01_CAMPO/04_SONDA", sep = ""),
                                     full.names = TRUE, 
                                     recursive = TRUE,
                                     pattern = "xls$")
  
  dados_sonda <- lista_arquivos_sonda %>%
    map_dfr(le_sonda_arquivo, .id = "registro_SONDA") %>%
    group_by(registro_SONDA) %>% 
    na.omit() %>%
    suppressWarnings()
  
      
  invisible(dados_sonda)
  
  return(dados_sonda)
}

