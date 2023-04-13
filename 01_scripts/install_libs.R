rm(list = ls())

lista_funcs <- list.files(paste0(rprojroot::find_rstudio_root_file(), "/01_scripts"), "*.R", full.names = TRUE)

funcs <- suppressWarnings(unlist(lapply(lista_funcs, readLines)))

lines_libs <- funcs[regexpr("library", funcs)!=-1]

names_libs <- gsub("*library\\(", "", lines_libs)
names_libs <- gsub("\\)", "", names_libs) 
names_libs <- gsub(" ", "", names_libs) 

install.packages(unique(names_libs))