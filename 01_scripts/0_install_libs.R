rm(list = ls())

lista_funcs <- list.files(paste0(rprojroot::find_rstudio_root_file(), "/01_scripts"), "*.R", full.names = TRUE)[-1]

funcs <- suppressWarnings(unlist(lapply(lista_funcs, readLines)))

lib_lines <- funcs[regexpr("library", funcs)!=-1]

names_libs <- unique(gsub("*library\\(|\\)| ", "", lib_lines))

update.packages(names_libs)
install.packages(names_libs)