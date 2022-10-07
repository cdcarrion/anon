#' Title
#'
#' @param tabla_datos Tabla de datos para ser anonimizada
#' @param tabla_central Tabla con los identificadores para anonimizar. Esta tabla mÃÂ­nimo debe tener dos columnas: una con
#' el *identificador real* y otra con el cÃÂ³digo de anonimizaciÃÂ³n
#' @param td_id Texto en el que se indica el nombre de la variable presente en `tabla_datos` que contiene el
#' * identificador real*
#' @param tc_id Lista con dos elemtos:
#' * `id_real`: Texto en el que se indica el nombre de la variable presente en `tabla_central` que contiene el
#' * identificador real*
#' * `anon`: Texto en el que se indica el nombre de la variable presente en `tabla_central` que contiene el
#' * identificador anonimo*
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#' anonimizacion_tabla(tabla_datos,tabla_central,td_id,tc_id)
#'
#' }
anonimizacion_tabla <- function(tabla_datos,
                                tabla_central,
                                td_id,
                                tc_id){
  
  # browser()
  
  tc_id <- c(tc_id$id_real,
             tc_id$anon)
  
  
  id_anon_f <- stringr::str_c(td_id,"_an")
  
  name_identificador <- stringr::str_detect(string = names(tabla_datos),
                                            pattern = id_anon_f) %>% sum
  
  if(name_identificador > 0){
    
    id_anon_f <- stringr::str_c(id_anon_f,"_1")
    
  }
  
  tabla_central <- tabla_central %>%
    dplyr::mutate_at(.vars = tc_id[1],
                     .funs = list(~dplyr::if_else(nchar(.) %in% c(9,12),
                                                  stringr::str_c("0",.),
                                                  as.character(.)))) %>%
    dplyr::rename_at(.vars = tc_id,
                     .funs = ~c("id","anon")) %>%
    dplyr::select(id,anon)
  
  # browser()
  
  tabla_datos <- tabla_datos %>%
    dplyr::mutate_at(.vars = td_id,
                     .funs = list(~dplyr::if_else(nchar(.) %in% c(9,12),
                                                  stringr::str_c("0",.),
                                                  as.character(.)))) %>%
    dplyr::rename_at(.vars = td_id,
                     .funs = ~"id") %>%
    dplyr::left_join(tabla_central,
                     by =  "id") %>%
    dplyr::rename_at(.vars = c("id","anon"),
                     .funs = ~c(td_id,
                                id_anon_f)) %>%
    dplyr::select(dplyr::matches(match = td_id),
                  dplyr::everything())
  
  
  
  attr(tabla_datos,"resumen") <- tabla_datos %>%
    dplyr::transmute_at(.vars = id_anon_f,
                        .funs = is.na) %>%
    dplyr::group_by_at(.vars = id_anon_f) %>%
    dplyr::tally()
  
  return(tabla_datos)
}


#' Title
#'
#' @param tabla_datos Tabla de datos para ser anonimizada
#' @param tabla_central Tabla con los identificadores para anonimizar. Esta tabla mÃÂ­nimo debe tener dos columnas: una con
#' el *identificador real* y otra con el cÃÂ³digo de anonimizaciÃÂ³n
#' @param td_id Texto en el que se indica el nombre de la variable presente en `tabla_datos` que contiene el
#' * identificador real*
#' @param tc_id Lista con dos elemtos:
#' * `id_real`: Texto en el que se indica el nombre de la variable presente en `tabla_central` que contiene el
#' * identificador real*
#' * `anon`: Texto en el que se indica el nombre de la variable presente en `tabla_central` que contiene el
#' * identificador anonimo*
#' @param tabla_central_2 Tabla con los identificadores para anonimizar. Esta tabla mÃÂ­nimo debe tener dos columnas: una con
#' el *identificador real* y otra con el cÃÂ³digo de anonimizaciÃÂ³n
#' @param tc_id_2 Lista con dos elemtos:
#' * `id_real`: Texto en el que se indica el nombre de la variable presente en `tabla_central_2` que contiene el
#' * identificador real*
#' * `anon`: Texto en el que se indica el nombre de la variable presente en `tabla_central_2` que contiene el
#'
#' @return
#' @export
#'
#' @examples
anon_dos_fuentes <- function(tabla_datos,
                             td_id,
                             tabla_central,
                             tc_id,
                             tabla_central_2,
                             tc_id_2){
  
  
  if("resumen" %in% names(attributes(tabla_datos))){
    resumen_0 <- attr(tabla_datos,"resumen")
  }else{
    resumen_0 <- NULL
  }
  
  tabla_anonim <- purrr::reduce2(
    .x = list(tabla_central = tabla_central,
              tabla_central = tabla_central_2),
    .y = list(tc_id = tc_id,
              tc_id = tc_id_2)
    ,.f = anonimizacion_tabla,
    .init = tabla_datos,
    td_id = td_id)
  
  
  
  reg <- stringr::str_c(td_id,"_an.{0,2}$")
  
  id_anons <- stringr::str_subset(names(tabla_anonim),pattern = reg)
  
  
  tabla_anonim <- tabla_anonim %>%
    dplyr::rename_at(.vars = id_anons,
                     .f = ~c("alt_1","alt_2"))
  
  name_anon <- stringr::str_c(td_id,"_an")
  
  tabla_anonim[[name_anon]] <- tabla_anonim[[td_id]]
  
  tabla_anonim <- tabla_anonim %>%
    dplyr::mutate_at(.vars = name_anon,
                     list(~dplyr::case_when(
                       is.na(alt_1) & !is.na(alt_2) ~ alt_2,
                       !is.na(alt_1) & is.na(alt_2) ~ alt_1,
                       is.na(alt_1) & is.na(alt_2)  ~ .,
                       TRUE ~ NA_character_
                     )))
  
  if(!is.null(resumen_0)){
    resumen <- list(
      resumen_0,
      tabla_anonim %>%
        dplyr::transmute_at(.vars = c("alt_1","alt_2"),
                            .funs = is.na) %>%
        dplyr::group_by_at(.vars = c("alt_1","alt_2")) %>%
        dplyr::tally()
      
    )
    
  }else{
    resumen <-
      tabla_anonim %>%
      dplyr::transmute_at(.vars = c("alt_1","alt_2"),
                          .funs = is.na) %>%
      dplyr::group_by_at(.vars = c("alt_1","alt_2")) %>%
      dplyr::tally()
    
    
  }
  
  attr(tabla_anonim,"resumen") <- resumen
  
  
  tabla_anonim <- tabla_anonim  %>%
    dplyr::select(-alt_1,-alt_2)
  
  return(tabla_anonim)
  
}


#' Title
#'
#' @param tabla_datos
#' @param td_id
#' @param td_id_2
#' @param tabla_central
#' @param tc_id
#' @param tabla_central_2
#' @param tc_id_2
#' @param indicador
#'
#' @return
#' @export
#'
#' @examples
aplicacion_anonimizacion <- function(tabla_datos = NULL,
                                     td_id = NULL,
                                     tabla_central = NULL,
                                     tc_id = NULL,
                                     indicador = c(1,2),
                                     td_id_2 = NULL,
                                     tabla_central_2 = NULL,
                                     tc_id_2 = NULL){
  
  
  
  # browser()
  if(indicador == 2 & !is.null(tabla_central_2)){
    anon_2 <- purrr::reduce(
      .x = list(
        td_id = td_id,
        td_id = td_id_2
      ),
      .f = anon_dos_fuentes,
      .init = tabla_datos,
      tabla_central = tabla_central,
      tc_id = tc_id,
      tabla_central_2 = tabla_central_2,
      tc_id_2 = tc_id_2)
    
    anon_2 <- anon_2 %>%
      dplyr::select(-dplyr::matches(stringr::str_c(td_id,"$")),
                    -dplyr::matches(stringr::str_c(td_id_2,"$"))) %>%
      dplyr::select(dplyr::matches("_an$"),
                    dplyr::everything())
    
  }else if(indicador == 2 & is.null(tabla_central_2)){
    anon_2 <- purrr::reduce(
      .x = list(
        td_id = td_id,
        td_id = td_id_2
      ),
      .f = anonimizacion_tabla,
      .init = tabla_datos,
      tabla_central = tabla_central,
      tc_id = tc_id)
    
    anon_2 <- anon_2 %>%
      dplyr::select(-dplyr::matches(stringr::str_c(td_id,"$")),
                    -dplyr::matches(stringr::str_c(td_id_2,"$"))) %>%
      dplyr::select(dplyr::matches("_an$"),
                    dplyr::everything())
    
  }else if(indicador == 1 & is.null(tabla_central_2)){
    anon_2 <-
      anonimizacion_tabla(
        td_id = td_id,
        tabla_datos= tabla_datos,
        tabla_central = tabla_central,
        tc_id = tc_id)
    
    anon_2 <- anon_2 %>%
      dplyr::select(-dplyr::matches(stringr::str_c(td_id,"$"))) %>%
      dplyr::select(dplyr::matches("_an$"),
                    dplyr::everything())
    
  }else if(indicador == 1 & !is.null(tabla_central_2)){
    anon_2 <-
      anon_dos_fuentes(
        td_id = td_id,
        tabla_datos= tabla_datos,
        tabla_central = tabla_central,
        tc_id = tc_id,
        tabla_central_2 = tabla_central_2,
        tc_id_2 = tc_id_2)
    
    anon_2 <- anon_2 %>%
      dplyr::select(-dplyr::matches(stringr::str_c(td_id,"$"))) %>%
      dplyr::select(dplyr::matches("_an$"),
                    dplyr::everything())
    
  }
  
  
  # browser()
  
  return(anon_2)
  
}


#' Funcion por chinks
#'
#' @param path Link
#' @param tabla_central Central  1
#' @param tabla_central_2  Central 2
#' @param tc_id
#' @param tc_id_2
#' @param td_id
#' @param td_id_2
#' @param indicador
#' @param lineas
#'
#' @return
#' @export
#'
#' @examples
lectura_chunk_funcion <- function(path,
                                  tabla_central = NULL,
                                  tabla_central_2 = NULL,
                                  tc_id = NULL,
                                  tc_id_2 = NULL,
                                  td_id = NULL,
                                  td_id_2 = NULL,
                                  indicador = NULL,
                                  lineas = NULL){
  
  
  indice <- 0
  
  lineas <- lineas
  
  con <- file(description=path,open="r")
  
  salida <- stringr::str_replace(path,pattern = ".txt$","_an.txt")
  
  
  init <- tictoc::tic()
  
  tramo <- read.table(con,
                      nrows = lineas,
                      header = TRUE,
                      fill=TRUE,
                      sep="\t")
  
  fin <- tictoc::toc()
  
  
  nombres_tabla <- names(tramo)
  
  # browser()
  
  
  init2 <- tictoc::tic()
  
  tramo <- aplicacion_anonimizacion(tabla_datos = tibble::as_tibble(tramo),
                                    tabla_central = tabla_central,
                                    tabla_central_2 = tabla_central_2,
                                    tc_id = tc_id,
                                    tc_id_2 = tc_id_2,
                                    td_id = td_id,
                                    td_id_2 = td_id_2,
                                    indicador = indicador)
  
  fin2 <- tictoc::toc()
  
  res <- attr(tramo,"resumen")
  
  
  init2_1 <- tictoc::tic()
  
  readr::write_delim(x = tramo,delim = "\t",file = salida)
  
  fin2_1 <- tictoc::toc()
  
  rendimiento <- tibble::tibble(
    iteracion = indice,
    lectura = fin$toc -fin$tic,
    anonimizacion = fin2$toc -fin2$tic,
    escritura = fin2_1$toc -fin2_1$tic,
  )
  
  repeat {
    
    indice <- indice + 1
    
    print(paste('Processing rows:', indice * lineas))
    
    if (nrow(tramo) != lineas){
      
      print('Processed all files!')
      
      break
      
    }
    
    
    init3 <- tictoc::tic()
    
    gc()
    
    tramo <- read.table(con,
                        nrows=lineas,
                        skip = 0,
                        header = FALSE,
                        fill = TRUE,
                        sep = "\t")
    
    fin3 <- tictoc::toc()
    
    names(tramo) <- nombres_tabla
    
    
    init4 <- tictoc::tic()
    
    tramo <- aplicacion_anonimizacion(tabla_datos = tibble::as_tibble(tramo),
                                      tabla_central = tabla_central,
                                      tabla_central_2 = tabla_central_2,
                                      tc_id = tc_id,
                                      tc_id_2 = tc_id_2,
                                      td_id = td_id,
                                      td_id_2 = td_id_2,
                                      indicador = indicador)
    
    fin4 <- tictoc::toc()
    
    res <- list(res,attr(tramo,"resumen"))
    
    init5 <- tictoc::tic()
    
    readr::write_delim(x = tramo,delim = "\t",file = salida,append = T)
    
    fin5 <- tictoc::toc()
    
    
    rendimiento <-
      dplyr::bind_rows(
        rendimiento,
        tibble::tibble(
          iteracion = indice,
          lectura = fin3$toc -fin3$tic,
          anonimizacion = fin4$toc -fin4$tic,
          escritura = fin5$toc -fin5$tic,
        ))
    
    # browser()
    
    # if(indice > 10) break
  }
  
  close(con)
  
  rm(tramo)
  
  insumos <- list(rendimiento= rendimiento,
                  res= res)
  
  return(insumos)
}
