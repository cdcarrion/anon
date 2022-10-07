full_join_with_id <- function(tabla_datos_x,
                              tabla_datos_y,
                              join_id = character()){
  
  
  tabla_datos <-  purrr::map2(.x = list(tabla_datos_x,
                                        tabla_datos_y),
                              .y = c("left","right"),
                              .f = ~ {
                                
                                dplyr::mutate(.x,merge_id = .y)
                                
                              }) %>%
    purrr::reduce(.f = dplyr::full_join,by = join_id)
  
  
  return(tabla_datos)
  
}