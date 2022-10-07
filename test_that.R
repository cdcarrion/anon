test_that("anonimizacion tabla works", {
  
  
  # Insumos test 1: función anonimizacion_tabla -----------------------------
  
  
  tabla_datos <- tibble::tibble(identificador = sample(letters,
                                                       size = 100,
                                                       replace = T),
                                numerico = rnorm(100))
  
  tabla_central <- tibble::tibble(id_real = sample(letters),
                                  id_anon = sample(LETTERS))
  
  result <- anonimizacion_tabla(tabla_datos = tabla_datos ,
                                tabla_central = tabla_central ,
                                td_id = "identificador" ,
                                tc_id = list(id_real = "id_real",
                                             anon = "id_anon") )
  
  # Test resultado:
  
  expect_equal(
    object = names(result),expected = c("identificador","identificador_an","numerico")
  )
  
  
  
  # Test atributos:
  
  
  atr <- attributes(result)
  
  
  expect_equal(object = names(atr),
               expected = c("names"  ,
                            "row.names",
                            "class" ,
                            "resumen"))
  
  
  
  
  # Insumos test 2: funciÃÂ³n anon_dos_fuentes --------------------------------
  
  
  
  
  set.seed(1984)
  
  unicos <- purrr::rerun(.n = 2,sample(letters,
                                       size = 10,
                                       replace = T)) %>%
    purrr::reduce(stringr::str_c) %>%
    unique()
  
  unicos_2 <- purrr::rerun(.n = 2,sample(LETTERS,
                                         size = 10,
                                         replace = T)) %>%
    purrr::reduce(stringr::str_c) %>%
    unique()
  
  tabla_datos_2 <- rbind(tabla_datos,
                         tibble::tibble(
                           identificador = sample(unicos,size = 50,replace = T),
                           numerico = rbeta(n = 50,shape1 = 10,shape2 = 10)))
  
  tabla_central_2 <- tibble::tibble(a_id_real = unicos,
                                    b_id_anon = unicos_2)
  
  elementos <- anon_dos_fuentes(tabla_datos = tabla_datos_2,
                                tabla_central = tabla_central ,
                                td_id = "identificador" ,
                                tc_id = list(id_real = "id_real",
                                             anon = "id_anon"),
                                tabla_central_2 = tabla_central_2,
                                tc_id_2 =  list(id_real = "a_id_real",
                                                anon = "b_id_anon"))
  
  # Test de los atrbutos:
  
  atr_2 <- attributes(elementos)
  
  
  expect_equal(object = names(atr_2),
               expected = c("names"  ,
                            "row.names",
                            "class" ,
                            "resumen"))
  
  
  # Insumos test 3: funciÃÂ³n aplicacion de anonimizacion ---------------------
  
  
  elementos_2 <- purrr::invoke_map(.f =
                                     list(function(x){x},
                                          function(x){x %>%
                                              dplyr::rename_all(~stringr::str_c(.,"2")) %>%
                                              dplyr::arrange(identificador2,identificador2)
                                          }),
                                   .x = purrr::rerun(list(tabla_datos_2),.n = 2)) %>%
    purrr::reduce(dplyr::bind_cols)
  
  
  elementos_2_1 <- aplicacion_anonimizacion(tabla_datos = elementos_2,
                                            tabla_central = tabla_central ,
                                            td_id = "identificador" ,
                                            tc_id = list(id_real = "id_real",
                                                         anon = "id_anon"),
                                            
                                            indicador = 1)
  
  
  elementos_2_2 <- aplicacion_anonimizacion(tabla_datos = elementos_2,
                                            tabla_central = tabla_central ,
                                            tabla_central_2 = tabla_central_2 ,
                                            td_id = "identificador" ,
                                            tc_id = list(id_real = "id_real",
                                                         anon = "id_anon"),
                                            tc_id_2 = list(id_real = "a_id_real",
                                                           anon = "b_id_anon"),
                                            indicador = 1)
  
  elementos_2_3<- aplicacion_anonimizacion(tabla_datos = elementos_2,
                                           tabla_central = tabla_central ,
                                           td_id = "identificador" ,
                                           td_id_2 = "identificador2" ,
                                           tc_id = list(id_real = "id_real",
                                                        anon = "id_anon"),
                                           tabla_central_2 = tabla_central_2,
                                           tc_id_2 =  list(id_real = "a_id_real",
                                                           anon = "b_id_anon"),
                                           indicador = 2)
  
  elementos_2_4 <- aplicacion_anonimizacion(tabla_datos = elementos_2,
                                            tabla_central = tabla_central ,
                                            td_id = "identificador" ,
                                            td_id_2 = "identificador2" ,
                                            tc_id = list(id_real = "id_real",
                                                         anon = "id_anon"),
                                            
                                            indicador = 2)
  
  
  elementos_2_5 <- aplicacion_anonimizacion(tabla_datos = elementos_2,
                                            tabla_central = tabla_central_2 ,
                                            td_id = "identificador" ,
                                            td_id_2 = "identificador2" ,
                                            tc_id = list(id_real = "a_id_real",
                                                         anon = "b_id_anon"),
                                            
                                            indicador = 2)
  
  
  expect_equal(
    object = names(elementos_2_1),expected = c('identificador_an','identificador2','numerico','numerico2')
  )
  
  expect_equal(
    object = names(elementos_2_2),expected = c('identificador_an','identificador2','numerico','numerico2')
  )
  
  expect_equal(
    object = names(elementos_2_3),expected = c('identificador_an','identificador2_an','numerico','numerico2')
  )
  
  expect_equal(
    object = names(elementos_2_4),expected = c('identificador2_an','identificador_an','numerico','numerico2')
  )
  
  expect_equal(
    object = names(elementos_2_5),expected = c('identificador2_an','identificador_an','numerico','numerico2')
  )
})