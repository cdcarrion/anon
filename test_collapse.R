#' Filtra transacciones, contruye variables
#'
#' @param ats Base de datos ATS
#'
#' @return
#' @export
#'
#' @examples
ats_preprocesing <- function(ats
                             #     ,
                             # catastro_especiales
){
  
  # browser()
  
  ats <- ats %>%
    dplyr::rename_all(.funs = ~ stringr::str_to_lower(.)) %>%
    dplyr::mutate(year = lubridate::ymd(fecha_emision_compro_yyyymmdd) %>%
                    lubridate::year(.))                                         ### EN SRI
  # dplyr::mutate(year = lubridate::ymd(fecha_emision_compro) %>%                  ### LOCAL
  #                 lubridate::year(.))
  
  nuevos <- c(
    "tax_base_12VAT",
    "tax_base_0VAT",
    "tax_base_noVAT",
    "VATamt",
    "VATret30",
    "VATret70",
    "VATret100",
    "VATret10",
    "VATret20"
  )
  
  anteriores <- c(
    "base_imponible_graba",
    "base_imponible_tari0",
    "base_no_grava_iva",
    "monto_iva",
    "monto_retencion_iva1",
    "monto_retencion_iva2",
    "valor_retencion_servicio100",
    "valor_retencion_bienes_10",
    "valor_retencion_bienes_20"
  )
  
  ats <- ats %>%
    dplyr::mutate_at(.vars = anteriores,
                     .funs = stringr::str_replace,
                     pattern =",",
                     replacement = ".") %>%
    dplyr::mutate_at(.vars = anteriores,
                     .funs = as.numeric) %>%
    dplyr::rename_at(.vars = anteriores,
                     .funs = ~nuevos)
  
  print("corregidas_numericas")
  
  
  ats <- ats %>%
    dplyr::rowwise() %>%
    dplyr::mutate(amt_trans = sum(tax_base_12VAT,
                                  tax_base_0VAT,
                                  tax_base_noVAT,
                                  na.rm = T),
                  amt_withheld = sum(VATret30,
                                     VATret70,
                                     VATret100,
                                     VATret10,
                                     VATret20,
                                     na.rm = T),
                  amt_withheld = tidyr::replace_na(amt_withheld,
                                                   replace = 0),
                  trans_witheld = dplyr::if_else(amt_withheld>0,
                                                 true = 1,
                                                 false = 0)) %>%
    dplyr::ungroup()
  
  ats <- ats %>%
    dplyr::mutate(
      VATamt = dplyr::if_else(is.na(VATamt),
                              0.0,
                              as.numeric(VATamt),
      ),
      
      VAT_calc = tax_base_12VAT*0.12,
      
      dif = VAT_calc - VATamt,
      
      tax_base_12VAT = dplyr::if_else(
        dplyr::between(dif,left = -1,right = 1),
        VATamt/0.12,
        tax_base_12VAT
      )
    )  %>%
    dplyr::arrange(desc(dif)) %>%
    dplyr::select(-dif)
  
  print("nuevas variables listas")
  
  
  ats <- ats %>%
    dplyr::filter(amt_trans > 0,
                  tax_base_0VAT < 1000000000,
                  !codigo_tipo_comproba %in%  c(4, 51, 47),
                  !identi_credito_tribu %in%  c(0, 8, 9)) %>%
    dplyr::rename(
      ruc_buyer = ruc_contrib_informan_an,
      ruc_seller = numero_documento_an                ## EN SRI
    )
  # dplyr::rename(
  #   ruc_buyer = an_ruc_contrib_informan,           ## LOCAL
  #   ruc_seller = an_numero_documento
  # )
  
  print("Fin procesamiento inicial")
  
  
  return(ats)
  
  
}



#' Reduce la base ATS que estÃÂ  a nivel de transacciÃÂ²n, a una base
#' a nivel de relaciÃÂ²n comercial
#'
#' @param ats_completo a nivel de transacciÃÂ³n
#'
#' @return
#' @export
#'
#' @examples
collapse_ats <- function(ats_completo){
  
  
  
  syms_0 <- c( "tax_base_12VAT",
               "tax_base_0VAT",
               "tax_base_noVAT",
               "VATamt",
               "VATret30",
               "VATret70",
               "VATret100",
               "VATret10",
               "VATret20",
               "amt_trans",
               "amt_withheld",
               "VATamt",
               "VAT_calc",
               "tax_base_12VAT") %>%
    rlang::enexprs()
  
  
  ats <- ats_completo %>%
    dplyr::group_by_at(.vars = c("year","ruc_buyer","ruc_seller"))
  
  if(!"trans" %in% names(ats)){
    
    ats <- ats %>%
      dplyr::summarise(dplyr::across(!!!syms_0,
                                     .fns = list(~sum(.x,na.rm = T)),.names = "{.col}"),
                       # dplyr::across(!!!syms_1,
                       #               .fns = list(~unique(.x)),.names = "{.col}"),
                       trans = dplyr::n(),
                       trans_witheld = sum(trans_witheld,na.rm = T)
      )
  }else{
    ats <- ats %>%
      dplyr::summarise(dplyr::across(!!!syms_0,
                                     .fns = list(~sum(.x,na.rm = T)),.names = "{.col}"),
                       # dplyr::across(!!!syms_1,
                       #               .fns = list(~unique(.x)),.names = "{.col}"),
                       trans = sum(trans,na.rm = T),
                       trans_witheld = sum(trans_witheld,na.rm = T)
      )
  }
  
  ats <- ats %>%
    dplyr::ungroup()
  
  # browser()
  print("collapse listo")
  
  return(ats)
  
}