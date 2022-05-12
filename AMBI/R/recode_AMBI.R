#' Recode AMBI survey's question
#' This function will clean the results to be sure that in case of abcense of answer the value will be replace by NA
#'
#' @param v_AMBI un vecteur numerique
#'
#' @return la fonction renvoie la moyenne d'un vecteur
#' @import tidyverse
#' @export

recode_AMBI <- function(v_AMBI){
  v_res <- ifelse(!(v_AMBI %in% c(1:5)), NA, v_AMBI)
  return(v_res)
}
