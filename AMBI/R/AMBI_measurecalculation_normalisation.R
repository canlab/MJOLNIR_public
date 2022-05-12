#' AMBI_measurecalculation_normalisation
#' Internal calculation used in calculate AMBI. Makes the sum of the items of a personality trait (reverse item are reversed before) and then normalize them between O and 1.
#' @param df data frame with our data cleaned.
#' @param v_pos vector of the items positively correlated with the personality trait we aim to calculate.
#' @param v_res vector of the items negatively correlated with the personality trait we aim to calculate.
#'
#' @return a vector of the result of the AMBI we wanted to calculate
#' @import tidyverse
#' @export
#'
#' @examples
AMBI_measurecalculation_normalisation <- function(df, v_pos, v_rev){
  df$NUL <- 0
    if(length(v_rev)>0&length(v_pos)>0){
      r <- (rowSums(df[,c(paste0("AMBI_",v_pos),"NUL")]) +
              (6*length(v_rev)-rowSums(df[,c(paste0("AMBI_",v_rev),"NUL")])) -
              (length(v_pos)+length(v_rev))) / (5*(length(v_pos)+length(v_rev)))
    }else{
      if(length(v_rev)<1&length(v_pos)>0){
        r <- (rowSums(df[,c(paste0("AMBI_",v_pos),"NUL")]) -
                (length(v_pos)+length(v_rev))) / (5*(length(v_pos)+length(v_rev)))
      }else{
        r <- ((6*length(v_rev)-rowSums(df[,c(paste0("AMBI_",v_rev),"NUL")])) -
                (length(v_pos)+length(v_rev))) / (5*(length(v_pos)+length(v_rev)))
      }
    }
  return(r)
}
