#' AMBI_measurecalculation_scale
#' Internal calculation used in calculate AMBI. Makes the sum of the items of a personality trait (reverse item are reversed before) and then scale them.
#' @param df data frame with our data cleaned.
#' @param v_pos vector of the items positively correlated with the personality trait we aim to calculate.
#' @param v_res vector of the items negatively correlated with the personality trait we aim to calculate.
#'
#' @return a vector of the result of the AMBI we wanted to calculate
#' @export
#'
#' @examples
AMBI_measurecalculation_scale <- function(df, v_pos, v_rev){
  df$NUL <- 0
  if(length(v_rev)>0&length(v_pos)>0){
      r <- (rowSums(df[,c(paste0("AMBI_",v_pos),"NUL")]) +
              (6*length(v_rev)-rowSums(df[,c(paste0("AMBI_",v_rev),"NUL")])))
    }else{
      if(length(v_rev)<1&length(v_pos)>0){
        r <- (rowSums(df[,c(paste0("AMBI_",v_pos),"NUL")]) )
      }else{
        r <- ((6*length(v_rev)-rowSums(df[,c(paste0("AMBI_",v_rev),"NUL")])))
      }
    }
  r <- scale(r)
  return(r)
}
