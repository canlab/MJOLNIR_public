#' f_normalisation
#' normalise a vector between 0 and 1 (it does not take into consideration standard-deviation)
#' @param v a numerical vector
#'
#' @return a normalized numerical vector
#' @export
#'
#' @examples
f_normalisation <- function(v){
  v <- v-min(v)
  v <- v/max(v)
  return(v)
}
