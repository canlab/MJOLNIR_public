#' semscoring_latent
#' method to score latent variable found in sem. The method use the estimate to determine the importance of each variable on the latent variable.
#' @param fit sem object from the lavaan package
#' @param df dataframe on which the the sem was calculated
#' @param calculation calculation method, must be in c('normalisation', 'scale')
#'
#' @return a data frame with the score of the latent variable
#' @import lavaan
#' @import tidyverse
#' @export
semscoring_latent <- function(fit, df, calculation = 'normalisation'){

  df_sem_estimate <- fit %>% lavaan::partable() %>% as_tibble %>% filter(user==1)
  v_latent_var <- df_sem_estimate$lhs %>% as_factor %>% levels

  f_calculate_latent <- function(latent_var){

    v_obs_var <- df_sem_estimate %>% filter(lhs==latent_var) %>% select(rhs) %>% as_vector()

    f_calculate_obs <- function(obs_var){

      res <- scale(df[,obs_var]) * (df_sem_estimate %>% filter(lhs==latent_var, rhs==obs_var) %>% select(est) %>% as_vector())
      return(res)
    }

    res <- map(v_obs_var, function(x){f_calculate_obs(x)}) %>% as_tibble %>% rowSums
    if(!(calculation %in% c("scale","normalisation", ""))) stop("calculation method/semscoring method must be in c('', scale','normalisation')")
    if(calculation=="scale") res <- res %>% scale
    if(calculation=="normalisation") res <- res %>% f_normalisation()
    return(res)
  }

  res <- map(v_latent_var,function(x){f_calculate_latent(x)}) %>% as.data.frame
  colnames(res) <- v_latent_var
  return(res)
}
