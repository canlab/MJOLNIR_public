calculate_latent <- function(latent_var){

  v_obs_var <- df_sem_estimate %>% filter(lhs==latent_var) %>% select(rhs) %>% as_vector()

  f_calculate_obs <- function(obs_var){

    res <- scale(data[,obs_var]) * (df_sem_estimate %>% filter(lhs==latent_var, rhs==obs_var) %>% select(est) %>% as_vector())
    return(res)
  }

  res <- map(v_obs_var, function(x){f_calculate_obs(x)}) %>% as_tibble %>% rowSums
  if(scaled) res <- res %>% scale
  return(res)
}
