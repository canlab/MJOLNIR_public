f_calculate_obs <- function(obs_var){

  res <- scale(data[,obs_var]) * (df_sem_estimate %>% filter(lhs==latent_var, rhs==obs_var) %>% select(est) %>% as_vector())
  return(res)
}
