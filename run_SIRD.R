####### Arguments ##########
#
# 1) df: A dataframe for a specific country (or region) with, at least, one 'date' column, one 'confirmed' column with cumulative cases, and one 'deaths' column with cumulative deaths
# 2) size_population: Population of the region 
# 3) minimum_number_cases: Determines how many days will be considered. Default is to remove days before the 50th case. 
# 4) kc: A constant factor of subnotification for confirmed cases. kc = 0.1 means that each day has, in fact, 10 times more cases. 
# 5) kd: A constant factor of subnotification for deaths.
# 6) kr: A constant factor of subnotification for recovered.
# 7) power_H: Determines the bandwidth for the Kernel, as in H = n^power_H. Default is H = n^0.4.
# 8) recovered_synthetic: If TRUE, recovered are created as R(t) = C(t-14) - D(t). If FALSE, recovered comes from a 'recovered' column in df.
# 9) remove_last: Default is to remove the last 6 days from Rt and from the time-varying parameters, in order to have more stable results. 

####### Returns #########
#
# A list with Rt, the time-varying parameters, C, S, I, R, D and its deltas.

run_SIRD <- function(df, size_population, minimum_number_cases = 50, kc = 1, kd = 1, kr = 1, power_H = 0.4, 
                     recovered_synthetic = TRUE, remove_last = 6) {
  
  N <- size_population
  
  if(!all(c("date", "confirmed", "deaths") %in% colnames(df))) {
    stop("df must have columns 'date', 'confirmed' and 'deaths'")
  }
  
  # kc and kd < 1 for subnotification
  df <- df %>% dplyr::mutate(
    confirmed = confirmed/kc,
    deaths = deaths/kd
  )
  
  if(!recovered_synthetic) {
    if(!"recovered" %in% colnames(df)) {
      stop("If the recovered column is not to be created, a 'recovered' column is needed in the df")
    }
    df <- df %>% dplyr::mutate(
      recovered = recovered/kr
    )
  } else { # If synthetic..
    df <- df %>% dplyr::mutate(recovered = dplyr::lag(confirmed, 14) - deaths,
                               recovered = ifelse(is.na(recovered), 0, recovered))
    if(kr != 1) warning("Since using synthetic recovered, kr is being ignored")
  }
  
  # Filtering: minimum number of cases (does not consider subnotification)
  df <- df %>% dplyr::mutate(original_confirmed = confirmed * kc) %>% 
    dplyr::filter(original_confirmed >= minimum_number_cases) %>% dplyr::select(-original_confirmed)
  
  # n is nrow()-1 since the last day is only used to calculate the deltas t+1
  n <- nrow(df) - 1
  
  H <- n^power_H
  
  C <- df %>% dplyr::pull(confirmed)
  D <- df %>% dplyr::pull(deaths)
  R <- df %>% dplyr::pull(recovered)
  
  S <- N - C # Susceptible = population - confirmed
  I <- C - D - R # Infectious = confirmed - deaths - recovered
  
  deltaC <- diff(C) %>% set_names(df$date[-1])
  deltaD <- diff(D) %>% set_names(df$date[-1])
  deltaS <- diff(S) %>% set_names(df$date[-1])
  deltaR <- diff(R) %>% set_names(df$date[-1])
  deltaI <- diff(I) %>% set_names(df$date[-1])
  
  # Removes last day, since it was already used to compute deltas t+1
  C <- C[1:n] %>% set_names(df$date[1:n])
  D <- D[1:n] %>% set_names(df$date[1:n])
  S <- S[1:n] %>% set_names(df$date[1:n])
  R <- R[1:n] %>% set_names(df$date[1:n])
  I <- I[1:n] %>% set_names(df$date[1:n])
  
  mu_hat <- estimator_calculation(n, n, x = rep(1, n), y = deltaD/I, H) %>% set_names(df$date[1:n])
  nu_hat <- estimator_calculation(n, n, x = rep(1, n), y = deltaR/I, H) %>% set_names(df$date[1:n])
  beta_hat <- - estimator_calculation(n, n, x = rep(1, n), y = deltaS/(I * S/N), H) %>% set_names(df$date[1:n])
  
  beta_hat <- ifelse(beta_hat < 0, 0, beta_hat) # Beta is non negative
  
  # Regularization in mu_hat and nu_hat, to get them closer to 1/14 * 0.06 and 1/14 * 0.94, respectively
  mu_fixed <- 1/14 * 0.06
  nu_fixed <- 1/14 * 0.94
  mu_hat <- pmax(mu_hat, 1/21 * 0.06)
  nu_hat <- pmax(nu_hat, 1/28 * 0.94)
  mu_hat <- pmin(mu_hat, 1/7 * 0.06)
  nu_hat <- pmin(nu_hat, 1/7 * 0.94)
  mu_hat <- 0.75 * mu_fixed + 0.25 * mu_hat
  nu_hat <- 0.75 * nu_fixed + 0.25 * nu_hat
  
  Rt <- beta_hat / (mu_hat + nu_hat) * S/N
  
  # Removing last days from nu_hat, mu_hat, beta_hat and Rt. They are more unstable.
  nu_hat <- nu_hat[1:(n - remove_last)]
  mu_hat <- mu_hat[1:(n - remove_last)]
  beta_hat <- beta_hat[1:(n - remove_last)]
  Rt <- Rt[1:(n - remove_last)]  
  
  return(
    list(C = C, D = D, S = S, R = R, I = I, 
         deltaC = deltaC, deltaD = deltaD, deltaS = deltaS, deltaR = deltaR, deltaI = deltaI,
         mu_hat = mu_hat, nu_hat = nu_hat, beta_hat = beta_hat, 
         Rt = Rt)
  )
}
