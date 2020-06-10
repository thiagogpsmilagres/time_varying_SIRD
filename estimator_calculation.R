# Estimator as in Giraitis, Kapetanios and Yates (JoE, 2014)

estimator_calculation <- function(t_range, k_range, x, y, H) {
  output <- rep(NA, t_range)
  for(t in 1:t_range) {
    output_num <- 0
    output_den <- 0
    for(k in 1:k_range) {
      output_num <- output_num + gaussian_kernel((t - k)/H) * y[k] * x[k]
      output_den <- output_den + gaussian_kernel((t - k)/H) * x[k] * x[k]
    }
    output[t] <- output_num/output_den
  }
  return(output)
}
