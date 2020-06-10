# Computes the gaussian Kernel, which will be used to calculate the estimator

gaussian_kernel <- function(x) {
  return((1 / (sqrt(2 * pi))) * exp(-(x^2/2)))
}

