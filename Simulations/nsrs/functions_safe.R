# Simulation settings
# regression model
generate_data <- function(N, b0, b1, b2, sigma = 1){
  x1 <- rgamma(N, 0.1, 0.1)
  x2 <- rgamma(N, 0.1, 0.1)
  x3 <- rnorm(N)
  x4 <- rnorm(N)
  x5 <- rnorm(N)
  y <- b0 + b1*x1 + b2*x2 + rnorm(length(x1), 0, sigma)
  return(data.frame(y, x1, x2, x3, x4, x5))
}


# Horvitz Thompson correction (equation 14 from the paper)

HT_correction <- function(bmr, w, N){
  nfolds <- length(bmr$resample_results$resample_result[[1]]$predictions())
  nmods <- length(bmr$resample_results$resample_result)
  error <- matrix(0,
                  nrow <- nfolds,
                  ncol <- nmods
  )
  for(kk in 1:nmods){
    pred_frame <- bmr$resample_results$resample_result[[kk]]$predictions()
    for(fold in 1:nfolds){
      fold_frame <- pred_frame[[fold]]
      true_val <- fold_frame$response
      pred_val <- fold_frame$truth
      w_fold <- sample_weights[fold_frame$row_ids]
      error[fold, kk] <- (sum((w_fold^-1)*((true_val-pred_val)^2)))/(N*(1/nfolds))
    }
  }
  apply(error, 2, mean)
}
