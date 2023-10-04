# This function performs one repetition of the five times repeated 
# 5-fold stratified cross-validation on a specific data set using
# one of the fives compared methods.
#
# It takes the whole number 'iter', which corresponds to the iter-th line 
# of 'scenariogrid', which contains the necessary information
# on the iter-th setting.

evaluatesetting <- function(iter) {
  
  # Obtain information for the iter-th setting:
  N <- scenariogrid$N[iter] 
  correct_model <- scenariogrid$correct_model[iter] 
  repetition <- scenariogrid$repetition[iter] 
  seed <- scenariogrid$seed[iter] 
  
  # Set seed:
  
  set.seed(seed)
  
  result <- simulation(N=N, correct_model=correct_model, repetition=repetition)
  
  # Save results:
  
  return(result)
}






# Function for performing the simulation for a specific setting.

# Function input:

# niter: number of simulation iterations
# N: number of clusters
# ni: number of observations per cluster
# beta: coefficients of the variables
# sdbinter: standard deviation of the random intercepts
# sdbslope: standard deviation of the random slope of variable x3
# sdeps: standard deviation of the Gaussian noise
# type: type of variables. Can be "norm" for normally distributed variables
# and "bin" for binary variables (equal probability for each class)
# fixed: 

# Function output:

# No output. The MSE values resulting when dividing the data into training and test
# data (a) at the level of the observations and (b) at the level of the clusters.

# A data.frame containing the simulated data.

simulation <- function(N, correct_model, repetition, b0 = 5, b1 = 1, b2 = 1)
{
  
  library("mlr3")
  library("mlr3learners")
  library("mlr3verse")
  mlr_learners$get("regr.lm")
  library("dplyr")
  
  lgr::get_logger("mlr3")$set_threshold("warn")
  
  # Repeat:
  # Generate Population + (large) test set
  
  #####N <- experiments$N[k]
  n <- N/100
  #####correct_model <- experiments$correct_model[k]
  
  train_data <- generate_data(N, b0, b1, b2)
  test_data <- generate_data(N=200000, b0, b1, b2)
  
  # Calculate the auxiliary variable for PPS sampling
  valid <- FALSE
  while(!valid){
    u <- train_data$y + rnorm(N)
    if(all(u>0)){
      valid <- TRUE
    }
  }
  
  # Draw PPS-sample
  w <- n*u/(sum(u))
  ids <- sample(1:N, n, prob = w)
  sample_weights <- w[ids]
  
  # if not the correct model exclude one of the covariates
  if(!correct_model){
    train_data <- train_data %>% select(-x2)
    test_data <- test_data %>% select(-x2)
  }
  
  # Take the sample
  train_data <- train_data[ids,]
  
  # Estimate generalization performance via CV (mlr3)
  task_sp <- as_task_regr(y ~ ., data = train_data)
  measure <- msr("regr.mse")
  
  # Define the experiment setup
  learnerlist <- c(lrn("regr.lm"),
                   lrn("regr.ranger"))
  
  nrep <- 10
  design <- benchmark_grid(
    tasks = task_sp,
    learners = learnerlist,
    resamplings = rsmps("repeated_cv", folds = 5, repeats = nrep)
  )
  
  bmr <- benchmark(design)
  tab <- bmr$aggregate(measure)
  estimated_error <- tab$regr.mse
  corrected_estimates <- HT_correction(bmr, w, N, sample_weights, nrep)
  
  # Use test set to get "true" generalization performance
  true_error <- c()
  for(j in 1:length(learnerlist)){
    learnerlist[[j]]$train(task_sp)
    outpreds <- learnerlist[[j]]$predict_newdata(test_data)
    true_error[j] <- outpreds$score(measure)
  }
  
  result <- data.frame(iter = repetition,
                       n = n,
                       correct = correct_model,
                       model = tab$learner_id, 
                       estimated_error = estimated_error,
                       corrected = corrected_estimates,
                       true_error = true_error)
  
  return(result)
  
}






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

HT_correction <- function(bmr, w, N, sample_weights, nrep){
  
  nfolds <- length(bmr$resample_results$resample_result[[1]]$predictions())
  nmods <- length(bmr$resample_results$resample_result)
  error <- matrix(0, nrow = nfolds, ncol = nmods)
  
  for(kk in 1:nmods){
    pred_frame <- bmr$resample_results$resample_result[[kk]]$predictions()
    for(fold in 1:nfolds){
      fold_frame <- pred_frame[[fold]]
      true_val <- fold_frame$response
      pred_val <- fold_frame$truth
      w_fold <- sample_weights[fold_frame$row_ids]
      error[fold, kk] <- (sum((w_fold^-1)*((true_val-pred_val)^2)))/N
    }
  }
  
  apply(error, 2, sum)/nrep
}
