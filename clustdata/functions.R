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
ni <- scenariogrid$ni[iter] 
sdbinter <- scenariogrid$sdbinter[iter] 
sdbslope <- scenariogrid$sdbslope[iter] 
sdeps <- scenariogrid$sdeps[iter] 
fixed <- scenariogrid$fixed[iter]
repetition <- scenariogrid$repetition[iter] 
seed <- scenariogrid$seed[iter] 
  
  # Set seed:
  
  set.seed(seed)
  
  result <- simulation(niter=1, N=N, ni=ni, beta=c(1, 1, -1, 0, 0), sdbinter=sdbinter, sdbslope=sdbslope, sdeps=sdeps, fixed=fixed, method="rf")
  
  # Save results:
  
  return(result)
}








# Function for simulating the datasets.

# Function input:

# N: number of clusters
# ni: number of observations per cluster
# beta: coefficients of the variables
# sdbinter: standard deviation of the random intercepts
# sdbslope: standard deviation of the random slope of variable x1
# sdeps: standard deviation of the Gaussian noise
# type: type of variables. Can be "norm" for normally distributed variables
# and "bin" for binary variables (equal probability for each class)
# fixed: 

# Function output:

# A data.frame containing the simulated data.

simuldata <- function(N=50, ni=5, beta=c(1, 1, -1, 0, 0), sdbinter=0, sdbslope=0, sdeps=1, fixed=c("none", "first", "second"))
{
  
  p <- length(beta)
  
  x <- matrix(rnorm(N*ni*p), N*ni, p)
  
  index <- rep(1:N, each=ni)
  
if (fixed=="first")
    x[, 1] <- rnorm(N)[index]
  if (fixed=="second")
    x[, 2] <- rnorm(N)[index]

  b <- rep(rnorm(N, sd=sdbinter), each=ni)
  b2 <- rep(rnorm(N, sd=sdbslope), each=ni)
  eps <- rnorm(N*ni, sd=sdeps)
  
  y <- x%*%beta + b + x[, 1]*b2 + eps
  
  dataset <- data.frame(y=y, x=x, index=index)
  
  return(list(dataset=dataset, b=b, b2=b2))
  
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

simulation <- function(niter, N=50, ni=5, beta=c(1, 1, -1, 0, 0), sdbinter=0, sdbslope=0, sdeps=1, fixed=c("none", "first", "second"), method="lm")
{
  
  require("mlr3")
  require("mlr3verse")
  require("data.table")
  
  mse_cv3_rf <- mse_cv3_lm <- numeric(niter)
  mse_cv3g_rf <- mse_cv3g_lm <- numeric(niter)
  
  learner_temp_rf <- lrn("regr.ranger")
  learner_temp_lm <- lrn("regr.lm")
  
  # set_threads(learner_temp, n = 1)
  
  lgr::get_logger("mlr3")$set_threshold("warn")
  
  for (i in 1:niter)
  {
    # print(i)
    simuldati <- simuldata(N=N, ni=ni, beta=beta, sdbinter=sdbinter, sdbslope=sdbslope, sdeps=sdeps, fixed=fixed)$dataset
    task_i <- as_task_regr(simuldati, target="y")
    task_i$set_col_roles(cols="index", remove_from="feature")
    # subsamp0.8 <- rsmp("subsampling", repeats = 100, ratio = 0.8)
    cv3 <- rsmp("repeated_cv", repeats = 10, folds = 5)
    cv3$instantiate(task_i)
    result_cv3 <- resample(task=task_i, learner=learner_temp_rf, resampling=cv3)
    mse_cv3_rf[i] <- result_cv3$aggregate(msr("regr.mse"))
    result_cv3 <- resample(task=task_i, learner=learner_temp_lm, resampling=cv3)
    mse_cv3_lm[i] <- result_cv3$aggregate(msr("regr.mse"))
    
    task_i$col_roles$group = "index"
    cv3$instantiate(task_i)
    result_cv3g <- resample(task=task_i, learner=learner_temp_rf, resampling=cv3)
    mse_cv3g_rf[i] <- result_cv3g$aggregate(msr("regr.mse"))
    result_cv3g <- resample(task=task_i, learner=learner_temp_lm, resampling=cv3)
    mse_cv3g_lm[i] <- result_cv3g$aggregate(msr("regr.mse"))
    
  }
  result <- list(mse_cv3_rf=mse_cv3_rf, mse_cv3_lm=mse_cv3_lm, mse_cv3g_rf=mse_cv3g_rf, mse_cv3g_lm=mse_cv3g_lm)
  return(result)
  # save(result, file=paste("./clustdata/Results/intermediate_results/N", N, "ni", ni, "beta", paste(beta, collapse=""), "sdbinter", sdbinter, "sdbslope", sdbslope, "sdeps", sdeps, fixed, ".RData", sep=""))
  
}
