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

simuldata <- function(N=50, ni=5, beta=c(2, 0.7, 0), sdbinter=1, sdbslope=0, sdeps=1, type="norm", fixed=TRUE)
{
  p <- length(beta)
  if (fixed==TRUE)
  {
    if (type=="norm")
    {
      x <- matrix(rnorm(N*p), N, p) 
    }
    if (type=="bin")
    {
      x <- matrix(rbinom(N*p, 1, 0.5), N, p) 
    }
    x <- x[rep(1:N, each=ni), ]  
  }
  
  if (fixed==FALSE)
  {
    if (type=="norm")
    {
      x <- matrix(rnorm(N*ni*p), N*ni, p) 
    }
    if (type=="bin")
    {
      x <- matrix(rbinom(N*p*ni, 1, 0.5), N*ni, p) 
    }
  }
  
  index <- rep(1:N, each=ni)
  b <- rep(rnorm(N, sd=sdbinter), each=ni)
  b2 <- rep(rnorm(N, sd=sdbslope), each=ni)
  eps <- rnorm(N*ni, sd=sdeps)
  
  y <- x%*%beta + b + x[, 1]*b2 + eps
  return(data.frame(index=index, b=b, y=y, x))
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

simulation <- function(niter, N, ni, beta, sdbinter, sdbslope, sdeps, type, fixed)
{
  
  require("mlr3")
  require("mlr3verse")
  require("data.table")
  
  mse_subsamp0.8 <- numeric(niter)
  mse_subsamp0.8g <- numeric(niter)
  learner_linreg <- lrn("regr.lm")
  
  for (i in 1:niter)
  {
    print(i)
    simuldati <- simuldata(N=N, ni=ni, beta=beta, sdbinter=sdbinter, sdbslope=sdbslope, sdeps=sdeps, type=type, fixed=fixed)[, -2]
    task_i <- as_task_regr(simuldati, target="y")
    task_i$set_col_roles(cols="index", remove_from="feature")
    subsamp0.8 <- rsmp("subsampling", repeats = 100, ratio = 0.8)
    subsamp0.8$instantiate(task_i)
    result_subsamp0.8 <- resample(task=task_i, learner=learner_linreg, resampling=subsamp0.8)
    mse_subsamp0.8[i] <- result_subsamp0.8$aggregate(msr("regr.mse"))
    
    task_i$col_roles$group = "index"
    subsamp0.8$instantiate(task_i)
    result_subsamp0.8g <- resample(task=task_i, learner=learner_linreg, resampling=subsamp0.8)
    mse_subsamp0.8g[i] <- result_subsamp0.8g$aggregate(msr("regr.mse"))
    
  }
  result <- list(mse_subsamp0.8=mse_subsamp0.8, mse_subsamp0.8g=mse_subsamp0.8g)
  save(result, file=paste("./Simulations/ClustData/Results/N", N, "ni", ni, "beta", paste(beta, collapse=""), "sdbinter", sdbinter, "sdbslope", sdbslope, "sdeps", sdeps, type, fixed, ".RData", sep=""))
  
}
