# This function performs one repetition of the five times repeated 
# 5-fold stratified cross-validation on a specific data set using
# one of the fives compared methods.
#
# It takes the whole number 'iter', which corresponds to the iter-th line 
# of 'scenariogrid', which contains the necessary information
# on the iter-th setting.

evaluatesetting <- function(iter) {
  
  # Obtain information for the iter-th setting:
  
  xtrend <- scenariogrid$xtrend[iter] 
  ytrend <- scenariogrid$ytrend[iter] 
  n <- scenariogrid$n[iter] 
  predmethod <- scenariogrid$predmethod[iter] 
  repetition <- scenariogrid$repetition[iter] 
  seed <- scenariogrid$seed[iter] 
  
  if (xtrend == "none")
    x1muend <- 0; x2muend <- 0; x3muend <- 0
  if (xtrend == "weak")
    x1muend <- 2; x2muend <- 1; x3muend <- -2
  if (xtrend == "medium")
    x1muend <- 4; x2muend <- 2; x3muend <- -4
  if (xtrend == "strong")
    x1muend <- 8; x2muend <- 4; x3muend <- -8
  
  if (xtrend == "none")
    ymuend <- 0; yvarend <- 1
  if (xtrend == "weak")
    ymuend <- 1.5; yvarend <- 1.5
  if (xtrend == "medium")
    ymuend <- 3; yvarend <- 3
  if (xtrend == "strong")
    ymuend <- 6; yvarend <- 6
  
  # Set seed:
  
  set.seed(seed)
  
  res <- simulation(n=n, x1muend=x1muend, x2muend=x2muend, x3muend=x3muend, ymuend=ymuend, yvarend=yvarend, method=predmethod)
  
  
  save(res, file=paste0("./simulations/concdrift/results/intermediate_results/res_", iter, ".Rda"))
  
  # Save results:
  
  return(res)
}














simulation <- function(n, x1muend=8, x2muend=4, x3muend=-8, ymuend=6, yvarend=6, method="rf") {
  
  setwd(wd)
  
  # We simulate 7 seasons, 5 seasons for training and 2 seasons as future seasons
  # for testing. The following object give the time intervals of the seasons,
  # where we also include the midpoints in the last two seasons because we will
  # evaluate the error here as well.
  
  seasonbreaks <- seq(0, 1, length=8)
  seasonbreaks <- c(seasonbreaks[1:6], (seasonbreaks[6]+seasonbreaks[7])/2, 
                    seasonbreaks[7], (seasonbreaks[7]+seasonbreaks[8])/2, 
                    seasonbreaks[8])
  
  
  # Simulate the training dataset:
  
  datatrain <- sim_dataset(seq(seasonbreaks[1], seasonbreaks[6], length=n),
                           x1muend=x1muend, x2muend=x2muend, x3muend=x3muend, 
                           ymuend=ymuend, yvarend=yvarend)
  
  require("mlr3")
  require("mlr3temporal")
  require("mlr3verse")
  
  
  # Determine the learner to use:
  
  if (method=="lm")
    learner_temp <- lrn("regr.lm")
  if (method=="rf")
    learner_temp <- lrn("regr.ranger")
  
  
  # Suppress warning from mlr3:
  
  lgr::get_logger("mlr3")$set_threshold("warn")
  
  
  
  # Set the task:
  
  task <- as_task_regr(datatrain, target="y")
  
  
  
  # Regular CV:
  
  cv <- rsmp("repeated_cv", repeats = 10, folds = 5)
  cv$instantiate(task)
  result_cv <- resample(task=task, learner=learner_temp, resampling=cv)
  mse_cv <- result_cv$aggregate(msr("regr.mse"))
  
  
  
  # Time series CV - predict next season:
  
  # Determine the indices of the training and test sets for the times series CV:
    sizes <- rep(floor(n/7), 7)
  if(n - 7*floor(n/7) > 0)
    sizes[1:(n - 7*floor(n/7))] <- sizes[1:(n - 7*floor(n/7))] + 1
  train_setsTS_1s <- lapply(cumsum(sizes[-length(sizes)]), function(x) 1:x)
  train_setsTS_2s <- train_setsTS_1s[-length(train_setsTS_1s)]
  
  test_setsTS_1s <- lapply(data.frame(rbind(cumsum(sizes[-length(sizes)]) + 1, cumsum(sizes)[-1])), function(x) x[1]:x[2])
  test_setsTS_2s <- test_setsTS_1s[-1]
  TScv <- rsmp("custom")
  TScv$instantiate(task, train_setsTS_1s, test_setsTS_1s)
  
  result_TScv <- resample(task=task, learner=learner_temp, resampling=TScv)
  mse_TScv_1s <- result_TScv$aggregate(msr("regr.mse"))
  
  
  
  # Time series CV - predict next season but one:
  
  TScv <- rsmp("custom")
  TScv$instantiate(task, train_setsTS_2s, test_setsTS_2s)
  
  result_TScv <- resample(task=task, learner=learner_temp, resampling=TScv)
  mse_TScv_2s <- result_TScv$aggregate(msr("regr.mse"))
  
  
  
  # Hold-out - predict last (i.e., seventh) season, train on first
  # to sixth season:
  
  learner_temp$train(task, row_ids = train_setsTS_1s[[length(train_setsTS_1s)]])
  
  predictions <- learner_temp$predict(task, row_ids = test_setsTS_1s[[length(test_setsTS_1s)]])
  mse_TSholdout_1s <- predictions$score(msr("regr.mse"))
  
  
  
  # Hold-out - predict last (i.e., seventh) season, train on first
  # to fifth season:
  
  learner_temp$train(task, row_ids = train_setsTS_2s[[length(train_setsTS_2s)]])
  
  predictions <- learner_temp$predict(task, row_ids = test_setsTS_2s[[length(test_setsTS_2s)]])
  mse_TSholdout_2s <- predictions$score(msr("regr.mse"))
  
  
  # asdf
  train_task <- as_task_regr(datatrain, target = "y")
  learner_temp$train(train_task)
  
  mse_true <- c()
  
  for(count in 1:5) {
    
    datatest <- sim_dataset(rep(seasonbreaks[count+5], 20000),
                            x1muend=x1muend, x2muend=x2muend, x3muend=x3muend, ymuend=ymuend, yvarend=yvarend)
    
    test_task <- as_task_regr(datatest, target = "y")
    predictions <- learner_temp$predict(test_task)
    mse_true[count] <- predictions$score(msr("regr.mse"))
    
  }
  
  mse_true_endlast <- mse_true[1]
  mse_true_mid1fu <- mse_true[2]
  mse_true_end1fu <- mse_true[3]
  mse_true_mid2fu <- mse_true[4]
  mse_true_end2fu <- mse_true[5]
  
  
  
  res <- list(mse_cv=mse_cv, mse_TScv_1s=mse_TScv_1s, mse_TScv_2s=mse_TScv_2s, 
              mse_TSholdout_1s=mse_TSholdout_1s, mse_TSholdout_2s=mse_TSholdout_2s, 
              mse_true_endlast=mse_true_endlast, mse_true_mid1fu=mse_true_mid1fu, 
              mse_true_end1fu=mse_true_end1fu, mse_true_mid2fu=mse_true_mid2fu, 
              mse_true_end2fu=mse_true_end2fu)
  
  return(res)
  
}








getcoef <- function(t, startval, stopval) {
  
  # startval <- 0
  # stopval <- 8
  tstart <- 0
  tend <- 1
  
  b1 <- (stopval - startval)/(tend - tstart)
  b0 <- startval - b1*tstart
  
  return(b0 + b1*t)
  
}






sim_obs <- function(t, x1muend=8, x2muend=4, x3muend=-8,
                    ymuend=6, yvarend=6) {
  
  x1 <- rnorm(1, mean=getcoef(t, startval=0, stopval=x1muend), sd=1)
  x2 <- rnorm(1, mean=getcoef(t, startval=0, stopval=x2muend), sd=1)
  x3 <- rnorm(1, mean=getcoef(t, startval=0, stopval=x3muend), sd=1)
  
  xvec <- c(x1, x2, x3, rnorm(2))
  
  y <- getcoef(t, startval=0, stopval=ymuend) + xvec[1]*2 + xvec[2]*(-1) + 
    xvec[3]*2 +rnorm(1, mean=0, sd=sqrt(getcoef(t, startval=1, stopval=yvarend)))
  
  return(c(xvec, y))
  
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

sim_dataset <- function(timepoints, x1muend=8, x2muend=4, x3muend=-8, ymuend=6, yvarend=6) {
  
  dataset <- data.frame(t(sapply(timepoints, function(x) 
    sim_obs(x, x1muend=x1muend, x2muend=x2muend, x3muend=x3muend, 
            ymuend=ymuend, yvarend=yvarend))))
  names(dataset)[ncol(dataset)] <- "y"
  
  return(dataset)
  
}



