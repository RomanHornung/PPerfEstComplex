# DONE:

# This function performs one repetition of the simulation.
#
# It takes the whole number 'iter', which corresponds to the iter-th line 
# of 'scenariogrid', which contains the necessary information
# on the iter-th setting.

evaluatesetting <- function(iter) {
    
  # Obtain information for the iter-th setting:
  
  xtrend <- scenariogrid$xtrend[iter] 
  ytrend <- scenariogrid$ytrend[iter] 
  n <- scenariogrid$n[iter] 
  repetition <- scenariogrid$repetition[iter] 
  seed <- scenariogrid$seed[iter] 
  
  if (xtrend == "none") {
    x1muend <- 0; x2muend <- 0; x3muend <- 0
  }
  if (xtrend == "weak") {
    x1muend <- 2; x2muend <- 1; x3muend <- -2
  }
  if (xtrend == "medium") {
    x1muend <- 4; x2muend <- 2; x3muend <- -4
  }
  if (xtrend == "strong") {
    x1muend <- 8; x2muend <- 4; x3muend <- -8
  }
  
  if (ytrend == "none") {
    ymuend <- 0; yvarend <- 1
  }
  if (ytrend == "weak") {
    ymuend <- 2; yvarend <- 2
  }
  if (ytrend == "medium") {
    ymuend <- 4; yvarend <- 4
  }
  if (ytrend == "strong") {
    ymuend <- 8; yvarend <- 8
  }
    
  # Set seed:
  
  set.seed(seed)
  
  res <- simulation(n=n, x1muend=x1muend, x2muend=x2muend, x3muend=x3muend, ymuend=ymuend, yvarend=yvarend)
  
  # Return results:
  
  return(res)
  
}






# Function to perform one simulation iteration for a specific setting.

# Function input:

# n: number of training observations
# x1muend: expectancy of x1 at the end of the considered interval
# x2muend: expectancy of x2 at the end of the considered interval
# x3muend: expectancy of x3 at the end of the considered interval
# ymuend: intercept at the end of the considered interval
# yvarend: variance of the Gaussian noise at the end of the considered interval

# Function output:

# The estimated and (approximated) true MSE values

simulation <- function(n, x1muend=8, x2muend=4, x3muend=-8, ymuend=6, yvarend=6) {
  
  setwd(wd)
  
  # We simulate 10 seasons, 8 seasons for training and 2 seasons as future seasons
  # for testing. The following object give the time intervals of the seasons,
  # where we also include the midpoints in the last two seasons because we will
  # evaluate the error here as well.
  
  seasonbreaks <- seq(0, 1, length=11)
  seasonbreaks <- c(seasonbreaks[1:9], (seasonbreaks[9]+seasonbreaks[10])/2, 
                    seasonbreaks[10], (seasonbreaks[10]+seasonbreaks[11])/2, 
                    seasonbreaks[11])
  
  
  # Simulate the training dataset:
  
  datatrain <- sim_dataset(seq(seasonbreaks[1], seasonbreaks[9], length=n),
                           x1muend=x1muend, x2muend=x2muend, x3muend=x3muend, 
                           ymuend=ymuend, yvarend=yvarend)
  
  
  require("mlr3")
  require("mlr3temporal")
  require("mlr3verse")
  
  
  # Use random forests and linear models as learners:
  
  learner_temp_rf <- lrn("regr.ranger")
  learner_temp_lm <- lrn("regr.lm")
  
  
  
  # Suppress warning from mlr3:
  
  lgr::get_logger("mlr3")$set_threshold("warn")
  
  
  
  # Set the task:
  
  task <- as_task_regr(datatrain, target="y")
  
  
  
  # Regular CV:
  
  cv <- rsmp("repeated_cv", repeats = 10, folds = 8)
  cv$instantiate(task)
  result_cv <- resample(task=task, learner=learner_temp_rf, resampling=cv)
  mse_cv_rf <- result_cv$aggregate(msr("regr.mse"))
  result_cv <- resample(task=task, learner=learner_temp_lm, resampling=cv)
  mse_cv_lm <- result_cv$aggregate(msr("regr.mse"))

  
  
  # Determine the indices of the training and test sets for the times series CV:
  
  sizes <- rep(floor(n/8), 8)
  if(n - 8*floor(n/8) > 0)
    sizes[1:(n - 8*floor(n/8))] <- sizes[1:(n - 8*floor(n/8))] + 1
  train_setsTS_1s <- lapply(cumsum(sizes[-length(sizes)]), function(x) 1:x)
  train_setsTS_2s <- train_setsTS_1s[-length(train_setsTS_1s)]
  
  test_setsTS_1s <- lapply(data.frame(rbind(cumsum(sizes[-length(sizes)]) + 1, cumsum(sizes)[-1])), function(x) x[1]:x[2])
  test_setsTS_2s <- test_setsTS_1s[-1]
  
  
  
  # Time series CV - predict next season:
  
  TScv <- rsmp("custom")
  TScv$instantiate(task, train_setsTS_1s, test_setsTS_1s)
  
  result_TScv <- resample(task=task, learner=learner_temp_rf, resampling=TScv)
  mse_TScv_1s_rf <- result_TScv$aggregate(msr("regr.mse"))
  result_TScv <- resample(task=task, learner=learner_temp_lm, resampling=TScv)
  mse_TScv_1s_lm <- result_TScv$aggregate(msr("regr.mse"))

  
  
  # Time series CV - predict next season but one:
  
  TScv <- rsmp("custom")
  TScv$instantiate(task, train_setsTS_2s, test_setsTS_2s)
  
    result_TScv <- resample(task=task, learner=learner_temp_rf, resampling=TScv)
  mse_TScv_2s_rf <- result_TScv$aggregate(msr("regr.mse")) 
  result_TScv <- resample(task=task, learner=learner_temp_lm, resampling=TScv)
  mse_TScv_2s_lm <- result_TScv$aggregate(msr("regr.mse"))

  
  
  # Out-of-sample validation - predict last (i.e., eigth) season, train on first
  # to seventh season:
  
  learner_temp_rf$train(task, row_ids = train_setsTS_1s[[length(train_setsTS_1s)]])
  
  predictions <- learner_temp_rf$predict(task, row_ids = test_setsTS_1s[[length(test_setsTS_1s)]])
  mse_TSholdout_1s_rf <- predictions$score(msr("regr.mse"))
  
  learner_temp_lm$train(task, row_ids = train_setsTS_1s[[length(train_setsTS_1s)]])
  
  predictions <- learner_temp_lm$predict(task, row_ids = test_setsTS_1s[[length(test_setsTS_1s)]])
  mse_TSholdout_1s_lm <- predictions$score(msr("regr.mse"))

  
  
  # Out-of-sample validation - predict last (i.e., eigth) season, train on first
  # to sixth season:
    
  learner_temp_rf$train(task, row_ids = train_setsTS_2s[[length(train_setsTS_2s)]])
  
  predictions <- learner_temp_rf$predict(task, row_ids = test_setsTS_2s[[length(test_setsTS_2s)]])
  mse_TSholdout_2s_rf <- predictions$score(msr("regr.mse"))
  
  learner_temp_lm$train(task, row_ids = train_setsTS_2s[[length(train_setsTS_2s)]])
  
  predictions <- learner_temp_lm$predict(task, row_ids = test_setsTS_2s[[length(test_setsTS_2s)]])
  mse_TSholdout_2s_lm <- predictions$score(msr("regr.mse"))

  
  
  # Train on the whole training data set and evaluate on the huge tests
  # to approximate the true performance metric values:

  train_task <- as_task_regr(datatrain, target = "y")
  learner_temp_rf$train(train_task)
  learner_temp_lm$train(train_task)

  mse_true_lm <- c()
  mse_true_rf <- c()
  
  for(count in 1:5) {
    
    datatest <- sim_dataset(rep(seasonbreaks[count+8], 200000),
                            x1muend=x1muend, x2muend=x2muend, x3muend=x3muend, ymuend=ymuend, yvarend=yvarend)
    
    test_task <- as_task_regr(datatest, target = "y")
    
    predictions <- learner_temp_rf$predict(test_task)
    mse_true_rf[count] <- predictions$score(msr("regr.mse"))
    
    predictions <- learner_temp_lm$predict(test_task)
    mse_true_lm[count] <- predictions$score(msr("regr.mse"))
    
  }
  
  mse_true_endlast_lm <- mse_true_lm[1]
  mse_true_mid1fu_lm <- mse_true_lm[2]
  mse_true_end1fu_lm <- mse_true_lm[3]
  mse_true_mid2fu_lm <- mse_true_lm[4]
  mse_true_end2fu_lm <- mse_true_lm[5]
  
  mse_true_endlast_rf <- mse_true_rf[1]
  mse_true_mid1fu_rf <- mse_true_rf[2]
  mse_true_end1fu_rf <- mse_true_rf[3]
  mse_true_mid2fu_rf <- mse_true_rf[4]
  mse_true_end2fu_rf <- mse_true_rf[5]
  
  
  # Combine the results:
  
  res <- list(mse_cv_rf=mse_cv_rf, mse_cv_lm=mse_cv_lm, mse_TScv_1s_rf=mse_TScv_1s_rf, mse_TScv_1s_lm=mse_TScv_1s_lm, mse_TScv_2s_rf=mse_TScv_2s_rf, mse_TScv_2s_lm=mse_TScv_2s_lm, 
              mse_TSholdout_1s_rf=mse_TSholdout_1s_rf, mse_TSholdout_1s_lm=mse_TSholdout_1s_lm, mse_TSholdout_2s_rf=mse_TSholdout_2s_rf, mse_TSholdout_2s_lm=mse_TSholdout_2s_lm, 
              mse_true_endlast_rf=mse_true_endlast_rf, mse_true_endlast_lm=mse_true_endlast_lm, mse_true_mid1fu_rf=mse_true_mid1fu_rf, mse_true_mid1fu_lm=mse_true_mid1fu_lm, 
              mse_true_end1fu_rf=mse_true_end1fu_rf, mse_true_end1fu_lm=mse_true_end1fu_lm, mse_true_mid2fu_rf=mse_true_mid2fu_rf, mse_true_mid2fu_lm=mse_true_mid2fu_lm, 
              mse_true_end2fu_rf=mse_true_end2fu_rf, mse_true_end2fu_lm=mse_true_end2fu_lm)
  
  # Return the results:
  
  return(res)
  
}








# Function to simulate a dataset.

# Function input:

# timepoints: The time points at which to simulate the observations. At each
# time point a single observation is simulated.
# x1muend: expectancy of x1 at the end of the considered interval
# x2muend: expectancy of x2 at the end of the considered interval
# x3muend: expectancy of x3 at the end of the considered interval
# ymuend: intercept at the end of the considered interval
# yvarend: variance of the Gaussian noise at the end of the considered interval

# Function output:

# The simulated data set

sim_dataset <- function(timepoints, x1muend=8, x2muend=4, x3muend=-8, ymuend=6, yvarend=6) {
  
  dataset <- data.frame(t(sapply(timepoints, function(x) 
    sim_obs(x, x1muend=x1muend, x2muend=x2muend, x3muend=x3muend, 
            ymuend=ymuend, yvarend=yvarend))))
  names(dataset)[ncol(dataset)] <- "y"
  
  return(dataset)
  
}







# Function to get the value of a parameter at a given time.

# Function input:

# t: time at which the parameter value is to be obtained.
# startval: value of the parameter at the beginning of the considered interval
# stopval: value of the parameter at the end of the considered interval

# Function output:

# The value of the parameter

getcoef <- function(t, startval, stopval) {

  tstart <- 0
  tend <- 1
  
  b1 <- (stopval - startval)/(tend - tstart)
  b0 <- startval - b1*tstart
  
  return(b0 + b1*t)
  
}






# Function to simulate a single observation at a given time.

# Function input:

# t: time at which the observation should be simulated.
# x1muend: expectancy of x1 at the end of the considered interval
# x2muend: expectancy of x2 at the end of the considered interval
# x3muend: expectancy of x3 at the end of the considered interval
# ymuend: intercept at the end of the considered interval
# yvarend: variance of the Gaussian noise at the end of the considered interval

# Function output:

# A vector of length 6, where the first 5 elements are the feature
# values and the last element is the label value

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
