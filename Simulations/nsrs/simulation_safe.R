# Set working directory:

#setwd("~/PPerfEstComplex")
setwd("Z:/Projects/DESTATIS/PredErrorComplex/PPerfEstComplex")


library("mlr3")
library("mlr3learners")
library("mlr3verse")
mlr_learners$get("regr.lm")
library("dplyr")


# regression settings:

b0 <- 5
b1 <- 1
b2 <- 1

experiments <- expand.grid(list(N = c(10000, 50000, 100000),
                                correct_model = c(TRUE, FALSE)
))



# Source the functions that are used in performing the calculations 
# on the cluster:

source("./simulations/nsrs/functions.R")






nsim <- 10

lgr::get_logger("mlr3")$set_threshold("warn")

i <- 1
# Repeat:
# Generate Population + (large) test set
outlist <- list()
set.seed(1987)

starti <- Sys.time()

for(k in 1:nrow(experiments)){
  
  N <- experiments$N[k]
  n <- N/100
  correct_model <- experiments$correct_model[k]
  for(iter in 1:nsim){
    train_data <- generate_data(N, b0, b1, b2)
    test_data <- generate_data(N, b0, b1, b2)
    
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
    
    design <- benchmark_grid(
      tasks = task_sp,
      learners = learnerlist,
      resamplings = rsmps("repeated_cv", folds = 5, repeats = 10)
    )
    
    bmr <- benchmark(design)
    tab <- bmr$aggregate(measure)
    estimated_error <- tab$regr.mse
    corrected_estimates <- HT_correction(bmr, w, N)
    
    # Use test set to get "true" generalization performance
    true_error <- c()
    for(j in 1:length(learnerlist)){
      learnerlist[[j]]$train(task_sp)
      outpreds <- learnerlist[[j]]$predict_newdata(test_data)
      true_error[j] <- outpreds$score(measure)
    }
    
    outlist[[i]] <- data.frame(iter = iter,
                               n = n,
                               correct = correct_model,
                               model = tab$learner_id, 
                               estimated_error = estimated_error,
                               corrected = corrected_estimates,
                               true_error = true_error
    )
    
    cat(paste0("Iteration: ", i, " of ", nrow(experiments)*nsim), "\n")
    
    i = i+1
    
  }
}

stopi <- Sys.time()

as.numeric(difftime(stopi, starti, units="mins"))*100/60

results <- do.call('rbind', outlist)


# Save the results: 

save(results, file="./simulations/nsrs/results/intermediate_results/results.Rda") 
