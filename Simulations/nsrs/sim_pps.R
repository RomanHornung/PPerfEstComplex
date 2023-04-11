library(mlr3)
library(mlr3learners)
library(mlr3verse)
library(ggplot2)
library(dplyr)
mlr_learners$get("regr.lm")

nsim      = 1000


#### Simulation settings
#### regression model
generate_data = function(N, b0, b1, b2, sigma = 1){
  x1 = rgamma(N, 0.1, 0.1)
  x2 = rgamma(N, 0.1, 0.1)
  x3 = rnorm(N)
  x4 = rnorm(N)
  x5 = rnorm(N)
  y = b0 + b1*x1 + b2*x2 + rnorm(length(x1), 0, sigma)
  return(data.frame(y, x1, x2, x3, x4, x5))
}

#### regression settings
b0     = 5
b1     = 1
b2     = 1

experiments = expand.grid(list(N             = c(10000, 50000, 100000),
                              correct_model  = c(T, F)
                              )
                          )

#### Horvitz Thompson correction (equation 14 from the paper)

HT_correction = function(bmr, w, N){
  nfolds = length(bmr$resample_results$resample_result[[1]]$predictions())
  nmods  = length(bmr$resample_results$resample_result)
  error  = matrix(0,
                  nrow = nfolds,
                  ncol = nmods
                  )
  for(kk in 1:nmods){
  pred_frame = bmr$resample_results$resample_result[[kk]]$predictions()
    for(fold in 1:nfolds){
      fold_frame = pred_frame[[fold]]
      true_val   = fold_frame$response
      pred_val   = fold_frame$truth
      w_fold     = sample_weights[fold_frame$row_ids]
      error[fold, kk]  = (sum((w_fold^-1)*((true_val-pred_val)^2)))/(N*(1/nfolds))
    }
  }
  apply(error, 2, mean)
}


i = 1
#### Repeat:
#### Generate Population + (large) test set
outlist   = list()
set.seed(1987)
for(k in 1:nrow(experiments)){
  N             = experiments$N[k]
  n             = N/100
  correct_model = experiments$correct_model[k]
  for(iter in 1:nsim){
    train_data = generate_data(N, b0, b1, b2)
    test_data = generate_data(N, b0, b1, b2)
    
    # Calculate the auxiliary variable for PPS sampling
    valid = FALSE
    while(!valid){
      u  = b0 + b1*train_data$y + rnorm(N)
      if(all(u>0)){
        valid = TRUE
      }
    }
    
    #### Draw PPS-sample
    w      = n*u/(sum(u))
    ids    = sample(1:N, n, prob = w)
    sample_weights = w[ids]
    
    # if not the correct model exclude one of the covariates
    if(!correct_model == T){
      train_data      = train_data %>% select(-x2)
      test_data      = test_data %>% select(-x2)
    }
    
    # Take the sample
    train_data = train_data[ids,]
    
    #### Estimate generalization performance via CV (mlr3)
    task_sp    = as_task_regr(y ~ ., data = train_data)
    measure    = msr("regr.mse")

    #### Define the experiment setup
    learnerlist = c(lrn("regr.lm"),
                    lrn("regr.ranger")
                   )
    
    design = benchmark_grid(
             tasks        = task_sp,
             learners     = learnerlist,
             resamplings  = rsmps("cv", folds = 5)
             )
    
    bmr        = benchmark(design)
    tab        = bmr$aggregate(measure)
    estimated_error = tab$regr.mse
    corrected_estimates = HT_correction(bmr, w, N)
    
    #### Use test set to get "true" generalization performance
    true_error = c()
    for(j in 1:length(learnerlist)){
      learnerlist[[j]]$train(task_sp)
      outpreds            = learnerlist[[j]]$predict_newdata(test_data)
      true_error[j]       = outpreds$score(measure)
    }
    
    outlist[[i]] = data.frame(iter               = iter,
                                 n               = n,
                                 correct         = correct_model,
                                 model           = tab$learner_id, 
                                 estimated_error = estimated_error,
                                 corrected       = corrected_estimates,
                                 true_error      = true_error
                                )
    i = i+1
  }
}

#### Visualise the results, analyse the bias
final_out = do.call('rbind', outlist) 
write.csv2(final_out, "res.csv")

#### compare unbased and biased version of estimates
ggframe = rbind(final_out %>% mutate(type = "biased"),
                final_out %>% mutate(estimated_error = corrected,
                                     type = "de-biased (HT)")
                ) %>%
          mutate(bias       = estimated_error - true_error)

final_out %>% group_by(model, n, correct) %>% summarise(bias = mean(corrected - true_error))
final_out %>% group_by(model, n, correct) %>% summarise(bias = mean(estimated_error - true_error))

#### look at all settings jointly
ggplot(ggframe) +
        geom_boxplot(aes(y=bias, x = model, fill = type)) + 
        scale_fill_manual(values = c("#999999", "#E69F00")) +
        facet_grid(~n+correct) + 
        theme_bw()
