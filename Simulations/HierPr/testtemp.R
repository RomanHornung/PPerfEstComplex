setwd("Z:/Projects/DESTATIS/PredErrorComplex/PPerfEstComplex")


source("./simulations/hierpr/functions_simulation.R")






load("./simulations/hierpr/results/intermediate_results/treestruc.Rda")


# set.seed(1234)


coeflist <- simulate_coefs(treestruc=treestruc, sdbeta0=sqrt(1),
                           sdbeta=sqrt(c(2.5, 2, 0.9, 0.7, 0.5)))


datatrain <- sim_data(n=1000, coeflist=coeflist)

datatest <- sim_data(n=1000, coeflist=coeflist)




# Load the required packages:
library("mlr3")
library("hierclass")

lgr::get_logger("mlr3")$set_threshold("warn")

# Set seed for reproducibility:
# set.seed(123)


# Define the task for the top-down classification rule:
task = as_task_classif(y ~ ., data = datatrain)


# Initialize the learner for the top-down classification rule:
learner = lrn("classif.topdown")



# set.seed(1234)

cv3 <- rsmp("repeated_cv", repeats = 1, folds = 2)

cv3$instantiate(task)
result_cv3 <- resample(task=task, learner=learner, resampling=cv3)

CV_vals <- c(result_cv3$aggregate(msr("classif.hierfbeta", type="micro")),
             result_cv3$aggregate(msr("classif.hierfbeta", type="macro")),
             result_cv3$aggregate(msr("classif.hierpr", type="micro")),
             result_cv3$aggregate(msr("classif.hierpr", type="macro")),
             result_cv3$aggregate(msr("classif.hierre", type="micro")),
             result_cv3$aggregate(msr("classif.hierre", type="macro")),
             result_cv3$aggregate(msr("classif.hloss")),
             result_cv3$aggregate(msr("classif.spath")),
             result_cv3$aggregate(msr("classif.acc")))




# set.seed(1234)

task$col_roles$stratum = task$target_names

cv3$instantiate(task)
result_cv3 <- resample(task=task, learner=learner, resampling=cv3)

stratCV_vals <- c(result_cv3$aggregate(msr("classif.hierfbeta", type="micro")),
                  result_cv3$aggregate(msr("classif.hierfbeta", type="macro")),
                  result_cv3$aggregate(msr("classif.hierpr", type="micro")),
                  result_cv3$aggregate(msr("classif.hierpr", type="macro")),
                  result_cv3$aggregate(msr("classif.hierre", type="micro")),
                  result_cv3$aggregate(msr("classif.hierre", type="macro")),
                  result_cv3$aggregate(msr("classif.hloss")),
                  result_cv3$aggregate(msr("classif.spath")),
                  result_cv3$aggregate(msr("classif.acc")))





datacompl <- rbind(datatrain, datatest)
ntrain <- nrow(datatrain); nall <- nrow(datacompl)
rm(datatrain, datatest); gc()


# Define the task for the top-down classification rule:
task = as_task_classif(y ~ ., data = datacompl)


learner$train(task, row_ids = 1:ntrain)


predictions = learner$predict(task, row_ids = (ntrain+1):nall)

truth_vals <- c(predictions$score(msr("classif.hierfbeta", type="micro")),
                predictions$score(msr("classif.hierfbeta", type="macro")),
                predictions$score(msr("classif.hierpr", type="micro")),
                predictions$score(msr("classif.hierpr", type="macro")),
                predictions$score(msr("classif.hierre", type="micro")),
                predictions$score(msr("classif.hierre", type="macro")),
                predictions$score(msr("classif.hloss")),
                predictions$score(msr("classif.spath")),
                predictions$score(msr("classif.acc")))



res <- data.frame(measure=c("hierf_micro", "hierf_macro", "hierpr_micro", 
                            "hierpr_macro", "hierre_micro", "hierre_macro",
                            "hloss", "spath", "acc"), CV_vals=CV_vals, 
                  stratCV_vals=stratCV_vals, truth_vals=truth_vals)



  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  


  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  set.seed(1234)
  
  datatrain <- sim_data(n=1000, coeflist=coeflist)
  
  
    datatest <- sim_data(n=200000, coeflist=coeflist)
  
  
  
  
  
  datacompl <- rbind(datatrain, datatest)
  ntrain <- nrow(datatrain); nall <- nrow(datacompl)
  # rm(datatrain, datatest); gc()
  
  
  # Define the task for the top-down classification rule:
  task = as_task_classif(y ~ ., data = datacompl)
  
  
  set.seed(1234)
  learner$train(task, row_ids = 1:ntrain)
  
  
  predictions = learner$predict(task, row_ids = (ntrain+1):nall)
  
  predictions$score(msr("classif.hierfbeta", type="micro"))
  predictions$score(msr("classif.hierfbeta", type="macro"))
  predictions$score(msr("classif.hierpr", type="micro"))
  predictions$score(msr("classif.hierpr", type="macro"))
  predictions$score(msr("classif.hierre", type="micro"))
  predictions$score(msr("classif.hierre", type="macro"))
  predictions$score(msr("classif.hloss"))
  predictions$score(msr("classif.spath"))
  predictions$score(msr("classif.acc"))
  
  
  
  
  
  