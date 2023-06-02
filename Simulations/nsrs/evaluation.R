setwd("C:/Projects/DESTATIS/PredErrorComplex/PPerfEstComplex")


# Load the results:

load("./Simulations/nsrs/results/intermediate_results/results.Rda")
load("./simulations/nsrs/results/intermediate_results/scenariogrid.Rda")

reorderind <- order(scenariogrid$N, scenariogrid$repetition, scenariogrid$correct_model)
results <- results[reorderind]
scenariogrid <- scenariogrid[reorderind,]

head(scenariogrid)
head(results)


results <- do.call("rbind", results)


library("ggplot2")
library("dplyr")

dim(scenariogrid)
length(results)

head(scenariogrid)

head(results)




# Visualise the results and analyse the bias:

# compare unbiased and biased version of estimates
ggframe <- rbind(results %>% mutate(type = "biased"),
                results %>% mutate(estimated_error = corrected,
                                     type = "de-biased (HT)")
                ) %>%
          mutate(bias       = estimated_error - true_error)

results %>% group_by(model, n, correct) %>% summarise(bias = mean(corrected - true_error))
results %>% group_by(model, n, correct) %>% summarise(bias = mean(estimated_error - true_error))


ggframe$type
ggframe$correct

ggframe$correctmod <- "model correctly specified"
ggframe$correctmod[!ggframe$correct] <- "model misspecified"
ggframe$correctmod <- factor(ggframe$correctmod, levels=c("model correctly specified", "model misspecified"))

unique(ggframe$n)
ggframe$n <- paste0("n = ", ggframe$n)
ggframe$n <- factor(ggframe$n, levels=c("n = 100", "n = 500", "n = 1000"))

ggframe$model[ggframe$model=="regr.lm"] <- "linear models"
ggframe$model[ggframe$model=="regr.ranger"] <- "random forests"
ggframe$model <- factor(ggframe$model, levels=c("linear models", "random forests"))

# look at all settings jointly
p <- ggplot(data=ggframe, aes(y=bias, x = model, fill = type)) +
  geom_boxplot() + ylab("Difference between estimated and true MSE values") +
  facet_wrap(~correctmod+n, nrow=2, scales="free_y") + 
  theme_bw() + theme(axis.title.x=element_blank(), 
                     axis.title.y=element_text(size=19), 
                     axis.text.x = element_text(angle=45, hjust = 1, color="black", size=19), 
                     axis.text.y = element_text(color="black", size=14), 
                     strip.text = element_text(size=19),
                     legend.position = "none")
p

ggsave("./Simulations/nsrs/results/figures/nsrs.pdf", width=14, height=9)












source("./Simulations/nsrs/functions.R")




# Make table of settings:

scenariogrid <- expand.grid(repetition=1:100, correct_model = c(TRUE, FALSE), N = c(10000, 50000, 100000), stringsAsFactors = TRUE)
scenariogrid <- scenariogrid[,ncol(scenariogrid):1, drop=FALSE]

set.seed(1234)
seeds <- sample(1000:10000000, size=nrow(scenariogrid))

scenariogrid$seed <- seeds


set.seed(1234)
reorderind <- sample(1:nrow(scenariogrid))
scenariogrid <- scenariogrid[reorderind,,drop=FALSE]
rownames(scenariogrid) <- NULL






evaluatesetting2 <- function(iter) {
  
  # Obtain information for the iter-th setting:
  N <- scenariogrid$N[iter] 
  correct_model <- scenariogrid$correct_model[iter] 
  repetition <- scenariogrid$repetition[iter] 
  seed <- scenariogrid$seed[iter] 
  
  # Set seed:
  
  set.seed(seed)
  
  result <- simulation2(N=N, correct_model=correct_model, repetition=repetition)
  
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

simulation2 <- function(N, correct_model, repetition, b0 = 5, b1 = 1, b2 = 1)
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
  n <- 1000
  #####correct_model <- experiments$correct_model[k]
  
  train_data <- generate_data(N=100000, b0, b1, b2)
  test_data <- generate_data(N=1000, b0, b1, b2)
  
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
  ids <- sample(1:100000, n, prob = w)
  sample_weights <- w[ids]
  
  # if not the correct model exclude one of the covariates
  if(!correct_model){
    train_data <- train_data %>% select(-x2)
    test_data <- test_data %>% select(-x2)
  }
  
  # Take the sample
  train_data <- train_data[ids,]
  
  return(list(train_data=train_data, test_data=test_data))
  
}  
  


head(scenariogrid)


ui <- evaluatesetting2(4)

names(ui)

train_data <- ui$train_data
test_data <- ui$test_data

dim(train_data)

boxplot(train_data$y, test_data$y[sample(1:nrow(test_data), size=100)])


par(mfrow=c(2,1))
hist(train_data$y, 100, xlim=range(c(train_data$y, test_data$y)))
hist(test_data$y, 100, xlim=range(c(train_data$y, test_data$y)))
par(mfrow=c(1,1))

qqplot(train_data$y, test_data$y)
abline(c(0,1))
