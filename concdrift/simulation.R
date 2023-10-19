# DONE:

# Set working directory:

setwd("~/PPerfEstComplex")



# Make table of settings:

repetition <- 1:100
xtrend <- c("none", "weak", "medium", "strong")
ytrend <- c("none", "weak", "medium", "strong")
n <- c(100, 500, 1000, 3000)

scenariogrid <- expand.grid(repetition=repetition, ytrend=ytrend, xtrend=xtrend, 
  n=n, stringsAsFactors = TRUE)
scenariogrid <- scenariogrid[,ncol(scenariogrid):1, drop=FALSE]

set.seed(1234)
seeds <- sample(1000:10000000, size=nrow(scenariogrid))#sample(1000:10000000, size=length(n)*length(ytrend)*length(xtrend)*length(repetition))

scenariogrid$seed <- seeds#rep(seeds, each=length(predmethod))

set.seed(1234)
reorderind <- sample(1:nrow(scenariogrid))
scenariogrid <- scenariogrid[reorderind,,drop=FALSE]
rownames(scenariogrid) <- NULL



# Save scenariogrid, needed in evaluation of the results:

save(scenariogrid, file="./concdrift/results/intermediate_results/scenariogrid.Rda")


wd <- getwd()


# Source the functions that are used in performing the calculations 
# on the cluster:

source("./concdrift/functions.R")



# Start the cluster:

# NOTE: This syntax requires the use of the RMPISNOW script, see the README file
# contained in the root folder "PPerfEstComplex".

cl <- makeCluster()



# Export the objects in the workspace to the
# parallel jobs:

clusterExport(cl, list=ls())



# Perform the calculations:

results <- parLapply(cl, 1:nrow(scenariogrid), function(z)
  try({evaluatesetting(z)}))

  

# Save the results:  
  
save(results, file="./concdrift/results/intermediate_results/results.Rda")  
  
  
# Stop the cluster:

stopCluster(cl)
