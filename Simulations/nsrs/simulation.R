# Set working directory:

setwd("~/PPerfEstComplex")



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



# Save scenariogrid, needed in evaluation of the results:

save(scenariogrid, file="./simulations/nsrs/results/intermediate_results/scenariogrid.Rda")




# Source the functions that are used in performing the calculations 
# on the cluster:

source("./simulations/nsrs/functions.R")




# Start the cluster:

# NOTE: This syntax requires the use of the RMPISNOW script, see the README file
# contained in the root folder "PPerfEstComplex".

cl <- makeCluster()



# Export the objects in the workspace to the
# parallel jobs:

clusterExport(cl, list=ls())



# Perform the calculations and save the results (saving is performed within 'evaluatesetting'):

results <- parLapply(cl, 1:nrow(scenariogrid), function(z)
  try({evaluatesetting(z)}))

  

# Save the results:  
  
save(results, file="./simulations/nsrs/results/intermediate_results/results.Rda")  
  
  
# Stop the cluster:

stopCluster(cl)
