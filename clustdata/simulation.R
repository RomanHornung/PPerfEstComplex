####################################################################################

# NOTE: Before the code can be executed, the R working directory *MUST* 
# be set to the directory of the 'PPerfEstComplex' folder (for Linux
# systems, R can also be run directly from the 'PPerfEstComplex'
# directory):

# Remove the '#' from the line below and replace 'this/is/my/path/' by the path
# to the directory that contains 'PPerfEstComplex':

# setwd("this/is/my/path/PPerfEstComplex")

####################################################################################



# Make table of settings:

scenariogrid <- expand.grid(repetition=1:100, fixed=c("none", "first", "second"), sdeps=c(1,0.5), sdbslope=c(1,0), sdbinter=c(1,0), ni=c(5,25), N=c(10,50), stringsAsFactors = TRUE)
scenariogrid <- scenariogrid[,ncol(scenariogrid):1, drop=FALSE]

set.seed(1234)
seeds <- sample(1000:10000000, size=nrow(scenariogrid))

scenariogrid$seed <- seeds

set.seed(1234)
reorderind <- sample(1:nrow(scenariogrid))
scenariogrid <- scenariogrid[reorderind,,drop=FALSE]
rownames(scenariogrid) <- NULL



# Save scenariogrid, needed in evaluation of the results:

save(scenariogrid, file="./clustdata/results/intermediate_results/scenariogrid.Rda")




# Source the functions that are used in performing the calculations 
# on the cluster:

source("./clustdata/functions.R")



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

save(results, file="./clustdata/results/intermediate_results/results.Rda")  



# Stop the cluster:

stopCluster(cl)
