# DONE
#######

# Set working directory:

setwd("Z:/Projects/DESTATIS/PredErrorComplex/PPerfEstComplex")



# Source the "functions.R" script that contains the functions
# gen_tree_structure() and plot_structure() for generating and
# plotting tree structures:

source("./hierpr/functions.R")



# Generate the tree structure:

# Keep generating new tree structures until a tree structure
# with exactly 50 leaf nodes results:

nleaf <- 0
count <- 0

set.seed(12)

while(nleaf!=50) {
  tempobj <- gen_tree_structure(50)
  nleaf <- tempobj$rightnodes[length(tempobj$rightnodes)] - tempobj$leftnodes[length(tempobj$leftnodes)] + 1
  count <- count + 1
}
count

treestruc <- tempobj



# Plot the resulting tree structure:

plot_structure(treestruc)



# Number of internal and lead nodes:

sum(sapply(1:length(treestruc$leftnodes), function(x) treestruc$rightnodes[x] - treestruc$leftnodes[x] + 1))



# Save the tree structure because it is used in the simulation study:

save(treestruc, file="./hierpr/results/intermediate_results/treestruc.Rda")
