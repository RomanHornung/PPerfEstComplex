# Function for simulating the datasets.

# Function input:

# 

# Function output:

# A data.frame containing the simulated data.

simuldata <- function(n=500)
{
 
  
  
  
  # simdata <- 
  return(simdata)
}





# Function for randomly generating the tree structure:

# Function input:

# maxnleaf: The maximum number of leaf nodes; the tree is pruned
# by one level as soon as the number of leaf nodes exceed 'maxnleaf'.

# Function output:

# A list representing the tree structure. More precisely, this list provides
# for each node its child nodes. For example, a list entry "[[3]] 7 8" means
# that node "3" has nodes "7" and "8" as child nodes.

gen_tree_structure <- function(maxnleaf=20) { 
  
  nodelist <- list()
  leftnodes <- rightnodes <- 0
  
  count <- 1
  
  leftnodes[count] <- rightnodes[count] <- 1
  count <- count+1
  
  numnodes <- sample(2:3, size=1, prob=c(2,1))
  numnodes
  if(numnodes > maxnleaf)
    stop("bla bla bla.")
  
  nodelist[[1]] <- 1 + (1:numnodes)
  
  leftnode <- min(nodelist[[1]])
  rightnode <- max(nodelist[[1]])
  
  nleaf <- rightnode-(leftnode-1)
  
  leftnodes[count] <- leftnode
  rightnodes[count] <- rightnode
  
  count <- count+1
  
  
  while(nleaf <= maxnleaf) {
    
    for(i in leftnode:rightnode) {
      numnodes <- sample(2:3, size=1, prob=c(2,1))
      nodelist[[i]] <- max(nodelist[[i-1]]) + (1:numnodes)
    }
    
    leftnode <- min(nodelist[[leftnode]])
    rightnode <- max(nodelist[[rightnode]])
    
    nleaf <- rightnode-(leftnode-1)
    
    leftnodes[count] <- leftnode
    rightnodes[count] <- rightnode
    
    count <- count+1
    
  }
  
  leftnodes[length(leftnodes)-2]
  nodelist <- nodelist[1:rightnodes[length(rightnodes)-2]]
  
  return(nodelist)
  
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

simulation <- function(niter, N, ni, beta, sdbinter, sdbslope, sdeps, type, fixed)
{
  
  require("mlr3")
  require("mlr3verse")
  require("data.table")
  
  mse_subsamp0.8 <- numeric(niter)
  mse_subsamp0.8g <- numeric(niter)
  learner_linreg <- lrn("regr.lm")
  
  for (i in 1:niter)
  {
    print(i)
    simuldati <- simuldata(N=N, ni=ni, beta=beta, sdbinter=sdbinter, sdbslope=sdbslope, sdeps=sdeps, type=type, fixed=fixed)[, -2]
    task_i <- as_task_regr(simuldati, target="y")
    task_i$set_col_roles(cols="index", remove_from="feature")
    subsamp0.8 <- rsmp("subsampling", repeats = 100, ratio = 0.8)
    subsamp0.8$instantiate(task_i)
    result_subsamp0.8 <- resample(task=task_i, learner=learner_linreg, resampling=subsamp0.8)
    mse_subsamp0.8[i] <- result_subsamp0.8$aggregate(msr("regr.mse"))
    
    task_i$col_roles$group = "index"
    subsamp0.8$instantiate(task_i)
    result_subsamp0.8g <- resample(task=task_i, learner=learner_linreg, resampling=subsamp0.8)
    mse_subsamp0.8g[i] <- result_subsamp0.8g$aggregate(msr("regr.mse"))
    
  }
  result <- list(mse_subsamp0.8=mse_subsamp0.8, mse_subsamp0.8g=mse_subsamp0.8g)
  save(result, file=paste("./Simulations/ClustData/Results/N", N, "ni", ni, "beta", paste(beta, collapse=""), "sdbinter", sdbinter, "sdbslope", sdbslope, "sdeps", sdeps, type, fixed, ".RData", sep=""))
  
}
