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
  
  nodelist <- nodelist[1:rightnodes[length(rightnodes)-2]]
  
  nlayer <- length(leftnodes)
  leftnodes <- leftnodes[-nlayer]
  rightnodes <- rightnodes[-nlayer]

  return(list(nodelist=nodelist, leftnodes=leftnodes, rightnodes=rightnodes))
  
}





# Function for plotting the tree structure:

# Function input:

# x: Output of gen_tree_structure()

# Function output:

# A ggplot2 plot of the tree structure.

plot_structure <- function(x) {
  
  require("ggplot2")
  require("ggforce")
  
  # Calculate the dimension of the plot:
  xw <- 1.5*(x$rightnodes[length(x$rightnodes)] - x$leftnodes[length(x$leftnodes)]) + 1
  xh <- 1.5*(length(x$leftnodes)-1) + 1
  
  
  # Calculate the positions of the circles and the start and end points of the lines
  # connecting the hierarchy:
  
  # Positions of the points:
  x0list <- y0list <- namelist <- list()
  
  # Start and end points of the lines:
  xyllist <- list() 
  
  x0list[[1]] <- 0.5 + 1.5*(0:(x$rightnodes[length(x$rightnodes)] - x$leftnodes[length(x$leftnodes)]))
  y0list[[1]] <- rep(0.5, length(x0list[[1]]))
  namelist[[1]] <- x$leftnodes[length(x$leftnodes)]:x$rightnodes[length(x$rightnodes)]
  
  for(count in 2:length(x$leftnodes)) {
    
    tempobj <- sapply(namelist[[count-1]], function(x1) which(sapply(x$nodelist, function(y1) x1 %in% y1)))
    namelist[[count]] <- unique(tempobj)
    x0list[[count]] <- tapply(x0list[[count-1]], tempobj, mean)
    y0list[[count]] <- rep(y0list[[count-1]][1] + 1.5, length(namelist[[count]]))
    
    tempnodes <- x$leftnodes[length(x$leftnodes)-count+1]:x$rightnodes[length(x$leftnodes)-count+1]
    
    xltemp <- c(x0list[[count-1]], x0list[[count]][sapply(tempobj, function(x) which(namelist[[count]]==x))])
    yltemp <- c(y0list[[count-1]] + 0.5, rep(y0list[[count]][1], length(namelist[[count-1]])) - 0.5)
    grouptemp <- rep(1:length(x0list[[count-1]]), 2)
    
    dftemp <- data.frame(xl=xltemp, yl=yltemp, group=grouptemp)
    
    if(count > 2)
      dftemp$group <- dftemp$group + xyllist[[count-2]]$group[length(xyllist[[count-2]]$group)]
    
    xyllist[[count-1]] <- dftemp
    
  }
  
  xyldf <- do.call("rbind", xyllist)
  xypdf <- data.frame(x0=unlist(x0list), y0=unlist(y0list), nam=unlist(namelist))
  
  
  # Make the ggplot2 plot (for 'geom_circle' the package 'ggforce' is required):
  
  p <- ggplot(data=xypdf) + geom_circle(aes(x0=x0, y0=y0, r=0.5)) + theme_bw() +
    geom_text(aes(x=x0, y=y0, label=nam)) +
    
    geom_line(data=xyldf,
              aes(x=xl, y=yl, group=group)) +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())
  p
  
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
