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
  save(result, file=paste("./clustdata/Results/N", N, "ni", ni, "beta", paste(beta, collapse=""), "sdbinter", sdbinter, "sdbslope", sdbslope, "sdeps", sdeps, type, fixed, ".RData", sep=""))
  
}


























# Function to simulate the coefficients:


# Function input:

# treestruc: A list. The tree structure.
# sdbeta0: The standard deviation of the normal distribution from which
# the intercepts are drawn.
# sdbeta: The standard deviation of the normal distribution from which
# the coefficients are drawn. These depend on the layer of the tree because
# lower tree layers should feature less predictive signal.

# Function output:

# A list containing, for each internal node the coefs to be used in the
# simulation, the child nodes, the parent nodes, and the layer of each node.

simulate_coefs <- function(treestruc, sdbeta0=sqrt(1),
                           sdbeta=sqrt(c(2.5, 2, 0.9, 0.7, 0.5))) {

  # Make a list that will contain the simulated coefficients as well as
  # information on the tree structure (specifying the position of the
  # node in the trees):
  
  coeflist <- vector(mode = "list", length = length(treestruc$nodelist))
  
  
  # Add the information on the tree structure:
  
  for(i in seq(along=coeflist)) {
    
    coeflist[[i]] <- list()

    # Add the information on the child nodes for each node:
    coeflist[[i]]$childnodes <- treestruc$nodelist[[i]]

    # Add the information on the parent nodes for each node:
    coeflist[[i]]$parentnodes <- which(sapply(1:length(coeflist), function(x) i %in% coeflist[[x]]$childnodes))

    # Add the layer of each node:
    coeflist[[i]]$layer <- which(sapply(1:length(treestruc$leftnodes), function(x) (i >= treestruc$leftnodes[x]) & (i <= treestruc$rightnodes[x])))

  }

  # Simulate the coefficients:

  for(i in seq(along=coeflist)) {

    # If the node has only two child nodes, we need only one set of coefficients:
    if(length(coeflist[[i]]$childnodes)==2) {
      coefs <- matrix(nrow=1, ncol=6, data=c(rnorm(1, sd=sdbeta0), rnorm(5, sd=sdbeta[coeflist[[i]]$layer])))
    }
    # If the node has three child nodes, we need two sets of coefficients:
    if(length(coeflist[[i]]$childnodes)==3) {
      coefs <- rbind(c(rnorm(1, sd=sdbeta0), rnorm(5, sd=sdbeta[coeflist[[i]]$layer])),
                     c(rnorm(1, sd=sdbeta0), rnorm(5, sd=sdbeta[coeflist[[i]]$layer])))
    }

    coeflist[[i]]$coefs <- coefs

  }

  return(coeflist)

}




# Function that takes the covariate matrix of the subset of observations
# contained in a node and the coefficients associated with that node
# to output the indices of the child nodes to which the observations
# get assigned to.
# Note that the assignments are performed according to a multinomial
# regression model.


# Function input:

# Xsub: The matrix of observations in the current node. Observations in
# rows and variables in columns.
# coefs: The matrix of coefficients associated with the current node.

# Function output:

# A vector that gives the index of the child node for each observation.

get_child_nodes <- function(Xsub, coefs, i) {

  # Determine the (unstandardized) probabilities for each child node:
  
  desmat <- cbind(1, Xsub)

  # if(i == 34) {
  # 
  #   cat("hier", "\n")
  # 
  #   cat("ist es", "\n")
  # 
  # }
  
  if (nrow(coefs)==2)
    vProb <- cbind(1, exp(desmat%*%coefs[1,]), exp(desmat%*%coefs[2,]))
  else
    vProb <- cbind(1, exp(desmat%*%coefs[1,]))

  # Draw the child nodes based on the probabilities obtained in the
  # first step:
  
  mChoices <- t(apply(vProb, 1, rmultinom, n = 1, size = 1))
  
  
  # Make a vector of the child node indices:
  
  ys <- apply(mChoices, 1, function(x) which(x==1))

  return(ys)

}







# Function to simulate the data:


# Function input:

# n: Number of observations to simulate.
# coeflist: Result of simulate_coefs. A list that the coefs to be used in the
# simulation as well as other information about the tree structure.

# Function output:

# A list containing the following:
# data: The simulated data.frame.
# coeflist: The input list 'coeflist' to which the simulated data associated
# with each node was added.

sim_data <- function(n, coeflist) {

  # Simulate the covariate matrix:
  
  X <- matrix(nrow=n, ncol=5, rnorm(n*5))


  # Simulate the outcome:
  
  maxlayer <- max(sapply(coeflist, function(x) x$layer))

  # Outcome matrix with 5 columns, where the j-th column
  # will contain the classes of the observations in the
  # j-th layer:
  outcomemat <- matrix(nrow=nrow(X), ncol=maxlayer)

  
  # Assign the child node classes of the observations in the root node:

  # Determine the child nodes of the root node:
  tempclass <- coeflist[[1]]$childnodes[get_child_nodes(X, coeflist[[1]]$coefs, 1)]

  # Assign these child node classes:
  outcomemat[,1] <- tempclass

  # Store the data associated with the node also in 'coeflist':
  coeflist[[1]]$datanode <- data.frame(X)
  coeflist[[1]]$datanode$y <- factor(tempclass)


  # Assign the child node classes of the observations in all other subsequent nodes:

  for(i in 2:length(coeflist)) {

    # if(i == 34) {
    # 
    #   cat("hier", "\n")
    # 
    #   cat("ist es", "\n")
    # 
    # }
    
    # Determine the subset of observations that are in the i-th node:
    subs <- which(outcomemat[,coeflist[[i]]$layer-1]==i)
    
    # cat(i, "\n")
    # Sys.sleep(0.5)
    
    # if(i == 2) {
    # 
    #   cat("hier", "\n")
    # 
    #   cat("ist es", "\n")
    # 
    # }
    
    if (length(subs) == 0)
      coeflist[[i]]$datanode <- NA
    else {
    
    # Determine the child nodes for this subset of observations:
    tempclass <- coeflist[[i]]$childnodes[get_child_nodes(X[subs, , drop=FALSE], coeflist[[i]]$coefs, i)]

    # Assign these child nodes to the corresponding rows in 'outcomemat':
    outcomemat[subs,coeflist[[i]]$layer] <- tempclass

    # Store the data associated with the node also in 'coeflist':
    coeflist[[i]]$datanode <- data.frame(X[subs, , drop=FALSE])
    coeflist[[i]]$datanode$y <- factor(tempclass)
    
    }

  }

  # Bring the outcome into the format needed by 'hierclass':
  ystring <- apply(outcomemat, 1, function(x) paste(x, collapse="."))

  # Make the data.frame:
  data <- data.frame(X)
  data$y <- factor(ystring)

  return(list(data=data, coeflist=coeflist, outcomemat=outcomemat))

}







# Function used to evaluate how well the simulated data
# works:


# Function input:

# data: simulated data as output by sim_data().
# ntrain: Number of observations used for training
#  (the rest is used for testing).

# Function output:

# A list containing the following:
# precs: A vector of length 5, where the j-th element is the precision at the j-th layer.
# precscond: A vector of length 5, where the j-th element is the 'conditional precision' at the j-th layer,
# meaning the number of observations correctly predicted at the j-th layer divided by the number
# of observations correctly predicted a the j-1-th layer.

eval_perm <- function(data, ntrain=round(nrow(data)*(2/3))) {

  
  # Train the prediction rule on the training data and obtain predictions
  # for the test data:
  
  # Load the required packages:
  library("mlr3")
  library("hierclass")

  # Define the task for the top-down classification rule:
  task = as_task_classif(y ~ ., data = data)

  # Initialize the learner for the top-down classification rule:
  learner = lrn("classif.topdown")

  learner$train(task, row_ids = 1:ntrain)

  predictions = learner$predict(task, row_ids = (ntrain+1):nrow(data))


  
  # Data frame containing the true and predicted observations:
  
  truthpred <- data.frame(truth=data[(ntrain+1):nrow(data),]$y, pred=predictions$response)

  
  
  # Subset the above data frame to only contain the observations correctly predicted
  # at the j-th layer:
  
  truthpred1 <- truthpred[sapply(as.character(truthpred$truth), function(x) strsplit(x, split="\\.")[[1]][1])==
              sapply(as.character(truthpred$pred), function(x) strsplit(x, split="\\.")[[1]][1]),]

  truthpred2 <- truthpred[sapply(as.character(truthpred$truth), function(x) paste(strsplit(x, split="\\.")[[1]][1:2], collapse="."))==
              sapply(as.character(truthpred$pred), function(x) paste(strsplit(x, split="\\.")[[1]][1:2], collapse=".")),]

  truthpred3 <- truthpred[sapply(as.character(truthpred$truth), function(x) paste(strsplit(x, split="\\.")[[1]][1:3], collapse="."))==
              sapply(as.character(truthpred$pred), function(x) paste(strsplit(x, split="\\.")[[1]][1:3], collapse=".")),]

  truthpred4 <- truthpred[sapply(as.character(truthpred$truth), function(x) paste(strsplit(x, split="\\.")[[1]][1:4], collapse="."))==
              sapply(as.character(truthpred$pred), function(x) paste(strsplit(x, split="\\.")[[1]][1:4], collapse=".")),]

  truthpred5 <- truthpred[as.character(truthpred$truth)==as.character(truthpred$pred),]

  
  
  # Calculate the precisions at the different layers:
  
  precs <- c(nrow(truthpred1),
           nrow(truthpred2),
           nrow(truthpred3),
           nrow(truthpred4),
           nrow(truthpred5))/(nrow(data)-ntrain)

  
  
  # Calculate the conditional precisions at the different layers:
  
  precscond <- c(nrow(truthpred1)/(nrow(data)-ntrain),
            nrow(truthpred2)/nrow(truthpred1),
            nrow(truthpred3)/nrow(truthpred2),
            nrow(truthpred4)/nrow(truthpred3),
            nrow(truthpred5)/nrow(truthpred4))

  
  return(list(precs=precs, precscond=precscond, ntrain=ntrain, ntest=nrow(data)-ntrain))

}

