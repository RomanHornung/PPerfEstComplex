# This function performs one repetition of the simulation.
#
# It takes the whole number 'iter', which corresponds to the iter-th line 
# of 'scenariogrid', which contains the necessary information
# on the iter-th setting.

evaluatesetting <- function(iter) {
  
  setwd(wd)
    
  # Obtain information for the iter-th setting:
  
  n <- scenariogrid$n[iter]
  seed_start <- scenariogrid$seed_start[iter]
  seed_res <- scenariogrid$seed_res[iter]


  # Set seed:

  set.seed(seed_start)
  

# Simulate the training data set and the huge test data set:
  
  load("./hierpr/results/intermediate_results/treestruc.Rda")

  coeflist <- simulate_coefs(treestruc=treestruc, sdbeta0=sqrt(1),
                             sdbeta=sqrt(c(2.5, 2, 0.9, 0.7, 0.5)))
  
  datatrain <- sim_data(n=n, coeflist=coeflist)
  
  datatest <- sim_data(n=200000, coeflist=coeflist)
  

  
  # Include in the huge test data sets only classes that were also contained in the training
  # data (this is the only difference to "functions.R"!):
  
  datatest <- datatest[as.character(datatest$y) %in% unique(as.character(datatrain$y)),]
  ntest <- nrow(datatest)

  
  
  # Load the required packages:
  
  library("mlr3")
  library("hierclass")
  
  lgr::get_logger("mlr3")$set_threshold("warn")


  # Define the task for the top-down classification rule:
  
  task = as_task_classif(y ~ ., data = datatrain)
  
  
  # Initialize the learner for the top-down classification rule:
  
  learner = lrn("classif.topdown")
  
  
  
  
  # Perform standard CV:
  
  set.seed(seed_res)
  
  cv3 <- rsmp("repeated_cv", repeats = 10, folds = 5)
  
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
  
  
  
  
  # Perform stratified CV:
  
  set.seed(seed_res)
  
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
  
  
  
  
  # Approximate the true performance metrics values using the huge test data set:
  
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
  
  
  
  # Combine the results:
  
  res <- data.frame(measure=c("hierf_micro", "hierf_macro", "hierpr_micro", 
                              "hierpr_macro", "hierre_micro", "hierre_macro",
                              "hloss", "spath", "acc"), CV_vals=CV_vals, 
                    stratCV_vals=stratCV_vals, truth_vals=truth_vals, ntest=ntest)
  
  
  # Return the results:
  
  return(res)
  
}










# Function to simulate a dataset.

# Function input:

# n: number of observations to simulate
# coeflist: result of simulate_coefs. A list of the coefs to be used in the
# simulation as well as other information about the tree structure.

# Function output:

# A list containing the following:
# data: The simulated data.frame.
# coeflist: The input list 'coeflist' to which the simulated data associated
# with each node was added during the execution of sim_data()

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


  # Assign the child node classes of the observations in all other subsequent nodes:

  for(i in 2:length(coeflist)) {

    # Determine the subset of observations that are in the i-th node:
    subs <- which(outcomemat[,coeflist[[i]]$layer-1]==i)
    
    if (length(subs) > 0) {
    
    # Determine the child nodes for this subset of observations:
    tempclass <- coeflist[[i]]$childnodes[get_child_nodes(X[subs, , drop=FALSE], coeflist[[i]]$coefs, i)]

    # Assign these child nodes to the corresponding rows in 'outcomemat':
    outcomemat[subs,coeflist[[i]]$layer] <- tempclass
    
    }

  }

  # Bring the outcome into the format needed by 'hierclass':
  ystring <- apply(outcomemat, 1, function(x) paste(x, collapse="."))

  allclasses <- sort(unique(unlist(lapply(lapply(unique(ystring), function(x) strsplit(x, split="\\.")[[1]]), function(x) lapply(1:length(x), function(y) paste(x[1:y], collapse="."))))))

  # Make the data.frame:
  data <- data.frame(X)
  data$y <- factor(ystring, levels=allclasses)

  return(data)

}






# Function to simulate the coefficients.

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












# Function to randomly generate a tree structure.

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





# Function to plot a tree structure.

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
