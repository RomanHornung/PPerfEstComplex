setwd("Z:/Projects/DESTATIS/PredErrorComplex/PPerfEstComplex")


source("./simulations/hierpr/functions.R")


if (FALSE) {

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
  plot_structure(treestruc)

  save(treestruc, file="./simulations/hierpr/results/intermediate_results/treestruc.Rda")

  sum(sapply(1:length(treestruc$leftnodes), function(x) treestruc$rightnodes[x] - treestruc$leftnodes[x] + 1))

}



# -	Erster Schritt ist die hierarchische Baumstruktur festlegen.
# -	Jeder interne Knoten ist mit einem lokalen Modell verknüpft, wobei die Unterscheidbarkeit der Klassen geringer werden soll, je weiter unten man in der hierarchischen Struktur ankommt. Das wird realisiert in dem das Rauschen (epsilon) immer weiter vergrößert wird, je weiter man nach unten kommt.
# -	Dass die Koeffizienten von vertikal benachbarten Schichten sich ähnlich sind, wird so realisiert, dass die Koeffizenten aus Normalverteilungen mit Mittelwerten gleich den Koeffizienten der darüberliegenden Schichten gezogen werden.




# unterschiedlche intercept sind, wichtig, damit ungeliche klassengrößen entstehen.
# die intercept von benachbarten schichten sollen nicht voneinander abhängen, weil
# die degrees der klassenungleichheiten nicht voneinander abhägne sollen.



# n <- 50000
#
# # erster coefficent intercept, rest sind die beeinflussenden betas:
# vCoef1 = rep(0, 6)
# vCoef2 = rnorm(6)
# vCoef3 = rnorm(6)
#
# # sim_multinom <- function(n, coefs) {
#
# mX = cbind(rep(1, n), matrix(rnorm(n*5), n, 5))
#
# # vector of probabilities
# vProb = cbind(exp(mX%*%vCoef1), exp(mX%*%vCoef2), exp(mX%*%vCoef3))
#
# # multinomial draws
# mChoices = t(apply(vProb, 1, rmultinom, n = 1, size = 1))
# dfM = cbind.data.frame(y = apply(mChoices, 1, function(x) which(x==1)), mX)
#
# library("nnet")
# m <- multinom(y ~ ., data = dfM)
# summary(m)
#
# vCoef2
# vCoef3



50*20
50*50

# 1. coefficenten simulieren:
#

# 2. Daten simulieren

# 3. Daten testen:
# da brauch ich




load("./simulations/hierpr/results/intermediate_results/treestruc.Rda")






# Variance of the normal distribution from which the intercepts are drawn:
# sdbeta0 <- sqrt(1)

# The betas have layer-specific variances:
# sdbeta <- sqrt(c(1, 1.5, 2, 2.5, 3))

# treestruc

simulate_coefs <- function(treestruc, sdbeta0=sqrt(1),
                           sdbeta=sqrt(c(1, 1.5, 2, 2.5, 3))) {


  coeflist <- vector(mode = "list", length = length(treestruc$nodelist))
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

  maxlayer <- max(sapply(coeflist, function(x) x$layer))


  for(i in seq(along=coeflist)) {

    # if(coeflist[[i]]$layer==maxlayer) {
    #   coeflist[[i]]$coefs <- NA
    # } else {

    if(length(coeflist[[i]]$childnodes)==2) {
      coefs <- matrix(nrow=1, ncol=6, data=c(rnorm(1, sd=sdbeta0), rnorm(5, sd=sdbeta[coeflist[[i]]$layer])))
    }
    if(length(coeflist[[i]]$childnodes)==3) {
      coefs <- rbind(c(rnorm(1, sd=sdbeta0), rnorm(5, sd=sdbeta[coeflist[[i]]$layer])),
                     c(rnorm(1, sd=sdbeta0), rnorm(5, sd=sdbeta[coeflist[[i]]$layer])))
    }

    coeflist[[i]]$coefs <- coefs

    # }

  }

  return(coeflist)

}




# Function that takes the covariate matrix of the subset of observations
# contained in a node and the coefficients associated with that node
# to output the indices of the child nodes to which the observations
# get assigned to.

get_child_nodes <- function(Xsub, coefs) {

  desmat <- cbind(1, Xsub)

  if (nrow(coefs)==2)
    vProb <- cbind(1, exp(desmat%*%coefs[1,]), exp(desmat%*%coefs[2,]))
  else
    vProb <- cbind(1, exp(desmat%*%coefs[1,]))

  mChoices <- t(apply(vProb, 1, rmultinom, n = 1, size = 1))
  ys <- apply(mChoices, 1, function(x) which(x==1))

  return(ys)

}







sim_data <- function(n, coeflist) {

  X <- matrix(nrow=n, ncol=5, rnorm(n*5))


  maxlayer <- max(sapply(coeflist, function(x) x$layer))

  outcomemat <- matrix(nrow=nrow(X), ncol=maxlayer)


  tempclass <- coeflist[[1]]$childnodes[get_child_nodes(X, coeflist[[1]]$coefs)]

  outcomemat[,1] <- tempclass

  coeflist[[1]]$datanode <- data.frame(X)
  coeflist[[1]]$datanode$y <- factor(tempclass)



  for(i in 2:length(coeflist)) {

    subs <- outcomemat[,coeflist[[i]]$layer-1]==i
    tempclass <- coeflist[[i]]$childnodes[get_child_nodes(X[subs,], coeflist[[i]]$coefs)]

    outcomemat[subs,coeflist[[i]]$layer] <- tempclass

    coeflist[[i]]$datanode <- data.frame(X[subs,])
    coeflist[[i]]$datanode$y <- factor(tempclass)

  }

  ystring <- apply(outcomemat, 1, function(x) paste(x, collapse="."))

  data <- data.frame(X)
  data$y <- factor(ystring)

  return(list(data=data, coeflist=coeflist))

}







eval_perm <- function(data, ntrain=round(nrow(data)*(2/3))) {

  # Load the required packages:
  library("mlr3")
  library("hierclass")

  # Define the task for the top-down classification rule:
  task = as_task_classif(y ~ ., data = data)

  # Initialize the learner for the top-down classification rule:
  learner = lrn("classif.topdown")

  learner$train(task, row_ids = 1:ntrain)

  predictions = learner$predict(task, row_ids = (ntrain+1):nrow(data))


  ui <- data.frame(truth=data[(ntrain+1):nrow(data),]$y, pred=predictions$response)

  ui0 <- ui[sapply(as.character(ui$truth), function(x) strsplit(x, split="\\.")[[1]][1])!=
              sapply(as.character(ui$pred), function(x) strsplit(x, split="\\.")[[1]][1]),]

  ui1 <- ui[sapply(as.character(ui$truth), function(x) strsplit(x, split="\\.")[[1]][1])==
              sapply(as.character(ui$pred), function(x) strsplit(x, split="\\.")[[1]][1]),]

  ui2 <- ui[sapply(as.character(ui$truth), function(x) paste(strsplit(x, split="\\.")[[1]][1:2], collapse="."))==
              sapply(as.character(ui$pred), function(x) paste(strsplit(x, split="\\.")[[1]][1:2], collapse=".")),]

  ui3 <- ui[sapply(as.character(ui$truth), function(x) paste(strsplit(x, split="\\.")[[1]][1:3], collapse="."))==
              sapply(as.character(ui$pred), function(x) paste(strsplit(x, split="\\.")[[1]][1:3], collapse=".")),]

  ui4 <- ui[sapply(as.character(ui$truth), function(x) paste(strsplit(x, split="\\.")[[1]][1:4], collapse="."))==
              sapply(as.character(ui$pred), function(x) paste(strsplit(x, split="\\.")[[1]][1:4], collapse=".")),]

  ui5 <- ui[as.character(ui$truth)==as.character(ui$pred),]

  aha <- c(nrow(ui1),
           nrow(ui2),
           nrow(ui3),
           nrow(ui4),
           nrow(ui5))/(nrow(data)-ntrain)

  accs <- c(nrow(ui1)/(nrow(data)-ntrain),
            nrow(ui2)/nrow(ui1),
            nrow(ui3)/nrow(ui2),
            nrow(ui4)/nrow(ui3),
            nrow(ui5)/nrow(ui4))

  return(list(precs=aha, precscond=accs, ntrain=ntrain, ntest=nrow(data)-ntrain))

}



# res1_s:

# sqrt(1),
# sdbeta=sqrt(c(1, 1.5, 2, 2.5, 3))

# res2_s:

# sqrt(1),
# sdbeta=sqrt(c(1.3, 1.5, 1.7, 1.9, 2.1))

# sqrt(1),
# sdbeta=sqrt(c(1.5, 1.3, 1.1, 0.9, 0.7))

# sqrt(1),
# sdbeta=sqrt(c(2, 1.5, 0.9, 0.7, 0.5))

# sqrt(1),
# sdbeta=sqrt(c(2.5, 2, 0.9, 0.7, 0.5))



diff(c(2, 1.5, 0.9, 0.7, 0.5))


res1_s <- res
res1_l <- res2



# innerclasses

set.seed(1234)

res5_s <- list()

for(i in 1:10) {

  coeflist <- simulate_coefs(treestruc=treestruc, sdbeta0=sqrt(1),
                             sdbeta=sqrt(c(2.5, 2, 0.9, 0.7, 0.5)))

  dataobj <- sim_data(n=1000, coeflist=coeflist)

  res5_s[[i]] <- eval_perm(data=dataobj$data)

  cat(paste("Iteration: ", i), "\n")

}

res5_s





res5_l <- list()

for(i in 1:10) {

  coeflist <- simulate_coefs(treestruc=treestruc, sdbeta0=sqrt(1),
                             sdbeta=sqrt(c(2.5, 2, 0.9, 0.7, 0.5)))

  dataobj <- sim_data(n=3000, coeflist=coeflist)

  res5_l[[i]] <- eval_perm(data=dataobj$data)

  cat(paste("Iteration: ", i), "\n")

}

res5_l



aha <- as.character(dataobj$data$y)
fix(aha)

barplot(table(aha))

boxplot(t(sapply(res5_s, function(x) x$precs)))

boxplot(t(sapply(res5_s, function(x) x$precscond)))

t(sapply(res5_s, function(x) x$precscond))



boxplot(t(sapply(res5_l, function(x) x$precs)))

boxplot(t(sapply(res5_l, function(x) x$precscond)))
t(sapply(res5_l, function(x) x$precscond))
x11()
boxplot(t(sapply(res3_l, function(x) x$precscond)))

# NAECHSTER SCHRITT: SCHAUEN, OB DIE SIMULATIONSPARAMETER PASSEN,
# INDEM GESCHAUT WIRD WIR GUT DIE MODELLE IN DEN EINZELNEN KNOTEN
# SIND UND AUCH WIE UNGLEICH GROS DIE ENDKNOTEN SIND (VIELLEICHT DAS AUCH
# VISUALIIEREN).
# NATÜRLICH AUCH HIERCLASS DRÜBER LAUFEN LASSEN, UM ZU SEHEN, WIE
# GUT DIE PRÄDIKTION FUNKTIONIERT.



save(res1_s, res1_l, res2_s, res2_l, res3_s, res3_l, res4_s, res4_l, res5_s, res5_l,
     file="./Simulations/HierPr/results/intermediate_results/restrials.Rda")



# Simulate a whole dataset:

n <- 5000







p <- plot_structure(treestruc)
ggsave("./Simulations/HierPr/results/figures/Figure_treestruc.pdf", width=18, height=4)










































y <- outcomemat[,5]
y <- factor(y, levels=41:90)
length(table(y))

length(table(outcomemat[,5]))

par(mfrow=c(2,3))
barplot(table(outcomemat[,1]))
barplot(table(outcomemat[,2]))
barplot(table(outcomemat[,3]))
barplot(table(outcomemat[,4]))
barplot(table(outcomemat[,5]))
par(mfrow=c(1,1))

x11()






ui <- coeflist[[1]]$datanode
names(ui)

library("nnet")

m <- multinom(y ~ ., data = coeflist[[1]]$datanode)
summary(m)

m <- glm(y ~ ., data = coeflist[[1]]$datanode, family="binomial")
summary(m)

par(mfrow=c(2,3))
boxplot(coeflist[[1]]$datanode$X1 ~ coeflist[[1]]$datanode$y)
boxplot(coeflist[[1]]$datanode$X2 ~ coeflist[[1]]$datanode$y)
boxplot(coeflist[[1]]$datanode$X3 ~ coeflist[[1]]$datanode$y)
boxplot(coeflist[[1]]$datanode$X4 ~ coeflist[[1]]$datanode$y)
boxplot(coeflist[[1]]$datanode$X5 ~ coeflist[[1]]$datanode$y)
par(mfrow=c(1,1))




node <- 2

m <- glm(y ~ ., data = coeflist[[node]]$datanode, family="binomial")
summary(m)

par(mfrow=c(2,3))
boxplot(coeflist[[node]]$datanode$X1 ~ coeflist[[node]]$datanode$y)
boxplot(coeflist[[node]]$datanode$X2 ~ coeflist[[node]]$datanode$y)
boxplot(coeflist[[node]]$datanode$X3 ~ coeflist[[node]]$datanode$y)
boxplot(coeflist[[node]]$datanode$X4 ~ coeflist[[node]]$datanode$y)
boxplot(coeflist[[node]]$datanode$X5 ~ coeflist[[node]]$datanode$y)
par(mfrow=c(1,1))




node <- 3

m <- glm(y ~ ., data = coeflist[[node]]$datanode, family="binomial")
summary(m)

par(mfrow=c(2,3))
boxplot(coeflist[[node]]$datanode$X1 ~ coeflist[[node]]$datanode$y)
boxplot(coeflist[[node]]$datanode$X2 ~ coeflist[[node]]$datanode$y)
boxplot(coeflist[[node]]$datanode$X3 ~ coeflist[[node]]$datanode$y)
boxplot(coeflist[[node]]$datanode$X4 ~ coeflist[[node]]$datanode$y)
boxplot(coeflist[[node]]$datanode$X5 ~ coeflist[[node]]$datanode$y)
par(mfrow=c(1,1))







node <- 4

m <- glm(y ~ ., data = coeflist[[node]]$datanode, family="binomial")
summary(m)

par(mfrow=c(2,3))
boxplot(coeflist[[node]]$datanode$X1 ~ coeflist[[node]]$datanode$y)
boxplot(coeflist[[node]]$datanode$X2 ~ coeflist[[node]]$datanode$y)
boxplot(coeflist[[node]]$datanode$X3 ~ coeflist[[node]]$datanode$y)
boxplot(coeflist[[node]]$datanode$X4 ~ coeflist[[node]]$datanode$y)
boxplot(coeflist[[node]]$datanode$X5 ~ coeflist[[node]]$datanode$y)
par(mfrow=c(1,1))




node <- 5

m <- glm(y ~ ., data = coeflist[[node]]$datanode, family="binomial")
summary(m)

par(mfrow=c(2,3))
boxplot(coeflist[[node]]$datanode$X1 ~ coeflist[[node]]$datanode$y)
boxplot(coeflist[[node]]$datanode$X2 ~ coeflist[[node]]$datanode$y)
boxplot(coeflist[[node]]$datanode$X3 ~ coeflist[[node]]$datanode$y)
boxplot(coeflist[[node]]$datanode$X4 ~ coeflist[[node]]$datanode$y)
boxplot(coeflist[[node]]$datanode$X5 ~ coeflist[[node]]$datanode$y)
par(mfrow=c(1,1))






node <- 34

m <- glm(y ~ ., data = coeflist[[node]]$datanode, family="binomial")
summary(m)

par(mfrow=c(2,3))
boxplot(coeflist[[node]]$datanode$X1 ~ coeflist[[node]]$datanode$y)
boxplot(coeflist[[node]]$datanode$X2 ~ coeflist[[node]]$datanode$y)
boxplot(coeflist[[node]]$datanode$X3 ~ coeflist[[node]]$datanode$y)
boxplot(coeflist[[node]]$datanode$X4 ~ coeflist[[node]]$datanode$y)
boxplot(coeflist[[node]]$datanode$X5 ~ coeflist[[node]]$datanode$y)
par(mfrow=c(1,1))



dim(outcomemat)











# Hierarchical F-score:
measure = msr("classif.hierfbeta")
predictions$score(measure)
## classifhierfbeta
##        0.5721372
# Hierarchical precision:
measure = msr("classif.hierpr")
predictions$score(measure)
## classifhierpr
##     0.5187406
# Hierarchical recall:
measure = msr("classif.hierre")
predictions$score(measure)
## classifhierre
##      0.637788
# H-loss measure:
measure = msr("classif.hloss")
predictions$score(measure)
## classifhloss
##     0.376924
# Shortest path loss measure:
measure = msr("classif.spath")
predictions$score(measure)
## classifspath
##     1.543976
# Classification accuracy (not a hierarchical performance/loss measure):
measure = msr("classif.acc")
predictions$score(measure)
## classif.acc
##        0.15
