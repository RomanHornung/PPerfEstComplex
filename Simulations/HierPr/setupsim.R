setwd("D:/Projects/DESTATIS/PredErrorComplex/PPerfEstComplex")


source("./simulations/hierpr/functions.R")


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





# -	Erster Schritt ist die hierarchische Baumstruktur festlegen.
# -	Jeder interne Knoten ist mit einem lokalen Modell verknüpft, wobei die Unterscheidbarkeit der Klassen geringer werden soll, je weiter unten man in der hierarchischen Struktur ankommt. Das wird realisiert in dem das Rauschen (epsilon) immer weiter vergrößert wird, je weiter man nach unten kommt.
# -	Dass die Koeffizienten von vertikal benachbarten Schichten sich ähnlich sind, wird so realisiert, dass die Koeffizenten aus Normalverteilungen mit Mittelwerten gleich den Koeffizienten der darüberliegenden Schichten gezogen werden.




# unterschiedlche intercept sind, wichtig, damit ungeliche klassengrößen entstehen.
# die intercept von benachbarten schichten sollen nicht voneinander abhängen, weil
# die degrees der klassenungleichheiten nicht voneinander abhägne sollen.



n <- 50000

# erster coefficent intercept, rest sind die beeinflussenden betas:
vCoef1 = rep(0, 6)
vCoef2 = rnorm(6)
vCoef3 = rnorm(6)

# sim_multinom <- function(n, coefs) {

mX = cbind(rep(1, n), matrix(rnorm(n*5), n, 5))

# vector of probabilities
vProb = cbind(exp(mX%*%vCoef1), exp(mX%*%vCoef2), exp(mX%*%vCoef3))

# multinomial draws
mChoices = t(apply(vProb, 1, rmultinom, n = 1, size = 1))
dfM = cbind.data.frame(y = apply(mChoices, 1, function(x) which(x==1)), mX)

library("nnet")
m <- multinom(y ~ ., data = dfM)
summary(m)

vCoef2
vCoef3



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




# Simulate a whole dataset:

n <- 5000

X <- matrix(nrow=n, ncol=5, rnorm(n*5))




# innerclasses

set.seed(1234)

#asdf

coeflist <- simulate_coefs(treestruc=treestruc, sdbeta0=sqrt(1),
                           sdbeta=sqrt(c(1, 1.5, 2, 2.5, 3)))



# NAECHSTER SCHRITT: SCHAUEN, OB DIE SIMULATIONSPARAMETER PASSEN,
# INDEM GESCHAUT WIRD WIR GUT DIE MODELLE IN DEN EINZELNEN KNOTEN
# SIND UND AUCH WIE UNGLEICH GROS DIE ENDKNOTEN SIND (VIELLEICHT DAS AUCH
# VISUALIIEREN).
# NATÜRLICH AUCH HIERCLASS DRÜBER LAUFEN LASSEN, UM ZU SEHEN, WIE
# GUT DIE PRÄDIKTION FUNKTIONIERT.



maxlayer <- max(sapply(coeflist, function(x) x$layer))

outcomemat <- matrix(nrow=nrow(X), ncol=maxlayer)


tempclass <- coeflist[[1]]$childnodes[get_child_nodes(X, coeflist[[1]]$coefs)]

outcomemat[,1] <- tempclass

coeflist[[1]]$datanode <- data.frame(X)
coeflist[[1]]$datanode$y <- factor(tempclass)



for(i in 2:(length(coeflist)-1)) {
  
  subs <- outcomemat[,coeflist[[i]]$layer-1]==i
  tempclass <- coeflist[[i]]$childnodes[get_child_nodes(X[subs,], coeflist[[i]]$coefs)]
  
  outcomemat[subs,coeflist[[i]]$layer] <- tempclass
  
  coeflist[[i]]$datanode <- data.frame(X[subs,])
  coeflist[[i]]$datanode$y <- factor(tempclass)
  
}


# fix(outcomemat)

length(table(outcomemat[,5]))
barplot(table(outcomemat[,5]))
