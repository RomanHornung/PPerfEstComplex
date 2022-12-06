setwd("D:/Projects/DESTATIS/PredErrorComplex/PPerfEstComplex")


source("./simulations/hierpr/functions.R")

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



# covariate matrix

n <- 50000



# unterschiedlche intercept sind, wichtig, damit ungeliche klassengrößen entstehen.
# die intercept von benachbarten schichten sollen nicht voneinander abhängen, weil
# die degrees der klassenungleichheiten nicht voneinander abhägne sollen.



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



treestruc

coeflist <- vector(mode = "list", length = length(treestruc$nodelist))
for(i in seq(along=coeflist)) {
  coeflist[[i]] <- list()
  coeflist[[i]][[1]] <- treestruc$nodelist[[i]]
  
  coeflist[[i]][[2]] <- which(sapply(1:length(treestruc$leftnodes), function(x) (i >= treestruc$leftnodes[x]) & (i <= treestruc$rightnodes[x])))
  
  names(coeflist[[i]]) <- c("childnodes", "layer")
  
}






treestruc$leftnodes
treestruc$rightnodes


vCoef1 = rep(0, 6)
vCoef2 = rnorm(6)
vCoef3 = rnorm(6)


vCoef2 = rnorm(6)
vCoef3 = rnorm(6)


sdbeta0 <- sqrt(1)
sdbeta <- sqrt(c(1, 1.5, 2, 2.5))


maxlayer <- max(sapply(coeflist, function(x) x$layer))


for(i in seq(along=coeflist)) {
  
  if(coeflist[[i]]$layer==maxlayer) {
    coeflist[[i]]$coefs <- NA
  } else {
    
    if(length(coeflist[[i]]$childnodes)==2) {
      coefs <- matrix(nrow=1, ncol=6, data=c(rnorm(1, sd=sdbeta0), rnorm(5, sd=sdbeta[coeflist[[i]]$layer])))
    }
    if(length(coeflist[[i]]$childnodes)==3) {
      coefs <- rbind(c(rnorm(1, sd=sdbeta0), rnorm(5, sd=sdbeta[coeflist[[i]]$layer])),
                       c(rnorm(1, sd=sdbeta0), rnorm(5, sd=sdbeta[coeflist[[i]]$layer])))
    }
    
    coeflist[[i]]$coefs <- coefs
    
  }
  
}
  




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


coeflist[[1]]



length(treestruc$nodelist)


# Here mChoices and dfM$y encode the same information differently.
