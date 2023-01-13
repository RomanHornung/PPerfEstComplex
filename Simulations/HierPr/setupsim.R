setwd("Z:/Projects/DESTATIS/PredErrorComplex/PPerfEstComplex")


source("./simulations/hierpr/functions_setupsim.R")


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



# Simulate 10 datasets of size n=1000 and evaluate their performance
# using eval_perm():

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



# Simulate 10 datasets of size n=3000 and evaluate their performance
# using eval_perm():

res5_l <- list()

for(i in 1:10) {

  coeflist <- simulate_coefs(treestruc=treestruc, sdbeta0=sqrt(1),
                             sdbeta=sqrt(c(2.5, 2, 0.9, 0.7, 0.5)))

  dataobj <- sim_data(n=3000, coeflist=coeflist)

  res5_l[[i]] <- eval_perm(data=dataobj$data)

  cat(paste("Iteration: ", i), "\n")

}

res5_l




# Plot the results:

boxplot(t(sapply(res5_s, function(x) x$precs)))
boxplot(t(sapply(res5_s, function(x) x$precscond)))

boxplot(t(sapply(res5_l, function(x) x$precs)))
boxplot(t(sapply(res5_l, function(x) x$precscond)))

res5 <- c(res5_s, res5_l)
boxplot(t(sapply(res5, function(x) x$precs)))
boxplot(t(sapply(res5, function(x) x$precscond)))
colMeans(t(sapply(res5, function(x) x$precscond)))




# Frequencies of the end node classes in one of the
# datasets:

barplot(table(dataobj$data$y))




# Previous tries with different parameter values:

# sqrt(1),
# sdbeta=sqrt(c(1, 1.5, 2, 2.5, 3))

# sqrt(1),
# sdbeta=sqrt(c(1.3, 1.5, 1.7, 1.9, 2.1))

# sqrt(1),
# sdbeta=sqrt(c(1.5, 1.3, 1.1, 0.9, 0.7))

# sqrt(1),
# sdbeta=sqrt(c(2, 1.5, 0.9, 0.7, 0.5))






set.seed(1234)

coeflist <- simulate_coefs(treestruc=treestruc, sdbeta0=sqrt(1),
                           sdbeta=sqrt(c(2.5, 2, 0.9, 0.7, 0.5)))

dataobj <- sim_data(n=1000, coeflist=coeflist)

for(i in seq(along=dataobj$coeflist)) {
  
  datatemp <- dataobj$coeflist[[i]]$datanode
  
  if(!is.data.frame(datatemp))
    dataobj$coeflist[[i]]$p <- NA
  else {
    
    library("nnet")
    modtemp <- multinom(y ~ ., data = datatemp)
    summary(modtemp)
    z <- summary(modtemp)$coefficients/summary(modtemp)$standard.errors
    p <- (1 - pnorm(abs(z), 0, 1)) * 2
    
    if(class(p)[1]=="matrix")
      res <- p[,-1]
    if(class(p)[1]=="numeric")
      res <- p[-1]
    
    dataobj$coeflist[[i]]$p <- res
    
  }
  
}

pvals <- lapply(dataobj$coeflist, function(x) as.vector(x$p))
layers <- sapply(dataobj$coeflist, function(x) x$layer)

boxplot(unlist(pvals) ~ rep(layers, times=sapply(pvals, length)))




set.seed(1234)

pvalsvec <- layersvec <- c()

for(count in 1:20) {
  
  boolFalse <- FALSE
  while (boolFalse == FALSE)
  {
    tryCatch({
      
      coeflist <- simulate_coefs(treestruc=treestruc, sdbeta0=sqrt(1),
                                 sdbeta=sqrt(c(2.5, 2, 0.9, 0.7, 0.5)))
      
      dataobj <- sim_data(n=1000, coeflist=coeflist)
      
      for(i in seq(along=dataobj$coeflist)) {
        
        datatemp <- dataobj$coeflist[[i]]$datanode
        
        if(!is.data.frame(datatemp))
          dataobj$coeflist[[i]]$p <- NA
        else {
          
          library("nnet")
          modtemp <- multinom(y ~ ., data = datatemp)
          summary(modtemp)
          z <- summary(modtemp)$coefficients/summary(modtemp)$standard.errors
          p <- (1 - pnorm(abs(z), 0, 1)) * 2
          
          if(class(p)[1]=="matrix")
            res <- p[,-1]
          if(class(p)[1]=="numeric")
            res <- p[-1]
          
          dataobj$coeflist[[i]]$p <- res
          
        }
        
      }
      
      boolFalse <- TRUE
    }, error = function(e){
    }, finally = {})
  }
  
  pvals <- lapply(dataobj$coeflist, function(x) as.vector(x$p))
  layers <- sapply(dataobj$coeflist, function(x) x$layer)
  
  pvalsvec <- c(pvalsvec, unlist(pvals))
  layersvec <- c(layersvec, rep(layers, times=sapply(pvals, length)))
  
}


boxplot(pvalsvec ~ layersvec)
hist(pvalsvec[layersvec==5], 50)












# NAECHSTER SCHRITT: SCHAUEN, OB DIE SIMULATIONSPARAMETER PASSEN,
# INDEM GESCHAUT WIRD WIR GUT DIE MODELLE IN DEN EINZELNEN KNOTEN
# SIND UND AUCH WIE UNGLEICH GROS DIE ENDKNOTEN SIND (VIELLEICHT DAS AUCH
# VISUALIIEREN).
# NATÜRLICH AUCH HIERCLASS DRÜBER LAUFEN LASSEN, UM ZU SEHEN, WIE
# GUT DIE PRÄDIKTION FUNKTIONIERT.



save(res1_s, res1_l, res2_s, res2_l, res3_s, res3_l, res4_s, res4_l, res5_s, res5_l,
     file="./Simulations/HierPr/results/intermediate_results/restrials.Rda")


load("./Simulations/HierPr/results/intermediate_results/restrials.Rda")


res5_l

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
