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

n <- 10000
mX = cbind(rep(1, n), matrix(rnorm(n*5), n, 5))

vCoef1 = rep(0, 6)
vCoef2 = rnorm(6)
vCoef3 = rnorm(6)


# vector of probabilities
vProb = cbind(exp(mX%*%vCoef1), exp(mX%*%vCoef2), exp(mX%*%vCoef3))

# multinomial draws
mChoices = t(apply(vProb, 1, rmultinom, n = 1, size = 1))
dfM = cbind.data.frame(y = apply(mChoices, 1, function(x) which(x==1)), mX)

library("nnet")
m <- multinom(y ~ ., data = dfM[,-2])
summary(m)

vCoef2
vCoef3


# Here mChoices and dfM$y encode the same information differently.
