# setwd("D:/Projects/DESTATIS/PredErrorComplex/PPerfEstComplex")

# Source the functions necessary for the simulation:

source("./Simulations/ClustData/functions.R")



# Perform the simulation:

set.seed(1234)

simulation(niter=100, N=10, ni=5, beta=c(2, 0, 0), sdbinter=1, sdbslope=2, sdeps=1, type="norm", fixed=FALSE)
simulation(niter=100, N=10, ni=5, beta=c(2, 0, 0), sdbinter=1, sdbslope=0, sdeps=1, type="norm", fixed=FALSE)

simulation(niter=100, N=50, ni=5, beta=c(2, 0, 0), sdbinter=1, sdbslope=2, sdeps=1, type="norm", fixed=FALSE)
simulation(niter=100, N=50, ni=5, beta=c(2, 0, 0), sdbinter=1, sdbslope=0, sdeps=1, type="norm", fixed=FALSE)

simulation(niter=100, N=10, ni=25, beta=c(2, 0, 0), sdbinter=1, sdbslope=2, sdeps=1, type="norm", fixed=FALSE)
simulation(niter=100, N=10, ni=25, beta=c(2, 0, 0), sdbinter=1, sdbslope=0, sdeps=1, type="norm", fixed=FALSE)

simulation(niter=100, N=10, ni=5, beta=c(2, 0, 0), sdbinter=0, sdbslope=2, sdeps=1, type="norm", fixed=FALSE)
simulation(niter=100, N=10, ni=5, beta=c(2, 0, 0), sdbinter=0, sdbslope=0, sdeps=1, type="norm", fixed=FALSE)


simulation(niter=100, N=10, ni=5, beta=c(2, 0, 0), sdbinter=1, sdbslope=2, sdeps=1, type="norm", fixed=TRUE)
simulation(niter=100, N=10, ni=5, beta=c(2, 0, 0), sdbinter=1, sdbslope=0, sdeps=1, type="norm", fixed=TRUE)

simulation(niter=100, N=50, ni=5, beta=c(2, 0, 0), sdbinter=1, sdbslope=2, sdeps=1, type="norm", fixed=TRUE)
simulation(niter=100, N=50, ni=5, beta=c(2, 0, 0), sdbinter=1, sdbslope=0, sdeps=1, type="norm", fixed=TRUE)

simulation(niter=100, N=10, ni=25, beta=c(2, 0, 0), sdbinter=1, sdbslope=2, sdeps=1, type="norm", fixed=TRUE)
simulation(niter=100, N=10, ni=25, beta=c(2, 0, 0), sdbinter=1, sdbslope=0, sdeps=1, type="norm", fixed=TRUE)

simulation(niter=100, N=10, ni=5, beta=c(2, 0, 0), sdbinter=0, sdbslope=2, sdeps=1, type="norm", fixed=TRUE)
simulation(niter=100, N=10, ni=5, beta=c(2, 0, 0), sdbinter=0, sdbslope=0, sdeps=1, type="norm", fixed=TRUE)




rm(list=ls());gc()

allfiles <- list.files("./Simulations/ClustData/Results")
allfiles

stump <- gsub("TRUE.RData", "", grep("TRUE", allfiles, value=TRUE))

stumptrue <- paste0(stump, "TRUE.RData")
stumpfalse <- paste0(stump, "FALSE.RData")

stumptrue
stumpfalse

ls()

pdf("./Simulations/ClustData/Results/figures/comparison.pdf", width=7, height=30)
par(mfrow=c(length(stumpfalse),2))
for(i in 1:length(stumpfalse)) {

load(paste0("./Simulations/ClustData/Results/intermediate_results/", stumpfalse[i]))
resultfalse <- result
load(paste0("./Simulations/ClustData/Results/intermediate_results/", stumptrue[i]))
resulttrue <- result


boxplot(resultfalse$mse_subsamp0.8, resultfalse$mse_subsamp0.8g, main=stump[i])
boxplot(resulttrue$mse_subsamp0.8, resulttrue$mse_subsamp0.8g)


# Sys.sleep(1)
}
par(mfrow=c(1,1))
dev.off()
