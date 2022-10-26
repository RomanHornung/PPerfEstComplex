# setwd("Z:/Projects/DESTATIS/PredErrorComplex/PPerfEstComplex")

# Source the functions necessary for the simulation:

source("./Simulations/ClustData/functions.R")


# Perform the simulation:

simulation(niter=100, N=10, ni=5, beta=c(2, 0, 0), sdbinter=1, sdbslope=2, sdeps=1, type="norm", fixed=TRUE)
simulation(niter=100, N=10, ni=5, beta=c(2, 0, 0), sdbinter=1, sdbslope=0, sdeps=1, type="norm", fixed=TRUE)

simulation(niter=100, N=50, ni=5, beta=c(2, 0, 0), sdbinter=1, sdbslope=2, sdeps=1, type="norm", fixed=TRUE)
simulation(niter=100, N=50, ni=5, beta=c(2, 0, 0), sdbinter=1, sdbslope=0, sdeps=1, type="norm", fixed=TRUE)

simulation(niter=100, N=10, ni=25, beta=c(2, 0, 0), sdbinter=1, sdbslope=2, sdeps=1, type="norm", fixed=TRUE)
simulation(niter=100, N=10, ni=25, beta=c(2, 0, 0), sdbinter=1, sdbslope=0, sdeps=1, type="norm", fixed=TRUE)

simulation(niter=100, N=10, ni=5, beta=c(2, 0, 0), sdbinter=0, sdbslope=2, sdeps=1, type="norm", fixed=TRUE)
simulation(niter=100, N=10, ni=5, beta=c(2, 0, 0), sdbinter=0, sdbslope=0, sdeps=1, type="norm", fixed=TRUE)


simulation(niter=100, N=10, ni=5, beta=c(2, 0, 0), sdbinter=1, sdbslope=2, sdeps=1, type="norm", fixed=FALSE)
simulation(niter=100, N=10, ni=5, beta=c(2, 0, 0), sdbinter=1, sdbslope=0, sdeps=1, type="norm", fixed=FALSE)

simulation(niter=100, N=50, ni=5, beta=c(2, 0, 0), sdbinter=1, sdbslope=2, sdeps=1, type="norm", fixed=FALSE)
simulation(niter=100, N=50, ni=5, beta=c(2, 0, 0), sdbinter=1, sdbslope=0, sdeps=1, type="norm", fixed=FALSE)

simulation(niter=100, N=10, ni=25, beta=c(2, 0, 0), sdbinter=1, sdbslope=2, sdeps=1, type="norm", fixed=FALSE)
simulation(niter=100, N=10, ni=25, beta=c(2, 0, 0), sdbinter=1, sdbslope=0, sdeps=1, type="norm", fixed=FALSE)

simulation(niter=100, N=10, ni=5, beta=c(2, 0, 0), sdbinter=0, sdbslope=2, sdeps=1, type="norm", fixed=FALSE)
simulation(niter=100, N=10, ni=5, beta=c(2, 0, 0), sdbinter=0, sdbslope=0, sdeps=1, type="norm", fixed=FALSE)
