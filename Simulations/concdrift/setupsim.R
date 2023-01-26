setwd("Z:/Projects/DESTATIS/PredErrorComplex/PPerfEstComplex")





xseq <- seq(-15, 10, length=400)



plot(xseq, dnorm(xseq, mean=0, sd=1), ty="l")
lines(xseq, dnorm(xseq, mean=5, sd=1), col=2)
lines(xseq, dnorm(xseq, mean=-3, sd=1), col=3)

# Parameter:

# Only covariate shift vs. Covariate Shift & Label Shift (2)
# Weak covariate shift vs. strong covariate shift (2) 
# Weak label shift vs. strong label shift (2)
# 




beta0 <- function(t) {
  
  startval <- 0
  stopval <- 5
  tstart <- 0
  tend <- 1
  
  b1 <- (stopval - startval)/(tend - tstart)
  b0 <- startval - b1*tstart
  
  return(b0 + b1*t)
  
}













# Source the functions necessary for the simulation:

source("./Simulations/ClustData/functions.R")



# Perform the simulation:

set.seed(1234)

ui <- simulation(niter=5, N=10, ni=5, sdbinter=1, sdbslope=1, sdeps=1, fixed="none")

summary(lm(y ~ ., data=ah))


simd <- simuldata(N=50, ni=50, beta=c(1, 1, -1, 0, 0), sdbinter=1, sdbslope=1, sdeps=1, fixed="none")

boxplot(simd$y ~ simd$index)


simd <- simuldata(N=5, ni=20, beta=c(1, 1, -1, 0, 0), sdbinter=1, sdbslope=1, sdeps=0.5, fixed="first")
datat <- simd$dataset
b0s <- unique(simd$b)
b1s <- unique(simd$b2)
plot(datat$x.1, datat$y)
for(i in seq(along=b0s)) {
  points(datat$x.1[datat$index==i], datat$y[datat$index==i], col=i)
  xseq <- seq(min(datat$x.1[datat$index==i]), max(datat$x.1[datat$index==i]), length=100)
  lines(xseq, b0s[i] + (1 + b1s[i])*xseq, col=i)
}

datat$x.1



# N10ni25beta200sdbinter1sdbslope0sdeps1norm

ti1 <- Sys.time()
set.seed(123456)
casefirst1 <- simulation(niter=15, N=10, ni=25, beta=c(2, 0, 0), sdbinter=1, sdbslope=0, sdeps=1, fixed="first")
ti2 <- Sys.time()
as.numeric(difftime(ti2, ti1, unit="secs"))

ti1 <- Sys.time()
set.seed(123456)
casefirst2 <- simulation(niter=15, N=10, ni=25, beta=c(1, 1, -1, 0, 0), sdbinter=1, sdbslope=0, sdeps=1, fixed="first")
ti2 <- Sys.time()
as.numeric(difftime(ti2, ti1, unit="secs"))



ti1 <- Sys.time()
set.seed(123456)
casefirstrf1 <- simulation(niter=10, N=10, ni=25, beta=c(2, 0, 0), sdbinter=1, sdbslope=0, sdeps=1, fixed="first", method="rf")
ti2 <- Sys.time()
as.numeric(difftime(ti2, ti1, unit="secs"))

ti1 <- Sys.time()
set.seed(123456)
casefirstrf2 <- simulation(niter=10, N=10, ni=25, beta=c(1, 1, -1, 0, 0), sdbinter=1, sdbslope=0, sdeps=1, fixed="first", method="rf")
ti2 <- Sys.time()
as.numeric(difftime(ti2, ti1, unit="secs"))


boxplot(c(casefirstrf1, casefirstrf1_2))
boxplot(c(casefirstrf2, casefirstrf2_2))




ti1 <- Sys.time()
set.seed(123456)
casefirstrf1_2 <- simulation(niter=10, N=10, ni=25, beta=c(2, 0, 0), sdbinter=1, sdbslope=0, sdeps=0.5, fixed="first", method="rf")
ti2 <- Sys.time()
as.numeric(difftime(ti2, ti1, unit="secs"))

ti1 <- Sys.time()
set.seed(123456)
casefirstrf2_2 <- simulation(niter=10, N=10, ni=25, beta=c(1, 1, -1, 0, 0), sdbinter=1, sdbslope=0, sdeps=0.5, fixed="first", method="rf")
ti2 <- Sys.time()
as.numeric(difftime(ti2, ti1, unit="secs"))





casefirstrf1
casefirstrf2


ti1 <- Sys.time()
set.seed(123456)
bla <- simulation(niter=1, N=10, ni=25, beta=c(1, 1, -1, 0, 0), sdbinter=1, sdbslope=0, sdeps=1, fixed="first", method="rf")
ti2 <- Sys.time()
as.numeric(difftime(ti2, ti1, unit="secs"))



ti1 <- Sys.time()
set.seed(123456)
bla <- simulation(niter=1, N=10, ni=25, beta=c(1, 1, -1, 0, 0), sdbinter=1, sdbslope=0, sdeps=1, fixed="first", method="rf")
ti2 <- Sys.time()
as.numeric(difftime(ti2, ti1, unit="secs"))




simdata <- simuldata(N=10, ni=25, beta=c(2, 0, 0), sdbinter=1, sdbslope=0, sdeps=1, fixed="first")$dataset


ypredsrf <- rep(NA, nrow(simdata))

for(i in seq(along=unique(simdata$index))) {
  model <- ranger(y ~ ., data=simdata[simdata$index!=i,-ncol(simdata)])
  ypredsrf[simdata$index==i] <- predict(model, data=simdata[simdata$index==i,-ncol(simdata)])$predictions
}

plot(simdata$y, ypredsrf)
cor(simdata$y, ypredsrf)


simdataperm <- simdata

simdataperm$index <- sample(simdataperm$index)

ypredsrf <- rep(NA, nrow(simdataperm))

for(i in seq(along=unique(simdataperm$index))) {
  model <- ranger(y ~ ., data=simdataperm[simdataperm$index!=i,-ncol(simdataperm)])
  ypredsrf[simdataperm$index==i] <- predict(model, data=simdataperm[simdataperm$index==i,-ncol(simdataperm)])$predictions
}

plot(simdataperm$y, ypredsrf)
cor(simdataperm$y, ypredsrf)





simdata <- simuldata(N=10, ni=25, beta=c(1, 1, -1, 0, 0), sdbinter=0, sdbslope=0, sdeps=1, fixed="first")$dataset
ypredsrf <- rep(NA, nrow(simdata))

for(i in seq(along=unique(simdata$index))) {
  model <- ranger(y ~ ., data=simdata[simdata$index!=i,-ncol(simdata)])
  ypredsrf[simdata$index==i] <- predict(model, data=simdata[simdata$index==i,-ncol(simdata)])$predictions
}

plot(simdata$y, ypredsrf)
cor(simdata$y, ypredsrf)



simdataperm <- simdata

simdataperm$index <- sample(simdataperm$index)

ypredsrf <- rep(NA, nrow(simdataperm))

for(i in seq(along=unique(simdataperm$index))) {
  model <- ranger(y ~ ., data=simdataperm[simdataperm$index!=i,-ncol(simdataperm)])
  ypredsrf[simdataperm$index==i] <- predict(model, data=simdataperm[simdataperm$index==i,-ncol(simdataperm)])$predictions
}

plot(simdataperm$y, ypredsrf)
cor(simdataperm$y, ypredsrf)







par(mfrow=c(2,2))
boxplot(casefirst1)
boxplot(casefirst2)

boxplot(casefirstrf1)
boxplot(casefirstrf2)
par(mfrow=c(1,1))


par(mfrow=c(1,2))
boxplot(c(casefirst1, casefirstrf1))
boxplot(c(casefirst2, casefirstrf2))
boxplot(casefirst2)

sapply(c(casefirst2, casefirstrf2), mean)

plot(rep(1:2, each=15), unlist(casefirst1))
plot(rep(1:2, each=15), unlist(casefirst2))



boxplot(c(case1$mse_subsamp0.8, case11$mse_subsamp0.8), c(case1$mse_subsamp0.8g, case11$mse_subsamp0.8g))
boxplot(c(case2$mse_subsamp0.8, case21$mse_subsamp0.8), c(case2$mse_subsamp0.8g, case21$mse_subsamp0.8g))


case1
boxplot(case11)
boxplot(case21)



simd <- simuldata(N=5, ni=20, beta=c(1, 0, 0, 0, 0), sdbinter=1, sdbslope=1, sdeps=1, fixed="none")
datat <- simd$dataset
b0s <- unique(simd$b)
b1s <- unique(simd$b2)
plot(datat$x.1, datat$y)
for(i in seq(along=b0s)) {
  points(datat$x.1[datat$index==i], datat$y[datat$index==i], col=i)
  xseq <- seq(min(datat$x.1[datat$index==i]), max(datat$x.1[datat$index==i]), length=100)
  lines(xseq, b0s[i] + (1 + b1s[i])*xseq, col=i)
}







# GRAFIK MACHEN MIT ZUSAMMEN HANG FÜR JEDEN CLUSTER OIDER: 



makediagplot <- function(simdata) {
  
  library("ranger")
  
  ypredsrf <- rep(NA, nrow(simdata))
  
  for(i in seq(along=unique(simdata$index))) {
    model <- ranger(y ~ X1 + X2 + X3 + X4 + X5, data=simdata[simdata$index!=i,])
    ypredsrf[simdata$index==i] <- predict(model, data=simdata[simdata$index==i,])$predictions
  }
  
  
  ypredslm <- rep(NA, nrow(simdata))
  
  for(i in seq(along=unique(simdata$index))) {
    model <- lm(y ~ X1 + X2 + X3 + X4 + X5, data=simdata[simdata$index!=i,])
    ypredslm[simdata$index==i] <- predict(model, newdata=simdata[simdata$index==i,])
  }
  
  
  simdata$index_ran <- sample(simdata$index)
  
  
  ypredsrf_ran <- rep(NA, nrow(simdata))
  
  for(i in seq(along=unique(simdata$index_ran))) {
    model <- ranger(y ~ X1 + X2 + X3 + X4 + X5, data=simdata[simdata$index_ran!=i,])
    ypredsrf_ran[simdata$index_ran==i] <- predict(model, data=simdata[simdata$index_ran==i,])$predictions
  }
  
  
  ypredslm_ran <- rep(NA, nrow(simdata))
  
  for(i in seq(along=unique(simdata$index_ran))) {
    model <- lm(y ~ X1 + X2 + X3 + X4 + X5, data=simdata[simdata$index_ran!=i,])
    ypredslm_ran[simdata$index_ran==i] <- predict(model, newdata=simdata[simdata$index_ran==i,])
  }
  
  par(mfrow=c(2,2))
  plot(ypredsrf, simdata$y, main=paste0("corr: ", round(cor(ypredsrf, simdata$y), 3)))
  plot(ypredslm, simdata$y, main=paste0("corr: ", round(cor(ypredslm, simdata$y), 3)))
  plot(ypredsrf_ran, simdata$y, main=paste0("corr: ", round(cor(ypredsrf_ran, simdata$y), 3)))
  plot(ypredslm_ran, simdata$y, main=paste0("corr: ", round(cor(ypredslm_ran, simdata$y), 3)))
  par(mfrow=c(1,1))
  
}


pdf("./Simulations/ClustData/results/figures/signalstrength.pdf")

setting <- "N=50, ni=5, beta=c(1, 1, -1, 0, 0), sdbinter=0, sdbslope=0, sdeps=1, fixed=\"none\""

eval(parse(text=paste0("simd <- simuldata(", setting, ")")))

makediagplot(simdata = simd)
mtext(setting, side = 3, line = -1, outer = TRUE)

dev.off()



plot(ypreds, simd$y)
cor(ypreds, simd$y)




ypreds

names(ui)
ui$predictions

plot(ah$X1, ah$y)
plot(ah$X2, ah$y)
plot(ah$X3, ah$y)
plot(ah$X4, ah$y)

names(ah)



scenmat <- expand.grid(fixed=c("none", "first", "second"), sdbinter=c(1,0), sdbslope=c(1,0), sdeps=c(1,0.7), ni=c(5,25), N=c(10,50), stringsAsFactors = TRUE)

head(scenmat)


settings <- apply(scenmat, 1, function(x) paste0("N=", x[6], ", ni=", x[5], ", beta=c(1, 1, -1, 0, 0), sdbinter=", x[2], ", sdbslope=", x[3], ", sdeps=", x[4], ", fixed=\"", x[1], "\""))


pdf("./Simulations/ClustData/results/figures/signalstrength.pdf")

for(i in seq(along=settings)) {
  
  eval(parse(text=paste0("simd <- simuldata(", settings[i], ")")))
  
  makediagplot(simdata = simd)
  mtext(settings[i], side = 3, line = -1, outer = TRUE)
  
  cat(paste0("Iteration: ", i, " of ", length(settings)), "\n")
  
}

dev.off()





load("./simulations/clustdata/results/intermediate_results/N10ni5beta11-100sdbinter1sdbslope1sdeps1none.RData")


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








simd <- simuldata(N=200, ni=50, beta=c(1, 1, -1, 0, 0), sdbinter=1, sdbslope=1, sdeps=1, fixed="none")$dataset
simd$index <- NULL

# Define the task for the top-down classification rule:
task = as_task_regr(y ~ ., data = simd)

# Initialize the learner for the top-down classification rule:
learner = lrn("regr.ranger")
learner$param_set$values = list(num.threads=1)

# Train a model using the above learner for a subset of the task:


ti1 <- Sys.time()
learner$train(task)
ti2 <- Sys.time()
as.numeric(difftime(ti2, ti1, unit="secs"))






simd <- simuldata(N=200, ni=50, beta=c(1, 1, -1, 0, 0), sdbinter=1, sdbslope=1, sdeps=1, fixed="none")$dataset
simd$index <- NULL

# Define the task for the top-down classification rule:
task = as_task_regr(y ~ ., data = simd)

# Initialize the learner for the top-down classification rule:
learner = lrn("regr.ranger")
# learner$param_set$values = list(num.threads=1)

# Train a model using the above learner for a subset of the task:


ti1 <- Sys.time()
learner$train(task)
ti2 <- Sys.time()
as.numeric(difftime(ti2, ti1, unit="secs"))




ti1 <- Sys.time()
ui <- ranger(y ~ ., data=simd)
ti2 <- Sys.time()
as.numeric(difftime(ti2, ti1, unit="secs"))


ti1 <- Sys.time()
ui <- ranger(y ~ ., data=simd, num.threads = 1)
ti2 <- Sys.time()
as.numeric(difftime(ti2, ti1, unit="secs"))




ui <- ranger(y ~ ., data=simd)





learner$model




# set_threads(learner_temp, n = 1)

