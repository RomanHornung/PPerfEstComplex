setwd("Z:/Projects/DESTATIS/PredErrorComplex/PPerfEstComplex")










xseq <- seq(-10.5, 10.5, length=400)



seasons <- 1:10
strengths <- c("weak", "medium-strong", "strong")
variables <- c("baseline", "x1", "x2", "x3")

ggdata <- expand.grid(x=xseq, variable=variables, season=seasons, strength=strengths, stringsAsFactors = FALSE)
ggdata <- ggdata[,ncol(ggdata):1]



getcoef <- function(t, startval, stopval) {
  
  tstart <- 0
  tend <- 1
  
  b1 <- (stopval - startval)/(tend - tstart)
  b0 <- startval - b1*tstart
  
  return(b0 + b1*t)
  
}






scengrid <- expand.grid(variable=variables[-1], season=seasons, strength=strengths, stringsAsFactors = FALSE)
scengrid <- scengrid[,ncol(scengrid):1]

scengrid$xmu <- NA

ts <- seq(0.05,0.95, length=10)

for(i in 1:nrow(scengrid)) {
  if (scengrid$strength[i]=="weak" & scengrid$variable[i]=="x1") {
    scengrid$xmu[i] <- getcoef(ts[scengrid$season[i]], startval=0, stopval = 2)
  }
  if (scengrid$strength[i]=="weak" & scengrid$variable[i]=="x2") {
    scengrid$xmu[i] <- getcoef(ts[scengrid$season[i]], startval=0, stopval = 1)
  }
  if (scengrid$strength[i]=="weak" & scengrid$variable[i]=="x3") {
    scengrid$xmu[i] <- getcoef(ts[scengrid$season[i]], startval=0, stopval = -2)
  }
  if (scengrid$strength[i]=="medium-strong" & scengrid$variable[i]=="x1") {
    scengrid$xmu[i] <- getcoef(ts[scengrid$season[i]], startval=0, stopval = 4)
  }
  if (scengrid$strength[i]=="medium-strong" & scengrid$variable[i]=="x2") {
    scengrid$xmu[i] <- getcoef(ts[scengrid$season[i]], startval=0, stopval = 2)
  }
  if (scengrid$strength[i]=="medium-strong" & scengrid$variable[i]=="x3") {
    scengrid$xmu[i] <- getcoef(ts[scengrid$season[i]], startval=0, stopval = -4)
  }
  if (scengrid$strength[i]=="strong" & scengrid$variable[i]=="x1") {
    scengrid$xmu[i] <- getcoef(ts[scengrid$season[i]], startval=0, stopval = 8)
  }
  if (scengrid$strength[i]=="strong" & scengrid$variable[i]=="x2") {
    scengrid$xmu[i] <- getcoef(ts[scengrid$season[i]], startval=0, stopval = 4)
  }
  if (scengrid$strength[i]=="strong" & scengrid$variable[i]=="x3") {
    scengrid$xmu[i] <- getcoef(ts[scengrid$season[i]], startval=0, stopval = -8)
  }
}




ggdata$y <- NA

for(i in seq(along=strengths)) {
  for(j in seq(along=seasons)) {
    for(k in seq(along=variables)) {
      
      indtemp <- which(ggdata$strength==strengths[i] & ggdata$season==seasons[j] & ggdata$variable==variables[k])
      
      if (variables[k]=="baseline")
        ggdata$y[indtemp] <- dnorm(xseq, mean=0, sd=1)
      else
        ggdata$y[indtemp] <- dnorm(xseq, mean=scengrid$xmu[scengrid$strength==strengths[i] & scengrid$season==seasons[j] & scengrid$variable==variables[k]], sd=1)
      
    }
  }
}



ggdata$season <- factor(paste0("season ", ggdata$season), levels=paste0("season ", 1:10))



library("ggplot2")


library("scales")
default_colors <- hue_pal()(3)
default_colors

# Custom colors for the 4 factors
custom_colors <- c("baseline" = "darkgrey", "x1" = "#F8766D", "x2" = "#00BA38", "x3" = "#619CFF")

# Custom line types for the 4 factors
custom_linetypes <- c("baseline" = "dashed", "x1" = "solid", "x2" = "solid", "x3" = "solid")



p1 <- ggplot(data = ggdata[ggdata$strength == "weak",], aes(x = x, y = y, color = variable, linetype = variable)) +
  geom_line() +
  facet_wrap(~season, ncol = 1) +
  ggtitle("weak") +
  theme_bw() +
  theme(plot.title=element_text(size=16), axis.title = element_blank(), axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), strip.text.x=element_text(size=15), legend.position = "none") +
  scale_color_manual(values = custom_colors) +
  scale_linetype_manual(values = custom_linetypes)
p1

p2 <- ggplot(data = ggdata[ggdata$strength == "medium-strong",], aes(x = x, y = y, color = variable, linetype = variable)) +
  geom_line() +
  facet_wrap(~season, ncol = 1) +
  ggtitle("medium-strong") +
  theme_bw() +
  theme(plot.title=element_text(size=16), axis.title = element_blank(), axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), strip.text.x=element_text(size=15), legend.position = "none") +
  scale_color_manual(values = custom_colors) +
  scale_linetype_manual(values = custom_linetypes)
p2

p3 <- ggplot(data = ggdata[ggdata$strength == "strong",], aes(x = x, y = y, color = variable, linetype = variable)) +
  geom_line() +
  facet_wrap(~season, ncol = 1) +
  ggtitle("strong") +
  theme_bw() +
  theme(plot.title=element_text(size=16), axis.title = element_blank(), axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), strip.text.x=element_text(size=15), legend.position = "none") +
  scale_color_manual(values = custom_colors) +
  scale_linetype_manual(values = custom_linetypes)
p3

library("ggpubr")
p <- ggarrange(p1, p2, p3, ncol = 3, nrow = 1)#, widths=c(0.7, 0.7, 1))
p

ggsave("./concdrift/results/figures/FigureS7.pdf", plot=p, width=12, height=16)



















xseq <- seq(-4, 16, length=400)



seasons <- 1:10
strengths <- c("weak", "medium-strong", "strong")
variables <- c("baseline", "y")

ggdata <- expand.grid(x=xseq, variable=variables, season=seasons, strength=strengths, stringsAsFactors = FALSE)
ggdata <- ggdata[,ncol(ggdata):1]



getcoef <- function(t, startval, stopval) {
  
  tstart <- 0
  tend <- 1
  
  b1 <- (stopval - startval)/(tend - tstart)
  b0 <- startval - b1*tstart
  
  return(b0 + b1*t)
  
}






scengrid <- expand.grid(variable=variables[-1], season=seasons, strength=strengths, stringsAsFactors = FALSE)
scengrid <- scengrid[,ncol(scengrid):1]

scengrid$xmu <- NA
scengrid$xsd <- NA

ts <- seq(0.05,0.95, length=10)

for(i in 1:nrow(scengrid)) {
  if (scengrid$strength[i]=="weak") {
    scengrid$ymu[i] <- getcoef(ts[scengrid$season[i]], startval=0, stopval = 2)
    scengrid$ysd[i] <- sqrt(getcoef(ts[scengrid$season[i]], startval=0, stopval = 2))
  }
  if (scengrid$strength[i]=="medium-strong") {
    scengrid$ymu[i] <- getcoef(ts[scengrid$season[i]], startval=0, stopval = 4)
    scengrid$ysd[i] <- sqrt(getcoef(ts[scengrid$season[i]], startval=0, stopval = 4))
  }
  if (scengrid$strength[i]=="strong") {
    scengrid$ymu[i] <- getcoef(ts[scengrid$season[i]], startval=0, stopval = 8)
    scengrid$ysd[i] <- sqrt(getcoef(ts[scengrid$season[i]], startval=0, stopval = 8))
  }
}




ggdata$y <- NA

for(i in seq(along=strengths)) {
  for(j in seq(along=seasons)) {
    for(k in seq(along=variables)) {
      
      indtemp <- which(ggdata$strength==strengths[i] & ggdata$season==seasons[j] & ggdata$variable==variables[k])
      
      if (variables[k]=="baseline")
        ggdata$y[indtemp] <- dnorm(xseq, mean=0, sd=1)
      else
        ggdata$y[indtemp] <- dnorm(xseq, mean=scengrid$ymu[scengrid$strength==strengths[i] & scengrid$season==seasons[j] & scengrid$variable==variables[k]], 
                                   sd=scengrid$ysd[scengrid$strength==strengths[i] & scengrid$season==seasons[j] & scengrid$variable==variables[k]])
      
    }
  }
}



ggdata$season <- factor(paste0("season ", ggdata$season), levels=paste0("season ", 1:10))





library("ggplot2")

# Custom colors for the 4 factors
custom_colors <- c("baseline" = "darkgrey", "y" = "black")

# Custom line types for the 4 factors
custom_linetypes <- c("baseline" = "dashed", "y" = "solid")



p1 <- ggplot(data = ggdata[ggdata$strength == "weak",], aes(x = x, y = y, color = variable, linetype = variable)) +
  geom_line() +
  facet_wrap(~season, ncol = 1, scales="free_y") +
  ggtitle("weak") +
  theme_bw() +
  theme(plot.title=element_text(size=16), axis.title = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
        strip.text.x=element_text(size=15), legend.position = "none") +
  scale_color_manual(values = custom_colors) +
  scale_linetype_manual(values = custom_linetypes)
p1

p2 <- ggplot(data = ggdata[ggdata$strength == "medium-strong",], aes(x = x, y = y, color = variable, linetype = variable)) +
  geom_line() +
  facet_wrap(~season, ncol = 1, scales="free_y") +
  ggtitle("medium-strong") +
  theme_bw() +
  theme(plot.title=element_text(size=16), axis.title = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
        strip.text.x=element_text(size=15), legend.position = "none") +
  scale_color_manual(values = custom_colors) +
  scale_linetype_manual(values = custom_linetypes)
p2

p3 <- ggplot(data = ggdata[ggdata$strength == "strong",], aes(x = x, y = y, color = variable, linetype = variable)) +
  geom_line() +
  facet_wrap(~season, ncol = 1, scales="free_y") +
  ggtitle("strong") +
  theme_bw() +
  theme(plot.title=element_text(size=16), axis.title = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
        strip.text.x=element_text(size=15), legend.position = "none") +
  scale_color_manual(values = custom_colors) +
  scale_linetype_manual(values = custom_linetypes)
p3

library("ggpubr")
p <- ggarrange(p1, p2, p3, ncol = 3, nrow = 1)#, widths=c(0.7, 0.7, 1))
p

ggsave("./concdrift/results/figures/FigureS8.pdf", plot=p, width=12, height=16)





























xseq <- seq(-8, 13, length=400)

par(mfrow=c(4,3))
par(mar=c(0, 0, 0, 0))
n <- 10
x1end <- 2
x2end <- -2
x1seq <- seq(0, x1end, length=n+1)[-1]
x2seq <- seq(0, x2end, length=n+1)[-1]
for(i in seq(along=x1seq)) {
  plot(xseq, dnorm(xseq, mean=0, sd=1), ty="l", xlab="", ylab="")
  lines(xseq, dnorm(xseq, mean=x1seq[i], sd=1), col=2)
  lines(xseq, dnorm(xseq, mean=x2seq[i], sd=1), col=3)
}
par(mar=c(5.1, 4.1, 4.1, 2.1))
par(mfrow=c(1,1))




xseq <- seq(-8, 13, length=400)

par(mfrow=c(4,3))
par(mar=c(0, 0, 0, 0))
n <- 10
x1end <- 4
x2end <- -4
x1seq <- seq(0, x1end, length=n+1)[-1]
x2seq <- seq(0, x2end, length=n+1)[-1]
for(i in seq(along=x1seq)) {
  plot(xseq, dnorm(xseq, mean=0, sd=1), ty="l", xlab="", ylab="")
  lines(xseq, dnorm(xseq, mean=x1seq[i], sd=1), col=2)
  lines(xseq, dnorm(xseq, mean=x2seq[i], sd=1), col=3)
}
par(mar=c(5.1, 4.1, 4.1, 2.1))
par(mfrow=c(1,1))



xseq <- seq(-8, 13, length=400)

par(mfrow=c(4,3))
par(mar=c(0, 0, 0, 0))
n <- 10
x1end <- 8
x2end <- -8
x1seq <- seq(0, x1end, length=n+1)[-1]
x2seq <- seq(0, x2end, length=n+1)[-1]
for(i in seq(along=x1seq)) {
  plot(xseq, dnorm(xseq, mean=0, sd=1), ty="l", xlab="", ylab="")
  lines(xseq, dnorm(xseq, mean=x1seq[i], sd=1), col=2)
  lines(xseq, dnorm(xseq, mean=x2seq[i], sd=1), col=3)
}
par(mar=c(5.1, 4.1, 4.1, 2.1))
par(mfrow=c(1,1))





par(mfrow=c(4,2))
par(mar=c(0, 0, 0, 0))
n <- 8
x1end <- 4
x2end <- -2
x1seq <- seq(0, x1end, length=n+1)[-1]
x2seq <- seq(0, x2end, length=n+1)[-1]
for(i in seq(along=x1seq)) {
  plot(xseq, dnorm(xseq, mean=0, sd=1), ty="l", xlab="", ylab="")
  lines(xseq, dnorm(xseq, mean=x1seq[i], sd=1), col=2)
  lines(xseq, dnorm(xseq, mean=x2seq[i], sd=1), col=3)
}
par(mar=c(5.1, 4.1, 4.1, 2.1))
par(mfrow=c(1,1))



par(mfrow=c(4,2))
par(mar=c(0, 0, 0, 0))
n <- 8
x1end <- 8
x2end <- -4
x1seq <- seq(0, x1end, length=n+1)[-1]
x2seq <- seq(0, x2end, length=n+1)[-1]
for(i in seq(along=x1seq)) {
  plot(xseq, dnorm(xseq, mean=0, sd=1), ty="l", xlab="", ylab="")
  lines(xseq, dnorm(xseq, mean=x1seq[i], sd=1), col=2)
  lines(xseq, dnorm(xseq, mean=x2seq[i], sd=1), col=3)
}
par(mar=c(5.1, 4.1, 4.1, 2.1))
par(mfrow=c(1,1))








xseq <- seq(-8, 13, length=400)

par(mfrow=c(4,2))
par(mar=c(0, 0, 0, 0))
n <- 8
ymeanend <- 1.5
yvarend <- 1.5
ymeanseq <- seq(0, ymeanend, length=n+1)[-1]
yvarseq <- seq(1, yvarend, length=n+1)[-1]
for(i in seq(along=x1seq)) {
  plot(xseq, dnorm(xseq, mean=0, sd=1), ty="l", xlab="", ylab="")
  lines(xseq, dnorm(xseq, mean=ymeanseq[i], sd=sqrt(yvarseq[i])), col=3)
}
par(mar=c(5.1, 4.1, 4.1, 2.1))
par(mfrow=c(1,1))


par(mfrow=c(4,2))
par(mar=c(0, 0, 0, 0))
n <- 8
ymeanend <- 3
yvarend <- 3
ymeanseq <- seq(0, ymeanend, length=n+1)[-1]
yvarseq <- seq(1, yvarend, length=n+1)[-1]
for(i in seq(along=x1seq)) {
  plot(xseq, dnorm(xseq, mean=0, sd=1), ty="l", xlab="", ylab="")
  lines(xseq, dnorm(xseq, mean=ymeanseq[i], sd=sqrt(yvarseq[i])), col=3)
}
par(mar=c(5.1, 4.1, 4.1, 2.1))
par(mfrow=c(1,1))


par(mfrow=c(4,2))
par(mar=c(0, 0, 0, 0))
n <- 8
ymeanend <- 6
yvarend <- 6
ymeanseq <- seq(0, ymeanend, length=n+1)[-1]
yvarseq <- seq(1, yvarend, length=n+1)[-1]
for(i in seq(along=x1seq)) {
  plot(xseq, dnorm(xseq, mean=0, sd=1), ty="l", xlab="", ylab="")
  lines(xseq, dnorm(xseq, mean=ymeanseq[i], sd=sqrt(yvarseq[i])), col=3)
}
par(mar=c(5.1, 4.1, 4.1, 2.1))
par(mfrow=c(1,1))




par(mfrow=c(4,2))
par(mar=c(0, 0, 0, 0))
n <- 8
ymeanend <- 2
yvarend <- 2
ymeanseq <- seq(0, ymeanend, length=n+1)[-1]
yvarseq <- seq(1, yvarend, length=n+1)[-1]
for(i in seq(along=x1seq)) {
  plot(xseq, dnorm(xseq, mean=0, sd=1), ty="l", xlab="", ylab="")
  lines(xseq, dnorm(xseq, mean=ymeanseq[i], sd=sqrt(yvarseq[i])), col=3)
}
par(mar=c(5.1, 4.1, 4.1, 2.1))
par(mfrow=c(1,1))


par(mfrow=c(4,2))
par(mar=c(0, 0, 0, 0))
n <- 8
ymeanend <- 4
yvarend <- 4
ymeanseq <- seq(0, ymeanend, length=n+1)[-1]
yvarseq <- seq(1, yvarend, length=n+1)[-1]
for(i in seq(along=x1seq)) {
  plot(xseq, dnorm(xseq, mean=0, sd=1), ty="l", xlab="", ylab="")
  lines(xseq, dnorm(xseq, mean=ymeanseq[i], sd=sqrt(yvarseq[i])), col=3)
}
par(mar=c(5.1, 4.1, 4.1, 2.1))
par(mfrow=c(1,1))


par(mfrow=c(4,2))
par(mar=c(0, 0, 0, 0))
n <- 8
ymeanend <- 8
yvarend <- 8
ymeanseq <- seq(0, ymeanend, length=n+1)[-1]
yvarseq <- seq(1, yvarend, length=n+1)[-1]
for(i in seq(along=x1seq)) {
  plot(xseq, dnorm(xseq, mean=0, sd=1), ty="l", xlab="", ylab="")
  lines(xseq, dnorm(xseq, mean=ymeanseq[i], sd=sqrt(yvarseq[i])), col=3)
}
par(mar=c(5.1, 4.1, 4.1, 2.1))
par(mfrow=c(1,1))











# 
# 
# lines(xseq, dnorm(xseq, mean=2, sd=1), col=2)
# lines(xseq, dnorm(xseq, mean=-0.5, sd=1), col=3)
# lines(xseq, dnorm(xseq, mean=-1, sd=1), col=3)



# Parameter:

# Only covariate shift vs. Covariate Shift & Label Shift (2)
# Weak covariate shift vs. strong covariate shift (2) 
# Weak label shift vs. strong label shift (2)
# 





getcoef <- function(t, startval, stopval) {
  
  # startval <- 0
  # stopval <- 8
  tstart <- 0
  tend <- 1
  
  b1 <- (stopval - startval)/(tend - tstart)
  b0 <- startval - b1*tstart
  
  return(b0 + b1*t)
  
}




# t <- 0.5


sim_obs <- function(t, x1muend=8, x2muend=4, x3muend=-8,
                    ymuend=6, yvarend=6) {
  
  x1 <- rnorm(1, mean=getcoef(t, startval=0, stopval=x1muend), sd=1)
  x2 <- rnorm(1, mean=getcoef(t, startval=0, stopval=x2muend), sd=1)
  x3 <- rnorm(1, mean=getcoef(t, startval=0, stopval=x3muend), sd=1)
  
  xvec <- c(x1, x2, x3, rnorm(2))
  
  y <- getcoef(t, startval=0, stopval=ymuend) + xvec[1]*2 + xvec[2]*(-1) + 
    xvec[3]*2 +rnorm(1, mean=0, sd=sqrt(getcoef(t, startval=1, stopval=yvarend)))
  
  return(c(xvec, y))
  
}


sim_dataset <- function(timepoints, x1muend=8, x2muend=4, x3muend=-8, ymuend=6, yvarend=6) {
  
  dataset <- data.frame(t(sapply(timepoints, function(x) 
    sim_obs(x, x1muend=x1muend, x2muend=x2muend, x3muend=x3muend, 
            ymuend=ymuend, yvarend=yvarend))))
  names(dataset)[ncol(dataset)] <- "y"
  
  return(dataset)
  
}





set.seed(1234)

n <- 1000


sizes <- rep(floor(n/7), 7)
if(n - 7*floor(n/7) > 0)
  sizes[1:(n - 7*floor(n/7))] <- sizes[1:(n - 7*floor(n/7))] + 1
train_sets1 <- lapply(cumsum(sizes[-length(sizes)]), function(x) 1:x)
train_sets2 <- train_sets1[-length(train_sets1)]

test_sets1 <- lapply(data.frame(rbind(cumsum(sizes[-length(sizes)]) + 1, cumsum(sizes)[-1])), function(x) x[1]:x[2])
test_sets2 <- test_sets1[-1]




seasonbreaks <- seq(0, 1, length=8)
seasonbreaks <- c(seasonbreaks[1:6], (seasonbreaks[6]+seasonbreaks[7])/2, seasonbreaks[7], (seasonbreaks[7]+seasonbreaks[8])/2, seasonbreaks[8])



x1muend <- 2; x2muend <- 1; x3muend <- -2; ymuend <- 1.5; yvarend <- 1.5


datatrain <- sim_dataset(seq(seasonbreaks[1], seasonbreaks[6], length=n),
                         x1muend=x1muend, x2muend=x2muend, x3muend=x3muend, ymuend=ymuend, yvarend=yvarend)

require("mlr3")
require("mlr3temporal")
require("mlr3verse")

# if (method=="lm")
#   learner_temp <- lrn("regr.lm")
# if (method=="rf")
  learner_temp <- lrn("regr.ranger")

# lgr::get_logger("mlr3")$set_threshold("warn")

task <- as_task_regr(datatrain, target="y")
# task_i$set_col_roles(cols="index", remove_from="feature")
# subsamp0.8 <- rsmp("subsampling", repeats = 100, ratio = 0.8)



# Regular CV:

cv <- rsmp("repeated_cv", repeats = 10, folds = 5)
cv$instantiate(task)
result_cv <- resample(task=task, learner=learner_temp, resampling=cv)
mse_cv <- result_cv$aggregate(msr("regr.mse"))


# One-season look ahead temporal CV:

tempcv <- rsmp("custom")
tempcv$instantiate(task, train_sets1, test_sets1)

result_tempcv <- resample(task=task, learner=learner_temp, resampling=tempcv)
mse_tempcv1 <- result_tempcv$aggregate(msr("regr.mse"))



# Two-seasons look ahead temporal CV:

tempcv <- rsmp("custom")
tempcv$instantiate(task, train_sets2, test_sets2)

result_tempcv <- resample(task=task, learner=learner_temp, resampling=tempcv)
mse_tempcv2 <- result_tempcv$aggregate(msr("regr.mse"))



# Hold-out: one season look-ahead:

learner_temp$train(task, row_ids = train_sets1[[length(train_sets1)]])

predictions <- learner_temp$predict(task, row_ids = test_sets1[[length(test_sets1)]])
mse_tempholdout1 <- predictions$score(msr("regr.mse"))



# Hold-out: two seasons look-ahead:

learner_temp$train(task, row_ids = train_sets2[[length(train_sets2)]])

predictions <- learner_temp$predict(task, row_ids = test_sets2[[length(test_sets2)]])
mse_tempholdout2 <- predictions$score(msr("regr.mse"))





train_task <- as_task_regr(datatrain, target = "y")
learner_temp$train(train_task)

mse_true <- c()

for(count in 1:5) {
  
  # End of last training season:
  datatest <- sim_dataset(rep(seasonbreaks[count+5], 100000),
                          x1muend=x1muend, x2muend=x2muend, x3muend=x3muend, ymuend=ymuend, yvarend=yvarend)

  test_task <- as_task_regr(datatest, target = "y")
  predictions <- learner_temp$predict(test_task)
  mse_true[count] <- predictions$score(msr("regr.mse"))
  
}


# mse_true <- c()
# 
# for(count in 1:5) {
#   
#   # End of last training season:
#   datatest <- sim_dataset(rep(seasonbreaks[count+5], 100000),
#                           x1muend=x1muend, x2muend=x2muend, x3muend=x3muend, ymuend=ymuend, yvarend=yvarend)
#   
#   datacompl <- rbind(datatrain, datatest)
#   ntrain <- nrow(datatrain)
#   
#   # Define the task for the top-down classification rule:
#   task <- as_task_regr(datacompl, target="y")
#   
#   
#   # Learn on the whole dataset:
#   learner_temp$train(task, row_ids = 1:ntrain)
#   
#   
#   predictions <- learner_temp$predict(task, row_ids = (ntrain+1):nrow(datacompl))
#   mse_true[count] <- predictions$score(msr("regr.mse"))
#   
# }







# datacompl <- rbind(datatrain, datatest_1, datatest_2, datatest_3, datatest_4, datatest_5)
# ntrain <- nrow(datatrain); ntest_1 <- nrow(datatest_1); ntest_2 <- nrow(datatest_2)
# ntest_3 <- nrow(datatest_3); ntest_4 <- nrow(datatest_4); ntest_5 <- nrow(datatest_5)
# rm(datatrain, datatest_1, datatest_2, datatest_3, datatest_4, datatest_5); gc()
# 
# 
# # Define the task for the top-down classification rule:
# task <- as_task_regr(datacompl, target="y")
# 
# 
# # Learn on the whole dataset:
# learner_temp$train(task, row_ids = 1:ntrain)
# 
# 
# predictions <- learner_temp$predict(task, row_ids = (ntrain+1):(ntrain+ntest_1))
# mse_true_1 <- predictions$score(msr("regr.mse"))
# 
# predictions <- learner_temp$predict(task, row_ids = (ntrain+ntest_1+1):(ntrain+ntest_1+ntest_2))
# mse_true_2 <- predictions$score(msr("regr.mse"))
# 
# predictions <- learner_temp$predict(task, row_ids = (ntrain+ntest_1+ntest_2+1):(ntrain+ntest_1+ntest_2+ntest_3))
# mse_true_3 <- predictions$score(msr("regr.mse"))
# 
# predictions <- learner_temp$predict(task, row_ids = (ntrain+ntest_1+ntest_2+ntest_3+1):(ntrain+ntest_1+ntest_2+ntest_3+ntest_4))
# mse_true_4 <- predictions$score(msr("regr.mse"))
# 
# predictions <- learner_temp$predict(task, row_ids = (ntrain+ntest_1+ntest_2+ntest_3+ntest_4+1):(ntrain+ntest_1+ntest_2+ntest_3+ntest_4+ntest_5))
# mse_true_5 <- predictions$score(msr("regr.mse"))



# Vorhere Vorgehensweise:

# > # Regular CV:
#   > mse_cv
# regr.mse 
# 1.703663 
# > # One-season look ahead temporal CV:
#   > mse_tempcv1
# regr.mse 
# 2.116256 
# > # Two-seasons look ahead temporal CV:
#   > mse_tempcv2
# regr.mse 
# 2.465029 
# > # Hold-out: one season look-ahead:
#   > mse_tempholdout1
# regr.mse 
# 2.156121 
# > # Hold-out: two seasons look-ahead:
#   > mse_tempholdout2
# regr.mse 
# 2.419771 
# > 
#   > 
#   > 
#   > # True MSE end of last training season:
#   > mse_true_1
# regr.mse 
# 2.111301 
# > # True MSE mid of first follow-up season:
#   > mse_true_2
# regr.mse 
# 2.302465 
# > # True MSE end first follow-up season:
#   > mse_true_3
# regr.mse 
# 2.537849 
# > # True MSE mid of second follow-up season:
#   > mse_true_4
# regr.mse 
# 2.79865 
# > # True MSE end of second follow-up season:
#   > mse_true_5
# regr.mse 
# 3.164249 



# Regular CV:
mse_cv
# One-season look ahead temporal CV:
mse_tempcv1
# Two-seasons look ahead temporal CV:
mse_tempcv2
# Hold-out: one season look-ahead:
mse_tempholdout1
# Hold-out: two seasons look-ahead:
mse_tempholdout2



# True MSE end of last training season:
mse_true_1
# True MSE mid of first follow-up season:
mse_true_2
# True MSE end first follow-up season:
mse_true_3
# True MSE mid of second follow-up season:
mse_true_4
# True MSE end of second follow-up season:
mse_true_5








set.seed(1234)

n <- 1000


sizes <- rep(floor(n/7), 7)
if(n - 7*floor(n/7) > 0)
  sizes[1:(n - 7*floor(n/7))] <- sizes[1:(n - 7*floor(n/7))] + 1
train_sets1 <- lapply(cumsum(sizes[-length(sizes)]), function(x) 1:x)
train_sets2 <- train_sets1[-length(train_sets1)]

test_sets1 <- lapply(data.frame(rbind(cumsum(sizes[-length(sizes)]) + 1, cumsum(sizes)[-1])), function(x) x[1]:x[2])
test_sets2 <- test_sets1[-1]




seasonbreaks <- seq(0, 1, length=8)
seasonbreaks <- c(seasonbreaks[1:6], (seasonbreaks[6]+seasonbreaks[7])/2, seasonbreaks[7], (seasonbreaks[7]+seasonbreaks[8])/2, seasonbreaks[8])



x1muend <- 8; x2muend <- 4; x3muend <- -8; ymuend <- 6; yvarend <- 6


datatrain <- sim_dataset(seq(seasonbreaks[1], seasonbreaks[6], length=n),
                         x1muend=x1muend, x2muend=x2muend, x3muend=x3muend, ymuend=ymuend, yvarend=yvarend)


tempobj <- ranger(y ~ ., data=datatrain[trainind,], num.trees=1000)

library("ranger")
trainind <- 1:round((6/7)*n)
testind <- (round((6/7)*n)+1):n
tempobj <- ranger(y ~ ., data=datatrain[trainind,], num.trees=1000)

plot(predict(tempobj, data=datatrain[testind,])$predictions, datatrain[testind,]$y)
cor(predict(tempobj, data=datatrain[testind,])$predictions, datatrain[testind,]$y)


trainind <- 1:round((5/7)*n)
testind <- (round((5/7)*n)+1):n
tempobj <- ranger(y ~ ., data=datatrain[trainind,], num.trees=1000)

plot(predict(tempobj, data=datatrain[testind,])$predictions, datatrain[testind,]$y)
cor(predict(tempobj, data=datatrain[testind,])$predictions, datatrain[testind,]$y)


trainind <- 1:round((4/7)*n)
testind <- (round((4/7)*n)+1):n
tempobj <- ranger(y ~ ., data=datatrain[trainind,], num.trees=1000)

plot(predict(tempobj, data=datatrain[testind,])$predictions, datatrain[testind,]$y)
cor(predict(tempobj, data=datatrain[testind,])$predictions, datatrain[testind,]$y)




library("ranger")
trainind <- 1:round((6/7)*n)
testind <- (round((6/7)*n)+1):n
tempobj <- ranger(y ~ ., data=datatrain[trainind,], num.trees=1000)

plot(predict(tempobj, data=datatrain[testind,])$predictions, datatrain[testind,]$y)
cor(predict(tempobj, data=datatrain[testind,])$predictions, datatrain[testind,]$y)


trainind <- 1:round((5/7)*n)
testind <- (round((5/7)*n)+1):round((6/7)*n)
tempobj <- ranger(y ~ ., data=datatrain[trainind,], num.trees=1000)

plot(predict(tempobj, data=datatrain[testind,])$predictions, datatrain[testind,]$y)
cor(predict(tempobj, data=datatrain[testind,])$predictions, datatrain[testind,]$y)


trainind <- 1:round((4/7)*n)
testind <- (round((4/7)*n)+1):round((5/7)*n)
tempobj <- ranger(y ~ ., data=datatrain[trainind,], num.trees=1000)

plot(predict(tempobj, data=datatrain[testind,])$predictions, datatrain[testind,]$y)
cor(predict(tempobj, data=datatrain[testind,])$predictions, datatrain[testind,]$y)










set.seed(1234)

n <- 1000


sizes <- rep(floor(n/7), 7)
if(n - 7*floor(n/7) > 0)
  sizes[1:(n - 7*floor(n/7))] <- sizes[1:(n - 7*floor(n/7))] + 1
train_sets1 <- lapply(cumsum(sizes[-length(sizes)]), function(x) 1:x)
train_sets2 <- train_sets1[-length(train_sets1)]

test_sets1 <- lapply(data.frame(rbind(cumsum(sizes[-length(sizes)]) + 1, cumsum(sizes)[-1])), function(x) x[1]:x[2])
test_sets2 <- test_sets1[-1]




seasonbreaks <- seq(0, 1, length=8)
seasonbreaks <- c(seasonbreaks[1:6], (seasonbreaks[6]+seasonbreaks[7])/2, seasonbreaks[7], (seasonbreaks[7]+seasonbreaks[8])/2, seasonbreaks[8])



x1muend <- 4; x2muend <- 2; x3muend <- -4; ymuend <- 3; yvarend <- 3


datatrain <- sim_dataset(seq(seasonbreaks[1], seasonbreaks[6], length=n),
                         x1muend=x1muend, x2muend=x2muend, x3muend=x3muend, ymuend=ymuend, yvarend=yvarend)




library("ranger")
trainind <- 1:round((6/7)*n)
testind <- (round((6/7)*n)+1):n
tempobj <- ranger(y ~ ., data=datatrain[trainind,], num.trees=1000)

plot(predict(tempobj, data=datatrain[testind,])$predictions, datatrain[testind,]$y)
cor(predict(tempobj, data=datatrain[testind,])$predictions, datatrain[testind,]$y)


trainind <- 1:round((5/7)*n)
testind <- (round((5/7)*n)+1):n
tempobj <- ranger(y ~ ., data=datatrain[trainind,], num.trees=1000)

plot(predict(tempobj, data=datatrain[testind,])$predictions, datatrain[testind,]$y)
cor(predict(tempobj, data=datatrain[testind,])$predictions, datatrain[testind,]$y)


trainind <- 1:round((4/7)*n)
testind <- (round((4/7)*n)+1):n
tempobj <- ranger(y ~ ., data=datatrain[trainind,], num.trees=1000)

plot(predict(tempobj, data=datatrain[testind,])$predictions, datatrain[testind,]$y)
cor(predict(tempobj, data=datatrain[testind,])$predictions, datatrain[testind,]$y)




library("ranger")
trainind <- 1:round((6/7)*n)
testind <- (round((6/7)*n)+1):n
tempobj <- ranger(y ~ ., data=datatrain[trainind,], num.trees=1000)

plot(predict(tempobj, data=datatrain[testind,])$predictions, datatrain[testind,]$y)
cor(predict(tempobj, data=datatrain[testind,])$predictions, datatrain[testind,]$y)


trainind <- 1:round((5/7)*n)
testind <- (round((5/7)*n)+1):round((6/7)*n)
tempobj <- ranger(y ~ ., data=datatrain[trainind,], num.trees=1000)

plot(predict(tempobj, data=datatrain[testind,])$predictions, datatrain[testind,]$y)
cor(predict(tempobj, data=datatrain[testind,])$predictions, datatrain[testind,]$y)


trainind <- 1:round((4/7)*n)
testind <- (round((4/7)*n)+1):round((5/7)*n)
tempobj <- ranger(y ~ ., data=datatrain[trainind,], num.trees=1000)

plot(predict(tempobj, data=datatrain[testind,])$predictions, datatrain[testind,]$y)
cor(predict(tempobj, data=datatrain[testind,])$predictions, datatrain[testind,]$y)







set.seed(1234)

n <- 1000


sizes <- rep(floor(n/7), 7)
if(n - 7*floor(n/7) > 0)
  sizes[1:(n - 7*floor(n/7))] <- sizes[1:(n - 7*floor(n/7))] + 1
train_sets1 <- lapply(cumsum(sizes[-length(sizes)]), function(x) 1:x)
train_sets2 <- train_sets1[-length(train_sets1)]

test_sets1 <- lapply(data.frame(rbind(cumsum(sizes[-length(sizes)]) + 1, cumsum(sizes)[-1])), function(x) x[1]:x[2])
test_sets2 <- test_sets1[-1]




seasonbreaks <- seq(0, 1, length=8)
seasonbreaks <- c(seasonbreaks[1:6], (seasonbreaks[6]+seasonbreaks[7])/2, seasonbreaks[7], (seasonbreaks[7]+seasonbreaks[8])/2, seasonbreaks[8])



x1muend <- 2; x2muend <- 1; x3muend <- -2; ymuend <- 1.5; yvarend <- 1.5


datatrain <- sim_dataset(seq(seasonbreaks[1], seasonbreaks[6], length=n),
                         x1muend=x1muend, x2muend=x2muend, x3muend=x3muend, ymuend=ymuend, yvarend=yvarend)




library("ranger")
trainind <- 1:round((6/7)*n)
testind <- (round((6/7)*n)+1):n
tempobj <- ranger(y ~ ., data=datatrain[trainind,], num.trees=1000)

plot(predict(tempobj, data=datatrain[testind,])$predictions, datatrain[testind,]$y)
cor(predict(tempobj, data=datatrain[testind,])$predictions, datatrain[testind,]$y)


trainind <- 1:round((5/7)*n)
testind <- (round((5/7)*n)+1):n
tempobj <- ranger(y ~ ., data=datatrain[trainind,], num.trees=1000)

plot(predict(tempobj, data=datatrain[testind,])$predictions, datatrain[testind,]$y)
cor(predict(tempobj, data=datatrain[testind,])$predictions, datatrain[testind,]$y)


trainind <- 1:round((4/7)*n)
testind <- (round((4/7)*n)+1):n
tempobj <- ranger(y ~ ., data=datatrain[trainind,], num.trees=1000)

plot(predict(tempobj, data=datatrain[testind,])$predictions, datatrain[testind,]$y)
cor(predict(tempobj, data=datatrain[testind,])$predictions, datatrain[testind,]$y)




library("ranger")
trainind <- 1:round((6/7)*n)
testind <- (round((6/7)*n)+1):n
tempobj <- ranger(y ~ ., data=datatrain[trainind,], num.trees=1000)

plot(predict(tempobj, data=datatrain[testind,])$predictions, datatrain[testind,]$y)
cor(predict(tempobj, data=datatrain[testind,])$predictions, datatrain[testind,]$y)


trainind <- 1:round((5/7)*n)
testind <- (round((5/7)*n)+1):round((6/7)*n)
tempobj <- ranger(y ~ ., data=datatrain[trainind,], num.trees=1000)

plot(predict(tempobj, data=datatrain[testind,])$predictions, datatrain[testind,]$y)
cor(predict(tempobj, data=datatrain[testind,])$predictions, datatrain[testind,]$y)


trainind <- 1:round((4/7)*n)
testind <- (round((4/7)*n)+1):round((5/7)*n)
tempobj <- ranger(y ~ ., data=datatrain[trainind,], num.trees=1000)

plot(predict(tempobj, data=datatrain[testind,])$predictions, datatrain[testind,]$y)
cor(predict(tempobj, data=datatrain[testind,])$predictions, datatrain[testind,]$y)
























library("ranger")
trainind <- 1:round((6/7)*n)
tempobj <- ranger(y ~ ., data=datatrain[trainind,], num.trees=1000)

plot(predict(tempobj, data=datatrain[-trainind,])$predictions, datatrain[-trainind,]$y)
cor(predict(tempobj, data=datatrain[-trainind,])$predictions, datatrain[-trainind,]$y)


trainind <- 1:round((5/7)*n)
tempobj <- ranger(y ~ ., data=datatrain[trainind,], num.trees=1000)

plot(predict(tempobj, data=datatrain[-trainind,])$predictions, datatrain[-trainind,]$y)
cor(predict(tempobj, data=datatrain[-trainind,])$predictions, datatrain[-trainind,]$y)


trainind <- 1:round((4/7)*n)
tempobj <- ranger(y ~ ., data=datatrain[trainind,], num.trees=1000)

plot(predict(tempobj, data=datatrain[-trainind,])$predictions, datatrain[-trainind,]$y)
cor(predict(tempobj, data=datatrain[-trainind,])$predictions, datatrain[-trainind,]$y)









#asdf

simdataperm <- simdata

simdataperm$index <- sample(simdataperm$index)

ypredsrf <- rep(NA, nrow(simdataperm))

for(i in seq(along=unique(simdataperm$index))) {
  model <- ranger(y ~ ., data=simdataperm[simdataperm$index!=i,-ncol(simdataperm)])
  ypredsrf[simdataperm$index==i] <- predict(model, data=simdataperm[simdataperm$index==i,-ncol(simdataperm)])$predictions
}

plot(simdataperm$y, ypredsrf)
cor(simdataperm$y, ypredsrf)












# > # Regular CV:
#   > mse_cv
# regr.mse 
# 6.102252 
# > # One-season look ahead temporal CV:
#   > mse_tempcv1
# regr.mse 
# 8.247698 
# > # Two-seasons look ahead temporal CV:
#   > mse_tempcv2
# regr.mse 
# 8.894035 
# > # Hold-out: one season look-ahead:
#   > mse_tempholdout1
# regr.mse 
# 8.09707 
# > # Hold-out: two seasons look-ahead:
#   > mse_tempholdout2
# regr.mse 
# 10.45531 
# > 
#   > 
#   > 
#   > # True MSE end of last training season:
#   > mse_true_1
# regr.mse 
# 9.928108 
# > # True MSE mid of first follow-up season:
#   > mse_true_2
# regr.mse 
# 11.80236 
# > # True MSE end first follow-up season:
#   > mse_true_3
# regr.mse 
# 14.05882 
# > # True MSE mid of second follow-up season:
#   > mse_true_4
# regr.mse 
# 16.52623 
# > # True MSE end of second follow-up season:
#   > mse_true_5
# regr.mse 
# 18.58472



# Als n?chstes Schauen, wie die singalst?rker ist, auch zu verschiedenen Zieptunkten.
# Und f?r die verschieden Starken signale







mse_true1
mse_true2

predictions$score(msr("regr.mse"))


  
  
  
  
  
  task_i$col_roles$group = "index"
  cv3$instantiate(task_i)
  result_cv3g <- resample(task=task_i, learner=learner_temp, resampling=cv3)
  mse_cv3g[i] <- result_cv3g$aggregate(msr("regr.mse"))
  

  
  
result <- list(mse_cv3=mse_cv3, mse_cv3g=mse_cv3g)
return(result)



task = tsk("petrol")
learner = lrn("forecast.VAR")
resampling = rsmp("forecast_cv", folds = 5, fixed_window = FALSE)
rr = resample(task, learner, resampling, store_models = TRUE)
rr$aggregate(msr("forecast.mae"))




head(dataset)





# Source the functions necessary for the simulation:

source("./clustdata/functions.R")



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


pdf("./clustdata/results/figures/signalstrength.pdf")

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


pdf("./clustdata/results/figures/signalstrength.pdf")

for(i in seq(along=settings)) {
  
  eval(parse(text=paste0("simd <- simuldata(", settings[i], ")")))
  
  makediagplot(simdata = simd)
  mtext(settings[i], side = 3, line = -1, outer = TRUE)
  
  cat(paste0("Iteration: ", i, " of ", length(settings)), "\n")
  
}

dev.off()





load("./clustdata/results/intermediate_results/N10ni5beta11-100sdbinter1sdbslope1sdeps1none.RData")


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

allfiles <- list.files("./clustdata/Results")
allfiles

stump <- gsub("TRUE.RData", "", grep("TRUE", allfiles, value=TRUE))

stumptrue <- paste0(stump, "TRUE.RData")
stumpfalse <- paste0(stump, "FALSE.RData")

stumptrue
stumpfalse

ls()

pdf("./clustdata/Results/figures/comparison.pdf", width=7, height=30)
par(mfrow=c(length(stumpfalse),2))
for(i in 1:length(stumpfalse)) {

load(paste0("./clustdata/Results/intermediate_results/", stumpfalse[i]))
resultfalse <- result
load(paste0("./clustdata/Results/intermediate_results/", stumptrue[i]))
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





