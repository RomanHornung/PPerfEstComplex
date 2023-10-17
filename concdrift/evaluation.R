setwd("C:/Projects/DESTATIS/PredErrorComplex/PPerfEstComplex/PPerfEstComplex")


# Load and pre-process the results:
###################################

load("./concdrift/results/intermediate_results/scenariogrid.Rda")
load("./concdrift/results/intermediate_results/results.Rda")


reorderind <- order(scenariogrid$xtrend, scenariogrid$ytrend, scenariogrid$n, scenariogrid$repetition)
scenariogrid <- scenariogrid[reorderind,]
results <- results[reorderind]


res <- scenariogrid
res$seed <- NULL

res$CV_rf <- sapply(results, function(x) x$mse_cv_rf) 
res$CV_lm <- sapply(results, function(x) x$mse_cv_lm) 
res$CV1s_rf <- sapply(results, function(x) x$mse_TScv_1s_rf) 
res$CV1s_lm <- sapply(results, function(x) x$mse_TScv_1s_lm) 
res$CV2s_rf <- sapply(results, function(x) x$mse_TScv_2s_rf) 
res$CV2s_lm <- sapply(results, function(x) x$mse_TScv_2s_lm) 
res$OOS1s_rf <- sapply(results, function(x) x$mse_TSholdout_1s_rf) 
res$OOS1s_lm <- sapply(results, function(x) x$mse_TSholdout_1s_lm) 
res$OOS2s_rf <- sapply(results, function(x) x$mse_TSholdout_2s_rf) 
res$OOS2s_lm <- sapply(results, function(x) x$mse_TSholdout_2s_lm) 
res$Els_rf <- sapply(results, function(x) x$mse_true_endlast_rf) 
res$Els_lm <- sapply(results, function(x) x$mse_true_endlast_lm) 
res$M1fus_rf <- sapply(results, function(x) x$mse_true_mid1fu_rf) 
res$M1fus_lm <- sapply(results, function(x) x$mse_true_mid1fu_lm) 
res$E1fus_rf <- sapply(results, function(x) x$mse_true_end1fu_rf) 
res$E1fus_lm <- sapply(results, function(x) x$mse_true_end1fu_lm) 
res$M2fus_rf <- sapply(results, function(x) x$mse_true_mid2fu_rf) 
res$M2fus_lm <- sapply(results, function(x) x$mse_true_mid2fu_lm) 
res$E2fus_rf <- sapply(results, function(x) x$mse_true_end2fu_rf) 
res$E2fus_lm <- sapply(results, function(x) x$mse_true_end2fu_lm)


res$n <- paste0("n = ", res$n)
res$n <- factor(res$n, levels=c("n = 100", "n = 500", "n = 1000", "n = 3000"))

unique(res$xtrend)

res$xtrend <- as.character(res$xtrend)
res$xtrend[res$xtrend=="none"] <- "no covariate shift"
res$xtrend[res$xtrend=="weak"] <- "weak covariate shift"
res$xtrend[res$xtrend=="medium"] <- "medium-strong covariate shift"
res$xtrend[res$xtrend=="strong"] <- "strong covariate shift"
res$xtrend <- factor(res$xtrend, levels=c("no covariate shift", "weak covariate shift", 
                                          "medium-strong covariate shift", "strong covariate shift"))

res$ytrend <- as.character(res$ytrend)
res$ytrend[res$ytrend=="none"] <- "no label shift"
res$ytrend[res$ytrend=="weak"] <- "weak label shift"
res$ytrend[res$ytrend=="medium"] <- "medium-strong label shift"
res$ytrend[res$ytrend=="strong"] <- "strong label shift"
res$ytrend <- factor(res$ytrend, levels=c("no label shift", "weak label shift", 
                                          "medium-strong label shift", "strong label shift"))



rownames(res) <- NULL

results <- reshape(res, varying=list(c("CV_rf", "CV_lm"), c("CV1s_rf", "CV1s_lm"), c("CV2s_rf", "CV2s_lm"), c("OOS1s_rf", "OOS1s_lm"), c("OOS2s_rf", "OOS2s_lm"), c("Els_rf", "Els_lm"), c("M1fus_rf", "M1fus_lm"), c("E1fus_rf", "E1fus_lm"), c("M2fus_rf", "M2fus_lm"), c("E2fus_rf", "E2fus_lm")),
                   v.names=c("CV", "CV1s", "CV2s", "OOS1s", "OOS2s", "Els", "M1fus", "E1fus", "M2fus", "E2fus"), 
                   timevar="predmethod", times=c("rf", "lm"),
                   direction="long")
results$predmethod <- factor(results$predmethod, levels=c("lm", "rf"))
results$id <- NULL

# library("plyr")


resultstemp <- results
resultstemp$E1fus_version2 <- resultstemp$E1fus
allmses <- c("CV", "CV1s", "OOS1s", "Els", "M1fus", "E1fus", "CV2s", "OOS2s", "E1fus_version2", "M2fus", "E2fus")
resultstemplong <- reshape(resultstemp, varying=allmses,
                       v.names="mse", 
                       timevar="type", times=allmses,
                       direction="long")
resultstemplong$type <- factor(resultstemplong$type, levels=allmses)












res <- resultstemplong[resultstemplong$n=="n = 500" & resultstemplong$xtrend %in% c("no covariate shift", "strong covariate shift") &
                         resultstemplong$ytrend %in% c("no label shift", "strong label shift"),]

newlabels <- levels(res$type)
newlabels[newlabels == "E1fus_version2"] <- "E1fus"


library("RColorBrewer")

display.brewer.all(n=3, type="div")

colors <- brewer.pal(3, "RdBu")
selectedColors <- c(colors[1], colors[3])


levels(res$predmethod) <- c("linear models", "random forests")

library("ggplot2")
p <- ggplot(data=res, aes(x=type, y=mse, fill=type)) + theme_bw() +
  geom_boxplot() + facet_wrap(~ predmethod + xtrend + ytrend, ncol = 4, scales="free_y") +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_text(size=14), 
        axis.text.x = element_text(angle=45, hjust = 1, color="black", size=12), 
        axis.text.y = element_text(color="black", size=11), 
        strip.text = element_text(size=12),
        legend.position = "none") + ylab("Estimated and true MSE values") +
  scale_fill_manual(values = c(rep(selectedColors[1], 3), rep(selectedColors[2], 3), rep(selectedColors[1], 2), rep(selectedColors[2], 3))) +
  scale_x_discrete(labels = newlabels) + geom_vline(xintercept = 6.5)
p

ggsave("./concdrift/results/figures/Figure7.pdf", width=13.5, height=7)







res <- resultstemplong[resultstemplong$predmethod=="rf" & resultstemplong$xtrend=="no covariate shift",]

newlabels <- levels(res$type)
newlabels[newlabels == "E1fus_version2"] <- "E1fus"

library("ggplot2")
p <- ggplot(data=res, aes(x=type, y=mse, fill=type)) + theme_bw() +
  geom_boxplot() + facet_wrap(~ n + ytrend, ncol = 4, scales="free_y") +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_text(size=14), 
        axis.text.x = element_text(angle=45, hjust = 1, color="black", size=12), 
        axis.text.y = element_text(color="black", size=11), 
        strip.text = element_text(size=12),
        legend.position = "none") + ylab("Estimated and true MSE values") +
  scale_fill_manual(values = c(rep(selectedColors[1], 3), rep(selectedColors[2], 3), rep(selectedColors[1], 2), rep(selectedColors[2], 3))) +
  scale_x_discrete(labels = newlabels) + geom_vline(xintercept = 6.5)
p

ggsave("./concdrift/results/figures/FigureS13.pdf", width=11, height=9)




res <- resultstemplong[resultstemplong$predmethod=="rf" & resultstemplong$xtrend=="weak covariate shift",]

p <- ggplot(data=res, aes(x=type, y=mse, fill=type)) + theme_bw() +
  geom_boxplot() + facet_wrap(~ n + ytrend, ncol = 4, scales="free_y") +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_text(size=14), 
        axis.text.x = element_text(angle=45, hjust = 1, color="black", size=12), 
        axis.text.y = element_text(color="black", size=11), 
        strip.text = element_text(size=12),
        legend.position = "none") + ylab("Estimated and true MSE values") +
  scale_fill_manual(values = c(rep(selectedColors[1], 3), rep(selectedColors[2], 3), rep(selectedColors[1], 2), rep(selectedColors[2], 3))) +
  scale_x_discrete(labels = newlabels) + geom_vline(xintercept = 6.5)
p

ggsave("./concdrift/results/figures/FigureS14.pdf", width=11, height=9)




res <- resultstemplong[resultstemplong$predmethod=="rf" & resultstemplong$xtrend=="medium-strong covariate shift",]

p <- ggplot(data=res, aes(x=type, y=mse, fill=type)) + theme_bw() +
  geom_boxplot() + facet_wrap(~ n + ytrend, ncol = 4, scales="free_y") +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_text(size=14), 
        axis.text.x = element_text(angle=45, hjust = 1, color="black", size=12), 
        axis.text.y = element_text(color="black", size=11), 
        strip.text = element_text(size=12),
        legend.position = "none") + ylab("Estimated and true MSE values") +
  scale_fill_manual(values = c(rep(selectedColors[1], 3), rep(selectedColors[2], 3), rep(selectedColors[1], 2), rep(selectedColors[2], 3))) +
  scale_x_discrete(labels = newlabels) + geom_vline(xintercept = 6.5)
p

ggsave("./concdrift/results/figures/FigureS15.pdf", width=11, height=9)




res <- resultstemplong[resultstemplong$predmethod=="rf" & resultstemplong$xtrend=="strong covariate shift",]

p <- ggplot(data=res, aes(x=type, y=mse, fill=type)) + theme_bw() +
  geom_boxplot() + facet_wrap(~ n + ytrend, ncol = 4, scales="free_y") +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_text(size=14), 
        axis.text.x = element_text(angle=45, hjust = 1, color="black", size=12), 
        axis.text.y = element_text(color="black", size=11), 
        strip.text = element_text(size=12),
        legend.position = "none") + ylab("Estimated and true MSE values") +
  scale_fill_manual(values = c(rep(selectedColors[1], 3), rep(selectedColors[2], 3), rep(selectedColors[1], 2), rep(selectedColors[2], 3))) +
  scale_x_discrete(labels = newlabels) + geom_vline(xintercept = 6.5)
p

ggsave("./concdrift/results/figures/FigureS16.pdf", width=11, height=9)














res <- resultstemplong[resultstemplong$predmethod=="lm" & resultstemplong$xtrend=="no covariate shift",]

library("ggplot2")
p <- ggplot(data=res, aes(x=type, y=mse, fill=type)) + theme_bw() +
  geom_boxplot() + facet_wrap(~ n + ytrend, ncol = 4, scales="free_y") +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_text(size=14), 
        axis.text.x = element_text(angle=45, hjust = 1, color="black", size=12), 
        axis.text.y = element_text(color="black", size=11), 
        strip.text = element_text(size=12),
        legend.position = "none") + ylab("Estimated and true MSE values") +
  scale_fill_manual(values = c(rep(selectedColors[1], 3), rep(selectedColors[2], 3), rep(selectedColors[1], 2), rep(selectedColors[2], 3))) +
  scale_x_discrete(labels = newlabels) + geom_vline(xintercept = 6.5)
p

ggsave("./concdrift/results/figures/FigureS9.pdf", width=11, height=9)




res <- resultstemplong[resultstemplong$predmethod=="lm" & resultstemplong$xtrend=="weak covariate shift",]

p <- ggplot(data=res, aes(x=type, y=mse, fill=type)) + theme_bw() +
  geom_boxplot() + facet_wrap(~ n + ytrend, ncol = 4, scales="free_y") +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_text(size=14), 
        axis.text.x = element_text(angle=45, hjust = 1, color="black", size=12), 
        axis.text.y = element_text(color="black", size=11), 
        strip.text = element_text(size=12),
        legend.position = "none") + ylab("Estimated and true MSE values") +
  scale_fill_manual(values = c(rep(selectedColors[1], 3), rep(selectedColors[2], 3), rep(selectedColors[1], 2), rep(selectedColors[2], 3))) +
  scale_x_discrete(labels = newlabels) + geom_vline(xintercept = 6.5)
p

ggsave("./concdrift/results/figures/FigureS10.pdf", width=11, height=9)




res <- resultstemplong[resultstemplong$predmethod=="lm" & resultstemplong$xtrend=="medium-strong covariate shift",]

p <- ggplot(data=res, aes(x=type, y=mse, fill=type)) + theme_bw() +
  geom_boxplot() + facet_wrap(~ n + ytrend, ncol = 4, scales="free_y") +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_text(size=14), 
        axis.text.x = element_text(angle=45, hjust = 1, color="black", size=12), 
        axis.text.y = element_text(color="black", size=11), 
        strip.text = element_text(size=12),
        legend.position = "none") + ylab("Estimated and true MSE values") +
  scale_fill_manual(values = c(rep(selectedColors[1], 3), rep(selectedColors[2], 3), rep(selectedColors[1], 2), rep(selectedColors[2], 3))) +
  scale_x_discrete(labels = newlabels) + geom_vline(xintercept = 6.5)
p

ggsave("./concdrift/results/figures/FigureS11.pdf", width=11, height=9)




res <- resultstemplong[resultstemplong$predmethod=="lm" & resultstemplong$xtrend=="strong covariate shift",]

p <- ggplot(data=res, aes(x=type, y=mse, fill=type)) + theme_bw() +
  geom_boxplot() + facet_wrap(~ n + ytrend, ncol = 4, scales="free_y") +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_text(size=14), 
        axis.text.x = element_text(angle=45, hjust = 1, color="black", size=12), 
        axis.text.y = element_text(color="black", size=11), 
        strip.text = element_text(size=12),
        legend.position = "none") + ylab("Estimated and true MSE values") +
  scale_fill_manual(values = c(rep(selectedColors[1], 3), rep(selectedColors[2], 3), rep(selectedColors[1], 2), rep(selectedColors[2], 3))) +
  scale_x_discrete(labels = newlabels) + geom_vline(xintercept = 6.5)
p

ggsave("./concdrift/results/figures/FigureS12.pdf", width=11, height=9)











# Some calculations to support the reasoning in the last paragraph of Section B.2 of Supplementary Material 1.
#############################################################################################################


# Simulate four data sets: 1) no label shift, no feature shift,
# 2) no label shift, feature shift, 3) label shift, no feature shift,
# 4) label shift, feature shift.


source("./concdrift/functions.R")

n <- 500

seasonbreaks <- seq(0, 1, length=11)
seasonbreaks <- c(seasonbreaks[1:9], (seasonbreaks[9]+seasonbreaks[10])/2, 
                  seasonbreaks[10], (seasonbreaks[10]+seasonbreaks[11])/2, 
                  seasonbreaks[11])


set.seed(1234)

x1muend <- 0; x2muend <- 0; x3muend <- 0
ymuend <- 0; yvarend <- 1

datatrain_Noyshift_Nocovshift <- sim_dataset(seq(seasonbreaks[1], seasonbreaks[9], length=n),
                                             x1muend=x1muend, x2muend=x2muend, x3muend=x3muend, 
                                             ymuend=ymuend, yvarend=yvarend)


x1muend <- 8; x2muend <- 4; x3muend <- -8
ymuend <- 0; yvarend <- 1

datatrain_Noyshift_covshift <- sim_dataset(seq(seasonbreaks[1], seasonbreaks[9], length=n),
                                  x1muend=x1muend, x2muend=x2muend, x3muend=x3muend, 
                                  ymuend=ymuend, yvarend=yvarend)



x1muend <- 0; x2muend <- 0; x3muend <- 0
ymuend <- 8; yvarend <- 8

datatrain_yshift_Nocovshift <- sim_dataset(seq(seasonbreaks[1], seasonbreaks[9], length=n),
                                           x1muend=x1muend, x2muend=x2muend, x3muend=x3muend, 
                                           ymuend=ymuend, yvarend=yvarend)



x1muend <- 8; x2muend <- 4; x3muend <- -8
ymuend <- 8; yvarend <- 8

datatrain_yshift_covshift <- sim_dataset(seq(seasonbreaks[1], seasonbreaks[9], length=n),
                                           x1muend=x1muend, x2muend=x2muend, x3muend=x3muend, 
                                           ymuend=ymuend, yvarend=yvarend)





# Compare predictions obtained with the different data sets:


# Predictions into the future:

library("ranger")

mod_Noyshift_Nocovshift <- ranger(dependent.variable.name = "y", data=datatrain_Noyshift_Nocovshift[1:400,], num.trees=1000)
library("measures")
MSE(truth=datatrain_Noyshift_Nocovshift[-(1:400),]$y, response=predict(mod_Noyshift_Nocovshift, data=datatrain_Noyshift_Nocovshift[-(1:400),])$predictions)

mod_Noyshift_covshift <- ranger(dependent.variable.name = "y", data=datatrain_Noyshift_covshift[1:400,], num.trees=1000)
library("measures")
MSE(truth=datatrain_Noyshift_covshift[-(1:400),]$y, response=predict(mod_Noyshift_covshift, data=datatrain_Noyshift_covshift[-(1:400),])$predictions)



mod_yshift_Nocovshift <- ranger(dependent.variable.name = "y", data=datatrain_yshift_Nocovshift[1:400,], num.trees=1000)
library("measures")
MSE(truth=datatrain_yshift_Nocovshift[-(1:400),]$y, response=predict(mod_yshift_Nocovshift, data=datatrain_yshift_Nocovshift[-(1:400),])$predictions)

mod_yshift_covshift <- ranger(dependent.variable.name = "y", data=datatrain_yshift_covshift[1:400,], num.trees=1000)
library("measures")
MSE(truth=datatrain_yshift_covshift[-(1:400),]$y, response=predict(mod_yshift_covshift, data=datatrain_yshift_covshift[-(1:400),])$predictions)


# Internal validation (OOB-MSE of RFs):

ranger(dependent.variable.name = "y", data=datatrain_Noyshift_Nocovshift, num.trees=1000)$prediction.error
ranger(dependent.variable.name = "y", data=datatrain_Noyshift_covshift, num.trees=1000)$prediction.error

ranger(dependent.variable.name = "y", data=datatrain_yshift_Nocovshift, num.trees=1000)$prediction.error
ranger(dependent.variable.name = "y", data=datatrain_yshift_covshift, num.trees=1000)$prediction.error


# --> Results correspond to those in paper: If there is label shift, the MSE get smaller if there is feature shift in addition.




# Observation: If there is label shift, the variance of the label gets smaller if there is
# feature shift in addition (not the case for no label shift):

apply(cbind(datatrain_yshift_Nocovshift$y, datatrain_yshift_covshift$y), 2, var)

apply(cbind(datatrain_Noyshift_Nocovshift$y, datatrain_Noyshift_covshift$y), 2, var)

# --> This could explain why, in the presence of label shift, the MSE values get smaller if
# there is feature shift in addition because the smaller the variance, the smaller the MSE
# tends to be.



# But why is it that the variance of the label gets smaller when there is feature shift 
# in addition to label shift than when there is only label shift?



# The reason is that, in the case of label shift, the variance of the linear predictor
# gets smaller if there is feature shift in addition:

linpred_yshift_Nocovshift <- sapply(seq(seasonbreaks[1], seasonbreaks[9], length=n), function(t) getcoef(t, startval=0, stopval=ymuend)) + datatrain_yshift_Nocovshift$X1*2 + datatrain_yshift_Nocovshift$X2*(-1) + 
  datatrain_yshift_Nocovshift$X3*2 

linpred_yshift_covshift <- sapply(seq(seasonbreaks[1], seasonbreaks[9], length=n), function(t) getcoef(t, startval=0, stopval=ymuend)) + datatrain_yshift_covshift$X1*2 + datatrain_yshift_covshift$X2*(-1) + 
  datatrain_yshift_covshift$X3*2 

apply(cbind(linpred_yshift_Nocovshift, linpred_yshift_covshift), 2, var)


# But why is that the case?


# The reason for that is that, in the case of label shift, there is a negative association
# between the intercept and the rest of the linear predictor:

cor(sapply(seq(seasonbreaks[1], seasonbreaks[9], length=n), function(t) getcoef(t, startval=0, stopval=ymuend)), datatrain_yshift_Nocovshift$X1*2 + datatrain_yshift_Nocovshift$X2*(-1) + 
      datatrain_yshift_Nocovshift$X3*2)
cor(sapply(seq(seasonbreaks[1], seasonbreaks[9], length=n), function(t) getcoef(t, startval=0, stopval=ymuend)), datatrain_yshift_covshift$X1*2 + datatrain_yshift_covshift$X2*(-1) + 
      datatrain_yshift_covshift$X3*2)

# If two variables (in this case the intercept and the rest of the linear predictor)
# have a negative association, the variance of their sum gets smaller compared to when they
# don't have a negative association.
