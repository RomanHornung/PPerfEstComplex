setwd("Z:/Projects/DESTATIS/PredErrorComplex/PPerfEstComplex")


# Load and pre-process the results:
###################################

load("./Simulations/concdrift/results/intermediate_results/scenariogrid.Rda")
load("./Simulations/concdrift/results/intermediate_results/results.Rda")



# mse_cv_rf, mse_cv_lm, mse_TSCV1s_rf, mse_TSCV1s_lm, mse_TSCV2s_rf, mse_TSCV2s_lm, mse_TSholdout_1s_rf, mse_TSholdout_1s_lm, mse_TSholdout_2s_rf, mse_TSholdout_2s_lm


# mse_true_endlast_rf, mse_true_endlast_lm, mse_true_mid1fu_rf, mse_true_mid1fu_lm, mse_true_end1fu_rf, mse_true_end1fu_lm, mse_true_mid2fu_rf, mse_true_mid2fu_lm, mse_true_end2fu_rf, mse_true_end2fu_lm

reorderind <- order(scenariogrid$xtrend, scenariogrid$ytrend, scenariogrid$n, scenariogrid$repetition)
scenariogrid <- scenariogrid[reorderind,]
results <- results[reorderind]


names(results[[1]])

head(scenariogrid)

length(results[[1]])

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







unique(res$n)


resultssafe <- results

cat(paste(gsub("_rf", "", grep("_rf", names(results[[1]]), value=TRUE)), collapse = "\", \""), "\n")


cat(paste(apply(matrix(data=1:length(results[[1]]), ncol=2, byrow=TRUE), 1, function(x) paste0("c(\"", names(results[[1]])[x[1]], "\", \"", names(results[[1]])[x[2]], "\")")), collapse=", "), "\n")




rownames(res) <- NULL

results <- reshape(res, varying=list(c("CV_rf", "CV_lm"), c("CV1s_rf", "CV1s_lm"), c("CV2s_rf", "CV2s_lm"), c("OOS1s_rf", "OOS1s_lm"), c("OOS2s_rf", "OOS2s_lm"), c("Els_rf", "Els_lm"), c("M1fus_rf", "M1fus_lm"), c("E1fus_rf", "E1fus_lm"), c("M2fus_rf", "M2fus_lm"), c("E2fus_rf", "E2fus_lm")),
                   v.names=c("CV", "CV1s", "CV2s", "OOS1s", "OOS2s", "Els", "M1fus", "E1fus", "M2fus", "E2fus"), 
                   timevar="predmethod", times=c("rf", "lm"),
                   direction="long")
results$predmethod <- factor(results$predmethod, levels=c("lm", "rf"))
results$id <- NULL


ui <- c("CV_rf", "CV_lm", "CV1s_rf", "CV1s_lm", "CV2s_rf", "CV2s_lm", "OOS1s_rf", "OOS1s_lm", "OOS2s_rf", "OOS2s_lm", "Els_rf", "Els_lm", "M1fus_rf", "M1fus_lm", "E1fus_rf", "E1fus_lm", "M2fus_rf", "M2fus_lm", "E2fus_rf", "E2fus_lm")
ui[!(ui %in% names(res))]


library("plyr")

meanvars <- setdiff(names(results), c("n", "xtrend", "ytrend", "repetition", "predmethod"))
eval(parse(text=paste0("resultsum <- ddply(results, .variables=c(\"n\", \"xtrend\", \"ytrend\", \"predmethod\"), .fun=summarise, ", paste(paste0(meanvars, " = mean(", meanvars, ")"), collapse = ", "), ")")))


resultstemp <- results
resultstemp$E1fus_version2 <- resultstemp$E1fus
allmses <- c("CV", "CV1s", "OOS1s", "Els", "M1fus", "E1fus", "CV2s", "OOS2s", "E1fus_version2", "M2fus", "E2fus")
resultstemplong <- reshape(resultstemp, varying=allmses,
                       v.names="mse", 
                       timevar="type", times=allmses,
                       direction="long")
resultstemplong$type <- factor(resultstemplong$type, levels=allmses)




# library("ranger")
# datatemp <- res[,1:3]
# datatemp$y <- res$E2fus_lm - res$CV_lm
# 
# model <- ranger(dependent.variable.name = "y", importance="permutation", respect.unordered.factors="partition", data=datatemp, num.trees=5000)
# 
# model2 <- interactionfor(dependent.variable.name = "y", data=datatemp)
# model2$eim.qual.sorted
# model2$eim.quant.sorted
# 
# p <- plotPair(pair=c("ytrend", "xtrend"), yvarname="y", data=datatemp)
# p
# ggsave("./Simulations/concdrift/results/figures/interaction_xtrend_ytrend_lm.pdf", width=14, height=7)
# 
# 
# 
# datatemp <- res[,1:3]
# datatemp$y <- res$E2fus_rf - res$CV_rf
# 
# model <- ranger(dependent.variable.name = "y", importance="permutation", respect.unordered.factors="partition", data=datatemp, num.trees=5000)
# 
# library("diversityForest")
# 
# model2 <- interactionfor(dependent.variable.name = "y", data=datatemp)
# model2$eim.qual.sorted
# model2$eim.quant.sorted
# 
# p <- plotPair(pair=c("ytrend", "xtrend"), yvarname="y", data=datatemp)
# p
# ggsave("./Simulations/concdrift/results/figures/interaction_xtrend_ytrend_rf.pdf", width=14, height=7)






res <- resultstemplong[resultstemplong$predmethod=="rf" & resultstemplong$n=="n = 100",]

newlabels <- levels(res$type)
newlabels[newlabels == "E1fus_version2"] <- "E1fus"

# colors to use:
scales::hue_pal()(3)[1:2]

library("ggplot2")
p <- ggplot(data=res, aes(x=type, y=mse, fill=type)) + theme_bw() +
  geom_boxplot() + facet_wrap(~ xtrend + ytrend, ncol = 4, scales="free_y") +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_text(size=14), 
        axis.text.x = element_text(angle=45, hjust = 1, color="black", size=12), 
        axis.text.y = element_text(color="black", size=11), 
        strip.text = element_text(size=12),
        legend.position = "none") + ylab("Estimated and true MSE values") +
  scale_fill_manual(values = c(rep("#F8766D", 3), rep("#00BA38", 3), rep("#F8766D", 2), rep("#00BA38", 3))) +
  scale_x_discrete(labels = newlabels) + geom_vline(xintercept = 6.5)
p

ggsave("./Simulations/concdrift/results/figures/mse_n100_rf.pdf", width=11, height=9)




res <- resultstemplong[resultstemplong$predmethod=="rf" & resultstemplong$n=="n = 1000",]

library("ggplot2")
p <- ggplot(data=res, aes(x=type, y=mse, fill=type)) + theme_bw() +
  geom_boxplot() + facet_wrap(~ xtrend + ytrend, ncol = 4, scales="free_y") +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_text(size=14), 
        axis.text.x = element_text(angle=45, hjust = 1, color="black", size=12), 
        axis.text.y = element_text(color="black", size=11), 
        strip.text = element_text(size=12),
        legend.position = "none") + ylab("Estimated and true MSE values") +
  scale_fill_manual(values = c(rep("#F8766D", 3), rep("#00BA38", 3), rep("#F8766D", 2), rep("#00BA38", 3))) +
  scale_x_discrete(labels = newlabels) + geom_vline(xintercept = 6.5)
p

ggsave("./Simulations/concdrift/results/figures/mse_n1000_rf.pdf", width=11, height=9)





res <- resultstemplong[resultstemplong$predmethod=="lm" & resultstemplong$n=="n = 100",]

newlabels <- levels(res$type)
newlabels[newlabels == "E1fus_version2"] <- "E1fus"

# colors to use:
scales::hue_pal()(3)[1:2]

library("ggplot2")
p <- ggplot(data=res, aes(x=type, y=mse, fill=type)) + theme_bw() +
  geom_boxplot() + facet_wrap(~ xtrend + ytrend, ncol = 4, scales="free_y") +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_text(size=14), 
        axis.text.x = element_text(angle=45, hjust = 1, color="black", size=12), 
        axis.text.y = element_text(color="black", size=11), 
        strip.text = element_text(size=12),
        legend.position = "none") + ylab("Estimated and true MSE values") +
  scale_fill_manual(values = c(rep("#F8766D", 3), rep("#00BA38", 3), rep("#F8766D", 2), rep("#00BA38", 3))) +
  scale_x_discrete(labels = newlabels) + geom_vline(xintercept = 6.5)
p

ggsave("./Simulations/concdrift/results/figures/mse_n100_lm.pdf", width=11, height=9)




res <- resultstemplong[resultstemplong$predmethod=="lm" & resultstemplong$n=="n = 1000",]

library("ggplot2")
p <- ggplot(data=res, aes(x=type, y=mse, fill=type)) + theme_bw() +
  geom_boxplot() + facet_wrap(~ xtrend + ytrend, ncol = 4, scales="free_y") +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_text(size=14), 
        axis.text.x = element_text(angle=45, hjust = 1, color="black", size=12), 
        axis.text.y = element_text(color="black", size=11), 
        strip.text = element_text(size=12),
        legend.position = "none") + ylab("Estimated and true MSE values") +
  scale_fill_manual(values = c(rep("#F8766D", 3), rep("#00BA38", 3), rep("#F8766D", 2), rep("#00BA38", 3))) +
  scale_x_discrete(labels = newlabels) + geom_vline(xintercept = 6.5)
p

ggsave("./Simulations/concdrift/results/figures/mse_n1000_lm.pdf", width=11, height=9)












names(resultstemplong)


table(resultstemplong$xtrend)
table(resultstemplong$ytrend)

c("no covariate shift", "strong covariate shift")
c("no label shift", "strong label shift")



res <- resultstemplong[resultstemplong$n=="n = 500" & resultstemplong$xtrend %in% c("no covariate shift", "strong covariate shift") &
                         resultstemplong$ytrend %in% c("no label shift", "strong label shift"),]

newlabels <- levels(res$type)
newlabels[newlabels == "E1fus_version2"] <- "E1fus"

# colors to use:
scales::hue_pal()(3)[1:2]

library("ggplot2")
p <- ggplot(data=res, aes(x=type, y=mse, fill=type)) + theme_bw() +
  geom_boxplot() + facet_wrap(~ predmethod + xtrend + ytrend, ncol = 4, scales="free_y") +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_text(size=14), 
        axis.text.x = element_text(angle=45, hjust = 1, color="black", size=12), 
        axis.text.y = element_text(color="black", size=11), 
        strip.text = element_text(size=12),
        legend.position = "none") + ylab("Estimated and true MSE values") +
  scale_fill_manual(values = c(rep("#F8766D", 3), rep("#00BA38", 3), rep("#F8766D", 2), rep("#00BA38", 3))) +
  scale_x_discrete(labels = newlabels) + geom_vline(xintercept = 6.5)
p

ggsave("./Simulations/concdrift/results/figures/covdriftsim.pdf", width=13.5, height=7)










res <- resultstemplong[resultstemplong$predmethod=="rf" & resultstemplong$xtrend=="no covariate shift",]

newlabels <- levels(res$type)
newlabels[newlabels == "E1fus_version2"] <- "E1fus"

# colors to use:
scales::hue_pal()(3)[1:2]

library("ggplot2")
p <- ggplot(data=res, aes(x=type, y=mse, fill=type)) + theme_bw() +
  geom_boxplot() + facet_wrap(~ n + ytrend, ncol = 4, scales="free_y") +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_text(size=14), 
        axis.text.x = element_text(angle=45, hjust = 1, color="black", size=12), 
        axis.text.y = element_text(color="black", size=11), 
        strip.text = element_text(size=12),
        legend.position = "none") + ylab("Estimated and true MSE values") +
  scale_fill_manual(values = c(rep("#F8766D", 3), rep("#00BA38", 3), rep("#F8766D", 2), rep("#00BA38", 3))) +
  scale_x_discrete(labels = newlabels) + geom_vline(xintercept = 6.5)
p

ggsave("./Simulations/concdrift/results/figures/mse_nocovdrift_rf.pdf", width=11, height=9)




res <- resultstemplong[resultstemplong$predmethod=="rf" & resultstemplong$xtrend=="weak covariate shift",]

p <- ggplot(data=res, aes(x=type, y=mse, fill=type)) + theme_bw() +
  geom_boxplot() + facet_wrap(~ n + ytrend, ncol = 4, scales="free_y") +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_text(size=14), 
        axis.text.x = element_text(angle=45, hjust = 1, color="black", size=12), 
        axis.text.y = element_text(color="black", size=11), 
        strip.text = element_text(size=12),
        legend.position = "none") + ylab("Estimated and true MSE values") +
  scale_fill_manual(values = c(rep("#F8766D", 3), rep("#00BA38", 3), rep("#F8766D", 2), rep("#00BA38", 3))) +
  scale_x_discrete(labels = newlabels) + geom_vline(xintercept = 6.5)
p

ggsave("./Simulations/concdrift/results/figures/mse_weakcovdrift_rf.pdf", width=11, height=9)




res <- resultstemplong[resultstemplong$predmethod=="rf" & resultstemplong$xtrend=="medium-strong covariate shift",]

p <- ggplot(data=res, aes(x=type, y=mse, fill=type)) + theme_bw() +
  geom_boxplot() + facet_wrap(~ n + ytrend, ncol = 4, scales="free_y") +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_text(size=14), 
        axis.text.x = element_text(angle=45, hjust = 1, color="black", size=12), 
        axis.text.y = element_text(color="black", size=11), 
        strip.text = element_text(size=12),
        legend.position = "none") + ylab("Estimated and true MSE values") +
  scale_fill_manual(values = c(rep("#F8766D", 3), rep("#00BA38", 3), rep("#F8766D", 2), rep("#00BA38", 3))) +
  scale_x_discrete(labels = newlabels) + geom_vline(xintercept = 6.5)
p

ggsave("./Simulations/concdrift/results/figures/mse_mediumcovdrift_rf.pdf", width=11, height=9)




res <- resultstemplong[resultstemplong$predmethod=="rf" & resultstemplong$xtrend=="strong covariate shift",]

p <- ggplot(data=res, aes(x=type, y=mse, fill=type)) + theme_bw() +
  geom_boxplot() + facet_wrap(~ n + ytrend, ncol = 4, scales="free_y") +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_text(size=14), 
        axis.text.x = element_text(angle=45, hjust = 1, color="black", size=12), 
        axis.text.y = element_text(color="black", size=11), 
        strip.text = element_text(size=12),
        legend.position = "none") + ylab("Estimated and true MSE values") +
  scale_fill_manual(values = c(rep("#F8766D", 3), rep("#00BA38", 3), rep("#F8766D", 2), rep("#00BA38", 3))) +
  scale_x_discrete(labels = newlabels) + geom_vline(xintercept = 6.5)
p

ggsave("./Simulations/concdrift/results/figures/mse_strongcovdrift_rf.pdf", width=11, height=9)














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
  scale_fill_manual(values = c(rep("#F8766D", 3), rep("#00BA38", 3), rep("#F8766D", 2), rep("#00BA38", 3))) +
  scale_x_discrete(labels = newlabels) + geom_vline(xintercept = 6.5)
p

ggsave("./Simulations/concdrift/results/figures/mse_nocovdrift_lm.pdf", width=11, height=9)




res <- resultstemplong[resultstemplong$predmethod=="lm" & resultstemplong$xtrend=="weak covariate shift",]

p <- ggplot(data=res, aes(x=type, y=mse, fill=type)) + theme_bw() +
  geom_boxplot() + facet_wrap(~ n + ytrend, ncol = 4, scales="free_y") +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_text(size=14), 
        axis.text.x = element_text(angle=45, hjust = 1, color="black", size=12), 
        axis.text.y = element_text(color="black", size=11), 
        strip.text = element_text(size=12),
        legend.position = "none") + ylab("Estimated and true MSE values") +
  scale_fill_manual(values = c(rep("#F8766D", 3), rep("#00BA38", 3), rep("#F8766D", 2), rep("#00BA38", 3))) +
  scale_x_discrete(labels = newlabels) + geom_vline(xintercept = 6.5)
p

ggsave("./Simulations/concdrift/results/figures/mse_weakcovdrift_lm.pdf", width=11, height=9)




res <- resultstemplong[resultstemplong$predmethod=="lm" & resultstemplong$xtrend=="medium-strong covariate shift",]

p <- ggplot(data=res, aes(x=type, y=mse, fill=type)) + theme_bw() +
  geom_boxplot() + facet_wrap(~ n + ytrend, ncol = 4, scales="free_y") +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_text(size=14), 
        axis.text.x = element_text(angle=45, hjust = 1, color="black", size=12), 
        axis.text.y = element_text(color="black", size=11), 
        strip.text = element_text(size=12),
        legend.position = "none") + ylab("Estimated and true MSE values") +
  scale_fill_manual(values = c(rep("#F8766D", 3), rep("#00BA38", 3), rep("#F8766D", 2), rep("#00BA38", 3))) +
  scale_x_discrete(labels = newlabels) + geom_vline(xintercept = 6.5)
p

ggsave("./Simulations/concdrift/results/figures/mse_mediumcovdrift_lm.pdf", width=11, height=9)




res <- resultstemplong[resultstemplong$predmethod=="lm" & resultstemplong$xtrend=="strong covariate shift",]

p <- ggplot(data=res, aes(x=type, y=mse, fill=type)) + theme_bw() +
  geom_boxplot() + facet_wrap(~ n + ytrend, ncol = 4, scales="free_y") +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_text(size=14), 
        axis.text.x = element_text(angle=45, hjust = 1, color="black", size=12), 
        axis.text.y = element_text(color="black", size=11), 
        strip.text = element_text(size=12),
        legend.position = "none") + ylab("Estimated and true MSE values") +
  scale_fill_manual(values = c(rep("#F8766D", 3), rep("#00BA38", 3), rep("#F8766D", 2), rep("#00BA38", 3))) +
  scale_x_discrete(labels = newlabels) + geom_vline(xintercept = 6.5)
p

ggsave("./Simulations/concdrift/results/figures/mse_strongcovdrift_lm.pdf", width=11, height=9)









source("./Simulations/concdrift/functions.R")

n <- 500

# We simulate 7 seasons, 5 seasons for training and 2 seasons as future seasons
# for testing. The following object give the time intervals of the seasons,
# where we also include the midpoints in the last two seasons because we will
# evaluate the error here as well.

seasonbreaks <- seq(0, 1, length=11)
seasonbreaks <- c(seasonbreaks[1:9], (seasonbreaks[9]+seasonbreaks[10])/2, 
                  seasonbreaks[10], (seasonbreaks[10]+seasonbreaks[11])/2, 
                  seasonbreaks[11])




x1muend <- 8; x2muend <- 4; x3muend <- -8
ymuend <- 0; yvarend <- 1


# Simulate the training dataset:

datatrain_covshift <- sim_dataset(seq(seasonbreaks[1], seasonbreaks[9], length=n),
                         x1muend=x1muend, x2muend=x2muend, x3muend=x3muend, 
                         ymuend=ymuend, yvarend=yvarend)






x1muend <- 0; x2muend <- 0; x3muend <- 0
ymuend <- 0; yvarend <- 1


# Simulate the training dataset:

datatrain_Nocovshift <- sim_dataset(seq(seasonbreaks[1], seasonbreaks[9], length=n),
                          x1muend=x1muend, x2muend=x2muend, x3muend=x3muend, 
                          ymuend=ymuend, yvarend=yvarend)



boxplot(datatrain_covshift$y, datatrain_Nocovshift$y)
apply(cbind(datatrain_covshift$y, datatrain_Nocovshift$y), 2, var)


dim(datatrain_covshift)
dim(datatrain_Nocovshift)

par(mfrow=c(3,2))
plot(datatrain_covshift$X1, datatrain_covshift$y)
cor(datatrain_covshift$X1, datatrain_covshift$y)

plot(datatrain_Nocovshift$X1, datatrain_Nocovshift$y)
cor(datatrain_Nocovshift$X1, datatrain_Nocovshift$y)


plot(datatrain_covshift$X2, datatrain_covshift$y)
cor(datatrain_covshift$X2, datatrain_covshift$y)

plot(datatrain_Nocovshift$X2, datatrain_Nocovshift$y)
cor(datatrain_Nocovshift$X2, datatrain_Nocovshift$y)


plot(datatrain_covshift$X3, datatrain_covshift$y)
cor(datatrain_covshift$X3, datatrain_covshift$y)

plot(datatrain_Nocovshift$X3, datatrain_Nocovshift$y)
cor(datatrain_Nocovshift$X3, datatrain_Nocovshift$y)
par(mfrow=c(1,1))




library("ranger")
ranger(dependent.variable.name = "y", data=datatrain_covshift, num.trees=1000)$prediction.error
ranger(dependent.variable.name = "y", data=datatrain_Nocovshift, num.trees=1000)$prediction.error

ranger(dependent.variable.name = "y", data=datatrain_covshift, num.trees=1000)$prediction.error/ranger(dependent.variable.name = "y", data=datatrain_Nocovshift, num.trees=1000)$prediction.error


mod_covshift <- ranger(dependent.variable.name = "y", data=datatrain_covshift[1:400,], num.trees=1000)
library("measures")
MSE(truth=datatrain_covshift[-(1:400),]$y, response=predict(mod_covshift, data=datatrain_covshift[-(1:400),])$predictions)

mod_Nocovshift <- ranger(dependent.variable.name = "y", data=datatrain_Nocovshift[1:400,], num.trees=1000)
library("measures")
MSE(truth=datatrain_Nocovshift[-(1:400),]$y, response=predict(mod_Nocovshift, data=datatrain_Nocovshift[-(1:400),])$predictions)


MSE(truth=datatrain_covshift[-(1:400),]$y, response=predict(mod_covshift, data=datatrain_covshift[-(1:400),])$predictions)/MSE(truth=datatrain_Nocovshift[-(1:400),]$y, response=predict(mod_Nocovshift, data=datatrain_Nocovshift[-(1:400),])$predictions)



boxplot(datatrain_covshift$y, datatrain_Nocovshift$y)
apply(cbind(datatrain_covshift$y, datatrain_Nocovshift$y), 2, var)

mean(replicate(10000, MSE(truth=datatrain_Nocovshift$y, response=sample(datatrain_Nocovshift$y))))

mean(replicate(10000, MSE(truth=datatrain_covshift$y, response=sample(datatrain_covshift$y))))


ranger(dependent.variable.name = "y", data=datatrain_Nocovshift, num.trees=1000)$prediction.error

# ---> MSE (unterschied zw. FU2 und CV) bei covdrift mit Labelshift geringer als bei no covdrift
# mit Labelshift, weil die Varianz der y-Werte (und deshalb die MSE-Werte) bei covdrift allgemein geringer sind.

# Fragen: Warum ist die Varianz der y-Werte bei covdrift geringer, wenn es einen labelshift gibt?
# Und warum ist dann nicht so, wenn es keinen labelshift gibt? Da ist die Varianz der y-Werte bei
# covdrift größer.

# --> Einzige Erklärung: es muss mit varrienden Intercept zu tun haben bei labelshift.

# Warum führt dieser Intercept dazu, dass bei covdrift die Varianz geringer wird.



par(mfrow=c(2,3))

plot(datatrain_covshift$X1)
plot(datatrain_covshift$X2)
plot(datatrain_covshift$X3)

linpred_covshift <- sapply(seq(seasonbreaks[1], seasonbreaks[9], length=n), function(t) getcoef(t, startval=0, stopval=ymuend)) + datatrain_covshift$X1*2 + datatrain_covshift$X2*(-1) + 
  datatrain_covshift$X3*2 


plot(datatrain_Nocovshift$X1)
plot(datatrain_Nocovshift$X2)
plot(datatrain_Nocovshift$X3)

linpred_Nocovshift <- sapply(seq(seasonbreaks[1], seasonbreaks[9], length=n), function(t) getcoef(t, startval=0, stopval=ymuend)) + datatrain_Nocovshift$X1*2 + datatrain_Nocovshift$X2*(-1) + 
  datatrain_Nocovshift$X3*2 

par(mfrow=c(1,1))

boxplot(linpred_covshift, linpred_Nocovshift)
apply(cbind(linpred_covshift, linpred_Nocovshift), 2, var)
13.4/9.6

# --> Die Varianz des lin. pred. ist bei covshift geringer.
# --> Warum?


boxplot(datatrain_covshift$X1*2 + datatrain_covshift$X2*(-1) + 
  datatrain_covshift$X3*2,
  datatrain_Nocovshift$X1*2 + datatrain_Nocovshift$X2*(-1) + 
    datatrain_Nocovshift$X3*2)
apply(cbind(datatrain_covshift$X1*2 + datatrain_covshift$X2*(-1) + 
          datatrain_covshift$X3*2,
        datatrain_Nocovshift$X1*2 + datatrain_Nocovshift$X2*(-1) + 
          datatrain_Nocovshift$X3*2), 2, var)

plot(sapply(seq(seasonbreaks[1], seasonbreaks[9], length=n), function(t) getcoef(t, startval=0, stopval=ymuend)), datatrain_covshift$X1*2 + datatrain_covshift$X2*(-1) + 
       datatrain_covshift$X3*2)
cor(sapply(seq(seasonbreaks[1], seasonbreaks[9], length=n), function(t) getcoef(t, startval=0, stopval=ymuend)), datatrain_covshift$X1*2 + datatrain_covshift$X2*(-1) + 
           datatrain_covshift$X3*2)
plot(sapply(seq(seasonbreaks[1], seasonbreaks[9], length=n), function(t) getcoef(t, startval=0, stopval=ymuend)), datatrain_Nocovshift$X1*2 + datatrain_Nocovshift$X2*(-1) + 
       datatrain_Nocovshift$X3*2)
cor(sapply(seq(seasonbreaks[1], seasonbreaks[9], length=n), function(t) getcoef(t, startval=0, stopval=ymuend)), datatrain_Nocovshift$X1*2 + datatrain_Nocovshift$X2*(-1) + 
      datatrain_Nocovshift$X3*2)

# --> Erklaerung: Bei covshift sind der intercept und der rest vom linearen Prädiktor negativ assoziert, was
# dazu führt, dass die Summe der beiden geringer wird (Var(X+Y)=Var(X)+Var(Y)+2*Cov(X,Y)).


plot(sapply(seq(seasonbreaks[1], seasonbreaks[9], length=n), function(t) getcoef(t, startval=0, stopval=ymuend)))
plot(datatrain_covshift$X1*2 + datatrain_covshift$X2*(-1) + 
       datatrain_covshift$X3*2)
plot(datatrain_Nocovshift$X1*2 + datatrain_Nocovshift$X2*(-1) + 
       datatrain_Nocovshift$X3*2)



# Finale Erklaerung, warum für Szenarien mit Label shift die Varianz von y geringer ist, wenn es covariate shift
# gibt wie wenn es keinen covariate shift gibt:
# Für Szenarien mit Labelshift ist der Intercept (linear) ansteigend. Und die Summe x_1*beta_1 + x_2*beta_2 + x_3*beta_3
# hat bei covariate shift einen leicht abfallenen Trend, da sich der positive und negative Trend von x_1*beta_1 und
# x_3*beta_3 jeweils canceln und da x_2*beta_2 einen leicht abfallenden Trend hat. Aus der Kombination dass 
# der Intercept ansteigend ist und x_1*beta_1 + x_2*beta_2 + x_3*beta_3 abfallend, ergibt sich dass die Summe
# beta_0(t) + x_1*beta_1 + x_2*beta_2 + x_3*beta_3 eine geringere Streuung hat als in dem Fall, dass es keinen
# Covariate shift gibt, weil in letzterem Fall x_1*beta_1 + x_2*beta_2 + x_3*beta_3 keinen abfallenden Trend
# hat.

# Und daraus, dass für Szenarien mit Label shift die Varianz von y geringer ist, wenn es covariate shift
# gibt wie wenn es keinen covariate shift gibt ergibt sich, dass für erstere Szenarien auch der MSE geringer
# ist, weil der MSE bei geringerer Varianz i.d.R. geringer wird.

# Wenn es keinen Label shift gibt ist die Varianz von  beta_0(t) + x_1*beta_1 + x_2*beta_2 + x_3*beta_3
# bei covariate shift größer, was erklärt, warum hier der MSE (wie man eigentlich erwarten würde) bei
# covariate shift auch größer ist.




boxplot(linpred_covshift + sapply(seq(seasonbreaks[1], seasonbreaks[9], length=n), function(t) rnorm(1, mean=0, sd=sqrt(getcoef(t, startval=1, stopval=yvarend)))),
        linpred_Nocovshift + sapply(seq(seasonbreaks[1], seasonbreaks[9], length=n), function(t) rnorm(1, mean=0, sd=sqrt(getcoef(t, startval=1, stopval=yvarend)))))
apply(cbind(linpred_covshift + sapply(seq(seasonbreaks[1], seasonbreaks[9], length=n), function(t) rnorm(1, mean=0, sd=sqrt(getcoef(t, startval=1, stopval=yvarend)))),
        linpred_Nocovshift + sapply(seq(seasonbreaks[1], seasonbreaks[9], length=n), function(t) rnorm(1, mean=0, sd=sqrt(getcoef(t, startval=1, stopval=yvarend))))), 2, var)
17.6/12.23




#asdf





# ---> Hier gehts weiter.




dim(datatrain)

plot(datatrain$X1)
plot(datatrain$X2)
plot(datatrain$X3)

count+5
datatest <- sim_dataset(rep(seasonbreaks[5+8], 100),
                        x1muend=x1muend, x2muend=x2muend, x3muend=x3muend, ymuend=ymuend, yvarend=yvarend)



plot(c(seq(seasonbreaks[1], seasonbreaks[9], length=n), rep(seasonbreaks[5+8], 100)), c(datatrain$X1, datatest$X1))
