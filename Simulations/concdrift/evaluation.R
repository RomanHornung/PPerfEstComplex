setwd("Z:/Projects/DESTATIS/PredErrorComplex/PPerfEstComplex")


# Load and pre-process the results:
###################################

load("./Simulations/concdrift/results/intermediate_results/scenariogrid.Rda")
load("./Simulations/concdrift/results/intermediate_results/results.Rda")



# mse_cv_rf, mse_cv_lm, mse_TScv_1s_rf, mse_TScv_1s_lm, mse_TScv_2s_rf, mse_TScv_2s_lm, mse_TSholdout_1s_rf, mse_TSholdout_1s_lm, mse_TSholdout_2s_rf, mse_TSholdout_2s_lm


# mse_true_endlast_rf, mse_true_endlast_lm, mse_true_mid1fu_rf, mse_true_mid1fu_lm, mse_true_end1fu_rf, mse_true_end1fu_lm, mse_true_mid2fu_rf, mse_true_mid2fu_lm, mse_true_end2fu_rf, mse_true_end2fu_lm

reorderind <- order(scenariogrid$xtrend, scenariogrid$ytrend, scenariogrid$n, scenariogrid$repetition)
scenariogrid <- scenariogrid[reorderind,]
results <- results[reorderind]


head(scenariogrid)

length(results[[1]])

res <- scenariogrid
res$seed <- NULL

res$cv_rf <- sapply(results, function(x) x$mse_cv_rf) 
res$cv_lm <- sapply(results, function(x) x$mse_cv_lm) 
res$cv_1s_rf <- sapply(results, function(x) x$mse_TScv_1s_rf) 
res$cv_1s_lm <- sapply(results, function(x) x$mse_TScv_1s_lm) 
res$cv_2s_rf <- sapply(results, function(x) x$mse_TScv_2s_rf) 
res$cv_2s_lm <- sapply(results, function(x) x$mse_TScv_2s_lm) 
res$ho_1s_rf <- sapply(results, function(x) x$mse_TSholdout_1s_rf) 
res$ho_1s_lm <- sapply(results, function(x) x$mse_TSholdout_1s_lm) 
res$ho_2s_rf <- sapply(results, function(x) x$mse_TSholdout_2s_rf) 
res$ho_2s_lm <- sapply(results, function(x) x$mse_TSholdout_2s_lm) 
res$els_rf <- sapply(results, function(x) x$mse_true_endlast_rf) 
res$els_lm <- sapply(results, function(x) x$mse_true_endlast_lm) 
res$m1fus_rf <- sapply(results, function(x) x$mse_true_mid1fu_rf) 
res$m1fus_lm <- sapply(results, function(x) x$mse_true_mid1fu_lm) 
res$e1fus_rf <- sapply(results, function(x) x$mse_true_end1fu_rf) 
res$e1fus_lm <- sapply(results, function(x) x$mse_true_end1fu_lm) 
res$m2fus_rf <- sapply(results, function(x) x$mse_true_mid2fu_rf) 
res$m2fus_lm <- sapply(results, function(x) x$mse_true_mid2fu_lm) 
res$e2fus_rf <- sapply(results, function(x) x$mse_true_end2fu_rf) 
res$e2fus_lm <- sapply(results, function(x) x$mse_true_end2fu_lm)


res$n <- paste0("n = ", res$n)
res$n <- factor(res$n, levels=c("n = 100", "n = 500", "n = 1000", "n = 3000"))

unique(res$xtrend)

res$xtrend <- as.character(res$xtrend)
res$xtrend[res$xtrend=="none"] <- "no covariate shift"
res$xtrend[res$xtrend=="weak"] <- "weak covariate shift"
res$xtrend[res$xtrend=="medium"] <- "medium covariate shift"
res$xtrend[res$xtrend=="strong"] <- "strong covariate shift"
res$xtrend <- factor(res$xtrend, levels=c("no covariate shift", "weak covariate shift", 
                                          "medium covariate shift", "strong covariate shift"))

res$ytrend <- as.character(res$ytrend)
res$ytrend[res$ytrend=="none"] <- "no label shift"
res$ytrend[res$ytrend=="weak"] <- "weak label shift"
res$ytrend[res$ytrend=="medium"] <- "medium label shift"
res$ytrend[res$ytrend=="strong"] <- "strong label shift"
res$ytrend <- factor(res$ytrend, levels=c("no label shift", "weak label shift", 
                                          "medium label shift", "strong label shift"))







unique(res$n)


resultssafe <- results

cat(paste(gsub("_rf", "", grep("_rf", names(results[[1]]), value=TRUE)), collapse = "\", \""), "\n")


cat(paste(apply(matrix(data=1:length(results[[1]]), ncol=2, byrow=TRUE), 1, function(x) paste0("c(\"", names(results[[1]])[x[1]], "\", \"", names(results[[1]])[x[2]], "\")")), collapse=", "), "\n")




rownames(res) <- NULL

results <- reshape(res, varying=list(c("cv_rf", "cv_lm"), c("cv_1s_rf", "cv_1s_lm"), c("cv_2s_rf", "cv_2s_lm"), c("ho_1s_rf", "ho_1s_lm"), c("ho_2s_rf", "ho_2s_lm"), c("els_rf", "els_lm"), c("m1fus_rf", "m1fus_lm"), c("e1fus_rf", "e1fus_lm"), c("m2fus_rf", "m2fus_lm"), c("e2fus_rf", "e2fus_lm")),
                   v.names=c("cv", "cv_1s", "cv_2s", "ho_1s", "ho_2s", "els", "m1fus", "e1fus", "m2fus", "e2fus"), 
                   timevar="predmethod", times=c("rf", "lm"),
                   direction="long")
results$predmethod <- factor(results$predmethod, levels=c("lm", "rf"))
results$id <- NULL


ui <- c("cv_rf", "cv_lm", "cv_1s_rf", "cv_1s_lm", "cv_2s_rf", "cv_2s_lm", "ho_1s_rf", "ho_1s_lm", "ho_2s_rf", "ho_2s_lm", "els_rf", "els_lm", "m1fus_rf", "m1fus_lm", "e1fus_rf", "e1fus_lm", "m2fus_rf", "m2fus_lm", "e2fus_rf", "e2fus_lm")
ui[!(ui %in% names(res))]


library("plyr")

meanvars <- setdiff(names(results), c("n", "xtrend", "ytrend", "repetition", "predmethod"))
eval(parse(text=paste0("resultsum <- ddply(results, .variables=c(\"n\", \"xtrend\", \"ytrend\", \"predmethod\"), .fun=summarise, ", paste(paste0(meanvars, " = mean(", meanvars, ")"), collapse = ", "), ")")))


resultstemp <- results
resultstemp$e1fus_version2 <- resultstemp$e1fus
allmses <- c("cv", "cv_1s", "ho_1s", "els", "m1fus", "e1fus", "cv_2s", "ho_2s", "e1fus_version2", "m2fus", "e2fus")
resultstemplong <- reshape(resultstemp, varying=allmses,
                       v.names="mse", 
                       timevar="type", times=allmses,
                       direction="long")
resultstemplong$type <- factor(resultstemplong$type, levels=allmses)





res <- resultstemplong[resultstemplong$predmethod=="rf" & resultstemplong$xtrend=="no covariate shift",]

newlabels <- levels(res$type)
newlabels[newlabels == "e1fus_version2"] <- "e1fus"

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




res <- resultstemplong[resultstemplong$predmethod=="rf" & resultstemplong$xtrend=="medium covariate shift",]

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




res <- resultstemplong[resultstemplong$predmethod=="lm" & resultstemplong$xtrend=="medium covariate shift",]

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


