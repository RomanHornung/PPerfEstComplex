setwd("Z:/Projects/DESTATIS/PredErrorComplex/PPerfEstComplex")


# Load and pre-process the results:
###################################

load("./Simulations/ClustData/Results/intermediate_results/scenariogrid.Rda")
load("./Simulations/ClustData/Results/intermediate_results/results.Rda")

metrics <- data.frame(random=sapply(results, function(x) x$mse_cv3), 
                      grouped=sapply(results, function(x) x$mse_cv3g))

reorderind <- order(scenariogrid$fixed, scenariogrid$N, scenariogrid$ni,
                    scenariogrid$sdbinter, scenariogrid$sdbslope, scenariogrid$sdeps,
                    scenariogrid$iter)
scengrid <- scenariogrid[reorderind,]
metrics <- metrics[reorderind,]
rownames(scengrid) <- rownames(metrics) <- NULL


results <- scengrid
results$seed <- NULL
results$random <- metrics$random
results$grouped <- metrics$grouped

head(results)

results$sdeps <- results$sdeps^2
namesbefore <- c("N", "ni", "sdbinter", "sdbslope", "sdeps", "fixed", "iter", "random", "grouped")
if(ncol(results) == length(namesbefore) & all(names(results)==c("N", "ni", "sdbinter", "sdbslope", "sdeps", "fixed", "iter", "random", "grouped")))
  names(results) <- c("N", "n_i", "var_intercept", "var_slope", "var_eps", "fixed", "iter", "random", "grouped")

results$N <- factor(results$N)
results$n_i <- factor(results$n_i)
results$var_intercept <- factor(results$var_intercept)
results$var_slope <- factor(results$var_slope)
results$var_eps <- factor(results$var_eps)
results$fixed <- factor(results$fixed)

results <- reshape(results, varying=c("random", "grouped"),
                   v.names="CV_err", 
                   timevar="type", times=c("random", "grouped"),
                   direction="long")
results$type <- factor(results$type, levels=c("random", "grouped"))

library("plyr")

resultsum <- ddply(results, .variables=c("fixed", "N", "n_i", "var_intercept", "var_slope", "var_eps", "type"), 
                   .fun=summarise, CV_err = mean(CV_err))





# res <- resultsum[resultsum$fixed=="none",]
# res$N_n_i <- paste0("N = ", res$N, ", n_i = ", res$n_i)
# res$N_n_i <- factor(res$N_n_i, levels=c("N = 10, n_i = 5", "N = 10, n_i = 25", "N = 50, n_i = 5", "N = 50, n_i = 25"))
# 
# library("ggplot2")
# p <- ggplot(data=res, aes(x=N_n_i, y=CV_err, color=type, group=type)) + theme_bw() +
#   geom_line() + facet_wrap(~ var_intercept + var_slope + var_eps, labeller = label_both, ncol = 2) +
#   ylab("Mean cross-validation error") +
#   theme(axis.title.x=element_blank(), 
#         axis.text.x = element_text(angle=45, hjust = 1, color="black", size=10), 
#         legend.position = "none")
# p
# 
# ggsave("./Simulations/ClustData/Results/figures/MeanCV_none_fixed.pdf", width=7, height=9)


# PLOT DESCRIPTION:

res <- results[results$fixed=="none",]
res$N_n_i <- paste0("N = ", res$N, ", n_i = ", res$n_i)
res$N_n_i <- factor(res$N_n_i, levels=c("N = 10, n_i = 5", "N = 10, n_i = 25", "N = 50, n_i = 5", "N = 50, n_i = 25"))

library("ggplot2")
p <- ggplot(data=res, aes(x=N_n_i, y=CV_err, fill=type)) + theme_bw() +
  geom_boxplot() + facet_wrap(~ var_intercept + var_slope + var_eps, labeller = label_both, ncol = 2, scales="free_y") +
  ylab("Cross-validation error") +
  theme(axis.title.x=element_blank(), 
        axis.text.x = element_text(angle=45, hjust = 1, color="black", size=10), 
        legend.position = "none")
p

ggsave("./Simulations/ClustData/Results/figures/cv_none_fixed.pdf", width=9, height=9)


res <- results[results$fixed=="first",]
res$N_n_i <- paste0("N = ", res$N, ", n_i = ", res$n_i)
res$N_n_i <- factor(res$N_n_i, levels=c("N = 10, n_i = 5", "N = 10, n_i = 25", "N = 50, n_i = 5", "N = 50, n_i = 25"))

library("ggplot2")
p <- ggplot(data=res, aes(x=N_n_i, y=CV_err, fill=type)) + theme_bw() +
  geom_boxplot() + facet_wrap(~ var_intercept + var_slope + var_eps, labeller = label_both, ncol = 2, scales="free_y") +
  ylab("Cross-validation error") +
  theme(axis.title.x=element_blank(), 
        axis.text.x = element_text(angle=45, hjust = 1, color="black", size=10), 
        legend.position = "none")
p

ggsave("./Simulations/ClustData/Results/figures/cv_first_fixed.pdf", width=9, height=9)




res <- results[results$fixed=="second",]
res$N_n_i <- paste0("N = ", res$N, ", n_i = ", res$n_i)
res$N_n_i <- factor(res$N_n_i, levels=c("N = 10, n_i = 5", "N = 10, n_i = 25", "N = 50, n_i = 5", "N = 50, n_i = 25"))

library("ggplot2")
p <- ggplot(data=res, aes(x=N_n_i, y=CV_err, fill=type)) + theme_bw() +
  geom_boxplot() + facet_wrap(~ var_intercept + var_slope + var_eps, labeller = label_both, ncol = 2, scales="free_y") +
  ylab("Cross-validation error") +
  theme(axis.title.x=element_blank(), 
        axis.text.x = element_text(angle=45, hjust = 1, color="black", size=10), 
        legend.position = "none")
p

ggsave("./Simulations/ClustData/Results/figures/cv_second_fixed.pdf", width=9, height=9)
