setwd("D:/Projects/DESTATIS/PredErrorComplex/PPerfEstComplex")


# Load and pre-process the results:
###################################

load("./simulations/hierpr/results/intermediate_results/scenariogrid.Rda")
load("./simulations/hierpr/results/intermediate_results/results.Rda")

reorderind <- order(scenariogrid$n, scenariogrid$iter)
scengrid <- scenariogrid[reorderind,]
results <- results[reorderind]

scengrid$seed_start <- scengrid$seed_res <- NULL

scengrid <- scengrid[rep(1:nrow(scengrid), times=sapply(results, nrow)),]

head(scengrid, 12)

head(scenariogrid)

resultstab <- do.call("rbind", results)

results <- scengrid

results <- cbind(scengrid, resultstab)

namesbefore <- c("n", "iter", "measure", "CV_vals", "stratCV_vals", "truth_vals")
if(ncol(results) == length(namesbefore) & all(names(results)==namesbefore))
  names(results) <- c("n", "iter", "measure", "CV", "stratCV", "truth")

results$n <- factor(results$n)
results$measure <- factor(results$measure, levels=c("hierf_micro", "hierf_macro", "hierpr_micro", 
                                                    "hierpr_macro", "hierre_micro", "hierre_macro", 
                                                    "hloss", "spath", "acc"))

results$CV_diff <- results$CV - results$truth
results$stratCV_diff <- results$stratCV - results$truth

results <- reshape(results, varying=c("CV", "stratCV", "truth", "CV_diff", "stratCV_diff"),
                   v.names="value", 
                   timevar="type", times=c("CV", "stratCV", "truth", "CV_diff", "stratCV_diff"),
                   direction="long")




library("plyr")

resultsdiffsum <- ddply(results[results$type %in% c("CV_diff", "stratCV_diff"),], .variables=c("n", "measure", "type"), 
                   .fun=summarise, value=mean(abs(value)))

resultsdiffsum




resultsdiffsum$type <-factor(resultsdiffsum$type, levels=c("CV_diff", "stratCV_diff"))

library("ggplot2")
p <- ggplot(data=resultsdiffsum, aes(x=n, y=value, fill=type)) + theme_bw() +
  geom_bar(stat="identity", width=.5, position = "dodge") + facet_wrap(~measure)
p


resultssum <- ddply(results[results$type %in% c("CV", "stratCV", "truth"),], .variables=c("n", "measure", "type"), 
                    .fun=summarise, value=mean(value))

library("ggplot2")
p <- ggplot(data=resultssum[resultssum$type=="truth",], aes(x=n, y=value)) + theme_bw() +
  geom_bar(stat="identity", width=.5) + facet_wrap(~measure, scales="free_y")
p




resultsdiffvarsum <- ddply(results[results$type %in% c("CV_diff", "stratCV_diff"),], .variables=c("n", "measure", "type"), 
                        .fun=summarise, var=var(value))

resultsdiffvarsum




resultsdiffvarsum$type <-factor(resultsdiffvarsum$type, levels=c("CV_diff", "stratCV_diff"))

library("ggplot2")
p <- ggplot(data=resultsdiffvarsum, aes(x=n, y=var, fill=type)) + theme_bw() +
  geom_bar(stat="identity", width=.5, position = "dodge") + facet_wrap(~measure, scales="free_y")
p


# res <- resultsum[resultsum$fixed!="none",]
# res$N_n_m <- paste0("N = ", res$N, ", n_m = ", res$n_i)
# res$N_n_m <- factor(res$N_n_m, levels=c("N = 10, n_m = 5", "N = 10, n_m = 25", "N = 50, n_m = 5", "N = 50, n_m = 25"))
# 
# library("ggplot2")
# p <- ggplot(data=res, aes(x=as.numeric(N_n_m), y=CV_err)) + theme_bw() +
#   geom_line(aes(color=type, linetype=fixed)) + facet_wrap(~ var_intercept + var_slope + var_eps, labeller = label_both, ncol = 2) +
#   ylab("Mean cross-validation error") +
#   theme(axis.title.x=element_blank(),
#         axis.text.x = element_text(angle=45, hjust = 1, color="black", size=10))
# p
# 
# ggsave("./Simulations/ClustData/Results/figures/bla.pdf", width=7, height=9)
# 
# res <- resultsum#[resultsum$fixed!="none",]
# res$N_n_m <- paste0("N = ", res$N, ", n_m = ", res$n_i)
# res$N_n_m <- factor(res$N_n_m, levels=c("N = 10, n_m = 5", "N = 10, n_m = 25", "N = 50, n_m = 5", "N = 50, n_m = 25"))
# 
# library("ggplot2")
# p <- ggplot(data=res, aes(x=as.numeric(N_n_m), y=CV_err)) + theme_bw() +
#   geom_line(aes(color=type, linetype=fixed)) + facet_wrap(~ var_intercept + var_slope + var_eps, labeller = label_both, ncol = 2) +
#   ylab("Mean cross-validation error") +
#   theme(axis.title.x=element_blank(),
#         axis.text.x = element_text(angle=45, hjust = 1, color="black", size=10))
# p
# 
# ggsave("./Simulations/ClustData/Results/figures/bla2.pdf", width=7, height=9)



# PLOT DESCRIPTION:

res <- results[results$fixed=="none",]
res$N_n_i <- paste0("N = ", res$N, ", n_m = ", res$n_i)
res$N_n_i <- factor(res$N_n_i, levels=c("N = 10, n_m = 5", "N = 10, n_m = 25", "N = 50, n_m = 5", "N = 50, n_m = 25"))

library("ggplot2")
p <- ggplot(data=res, aes(x=N_n_i, y=CV_err, fill=type)) + theme_bw() +
  geom_boxplot() + facet_wrap(~ var_intercept + var_slope + var_eps, labeller = label_both, ncol = 2, scales="free_y") +
  ylab("Cross-validated MSE") +
  theme(axis.title.x=element_blank(), 
        axis.text.x = element_text(angle=45, hjust = 1, color="black", size=10), 
        legend.position = "none")
p

ggsave("./Simulations/ClustData/Results/figures/cv_none_fixed.pdf", width=9, height=9)


res <- results[results$fixed=="first",]
res$N_n_i <- paste0("N = ", res$N, ", n_m = ", res$n_i)
res$N_n_i <- factor(res$N_n_i, levels=c("N = 10, n_m = 5", "N = 10, n_m = 25", "N = 50, n_m = 5", "N = 50, n_m = 25"))

library("ggplot2")
p <- ggplot(data=res, aes(x=N_n_i, y=CV_err, fill=type)) + theme_bw() +
  geom_boxplot() + facet_wrap(~ var_intercept + var_slope + var_eps, labeller = label_both, ncol = 2, scales="free_y") +
  ylab("Cross-validated MSE") +
  theme(axis.title.x=element_blank(), 
        axis.text.x = element_text(angle=45, hjust = 1, color="black", size=10), 
        legend.position = "none")
p

ggsave("./Simulations/ClustData/Results/figures/cv_first_fixed.pdf", width=9, height=9)




res <- results[results$fixed=="second",]
res$N_n_i <- paste0("N = ", res$N, ", n_m = ", res$n_i)
res$N_n_i <- factor(res$N_n_i, levels=c("N = 10, n_m = 5", "N = 10, n_m = 25", "N = 50, n_m = 5", "N = 50, n_m = 25"))

library("ggplot2")
p <- ggplot(data=res, aes(x=N_n_i, y=CV_err, fill=type)) + theme_bw() +
  geom_boxplot() + facet_wrap(~ var_intercept + var_slope + var_eps, labeller = label_both, ncol = 2, scales="free_y") +
  ylab("Cross-validated MSE") +
  theme(axis.title.x=element_blank(), 
        axis.text.x = element_text(angle=45, hjust = 1, color="black", size=10), 
        legend.position = "none")
p

ggsave("./Simulations/ClustData/Results/figures/cv_second_fixed.pdf", width=9, height=9)
