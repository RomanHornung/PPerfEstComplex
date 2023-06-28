setwd("C:/Projects/DESTATIS/PredErrorComplex/PPerfEstComplex")

# Load and pre-process the results:
###################################

load("./Simulations/ClustData/Results/intermediate_results/scenariogrid.Rda")
load("./Simulations/ClustData/Results/intermediate_results/results.Rda")

metrics <- data.frame(random_rf=sapply(results, function(x) x$mse_cv3_rf), 
                      grouped_rf=sapply(results, function(x) x$mse_cv3g_rf),
                      random_lm=sapply(results, function(x) x$mse_cv3_lm), 
                      grouped_lm=sapply(results, function(x) x$mse_cv3g_lm))

reorderind <- order(scenariogrid$fixed, scenariogrid$N, scenariogrid$ni,
                    scenariogrid$sdbinter, scenariogrid$sdbslope, scenariogrid$sdeps,
                    scenariogrid$repetition)
scengrid <- scenariogrid[reorderind,]
metrics <- metrics[reorderind,]
rownames(scengrid) <- rownames(metrics) <- NULL


results <- scengrid
results$seed <- NULL
results$random_rf <- metrics$random_rf
results$grouped_rf <- metrics$grouped_rf
results$random_lm <- metrics$random_lm
results$grouped_lm <- metrics$grouped_lm

head(results)

results$sdeps <- results$sdeps^2
namesbefore <- c("N", "ni", "sdbinter", "sdbslope", "sdeps", "fixed", "repetition", "random_rf", "grouped_rf", "random_lm", "grouped_lm")
if(ncol(results) == length(namesbefore) & all(names(results)==namesbefore))
  names(results) <- c("N", "n_i", "var_intercept", "var_slope", "var_eps", "fixed", "repetition", "random_rf", "grouped_rf", "random_lm", "grouped_lm")

results$N <- factor(results$N)
results$n_i <- factor(results$n_i)
results$var_intercept <- factor(results$var_intercept)
results$var_slope <- factor(results$var_slope)
results$var_eps <- factor(results$var_eps)
results$fixed <- factor(results$fixed)

resultssafe <- results

results <- reshape(results, varying=list(c("random_rf", "grouped_rf"), c("random_lm", "grouped_lm")),
                   v.names="CV_err", 
                   timevar="type", times=c("random", "grouped"),
                   direction="long")
results$type <- factor(results$type, levels=c("random", "grouped"))


results <- resultssafe

# reshape data into long format
results <- reshape(
  results,
  idvar = "id",
  varying = list(c("random_rf", "grouped_rf", "random_lm", "grouped_lm")),
  v.names = "CV_err",
  timevar = "type_predmethod",
  times = c("random_rf", "grouped_rf", "random_lm", "grouped_lm"),
  direction = "long"
)

head(results)

# split "type_predmethod" variable into "type" and "predmethod" variables
results$type <- substr(results$type_predmethod, 1, nchar(results$type_predmethod) - 3)
results$predmethod <- substr(results$type_predmethod, nchar(results$type_predmethod) - 1, nchar(results$type_predmethod))

# remove the original "type_predmethod" variable
results$type_predmethod <- NULL
results$id <- NULL

rownames(results) <- NULL

# show the resulting long-format dataset

results$type <- factor(results$type, levels=c("random", "grouped"))
results$predmethod <- factor(results$predmethod, levels=c("rf", "lm"))



library("plyr")

resultsum <- ddply(results, .variables=c("fixed", "N", "n_i", "var_intercept", "var_slope", "var_eps", "type", "predmethod"), 
                   .fun=summarise, CV_err = mean(CV_err))


head(resultsum)





res <- results[results$var_intercept==1 & results$var_slope==1 & results$var_eps==0.25,]

res$N_n_i <- paste0("N = ", res$N, ", n_m = ", res$n_i)
res$N_n_i <- factor(res$N_n_i, levels=c("N = 10, n_m = 5", "N = 10, n_m = 25", "N = 50, n_m = 5", "N = 50, n_m = 25"))

res$predmethod <- factor(res$predmethod, levels = rev(levels(res$predmethod)))

levels(res$predmethod) <- c("'linear models'", "'random forests'")
levels(res$fixed) <- c("'without cluster-constant feature values'", "paste(x[1], ' values constant within clusters')",
                       "paste(x[2], ' values constant within clusters')")

p <- ggplot(data=res, aes(x=N_n_i, y=CV_err, fill=type)) + theme_bw() +
  geom_boxplot() + 
  facet_wrap(~ predmethod + fixed, labeller = label_parsed, ncol = 3, scales="free_y") +
  ylab("Cross-validated MSE") +
  theme(axis.title.x=element_blank(), 
        axis.text.x = element_text(angle=45, hjust = 1, color="black", size=10), 
        strip.text.x = element_text(size = 11),
        legend.position = "none")
p

ggsave("./Simulations/ClustData/Results/figures/clustersim.pdf", width=10, height=7)






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

res <- results[results$fixed=="none" & results$predmethod=="rf",]
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

ggsave("./Simulations/ClustData/Results/figures/cv_none_fixed_rf.pdf", width=9, height=9)


res <- results[results$fixed=="first" & results$predmethod=="rf",]
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

ggsave("./Simulations/ClustData/Results/figures/cv_first_fixed_rf.pdf", width=9, height=9)




res <- results[results$fixed=="second" & results$predmethod=="rf",]
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

ggsave("./Simulations/ClustData/Results/figures/cv_second_fixed_rf.pdf", width=9, height=9)









# PLOT DESCRIPTION:

res <- results[results$fixed=="none" & results$predmethod=="lm",]
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

ggsave("./Simulations/ClustData/Results/figures/cv_none_fixed_lm.pdf", width=9, height=9)


res <- results[results$fixed=="first" & results$predmethod=="lm",]
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

ggsave("./Simulations/ClustData/Results/figures/cv_first_fixed_lm.pdf", width=9, height=9)




res <- results[results$fixed=="second" & results$predmethod=="lm",]
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

ggsave("./Simulations/ClustData/Results/figures/cv_second_fixed_lm.pdf", width=9, height=9)
