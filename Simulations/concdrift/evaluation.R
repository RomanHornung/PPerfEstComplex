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

for(i in seq(along=ui))
  cat(ui[i], "\n")


ui <- paste0("res$", names(results[[1]]), " <- sapply(results, function(x) x$", names(results[[1]]), ")")

res$mse_cv_rf <- sapply(results, function(x) x$mse_cv_rf) 
res$mse_cv_lm <- sapply(results, function(x) x$mse_cv_lm) 
res$mse_TScv_1s_rf <- sapply(results, function(x) x$mse_TScv_1s_rf) 
res$mse_TScv_1s_lm <- sapply(results, function(x) x$mse_TScv_1s_lm) 
res$mse_TScv_2s_rf <- sapply(results, function(x) x$mse_TScv_2s_rf) 
res$mse_TScv_2s_lm <- sapply(results, function(x) x$mse_TScv_2s_lm) 
res$mse_TSholdout_1s_rf <- sapply(results, function(x) x$mse_TSholdout_1s_rf) 
res$mse_TSholdout_1s_lm <- sapply(results, function(x) x$mse_TSholdout_1s_lm) 
res$mse_TSholdout_2s_rf <- sapply(results, function(x) x$mse_TSholdout_2s_rf) 
res$mse_TSholdout_2s_lm <- sapply(results, function(x) x$mse_TSholdout_2s_lm) 
res$mse_true_endlast_rf <- sapply(results, function(x) x$mse_true_endlast_rf) 
res$mse_true_endlast_lm <- sapply(results, function(x) x$mse_true_endlast_lm) 
res$mse_true_mid1fu_rf <- sapply(results, function(x) x$mse_true_mid1fu_rf) 
res$mse_true_mid1fu_lm <- sapply(results, function(x) x$mse_true_mid1fu_lm) 
res$mse_true_end1fu_rf <- sapply(results, function(x) x$mse_true_end1fu_rf) 
res$mse_true_end1fu_lm <- sapply(results, function(x) x$mse_true_end1fu_lm) 
res$mse_true_mid2fu_rf <- sapply(results, function(x) x$mse_true_mid2fu_rf) 
res$mse_true_mid2fu_lm <- sapply(results, function(x) x$mse_true_mid2fu_lm) 
res$mse_true_end2fu_rf <- sapply(results, function(x) x$mse_true_end2fu_rf) 
res$mse_true_end2fu_lm <- sapply(results, function(x) x$mse_true_end2fu_lm)

resultssafe <- results

cat(paste(gsub("_rf", "", grep("_rf", names(results[[1]]), value=TRUE)), collapse = "\", \""), "\n")


cat(paste(apply(matrix(data=1:length(results[[1]]), ncol=2, byrow=TRUE), 1, function(x) paste0("c(\"", names(results[[1]])[x[1]], "\", \"", names(results[[1]])[x[2]], "\")")), collapse=", "), "\n")



rownames(res) <- NULL

results <- reshape(res, varying=list(c("mse_cv_rf", "mse_cv_lm"), c("mse_TScv_1s_rf", "mse_TScv_1s_lm"), c("mse_TScv_2s_rf", "mse_TScv_2s_lm"), c("mse_TSholdout_1s_rf", "mse_TSholdout_1s_lm"), c("mse_TSholdout_2s_rf", "mse_TSholdout_2s_lm"), c("mse_true_endlast_rf", "mse_true_endlast_lm"), c("mse_true_mid1fu_rf", "mse_true_mid1fu_lm"), c("mse_true_end1fu_rf", "mse_true_end1fu_lm"), c("mse_true_mid2fu_rf", "mse_true_mid2fu_lm"), c("mse_true_end2fu_rf", "mse_true_end2fu_lm")),
                   v.names=c("mse_cv", "mse_TScv_1s", "mse_TScv_2s", "mse_TSholdout_1s", "mse_TSholdout_2s", "mse_true_endlast", "mse_true_mid1fu", "mse_true_end1fu", "mse_true_mid2fu", "mse_true_end2fu"), 
                   timevar="predmethod", times=c("rf", "lm"),
                   direction="long")
results$predmethod <- factor(results$predmethod, levels=c("lm", "rf"))



# HIER GEHTS WEITER
# HIER GEHTS WEITER
# HIER GEHTS WEITER
# HIER GEHTS WEITER
# HIER GEHTS WEITER
# HIER GEHTS WEITER


library("plyr")

resultsum <- ddply(results, .variables=c("fixed", "N", "n_i", "var_intercept", "var_slope", "var_eps", "type"), 
                   .fun=summarise, CV_err = mean(CV_err))





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
