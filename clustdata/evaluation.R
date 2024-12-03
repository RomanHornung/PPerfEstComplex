####################################################################################

# NOTE: Before the code can be executed, the R working directory *MUST* 
# be set to the directory of the 'PPerfEstComplex' folder (for Linux
# systems, R can also be run directly from the 'PPerfEstComplex'
# directory):

# Remove the '#' from the line below and replace 'this/is/my/path/' by the path
# to the directory that contains 'PPerfEstComplex':

# setwd("this/is/my/path/PPerfEstComplex")

####################################################################################

# Load and pre-process the results:
###################################

load("./clustdata/results/intermediate_results/scenariogrid.Rda")
load("./clustdata/results/intermediate_results/results.Rda")

names(results[[1]])

metrics <- data.frame(random_rf=sapply(results, function(x) x$mse_cv3_rf), 
                      grouped_rf=sapply(results, function(x) x$mse_cv3g_rf), 
                      truth_rf=sapply(results, function(x) x$mse_true_rf),
                      random_lm=sapply(results, function(x) x$mse_cv3_lm), 
                      grouped_lm=sapply(results, function(x) x$mse_cv3g_lm), 
                      truth_lm=sapply(results, function(x) x$mse_true_lm))

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
results$random_diff_rf <- metrics$random_rf - metrics$truth_rf
results$grouped_diff_rf <- metrics$grouped_rf - metrics$truth_rf
results$truth_rf <- metrics$truth_rf
results$random_lm <- metrics$random_lm
results$grouped_lm <- metrics$grouped_lm
results$random_diff_lm <- metrics$random_lm - metrics$truth_lm
results$grouped_diff_lm <- metrics$grouped_lm - metrics$truth_lm
results$truth_lm <- metrics$truth_lm

head(results)

results$sdeps <- results$sdeps^2
namesbefore <- c("N", "ni", "sdbinter", "sdbslope", "sdeps", "fixed", "repetition", "random_rf", "grouped_rf", "random_diff_rf", "grouped_diff_rf", "truth_rf", "random_lm", "grouped_lm", "random_diff_lm", "grouped_diff_lm", "truth_lm")
if(ncol(results) == length(namesbefore) & all(names(results)==namesbefore))
  names(results) <- c("N", "n_i", "var_intercept", "var_slope", "var_eps", "fixed", "repetition", "random_rf", "grouped_rf", "random_diff_rf", "grouped_diff_rf", "truth_rf", "random_lm", "grouped_lm", "random_diff_lm", "grouped_diff_lm", "truth_lm")

results$N <- factor(results$N)
results$n_i <- factor(results$n_i)
results$var_intercept <- factor(results$var_intercept)
results$var_slope <- factor(results$var_slope)
results$var_eps <- factor(results$var_eps)
results$fixed <- factor(results$fixed)

results <- reshape(
  results,
  idvar = "id",
  varying = list(c("random_rf", "grouped_rf", "random_diff_rf", "grouped_diff_rf", "truth_rf", "random_lm", "grouped_lm", "random_diff_lm", "grouped_diff_lm", "truth_lm")),
  v.names = "CV_err",
  timevar = "type_predmethod",
  times = c("random_rf", "grouped_rf", "random_diff_rf", "grouped_diff_rf", "truth_rf", "random_lm", "grouped_lm", "random_diff_lm", "grouped_diff_lm", "truth_lm"),
  direction = "long"
)

results$type <- substr(results$type_predmethod, 1, nchar(results$type_predmethod) - 3)
results$predmethod <- substr(results$type_predmethod, nchar(results$type_predmethod) - 1, nchar(results$type_predmethod))

results$type_predmethod <- NULL
results$id <- NULL

rownames(results) <- NULL

results$type <- factor(results$type, levels=c("random", "grouped", "random_diff", "grouped_diff", "truth"))
results$predmethod <- factor(results$predmethod, levels=c("rf", "lm"))






# Choose pleasing colors with "RColorBrewer":

library("RColorBrewer")

display.brewer.all(n=3, type="div")

colors <- brewer.pal(3, "RdBu")
selectedColors <- c(colors[1], colors[3])
print(selectedColors)









# Fig. 2: Simulation on clustered data: differences between estimated and true MSE values for each setting with cluster-specific means
# cluster-specific effects, and strong signal.

library("ggplot2")

res <- results[results$var_intercept==1 & results$var_slope==1 & results$var_eps==0.25 & results$type %in% c("random_diff", "grouped_diff"),]

res$N_n_i <- paste0("M = ", res$N, ", n_m = ", res$n_i)
res$N_n_i <- factor(res$N_n_i, levels=c("M = 10, n_m = 5", "M = 10, n_m = 25", "M = 50, n_m = 5", "M = 50, n_m = 25"))

res$predmethod <- factor(res$predmethod, levels = rev(levels(res$predmethod)))

levels(res$predmethod) <- c("'linear models'", "'random forests'")
levels(res$fixed) <- c("'without cluster-constant feature values'", "paste(x[1], ' values constant within clusters')",
                       "paste(x[2], ' values constant within clusters')")

head(res)

p <- ggplot(data=res, aes(x=N_n_i, y=CV_err, fill=type)) + theme_bw() +
  geom_boxplot() + 
  geom_hline(yintercept = 0, linetype="dashed", color="darkgray") + 
  facet_wrap(~ predmethod + fixed, labeller = label_parsed, ncol = 3) + #, scales="free_y") +
  ylab("Difference between estimated and true MSE values") +
  scale_fill_manual(values=selectedColors) +
  theme(axis.title.x=element_blank(), 
        axis.text.x = element_text(angle=45, hjust = 1, color="black", size=10), 
        strip.text.x = element_text(size = 11),
        legend.position = "none")
p

ggsave("./clustdata/results/figures/Figure2.pdf", width=10, height=7)











# Fig. S1: Simulation on clustered data: differences between estimated and true MSE values for the linear models for each setting without
# cluster-constant feature values.

res <- results[results$fixed=="none" & results$predmethod=="lm" & results$type %in% c("random_diff", "grouped_diff"),]
res$N_n_i <- paste0("M = ", res$N, ", n_m = ", res$n_i)
res$N_n_i <- factor(res$N_n_i, levels=c("M = 10, n_m = 5", "M = 10, n_m = 25", "M = 50, n_m = 5", "M = 50, n_m = 25"))

p <- ggplot(data=res, aes(x=N_n_i, y=CV_err, fill=type)) + theme_bw() +
  geom_boxplot() + geom_hline(yintercept = 0, linetype="dashed", color="darkgray") + 
  facet_wrap(~ var_intercept + var_slope + var_eps, labeller = label_both, ncol = 2) + #, scales="free_y") +
  ylab("Difference between estimated and true MSE values") +
  scale_fill_manual(values=selectedColors) +
  theme(axis.title.x=element_blank(), 
        axis.text.x = element_text(angle=45, hjust = 1, color="black", size=10), 
        legend.position = "none")
p

ggsave("./clustdata/results/figures/FigureS1.pdf", width=9, height=9)



# Fig. S2: Simulation on clustered data: differences between estimated and true MSE values for the linear models for each setting for which
# the x1 values were constant within clusters.

res <- results[results$fixed=="first" & results$predmethod=="lm" & results$type %in% c("random_diff", "grouped_diff"),]
res$N_n_i <- paste0("M = ", res$N, ", n_m = ", res$n_i)
res$N_n_i <- factor(res$N_n_i, levels=c("M = 10, n_m = 5", "M = 10, n_m = 25", "M = 50, n_m = 5", "M = 50, n_m = 25"))

p <- ggplot(data=res, aes(x=N_n_i, y=CV_err, fill=type)) + theme_bw() +
  geom_boxplot() + geom_hline(yintercept = 0, linetype="dashed", color="darkgray") + 
  facet_wrap(~ var_intercept + var_slope + var_eps, labeller = label_both, ncol = 2) + #, scales="free_y") +
  ylab("Difference between estimated and true MSE values") +
  scale_fill_manual(values=selectedColors) +
  theme(axis.title.x=element_blank(), 
        axis.text.x = element_text(angle=45, hjust = 1, color="black", size=10), 
        legend.position = "none")
p

ggsave("./clustdata/results/figures/FigureS2.pdf", width=9, height=9)



# Fig. S3: Simulation on clustered data: differences between estimated and true MSE values for the linear models for each setting for which
# the x2 values were constant within clusters.

res <- results[results$fixed=="second" & results$predmethod=="lm" & results$type %in% c("random_diff", "grouped_diff"),]
res$N_n_i <- paste0("M = ", res$N, ", n_m = ", res$n_i)
res$N_n_i <- factor(res$N_n_i, levels=c("M = 10, n_m = 5", "M = 10, n_m = 25", "M = 50, n_m = 5", "M = 50, n_m = 25"))

p <- ggplot(data=res, aes(x=N_n_i, y=CV_err, fill=type)) + theme_bw() +
  geom_boxplot() + geom_hline(yintercept = 0, linetype="dashed", color="darkgray") + 
  facet_wrap(~ var_intercept + var_slope + var_eps, labeller = label_both, ncol = 2) + #, scales="free_y") +
  ylab("Difference between estimated and true MSE values") +
  scale_fill_manual(values=selectedColors) +
  theme(axis.title.x=element_blank(), 
        axis.text.x = element_text(angle=45, hjust = 1, color="black", size=10), 
        legend.position = "none")
p

ggsave("./clustdata/results/figures/FigureS3.pdf", width=9, height=9)







# Fig. S4: Simulation on clustered data: differences between estimated and true MSE values for the random forests without cluster-constant
# feature values.

res <- results[results$fixed=="none" & results$predmethod=="rf" & results$type %in% c("random_diff", "grouped_diff"),]
res$N_n_i <- paste0("M = ", res$N, ", n_m = ", res$n_i)
res$N_n_i <- factor(res$N_n_i, levels=c("M = 10, n_m = 5", "M = 10, n_m = 25", "M = 50, n_m = 5", "M = 50, n_m = 25"))

p <- ggplot(data=res, aes(x=N_n_i, y=CV_err, fill=type)) + theme_bw() +
  geom_boxplot() + geom_hline(yintercept = 0, linetype="dashed", color="darkgray") + 
  facet_wrap(~ var_intercept + var_slope + var_eps, labeller = label_both, ncol = 2) + #, scales="free_y") +
  ylab("Difference between estimated and true MSE values") +
  scale_fill_manual(values=selectedColors) +
  theme(axis.title.x=element_blank(), 
        axis.text.x = element_text(angle=45, hjust = 1, color="black", size=10), 
        legend.position = "none")
p

ggsave("./clustdata/results/figures/FigureS4.pdf", width=9, height=9)




# Fig. S5: Simulation on clustered data: differences between estimated and true MSE values for the random forests for which the x1 values
# were constant within clusters.

res <- results[results$fixed=="first" & results$predmethod=="rf" & results$type %in% c("random_diff", "grouped_diff"),]
res$N_n_i <- paste0("M = ", res$N, ", n_m = ", res$n_i)
res$N_n_i <- factor(res$N_n_i, levels=c("M = 10, n_m = 5", "M = 10, n_m = 25", "M = 50, n_m = 5", "M = 50, n_m = 25"))

p <- ggplot(data=res, aes(x=N_n_i, y=CV_err, fill=type)) + theme_bw() +
  geom_boxplot() + geom_hline(yintercept = 0, linetype="dashed", color="darkgray") + 
  facet_wrap(~ var_intercept + var_slope + var_eps, labeller = label_both, ncol = 2) + #, scales="free_y") +
  ylab("Difference between estimated and true MSE values") +
  scale_fill_manual(values=selectedColors) +
  theme(axis.title.x=element_blank(), 
        axis.text.x = element_text(angle=45, hjust = 1, color="black", size=10), 
        legend.position = "none")
p

ggsave("./clustdata/results/figures/FigureS5.pdf", width=9, height=9)




# Fig. S6: Simulation on clustered data: differences between estimated and true MSE values for the random forests for which the x2 values
# were constant within clusters.

res <- results[results$fixed=="second" & results$predmethod=="rf" & results$type %in% c("random_diff", "grouped_diff"),]
res$N_n_i <- paste0("M = ", res$N, ", n_m = ", res$n_i)
res$N_n_i <- factor(res$N_n_i, levels=c("M = 10, n_m = 5", "M = 10, n_m = 25", "M = 50, n_m = 5", "M = 50, n_m = 25"))

p <- ggplot(data=res, aes(x=N_n_i, y=CV_err, fill=type)) + theme_bw() +
  geom_boxplot() + geom_hline(yintercept = 0, linetype="dashed", color="darkgray") + 
  facet_wrap(~ var_intercept + var_slope + var_eps, labeller = label_both, ncol = 2) + #, scales="free_y") +
  ylab("Difference between estimated and true MSE values") +
  scale_fill_manual(values=selectedColors) +
  theme(axis.title.x=element_blank(), 
        axis.text.x = element_text(angle=45, hjust = 1, color="black", size=10), 
        legend.position = "none")
p

ggsave("./clustdata/results/figures/FigureS6.pdf", width=9, height=9)
