setwd("C:/Projects/DESTATIS/PredErrorComplex/PPerfEstComplex/PPerfEstComplex")


# Load and pre-process the results:
###################################

load("./hierpr/results/intermediate_results/scenariogrid_subset_test.Rda")
load("./hierpr/results/intermediate_results/results_subset_test.Rda")

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

namesbefore <- c("n", "iter", "measure", "CV_vals", "stratCV_vals", "truth_vals", "ntest")
if(ncol(results) == length(namesbefore) & all(names(results)==namesbefore))
  names(results) <- c("n", "iter", "measure", "CV", "stratCV", "truth", "ntest")

results$n <- factor(results$n)
results$measure <- factor(results$measure, levels=c("acc", "hierpr_micro", "hierpr_macro", 
                                                    "hierre_micro", "hierre_macro",
                                                    "hierf_micro", "hierf_macro",
                                                    "spath", "hloss"))

results$CV_diff <- results$CV - results$truth
results$stratCV_diff <- results$stratCV - results$truth

results$CV_absdiff <- abs(results$CV - results$truth)
results$stratCV_absdiff <- abs(results$stratCV - results$truth)

results$CV_percdiff <- 100*results$CV_diff/results$truth
results$stratCV_percdiff <- 100*results$stratCV_diff/results$truth

results$CV_percabsdiff <- 100*results$CV_absdiff/results$truth
results$stratCV_percabsdiff <- 100*results$stratCV_absdiff/results$truth



results <- reshape(results, varying=c("CV", "stratCV", "truth", "CV_diff", "stratCV_diff", "CV_absdiff", "stratCV_absdiff", "CV_percdiff",
                                      "stratCV_percdiff", "CV_percabsdiff", "stratCV_percabsdiff"),
                   v.names="value", 
                   timevar="type", times=c("CV", "stratCV", "truth", "CV_diff", "stratCV_diff", "CV_absdiff", "stratCV_absdiff", "CV_percdiff",
                                           "stratCV_percdiff", "CV_percabsdiff", "stratCV_percabsdiff"),
                   direction="long")


library("plyr")

resultspercdiffvar <- ddply(results[results$type %in% c("CV_percdiff", "stratCV_percdiff"),], .variables=c("n", "measure", "type"), 
                           .fun=summarise, var=var(value))


library("RColorBrewer")

display.brewer.all(n=3, type="div")

colors <- brewer.pal(3, "RdBu")
selectedColors <- c(colors[1], colors[3])



resultstemp <- results[results$type %in% c("CV", "stratCV", "truth"),]
resultstemp$type <-factor(resultstemp$type, levels=c("CV", "stratCV", "truth"))

labelstemp <- rep("", length(levels(resultstemp$n))*3)
labelstemp[seq(from=2, by=3, length=length(levels(resultstemp$n)))] <- levels(resultstemp$n)

library("ggplot2")

p <- ggplot(resultstemp, aes(x = interaction(type, n), y = value)) + theme_bw() +
  geom_line(aes(group = interaction(iter, n)),
            alpha = 0.5, colour = "darkgray") +
  geom_boxplot(aes(fill = type), alpha = 0.5) +
  facet_wrap(~measure, scales="free_y") +
  scale_x_discrete(labels = labelstemp) +
  xlab("n") + ylab("Evaluation metric values") +
  theme(axis.ticks.x=element_blank(),
        axis.text=element_text(color="black"),
        legend.position = "none")

p

ggsave("./hierpr/Results/figures/hierpr_raw_values_subset_test.pdf", width=9, height=9)







resultstemp <- results[results$type %in% c("CV_percdiff", "stratCV_percdiff"),]
resultstemp$type <-factor(resultstemp$type, levels=c("CV_percdiff", "stratCV_percdiff"))

library("ggplot2")
p <- ggplot(data=resultstemp, aes(x=n, y=value, fill=type)) + theme_bw() +
  geom_boxplot() + geom_hline(yintercept=0, linetype="dashed", color="darkgray") + 
  scale_fill_manual(values=selectedColors) +
  facet_wrap(~measure, scales="free_y") + ylab("Difference between estimated and true evaluation metric values divided by true values") +
  theme(axis.title = element_text(color="black"), legend.position = "none")
p

ggsave("./hierpr/Results/figures/hierpr_standardized_difference_subset_test.pdf", width=9, height=9)
