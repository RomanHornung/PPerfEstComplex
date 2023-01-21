setwd("D:/Projects/DESTATIS/PredErrorComplex/PPerfEstComplex")


# Load and pre-process the results:
###################################

load("./simulations/hierpr/results/intermediate_results/scenariogrid_2.Rda")
load("./simulations/hierpr/results/intermediate_results/results_2.Rda")

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

# resultsabsdiffsum <- ddply(results[results$type %in% c("CV_absdiff", "stratCV_absdiff"),], .variables=c("n", "measure", "type"), 
#                    .fun=summarise, value=mean(value))

# resultspercdiffsum <- ddply(results[results$type %in% c("CV_percdiff", "stratCV_percdiff"),], .variables=c("n", "measure", "type"), 
#                         .fun=summarise, value=mean(value))

resultspercdiffvar <- ddply(results[results$type %in% c("CV_percdiff", "stratCV_percdiff"),], .variables=c("n", "measure", "type"), 
                           .fun=summarise, var=var(value))

# resultssum <- ddply(results[results$type %in% c("CV", "stratCV", "truth"),], .variables=c("n", "measure", "type"), 
#                         .fun=summarise, value=abs(mean(value)))
# resultssum1 <- resultssum[resultssum$type %in% c("CV", "stratCV"),]
# resultssum2 <- resultssum[resultssum$type %in% "truth",]
# 
# resultssum1[resultssum1$type=="CV",]
# resultssum2
# 
# all(resultssum1[resultssum1$type=="CV",]$n==resultssum2$n)
# all(resultssum1[resultssum1$type=="CV",]$measure==resultssum2$measure)
# 
# all(resultssum1[resultssum1$type=="stratCV",]$n==resultssum2$n)
# all(resultssum1[resultssum1$type=="stratCV",]$measure==resultssum2$measure)
# 
# resultssum1$percchange[resultssum1$type=="CV"] <- 100*(abs(resultssum1$value[resultssum1$type=="CV"] - resultssum2$value))/resultssum2$value
# resultssum1$percchange[resultssum1$type=="stratCV"] <- 100*(abs(resultssum1$value[resultssum1$type=="stratCV"] - resultssum2$value))/resultssum2$value





resultstemp <- results[results$type %in% c("CV", "stratCV", "truth"),]
resultstemp$type <-factor(resultstemp$type, levels=c("CV", "stratCV", "truth"))

labelstemp <- rep("", length(levels(resultstemp$n))*3)
labelstemp[seq(from=2, by=3, length=length(levels(resultstemp$n)))] <- levels(resultstemp$n)

library("ggplot2")

p <- ggplot(resultstemp, aes(x = interaction(type, n), y = value)) + theme_bw() +
  geom_line(aes(group = interaction(iter, n)),
            alpha = 0.5, colour = "darkgrey") +
  geom_boxplot(aes(fill = type), alpha = 0.5) +
  facet_wrap(~measure, scales="free_y") +
  scale_x_discrete(labels = labelstemp) +
  xlab("n") + ylab("Evaluation metric values") +
  theme(axis.ticks.x=element_blank(),
        axis.text=element_text(color="black"),
        legend.position = "none")

p

ggsave("./Simulations/HierPr/Results/figures/hierpr_raw_values_2.pdf", width=9, height=9)

p <- ggplot(resultstemp, aes(x = interaction(type, n), y = value)) + theme_bw() +
  geom_line(aes(group = interaction(iter, n)),
            alpha = 0.5, colour = "darkgrey") +
  geom_boxplot(aes(fill = type), alpha = 0.5) +
  facet_wrap(~measure, scales="free_y") +
  scale_x_discrete(labels = labelstemp) +
  xlab("n") + ylab("Evaluation metric values") +
  theme(axis.ticks.x=element_blank(),
        axis.text=element_text(color="black"))

p





resultstemp <- results[results$type %in% c("CV_percdiff", "stratCV_percdiff"),]
resultstemp$type <-factor(resultstemp$type, levels=c("CV_percdiff", "stratCV_percdiff"))

library("ggplot2")
p <- ggplot(data=resultstemp, aes(x=n, y=value, fill=type)) + theme_bw() +
  geom_boxplot() + geom_hline(yintercept=0, linetype="dashed", color="blue") + 
  facet_wrap(~measure, scales="free_y") + ylab("Difference between estimated and true evaluation metric values divided by true values") +
  theme(axis.title = element_text(color="black"), legend.position = "none")
p

ggsave("./Simulations/HierPr/Results/figures/hierpr_standardized_difference_2.pdf", width=9, height=9)
# stDiff

p <- ggplot(data=resultstemp, aes(x=n, y=value, fill=type)) + theme_bw() +
  geom_boxplot() + geom_hline(yintercept=0, linetype="dashed", color="blue") + 
  facet_wrap(~measure, scales="free_y") + ylab("Difference between estimated and true evaluation metric values divided by true values") +
  theme(axis.title = element_text(color="black"))
p
