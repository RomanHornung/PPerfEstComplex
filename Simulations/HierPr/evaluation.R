setwd("Z:/Projects/DESTATIS/PredErrorComplex/PPerfEstComplex")


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

p <- ggplot(resultstemp, aes(x = interaction(type, n), y = value)) + theme_bw() +
  geom_line(aes(group = interaction(iter, n)),
            alpha = 0.5, colour = "darkgrey") +
  geom_boxplot(aes(fill = type), alpha = 0.5) +
  facet_wrap(~measure, scales="free_y") +
  scale_x_discrete(labels = labelstemp) +
  xlab("n") + ylab("Error") +
  theme(axis.ticks.x=element_blank(),
        axis.text=element_text(color="black"),
        legend.position = "none")

p

ggsave("./Simulations/HierPr/Results/figures/Raw_values.pdf", width=9, height=9)





resultstemp <- results[results$type %in% c("CV_percdiff", "stratCV_percdiff"),]
resultstemp$type <-factor(resultstemp$type, levels=c("CV_percdiff", "stratCV_percdiff"))

library("ggplot2")
p <- ggplot(data=resultstemp, aes(x=n, y=value, fill=type)) + theme_bw() +
  geom_boxplot() + geom_hline(yintercept=0, linetype="dashed", color="blue") + 
  facet_wrap(~measure, scales="free_y") + ylab("Standardized difference") +
  theme(axis.title = element_text(color="black"), legend.position = "none")
p

ggsave("./Simulations/HierPr/Results/figures/standardized_difference.pdf", width=9, height=9)
# stDiff



# resultstemp <- results[results$type %in% c("CV_percabsdiff", "stratCV_percabsdiff"),]
# resultstemp$type <-factor(resultstemp$type, levels=c("CV_percabsdiff", "stratCV_percabsdiff"))
# 
# library("ggplot2")
# p <- ggplot(data=resultstemp, aes(x=n, y=value, fill=type)) + theme_bw() +
#   geom_boxplot() + facet_wrap(~measure, scales="free_y") + ylab("Standardized deviation") +
#   theme(axis.title = element_text(color="black"), legend.position = "none")
# p
# 
# ggsave("./Simulations/HierPr/Results/figures/standardized_deviation.pdf", width=9, height=9)
# stDev


# resultstemp <- resultspercdiffvar#[resultspercdiffvar$n!=200,]
# resultstemp$type <-factor(resultstemp$type, levels=c("CV_percdiff", "stratCV_percdiff"))
# 
# library("ggplot2")
# p <- ggplot(data=resultstemp, aes(x=n, y=var, fill=type)) + theme_bw() +
#   geom_bar(stat="identity", width=.5, position = "dodge") + facet_wrap(~measure, scales="free_y")
# p











## Load package:

library("hierclass")


## Load the example data set 'datasim':

data(datasim)


## Set seed to make results reproducible:

set.seed(1234)


## Split data set into training and test data:

trainind <- sample(1:nrow(datasim), size=round((3/4)*nrow(datasim)))
datatrain <- datasim[trainind,]
datatest <- datasim[-trainind,]


## Construct a top-down hierarchical prediction rule using the training data:

object <- topdown(ydepvar ~ ., data=datatrain, num.trees=50)
# NOTE: In practice 'num.trees' should in general be larger
# to achieve better performance (default is 500).
# We use 50 trees here only for computational efficiency of
# the example.


## Predict the classes of observations in the test data (without
## early stopping because 'confid=1' by default):

preds <- predict(object, data=datatest)


## Compare the first predictions with the true labels:

ui <- data.frame(preds=preds, truth=datatest$ydepvar)


fix(ui)


hierpr <- function(truth, response, type="micro") {
  
  truth <- ui$truth; response <- ui$preds
  
  if (!(type %in% c("micro", "macro")))
    stop("'type' has to be either 'micro' or 'macro'.")
  
  # Convert the factors to characters, because we perform string
  # operations on them:
  truth <- as.character(truth)
  response <- as.character(response)
  
  # Replace the NA predictions to predictions of the root class:
  response[is.na(response)] <- "rootclass"
  
  truthall <- sapply(truth, strsplit, split="\\.")
  responseall <- sapply(response, strsplit, split="\\.")
  
  if (type=="micro") {
    
    # Micro average:
    
    nominator <- sum(mapply(function(x, y) length(intersect(x, y)), truthall, responseall))
    denominator <- length(unlist(responseall))
    
    result <- nominator/denominator
    
  } else {
    
    # Macro average:
    
    responseleaf <- sapply(responseall, function(x) x[length(x)])
    responseleafun <- unique(responseleaf)
    
    prl <- sapply(responseleafun, function(l) {
      nominator <- sum(mapply(function(x, y) length(intersect(x, y)), truthall[responseleaf==l], responseall[responseleaf==l]))
      denominator <- length(unlist(responseall[responseleaf==l]))
      nominator/denominator
    })
    
    result <- mean(prl)
    
  }
  
  return(result)
  
}





hierre <- function(truth, response, type="micro") {
  
  if (!(type %in% c("micro", "macro")))
    stop("'type' has to be either 'micro' or 'macro'.")
  
  # Convert the factors to characters, because we perform string
  # operations on them:
  truth <- as.character(truth)
  response <- as.character(response)
  
  # Replace the NA predictions to predictions of the root class:
  response[is.na(response)] <- "rootclass"
  
  truthall <- sapply(truth, strsplit, split="\\.")
  responseall <- sapply(response, strsplit, split="\\.")
  
  if (type=="micro") {
    
    # Micro average:
    
    nominator <- sum(mapply(function(x, y) length(intersect(x, y)), truthall, responseall))
    denominator <- length(unlist(truthall))
    
    result <- nominator/denominator
    
  } else {
    
    # Macro average:
    
    truthleaf <- sapply(truthall, function(x) x[length(x)])
    truthleafun <- unique(truthleaf)
    
    rel <- sapply(truthleafun, function(l) {
      nominator <- sum(mapply(function(x, y) length(intersect(x, y)), truthall[truthleaf==l], responseall[truthleaf==l]))
      denominator <- length(unlist(truthall[truthleaf==l]))
      nominator/denominator
    })
    
    result <- mean(rel)
    
  }
  
  return(result)
  
}