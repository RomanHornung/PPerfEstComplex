# DONE
############

setwd("Z:/Projects/DESTATIS/PredErrorComplex/PPerfEstComplex")


# Load and pre-process the results:
###################################

load("./hierpr/results/intermediate_results/scenariogrid.Rda")
load("./hierpr/results/intermediate_results/results.Rda")

reorderind <- order(scenariogrid$n, scenariogrid$iter)
scengrid <- scenariogrid[reorderind,]
results <- results[reorderind]

scengrid$seed_start <- scengrid$seed_res <- NULL

scengrid <- scengrid[rep(1:nrow(scengrid), times=sapply(results, nrow)),]


resultstab <- do.call("rbind", results)

results <- scengrid

results <- cbind(scengrid, resultstab)

namesbefore <- c("n", "iter", "measure", "CV_vals", "stratCV_vals", "truth_vals")
if(ncol(results) == length(namesbefore) & all(names(results)==namesbefore))
  names(results) <- c("n", "iter", "measure", "CV", "stratCV", "truth")

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








# Fig. S18: Simulation on hierarchical classification: estimated and (approximated) true evaluation metric values.

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

ggsave("./hierpr/Results/figures/FigureS18.pdf", width=9, height=9)








# Fig. S19: Simulation on hierarchical classification: differences between 
# estimated and true evaluation metric values divided by true values.

library("RColorBrewer")

display.brewer.all(n=3, type="div")

colors <- brewer.pal(3, "RdBu")
selectedColors <- c(colors[1], colors[3])

resultstemp <- results[results$type %in% c("CV_percdiff", "stratCV_percdiff"),]
resultstemp$type <-factor(resultstemp$type, levels=c("CV_percdiff", "stratCV_percdiff"))

library("ggplot2")
p <- ggplot(data=resultstemp, aes(x=n, y=value, fill=type)) + theme_bw() +
  geom_boxplot() + geom_hline(yintercept=0, linetype="dashed", color="darkgray") + 
  scale_fill_manual(values=selectedColors) +
  facet_wrap(~measure, scales="free_y") + ylab("Difference between estimated and true evaluation metric values divided by true values") +
  theme(axis.title = element_text(color="black"), legend.position = "none")
p

ggsave("./hierpr/Results/figures/FigureS19.pdf", width=9, height=9)







# Fig. 9: Simulation on hierarchical classification: differences between estimated 
# and true evaluation metric values divided by true values.

resultstemp <- results[results$type %in% c("CV_percdiff", "stratCV_percdiff"),]
resultstemp$type <-factor(resultstemp$type, levels=c("CV_percdiff", "stratCV_percdiff"))

if(all(levels(resultstemp$measure) == c("acc", "hierpr_micro", "hierpr_macro", "hierre_micro", "hierre_macro", "hierf_micro", "hierf_macro", "spath", "hloss" )))
  levels(resultstemp$measure) <- c("'accuracy'", "'micro-averaged hierarchical precision'", "'macro-averaged hierarchical precision'", 
                                   "'micro-averaged hierarchical recall'", "'macro-averaged hierarchical recall'", 
                                   "paste('micro-averaged hierarchical ', F[1], ' score')", "paste('macro-averaged hierarchical ', F[1], ' score')", 
                                   "'weighted shortest path loss measure'", "'H-loss'" )

library("ggplot2")
p <- ggplot(data=resultstemp, aes(x=n, y=value, fill=type)) + theme_bw() +
  geom_boxplot() + geom_hline(yintercept=0, linetype="dashed", color="darkgray") + 
  facet_wrap(~measure, labeller = label_parsed, scales="free_y") + ylab("Difference between estimated and true evaluation metric values divided by true values") +
  scale_fill_manual(values=selectedColors) +
  xlab(expression(paste("n"[train]))) +
  theme(axis.title = element_text(color="black", size=12), 
        strip.text.x = element_text(size=11),
        legend.position = "none")
p

ggsave("./hierpr/Results/figures/Figure9.pdf", width=10, height=8)







# Below we provide code that supports the statements in Section C.2 of 
# Supplementary Material 1. The code is largely uncommented. However, we 
# make it clear where in the code the statements are supported by quoting 
# the appropriate texts from Section C.2 that make the statements immediately 
# before the lines of code that support the statements (these comments are 
# preceded by "Statement:"). Note that sometimes the same lines of code
# support two statements. Note also that the statement "However, even when 
# including in the test data only observations from classes that were 
# contained in the training data, SCV still overestimated the true performance, 
# albeit less strongly and not for all evaluation metrics (results not shown)."
# is an exception. This statement is supported by the scripts "evaluation_subset_test",
# "simulation_subset_test.R", and "functions_simulation_subset_test.R".
#############################################################################


source("./hierpr/functions_simulation.R")


load("./hierpr/results/intermediate_results/treestruc.Rda")

set.seed(1234)

coeflist <- simulate_coefs(treestruc=treestruc, sdbeta0=sqrt(1),
                           sdbeta=sqrt(c(2.5, 2, 0.9, 0.7, 0.5)))

datatrain <- sim_data(n=200, coeflist=coeflist)

datatesthuge <- sim_data(n=200000, coeflist=coeflist)




library("hierclass")
library("mlr3")

task = as_task_classif(y ~ ., data = datatrain)

learner = lrn("classif.topdown")

cv3 <- rsmp("repeated_cv", repeats = 1, folds = 5)

cv3$instantiate(task)

foldinfoCV <- as.data.frame(cv3$instance)
foldinfoCV <- foldinfoCV[order(foldinfoCV$row_id),]

ynew <- datatrain$y
ynew <- factor(as.character(ynew))


foldCV <- sapply(1:5, function(x) table(ynew[foldinfoCV$fold==x]))
# Statement: [regarding CV and ntrain = 200] "... small classes frequently did not occur in the test folds in CV..."
foldCV


task$col_roles$stratum = task$target_names

cv3$instantiate(task)

foldinfostratCV <- as.data.frame(cv3$instance)
foldinfostratCV <- foldinfostratCV[order(foldinfostratCV$row_id),]

foldstratCV <- sapply(1:5, function(x) table(ynew[foldinfostratCV$fold==x]))

# Statement: "... for $\ntrain = 200$, small classes often only featured very few observations ($< 5$), ..."
table(datatrain$y)[sapply(names(table(datatrain$y)), function(x) length(strsplit(x, split="\\.")[[1]]))==5]
# Statement: [regarding stratified CV] "..., which had the effect that only a subset of the folds featured these classes."
foldstratCV

trainclasses <- as.character(unique(datatrain$y))
freqclasses <- table(as.character(datatesthuge$y))/nrow(datatesthuge)
leafnodeclasses <- names(freqclasses)

boxplot(freqclasses ~ as.numeric(leafnodeclasses %in% trainclasses))
# Statement: "..., for $\ntrain = 200$, many small classes did not even occur in the training sets ..."
# Statement: "... for $\ntrain = 200$ many classes were contained only in the test data sets used for 
# approximating the true values of the evaluation metrics."
sum(!(leafnodeclasses %in% trainclasses))





rclassvec <- droplevels(datatrain$y)

# Statement [regarding CV]: "$\ntrain = 200$, the differences in class distributions between training and test folds were frequently quite strong."

rbind(table(trclassvec[foldinfoCV$fold!=1])/sum(foldinfoCV$fold!=1),
table(trclassvec[foldinfoCV$fold==1])/sum(foldinfoCV$fold==1))

rbind(table(trclassvec[foldinfoCV$fold!=2])/sum(foldinfoCV$fold!=2),
      table(trclassvec[foldinfoCV$fold==2])/sum(foldinfoCV$fold==2))






object <- topdown(y ~ ., data=datatrain)
preds <- predict(object, data=datatesthuge)

tempdf <- data.frame(class=names(table(datatrain$y)), ntrain=as.vector(table(datatrain$y)), npred=as.vector(table(preds)))
# Statement: "... many of the smallest classes were not predicted at all, even on the huge test sets ($\ntest = 200,000$) ..."
tempdf





# Modified version of function "hierre" which is modified to return the class-specific 
# hierarchical recall values:
myhierre <- function(truth, response, type="micro") {
  
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
    
    result <- data.frame(class=truthleafun, Rel=rel)
    # result <- mean(rel)
    
  }
  
  return(result)
  
}


# Modified version of function "hierre" which is modified to return the class-specific 
# hierarchical precision values:
myhierpr <- function(truth, response, type="micro") {
  
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
    
    result <- data.frame(class=responseleafun, Prl=prl)
    # result <- mean(prl)
    
  }
  
  return(result)
  
}

rel_res <- myhierre(truth=datatesthuge$y, response=preds, type="macro")
prl_res <- myhierpr(truth=datatesthuge$y, response=preds, type="macro")

tempdf <- data.frame(class=sapply(sapply(names(table(datatrain$y)), strsplit, split="\\."), function(x) x[length(x)]), ntrain=as.vector(table(datatrain$y)))

rel_res2 <- merge(rel_res, tempdf)
prl_res2 <- merge(prl_res, tempdf)


# Statement: "Lastly, the class-specific hierarchical precision values tended to 
# be less worse for small classes compared to in the cases of the class-specific 
# hierarchical recall values. This is because, in cases in which prediction rules 
# predicted small classes, the chance that the predicted class was actually the 
# true class was relatively high."

# Statement: "... smaller classes tended to receive much worse class-specific hierarchical recall values ..."

par(mfrow=c(1,2))
plot(rel_res2$ntrain, rel_res2$Rel, 
     main=paste0("Cor: ", round(cor(rel_res2$ntrain, rel_res2$Rel, method="spearman"), 2)), ylim=range(c(rel_res2$Rel, prl_res2$Prl)))

plot(prl_res2$ntrain, prl_res2$Prl,
     main=paste0("Cor: ", round(cor(prl_res2$ntrain, prl_res2$Prl, method="spearman"), 2)), ylim=range(c(rel_res2$Rel, prl_res2$Prl)))
cor(prl_res2$ntrain, prl_res2$Prl)
par(mfrow=c(1,1))





# Statement: "... many small classes did not even occur in the training sets ... "
sum(!(names(table(as.character(datatrain$y)[foldinfoCV$fold==3])) %in% names(table(as.character(datatrain$y)[foldinfoCV$fold!=3]))))

objecttemp <- topdown(y ~ ., data=datatrain[foldinfoCV$fold!=3,])
predstemp <- predict(objecttemp, data=datatrain[foldinfoCV$fold==3,])

rel_restemp <- myhierre(truth=datatrain[foldinfoCV$fold==3,]$y, response=predstemp, type="macro")

classnotincluded <- sapply(sapply(names(table(as.character(datatrain$y)[foldinfoCV$fold==3]))[!(names(table(as.character(datatrain$y)[foldinfoCV$fold==3])) %in% names(table(as.character(datatrain$y)[foldinfoCV$fold!=3])))], strsplit, split="\\."), function(x) x[length(x)])

rel_restemp$intrain <- FALSE
rel_restemp$intrain[!(rel_restemp$class %in% classnotincluded)] <- TRUE

# Statement: "... and these classes naturally tend to receive very bad class-specific hierarchical recall values 
# in the approximation of the true \rcode{hierre\_macro} values."
boxplot(rel_restemp$Rel ~ rel_restemp$intrain)





set.seed(1234)
library("ranger")
objectrf <- ranger(dependent.variable.name = "y", data=datatrain)
predsrf <- predict(objectrf, data=datatesthuge)$predictions


tempdf1 <- data.frame(class=names(table(datatrain$y)), ntrain=as.vector(table(datatrain$y)))
tempdf2 <- data.frame(class=names(table(predsrf)), npred=as.vector(table(predsrf)))

tempdf12 <- merge(tempdf1, tempdf2)
tempdf12 <- tempdf12[tempdf12$ntrain!=0,]
tempdf12$predperobs <- tempdf12$npred/tempdf12$ntrain 

# Statement: "... random forests in particular, have a strong tendency towards predicting larger classes ..."
plot(tempdf12$ntrain, tempdf12$predperobs, main=paste0("Cor: ", round(cor(tempdf12$ntrain, tempdf12$predperobs, method="spearman"), 2)))
