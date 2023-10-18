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

head(scengrid, 12)

head(scenariogrid)

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

ggsave("./hierpr/Results/figures/FigureS18.pdf", width=9, height=9)







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






source("./hierpr/functions_simulation.R")


load("./hierpr/results/intermediate_results/treestruc.Rda")

# Simulate 10 datasets of size n=1000 and evaluate their performance
# using eval_perm():

# set.seed(1234)
  
coeflist <- simulate_coefs(treestruc=treestruc, sdbeta0=sqrt(1),
                             sdbeta=sqrt(c(2.5, 2, 0.9, 0.7, 0.5)))
  

allclasses <- sort(unique(as.character(dataset$y)))


# dataset <- sim_data(n=200, coeflist=coeflist)
# dataset$y <- factor(as.character(dataset$y), levels=allclasses)

# levels(dataset$y)
# table(sapply(levels(dataset$y), function(x) length(strsplit(x, split="\\.")[[1]])))

# pdf("./hierpr/results/intermediate_results/data200.pdf", width=30)
par(mfrow=c(2,1))
par(mar=c(8.1, 4.1, 4.1, 2.1))
dataset <- sim_data(n=200, coeflist=coeflist)
dataset$y <- factor(as.character(dataset$y), levels=allclasses)
barplot(table(dataset$y),las=2)
dataset <- sim_data(n=20000, coeflist=coeflist)
dataset$y <- factor(as.character(dataset$y), levels=allclasses)
barplot(table(dataset$y),las=2)
par(mar=c(5.1, 4.1, 4.1, 2.1))
par(mfrow=c(1,1))
# dev.off()


dataset <- sim_data(n=10000, coeflist=coeflist)
unique(as.character(dataset$y))

allclasses <- sort(unique(as.character(dataset$y)))


mean(table(dataset$y)==0)









coeflist <- simulate_coefs(treestruc=treestruc, sdbeta0=sqrt(1),
                           sdbeta=sqrt(c(2.5, 2, 0.9, 0.7, 0.5)))


datatrain <- sim_data(n=200, coeflist=coeflist)

datatest <- sim_data(n=20000, coeflist=coeflist)




trainind <- sample(1:nrow(datatrain), size=0.8*nrow(datatrain))
datatrain1 <- datatrain[trainind,]
datatrain2 <- datatrain[setdiff(1:nrow(datatrain), trainind),]


set.seed(1234)
object <- topdown(y ~ ., data=datatrain1)
predsold <- mypredict(object, data=datatrain2)

set.seed(1234)
object <- mytopdown(y ~ ., data=datatrain1)
predsnew <- mypredict(object, data=datatrain2)

hierre(truth=datatrain2$y, response=predsold, type="macro")
hierre(truth=datatrain2$y, response=predsnew, type="macro")

hierpr(truth=datatrain2$y, response=predsold, type="macro")
hierpr(truth=datatrain2$y, response=predsnew, type="macro")


set.seed(1234)
object1 <- topdown(y ~ ., data=datatrain)
predsbigold <- mypredict(object1, data=datatest)

hierre(truth=datatest$y, response=predsbigold, type="macro")
hierpr(truth=datatest$y, response=predsbigold, type="macro")

set.seed(1234)
object2 <- mytopdown(y ~ ., data=datatrain)
predsbignew <- mypredict(object2, data=datatest)

hierre(truth=datatest$y, response=predsbignew, type="macro")
hierpr(truth=datatest$y, response=predsbignew, type="macro")


ui1 <- data.frame(truth=datatrain2$y, prednew=predsnew)
ui2 <- data.frame(truth=datatest$y, prednew=predsbignew)







set.seed(1234)

seeds <- sample(1000:1000000, size=4)


stratCVaccs <- CVaccs <- 0

for(count in 1:4) {
  
  set.seed(seeds[count])
  
  library("hierclass")
  library("mlr3")
  
  coeflist <- simulate_coefs(treestruc=treestruc, sdbeta0=sqrt(1),
                             sdbeta=sqrt(c(2.5, 2, 0.9, 0.7, 0.5)))
  datatrain <- sim_data(n=200, coeflist=coeflist)
  
  
  # Define the task for the top-down classification rule:
  task = as_task_classif(y ~ ., data = datatrain)
  
  # Initialize the learner for the top-down classification rule:
  learner = lrn("classif.topdown")
  
  cv3 <- rsmp("repeated_cv", repeats = 1, folds = 5)
  
  # task$col_roles$stratum = task$target_names
  
  cv3$instantiate(task)
  
  foldinfoCV <- as.data.frame(cv3$instance)
  foldinfoCV <- foldinfoCV[order(foldinfoCV$row_id),]
  
  ynew <- datatrain$y
  ynew <- factor(as.character(ynew))
  
  
  foldCV <- sapply(1:5, function(x) table(ynew[foldinfoCV$fold==x]))#/sum(table(ynew[ui$fold==x])))
  foldCV
  boxplot(t(foldCV))
  
  
  
  task$col_roles$stratum = task$target_names
  
  cv3$instantiate(task)
  
  foldinfostratCV <- as.data.frame(cv3$instance)
  foldinfostratCV <- foldinfostratCV[order(foldinfostratCV$row_id),]
  
  foldstratCV <- sapply(1:5, function(x) table(ynew[foldinfostratCV$fold==x]))#/sum(table(ynew[ui$fold==x])))
  boxplot(t(foldstratCV))
  
  
  
  
  stratCVacc <- CVacc <- 0
  
  for(i in 1:5) {
    
    object <- topdown(y ~ ., data=datatrain[foldinfostratCV$fold!=i,])
    predsstratCV <- predict(object, data=datatrain[foldinfostratCV$fold==i,])
    truthstratCV <- datatrain[foldinfostratCV$fold==i,]$y
    
    object <- topdown(y ~ ., data=datatrain[foldinfoCV$fold!=i,])
    predsCV <- predict(object, data=datatrain[foldinfoCV$fold==i,])
    truthCV <- datatrain[foldinfoCV$fold==i,]$y
    
    stratCVacc[i] <- mean(predsstratCV==truthstratCV)
    CVacc[i] <- mean(predsCV==truthCV)
    
    cat(mean(predsstratCV==truthstratCV), "\n")
    cat(mean(predsCV==truthCV), "\n")
    
  }
  
  stratCVaccs[count] <- mean(stratCVacc)
  CVaccs[count] <- mean(CVacc)
  
  cat(paste0("Iteration: ", count), "\n")
  
}


stratCVaccs
CVaccs

plot(rep(1:2, each=4), c(stratCVaccs, CVaccs))


plot(rep(1:2, each=5), c(stratCVacc, CVacc))


stratCVacc

i<-4
object <- topdown(y ~ ., data=datatrain[foldinfostratCV$fold!=i,])
predsstratCV <- predict(object, data=datatrain[foldinfostratCV$fold==i,])
truthstratCV <- datatrain[foldinfostratCV$fold==i,]$y

ui <- data.frame(pred=predsstratCV, truth=truthstratCV)

ui2 <- ui[ui$pred==ui$truth,]

indic <- rep(0, nrow(foldstratCV))
indic[rownames(foldstratCV) %in% as.character(ui2$pred)] <- 1

oh <- cbind(foldstratCV, indic)



i<-1
object <- topdown(y ~ ., data=datatrain[foldinfoCV$fold!=i,])
predsCV <- predict(object, data=datatrain[foldinfoCV$fold==i,])
truthCV <- datatrain[foldinfoCV$fold==i,]$y

ui <- data.frame(pred=predsCV, truth=truthCV)

ui2 <- ui[ui$pred==ui$truth,]

indic <- rep(0, nrow(foldCV))
indic[rownames(foldCV) %in% as.character(ui2$pred)] <- 1

ohCV <- cbind(foldCV, indic)

fix(oh)


par(mfrow=c(1,2))
boxplot(t(foldCV))
boxplot(t(foldstratCV))
par(mfrow=c(1,1))



foldCV
foldstratCV
all(rownames(foldCV)==rownames(foldstratCV))

ui <- cbind(foldCV, foldstratCV)
fix(ui)


foldinfo



# HIER GEHTS WEITER



set.seed(seeds[2])

library("hierclass")
library("mlr3")

coeflist <- simulate_coefs(treestruc=treestruc, sdbeta0=sqrt(1),
                           sdbeta=sqrt(c(2.5, 2, 0.9, 0.7, 0.5)))
datatrain <- sim_data(n=200, coeflist=coeflist)


# Define the task for the top-down classification rule:
task = as_task_classif(y ~ ., data = datatrain)

# Initialize the learner for the top-down classification rule:
learner = lrn("classif.topdown")

cv3 <- rsmp("repeated_cv", repeats = 1, folds = 5)

# task$col_roles$stratum = task$target_names

cv3$instantiate(task)

foldinfoCV <- as.data.frame(cv3$instance)
foldinfoCV <- foldinfoCV[order(foldinfoCV$row_id),]

ynew <- datatrain$y
ynew <- factor(as.character(ynew))


foldCV <- sapply(1:5, function(x) table(ynew[foldinfoCV$fold==x]))#/sum(table(ynew[ui$fold==x])))
foldCV
boxplot(t(foldCV))



task$col_roles$stratum = task$target_names

cv3$instantiate(task)

foldinfostratCV <- as.data.frame(cv3$instance)
foldinfostratCV <- foldinfostratCV[order(foldinfostratCV$row_id),]

foldstratCV <- sapply(1:5, function(x) table(ynew[foldinfostratCV$fold==x]))#/sum(table(ynew[ui$fold==x])))
boxplot(t(foldstratCV))




stratCVacc <- CVacc <- 0

allclasses <- as.character(unique(datatrain$y))
allclassesscoresCV <- allclassesscoresstratCV <- rep(0, length(allclasses))

for(i in 1:5) {
  
  object <- topdown(y ~ ., data=datatrain[foldinfostratCV$fold!=i,])
  predsstratCV <- predict(object, data=datatrain[foldinfostratCV$fold==i,])
  truthstratCV <- datatrain[foldinfostratCV$fold==i,]$y
  
  object <- topdown(y ~ ., data=datatrain[foldinfoCV$fold!=i,])
  predsCV <- predict(object, data=datatrain[foldinfoCV$fold==i,])
  truthCV <- datatrain[foldinfoCV$fold==i,]$y
  
  stratCVacc[i] <- mean(predsstratCV==truthstratCV)
  CVacc[i] <- mean(predsCV==truthCV)
  
  for(j in seq(along=allclasses)) {
    allclassesscoresstratCV[j] <- allclassesscoresstratCV[j] + sum(predsstratCV==truthstratCV & truthstratCV==allclasses[j])
    allclassesscoresCV[j] <- allclassesscoresCV[j] + sum(predsCV==truthCV & truthCV==allclasses[j])
  }
  
  
  cat(mean(predsstratCV==truthstratCV), "\n")
  cat(mean(predsCV==truthCV), "\n")
  
}

allclassesscoresstratCV

#asdf

sapply(allclasses, function(x) sum(datatrain$y==x))


allclassesscoresstratCV <- allclassesscoresstratCV/sapply(allclasses, function(x) sum(datatrain$y==x))
allclassesscoresCV <- allclassesscoresCV/sapply(allclasses, function(x) sum(datatrain$y==x))

# stratCVaccs[count] <- mean(stratCVacc)
# CVaccs[count] <- mean(CVacc)
# 
# cat(paste0("Iteration: ", count), "\n")

  
stratCVres <- data.frame(pred=predsstratCV, truth=truthstratCV)
stratCVres$hit <- as.numeric(predsstratCV==truthstratCV)
stratCVres$size <- sapply(stratCVres$truth, function(x) sum(datatrain$y==x))

CVres <- data.frame(pred=predsCV, truth=truthCV)
CVres$hit <- as.numeric(predsCV==truthCV)
CVres$size <- sapply(CVres$truth, function(x) sum(datatrain$y==x))

fix(CVres)

boxplot(CVres$size ~ CVres$hit)
boxplot(stratCVres$size ~ stratCVres$hit)

# boxplot(stratCVres$size, CVres$size )


fix(stratCVres)


fix(stratCVres)









ui <- cbind(foldCV, NA, foldstratCV)
fix(ui)
foldCV <- foldCV[order(rownames(foldCV)),]
foldstratCV <- foldstratCV[order(rownames(foldstratCV)),]

allclassesscoresstratCV <- allclassesscoresstratCV[order(names(allclassesscoresstratCV))]
allclassesscoresCV <- allclassesscoresCV[order(names(allclassesscoresCV))]

names(allclassesscoresstratCV)==rownames(foldCV)



ui <- cbind(foldCV, allclassesscoresCV, NA, foldstratCV, allclassesscoresstratCV)
fix(ui)

ui <- cbind(foldCV, allclassesscoresCV*rowSums(foldCV), NA, foldstratCV, allclassesscoresstratCV*rowSums(foldstratCV))

fix(ui)

datatrain500 <- sim_data(n=500, coeflist=coeflist)


# WICHTIG: IN DEN FOLGENDEN BESCHREIBUNGEN NICHT VERGESSEN, DASS ES HIER
# SPEZIFISCH UM N=200 GEHT.
# Die Rfs sgaen vermehrt die gro?en klassen richtig vorher.
# strat liefert f?r manche gro?e klassen mehr richtige pr?diktionen.
# dazu m?ssen mehr pr?dregeln richtige ergebnisse
# liefern als bei regcv.
# regcv hat den nachteil, dass wenn im testfold viele beobachtungen
# einer gro?en klasse sind, sind im trainingsfold nur noch wenige beobcahtungen,
# weshalb diese klasse dann nicht mehr so gut vorverhergesagt werden,
# was wiederum schlecht ist, weil dann die vielen beobacthungen aus dem
# testfold die eigentlich gut vorhergesagt werden k?nnen dann nicht mehr
# gut vorhergesagt werden k?nnen. Umgekehrt, wenn im testfold nur wenige
# beobachtungen der gro?en klasse sind, k?nnen diese zwar gut vorhergesagt
# werden, weil dann im trainingsfold viele sind, aber das bringt nicht so viel,
# weil die wenigen beobcahtungen im testfold dann die gesamt-performancesch?tzung
# nicht stark beeinflussen.
# bei strat gibt es diese probleme nicht. Da funktionieren die pr?dregeln immer
# etwa gleich gut bzgl der gro?en klassen (die ja die einzig relvanten sind),
# weshalb am ende dann mehr beobachtugnen dieser klasse vorhergesagt werden.
# das obige kl?rt warum, stratcv besssere performanzsch?tzungen liefert als
# cv, aber warum ?bersch?tzt stratcv den fehler?
# das k?nnte damit zusammenh?ngen, dass f?r n=200 viele klassen nicht in den
# daten vorkommen, sondenr nur auf den testdaten, weshalb die testdatenperformanz
# dann geringer ist. dass deshalb die performanz ?bersch?tz wird ist 'normal',
# also liefert stratcv hier eigentlich gar kein 'gebiastes' verhalten.
# dass das bei regcv nicht der fall ist, liegt einfach daran, dass regcv auf
# die art wie oben beschrieben gebiast ist. UPDATE: Habe mal ausprobiert, was
# passiert wenn  man nur die Klassen in den Testdaten berücksichtigt, die auch
# in den Traininsdaten vorkommen. Das ist die testdatenperformanz zwar besser,
# aber immmer noch kleiner als die von stratcv. Aber warum ist dann der stratcv-Fehler
# größer als der testdatensatzfehler? Im Notfall, wenn wahre erklärung nicht
# finde muss ich schreiben, dass die wahre erklaerung unklar ist, dass wir aber zumindest
# beobachtet haben, dass die testdatenperformanz besser ist, wenn wir nur die
# klassen aus den trainingsdaten berücksichtigen. Außerdem ist dann der fehler
# für manche perfmaße wie spath nicht mehr unterschätzt durch stratcv. NEUE *BESTE*
# ERKLAERUNG: Für kleines n ist die Klassenverteilung in den Trainingsdaten
# durch Zufall ein bisschen ander als die wahre in den Testdaten. Auch dadurch könnte der Fehler
# durch stratcv etwas überschätzt werden, weil da die Klassenverteilungen in
# den testfolds künstlich gleich zu denen in der trainingfolds gemacht wird.








# Define the task for the top-down classification rule:
task = as_task_classif(y ~ ., data = datatrain500)

# Initialize the learner for the top-down classification rule:
learner = lrn("classif.topdown")

cv3 <- rsmp("repeated_cv", repeats = 1, folds = 5)

task$col_roles$stratum = task$target_names

cv3$instantiate(task)

aha <- as.data.frame(cv3$instance)
ui <- aha[order(aha$row_id),]

ynew <- datatrain500$y
ynew <- factor(as.character(ynew))


so <- sapply(1:5, function(x) table(ynew[ui$fold==x]))#/sum(table(ynew[ui$fold==x])))
boxplot(t(so))

so












ynew <- datatrain500$y
ynew <- factor(as.character(ynew))
so <- sapply(1:5, function(x) table(ynew[ui$fold==x])/sum(table(ynew[ui$fold==x])))
boxplot(t(so))


datatrain500 <- sim_data(n=1000, coeflist=coeflist)


# Define the task for the top-down classification rule:
task = as_task_classif(y ~ ., data = datatrain500)

# Initialize the learner for the top-down classification rule:
learner = lrn("classif.topdown")

cv3 <- rsmp("repeated_cv", repeats = 1, folds = 5)

#  task$col_roles$stratum = task$target_names

cv3$instantiate(task)

aha <- as.data.frame(cv3$instance)
ui <- aha[order(aha$row_id),]

ynew <- datatrain500$y
ynew <- factor(as.character(ynew))


so <- sapply(1:5, function(x) table(ynew[ui$fold==x]))#/sum(table(ynew[ui$fold==x])))
boxplot(t(so))


so
so2

task$col_roles$stratum = task$target_names

cv3$instantiate(task)

aha <- as.data.frame(cv3$instance)
ui <- aha[order(aha$row_id),]

so2 <- sapply(1:5, function(x) table(ynew[ui$fold==x]))#/sum(table(ynew[ui$fold==x])))
boxplot(t(so2))



boxplot(apply(so, 1, var), apply(so2, 1, var))


boxplot()







par(mfrow=c(2,1))
boxplot(t(so))
boxplot(t(so2))
par(mfrow=c(1,1))

so



hierre <- function(truth, response, type="micro") {
  
  truth <- ui2$truth
  response <- ui2$prednew
  
  
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









hierpr <- function(truth, response, type="micro") {

    truth <- ui2$truth
  response <- ui2$prednew
  
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











predict.topdown <- function(object, data, confid=1, ...) {
  
  data <- datatrain2[1:2,]
  
  
  
  # Check whether the 'confid' parameter is specified appropriately. If it
  # is not on the prespecified grid, set it to the value closest  on the
  # grid.
  if(!is.numeric(confid) | confid <= 0 | confid > 1)
    stop("'confid' must be a numeric value in the interval ]0, 1].")
  
  thresholdmat <- rbind(c(0.995, 0.975, 0.95, 0.85, 0.75, 0.65, 0.55, 0),
                        c(0.942, 0.871, 0.817, 0.684, 0.592, 0.517, 0.439, 0),
                        c(0.864, 0.768, 0.707, 0.578, 0.5, 0.438, 0.368, 0),
                        c(0.788, 0.684, 0.624, 0.505, 0.437, 0.382, 0.32, 0),
                        c(0.722, 0.616, 0.559, 0.451, 0.39, 0.34, 0.286, 0),
                        c(0.664, 0.561, 0.507, 0.408, 0.353, 0.308, 0.259, 0),
                        c(0.615, 0.516, 0.465, 0.374, 0.323, 0.283, 0.238, 0),
                        c(0.573, 0.477, 0.43, 0.345, 0.299, 0.262, 0.22, 0),
                        c(0.536, 0.445, 0.401, 0.322, 0.279, 0.244, 0.205, 0),
                        c(0.503, 0.417, 0.375, 0.301, 0.261, 0.229, 0.193, 0),
                        c(0.475, 0.392, 0.353, 0.284, 0.246, 0.216, 0.182, 0),
                        c(0.45, 0.371, 0.333, 0.268, 0.233, 0.204, 0.172, 0),
                        c(0.427, 0.352, 0.316, 0.254, 0.221, 0.194, 0.164, 0),
                        c(0.407, 0.335, 0.301, 0.242, 0.21, 0.185, 0.156, 0))
  colnames(thresholdmat) <- c(0.01, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 1)
  rownames(thresholdmat) <- 2:15
  
  if(!(confid %in% as.numeric(colnames(thresholdmat)))) {
    confidvalues <- as.numeric(colnames(thresholdmat))
    confid <- confidvalues[nnet::which.is.max(1-abs(confidvalues-confid))]
    warning(paste0("'confid' must take one of the following values: ",
                   paste(colnames(thresholdmat), collapse=", "),
                   ". 'confid' set to ", confid))
  }
  
  # Check whether all independent variables are present in the data:
  if (sum(!(object$modellist[[max(which(sapply(object$modellist, function(x) class(x$learner)[1]!="logical")))]]$learner$model$forest$independent.variable.names %in% colnames(data))) > 0) {
    stop("One or more independent variables not found in data.")
  }
  
  # We obtain the predictions of all forests for all observations, even though
  # each forest only will see a fraction of the observations.
  # However, it is computationally more efficient to call 'ranger::predict' on
  # all observations beforehand than separately for each observation while
  # dropping the observations down the category tree.
  for(i in seq(along=object$modellist)) {
    if(length(object$modellist[[i]]$subclasses) > 1)
      object$modellist[[i]]$preds <- predict(object$modellist[[i]]$learner, newdata=data, predict_type = "prob")
  }
  
  # Drop the observations down the category tree:
  
  thresholds <- thresholdmat[,as.character(confid)]
  
  preds <- vector(mode="character", length=nrow(data))
  
  for(i in 1:nrow(data)) {
    
    # Obtain a prediction using the classifier in the root node:
    
    predprobs <- object$modellist[[1]]$preds[i,]
    
    predclass <- names(predprobs)[nnet::which.is.max(predprobs)]
    preds[i] <- predclass
    
    # Drop the observation further down the category tree:
    
    if(!is.na(preds[i])) {
      
      repeat {
        
        # If, for the current class length(object$modellist[[predclass]]$subclasses)==1,
        # this can mean that is.na(object$modellist[[predclass]]$subclasses)==TRUE
        # which indicates that the current class is a leaf node or
        # the current class only has one subclass:
        stoploop <- FALSE
        while(length(object$modellist[[predclass]]$subclasses)==1) {
          # In case the current class has no subclass
          # (indicated by is.na(object$modellist[[predclass]]$subclasses)==TRUE),
          # the class is a leaf node and we stop the prediction process:
          if(is.na(object$modellist[[predclass]]$subclasses)) {
            stoploop <- TRUE
            break
          }
          else {
            # In rare cases a class only has a single subclass. In that case,
            # of course, the next predicted class is that single subclass:
            predclass <- object$modellist[[predclass]]$subclasses
            preds[i] <- paste0(preds[i], ".", predclass)
          }
        }
        if(stoploop)
          break
        
        # If the current class has more than one subclasses we obtain a prediction
        # using the classifier associated with the current node:
        
        predprobs <- object$modellist[[predclass]]$preds[i,]
        
        # If there are more than 15 subclasses the prediction process stops:
        if(length(object$modellist[[predclass]]$subclasses) <= max(as.numeric(names(thresholds))))
          threshold <- thresholds[as.character(length(object$modellist[[predclass]]$subclasses))]
        else
          break
        
        # We obtain a prediction for the next subclass only if the maximum
        # probability exceeds the threshold:
        if(max(predprobs) > threshold) {
          
          predclass <- names(predprobs)[nnet::which.is.max(predprobs)]
          preds[i] <- paste0(preds[i], ".", predclass)
          
        } else {
          break
        }
        
      }
      
    }
    
  }
  
  # Assign the class levels obtained from the training
  # data:
  preds <- factor(preds, levels=object$allclasses)
  
  return(preds)
  
}













#asdf

  
  mytopdown <- function(formula, data, dependent.variable.name, num.trees=500,
                      mtry, sample.fraction=0.667, maxnthreads=TRUE) {

    ## Formula interface. Use whole data frame is no formula provided and depvarname given
    if (is.null(formula)) {
      if (is.null(dependent.variable.name)) {
        stop("Error: Please give formula or outcome variable name.")
      }
      response <- data[, dependent.variable.name, drop = TRUE]
      data.selected <- data
      yvarname <- dependent.variable.name
    } else {
      formula <- formula(formula)
      if (!inherits(formula, "formula")) {
        stop("Error: Invalid formula.")
      }
      data.selected <- parse.formula(formula, data, env = parent.frame())
      response <- data.selected[, 1]
      yvarname <- names(data.selected)[1]
    }
    
    
    ## Check missing values
    if (any(is.na(data.selected))) {
      offending_columns <- colnames(data.selected)[colSums(is.na(data.selected)) > 0]
      stop("Missing data in columns: ",
           paste0(offending_columns, collapse = ", "), ".", call. = FALSE)
    }
    
    # Set mtry to the default value if it is missing:
    if(missing(mtry))
      mtry <- floor(sqrt(ncol(data.selected)-1))
    
    
    # Get the unique classes and order them according their depth:
    
    yun <- as.character(unique(response))
    yun <- yun[order(sapply(yun, function(x) length(strsplit(x, split="\\.")[[1]])))]
    
    
    # Make a matrix which contains the class hierarchy, where each column
    # corresponds to a specific level of the hierarchy (used lateron for determening
    # the direct descendants of the internal nodes):
    
    ylist <- lapply(yun, function(x) strsplit(x, split="\\.")[[1]])
    
    # all classes and sub classes, used to assign the
    # appropriate class levels to the predictions performed by predict.topdown:
    if (class(response)=="factor")
      allclasses <- unique(unlist(lapply(lapply(levels(response), function(x) strsplit(x, split="\\.")[[1]]), function(x) lapply(1:length(x), function(y) paste(x[1:y], collapse=".")))))
    else
      allclasses <- unique(unlist(lapply(ylist, function(x) lapply(1:length(x), function(y) paste(x[1:y], collapse=".")))))
    
    mdepth <- max(sapply(ylist, length))
    
    ymat <- t(sapply(ylist, function(x) {
      c(x, rep(NA, mdepth-length(x)))
    }))
    
    
    
    # All classes, external and internal ones:
    
    yclasses <- unique(as.vector(ymat))
    yclasses <- yclasses[!is.na(yclasses)]
    
    
    
    # For each class get its direct descendants:
    
    modellist <- vector(mode = "list", length = length(yclasses)+1)
    
    modellist[[1]] <- list()
    modellist[[1]]$subclasses <- unique(ymat[,1])
    
    for(i in seq(along=yclasses)) {
      
      modellist[[i+1]] <- list()
      
      indtemp <- which(apply(ymat, 2, function(x) yclasses[i] %in% x))
      
      # CHANGE ROMAN:
      #!# if(sum(ymat[,indtemp]==yclasses[i], na.rm=TRUE) > 1)
      if (indtemp < ncol(ymat))
        modellist[[i+1]]$subclasses <- unique(ymat[,indtemp+1][!is.na(ymat[,indtemp]) & ymat[,indtemp]==yclasses[i]])
      else
        modellist[[i+1]]$subclasses <- NA
      
    }
    
    names(modellist) <- "zeronode"
    names(modellist)[-1] <- yclasses
    
    
    
    # Add to the data set new columns, where each corresponds to one of the classification
    # problems in the category tree (these will be used by ranger lateron):
    
    yall <- lapply(as.character(response), function(x) strsplit(x, split="\\.")[[1]])
    
    for(i in seq(along=modellist)) {
      
      if(length(modellist[[i]]$subclasses) > 1) {
        
        data.selected$newvariable <- NA
        
        for(j in seq(along=modellist[[i]]$subclasses)) {
          data.selected$newvariable[sapply(yall, function(x) modellist[[i]]$subclasses[j] %in% x)] <- modellist[[i]]$subclasses[j]
        }
        data.selected$newvariable <- factor(data.selected$newvariable, levels=modellist[[i]]$subclasses)
        names(data.selected)[length(names(data.selected))] <- names(modellist)[i]
        
      }
      
    }
    
    
    # Learn a random forest for each classification problem in the tree:
    
    for(i in seq(along=modellist)) {
      
      if(length(modellist[[i]]$subclasses) > 1) {
        
        datatemp <- data.selected[,!(names(data.selected) %in% c(yvarname, names(modellist)[-i]))]
        names(datatemp)[names(datatemp)==names(modellist)[i]] <- "yvartemp"
        datatemp <- datatemp[!is.na(datatemp$yvartemp),]
        
        tasktemp <- mlr3::as_task_classif(yvartemp ~ ., data = datatemp)
        
        if(maxnthreads)
          learner <- mlr3::lrn("classif.ranger", predict_type = "prob", num.threads=parallel::detectCores())
        else
          learner <- mlr3::lrn("classif.ranger", predict_type = "prob")
        learner$param_set$values <- mlr3misc::insert_named(learner$param_set$values,
                                                           list(num.trees = num.trees, mtry = mtry, replace = FALSE, sample.fraction = sample.fraction))
        
        learner$train(tasktemp)
        
        modellist[[i]]$learner <- learner
        
      } else
        modellist[[i]]$learner <- NA
      
    }
    
    output <- list(modellist=modellist, allclasses=allclasses)
    
    # Assign class to the output:
    class(output) <- "topdown"
    
    # Return the output:
    return(output)
    
  }
  














?topdown


# Load the required packages:
library("mlr3")
library("hierclass")

lgr::get_logger("mlr3")$set_threshold("warn")


# Define the task for the top-down classification rule:
task = as_task_classif(y ~ ., data = datatrain)


# Initialize the learner for the top-down classification rule:
learner = lrn("classif.topdown")



set.seed(seed_res)

cv3 <- rsmp("repeated_cv", repeats = 10, folds = 5)

cv3$instantiate(task)
result_cv3 <- resample(task=task, learner=learner, resampling=cv3)

CV_vals <- c(result_cv3$aggregate(msr("classif.hierfbeta", type="micro")),
             result_cv3$aggregate(msr("classif.hierfbeta", type="macro")),
             result_cv3$aggregate(msr("classif.hierpr", type="micro")),
             result_cv3$aggregate(msr("classif.hierpr", type="macro")),
             result_cv3$aggregate(msr("classif.hierre", type="micro")),
             result_cv3$aggregate(msr("classif.hierre", type="macro")),
             result_cv3$aggregate(msr("classif.hloss")),
             result_cv3$aggregate(msr("classif.spath")),
             result_cv3$aggregate(msr("classif.acc")))




set.seed(seed_res)

task$col_roles$stratum = task$target_names

cv3$instantiate(task)
result_cv3 <- resample(task=task, learner=learner, resampling=cv3)

stratCV_vals <- c(result_cv3$aggregate(msr("classif.hierfbeta", type="micro")),
                  result_cv3$aggregate(msr("classif.hierfbeta", type="macro")),
                  result_cv3$aggregate(msr("classif.hierpr", type="micro")),
                  result_cv3$aggregate(msr("classif.hierpr", type="macro")),
                  result_cv3$aggregate(msr("classif.hierre", type="micro")),
                  result_cv3$aggregate(msr("classif.hierre", type="macro")),
                  result_cv3$aggregate(msr("classif.hloss")),
                  result_cv3$aggregate(msr("classif.spath")),
                  result_cv3$aggregate(msr("classif.acc")))





datacompl <- rbind(datatrain, datatest)
ntrain <- nrow(datatrain); nall <- nrow(datacompl)
rm(datatrain, datatest); gc()


# Define the task for the top-down classification rule:
task = as_task_classif(y ~ ., data = datacompl)


learner$train(task, row_ids = 1:ntrain)


predictions = learner$predict(task, row_ids = (ntrain+1):nall)

truth_vals <- c(predictions$score(msr("classif.hierfbeta", type="micro")),
                predictions$score(msr("classif.hierfbeta", type="macro")),
                predictions$score(msr("classif.hierpr", type="micro")),
                predictions$score(msr("classif.hierpr", type="macro")),
                predictions$score(msr("classif.hierre", type="micro")),
                predictions$score(msr("classif.hierre", type="macro")),
                predictions$score(msr("classif.hloss")),
                predictions$score(msr("classif.spath")),
                predictions$score(msr("classif.acc")))



res <- data.frame(measure=c("hierf_micro", "hierf_macro", "hierpr_micro", 
                            "hierpr_macro", "hierre_micro", "hierre_macro",
                            "hloss", "spath", "acc"), CV_vals=CV_vals, 
                  stratCV_vals=stratCV_vals, truth_vals=truth_vals)






# resultstemp <- results[results$type %in% c("CV_percabsdiff", "stratCV_percabsdiff"),]
# resultstemp$type <-factor(resultstemp$type, levels=c("CV_percabsdiff", "stratCV_percabsdiff"))
# 
# library("ggplot2")
# p <- ggplot(data=resultstemp, aes(x=n, y=value, fill=type)) + theme_bw() +
#   geom_boxplot() + facet_wrap(~measure, scales="free_y") + ylab("Standardized deviation") +
#   theme(axis.title = element_text(color="black"), legend.position = "none")
# p
# 
# ggsave("./hierpr/Results/figures/standardized_deviation.pdf", width=9, height=9)
# stDev


# resultstemp <- resultspercdiffvar#[resultspercdiffvar$n!=200,]
# resultstemp$type <-factor(resultstemp$type, levels=c("CV_percdiff", "stratCV_percdiff"))
# 
# library("ggplot2")
# p <- ggplot(data=resultstemp, aes(x=n, y=var, fill=type)) + theme_bw() +
#   geom_bar(stat="identity", width=.5, position = "dodge") + facet_wrap(~measure, scales="free_y")
# p





source("./hierpr/functions_simulation.R")
load("./hierpr/results/intermediate_results/treestruc.Rda")

p <- plot_structure(treestruc)

ggsave("./hierpr/results/figures/FigureS17.pdf", width=18*0.9, height=3.5*0.9)









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
