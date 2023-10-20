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

load("./nsrs/results/intermediate_results/results.Rda")
load("./nsrs/results/intermediate_results/scenariogrid.Rda")

reorderind <- order(scenariogrid$N, scenariogrid$repetition, scenariogrid$correct_model)
results <- results[reorderind]
scenariogrid <- scenariogrid[reorderind,]

results <- do.call("rbind", results)






# Fig. 4: Simulation on unequal sampling probabilities: differences between estimated and true MSE values.


library("ggplot2")
library("dplyr")

library("RColorBrewer")

display.brewer.all(n=3, type="div")

colors <- brewer.pal(3, "RdBu")
selectedColors <- c(colors[1], colors[3])
print(selectedColors)

# compare unbiased and biased version of estimates
ggframe <- rbind(results %>% mutate(type = "biased"),
                results %>% mutate(estimated_error = corrected,
                                     type = "de-biased (HT)")
                ) %>%
          mutate(bias       = estimated_error - true_error)

results %>% group_by(model, n, correct) %>% summarise(bias = mean(corrected - true_error))
results %>% group_by(model, n, correct) %>% summarise(bias = mean(estimated_error - true_error))


ggframe$type
ggframe$correct

ggframe$correctmod <- "model correctly specified"
ggframe$correctmod[!ggframe$correct] <- "model misspecified"
ggframe$correctmod <- factor(ggframe$correctmod, levels=c("model correctly specified", "model misspecified"))

unique(ggframe$n)
ggframe$n <- paste0("n = ", ggframe$n)
ggframe$n <- factor(ggframe$n, levels=c("n = 100", "n = 500", "n = 1000"))

ggframe$model[ggframe$model=="regr.lm"] <- "linear models"
ggframe$model[ggframe$model=="regr.ranger"] <- "random forests"
ggframe$model <- factor(ggframe$model, levels=c("linear models", "random forests"))

# look at all settings jointly
p <- ggplot(data=ggframe, aes(y=bias, x = model, fill = type)) +
  geom_boxplot() + ylab("Difference between estimated and true MSE values") +
  facet_wrap(~correctmod+n, nrow=2, scales="free_y") + 
  scale_fill_manual(values=selectedColors) +
  theme_bw() + theme(axis.title.x=element_blank(), 
                     axis.title.y=element_text(size=17), 
                     axis.text.x = element_text(angle=45, hjust = 1, color="black", size=17), 
                     axis.text.y = element_text(color="black", size=14), 
                     strip.text = element_text(size=17),
                     legend.position = "none")
p

ggsave("./nsrs/results/figures/Figure4.pdf", width=14, height=9)
