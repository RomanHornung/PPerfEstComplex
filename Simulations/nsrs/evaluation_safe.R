setwd("Z:/Projects/DESTATIS/PredErrorComplex/PPerfEstComplex")


# Load the results:

load("./Simulations/nsrs/results/intermediate_results/results.Rda")


library("ggplot2")
library("dplyr")



# Visualise the results and analyse the bias:

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
  geom_boxplot() + 
  facet_wrap(~correctmod+n, nrow=2, scales="free_y") + 
  theme_bw() + theme(axis.title.x=element_blank(), 
                     axis.title.y=element_text(size=14), 
                     axis.text.x = element_text(angle=45, hjust = 1, color="black", size=12), 
                     axis.text.y = element_text(color="black", size=11), 
                     strip.text = element_text(size=12),
                     legend.position = c(0.75, 0.9))
p

ggsave("./Simulations/nsrs/results/figures/nsrs.pdf", width=15, height=7)



# look at all settings jointly
p <- ggplot(data=ggframe %>% filter(model == "linear"), aes(y=bias, x = model, fill = type)) +
  geom_boxplot() + 
  facet_wrap(~correctmod+n, nrow=2, scales="free_y") + 
  theme_bw() + theme(axis.title.x=element_blank(), 
                     axis.title.y=element_text(size=14), 
                     axis.text.x = element_text(angle=45, hjust = 1, color="black", size=12), 
                     axis.text.y = element_text(color="black", size=11), 
                     strip.text = element_text(size=12),
                     legend.position = c(0.92, 0.95))
p



# scale_fill_manual(values = c("#999999", "#E69F00")) +