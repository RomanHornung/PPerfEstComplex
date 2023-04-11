setwd("C:/Projects/DESTATIS/PredErrorComplex/PPerfEstComplex")


# Load and pre-process the results:
###################################

load("./Simulations/nsrs/results/intermediate_results/results.Rda")


library(ggplot2)
library(dplyr)



#### Visualise the results, analyse the bias


#### compare unbased and biased version of estimates
ggframe <- rbind(results %>% mutate(type = "biased"),
                results %>% mutate(estimated_error = corrected,
                                     type = "de-biased (HT)")
                ) %>%
          mutate(bias       = estimated_error - true_error)

results %>% group_by(model, n, correct) %>% summarise(bias = mean(corrected - true_error))
results %>% group_by(model, n, correct) %>% summarise(bias = mean(estimated_error - true_error))

#### look at all settings jointly
p <- ggplot(ggframe) +
        geom_boxplot(aes(y=bias, x = model, fill = type)) + 
        scale_fill_manual(values = c("#999999", "#E69F00")) +
        facet_grid(~n+correct) + 
        theme_bw() + theme(axis.text=element_text(color="black"))
p

ggsave("./Simulations/nsrs/results/figures/nsrs.pdf", width=15, height=7)
