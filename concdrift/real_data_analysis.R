####################################################################################

# NOTE: Before the code can be executed, the R working directory *MUST* 
# be set to the directory of the 'PPerfEstComplex' folder (for Linux
# systems, R can also be run directly from the 'PPerfEstComplex'
# directory):

# Remove the '#' from the line below and replace 'this/is/my/path/' by the path
# to the directory that contains 'PPerfEstComplex':

# setwd("this/is/my/path/PPerfEstComplex")

####################################################################################



# We downloaded the OECD data from the following link (last accessed: 29/10/24): 
# https://data-explorer.oecd.org/vis?fs[0]=Topic%2C1%7CEconomy%23ECO%23%7CShort-term%20economic%20statistics%23ECO_STS%23&pg=0&fc=Topic&bp=true&snb=54&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_LFS%40DF_IALFS_EMP_WAP_Q&df[ag]=OECD.SDD.TPS&df[vs]=1.0&dq=.EMP_WAP.._Z.Y._T.Y15T64..Q&lom=LASTNPERIODS&lo=7&to[TIME_PERIOD]=false

# On the above webpage click 'Download --> Unfiltered data in tabular text (CSV)'.
# This download the file "OECD.SDD.TPS,DSD_LFS@DF_IALFS_EMP_WAP_Q,1.0+all.csv",
# which contains the data for all countries.
# Because the above file takes up a lot of storage space, we only included in this
# code supplement, the data file "OECD_employment_rate_data_germany.csv" for Germany 
# from the above file, which was obtained using the following code:

# data <- read.csv("./concdrift/data/OECD.SDD.TPS,DSD_LFS@DF_IALFS_EMP_WAP_Q,1.0+all.csv")
# 
# data_ger <- data[data$Reference.area=="Germany",]
# 
# write.csv(data_ger, "./concdrift/data/OECD_employment_rate_data_germany.csv", row.names = FALSE)



# Load and pre-process the data:
###################################

data_ger <- read.csv("./concdrift/data/OECD_employment_rate_data_germany.csv")

names(data_ger)[names(data_ger)=="Sex"] <- "Gender"

data_ger <- data_ger[data_ger$Adjustment=="Calendar and seasonally adjusted" & data_ger$Gender!="Total" &
                       data_ger$Age %in% c("From 55 to 64 years", "From 15 to 24 years", "From 25 to 54 years"),]
data_ger <- data_ger[grep("-Q", data_ger$TIME_PERIOD),]
data_ger <- data_ger[,sapply(data_ger, function(x) length(unique(x)))>1]
data_ger$SEX <- data_ger$AGE <- NULL
data_ger$TIME_PERIOD <- factor(data_ger$TIME_PERIOD, levels=sort(unique(data_ger$TIME_PERIOD)))
data_ger <- data_ger[order(data_ger$TIME_PERIOD),]
levels(data_ger$TIME_PERIOD)

data_ger$Age <- factor(data_ger$Age, levels=c("From 15 to 24 years", "From 25 to 54 years", "From 55 to 64 years"))
data_ger$Gender <- factor(data_ger$Gender, levels=c("Male", "Female"))

data_ger$Year <- sub("-Q.*", "", data_ger$TIME_PERIOD)
data_ger$Year <- as.numeric(data_ger$Year)


data_ger <- data_ger[data_ger$Year!=2024,]
data_ger$TIME_PERIOD <- droplevels(data_ger$TIME_PERIOD)





# Fig. 8: Real data analysis of concept drift: employment rates in Germany separated 
# by year. The figure displays the employment rates overall and separately by gender 
# and age group.


# Choose pleasing colors with "RColorBrewer":

library("RColorBrewer")

display.brewer.all(n=3, type="div")

colors <- brewer.pal(3, "RdYlBu")

selectedColors_1 <- c(colors[1], colors[3])

selectedColors_2 <- brewer.pal(3, "Spectral")



library("ggplot2")

data_plot <- data_ger
data_plot$Period <- "Observation"
data_plot$Period[data_plot$Year>2019] <- "Post-observation"
data_plot$Period <- factor(data_plot$Period, levels=c("Observation", "Post-observation"))

p1 <- ggplot(data_plot, aes(x = factor(Year), y = OBS_VALUE, fill = Period)) +
  geom_boxplot() +
  labs(title = "Employment rates by year",
       x = "Year",
       y = "Employment rate") +
  theme_bw() +
  theme(axis.title=element_text(size=17),
        legend.title=element_text(size=17),
        legend.text=element_text(size=15),
    axis.text.x = element_text(color="black", angle = 90, size = 13, hjust = 1),
    axis.text.y = element_text(color="black", size = 13),
    plot.title = element_text(size = 17)) + 
  scale_fill_manual(values=c("#4DB8A7", "#A9A9A9"))

# Add horizontal brackets
y_position = min(data_plot$OBS_VALUE) - 2 
gap = 0.15

p1 <- p1 + 
  annotate("segment", x = 0.8 - gap, xend = 3.5 - gap, y = y_position, yend = y_position, colour = "black") +
  annotate("segment", x = 0.8 - gap, xend = 0.8 - gap, y = y_position, yend = y_position + 1.5, colour = "black") +
  annotate("segment", x = 3.5 - gap, xend = 3.5 - gap, y = y_position, yend = y_position + 1.5, colour = "black") +
  
  annotate("segment", x = 3.5 + gap, xend = 5.5 - gap, y = y_position, yend = y_position, colour = "black") +
  annotate("segment", x = 3.5 + gap, xend = 3.5 + gap, y = y_position, yend = y_position + 1.5, colour = "black") +
  annotate("segment", x = 5.5 - gap, xend = 5.5 - gap, y = y_position, yend = y_position + 1.5, colour = "black")

for (i in 1:7) {
  p1 <- p1 + 
    annotate("segment", x = i*2 + 3.5 + gap, xend = i*2 + 5.5 - gap, y = y_position, yend = y_position, colour = "black") +
    annotate("segment", x = i*2 + 3.5 + gap, xend = i*2 + 3.5 + gap, y = y_position, yend = y_position + 1.5, colour = "black") +
    annotate("segment", x = i*2 + 5.5 - gap, xend = i*2 + 5.5 - gap, y = y_position, yend = y_position + 1.5, colour = "black")
}

p1


p2 <- ggplot(data_ger, aes(x = factor(Year), y = OBS_VALUE , fill = Gender)) +
  geom_boxplot() +
  labs(title = "Employment rates by year and gender",
       x = "Year",
       y = "Employment rate") +
  theme_bw() +
  scale_fill_manual(values = selectedColors_1) +
  theme(axis.title=element_text(size=17),
        legend.title=element_text(size=17),
        legend.text=element_text(size=15),
        axis.text.x = element_text(color="black", angle = 90, size = 13, hjust = 1),
        axis.text.y = element_text(color="black", size = 13),
        plot.title = element_text(size = 17))

p2


p3 <- ggplot(data_ger, aes(x = factor(Year), y = OBS_VALUE , fill = Age)) +
  geom_boxplot() +
  labs(title = "Employment rates by year and age",
       x = "Year",
       y = "Employment rate") +
  theme_bw() +
  scale_fill_manual(values = selectedColors_2) +
  theme(axis.title=element_text(size=17),
        legend.title=element_text(size=17),
        legend.text=element_text(size=15),
        axis.text.x = element_text(color="black", angle = 90, size = 13, hjust = 1),
        axis.text.y = element_text(color="black", size = 13),
        plot.title = element_text(size = 17))

p3


library("patchwork")

p <- p1 / p2 / p3
p

ggsave("./concdrift/results/figures/Figure8.pdf", width=12, height=13.5)









# Estimate MSE values with cross-validation (CV), time-series CV and 
# out-of-sample validation:
#####################################################################


# Prepare the data for this analysis:

data_final <- data_ger

data_final$Gender_male <- as.numeric(data_final$Gender=="Male")
data_final$Age_25_54 <- as.numeric(data_final$Age=="From 25 to 54 years")
data_final$Age_55_64 <- as.numeric(data_final$Age=="From 55 to 64 years")

data_final$Gender <- data_final$Age <- NULL



years <- data_final$Year

data_final$TIME_PERIOD <- data_final$Year <- NULL

names(data_final)[names(data_final)=="OBS_VALUE"] <- "empl_rate"



data_train <- data_final[years <= 2019,]
data_test <- data_final[years > 2019,]

years_train <- years[years <= 2019]
years_test <- years[years > 2019]




library("mlr3")
library("mlr3verse")


set.seed(1234)



# Use random forests and linear models as learners:

learner_temp_rf <- lrn("regr.ranger")
learner_temp_lm <- lrn("regr.lm")



# Set the task:

task <- as_task_regr(data_train, target="empl_rate")



# Regular CV:

cv <- rsmp("repeated_cv", repeats = 10, folds = 7)
cv$instantiate(task)
result_cv <- resample(task=task, learner=learner_temp_rf, resampling=cv)
mse_cv_rf <- result_cv$aggregate(msr("regr.mse"))
result_cv <- resample(task=task, learner=learner_temp_lm, resampling=cv)
mse_cv_lm <- result_cv$aggregate(msr("regr.mse"))





# Determine the indices of the training and test sets for the times-series CV:

train_setsTS_1s <- list(which(years_train %in% 2005:2007), which(years_train %in% 2005:2009),
     which(years_train %in% 2005:2011), which(years_train %in% 2005:2013),
     which(years_train %in% 2005:2015), which(years_train %in% 2005:2017))
train_setsTS_2s <- train_setsTS_1s[-length(train_setsTS_1s)]


test_setsTS_1s <- list(which(years_train %in% 2008:2009), which(years_train %in% 2010:2011),
                       which(years_train %in% 2012:2013), which(years_train %in% 2014:2015),
                       which(years_train %in% 2016:2017), which(years_train %in% 2018:2019))
test_setsTS_2s <- test_setsTS_1s[-1]




# Time-series CV - predict next season:

TScv <- rsmp("custom")
TScv$instantiate(task, train_setsTS_1s, test_setsTS_1s)

result_TScv <- resample(task=task, learner=learner_temp_rf, resampling=TScv)
mse_TScv_1s_rf <- result_TScv$aggregate(msr("regr.mse"))
result_TScv <- resample(task=task, learner=learner_temp_lm, resampling=TScv)
mse_TScv_1s_lm <- result_TScv$aggregate(msr("regr.mse"))



# Time series CV - predict next season but one:

TScv <- rsmp("custom")
TScv$instantiate(task, train_setsTS_2s, test_setsTS_2s)

result_TScv <- resample(task=task, learner=learner_temp_rf, resampling=TScv)
mse_TScv_2s_rf <- result_TScv$aggregate(msr("regr.mse")) 
result_TScv <- resample(task=task, learner=learner_temp_lm, resampling=TScv)
mse_TScv_2s_lm <- result_TScv$aggregate(msr("regr.mse"))



# Out-of-sample validation - predict last (i.e., seventh) season, train on first
# to sixth season:

learner_temp_rf$train(task, row_ids = train_setsTS_1s[[length(train_setsTS_1s)]])

predictions <- learner_temp_rf$predict(task, row_ids = test_setsTS_1s[[length(test_setsTS_1s)]])
mse_TSholdout_1s_rf <- predictions$score(msr("regr.mse"))

learner_temp_lm$train(task, row_ids = train_setsTS_1s[[length(train_setsTS_1s)]])

predictions <- learner_temp_lm$predict(task, row_ids = test_setsTS_1s[[length(test_setsTS_1s)]])
mse_TSholdout_1s_lm <- predictions$score(msr("regr.mse"))



# Out-of-sample validation - predict last (i.e., seventh) season, train on first
# to fifth season:

learner_temp_rf$train(task, row_ids = train_setsTS_2s[[length(train_setsTS_2s)]])

predictions <- learner_temp_rf$predict(task, row_ids = test_setsTS_2s[[length(test_setsTS_2s)]])
mse_TSholdout_2s_rf <- predictions$score(msr("regr.mse"))

learner_temp_lm$train(task, row_ids = train_setsTS_2s[[length(train_setsTS_2s)]])

predictions <- learner_temp_lm$predict(task, row_ids = test_setsTS_2s[[length(test_setsTS_2s)]])
mse_TSholdout_2s_lm <- predictions$score(msr("regr.mse"))



# Train on the whole observation period and evaluate on the eighth and ninth seasons
# to obtain proxies for the true performance metric values:

train_task <- as_task_regr(data_train, target = "empl_rate")
learner_temp_rf$train(train_task)
learner_temp_lm$train(train_task)


data_test_1 <- data_test[years_test %in% 2020:2021,]

test_task <- as_task_regr(data_test_1, target = "empl_rate")

predictions <- learner_temp_rf$predict(test_task)
mse_true_2020_2021_rf <- predictions$score(msr("regr.mse"))

predictions <- learner_temp_lm$predict(test_task)
mse_true_2020_2021_lm <- predictions$score(msr("regr.mse"))


data_test_2 <- data_test[years_test %in% 2022:2023,]

test_task <- as_task_regr(data_test_2, target = "empl_rate")

predictions <- learner_temp_rf$predict(test_task)
mse_true_2022_2023_rf <- predictions$score(msr("regr.mse"))

predictions <- learner_temp_lm$predict(test_task)
mse_true_2022_2023_lm <- predictions$score(msr("regr.mse"))




# Combine the results in a matrix:

res_mat <- rbind(c(mse_cv_lm,
  mse_TScv_1s_lm,
  mse_TSholdout_1s_lm,
  mse_true_2020_2021_lm,
  mse_TScv_2s_lm,
  mse_TSholdout_2s_lm,
  mse_true_2022_2023_lm),
  c(mse_cv_rf,
    mse_TScv_1s_rf,
    mse_TSholdout_1s_rf,
    mse_true_2020_2021_rf,
    mse_TScv_2s_rf,
    mse_TSholdout_2s_rf,
    mse_true_2022_2023_rf))

colnames(res_mat) <- c("CV", "CV1s", "OOS1s", "S_1post", "CV2s", "OOS2s", "S_2post")
rownames(res_mat) <- c("linear models", "random forests")

res_mat


# Make LaTeX code for Table 2 and save it:


fileConn <- file("./concdrift/results/table/Tab2.tex")
all_content <- c(
  "\\begin{table}[ht]",
  "\\centering",
  "\\caption{Real data analysis of concept drift: estimated MSE values and proxies for the true MSE values in the eighth ({\\rcode{S\\_1post}}) and ninth ({\\rcode{S\\_2post}}) seasons. {\\rcode{CV1s}} and {\\rcode{CV2s}} represent time-series CV without and with a buffer zone. {\\rcode{OOS1s}} and {\\rcode{OOS2s}} stand for out-of-sample validation without and with a buffer zone.}",
  "\\begin{tabular}{rrrrrrr}",
  "  \\hline",
  "  CV & CV1s & OOS1s & S\\_1post & CV2s & OOS2s & S\\_2post \\\\",
  "  \\hline",
  "  \\multicolumn{7}{c}{linear models} \\\\",
  "  \\hline",
  paste("  ", paste(format(round(res_mat[1,], 1), nsmall=1), collapse=" & "), " \\\\"),
  "  \\hline",
  "  \\multicolumn{7}{c}{random forests} \\\\",
  "  \\hline",
  paste("  ", paste(format(round(res_mat[2,], 1), nsmall=1), collapse=" & "), " \\\\"),
  "  \\hline",
  "\\end{tabular}",
  "\\label{tab:concdriftrealdatamse}",
  "\\end{table}"
)
writeLines(paste(all_content, collapse="\n"), fileConn)
close(fileConn)
