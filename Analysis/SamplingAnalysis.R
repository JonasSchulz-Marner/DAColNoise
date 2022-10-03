source("DAColNoise.R")

library(randomForest)
library(caret)
library(smotefamily)
library(pROC)
library(ggplot2)
library(PRROC)
library(mltools)
require(caTools)

### Aufbau ###
# - Einlesen aller Datensätze
# - Data Augmentation (SMOTE, ADASYN, White Noise, Colored Noise)
# - prediction errors bestimmen mit ML-Modellen
# - Signifikanz ermitteln


### Datasets ###

# NAFLD
dataset = na.omit(read.csv("/Users/jonas/Library/Mobile Documents/com~apple~CloudDocs/Documents/Uni/Masterarbeit/data/NAFLD_AFL_clean.csv", sep = ";", dec = ",", header = TRUE))
columns = c(2:10)
# column names: "N_AFLD", "Geschlecht", "Alter", "M30", "M65", "Adiponektin", "TNFa", "AST", "ALT", "ALT_AST_ratio"
violet_noise <- c("M30", "TNFa", "AST")
blue_noise <- c()
white_noise <- c("Alter", "Adiponektin")
pink_noise <- c("M65", "ALT")
red_noise <- c("Geschlecht", "ALT_AST_ratio")
dependent_variable = c(1)
balance_class = c("1")

# haberman
dataset = na.omit(read.csv("/Users/jonas/Library/Mobile Documents/com~apple~CloudDocs/Documents/Uni/Masterarbeit/data/haberman.data", sep = ",", header = FALSE))
columns = c(1:3)
# column names: "V1", "V2", "V3", "V4"
violet_noise <- c()
blue_noise <- c("V2")
white_noise <- c("V1")
pink_noise <- c()
red_noise <- c("V3")
dependent_variable = c(4)
balance_class = c("1")
dataset[,4] <- dataset[,4]-1

# sobar-72
dataset = na.omit(read.csv("/Users/jonas/Library/Mobile Documents/com~apple~CloudDocs/Documents/Uni/Masterarbeit/data/sobar-72.csv", sep = ",", header = TRUE))
columns = c(1:19)
# column names: "behavior_sexualRisk", "behavior_eating", "behavior_personalHygine", "intention_aggregation"
# "intention_commitment", "attitude_consistency", "attitude_spontaneity", "norm_significantPerson"
# "norm_fulfillment", "perception_vulnerability", "perception_severity", "motivation_strength"       
# "motivation_willingness", "socialSupport_emotionality", "socialSupport_appreciation"
# "socialSupport_instrumental", "empowerment_knowledge", "empowerment_abilities"     
# "empowerment_desires", "ca_cervix"
violet_noise <- c("intention_commitment", "attitude_consistency", "motivation_willingness", "empowerment_abilities")
blue_noise <- c("intention_aggregation", "norm_fulfillment", "perception_severity", "socialSupport_instrumental")
white_noise <- c("attitude_spontaneity", "perception_vulnerability", "socialSupport_appreciation", "empowerment_desires")
pink_noise <- c("behavior_sexualRisk", "behavior_personalHygine", "norm_significantPerson")
red_noise <- c("behavior_eating", "motivation_strength", "socialSupport_emotionality", "empowerment_knowledge")
dependent_variable = c(20)
balance_class = c("1")

# fertility
dataset = na.omit(read.csv("/Users/jonas/Library/Mobile Documents/com~apple~CloudDocs/Documents/Uni/Masterarbeit/data/fertility_Diagnosis.txt", sep = ",", dec = ".", header = FALSE))
dataset[,10] <- ifelse(dataset[,10] == "N", 1, 0)
columns = c(1:9)
# column names: "V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10"
violet_noise <- c("V7", "V5")
blue_noise <- c("V2", "V9")
white_noise <- c("V1")
pink_noise <- c("V4", "V6")
red_noise <- c("V3", "V8")
dependent_variable = c(10)
balance_class = c("0")

# risk cancer CCRF
dataset = na.omit(read.csv("/Users/jonas/Library/Mobile Documents/com~apple~CloudDocs/Documents/Uni/Masterarbeit/data/risk_factors_cervical_cancer_clean.csv", sep = ";", dec = ".", header = TRUE))
columns = c(1:7)
# column names: "Age", "Number_of_sexual_partners", "First_sexual_intercourse", "Num_of_pregnancies",       
# "Smokes", "Smokes_years", "Smokes_packs_year", "Dx_Cancer"                
violet_noise <- c("Smokes_packs_year", "Smokes")
blue_noise <- c("Num_of_pregnancies", "Age")
white_noise <- c("Smokes_years")
pink_noise <- c("Number_of_sexual_partners")
red_noise <- c("First_sexual_intercourse")
dependent_variable = c(8)
balance_class = c("1")

# wpbc
dataset = na.omit(read.csv("/Users/jonas/Library/Mobile Documents/com~apple~CloudDocs/Documents/Uni/Masterarbeit/data/wpbc.data.txt", sep = ",", dec = ".", header = FALSE))
dataset[,2] <- ifelse(dataset[,2] == "N", 1, 0)
columns = c(1,3:34)
# column names: "V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15",
# "V16", "V17", "V18", "V19", "V20", "V21", "V22", "V23", "V24", "V25", "V26", "V27", "V28", "V29", "V30",
# "V31", "V32", "V33", "V34"
violet_noise <- c("V7", "V5", "V10", "V12", "V18", "V26", "V32")
blue_noise <- c("V2", "V9", "V11", "V15", "V27", "V29", "V33")
white_noise <- c("V1","V19", "V20", "V21", "V17", "V28", "V34")
pink_noise <- c("V4", "V6", "V23", "V13", "V22", "V30", "V31")
red_noise <- c("V3", "V8", "V14", "V16", "V24", "V25")
dependent_variable = c(2)
balance_class = c("0")
dataset <- dataset[dataset$V35!="?",]
dataset$V35 <- as.numeric(dataset$V35)

# wdbc
dataset = na.omit(read.csv("/Users/jonas/Library/Mobile Documents/com~apple~CloudDocs/Documents/Uni/Masterarbeit/data/wdbc.data.txt", sep = ",", dec = ".", header = FALSE))
dataset[,2] <- ifelse(dataset[,2] == "B", 1, 0)
columns = c(1,3:32)
# column names: "V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15",
# "V16", "V17", "V18", "V19", "V20", "V21", "V22", "V23", "V24", "V25", "V26", "V27", "V28", "V29", "V30",
# "V31", "V32"
violet_noise <- c("V7", "V5", "V10", "V12", "V18", "V26", "V32")
blue_noise <- c("V9", "V11", "V15", "V27", "V29")
white_noise <- c("V1","V19", "V20", "V21", "V17", "V28")
pink_noise <- c("V4", "V6", "V23", "V13", "V22", "V30", "V31")
red_noise <- c("V3", "V8", "V14", "V16", "V24", "V25")
dependent_variable = c(2)
balance_class = c("0")

# CTG
dataset = na.omit(read.csv("/Users/jonas/Library/Mobile Documents/com~apple~CloudDocs/Documents/Uni/Masterarbeit/data/CTG_new.csv", sep = ";", dec = ",", header = TRUE))
dataset <- dataset[dataset$NSP != 2,]
dataset[,23] <- ifelse(dataset[,23] == "3", 1, 0)
columns = c(1:22)
# column names: "LB", "AC",  "FM", "UC", "DL", "DS", "DP", "ASTV", "MSTV", "ALTV", "MLTV", "Width", "Min", "Max"
# "Nmax", "Nzeros", "Mode", "Mean", "Median", "Variance", "Tendency", "CLASS", "NSP" 
violet_noise <- c("AC", "DS", "Min", "Nmax")
blue_noise <- c("LB", "DP", "MSTV", "Median", "Variance")
white_noise <- c("DL", "Width", "Nzeros")
pink_noise <- c("FM", "ASTV", "MLTV", "Mode", "Tendency")
red_noise <- c("UC", "ALTV", "Max", "Mean", "CLASS")
dependent_variable = c(23)
balance_class = c("1")

# drug consumption heroin
drug = na.omit(read.csv("/Users/jonas/Library/Mobile Documents/com~apple~CloudDocs/Documents/Uni/Masterarbeit/data/drug_consumption.csv", sep = ",", dec = ".", header = FALSE))
#V3 = gender, f=0.48246, m=0.-48246
drug <- drug[drug[,"V3"]==0.48246,] #select only female
drug = drug[,-c(1,3)] #delete id and gender
# select drug, e.g., V24=Heroin
drug$V24 <- ifelse(drug$V24 == "CL0", 0, 1)
# column names: "V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15",
# "V16", "V17", "V18", "V19", "V20", "V21", "V22", "V23", "V24", "V25", "V26", "V27", "V28", "V29", "V30",
# "V31", "V32"
violet_noise <- c("V7", "V5", "V11")
blue_noise <- c("V2", "V9")
white_noise <- c("V10", "V12")
pink_noise <- c("V4", "V6")
red_noise <- c("V13", "V8")
dataset <- drug[,c(1:11,22)]
columns = c(1:11)
dependent_variable = c(12)
balance_class = c("1")

# hcv hier wird nur binäre Entschiedung zweischen Blood Donor und Hepatits
dataset = na.omit(read.csv("/Users/jonas/Library/Mobile Documents/com~apple~CloudDocs/Documents/Uni/Masterarbeit/data/hcvdat0.csv", sep = ",", dec = ".", header = TRUE))
dataset[dataset[,"Sex"]=="m",4] <- 0
dataset[dataset[,"Sex"]=="f",4] <- 1
dataset[,"Sex"] <- as.numeric(dataset[,"Sex"])
dataset[dataset[,"Category"]=="0=Blood Donor","Category"] <- 0
dataset[dataset[,"Category"]=="1=Hepatitis","Category"] <- 1
dataset <- dataset[,-c(1)]
dataset <- dataset[dataset$Category != "0s=suspect Blood Donor",]
dataset <- dataset[dataset$Category != "2=Fibrosis",]
dataset <- dataset[dataset$Category != "3=Cirrhosis",]
dataset$Category <- as.numeric(dataset$Category)
# column names: "Category", "Age", "Sex", "ALB", "ALP", "ALT", "AST", "BIL", "CHE", "CHOL", "CREA", "GGT", "PROT"
violet_noise <- c("Age", "AST", "GGT")
blue_noise <- c("ALP", "BIL")
white_noise <- c("ALT", "CREA")
pink_noise <- c("CHE", "Sex", "PROT")
red_noise <- c("ALB", "CHOL")
columns <- c(2:13)
dependent_variable = c(1)
balance_class = c("1")




###################### Analysis ##########################

seed <- 654341

#amount of new data (2,3,4)
amount_new_data <- 3
if (amount_new_data == 2) {
  new_datapoints <- "double"
} else if (amount_new_data == 3) {
  new_datapoints <- "triple"
} else if (amount_new_data == 4) {
  new_datapoints <- "quadruple"
}

# function that generates the indices, so that the classes in the test set have the same proportion as in train set
stratified_sample <- function(var, p) {
  obs  <- seq_along(var)
  groups <- unique(var)
  indices <- numeric()
  for(g in groups) {
    indices <- c(indices, sample(obs[var==g], floor(sum(var==g)*p)))
  }
  return(indices)
}


misClasificError_lr_sample <- NULL
misClasificError_lr_no_sample <- NULL
misClasificError_rf_sample <- NULL
misClasificError_rf_no_sample <- NULL

auc_lr_sample <- NULL
auc_lr_no_sample <- NULL
auc_rf_sample <- NULL
auc_rf_no_sample <- NULL

aupr_lr_sample <- NULL
aupr_lr_no_sample <- NULL
aupr_rf_sample <- NULL
aupr_rf_no_sample <- NULL

mcc_lr_sample <- NULL
mcc_lr_no_sample <- NULL
mcc_rf_sample <- NULL
mcc_rf_no_sample <- NULL


for (i in 1:50){
  
  ## set the seed to make the partition reproducible
  set.seed(seed+i)
  train_ind <- stratified_sample(dataset[,dependent_variable], 0.7)
  
  train <- dataset[train_ind, ]
  test <- dataset[-train_ind, ]
  
  sample <- DAColNoise(same_noise = FALSE,     # "same" = TRUE or "different" = FALSE
                       tbl_columns_selected = columns,   # Indices of the selected columns
                       dataset = train, 
                       tbl_classification_columns_selected = dependent_variable,   # Index of the column of the dependent variable
                       balanced_classes = balance_class,   # Name of the classes that should be balanced in vector 
                       balance_process = "balancing_with_noise",    # "oversampling" or "balancing_with_noise"
                       direction_of_generating = "vertical",    # "vertical" or "horizontal"
                       seed = seed+i,   # any seed 
                       noise_color = -1.5,   # -1.5, -1, 0, 1, 1.5
                       violet_noise = violet_noise,  # names of the columns in a vector
                       blue_noise = blue_noise,  # names of the columns in a vector
                       white_noise = white_noise,   # names of the columns in a vector
                       pink_noise = pink_noise,  # names of the columns in a vector
                       red_noise = red_noise,   # names of the columns in a vector
                       new_datapoints = new_datapoints,   # "double", "triple", "quadruple" or "individual" 
                       amount_new_datapoints = 100,  # amount of new datapoints, if individual
                       balancing = TRUE,   # "balancing" = TRUE or "no_balancing" = FALSE
                       replacement = "replacement",
                       rounded_variables = c(),  # names of the columns in a vector
                       negative_values = c())   # names of the columns in a vector
  
  no_sample <- DAColNoise(same_noise = FALSE,     # "same" = TRUE or "different" = FALSE
                     tbl_columns_selected = columns,   # Indices of the selected columns
                     dataset = train, 
                     tbl_classification_columns_selected = dependent_variable,   # Index of the column of the dependent variable
                     balanced_classes = balance_class,   # Name of the classes that should be balanced in vector 
                     balance_process = "balancing_with_noise",    # "oversampling" or "balancing_with_noise"
                     direction_of_generating = "vertical",    # "vertical" or "horizontal"
                     seed = seed+i,   # any seed 
                     noise_color = -1,   # -1.5, -1, 0, 1, 1.5
                     violet_noise = violet_noise,  # names of the columns in a vector
                     blue_noise = blue_noise,  # names of the columns in a vector
                     white_noise = white_noise,   # names of the columns in a vector
                     pink_noise = pink_noise,  # names of the columns in a vector
                     red_noise = red_noise,   # names of the columns in a vector
                     new_datapoints = new_datapoints,   # "double", "triple", "quadruple" or "individual" 
                     amount_new_datapoints = 100,  # amount of new datapoints, if individual
                     balancing = TRUE,   # "balancing" = TRUE or "no_balancing" = FALSE
                     replacement = "no_replacement",
                     rounded_variables = c(),  # names of the columns in a vector
                     negative_values = c())

  #### Logistic Regression #####
  
  # Train model #
  model_sample <- glm(as.formula(paste(names(sample)[dependent_variable], "~.")), family = binomial, data = sample)
  model_no_sample <- glm(as.formula(paste(names(no_sample)[dependent_variable], "~.")), family = binomial, data = no_sample)
  
  # predict # # AUC, PR, MCC #
  
  fitted_results_sample <- predict(model_sample,newdata=subset(test,select=c(-dependent_variable)),type='response')
  fitted_results_sample <- ifelse(fitted_results_sample > 0.5,1,0)
  misClasificError_lr_sample[i] <- mean(fitted_results_sample != test[,dependent_variable])
  roc_sample_lr <- tryCatch({pROC::roc(fitted_results_sample, test[,dependent_variable])},
                            error=function(cond){return(NA)})
  auc_lr_sample[i] <- tryCatch({pROC::auc(roc_sample_lr)[1]}, error=function(cond){return(NA)})
  aupr_lr_sample[i] <- PRROC::pr.curve(as.factor(fitted_results_sample), test[,dependent_variable])$auc.integral
  mcc_lr_sample[i] <- mltools::mcc(as.factor(fitted_results_sample), as.factor(test[,dependent_variable]))
  
  
  fitted_results_no_sample <- predict(model_no_sample,newdata=subset(test,select=c(-dependent_variable)),type='response')
  fitted_results_no_sample <- ifelse(fitted_results_no_sample > 0.5,1,0)
  misClasificError_lr_no_sample[i] <- mean(fitted_results_no_sample != test[,dependent_variable])
  roc_no_sample_lr <- tryCatch({pROC::roc(fitted_results_no_sample, test[,dependent_variable])},
                            error=function(cond){return(NA)})
  auc_lr_no_sample[i] <- tryCatch({pROC::auc(roc_no_sample_lr)[1]}, error=function(cond){return(NA)})
  aupr_lr_no_sample[i] <- PRROC::pr.curve(as.factor(fitted_results_no_sample), test[,dependent_variable])$auc.integral
  mcc_lr_no_sample[i] <- mltools::mcc(as.factor(fitted_results_no_sample), as.factor(test[,dependent_variable]))
  
  
  
   
  
  #### Random Forest #####
  
  # convert response variable to factor
  sample <- as.data.frame(sample)
  sample[,dependent_variable] <- as.factor(sample[,dependent_variable])
  rf_sample <- randomForest(as.formula(paste(names(sample)[dependent_variable], "~.")), data = sample, ntree=100, mtry=2, importance=TRUE)
  
  no_sample <- as.data.frame(no_sample)
  no_sample[,dependent_variable] <- as.factor(no_sample[,dependent_variable])
  rf_no_sample <- randomForest(as.formula(paste(names(no_sample)[dependent_variable], "~.")), data = no_sample, ntree=100, mtry=2, importance=TRUE)
  
  # predict and quality measurements # # AUC, PR, MCC #
  predict_sample <- predict(rf_sample, test[,-dependent_variable])
  misClasificError_rf_sample[i] <- mean(predict_sample != test[,dependent_variable])
  roc_sample_rf <- tryCatch({pROC::roc(predict_sample, test[,dependent_variable])},
                            error=function(cond){return(NA)})
  auc_rf_sample[i] <- tryCatch({pROC::auc(roc_sample_rf)[1]}, error=function(cond){return(NA)})
  aupr_rf_sample[i] <- PRROC::pr.curve(predict_sample, test[,dependent_variable])$auc.integral
  mcc_rf_sample[i] <- mltools::mcc(predict_sample, as.factor(test[,dependent_variable]))
  
  predict_no_sample <- predict(rf_no_sample, test[,-dependent_variable])
  misClasificError_rf_no_sample[i] <- mean(predict_no_sample != test[,dependent_variable])
  roc_no_sample_rf <- tryCatch({pROC::roc(predict_no_sample, test[,dependent_variable])},
                          error=function(cond){return(NA)})
  auc_rf_no_sample[i] <- tryCatch({pROC::auc(roc_no_sample_rf)[1]}, error=function(cond){return(NA)})
  aupr_rf_no_sample[i] <- PRROC::pr.curve(predict_no_sample, test[,dependent_variable])$auc.integral
  mcc_rf_no_sample[i] <- mltools::mcc(predict_no_sample, as.factor(test[,dependent_variable]))
  
  
}




### Plot results ###

# accuracy

models <- c(rep("LR", 2*50), rep("RF", 2*50))
method <- c(rep(c(rep("With Sampling",50), rep("No Sampling",50)),2))
value <- c(1-misClasificError_lr_sample, 1-misClasificError_lr_no_sample,
           1-misClasificError_rf_sample, 1-misClasificError_rf_no_sample)
accuracies <- data.frame(model = models,
                         method = method,
                         value = value)
accuracies$method <- factor(accuracies$method, levels = unique(accuracies$method))

# Plot
pred_error <- ggplot2::ggplot(accuracies, aes(fill=models, y=value, x=method)) +
  coord_cartesian(ylim=c(0.95,1)) +
  geom_boxplot() + 
  ggtitle("Accuracy") + theme_minimal() + 
  scale_fill_manual(values=c("#A8DADC", "#457B9D")) + labs(x="")


# AUC

models <- c(rep("LR", 2*50), rep("RF", 2*50))
method <- c(rep(c(rep("With Sampling",50), rep("No Sampling",50)),2))
value <- c(auc_lr_sample, auc_lr_no_sample,
           auc_rf_sample, auc_rf_no_sample)
auc <- data.frame(model = models,
                  method = method,
                  value = na.omit(value))
auc$method <- factor(auc$method, levels = unique(auc$method))

# Plot 
auc_plot <- ggplot2::ggplot(auc, aes(fill=models, y=value, x=method)) + 
  geom_boxplot() + coord_cartesian(ylim=c(0.7,1)) +
  ggtitle("Area under Curve") + theme_minimal() + 
  scale_fill_manual(values=c("#A8DADC", "#457B9D")) + labs(x="")


# AUPR

models <- c(rep("LR", 2*50), rep("RF", 2*50))
method <- c(rep(c(rep("With Sampling",50), rep("No Sampling",50)),2))
value <- c(aupr_lr_sample, aupr_lr_no_sample,
           aupr_rf_sample, aupr_rf_no_sample)
aupr <- data.frame(model = models,
                   method = method,
                   value = value)
aupr$method <- factor(aupr$method, levels = unique(aupr$method))

# Plot 
aupr_plot <- ggplot2::ggplot(aupr, aes(fill=models, y=value, x=method)) + 
  geom_boxplot() + coord_cartesian(ylim=c(0.965,0.975)) +
  ggtitle("Area under Precision Recall") + theme_minimal() + 
  scale_fill_manual(values=c("#A8DADC", "#457B9D")) + labs(x="")



# MCC

models <- c(rep("LR", 2*50), rep("RF", 2*50))
method <- c(rep(c(rep("With Sampling",50), rep("No Sampling",50)),2))
value <- c(mcc_lr_sample, mcc_lr_no_sample,
           mcc_rf_sample, mcc_rf_no_sample)
mcc <- data.frame(model = models,
                  method = method,
                  value = value)
mcc$method <- factor(mcc$method, levels = unique(mcc$method))

# Plot 
mcc_plot <- ggplot2::ggplot(mcc, aes(fill=models, y=value, x=method)) + 
  geom_boxplot() + coord_cartesian(ylim=c(0.25,1)) +
  ggtitle("Matthew's correlation coefficient") + theme_minimal() + 
  scale_fill_manual(values=c("#A8DADC", "#457B9D")) + labs(x="")


# plot all
cowplot::plot_grid(pred_error, auc_plot, aupr_plot, mcc_plot, ncol = 2, nrow = 2)



# check whether there is a significant difference in the mean of no Data Augmentation to any Color of Noise
### Significance ###

# accuracy
p_values <- c(t.test(1-misClasificError_lr_sample, 1-misClasificError_lr_no_sample)$p.value,1,
              t.test(1-misClasificError_rf_sample, 1-misClasificError_rf_no_sample)$p.value,1)
means = c(1-mean(na.omit(misClasificError_lr_sample)), 1-mean(na.omit(misClasificError_lr_no_sample)), 
          1-mean(na.omit(misClasificError_rf_sample)), 1-mean(na.omit(misClasificError_rf_no_sample)))
models <- c(rep("LR", 2), rep("RF", 2))
method <- c(rep(c("With Sampling", "No Sampling"),2))
accuracy_p_values <- data.frame(model = models,
                                method = method,
                                p_value = p_values,
                                mean = round(means,3),
                                significance = rep("",4))
for (index in 1:4) {
  if (accuracy_p_values$p_value[index] < 0.1) {accuracy_p_values$significance[index] = "*"}
  if (accuracy_p_values$p_value[index] < 0.05 ) {accuracy_p_values$significance[index] = "**"}
  if (accuracy_p_values$p_value[index] < 0.01 ) {accuracy_p_values$significance[index] = "***"}
}

accuracy_p_values = accuracy_p_values[,c(1,2,4,5)]


# AUC
p_values <- c(t.test(auc_lr_sample, auc_lr_no_sample)$p.value,1,
              t.test(auc_rf_sample, auc_rf_no_sample)$p.value,1)
means = c(mean(na.omit(auc_lr_sample)), mean(na.omit(auc_lr_no_sample)),
          mean(na.omit(auc_rf_sample)), mean(na.omit(auc_rf_no_sample)))
models <- c(rep("LR", 2), rep("RF", 2))
method <- c(rep(c("With Sampling", "No Sampling"),2))
auc_p_values <- data.frame(model = models,
                           method = method,
                           p_values = p_values,
                           mean = round(means,3),
                           significance = rep("",4))
for (index in 1:4) {
  if (auc_p_values$p_value[index] < 0.1) {auc_p_values$significance[index] = "*"}
  if (auc_p_values$p_value[index] < 0.05) {auc_p_values$significance[index] = "**"}
  if (auc_p_values$p_value[index] < 0.01) {auc_p_values$significance[index] = "***"}
}

auc_p_values = auc_p_values[,c(1,2,4,5)]


# AUPR
p_values <- c(t.test(aupr_lr_sample, aupr_lr_no_sample)$p.value,1,
              t.test(aupr_rf_sample, aupr_rf_no_sample)$p.value,1)
means = c(mean(na.omit(aupr_lr_sample)), mean(na.omit(aupr_lr_no_sample)),
          mean(na.omit(aupr_rf_sample)), mean(na.omit(aupr_rf_no_sample)))
models <- c(rep("LR", 2), rep("RF", 2))
method <- c(rep(c("With Sampling", "No Sampling"),2))
aupr_p_values <- data.frame(model = models,
                           method = method,
                           p_values = p_values,
                           mean = round(means,3),
                           significance = rep("",4))
for (index in 1:4) {
  if (aupr_p_values$p_value[index] < 0.1) {aupr_p_values$significance[index] = "*"}
  if (aupr_p_values$p_value[index] < 0.05) {aupr_p_values$significance[index] = "**"}
  if (aupr_p_values$p_value[index] < 0.01) {aupr_p_values$significance[index] = "***"}
}

aupr_p_values = aupr_p_values[,c(1,2,4,5)]


# MCC
p_values <- c(t.test(mcc_lr_sample, mcc_lr_no_sample)$p.value,1,
              t.test(mcc_rf_sample, mcc_rf_no_sample)$p.value,1)
means = c(mean(na.omit(mcc_lr_sample)), mean(na.omit(mcc_lr_no_sample)),
          mean(na.omit(mcc_rf_sample)), mean(na.omit(mcc_rf_no_sample)))
models <- c(rep("LR", 2), rep("RF", 2))
method <- c(rep(c("With Sampling", "No Sampling"),2))
mcc_p_values <- data.frame(model = models,
                           method = method,
                           p_values = p_values,
                           mean = round(means,3),
                           significance = rep("",4))
for (index in 1:4) {
  if (mcc_p_values$p_value[index] < 0.1) {mcc_p_values$significance[index] = "*"}
  if (mcc_p_values$p_value[index] < 0.05) {mcc_p_values$significance[index] = "**"}
  if (mcc_p_values$p_value[index] < 0.01) {mcc_p_values$significance[index] = "***"}
}

mcc_p_values = mcc_p_values[,c(1,2,4,5)]



# print all significance tables
print(accuracy_p_values)
print(auc_p_values)
print(aupr_p_values)
print(mcc_p_values)


string_1 = ""
string_3 = ""

for (i in 1:nrow(accuracy_p_values)) {
  for (j in 1:ncol(accuracy_p_values)){
    if (j != 1) {string_1 = paste(string_1, " & ", accuracy_p_values[i,j])}
    if (j == 1) {string_1 = paste(string_1, accuracy_p_values[i,j])}
  }

  for (j in 1:ncol(accuracy_p_values)){
    string_1 = paste(string_1, " & ", auc_p_values[i,j])
    
  }
  string_1 = paste(string_1, " \\ ")
  
  for (j in 1:ncol(accuracy_p_values)){
    if (j != 1) {string_3 = paste(string_3, " & ", aupr_p_values[i,j])}
    if (j == 1) {string_3 = paste(string_3, aupr_p_values[i,j])}
  }

  for (j in 1:ncol(accuracy_p_values)){
    string_3 = paste(string_3, " & ", mcc_p_values[i,j])
  }
  string_3 = paste(string_3, " \\ ")
}

string = paste(string_1, "\\hline \\hline  \textbf{model} & \textbf{method} & \textbf{AUPR} &  & \textbf{model} & \textbf{method} & \textbf{MCC} & \\ \\hline", string_3,  "\\hline")
print(string)

