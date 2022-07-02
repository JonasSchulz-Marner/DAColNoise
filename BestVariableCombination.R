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
dataset = read.csv("/Users/jonas/Library/Mobile Documents/com~apple~CloudDocs/Documents/Uni/Masterarbeit/data/NAFLD_AFL_clean.csv", sep = ";", dec = ",", header = TRUE)
columns = c(2:10)
# column names: "N_AFLD", "Geschlecht", "Alter", "M30", "M65", "Adiponektin", "TNFa", "AST", "ALT", "ALT_AST_ratio"
dependent_variable = c(1)
balance_class = c("1")

# haberman
dataset = read.csv("/Users/jonas/Library/Mobile Documents/com~apple~CloudDocs/Documents/Uni/Masterarbeit/data/haberman.data", sep = ",", header = FALSE)
columns = c(1:3)
# column names: "V1", "V2", "V3", "V4"
dependent_variable = c(4)
balance_class = c("1")
dataset[,4] <- dataset[,4]-1

# sobar-72
dataset = read.csv("/Users/jonas/Library/Mobile Documents/com~apple~CloudDocs/Documents/Uni/Masterarbeit/data/sobar-72.csv", sep = ",", header = TRUE)
columns = c(1:19)
# column names: "behavior_sexualRisk", "behavior_eating", "behavior_personalHygine", "intention_aggregation"
# "intention_commitment", "attitude_consistency", "attitude_spontaneity", "norm_significantPerson"
# "norm_fulfillment", "perception_vulnerability", "perception_severity", "motivation_strength"       
# "motivation_willingness", "socialSupport_emotionality", "socialSupport_appreciation"
# "socialSupport_instrumental", "empowerment_knowledge", "empowerment_abilities"     
# "empowerment_desires", "ca_cervix" 
dependent_variable = c(20)
balance_class = c("1")

# fertility
dataset = read.csv("/Users/jonas/Library/Mobile Documents/com~apple~CloudDocs/Documents/Uni/Masterarbeit/data/fertility_Diagnosis.txt", sep = ",", dec = ".", header = FALSE)
dataset[,10] <- ifelse(dataset[,10] == "N", 1, 0)
columns = c(1:9)
# column names: "V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10"
dependent_variable = c(10)
balance_class = c("0")

# risk cancer
dataset = na.omit(read.csv("/Users/jonas/Library/Mobile Documents/com~apple~CloudDocs/Documents/Uni/Masterarbeit/data/risk_factors_cervical_cancer_clean.csv", sep = ";", dec = ".", header = TRUE))
columns = c(1:7)
# column names: "Age", "Number_of_sexual_partners", "First_sexual_intercourse", "Num_of_pregnancies",       
# "Smokes", "Smokes_years", "Smokes_packs_year", "Dx_Cancer"                
violet <- c("Smokes_packs_year", "Smokes")
blue <- c("Num_of_pregnancies", "Age")
white <- c("Smokes_years")
pink <- c("Number_of_sexual_partners")
red <- c("First_sexual_intercourse")
dependent_variable = c(8)
balance_class = c("1")

# wpbc
dataset = na.omit(read.csv("/Users/jonas/Library/Mobile Documents/com~apple~CloudDocs/Documents/Uni/Masterarbeit/data/wpbc.data.txt", sep = ",", dec = ".", header = FALSE))
dataset[,2] <- ifelse(dataset[,2] == "N", 1, 0)
columns = c(1,3:34)
# column names: "V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15",
# "V16", "V17", "V18", "V19", "V20", "V21", "V22", "V23", "V24", "V25", "V26", "V27", "V28", "V29", "V30",
# "V31", "V32", "V33", "V34"
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
violet <- c("V7", "V5", "V10", "V12", "V18", "V26", "V32")
blue <- c("V9", "V11", "V15", "V27", "V29")
white <- c("V1","V19", "V20", "V21", "V17", "V28")
pink <- c("V4", "V6", "V23", "V13", "V22", "V30", "V31")
red <- c("V3", "V8", "V14", "V16", "V24", "V25")
dependent_variable = c(2)
balance_class = c("0")

# drug consumption
drug = na.omit(read.csv("/Users/jonas/Library/Mobile Documents/com~apple~CloudDocs/Documents/Uni/Masterarbeit/data/drug_consumption.csv", sep = ",", dec = ".", header = FALSE))
#V3 = gender, f=0.48246, m=0.-48246
drug <- drug[drug[,"V3"]==0.48246,] #select only female
drug = drug[,-c(1,3)] #delete id and gender
# select drug, e.g., V24=Heroin
drug$V24 <- ifelse(drug$V24 == "CL0", 0, 1)
# column names: "V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15",
# "V16", "V17", "V18", "V19", "V20", "V21", "V22", "V23", "V24", "V25", "V26", "V27", "V28", "V29", "V30",
# "V31", "V32"
violet <- c("V7", "V5", "V11")
blue <- c("V2", "V9")
white <- c("V10", "V12")
pink <- c("V4", "V6")
red <- c("V13", "V8")
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
violet <- c("Age", "AST", "GGT")
blue <- c("ALP", "BIL")
white <- c("ALT", "CREA")
pink <- c("CHE", "Sex", "PROT")
red <- c("ALB", "CHOL")
columns <- c(2:13)
dependent_variable = c(1)
balance_class = c("1")

### Analysis ###

seed <- 54309

# NULLen
results <- list()
misClasificError_lr <- NULL
auc_lr <- NULL
aupr_lr <- NULL
mcc_lr <- NULL

misClasificError_rf <- NULL
auc_rf <- NULL
aupr_rf <- NULL
mcc_rf <- NULL

misClasificError_lr_wn <- NULL
auc_lr_wn <- NULL
aupr_lr_wn <- NULL
mcc_lr_wn <- NULL

misClasificError_rf_wn <- NULL
auc_rf_wn <- NULL
aupr_rf_wn <- NULL
mcc_rf_wn <- NULL


#amount of new data (2,3,4)
amount_new_data <- 2
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


# train and test data
#smp_size <- floor(0.7 * nrow(dataset))


### White Noise ###

for (i in 1:50){
  set.seed(seed+i)
  train_ind <- stratified_sample(dataset[,dependent_variable], 0.7)
  
  train <- dataset[train_ind, ]
  train[,dependent_variable] <- as.numeric(train[,dependent_variable])
  test <- dataset[-train_ind, ]
  
  
  data <- DAColNoise(same_noise = TRUE,     # "same" = TRUE or "different" = FALSE
                     tbl_columns_selected = columns,   # Indices of the selected columns
                     dataset = train, 
                     tbl_classification_columns_selected = dependent_variable,   # Index of the column of the dependent variable
                     balanced_classes = balance_class,   # Name of the classes that should be balanced in vector 
                     balance_process = "balancing_with_noise",    # "oversampling" or "balancing_with_noise"
                     direction_of_generating = "vertical",    # "vertical" or "horizontal"
                     seed = (seed+i),   # any seed 
                     noise_color = 0,   # -1.5, -1, 0, 1, 1.5
                     violet_noise = violet,  # names of the columns in a vector
                     blue_noise = blue,  # names of the columns in a vector
                     white_noise = white,   # names of the columns in a vector
                     pink_noise = pink,  # names of the columns in a vector
                     red_noise = red,   # names of the columns in a vector
                     new_datapoints = new_datapoints,   # "double", "triple", "quadruple" or "individual" 
                     amount_new_datapoints = 100,  # amount of new datapoints, if individual
                     balancing = TRUE,   # "balancing" = TRUE or "no_balancing" = FALSE
                     replacement = "replacement",
                     rounded_variables = c(),  # names of the columns in a vector
                     negative_values = c())   # names of the columns in a vector
  
  
  #### Logistic Regression #####
  
  # Train model #
  model <- glm(as.formula(paste(names(data)[dependent_variable], "~.")), family = binomial, data = data)
  fitted_results <- predict(model,newdata=subset(test,select=c(-dependent_variable)),type='response')
  fitted_results <- ifelse(fitted_results > 0.5,1,0)
  misClasificError_lr_wn[i] <- mean(fitted_results != test[,dependent_variable])
  roc_lr_wn <- tryCatch({pROC::roc(fitted_results, test[,dependent_variable])},
                        error=function(cond){return(NA)})
  auc_lr_wn[i] <- tryCatch({pROC::auc(roc_lr_wn)[1]}, error=function(cond){return(NA)})
  aupr_lr_wn[i] <- PRROC::pr.curve(as.factor(fitted_results), test[,dependent_variable])$auc.integral
  mcc_lr_wn[i] <- mltools::mcc(as.factor(fitted_results), as.factor(test[,dependent_variable]))
  
  
  #### Random Forest #####
  
  # convert response variable to factor
  train[,dependent_variable] <- as.factor(train[,dependent_variable])
  data <- as.data.frame(data)
  data[,dependent_variable] <- as.factor(data[,dependent_variable])
  rf_wn <- randomForest(as.formula(paste(names(train)[dependent_variable], "~.")), data = train, ntree=100, mtry=2, importance=TRUE)
  
  # predict and quality measurements # # AUC, PR, MCC #
  prediction <- predict(rf_wn, test[,-dependent_variable])
  misClasificError_rf_wn[i] <- mean(prediction != test[,dependent_variable])
  roc_rf_wn <- tryCatch({pROC::roc(prediction, test[,dependent_variable])},
                        error=function(cond){return(NA)})
  auc_rf_wn[i] <- tryCatch({pROC::auc(roc_rf_wn)[1]}, error=function(cond){return(NA)})
  aupr_rf_wn[i] <- PRROC::pr.curve(prediction, test[,dependent_variable])$auc.integral
  mcc_rf_wn[i] <- mltools::mcc(prediction, as.factor(test[,dependent_variable]))

}

measures <- data.frame(measure = c("Accuracy", "AUC", "AUPR", "MCC"),
                       mean_randomForest = c(mean(1-misClasificError_rf_wn), mean(auc_rf_wn), mean(aupr_rf_wn), mean(mcc_rf_wn)),
                       mean_logReg = c(mean(1-misClasificError_lr_wn), mean(auc_lr_wn), mean(aupr_lr_wn), mean(mcc_lr_wn)))
random_assign <- data.frame(names = names(dataset[,columns]),
                            color = rep(3, length(names(dataset[,columns]))))
results[[1]] <- list(random_assign = random_assign, measures = measures, accuracy_rf = 1-misClasificError_rf_wn, auc_rf = auc_rf_wn, aupr_rf = aupr_rf_wn, mcc_rf = mcc_rf_wn,
                        accuracy_lr = 1-misClasificError_lr_wn, auc_lr = auc_lr_wn, aupr_lr = aupr_lr_wn, mcc_lr = mcc_lr_wn)

accuracies_lr_eval <- mean(1-misClasificError_lr_wn)
auc_lr_eval <- mean(auc_lr_wn)
aupr_lr_eval <- mean(aupr_lr_wn)
mcc_lr_eval <- mean(mcc_lr_wn)

accuracies_rf_eval <- mean(1-misClasificError_rf_wn)
auc_rf_eval <- mean(auc_rf_wn)
aupr_rf_eval <- mean(aupr_rf_wn)
mcc_rf_eval <- mean(mcc_rf_wn)


for (perm in 2:200){
  
  random_assign <- data.frame(names = names(dataset[,columns]),
                              color = rep(3, length(names(dataset[,columns]))))
  for (row in 1:nrow(random_assign)) {
    set.seed((seed+row)*perm)
    random_assign$color[row] <- round(runif(1, min = 1, max = 5))
  }
  
  
  violet <- random_assign$names[which(random_assign$color == 1)]
  blue <- random_assign$names[which(random_assign$color == 2)]
  white <- random_assign$names[which(random_assign$color == 3)]
  pink <- random_assign$names[which(random_assign$color == 4)]
  red <- random_assign$names[which(random_assign$color == 5)]

  for (i in 1:50){
    
    ## set the seed to make the partition reproducible
    set.seed((seed+i)*perm)
    train_ind <- stratified_sample(dataset[,dependent_variable], 0.7)
    
    train <- dataset[train_ind, ]
    test <- dataset[-train_ind, ]
    
    data <- DAColNoise(same_noise = FALSE,     # "same" = TRUE or "different" = FALSE
                         tbl_columns_selected = columns,   # Indices of the selected columns
                         dataset = train, 
                         tbl_classification_columns_selected = dependent_variable,   # Index of the column of the dependent variable
                         balanced_classes = balance_class,   # Name of the classes that should be balanced in vector 
                         balance_process = "balancing_with_noise",    # "oversampling" or "balancing_with_noise"
                         direction_of_generating = "vertical",    # "vertical" or "horizontal"
                         seed = (seed+i)*perm,   # any seed 
                         noise_color = -1.5,   # -1.5, -1, 0, 1, 1.5
                         violet_noise = violet,  # names of the columns in a vector
                         blue_noise = blue,  # names of the columns in a vector
                         white_noise = white,   # names of the columns in a vector
                         pink_noise = pink,  # names of the columns in a vector
                         red_noise = red,   # names of the columns in a vector
                         new_datapoints = new_datapoints,   # "double", "triple", "quadruple" or "individual" 
                         amount_new_datapoints = 100,  # amount of new datapoints, if individual
                         balancing = TRUE,   # "balancing" = TRUE or "no_balancing" = FALSE
                         replacement = "replacement",
                         rounded_variables = c(),  # names of the columns in a vector
                         negative_values = c())   # names of the columns in a vector
   
    
     #### Logistic Regression #####
    
    # Train model #
    model <- glm(as.formula(paste(names(data)[dependent_variable], "~.")), family = binomial, data = data)
    fitted_results <- predict(model,newdata=subset(test,select=c(-dependent_variable)),type='response')
    fitted_results <- ifelse(fitted_results > 0.5,1,0)
    misClasificError_lr[i] <- mean(fitted_results != test[,dependent_variable])
    roc_lr <- tryCatch({pROC::roc(fitted_results, test[,dependent_variable])},
                          error=function(cond){return(NA)})
    auc_lr[i] <- tryCatch({pROC::auc(roc_lr)[1]}, error=function(cond){return(NA)})
    aupr_lr[i] <- PRROC::pr.curve(as.factor(fitted_results), test[,dependent_variable])$auc.integral
    mcc_lr[i] <- mltools::mcc(as.factor(fitted_results), as.factor(test[,dependent_variable]))
    
    
    #### Random Forest #####
    
    # convert response variable to factor
    train[,dependent_variable] <- as.factor(train[,dependent_variable])
    data <- as.data.frame(data)
    data[,dependent_variable] <- as.factor(data[,dependent_variable])
    rf <- randomForest(as.formula(paste(names(train)[dependent_variable], "~.")), data = train, ntree=100, mtry=2, importance=TRUE)
    
    # predict and quality measurements # # AUC, PR, MCC #
    prediction <- predict(rf, test[,-dependent_variable])
    misClasificError_rf[i] <- mean(prediction != test[,dependent_variable])
    roc_rf <- tryCatch({pROC::roc(prediction, test[,dependent_variable])},
                          error=function(cond){return(NA)})
    auc_rf[i] <- tryCatch({pROC::auc(roc_rf)[1]}, error=function(cond){return(NA)})
    aupr_rf[i] <- PRROC::pr.curve(prediction, test[,dependent_variable])$auc.integral
    mcc_rf[i] <- mltools::mcc(prediction, as.factor(test[,dependent_variable]))
  }
  
  measures <- data.frame(measure = c("Accuracy", "AUC", "AUPR", "MCC"),
                         mean_randomForest = c(mean(1-misClasificError_rf), mean(na.omit(auc_rf)), mean(aupr_rf), mean(mcc_rf)),
                         mean_logReg = c(mean(1-misClasificError_lr), mean(na.omit(auc_lr)), mean(aupr_lr), mean(mcc_lr)))
  results[[perm]] <- list(random_assign = random_assign, measures = measures, accuracy_rf = 1-misClasificError_rf, auc_rf = auc_rf, aupr_rf = aupr_rf, mcc_rf = mcc_rf,
                          accuracy_lr = 1-misClasificError_lr, auc_lr = auc_lr, aupr_lr = aupr_lr, mcc_lr = mcc_lr)
  
  
  ### Evaluation ###
  accuracies_rf_eval <- c(accuracies_rf_eval, results[[perm]][[2]][1,2])
  auc_rf_eval <- c(auc_rf_eval, results[[perm]][[2]][2,2])
  aupr_rf_eval <- c(aupr_rf_eval, results[[perm]][[2]][3,2])
  mcc_rf_eval <- c(mcc_rf_eval, results[[perm]][[2]][4,2])
  
  accuracies_lr_eval <- c(accuracies_lr_eval, results[[perm]][[2]][1,3])
  auc_lr_eval <- c(auc_lr_eval, results[[perm]][[2]][2,3])
  aupr_lr_eval <- c(aupr_lr_eval, results[[perm]][[2]][3,3])
  mcc_lr_eval <- c(mcc_lr_eval, results[[perm]][[2]][4,3])
  
}

print(which.max(accuracies_lr_eval))
print(which.max(auc_lr_eval))
print(which.max(aupr_lr_eval))
print(which.max(mcc_lr_eval))

print(which.max(accuracies_rf_eval))
print(which.max(auc_rf_eval))
print(which.max(aupr_rf_eval))
print(which.max(mcc_rf_eval))


# check whether this is significantly better then white noise
p_values <- c(t.test(1-misClasificError_lr_wn, results[[which.max(accuracies_lr_eval)]]$accuracy_lr)$p.value,
              t.test(auc_lr_wn, results[[which.max(auc_lr_eval)]]$auc_lr)$p.value,
              t.test(aupr_lr_wn, results[[which.max(aupr_lr_eval)]]$aupr_lr)$p.value,
              t.test(mcc_lr_wn, results[[which.max(mcc_lr_eval)]]$mcc_lr)$p.value,
              t.test(1-misClasificError_rf_wn, results[[which.max(accuracies_rf_eval)]]$accuracy_rf)$p.value,
              t.test(auc_rf_wn, results[[which.max(auc_rf_eval)]]$auc_rf)$p.value,
              t.test(aupr_rf_wn, results[[which.max(aupr_rf_eval)]]$aupr_rf)$p.value,
              t.test(mcc_rf_wn, results[[which.max(mcc_rf_eval)]]$mcc_rf)$p.value)
models <- c(rep("LR", 4), rep("RF", 4))
method <- c(rep(c("Accuracy", "AUC", "AUPR", "MCC"),2))
accuracy_p_values <- data.frame(model = models,
                                method = method,
                                p_value = p_values,
                                significance = rep("",8))
for (index in 1:nrow(accuracy_p_values)) {
  if (accuracy_p_values$p_value[index] < 0.1) {accuracy_p_values$significance[index] = "*"}
  if (accuracy_p_values$p_value[index] < 0.05) {accuracy_p_values$significance[index] = "**"}
  if (accuracy_p_values$p_value[index] < 0.01) {accuracy_p_values$significance[index] = "***"}
}

print(accuracy_p_values)




