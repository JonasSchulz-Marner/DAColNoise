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
violet <- c("M30", "TNFa", "AST")
blue <- c()
white <- c("Alter", "Adiponektin")
pink <- c("M65", "ALT")
red <- c("Geschlecht", "ALT_AST_ratio")
dependent_variable = c(1)
balance_class = c("1")

# haberman
dataset = na.omit(read.csv("/Users/jonas/Library/Mobile Documents/com~apple~CloudDocs/Documents/Uni/Masterarbeit/data/haberman.data", sep = ",", header = FALSE))
columns = c(1:3)
# column names: "V1", "V2", "V3", "V4"
violet <- c()
blue <- c("V2")
white <- c("V1")
pink <- c()
red <- c("V3")
dependent_variable = c(4)
balance_class = c("1")
dataset[,4] <- dataset[,4]-1

# sobar-72 CCBR
dataset = na.omit(read.csv("/Users/jonas/Library/Mobile Documents/com~apple~CloudDocs/Documents/Uni/Masterarbeit/data/sobar-72.csv", sep = ",", header = TRUE))
columns = c(1:19)
# column names: "behavior_sexualRisk", "behavior_eating", "behavior_personalHygine", "intention_aggregation"
# "intention_commitment", "attitude_consistency", "attitude_spontaneity", "norm_significantPerson"
# "norm_fulfillment", "perception_vulnerability", "perception_severity", "motivation_strength"       
# "motivation_willingness", "socialSupport_emotionality", "socialSupport_appreciation"
# "socialSupport_instrumental", "empowerment_knowledge", "empowerment_abilities"     
# "empowerment_desires", "ca_cervix"
violet <- c("intention_commitment", "attitude_consistency", "motivation_willingness", "empowerment_abilities")
blue <- c("intention_aggregation", "norm_fulfillment", "perception_severity", "socialSupport_instrumental")
white <- c("attitude_spontaneity", "perception_vulnerability", "socialSupport_appreciation", "empowerment_desires")
pink <- c("behavior_sexualRisk", "behavior_personalHygine", "norm_significantPerson")
red <- c("behavior_eating", "motivation_strength", "socialSupport_emotionality", "empowerment_knowledge")
dependent_variable = c(20)
balance_class = c("1")

# fertility
dataset = na.omit(read.csv("/Users/jonas/Library/Mobile Documents/com~apple~CloudDocs/Documents/Uni/Masterarbeit/data/fertility_Diagnosis.txt", sep = ",", dec = ".", header = FALSE))
dataset[,10] <- ifelse(dataset[,10] == "N", 1, 0)
columns = c(1:9)
# column names: "V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10"
violet <- c("V7", "V5")
blue <- c("V2", "V9")
white <- c("V1")
pink <- c("V4", "V6")
red <- c("V3", "V8")
dependent_variable = c(10)
balance_class = c("0")

# risk cancer CCRF
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
violet <- c("V7", "V5", "V10", "V12", "V18", "V26", "V32")
blue <- c("V2", "V9", "V11", "V15", "V27", "V29", "V33")
white <- c("V1","V19", "V20", "V21", "V17", "V28", "V34")
pink <- c("V4", "V6", "V23", "V13", "V22", "V30", "V31")
red <- c("V3", "V8", "V14", "V16", "V24", "V25")
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

# CTG
dataset = na.omit(read.csv("/Users/jonas/Library/Mobile Documents/com~apple~CloudDocs/Documents/Uni/Masterarbeit/data/CTG_new.csv", sep = ";", dec = ",", header = TRUE))
dataset <- dataset[dataset$NSP != 2,]
dataset[,23] <- ifelse(dataset[,23] == "3", 1, 0)
columns = c(1:22)
# column names: "LB", "AC",  "FM", "UC", "DL", "DS", "DP", "ASTV", "MSTV", "ALTV", "MLTV", "Width", "Min", "Max"
# "Nmax", "Nzeros", "Mode", "Mean", "Median", "Variance", "Tendency", "CLASS", "NSP" 
violet <- c("AC", "DS", "Min", "Nmax")
blue <- c("LB", "DP", "MSTV", "Median", "Variance")
white <- c("DL", "Width", "Nzeros")
pink <- c("FM", "ASTV", "MLTV", "Mode", "Tendency")
red <- c("UC", "ALTV", "Max", "Mean", "CLASS")
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




######################## Analysis ############################

seed <- 54309

#amount of new data (2,3,4)
amount_new_data <- 3
if (amount_new_data == 2) {
  new_datapoints <- "double"
  smote_amount <- 2
} else if (amount_new_data == 3) {
  new_datapoints <- "triple"
  smote_amount <- 3
} else if (amount_new_data == 4) {
  new_datapoints <- "quadruple"
  smote_amount <- 4
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

misClasificError_lr <- NULL
misClasificError_lr_augmented <- NULL
misClasificError_lr_smote <- NULL
misClasificError_lr_adasyn <- NULL
misClasificError_lr_wn <- NULL
misClasificError_rf <- NULL
misClasificError_rf_augmented <- NULL
misClasificError_rf_smote <- NULL
misClasificError_rf_adasyn <- NULL
misClasificError_rf_wn <- NULL

auc_lr <- NULL
auc_lr_augmented <- NULL
auc_lr_smote <- NULL
auc_lr_adasyn <- NULL
auc_lr_wn <- NULL
auc_rf <- NULL
auc_rf_augmented <- NULL
auc_rf_smote <- NULL
auc_rf_adasyn <- NULL
auc_rf_wn <- NULL

aupr_lr <- NULL
aupr_lr_augmented <- NULL
aupr_lr_smote <- NULL
aupr_lr_adasyn <- NULL
aupr_lr_wn <- NULL
aupr_rf <- NULL
aupr_rf_augmented <- NULL
aupr_rf_smote <- NULL
aupr_rf_adasyn <- NULL
aupr_rf_wn <- NULL

mcc_lr <- NULL
mcc_lr_augmented <- NULL
mcc_lr_smote <- NULL
mcc_lr_adasyn <- NULL
mcc_lr_wn <- NULL
mcc_rf <- NULL
mcc_rf_augmented <- NULL
mcc_rf_smote <- NULL
mcc_rf_adasyn <- NULL
mcc_rf_wn <- NULL

for (i in 1:50){
  
  ## set the seed to make the partition reproducible
  set.seed(seed+i)
  train_ind <- stratified_sample(dataset[,dependent_variable], 0.7)
  
  train <- dataset[train_ind, ]
  test <- dataset[-train_ind, ]

  augmented_data <- DAColNoise(same_noise = FALSE,     # "same" = TRUE or "different" = FALSE
                               tbl_columns_selected = columns,   # Indices of the selected columns
                               dataset = train, 
                               tbl_classification_columns_selected = dependent_variable,   # Index of the column of the dependent variable
                               balanced_classes = balance_class,   # Name of the classes that should be balanced in vector 
                               balance_process = "balancing_with_noise",    # "oversampling" or "balancing_with_noise"
                               seed = seed+i,   # any seed 
                               noise_color = 1.5,   # -1.5, -1, 0, 1, 1.5
                               violet_noise = violet,  # names of the columns in a vector
                               blue_noise = blue,  # names of the columns in a vector
                               white_noise = white,   # names of the columns in a vector
                               pink_noise = pink,  # names of the columns in a vector
                               red_noise = red,   # names of the columns in a vector
                               new_datapoints = "triple",   # "double", "triple", "quadruple" or "individual" 
                               amount_new_datapoints = 100,  # amount of new datapoints, if individual
                               balancing = TRUE,   # "balancing" = TRUE or "no_balancing" = FALSE
                               rounded_variables = c(),  # names of the columns in a vector
                               negative_values = c())   # names of the columns in a vector

  ### SMOTE ### mit k = 5 und k = 3
  smote_data <- smotefamily::SMOTE(X = train[-dependent_variable], target = train[dependent_variable], K = 5, 
                                   dup_size = ceiling(table(train[,dependent_variable])[1]/table(train[,dependent_variable])[2])*smote_amount)$data
  smote_data <- smotefamily::SMOTE(X = smote_data[-max(ncol(smote_data))], target = smote_data$class, K = 5)$data
  smote_data$class <- as.factor(smote_data$class)

  adasyn_data <- smotefamily::ADAS(X = train[-dependent_variable], target = train[dependent_variable], K = 5)$data
  adasyn_data$class <- as.factor(adasyn_data$class)
  wn_data <- DAColNoise(same_noise = TRUE,     # "same" = TRUE or "different" = FALSE
                        tbl_columns_selected = columns,   # Indices of the selected columns
                        dataset = train, 
                        tbl_classification_columns_selected = dependent_variable,   # Index of the column of the dependent variable
                        balanced_classes = balance_class,   # Name of the classes that should be balanced in vector 
                        balance_process = "balancing_with_noise",    # "oversampling" or "balancing_with_noise"
                        seed = seed+i,   # any seed 
                        noise_color = 0,   # -2, -1, 0, 1, 2
                        new_datapoints = "triple",   # "double", "triple", "quadruple" or "individual" 
                        amount_new_datapoints = 100,  # amount of new datapoints, if individual
                        balancing = TRUE,   # "balancing" = TRUE or "no_balancing" = FALSE
                        rounded_variables = c(),  # names of the columns in a vector
                        negative_values = c())
  
  #### Logistic Regression #####
  
  # Train model #
  model <- glm(as.formula(paste(names(train)[dependent_variable], "~.")), family = binomial, data = train)
  model_augmented <- glm(as.formula(paste(names(augmented_data)[dependent_variable], "~.")), family = binomial, data = augmented_data)
  model_smote <- glm(as.formula(paste("class", "~.")), family = binomial, data = smote_data)
  model_adasyn <- glm(as.formula(paste("class", "~.")), family = binomial, data = adasyn_data)
  model_wn <- glm(as.formula(paste(names(wn_data)[dependent_variable], "~.")), family = binomial, data = wn_data)
  
  # predict # # AUC, PR, MCC #
  fitted_results <- predict(model,newdata=subset(test,select=c(-dependent_variable)),type='response')
  fitted_results <- ifelse(fitted_results > 0.5,1,0)
  misClasificError_lr[i] <- mean(fitted_results != test[,dependent_variable])
  roc_lr <- tryCatch({pROC::roc(fitted_results, test[,dependent_variable])},
                     error=function(cond){return(NA)})
  auc_lr[i] <- tryCatch({pROC::auc(roc_lr)[1]}, error=function(cond){return(NA)})
  aupr_lr[i] <- PRROC::pr.curve(as.factor(fitted_results), test[,dependent_variable])$auc.integral
  mcc_lr[i] <- mltools::mcc(as.factor(fitted_results), as.factor(test[,dependent_variable]))

  fitted_results_augmented <- predict(model_augmented,newdata=subset(test,select=c(-dependent_variable)),type='response')
  fitted_results_augmented <- ifelse(fitted_results_augmented > 0.5,1,0)
  misClasificError_lr_augmented[i] <- mean(fitted_results_augmented != test[,dependent_variable])
  roc_lr_augmented <- tryCatch({pROC::roc(fitted_results_augmented, test[,dependent_variable])},
                     error=function(cond){return(NA)})
  auc_lr_augmented[i] <- tryCatch({pROC::auc(roc_lr_augmented)[1]}, error=function(cond){return(NA)})
  aupr_lr_augmented[i] <- PRROC::pr.curve(as.factor(fitted_results_augmented), test[,dependent_variable])$auc.integral
  mcc_lr_augmented[i] <- mltools::mcc(as.factor(fitted_results_augmented), as.factor(test[,dependent_variable]))
  
  fitted_results_smote <- predict(model_smote,newdata=subset(test,select=c(-dependent_variable)),type='response')
  fitted_results_smote <- ifelse(fitted_results_smote > 0.5,1,0)
  misClasificError_lr_smote[i] <- mean(fitted_results_smote != test[,dependent_variable])
  roc_lr_smote <- tryCatch({pROC::roc(fitted_results_smote, test[,dependent_variable])},
                     error=function(cond){return(NA)})
  auc_lr_smote[i] <- tryCatch({pROC::auc(roc_lr_smote)[1]}, error=function(cond){return(NA)})
  aupr_lr_smote[i] <- PRROC::pr.curve(as.factor(fitted_results_smote), test[,dependent_variable])$auc.integral
  mcc_lr_smote[i] <- mltools::mcc(as.factor(fitted_results_smote), as.factor(test[,dependent_variable]))
  
  fitted_results_adasyn <- predict(model_adasyn,newdata=subset(test,select=c(-dependent_variable)),type='response')
  fitted_results_adasyn <- ifelse(fitted_results_adasyn > 0.5,1,0)
  misClasificError_lr_adasyn[i] <- mean(fitted_results_adasyn != test[,dependent_variable])
  roc_lr_adasyn <- tryCatch({pROC::roc(fitted_results_adasyn, test[,dependent_variable])},
                     error=function(cond){return(NA)})
  auc_lr_adasyn[i] <- tryCatch({pROC::auc(roc_lr_adasyn)[1]}, error=function(cond){return(NA)})
  aupr_lr_adasyn[i] <- PRROC::pr.curve(as.factor(fitted_results_adasyn), test[,dependent_variable])$auc.integral
  mcc_lr_adasyn[i] <- mltools::mcc(as.factor(fitted_results_adasyn), as.factor(test[,dependent_variable]))
  
  fitted_results_wn <- predict(model_wn,newdata=subset(test,select=c(-dependent_variable)),type='response')
  fitted_results_wn <- ifelse(fitted_results_wn > 0.5,1,0)
  misClasificError_lr_wn[i] <- mean(fitted_results_wn != test[,dependent_variable])
  roc_lr_wn <- tryCatch({pROC::roc(fitted_results_wn, test[,dependent_variable])},
                     error=function(cond){return(NA)})
  auc_lr_wn[i] <- tryCatch({pROC::auc(roc_lr_wn)[1]}, error=function(cond){return(NA)})
  aupr_lr_wn[i] <- PRROC::pr.curve(as.factor(fitted_results_wn), test[,dependent_variable])$auc.integral
  mcc_lr_wn[i] <- mltools::mcc(as.factor(fitted_results_wn), as.factor(test[,dependent_variable]))
  
  
  #### Random Forest #####
  
  # convert response variable to factor
  train[,dependent_variable] <- as.factor(train[,dependent_variable])
  augmented_data <- as.data.frame(augmented_data)
  augmented_data[,dependent_variable] <- as.factor(augmented_data[,dependent_variable])
  rf <- randomForest(as.formula(paste(names(train)[dependent_variable], "~.")), data = train, ntree=100, mtry=2, importance=TRUE)
  rf_augmented <- randomForest(as.formula(paste(names(train)[dependent_variable], "~.")), data = augmented_data, ntree=100, mtry=2, importance=TRUE)
  rf_smote <- randomForest(as.formula(paste("class", "~.")), data = smote_data, ntree=100, mtry=2, importance=TRUE)
  rf_adasyn <- randomForest(as.formula(paste("class", "~.")), data = adasyn_data, ntree=100, mtry=2, importance=TRUE)
  
  wn_data <- as.data.frame(wn_data)
  wn_data[,dependent_variable] <- as.factor(wn_data[,dependent_variable])
  rf_wn <- randomForest(as.formula(paste(names(train)[dependent_variable], "~.")), data = wn_data, ntree=100, mtry=2, importance=TRUE)
  
  # predict and quality measurements # # AUC, PR, MCC #
  prediction <- predict(rf, test[,-dependent_variable])
  misClasificError_rf[i] <- mean(prediction != test[,dependent_variable])
  roc_rf <- tryCatch({pROC::roc(prediction, test[,dependent_variable])},
                     error=function(cond){return(NA)})
  auc_rf[i] <- tryCatch({pROC::auc(roc_rf)[1]}, error=function(cond){return(NA)})
  aupr_rf[i] <- PRROC::pr.curve(prediction, test[,dependent_variable])$auc.integral
  mcc_rf[i] <- mltools::mcc(prediction, as.factor(test[,dependent_variable]))
  
  predict_augmented <- predict(rf_augmented, test[,-dependent_variable])
  misClasificError_rf_augmented[i] <- mean(predict_augmented != test[,dependent_variable])
  roc_rf_augmented <- tryCatch({pROC::roc(predict_augmented, test[,dependent_variable])},
                     error=function(cond){return(NA)})
  auc_rf_augmented[i] <- tryCatch({pROC::auc(roc_rf_augmented)[1]}, error=function(cond){return(NA)})
  aupr_rf_augmented[i] <- PRROC::pr.curve(predict_augmented, test[,dependent_variable])$auc.integral
  mcc_rf_augmented[i] <- mltools::mcc(predict_augmented, as.factor(test[,dependent_variable]))
  
  predict_smote <- predict(rf_smote, test[,-dependent_variable])
  misClasificError_rf_smote[i] <- mean(predict_smote != test[,dependent_variable])
  roc_rf_smote <- tryCatch({pROC::roc(predict_smote, test[,dependent_variable])},
                               error=function(cond){return(NA)})
  auc_rf_smote[i] <- tryCatch({pROC::auc(roc_rf_smote)[1]}, error=function(cond){return(NA)})
  aupr_rf_smote[i] <- PRROC::pr.curve(predict_smote, test[,dependent_variable])$auc.integral
  mcc_rf_smote[i] <- mltools::mcc(predict_smote, as.factor(test[,dependent_variable]))
  
  predict_adasyn <- predict(rf_adasyn, test[,-dependent_variable])
  misClasificError_rf_adasyn[i] <- mean(predict_adasyn != test[,dependent_variable])
  roc_rf_adasyn <- tryCatch({pROC::roc(predict_adasyn, test[,dependent_variable])},
                               error=function(cond){return(NA)})
  auc_rf_adasyn[i] <- tryCatch({pROC::auc(roc_rf_adasyn)[1]}, error=function(cond){return(NA)})
  aupr_rf_adasyn[i] <- PRROC::pr.curve(predict_adasyn, test[,dependent_variable])$auc.integral
  mcc_rf_adasyn[i] <- mltools::mcc(predict_adasyn, as.factor(test[,dependent_variable]))
  
  predict_wn <- predict(rf_wn, test[,-dependent_variable])
  misClasificError_rf_wn[i] <- mean(predict_wn != test[,dependent_variable])
  roc_rf_wn <- tryCatch({pROC::roc(predict_wn, test[,dependent_variable])},
                               error=function(cond){return(NA)})
  auc_rf_wn[i] <- tryCatch({pROC::auc(roc_rf_wn)[1]}, error=function(cond){return(NA)})
  aupr_rf_wn[i] <- PRROC::pr.curve(predict_wn, test[,dependent_variable])$auc.integral
  mcc_rf_wn[i] <- mltools::mcc(predict_wn, as.factor(test[,dependent_variable]))

}


### Plot results ###

# accuracy

models <- c(rep("LR", 5*50), rep("RF", 5*50))
method <- c(rep(c(rep("No DA", 50), rep("SMOTE", 50), rep("ADASYN", 50), rep("WN", 50), rep("CN", 50)),2))
value <- c(1-(misClasificError_lr), 
           1-(misClasificError_lr_smote), 1-(misClasificError_lr_adasyn),
           1-(misClasificError_lr_wn), 1-(misClasificError_lr_augmented),
           1-(misClasificError_rf), 
           1-(misClasificError_rf_smote), 1-(misClasificError_rf_adasyn),
           1-(misClasificError_rf_wn), 1-(misClasificError_rf_augmented))
accuracies <- data.frame(model = models,
                         method = method,
                         value = value)
accuracies$method <- factor(accuracies$method, levels = unique(accuracies$method))

# Plot
pred_error <- ggplot2::ggplot(accuracies, aes(fill=models, y=value, x=method)) + 
  coord_cartesian(ylim=c(0.94,1)) +
  geom_boxplot() +
  ggtitle("Accuracy") + theme_minimal() + 
  scale_fill_manual(values=c("#A8DADC", "#457B9D")) + labs(x="")


# AUC

models <- c(rep("LR", 5*50), rep("RF", 5*50))
method <- c(rep(c(rep("No DA", 50), rep("SMOTE", 50), rep("ADASYN", 50), rep("WN", 50), rep("CN", 50)),2))
value <- c(auc_lr, 
          auc_lr_smote, auc_lr_adasyn,
          auc_lr_wn, auc_lr_augmented, 
          auc_rf, 
          auc_rf_smote, auc_rf_adasyn,
          auc_rf_wn, auc_rf_augmented)
auc <- data.frame(model = models,
                  method = method,
                  value = value)
auc$method <- factor(auc$method, levels = unique(auc$method))

# Plot 
auc_plot <- ggplot2::ggplot(auc, aes(fill=models, y=value, x=method)) + 
  geom_boxplot() + coord_cartesian(ylim=c(0.64,1)) +
  ggtitle("Area under the ROC-Curve") + theme_minimal() + 
  scale_fill_manual(values=c("#A8DADC", "#457B9D")) + labs(x="")


# AUPR

models <- c(rep("LR", 5*50), rep("RF", 5*50))
method <- c(rep(c(rep("No DA", 50), rep("SMOTE", 50), rep("ADASYN", 50), rep("WN", 50), rep("CN", 50)),2))
value <- c(aupr_lr, 
           aupr_lr_smote, aupr_lr_adasyn,
           aupr_lr_wn, aupr_lr_augmented, 
           aupr_rf, 
           aupr_rf_smote, aupr_rf_adasyn,
           aupr_rf_wn, aupr_rf_augmented)
aupr <- data.frame(model = models,
                         method = method,
                         value = value)
aupr$method <- factor(aupr$method, levels = unique(aupr$method))

# Plot
aupr_plot <- ggplot2::ggplot(aupr, aes(fill=models, y=value, x=method)) + 
  coord_cartesian(ylim=c(0.9625,0.975)) +
  geom_boxplot() + ggtitle("Area under Precision Recall") + theme_minimal() + 
  scale_fill_manual(values=c("#A8DADC", "#457B9D")) + labs(x="")


# MCC

models <- c(rep("LR", 5*50), rep("RF", 5*50))
method <- c(rep(c(rep("No DA", 50), rep("SMOTE", 50), rep("ADASYN", 50), rep("WN", 50), rep("CN", 50)),2))
value <- c(mcc_lr, 
           mcc_lr_smote, mcc_lr_adasyn,
           mcc_lr_wn, mcc_lr_augmented, 
           mcc_rf, 
           mcc_rf_smote, mcc_rf_adasyn,
           mcc_rf_wn, mcc_rf_augmented)
mcc <- data.frame(model = models,
                   method = method,
                   value = value)
mcc$method <- factor(mcc$method, levels = unique(mcc$method))

# Plot
mcc_plot <- ggplot2::ggplot(mcc, aes(fill=models, y=value, x=method)) + 
  coord_cartesian(ylim=c(0,1)) +
  geom_boxplot() + ggtitle("Matthews correlation coefficient") + theme_minimal() + 
  scale_fill_manual(values=c("#A8DADC", "#457B9D")) + labs(x="")


# plot all
cowplot::plot_grid(pred_error, auc_plot, aupr_plot, mcc_plot, ncol = 2, nrow = 2)



# check whether there is a significant difference in the mean of no Data Augmentation to any Data Augmentation Method
### Significance ###


# accuracy
p_values <- c(1,t.test(1-misClasificError_lr, 1-misClasificError_lr_smote)$p.value,
              t.test(1-misClasificError_lr, 1-misClasificError_lr_adasyn)$p.value,
              t.test(1-misClasificError_lr, 1-misClasificError_lr_wn)$p.value,
              t.test(1-misClasificError_lr, 1-misClasificError_lr_augmented)$p.value,1,
              t.test(1-misClasificError_rf, 1-misClasificError_rf_smote)$p.value,
              t.test(1-misClasificError_rf, 1-misClasificError_rf_adasyn)$p.value,
              t.test(1-misClasificError_rf, 1-misClasificError_rf_wn)$p.value,
              t.test(1-misClasificError_rf, 1-misClasificError_rf_augmented)$p.value)
means = c(1-mean(misClasificError_lr), 1-mean(misClasificError_lr_smote), 1-mean(misClasificError_lr_adasyn), 1-mean(misClasificError_lr_wn),
          1-mean(misClasificError_lr_augmented), 1-mean(misClasificError_rf), 1-mean(misClasificError_rf_smote), 
          1-mean(misClasificError_rf_adasyn), 1-mean(misClasificError_rf_wn), 1-mean(misClasificError_rf_augmented))
models <- c(rep("LR", 5), rep("RF", 5))
method <- c(rep(c("No DA", "SMOTE", "ADASYN", "WN", "CN"),2))
accuracy_p_values <- data.frame(model = models,
                                method = method,
                                p_value = p_values,
                                mean = round(means,3),
                                significance = rep("",10))
for (index in 1:5) {
  if (accuracy_p_values$p_value[index] < 0.1 && accuracy_p_values$mean[1] < accuracy_p_values$mean[index]) {accuracy_p_values$significance[index] = "*"}
  if (accuracy_p_values$p_value[index] < 0.05 && accuracy_p_values$mean[1] < accuracy_p_values$mean[index]) {accuracy_p_values$significance[index] = "**"}
  if (accuracy_p_values$p_value[index] < 0.01 && accuracy_p_values$mean[1] < accuracy_p_values$mean[index]) {accuracy_p_values$significance[index] = "***"}
}
for (index in 6:10) {
  if (accuracy_p_values$p_value[index] < 0.1 && accuracy_p_values$mean[6] < accuracy_p_values$mean[index]) {accuracy_p_values$significance[index] = "*"}
  if (accuracy_p_values$p_value[index] < 0.05 && accuracy_p_values$mean[6] < accuracy_p_values$mean[index]) {accuracy_p_values$significance[index] = "**"}
  if (accuracy_p_values$p_value[index] < 0.01 && accuracy_p_values$mean[6] < accuracy_p_values$mean[index]) {accuracy_p_values$significance[index] = "***"}
}
accuracy_p_values = accuracy_p_values[,c(1,2,4,5)]


# AUC
auc_lr = na.omit(auc_lr)
auc_lr_smote = na.omit(auc_lr_smote)
auc_lr_adasyn = na.omit(auc_lr_adasyn)
auc_lr_wn = na.omit(auc_lr_wn)
auc_lr_augmented = na.omit(auc_lr_augmented)
auc_rf = na.omit(auc_rf)
auc_rf_smote = na.omit(auc_rf_smote)
auc_rf_adasyn = na.omit(auc_rf_adasyn)
auc_rf_wn = na.omit(auc_rf_wn)
auc_rf_augmented = na.omit(auc_rf_augmented)


p_values <- c(1,t.test(auc_lr, auc_lr_smote)$p.value,
              t.test(auc_lr, auc_lr_adasyn)$p.value,
              t.test(auc_lr, auc_lr_wn)$p.value,
              t.test(auc_lr, auc_lr_augmented)$p.value,1,
              t.test(auc_rf, auc_rf_smote)$p.value,
              t.test(auc_rf, auc_rf_adasyn)$p.value,
              t.test(auc_rf, auc_rf_wn)$p.value,
              t.test(auc_rf, auc_rf_augmented)$p.value)
means = c(mean(auc_lr), mean(auc_lr_smote), mean(auc_lr_adasyn), mean(auc_lr_wn),
          mean(auc_lr_augmented), mean(na.omit(auc_rf)), mean(auc_rf_smote), 
          mean(auc_rf_adasyn), mean(auc_rf_wn), mean(auc_rf_augmented))
models <- c(rep("LR", 5), rep("RF", 5))
method <- c(rep(c("No DA", "SMOTE", "ADASYN", "WN", "CN"),2))
auc_p_values <- data.frame(model = models,
                                method = method,
                                p_value = p_values,
                                mean = round(means,3),
                                significance = rep("",10))
for (index in 1:5) {
  if (auc_p_values$p_value[index] < 0.1 && auc_p_values$mean[1] < auc_p_values$mean[index]) {auc_p_values$significance[index] = "*"}
  if (auc_p_values$p_value[index] < 0.05 && auc_p_values$mean[1] < auc_p_values$mean[index]) {auc_p_values$significance[index] = "**"}
  if (auc_p_values$p_value[index] < 0.01 && auc_p_values$mean[1] < auc_p_values$mean[index]) {auc_p_values$significance[index] = "***"}
}
for (index in 6:10) {
  if (auc_p_values$p_value[index] < 0.1 && auc_p_values$mean[6] < auc_p_values$mean[index]) {auc_p_values$significance[index] = "*"}
  if (auc_p_values$p_value[index] < 0.05 && auc_p_values$mean[6] < auc_p_values$mean[index]) {auc_p_values$significance[index] = "**"}
  if (auc_p_values$p_value[index] < 0.01 && auc_p_values$mean[6] < auc_p_values$mean[index]) {auc_p_values$significance[index] = "***"}
}
auc_p_values = auc_p_values[,c(1,2,4,5)]


# AUPR
p_values <- c(1,t.test(aupr_lr, aupr_lr_smote)$p.value,
              t.test(aupr_lr, aupr_lr_adasyn)$p.value,
              t.test(aupr_lr, aupr_lr_wn)$p.value,
              t.test(aupr_lr, aupr_lr_augmented)$p.value,1,
              t.test(aupr_rf, aupr_rf_smote)$p.value,
              t.test(aupr_rf, aupr_rf_adasyn)$p.value,
              t.test(aupr_rf, aupr_rf_wn)$p.value,
              t.test(aupr_rf, aupr_rf_augmented)$p.value)
models <- c(rep("LR", 5), rep("RF", 5))
method <- c(rep(c("No DA", "SMOTE", "ADASYN", "WN", "CN"),2))
means = c(mean(aupr_lr), mean(aupr_lr_smote), mean(aupr_lr_adasyn), mean(aupr_lr_wn),
          mean(aupr_lr_augmented), mean(na.omit(aupr_rf)), mean(aupr_rf_smote), 
          mean(aupr_rf_adasyn), mean(aupr_rf_wn), mean(aupr_rf_augmented))
aupr_p_values <- data.frame(model = models,
                           method = method,
                           p_value = p_values,
                           mean = round(means,3),
                           significance = rep("",10))
for (index in 1:5) {
  if (aupr_p_values$p_value[index] < 0.1 && aupr_p_values$mean[1] < aupr_p_values$mean[index]) {aupr_p_values$significance[index] = "*"}
  if (aupr_p_values$p_value[index] < 0.05 && aupr_p_values$mean[1] < aupr_p_values$mean[index]) {aupr_p_values$significance[index] = "**"}
  if (aupr_p_values$p_value[index] < 0.01 && aupr_p_values$mean[1] < aupr_p_values$mean[index]) {aupr_p_values$significance[index] = "***"}
}
for (index in 6:10) {
  if (aupr_p_values$p_value[index] < 0.1 && aupr_p_values$mean[6] < aupr_p_values$mean[index]) {aupr_p_values$significance[index] = "*"}
  if (aupr_p_values$p_value[index] < 0.05 && aupr_p_values$mean[6] < aupr_p_values$mean[index]) {aupr_p_values$significance[index] = "**"}
  if (aupr_p_values$p_value[index] < 0.01 && aupr_p_values$mean[6] < aupr_p_values$mean[index]) {aupr_p_values$significance[index] = "***"}
}
aupr_p_values = aupr_p_values[,c(1,2,4,5)]

# MCC
p_values <- c(1,t.test(mcc_lr, mcc_lr_smote)$p.value,
              t.test(mcc_lr, mcc_lr_adasyn)$p.value,
              t.test(mcc_lr, mcc_lr_wn)$p.value,
              t.test(mcc_lr, mcc_lr_augmented)$p.value,1,
              t.test(mcc_rf, mcc_rf_smote)$p.value,
              t.test(mcc_rf, mcc_rf_adasyn)$p.value,
              t.test(mcc_rf, mcc_rf_wn)$p.value,
              t.test(mcc_rf, mcc_rf_augmented)$p.value)
models <- c(rep("LR", 5), rep("RF", 5))
method <- c(rep(c("No DA", "SMOTE", "ADASYN", "WN", "CN"),2))
means = c(mean(mcc_lr), mean(mcc_lr_smote), mean(mcc_lr_adasyn), mean(mcc_lr_wn),
          mean(mcc_lr_augmented), mean(na.omit(mcc_rf)), mean(mcc_rf_smote), 
          mean(mcc_rf_adasyn), mean(mcc_rf_wn), mean(mcc_rf_augmented))
mcc_p_values <- data.frame(model = models,
                            method = method,
                            p_value = p_values,
                           mean = round(means,3),
                            significance = rep("",10))
for (index in 1:5) {
  if (mcc_p_values$p_value[index] < 0.1 && mcc_p_values$mean[1] < mcc_p_values$mean[index]) {mcc_p_values$significance[index] = "*"}
  if (mcc_p_values$p_value[index] < 0.05 && mcc_p_values$mean[1] < mcc_p_values$mean[index]) {mcc_p_values$significance[index] = "**"}
  if (mcc_p_values$p_value[index] < 0.01 && mcc_p_values$mean[1] < mcc_p_values$mean[index]) {mcc_p_values$significance[index] = "***"}
}
for (index in 6:10) {
  if (mcc_p_values$p_value[index] < 0.1 && mcc_p_values$mean[6] < mcc_p_values$mean[index]) {mcc_p_values$significance[index] = "*"}
  if (mcc_p_values$p_value[index] < 0.05 && mcc_p_values$mean[6] < mcc_p_values$mean[index]) {mcc_p_values$significance[index] = "**"}
  if (mcc_p_values$p_value[index] < 0.01 && mcc_p_values$mean[6] < mcc_p_values$mean[index]) {mcc_p_values$significance[index] = "***"}
}
mcc_p_values = mcc_p_values[,c(1,2,4,5)]

# print all significance tables
print("Significance against No DA")
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

table_noda = paste(string_1, "\\hline \\hline  \textbf{model} & \textbf{method} & \textbf{AUPR} &  & \textbf{model} & \textbf{method} & \textbf{MCC} & \\ \\hline", string_3,  "\\hline")





### Significance compared to SMOTE ###

# accuracy
p_values <- c(1,t.test(1-misClasificError_lr_smote, 1-misClasificError_lr_wn)$p.value,
              t.test(1-misClasificError_lr_smote, 1-misClasificError_lr_augmented)$p.value,1,
              t.test(1-misClasificError_rf_smote, 1-misClasificError_rf_wn)$p.value,
              t.test(1-misClasificError_rf_smote, 1-misClasificError_rf_augmented)$p.value)
p_values_wn <- c(t.test(1-misClasificError_lr_smote, 1-misClasificError_lr_wn)$p.value,1,
              t.test(1-misClasificError_lr_wn, 1-misClasificError_lr_augmented)$p.value,
              t.test(1-misClasificError_rf_smote, 1-misClasificError_rf_wn)$p.value,1,
              t.test(1-misClasificError_rf_wn, 1-misClasificError_rf_augmented)$p.value)
p_values_cn <- c(t.test(1-misClasificError_lr_smote, 1-misClasificError_lr_augmented)$p.value,
              t.test(1-misClasificError_lr_wn, 1-misClasificError_lr_augmented)$p.value,1,
              t.test(1-misClasificError_rf_smote, 1-misClasificError_rf_augmented)$p.value,
              t.test(1-misClasificError_rf_wn, 1-misClasificError_rf_augmented)$p.value,1)
models <- c(rep("LR", 3), rep("RF", 3))
method <- c(rep(c("SMOTE", "WN", "CN"),2))
means = c(1-mean(misClasificError_lr_smote), 1-mean(misClasificError_lr_wn),
          1-mean(misClasificError_lr_augmented), 1-mean(misClasificError_rf_smote), 
          1-mean(misClasificError_rf_wn), 1-mean(misClasificError_rf_augmented))
accuracy_p_values <- data.frame(model = models,
                           method = method,
                           p_value = p_values,
                           mean = round(means,3),
                           significance = rep("",6),
                           p_value_wn = p_values_wn,
                           significance_wn = rep("",6),
                           p_value_cn = p_values_cn,
                           significance_cn = rep("",6))
for (index in 1:3) {
  if (accuracy_p_values$p_value[index] < 0.1 && accuracy_p_values$mean[1] < accuracy_p_values$mean[index]) {accuracy_p_values$significance[index] = "*"}
  if (accuracy_p_values$p_value[index] < 0.05 && accuracy_p_values$mean[1] < accuracy_p_values$mean[index]) {accuracy_p_values$significance[index] = "**"}
  if (accuracy_p_values$p_value[index] < 0.01 && accuracy_p_values$mean[1] < accuracy_p_values$mean[index]) {accuracy_p_values$significance[index] = "***"}
}
for (index in 4:6) {
  if (accuracy_p_values$p_value[index] < 0.1 && accuracy_p_values$mean[4] < accuracy_p_values$mean[index]) {accuracy_p_values$significance[index] = "*"}
  if (accuracy_p_values$p_value[index] < 0.05 && accuracy_p_values$mean[4] < accuracy_p_values$mean[index]) {accuracy_p_values$significance[index] = "**"}
  if (accuracy_p_values$p_value[index] < 0.01 && accuracy_p_values$mean[4] < accuracy_p_values$mean[index]) {accuracy_p_values$significance[index] = "***"}
}

for (index in 1:3) {
  if (accuracy_p_values$p_value_wn[index] < 0.1 && accuracy_p_values$mean[2] < accuracy_p_values$mean[index]) {accuracy_p_values$significance_wn[index] = "*"}
  if (accuracy_p_values$p_value_wn[index] < 0.05 && accuracy_p_values$mean[2] < accuracy_p_values$mean[index]) {accuracy_p_values$significance_wn[index] = "**"}
  if (accuracy_p_values$p_value_wn[index] < 0.01 && accuracy_p_values$mean[2] < accuracy_p_values$mean[index]) {accuracy_p_values$significance_wn[index] = "***"}
}
for (index in 4:6) {
  if (accuracy_p_values$p_value_wn[index] < 0.1 && accuracy_p_values$mean[5] < accuracy_p_values$mean[index]) {accuracy_p_values$significance_wn[index] = "*"}
  if (accuracy_p_values$p_value_wn[index] < 0.05 && accuracy_p_values$mean[5] < accuracy_p_values$mean[index]) {accuracy_p_values$significance_wn[index] = "**"}
  if (accuracy_p_values$p_value_wn[index] < 0.01 && accuracy_p_values$mean[5] < accuracy_p_values$mean[index]) {accuracy_p_values$significance_wn[index] = "***"}
}

for (index in 1:3) {
  if (accuracy_p_values$p_value_cn[index] < 0.1 && accuracy_p_values$mean[3] < accuracy_p_values$mean[index]) {accuracy_p_values$significance_cn[index] = "*"}
  if (accuracy_p_values$p_value_cn[index] < 0.05 && accuracy_p_values$mean[3] < accuracy_p_values$mean[index]) {accuracy_p_values$significance_cn[index] = "**"}
  if (accuracy_p_values$p_value_cn[index] < 0.01 && accuracy_p_values$mean[3] < accuracy_p_values$mean[index]) {accuracy_p_values$significance_cn[index] = "***"}
}
for (index in 4:6) {
  if (accuracy_p_values$p_value_cn[index] < 0.1 && accuracy_p_values$mean[6] < accuracy_p_values$mean[index]) {accuracy_p_values$significance_cn[index] = "*"}
  if (accuracy_p_values$p_value_cn[index] < 0.05 && accuracy_p_values$mean[6] < accuracy_p_values$mean[index]) {accuracy_p_values$significance_cn[index] = "**"}
  if (accuracy_p_values$p_value_cn[index] < 0.01 && accuracy_p_values$mean[6] < accuracy_p_values$mean[index]) {accuracy_p_values$significance_cn[index] = "***"}
}
accuracy_p_values = accuracy_p_values[,c(1,2,4,5,7,9)]


# AUC
p_values <- c(1,t.test(auc_lr_smote, auc_lr_wn)$p.value,
              t.test(auc_lr_smote, auc_lr_augmented)$p.value,1,
              t.test(auc_rf_smote, auc_rf_wn)$p.value,
              t.test(auc_rf_smote, auc_rf_augmented)$p.value)
p_values_wn <- c(t.test(auc_lr_smote, auc_lr_wn)$p.value,1,
                 t.test(auc_lr_wn, auc_lr_augmented)$p.value,
                 t.test(auc_rf_smote, auc_rf_wn)$p.value,1,
                 t.test(auc_rf_wn, auc_rf_augmented)$p.value)
p_values_cn <- c(t.test(auc_lr_smote, auc_lr_augmented)$p.value,
                 t.test(auc_lr_wn, auc_lr_augmented)$p.value,1,
                 t.test(auc_rf_smote, auc_rf_augmented)$p.value,
                 t.test(auc_rf_wn, auc_rf_augmented)$p.value,1)
models <- c(rep("LR", 3), rep("RF", 3))
method <- c(rep(c("SMOTE", "WN", "CN"),2))
means = c(mean(auc_lr_smote), mean(auc_lr_wn),
          mean(auc_lr_augmented), mean(auc_rf_smote), 
          mean(auc_rf_wn), mean(auc_rf_augmented))
auc_p_values <- data.frame(model = models,
                           method = method,
                           p_value = p_values,
                           mean = round(means,3),
                           significance = rep("",6),
                           p_value_wn = p_values_wn,
                           significance_wn = rep("",6),
                           p_value_cn = p_values_cn,
                           significance_cn = rep("",6))
for (index in 1:3) {
  if (auc_p_values$p_value[index] < 0.1 && auc_p_values$mean[1] < auc_p_values$mean[index]) {auc_p_values$significance[index] = "*"}
  if (auc_p_values$p_value[index] < 0.05 && auc_p_values$mean[1] < auc_p_values$mean[index]) {auc_p_values$significance[index] = "**"}
  if (auc_p_values$p_value[index] < 0.01 && auc_p_values$mean[1] < auc_p_values$mean[index]) {auc_p_values$significance[index] = "***"}
}
for (index in 4:6) {
  if (auc_p_values$p_value[index] < 0.1 && auc_p_values$mean[4] < auc_p_values$mean[index]) {auc_p_values$significance[index] = "*"}
  if (auc_p_values$p_value[index] < 0.05 && auc_p_values$mean[4] < auc_p_values$mean[index]) {auc_p_values$significance[index] = "**"}
  if (auc_p_values$p_value[index] < 0.01 && auc_p_values$mean[4] < auc_p_values$mean[index]) {auc_p_values$significance[index] = "***"}
}

for (index in 1:3) {
  if (auc_p_values$p_value_wn[index] < 0.1 && auc_p_values$mean[2] < auc_p_values$mean[index]) {auc_p_values$significance_wn[index] = "*"}
  if (auc_p_values$p_value_wn[index] < 0.05 && auc_p_values$mean[2] < auc_p_values$mean[index]) {auc_p_values$significance_wn[index] = "**"}
  if (auc_p_values$p_value_wn[index] < 0.01 && auc_p_values$mean[2] < auc_p_values$mean[index]) {auc_p_values$significance_wn[index] = "***"}
}
for (index in 4:6) {
  if (auc_p_values$p_value_wn[index] < 0.1 && auc_p_values$mean[5] < auc_p_values$mean[index]) {auc_p_values$significance_wn[index] = "*"}
  if (auc_p_values$p_value_wn[index] < 0.05 && auc_p_values$mean[5] < auc_p_values$mean[index]) {auc_p_values$significance_wn[index] = "**"}
  if (auc_p_values$p_value_wn[index] < 0.01 && auc_p_values$mean[5] < auc_p_values$mean[index]) {auc_p_values$significance_wn[index] = "***"}
}

for (index in 1:3) {
  if (auc_p_values$p_value_cn[index] < 0.1 && auc_p_values$mean[3] < auc_p_values$mean[index]) {auc_p_values$significance_cn[index] = "*"}
  if (auc_p_values$p_value_cn[index] < 0.05 && auc_p_values$mean[3] < auc_p_values$mean[index]) {auc_p_values$significance_cn[index] = "**"}
  if (auc_p_values$p_value_cn[index] < 0.01 && auc_p_values$mean[3] < auc_p_values$mean[index]) {auc_p_values$significance_cn[index] = "***"}
}
for (index in 4:6) {
  if (auc_p_values$p_value_cn[index] < 0.1 && auc_p_values$mean[6] < auc_p_values$mean[index]) {auc_p_values$significance_cn[index] = "*"}
  if (auc_p_values$p_value_cn[index] < 0.05 && auc_p_values$mean[6] < auc_p_values$mean[index]) {auc_p_values$significance_cn[index] = "**"}
  if (auc_p_values$p_value_cn[index] < 0.01 && auc_p_values$mean[6] < auc_p_values$mean[index]) {auc_p_values$significance_cn[index] = "***"}
}
auc_p_values = auc_p_values[,c(1,2,4,5,7,9)]




# AUPR
p_values <- c(1,t.test(aupr_lr_smote, aupr_lr_wn)$p.value,
              t.test(aupr_lr_smote, aupr_lr_augmented)$p.value,1,
              t.test(aupr_rf_smote, aupr_rf_wn)$p.value,
              t.test(aupr_rf_smote, aupr_rf_augmented)$p.value)
p_values_wn <- c(t.test(aupr_lr_smote, aupr_lr_wn)$p.value,1,
                 t.test(aupr_lr_wn, aupr_lr_augmented)$p.value,
                 t.test(aupr_rf_smote, aupr_rf_wn)$p.value,1,
                 t.test(aupr_rf_wn, aupr_rf_augmented)$p.value)
p_values_cn <- c(t.test(aupr_lr_smote, aupr_lr_augmented)$p.value,
                 t.test(aupr_lr_wn, aupr_lr_augmented)$p.value,1,
                 t.test(aupr_rf_smote, aupr_rf_augmented)$p.value,
                 t.test(aupr_rf_wn, aupr_rf_augmented)$p.value,1)
models <- c(rep("LR", 3), rep("RF", 3))
method <- c(rep(c("SMOTE", "WN", "CN"),2))
means = c(mean(aupr_lr_smote), mean(aupr_lr_wn),
          mean(aupr_lr_augmented), mean(aupr_rf_smote), 
          mean(aupr_rf_wn), mean(aupr_rf_augmented))
aupr_p_values <- data.frame(model = models,
                           method = method,
                           p_value = p_values,
                           mean = round(means,3),
                           significance = rep("",6),
                           p_value_wn = p_values_wn,
                           significance_wn = rep("",6),
                           p_value_cn = p_values_cn,
                           significance_cn = rep("",6))
for (index in 1:3) {
  if (aupr_p_values$p_value[index] < 0.1 && aupr_p_values$mean[1] < aupr_p_values$mean[index]) {aupr_p_values$significance[index] = "*"}
  if (aupr_p_values$p_value[index] < 0.05 && aupr_p_values$mean[1] < aupr_p_values$mean[index]) {aupr_p_values$significance[index] = "**"}
  if (aupr_p_values$p_value[index] < 0.01 && aupr_p_values$mean[1] < aupr_p_values$mean[index]) {aupr_p_values$significance[index] = "***"}
}
for (index in 4:6) {
  if (aupr_p_values$p_value[index] < 0.1 && aupr_p_values$mean[4] < aupr_p_values$mean[index]) {aupr_p_values$significance[index] = "*"}
  if (aupr_p_values$p_value[index] < 0.05 && aupr_p_values$mean[4] < aupr_p_values$mean[index]) {aupr_p_values$significance[index] = "**"}
  if (aupr_p_values$p_value[index] < 0.01 && aupr_p_values$mean[4] < aupr_p_values$mean[index]) {aupr_p_values$significance[index] = "***"}
}

for (index in 1:3) {
  if (aupr_p_values$p_value_wn[index] < 0.1 && aupr_p_values$mean[2] < aupr_p_values$mean[index]) {aupr_p_values$significance_wn[index] = "*"}
  if (aupr_p_values$p_value_wn[index] < 0.05 && aupr_p_values$mean[2] < aupr_p_values$mean[index]) {aupr_p_values$significance_wn[index] = "**"}
  if (aupr_p_values$p_value_wn[index] < 0.01 && aupr_p_values$mean[2] < aupr_p_values$mean[index]) {aupr_p_values$significance_wn[index] = "***"}
}
for (index in 4:6) {
  if (aupr_p_values$p_value_wn[index] < 0.1 && aupr_p_values$mean[5] < aupr_p_values$mean[index]) {aupr_p_values$significance_wn[index] = "*"}
  if (aupr_p_values$p_value_wn[index] < 0.05 && aupr_p_values$mean[5] < aupr_p_values$mean[index]) {aupr_p_values$significance_wn[index] = "**"}
  if (aupr_p_values$p_value_wn[index] < 0.01 && aupr_p_values$mean[5] < aupr_p_values$mean[index]) {aupr_p_values$significance_wn[index] = "***"}
}

for (index in 1:3) {
  if (aupr_p_values$p_value_cn[index] < 0.1 && aupr_p_values$mean[3] < aupr_p_values$mean[index]) {aupr_p_values$significance_cn[index] = "*"}
  if (aupr_p_values$p_value_cn[index] < 0.05 && aupr_p_values$mean[3] < aupr_p_values$mean[index]) {aupr_p_values$significance_cn[index] = "**"}
  if (aupr_p_values$p_value_cn[index] < 0.01 && aupr_p_values$mean[3] < aupr_p_values$mean[index]) {aupr_p_values$significance_cn[index] = "***"}
}
for (index in 4:6) {
  if (aupr_p_values$p_value_cn[index] < 0.1 && aupr_p_values$mean[6] < aupr_p_values$mean[index]) {aupr_p_values$significance_cn[index] = "*"}
  if (aupr_p_values$p_value_cn[index] < 0.05 && aupr_p_values$mean[6] < aupr_p_values$mean[index]) {aupr_p_values$significance_cn[index] = "**"}
  if (aupr_p_values$p_value_cn[index] < 0.01 && aupr_p_values$mean[6] < aupr_p_values$mean[index]) {aupr_p_values$significance_cn[index] = "***"}
}
aupr_p_values = aupr_p_values[,c(1,2,4,5,7,9)]




# MCC
p_values <- c(1,t.test(mcc_lr_smote, mcc_lr_wn)$p.value,
              t.test(mcc_lr_smote, mcc_lr_augmented)$p.value,1,
              t.test(mcc_rf_smote, mcc_rf_wn)$p.value,
              t.test(mcc_rf_smote, mcc_rf_augmented)$p.value)
p_values_wn <- c(t.test(mcc_lr_smote, mcc_lr_wn)$p.value,1,
              t.test(mcc_lr_wn, mcc_lr_augmented)$p.value,
              t.test(mcc_rf_smote, mcc_rf_wn)$p.value,1,
              t.test(mcc_rf_wn, mcc_rf_augmented)$p.value)
p_values_cn <- c(t.test(mcc_lr_smote, mcc_lr_augmented)$p.value,
              t.test(mcc_lr_wn, mcc_lr_augmented)$p.value,1,
              t.test(mcc_rf_smote, mcc_rf_augmented)$p.value,
              t.test(mcc_rf_wn, mcc_rf_augmented)$p.value,1)
models <- c(rep("LR", 3), rep("RF", 3))
method <- c(rep(c("SMOTE", "WN", "CN"),2))
means = c(mean(mcc_lr_smote), mean(mcc_lr_wn),
          mean(mcc_lr_augmented), mean(mcc_rf_smote), 
          mean(mcc_rf_wn), mean(mcc_rf_augmented))
mcc_p_values <- data.frame(model = models,
                                method = method,
                                p_value = p_values,
                                mean = round(means,3),
                                significance = rep("",6),
                                p_value_wn = p_values_wn,
                                significance_wn = rep("",6),
                                p_value_cn = p_values_cn,
                                significance_cn = rep("",6))
for (index in 1:3) {
  if (mcc_p_values$p_value[index] < 0.1 && mcc_p_values$mean[1] < mcc_p_values$mean[index]) {mcc_p_values$significance[index] = "*"}
  if (mcc_p_values$p_value[index] < 0.05 && mcc_p_values$mean[1] < mcc_p_values$mean[index]) {mcc_p_values$significance[index] = "**"}
  if (mcc_p_values$p_value[index] < 0.01 && mcc_p_values$mean[1] < mcc_p_values$mean[index]) {mcc_p_values$significance[index] = "***"}
}
for (index in 4:6) {
  if (mcc_p_values$p_value[index] < 0.1 && mcc_p_values$mean[4] < mcc_p_values$mean[index]) {mcc_p_values$significance[index] = "*"}
  if (mcc_p_values$p_value[index] < 0.05 && mcc_p_values$mean[4] < mcc_p_values$mean[index]) {mcc_p_values$significance[index] = "**"}
  if (mcc_p_values$p_value[index] < 0.01 && mcc_p_values$mean[4] < mcc_p_values$mean[index]) {mcc_p_values$significance[index] = "***"}
}

for (index in 1:3) {
  if (mcc_p_values$p_value_wn[index] < 0.1 && mcc_p_values$mean[2] < mcc_p_values$mean[index]) {mcc_p_values$significance_wn[index] = "*"}
  if (mcc_p_values$p_value_wn[index] < 0.05 && mcc_p_values$mean[2] < mcc_p_values$mean[index]) {mcc_p_values$significance_wn[index] = "**"}
  if (mcc_p_values$p_value_wn[index] < 0.01 && mcc_p_values$mean[2] < mcc_p_values$mean[index]) {mcc_p_values$significance_wn[index] = "***"}
}
for (index in 4:6) {
  if (mcc_p_values$p_value_wn[index] < 0.1 && mcc_p_values$mean[5] < mcc_p_values$mean[index]) {mcc_p_values$significance_wn[index] = "*"}
  if (mcc_p_values$p_value_wn[index] < 0.05 && mcc_p_values$mean[5] < mcc_p_values$mean[index]) {mcc_p_values$significance_wn[index] = "**"}
  if (mcc_p_values$p_value_wn[index] < 0.01 && mcc_p_values$mean[5] < mcc_p_values$mean[index]) {mcc_p_values$significance_wn[index] = "***"}
}

for (index in 1:3) {
  if (mcc_p_values$p_value_cn[index] < 0.1 && mcc_p_values$mean[3] < mcc_p_values$mean[index]) {mcc_p_values$significance_cn[index] = "*"}
  if (mcc_p_values$p_value_cn[index] < 0.05 && mcc_p_values$mean[3] < mcc_p_values$mean[index]) {mcc_p_values$significance_cn[index] = "**"}
  if (mcc_p_values$p_value_cn[index] < 0.01 && mcc_p_values$mean[3] < mcc_p_values$mean[index]) {mcc_p_values$significance_cn[index] = "***"}
}
for (index in 4:6) {
  if (mcc_p_values$p_value_cn[index] < 0.1 && mcc_p_values$mean[6] < mcc_p_values$mean[index]) {mcc_p_values$significance_cn[index] = "*"}
  if (mcc_p_values$p_value_cn[index] < 0.05 && mcc_p_values$mean[6] < mcc_p_values$mean[index]) {mcc_p_values$significance_cn[index] = "**"}
  if (mcc_p_values$p_value_cn[index] < 0.01 && mcc_p_values$mean[6] < mcc_p_values$mean[index]) {mcc_p_values$significance_cn[index] = "***"}
}
mcc_p_values = mcc_p_values[,c(1,2,4,5,7,9)]

# print all significance tables
print("Significance against SMOTE")
print(accuracy_p_values)
print(auc_p_values)
print(aupr_p_values)
print(mcc_p_values)



string_1 = ""
string_3 = ""

for (i in 1:nrow(accuracy_p_values)) {
  for (j in 1:ncol(accuracy_p_values)){
    if (j != 1) {
      if (j == 4 && i == 1){string_1 = paste(string_1, " & ", " --- ")}
      else if (j == 5 && i == 2){string_1 = paste(string_1, " & ", " --- ")}
      else if (j == 5 && i == 5){string_1 = paste(string_1, " & ", " --- ")}
      else if (j == 4 && i == 4){string_1 = paste(string_1, " & ", " --- ")}
      else if (j == 6 && i == 3){string_1 = paste(string_1, " & ", " --- ")}
      else if (j == 6 && i == 6){string_1 = paste(string_1, " & ", " --- ")}
      else{string_1 = paste(string_1, " & ", accuracy_p_values[i,j])}
    }
    if (j == 1) {string_1 = paste(string_1, accuracy_p_values[i,j])}
  }
  
  for (j in 1:ncol(accuracy_p_values)){
    if (j == 4 && i == 1){string_1 = paste(string_1, " & ", " --- ")}
    else if (j == 5 && i == 2){string_1 = paste(string_1, " & ", " --- ")}
    else if (j == 5 && i == 5){string_1 = paste(string_1, " & ", " --- ")}
    else if (j == 4 && i == 4){string_1 = paste(string_1, " & ", " --- ")}
    else if (j == 6 && i == 3){string_1 = paste(string_1, " & ", " --- ")}
    else if (j == 6 && i == 6){string_1 = paste(string_1, " & ", " --- ")}
    else{string_1 = paste(string_1, " & ", auc_p_values[i,j])}
  }
  string_1 = paste(string_1, " \\ ")
  
  for (j in 1:ncol(accuracy_p_values)){
    if (j != 1) {
      if (j == 4 && i == 1){string_3 = paste(string_3, " & ", " --- ")}
      else if (j == 5 && i == 2){string_3 = paste(string_3, " & ", " --- ")}
      else if (j == 5 && i == 5){string_3 = paste(string_3, " & ", " --- ")}
      else if (j == 4 && i == 4){string_3 = paste(string_3, " & ", " --- ")}
      else if (j == 6 && i == 3){string_3 = paste(string_3, " & ", " --- ")}
      else if (j == 6 && i == 6){string_3 = paste(string_3, " & ", " --- ")}
      else{string_3 = paste(string_3, " & ", aupr_p_values[i,j])}
    }
    if (j == 1) {string_3 = paste(string_3, aupr_p_values[i,j])}
  }
  
  for (j in 1:ncol(accuracy_p_values)){
    if (j == 4 && i == 1){string_3 = paste(string_3, " & ", " --- ")}
    else if (j == 5 && i == 2){string_3 = paste(string_3, " & ", " --- ")}
    else if (j == 5 && i == 5){string_3 = paste(string_3, " & ", " --- ")}
    else if (j == 4 && i == 4){string_3 = paste(string_3, " & ", " --- ")}
    else if (j == 6 && i == 3){string_3 = paste(string_3, " & ", " --- ")}
    else if (j == 6 && i == 6){string_3 = paste(string_3, " & ", " --- ")}
    else{string_3 = paste(string_3, " & ", mcc_p_values[i,j])}
  }
  string_3 = paste(string_3, " \\ ")
}

table_smote = paste(string_1, "\\hline \\hline  \textbf{model} & \textbf{method} & \textbf{AUPR} &  & & & \textbf{model} & \textbf{method} & \textbf{MCC} & & & \\ \\hline", string_3,  "\\hline")

print(table_noda)
print(table_smote)




