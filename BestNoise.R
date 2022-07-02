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

# sobar-72 CCBR
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

seed <- 54309

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

misClasificError_lr <- NULL
misClasificError_lr_violet <- NULL
misClasificError_lr_blue <- NULL
misClasificError_lr_white <- NULL
misClasificError_lr_pink <- NULL
misClasificError_lr_red <- NULL
misClasificError_lr_different <- NULL
misClasificError_rf <- NULL
misClasificError_rf_violet <- NULL
misClasificError_rf_blue <- NULL
misClasificError_rf_white <- NULL
misClasificError_rf_pink <- NULL
misClasificError_rf_red <- NULL
misClasificError_rf_different <- NULL

auc_lr <- NULL
auc_lr_violet <- NULL
auc_lr_blue <- NULL
auc_lr_white <- NULL
auc_lr_pink <- NULL
auc_lr_red <- NULL
auc_lr_different <- NULL
auc_rf <- NULL
auc_rf_violet <- NULL
auc_rf_blue <- NULL
auc_rf_white <- NULL
auc_rf_pink <- NULL
auc_rf_red <- NULL
auc_rf_different <- NULL

aupr_lr <- NULL
aupr_lr_violet <- NULL
aupr_lr_blue <- NULL
aupr_lr_white <- NULL
aupr_lr_pink <- NULL
aupr_lr_red <- NULL
aupr_lr_different <- NULL
aupr_rf <- NULL
aupr_rf_violet <- NULL
aupr_rf_blue <- NULL
aupr_rf_white <- NULL
aupr_rf_pink <- NULL
aupr_rf_red <- NULL
aupr_rf_different <- NULL

mcc_lr <- NULL
mcc_lr_violet <- NULL
mcc_lr_blue <- NULL
mcc_lr_white <- NULL
mcc_lr_pink <- NULL
mcc_lr_red <- NULL
mcc_lr_different <- NULL
mcc_rf <- NULL
mcc_rf_violet <- NULL
mcc_rf_blue <- NULL
mcc_rf_white <- NULL
mcc_rf_pink <- NULL
mcc_rf_red <- NULL
mcc_rf_different <- NULL

for (i in 1:50){
  
  ## set the seed to make the partition reproducible
  set.seed(seed+i)
  train_ind <- stratified_sample(dataset[,dependent_variable], 0.7)
  
  train <- dataset[train_ind, ]
  test <- dataset[-train_ind, ]
  
  violet <- DAColNoise(same_noise = TRUE,     # "same" = TRUE or "different" = FALSE
                       tbl_columns_selected = columns,   # Indices of the selected columns
                       dataset = train, 
                       tbl_classification_columns_selected = dependent_variable,   # Index of the column of the dependent variable
                       balanced_classes = balance_class,   # Name of the classes that should be balanced in vector 
                       balance_process = "balancing_with_noise",    # "oversampling" or "balancing_with_noise"
                       seed = seed+i,   # any seed 
                       noise_color = -1.5,   # -1.5, -1, 0, 1, 1.5
                       violet_noise = c(),  # names of the columns in a vector
                       blue_noise = c("V2"),  # names of the columns in a vector
                       white_noise = c(),   # names of the columns in a vector
                       pink_noise = c("V3"),  # names of the columns in a vector
                       red_noise = c("V4"),   # names of the columns in a vector
                       new_datapoints = "triple",   # "double", "triple", "quadruple" or "individual" 
                       amount_new_datapoints = 100,  # amount of new datapoints, if individual
                       balancing = TRUE,   # "balancing" = TRUE or "no_balancing" = FALSE
                       rounded_variables = c(),  # names of the columns in a vector
                       negative_values = c())   # names of the columns in a vector
  
  blue <- DAColNoise(same_noise = TRUE,     # "same" = TRUE or "different" = FALSE
                       tbl_columns_selected = columns,   # Indices of the selected columns
                       dataset = train, 
                       tbl_classification_columns_selected = dependent_variable,   # Index of the column of the dependent variable
                       balanced_classes = balance_class,   # Name of the classes that should be balanced in vector 
                       balance_process = "balancing_with_noise",    # "oversampling" or "balancing_with_noise"
                       seed = seed+i,   # any seed 
                       noise_color = -1,   # -1.5, -1, 0, 1, 1.5
                       violet_noise = c("Geschlecht", "ALT_AST_ratio"),  # names of the columns in a vector
                       blue_noise = c("Alter"),  # names of the columns in a vector
                       white_noise = c("M65", "ALT"),   # names of the columns in a vector
                       pink_noise = c("TNFa", "AST"),  # names of the columns in a vector
                       red_noise = c("M30", "Adiponektin"),   # names of the columns in a vector
                       new_datapoints = "triple",   # "double", "triple", "quadruple" or "individual" 
                       amount_new_datapoints = 100,  # amount of new datapoints, if individual
                       balancing = TRUE,   # "balancing" = TRUE or "no_balancing" = FALSE
                       rounded_variables = c(),  # names of the columns in a vector
                       negative_values = c())
  
  white <- DAColNoise(same_noise = TRUE,     # "same" = TRUE or "different" = FALSE
                       tbl_columns_selected = columns,   # Indices of the selected columns
                       dataset = train, 
                       tbl_classification_columns_selected = dependent_variable,   # Index of the column of the dependent variable
                       balanced_classes = balance_class,   # Name of the classes that should be balanced in vector 
                       balance_process = "balancing_with_noise",    # "oversampling" or "balancing_with_noise"
                       seed = seed+i,   # any seed 
                       noise_color = 0,   # -1.5, -1, 0, 1, 1.5
                       violet_noise = c("Geschlecht", "ALT_AST_ratio"),  # names of the columns in a vector
                       blue_noise = c("Alter"),  # names of the columns in a vector
                       white_noise = c("M65", "ALT"),   # names of the columns in a vector
                       pink_noise = c("TNFa", "AST"),  # names of the columns in a vector
                       red_noise = c("M30", "Adiponektin"),   # names of the columns in a vector
                       new_datapoints = "triple",   # "double", "triple", "quadruple" or "individual" 
                       amount_new_datapoints = 100,  # amount of new datapoints, if individual
                       balancing = TRUE,   # "balancing" = TRUE or "no_balancing" = FALSE
                       rounded_variables = c(),  # names of the columns in a vector
                       negative_values = c())
  
  pink <- DAColNoise(same_noise = TRUE,     # "same" = TRUE or "different" = FALSE
                       tbl_columns_selected = columns,   # Indices of the selected columns
                       dataset = train, 
                       tbl_classification_columns_selected = dependent_variable,   # Index of the column of the dependent variable
                       balanced_classes = balance_class,   # Name of the classes that should be balanced in vector 
                       balance_process = "balancing_with_noise",    # "oversampling" or "balancing_with_noise"
                       seed = seed+i,   # any seed 
                       noise_color = 1,   # -1.5, -1, 0, 1, 1.5
                       violet_noise = c("Geschlecht", "ALT_AST_ratio"),  # names of the columns in a vector
                       blue_noise = c("Alter"),  # names of the columns in a vector
                       white_noise = c("M65", "ALT"),   # names of the columns in a vector
                       pink_noise = c("TNFa", "AST"),  # names of the columns in a vector
                       red_noise = c("M30", "Adiponektin"),   # names of the columns in a vector
                       new_datapoints = "triple",   # "double", "triple", "quadruple" or "individual" 
                       amount_new_datapoints = 100,  # amount of new datapoints, if individual
                       balancing = TRUE,   # "balancing" = TRUE or "no_balancing" = FALSE
                       rounded_variables = c(),  # names of the columns in a vector
                       negative_values = c())
  
  red <- DAColNoise(same_noise = TRUE,     # "same" = TRUE or "different" = FALSE
                     tbl_columns_selected = columns,   # Indices of the selected columns
                     dataset = train, 
                     tbl_classification_columns_selected = dependent_variable,   # Index of the column of the dependent variable
                     balanced_classes = balance_class,   # Name of the classes that should be balanced in vector 
                     balance_process = "balancing_with_noise",    # "oversampling" or "balancing_with_noise"
                     seed = seed+i,   # any seed 
                     noise_color = 1.5,   # -1.5, -1, 0, 1, 1.5
                     violet_noise = c("Geschlecht", "ALT_AST_ratio"),  # names of the columns in a vector
                     blue_noise = c("Alter"),  # names of the columns in a vector
                     white_noise = c("M65", "ALT"),   # names of the columns in a vector
                     pink_noise = c("TNFa", "AST"),  # names of the columns in a vector
                     red_noise = c("M30", "Adiponektin"),   # names of the columns in a vector
                     new_datapoints = "triple",   # "double", "triple", "quadruple" or "individual" 
                     amount_new_datapoints = 100,  # amount of new datapoints, if individual
                     balancing = TRUE,   # "balancing" = TRUE or "no_balancing" = FALSE
                     rounded_variables = c(),  # names of the columns in a vector
                     negative_values = c())
  
  different <- DAColNoise(same_noise = FALSE,     # "same" = TRUE or "different" = FALSE
                    tbl_columns_selected = columns,   # Indices of the selected columns
                    dataset = train, 
                    tbl_classification_columns_selected = dependent_variable,   # Index of the column of the dependent variable
                    balanced_classes = balance_class,   # Name of the classes that should be balanced in vector 
                    balance_process = "balancing_with_noise",    # "oversampling" or "balancing_with_noise"
                    seed = seed+i,   # any seed 
                    noise_color = 1.5,   # -1.5, -1, 0, 1, 1.5
                    violet_noise = violet_noise,  # names of the columns in a vector
                    blue_noise = blue_noise,  # names of the columns in a vector
                    white_noise = white_noise,   # names of the columns in a vector
                    pink_noise = pink_noise,  # names of the columns in a vector
                    red_noise = red_noise,   # names of the columns in a vector
                    new_datapoints = "triple",   # "double", "triple", "quadruple" or "individual" 
                    amount_new_datapoints = 100,  # amount of new datapoints, if individual
                    balancing = TRUE,   # "balancing" = TRUE or "no_balancing" = FALSE
                    rounded_variables = c(),  # names of the columns in a vector
                    negative_values = c())
  
  
  #### Logistic Regression #####
  
  # Train model #
  model <- glm(as.formula(paste(names(train)[dependent_variable], "~.")), family = binomial, data = train)
  model_violet <- glm(as.formula(paste(names(violet)[dependent_variable], "~.")), family = binomial, data = violet)
  model_blue <- glm(as.formula(paste(names(blue)[dependent_variable], "~.")), family = binomial, data = blue)
  model_white <- glm(as.formula(paste(names(white)[dependent_variable], "~.")), family = binomial, data = white)
  model_pink <- glm(as.formula(paste(names(pink)[dependent_variable], "~.")), family = binomial, data = pink)
  model_red <- glm(as.formula(paste(names(red)[dependent_variable], "~.")), family = binomial, data = red)
  model_different <- glm(as.formula(paste(names(different)[dependent_variable], "~.")), family = binomial, data = different)
  
  
  # predict # # AUC, PR, MCC #
  fitted_results <- predict(model,newdata=subset(test,select=c(-dependent_variable)),type='response')
  fitted_results <- ifelse(fitted_results > 0.5,1,0)
  misClasificError_lr[i] <- mean(fitted_results != test[,dependent_variable])
  roc_lr <- tryCatch({pROC::roc(fitted_results, test[,dependent_variable])},
                        error=function(cond){return(NA)})
  auc_lr[i] <- tryCatch({pROC::auc(roc_lr)[1]}, error=function(cond){return(NA)})
  aupr_lr[i] <- PRROC::pr.curve(as.factor(fitted_results), test[,dependent_variable])$auc.integral
  mcc_lr[i] <- mltools::mcc(as.factor(fitted_results), as.factor(test[,dependent_variable]))
  
  fitted_results_violet <- predict(model_violet,newdata=subset(test,select=c(-dependent_variable)),type='response')
  fitted_results_violet <- ifelse(fitted_results_violet > 0.5,1,0)
  misClasificError_lr_violet[i] <- mean(fitted_results_violet != test[,dependent_variable])
  roc_violet_lr <- tryCatch({pROC::roc(fitted_results_violet, test[,dependent_variable])},
                        error=function(cond){return(NA)})
  auc_lr_violet[i] <- tryCatch({pROC::auc(roc_violet_lr)[1]}, error=function(cond){return(NA)})
  aupr_lr_violet[i] <- PRROC::pr.curve(as.factor(fitted_results_violet), test[,dependent_variable])$auc.integral
  mcc_lr_violet[i] <- mltools::mcc(as.factor(fitted_results_violet), as.factor(test[,dependent_variable]))
  
  fitted_results_blue <- predict(model_blue,newdata=subset(test,select=c(-dependent_variable)),type='response')
  fitted_results_blue <- ifelse(fitted_results_blue > 0.5,1,0)
  misClasificError_lr_blue[i] <- mean(fitted_results_blue != test[,dependent_variable])
  roc_blue_lr <- tryCatch({pROC::roc(fitted_results_blue, test[,dependent_variable])},
                            error=function(cond){return(NA)})
  auc_lr_blue[i] <- tryCatch({pROC::auc(roc_blue_lr)[1]}, error=function(cond){return(NA)})
  aupr_lr_blue[i] <- PRROC::pr.curve(as.factor(fitted_results_blue), test[,dependent_variable])$auc.integral
  mcc_lr_blue[i] <- mltools::mcc(as.factor(fitted_results_blue), as.factor(test[,dependent_variable]))
  
  fitted_results_white <- predict(model_white,newdata=subset(test,select=c(-dependent_variable)),type='response')
  fitted_results_white <- ifelse(fitted_results_white > 0.5,1,0)
  misClasificError_lr_white[i] <- mean(fitted_results_white != test[,dependent_variable])
  roc_white_lr <- tryCatch({pROC::roc(fitted_results_white, test[,dependent_variable])},
                            error=function(cond){return(NA)})
  auc_lr_white[i] <- tryCatch({pROC::auc(roc_white_lr)[1]}, error=function(cond){return(NA)})
  aupr_lr_white[i] <- PRROC::pr.curve(as.factor(fitted_results_white), test[,dependent_variable])$auc.integral
  mcc_lr_white[i] <- mltools::mcc(as.factor(fitted_results_white), as.factor(test[,dependent_variable]))
  
  fitted_results_pink <- predict(model_pink,newdata=subset(test,select=c(-dependent_variable)),type='response')
  fitted_results_pink <- ifelse(fitted_results_pink > 0.5,1,0)
  misClasificError_lr_pink[i] <- mean(fitted_results_pink != test[,dependent_variable])
  roc_pink_lr <- tryCatch({pROC::roc(fitted_results_pink, test[,dependent_variable])},
                            error=function(cond){return(NA)})
  auc_lr_pink[i] <- tryCatch({pROC::auc(roc_pink_lr)[1]}, error=function(cond){return(NA)})
  aupr_lr_pink[i] <- PRROC::pr.curve(as.factor(fitted_results_pink), test[,dependent_variable])$auc.integral
  mcc_lr_pink[i] <- mltools::mcc(as.factor(fitted_results_pink), as.factor(test[,dependent_variable]))
  
  fitted_results_red <- predict(model_red,newdata=subset(test,select=c(-dependent_variable)),type='response')
  fitted_results_red <- ifelse(fitted_results_red > 0.5,1,0)
  misClasificError_lr_red[i] <- mean(fitted_results_red != test[,dependent_variable])
  roc_red_lr <- tryCatch({pROC::roc(fitted_results_red, test[,dependent_variable])},
                            error=function(cond){return(NA)})
  auc_lr_red[i] <- tryCatch({pROC::auc(roc_red_lr)[1]}, error=function(cond){return(NA)})
  aupr_lr_red[i] <- PRROC::pr.curve(as.factor(fitted_results_red), test[,dependent_variable])$auc.integral
  mcc_lr_red[i] <- mltools::mcc(as.factor(fitted_results_red), as.factor(test[,dependent_variable]))
  
  fitted_results_different <- predict(model_different,newdata=subset(test,select=c(-dependent_variable)),type='response')
  fitted_results_different <- ifelse(fitted_results_different > 0.5,1,0)
  misClasificError_lr_different[i] <- mean(fitted_results_different != test[,dependent_variable])
  roc_different_lr <- tryCatch({pROC::roc(fitted_results_different, test[,dependent_variable])},
                          error=function(cond){return(NA)})
  auc_lr_different[i] <- tryCatch({pROC::auc(roc_different_lr)[1]}, error=function(cond){return(NA)})
  aupr_lr_different[i] <- PRROC::pr.curve(as.factor(fitted_results_different), test[,dependent_variable])$auc.integral
  mcc_lr_different[i] <- mltools::mcc(as.factor(fitted_results_different), as.factor(test[,dependent_variable]))
  
  
  #### Random Forest #####
  
  # convert response variable to factor
  train[,dependent_variable] <- as.factor(train[,dependent_variable])
  rf <- randomForest(as.formula(paste(names(train)[dependent_variable], "~.")), data = train, ntree=100, mtry=2, importance=TRUE)
  
  violet <- as.data.frame(violet)
  violet[,dependent_variable] <- as.factor(violet[,dependent_variable])
  rf_violet <- randomForest(as.formula(paste(names(violet)[dependent_variable], "~.")), data = violet, ntree=100, mtry=2, importance=TRUE)
  blue <- as.data.frame(blue)
  blue[,dependent_variable] <- as.factor(blue[,dependent_variable])
  rf_blue <- randomForest(as.formula(paste(names(blue)[dependent_variable], "~.")), data = blue, ntree=100, mtry=2, importance=TRUE)
  white <- as.data.frame(white)
  white[,dependent_variable] <- as.factor(white[,dependent_variable])
  rf_white <- randomForest(as.formula(paste(names(white)[dependent_variable], "~.")), data = white, ntree=100, mtry=2, importance=TRUE)
  pink <- as.data.frame(pink)
  pink[,dependent_variable] <- as.factor(pink[,dependent_variable])
  rf_pink <- randomForest(as.formula(paste(names(pink)[dependent_variable], "~.")), data = pink, ntree=100, mtry=2, importance=TRUE)
  red <- as.data.frame(red)
  red[,dependent_variable] <- as.factor(red[,dependent_variable])
  rf_red <- randomForest(as.formula(paste(names(red)[dependent_variable], "~.")), data = red, ntree=100, mtry=2, importance=TRUE)
  different <- as.data.frame(different)
  different[,dependent_variable] <- as.factor(different[,dependent_variable])
  rf_different <- randomForest(as.formula(paste(names(different)[dependent_variable], "~.")), data = different, ntree=100, mtry=2, importance=TRUE)
  
  # predict and quality measurements # # AUC, PR, MCC #
  prediction <- predict(rf, test[,-dependent_variable])
  misClasificError_rf[i] <- mean(prediction != test[,dependent_variable])
  roc_rf <- tryCatch({pROC::roc(prediction, test[,dependent_variable])},
                            error=function(cond){return(NA)})
  auc_rf[i] <- tryCatch({pROC::auc(roc_rf)[1]}, error=function(cond){return(NA)})
  aupr_rf[i] <- PRROC::pr.curve(prediction, test[,dependent_variable])$auc.integral
  mcc_rf[i] <- mltools::mcc(prediction, as.factor(test[,dependent_variable]))
  
  predict_violet <- predict(rf_violet, test[,-dependent_variable])
  misClasificError_rf_violet[i] <- mean(predict_violet != test[,dependent_variable])
  roc_violet_rf <- tryCatch({pROC::roc(predict_violet, test[,dependent_variable])},
                            error=function(cond){return(NA)})
  auc_rf_violet[i] <- tryCatch({pROC::auc(roc_violet_rf)[1]}, error=function(cond){return(NA)})
  aupr_rf_violet[i] <- PRROC::pr.curve(predict_violet, test[,dependent_variable])$auc.integral
  mcc_rf_violet[i] <- mltools::mcc(predict_violet, as.factor(test[,dependent_variable]))
  
  predict_blue <- predict(rf_blue, test[,-dependent_variable])
  misClasificError_rf_blue[i] <- mean(predict_blue != test[,dependent_variable])
  roc_blue_rf <- tryCatch({pROC::roc(predict_blue, test[,dependent_variable])},
                            error=function(cond){return(NA)})
  auc_rf_blue[i] <- tryCatch({pROC::auc(roc_blue_rf)[1]}, error=function(cond){return(NA)})
  aupr_rf_blue[i] <- PRROC::pr.curve(predict_blue, test[,dependent_variable])$auc.integral
  mcc_rf_blue[i] <- mltools::mcc(predict_blue, as.factor(test[,dependent_variable]))
  
  predict_white <- predict(rf_white, test[,-dependent_variable])
  misClasificError_rf_white[i] <- mean(predict_white != test[,dependent_variable])
  roc_white_rf <- tryCatch({pROC::roc(predict_white, test[,dependent_variable])},
                            error=function(cond){return(NA)})
  auc_rf_white[i] <- tryCatch({pROC::auc(roc_white_rf)[1]}, error=function(cond){return(NA)})
  aupr_rf_white[i] <- PRROC::pr.curve(predict_white, test[,dependent_variable])$auc.integral
  mcc_rf_white[i] <- mltools::mcc(predict_white, as.factor(test[,dependent_variable]))
  
  predict_pink <- predict(rf_pink, test[,-dependent_variable])
  misClasificError_rf_pink[i] <- mean(predict_pink != test[,dependent_variable])
  roc_pink_rf <- tryCatch({pROC::roc(predict_pink, test[,dependent_variable])},
                            error=function(cond){return(NA)})
  auc_rf_pink[i] <- tryCatch({pROC::auc(roc_pink_rf)[1]}, error=function(cond){return(NA)})
  aupr_rf_pink[i] <- PRROC::pr.curve(predict_pink, test[,dependent_variable])$auc.integral
  mcc_rf_pink[i] <- mltools::mcc(predict_pink, as.factor(test[,dependent_variable]))
  
  predict_red <- predict(rf_red, test[,-dependent_variable])
  misClasificError_rf_red[i] <- mean(predict_red != test[,dependent_variable])
  roc_red_rf <- tryCatch({pROC::roc(predict_red, test[,dependent_variable])},
                            error=function(cond){return(NA)})
  auc_rf_red[i] <- tryCatch({pROC::auc(roc_red_rf)[1]}, error=function(cond){return(NA)})
  aupr_rf_red[i] <- PRROC::pr.curve(predict_red, test[,dependent_variable])$auc.integral
  mcc_rf_red[i] <- mltools::mcc(predict_red, as.factor(test[,dependent_variable]))
  
  predict_different <- predict(rf_different, test[,-dependent_variable])
  misClasificError_rf_different[i] <- mean(predict_different != test[,dependent_variable])
  roc_different_rf <- tryCatch({pROC::roc(predict_different, test[,dependent_variable])},
                          error=function(cond){return(NA)})
  auc_rf_different[i] <- tryCatch({pROC::auc(roc_different_rf)[1]}, error=function(cond){return(NA)})
  aupr_rf_different[i] <- PRROC::pr.curve(predict_different, test[,dependent_variable])$auc.integral
  mcc_rf_different[i] <- mltools::mcc(predict_different, as.factor(test[,dependent_variable]))
  
}


### Plot results ###

# accuracy

models <- c(rep("LR", 7*50), rep("RF", 7*50))
method <- c(rep(c(rep("No DA",50), rep("Violet",50), rep("Blue",50), rep("White",50), rep("Pink",50), rep("Red",50), rep("Different",50)),2))
value <- c(1-misClasificError_lr, 
           1-misClasificError_lr_violet, 1-misClasificError_lr_blue,
           1-misClasificError_lr_white, 1-misClasificError_lr_pink,
           1-misClasificError_lr_red,1-misClasificError_lr_different,
           1-misClasificError_rf, 
           1-misClasificError_rf_violet, 1-misClasificError_rf_blue,
           1-misClasificError_rf_white, 1-misClasificError_rf_pink,
           1-misClasificError_rf_red, 1-misClasificError_rf_different)
accuracies <- data.frame(model = models,
                         method = method,
                         value = value)
accuracies$method <- factor(accuracies$method, levels = unique(accuracies$method))

# Plot
pred_error <- ggplot2::ggplot(accuracies, aes(fill=models, y=value, x=method)) +
  coord_cartesian(ylim=c(0.7,0.9)) +
  geom_boxplot() + 
  ggtitle("Accuracy") + theme_minimal() + 
  scale_fill_manual(values=c("#A8DADC", "#457B9D")) + labs(x="")


# AUC

models <- c(rep("LR", 7*50), rep("RF", 7*50))
method <- c(rep(c(rep("No DA",50), rep("Violet",50), rep("Blue",50), rep("White",50), rep("Pink",50), rep("Red",50), rep("Different",50)),2))
value <- c(auc_lr, 
           auc_lr_violet, auc_lr_blue,
           auc_lr_white, auc_lr_pink,
           auc_lr_red, auc_lr_different,
           auc_rf, 
           auc_rf_violet, auc_rf_blue,
           auc_rf_white, auc_rf_pink,
           auc_rf_red, auc_rf_different)
auc <- data.frame(model = models,
                  method = method,
                  value = value)
auc$method <- factor(auc$method, levels = unique(auc$method))

# Plot 
auc_plot <- ggplot2::ggplot(auc, aes(fill=models, y=value, x=method)) + 
  geom_boxplot() + coord_cartesian(ylim=c(0.4,1)) +
  ggtitle("Area under Curve") + theme_minimal() + 
  scale_fill_manual(values=c("#A8DADC", "#457B9D")) + labs(x="")


# AUPR

models <- c(rep("LR", 7*50), rep("RF", 7*50))
method <- c(rep(c(rep("No DA",50), rep("Violet",50), rep("Blue",50), rep("White",50), rep("Pink",50), rep("Red",50), rep("Different",50)),2))
value <- c(aupr_lr, 
           aupr_lr_violet, aupr_lr_blue,
           aupr_lr_white, aupr_lr_pink,
           aupr_lr_red, aupr_lr_different,
           aupr_rf, 
           aupr_rf_violet, aupr_rf_blue,
           aupr_rf_white, aupr_rf_pink,
           aupr_rf_red, aupr_rf_different)
aupr <- data.frame(model = models,
                  method = method,
                  value = value)
aupr$method <- factor(aupr$method, levels = unique(aupr$method))

# Plot 
aupr_plot <- ggplot2::ggplot(aupr, aes(fill=models, y=value, x=method)) + 
  geom_boxplot() + coord_cartesian(ylim=c(0.9,0.96)) +
  ggtitle("Area under Precision Recall") + theme_minimal() + 
  scale_fill_manual(values=c("#A8DADC", "#457B9D")) + labs(x="")



# MCC

models <- c(rep("LR", 7*50), rep("RF", 7*50))
method <- c(rep(c(rep("No DA",50), rep("Violet",50), rep("Blue",50), rep("White",50), rep("Pink",50), rep("Red",50), rep("Different",50)),2))
value <- c(mcc_lr, 
           mcc_lr_violet, mcc_lr_blue,
           mcc_lr_white, mcc_lr_pink,
           mcc_lr_red, mcc_lr_different,
           mcc_rf, 
           mcc_rf_violet, mcc_rf_blue,
           mcc_rf_white, mcc_rf_pink,
           mcc_rf_red, mcc_rf_different)
mcc <- data.frame(model = models,
                   method = method,
                   value = value)
mcc$method <- factor(mcc$method, levels = unique(mcc$method))

# Plot 
mcc_plot <- ggplot2::ggplot(mcc, aes(fill=models, y=value, x=method)) + 
  geom_boxplot() + coord_cartesian(ylim=c(-0.05,0.5)) +
  ggtitle("Matthew's correlation coefficient") + theme_minimal() + 
  scale_fill_manual(values=c("#A8DADC", "#457B9D")) + labs(x="")


# plot all
cowplot::plot_grid(pred_error, auc_plot, aupr_plot, mcc_plot, ncol = 2, nrow = 2)


# check whether there is a significant difference in the mean of no Data Augmentation to any Color of Noise
### Significance ###

# accuracy
p_values <- c(1,t.test(1-misClasificError_lr, 1-misClasificError_lr_violet)$p.value,
              t.test(1-misClasificError_lr, 1-misClasificError_lr_blue)$p.value,
              t.test(1-misClasificError_lr, 1-misClasificError_lr_white)$p.value,
              t.test(1-misClasificError_lr, 1-misClasificError_lr_pink)$p.value,
              t.test(1-misClasificError_lr, 1-misClasificError_lr_red)$p.value,
              t.test(1-misClasificError_lr, 1-misClasificError_lr_different)$p.value,1,
              t.test(1-misClasificError_rf, 1-misClasificError_rf_violet)$p.value,
              t.test(1-misClasificError_rf, 1-misClasificError_rf_blue)$p.value,
              t.test(1-misClasificError_rf, 1-misClasificError_rf_white)$p.value,
              t.test(1-misClasificError_rf, 1-misClasificError_rf_pink)$p.value,
              t.test(1-misClasificError_rf, 1-misClasificError_rf_red)$p.value,
              t.test(1-misClasificError_rf, 1-misClasificError_rf_different)$p.value)
p_values_wn <- c(1,t.test(1-misClasificError_lr_white, 1-misClasificError_lr_violet)$p.value,
              t.test(1-misClasificError_lr_white, 1-misClasificError_lr_blue)$p.value,1,
              t.test(1-misClasificError_lr_white, 1-misClasificError_lr_pink)$p.value,
              t.test(1-misClasificError_lr_white, 1-misClasificError_lr_red)$p.value,
              t.test(1-misClasificError_lr_white, 1-misClasificError_lr_different)$p.value,1,
              t.test(1-misClasificError_rf_white, 1-misClasificError_rf_violet)$p.value,
              t.test(1-misClasificError_rf_white, 1-misClasificError_rf_blue)$p.value,1,
              t.test(1-misClasificError_rf_white, 1-misClasificError_rf_pink)$p.value,
              t.test(1-misClasificError_rf_white, 1-misClasificError_rf_red)$p.value,
              t.test(1-misClasificError_rf_white, 1-misClasificError_rf_different)$p.value)
means = c(1-mean(na.omit(misClasificError_lr)), 1-mean(na.omit(misClasificError_lr_violet)), 1-mean(na.omit(misClasificError_lr_blue)), 1-mean(na.omit(misClasificError_lr_white)),
          1-mean(na.omit(misClasificError_lr_pink)), 1-mean(na.omit(misClasificError_lr_red)), 1-mean(na.omit(misClasificError_lr_different)), 1-mean(na.omit(misClasificError_rf)), 1-mean(na.omit(misClasificError_rf_violet)), 
          1-mean(na.omit(misClasificError_rf_blue)), 1-mean(na.omit(misClasificError_rf_white)), 1-mean(na.omit(misClasificError_rf_pink)),
          1-mean(na.omit(misClasificError_rf_red)), 1-mean(na.omit(misClasificError_rf_different)))
models <- c(rep("LR", 7), rep("RF", 7))
method <- c(rep(c("No DA", "Violet", "Blue", "White", "Pink", "Red", "Different"),2))
accuracy_p_values <- data.frame(model = models,
                                method = method,
                                p_value = p_values,
                                mean = round(means,3),
                                significance = rep("",14),
                                p_value_white = p_values_wn,
                                significance_wn = rep("",14))
for (index in 1:7) {
  if (accuracy_p_values$p_value[index] < 0.1 && accuracy_p_values$mean[1] < accuracy_p_values$mean[index]) {accuracy_p_values$significance[index] = "*"}
  if (accuracy_p_values$p_value[index] < 0.05 && accuracy_p_values$mean[1] < accuracy_p_values$mean[index]) {accuracy_p_values$significance[index] = "**"}
  if (accuracy_p_values$p_value[index] < 0.01 && accuracy_p_values$mean[1] < accuracy_p_values$mean[index]) {accuracy_p_values$significance[index] = "***"}
}
for (index in 8:14) {
  if (accuracy_p_values$p_value[index] < 0.1 && accuracy_p_values$mean[8] < accuracy_p_values$mean[index]) {accuracy_p_values$significance[index] = "*"}
  if (accuracy_p_values$p_value[index] < 0.05 && accuracy_p_values$mean[8] < accuracy_p_values$mean[index]) {accuracy_p_values$significance[index] = "**"}
  if (accuracy_p_values$p_value[index] < 0.01 && accuracy_p_values$mean[8] < accuracy_p_values$mean[index]) {accuracy_p_values$significance[index] = "***"}
}

for (index in 1:7) {
  if (accuracy_p_values$p_value_white[index] < 0.1 && accuracy_p_values$mean[4] < accuracy_p_values$mean[index]) {accuracy_p_values$significance_wn[index] = "*"}
  if (accuracy_p_values$p_value_white[index] < 0.05 && accuracy_p_values$mean[4] < accuracy_p_values$mean[index]) {accuracy_p_values$significance_wn[index] = "**"}
  if (accuracy_p_values$p_value_white[index] < 0.01 && accuracy_p_values$mean[4] < accuracy_p_values$mean[index]) {accuracy_p_values$significance_wn[index] = "***"}
}
for (index in 8:14) {
  if (accuracy_p_values$p_value_white[index] < 0.1 && accuracy_p_values$mean[11] < accuracy_p_values$mean[index]) {accuracy_p_values$significance_wn[index] = "*"}
  if (accuracy_p_values$p_value_white[index] < 0.05 && accuracy_p_values$mean[11] < accuracy_p_values$mean[index]) {accuracy_p_values$significance_wn[index] = "**"}
  if (accuracy_p_values$p_value_white[index] < 0.01 && accuracy_p_values$mean[11] < accuracy_p_values$mean[index]) {accuracy_p_values$significance_wn[index] = "***"}
}
accuracy_p_values = accuracy_p_values[,c(1,2,4,5,7)]


# AUC
p_values_wn <- c(1,t.test(auc_lr_white, auc_lr_violet)$p.value,
              t.test(auc_lr_white, auc_lr_blue)$p.value,1,
              t.test(auc_lr_white, auc_lr_pink)$p.value,
              t.test(auc_lr_white, auc_lr_red)$p.value,
              t.test(auc_lr_white, auc_lr_different)$p.value,1,
              t.test(auc_rf_white, auc_rf_violet)$p.value,
              t.test(auc_rf_white, auc_rf_blue)$p.value,1,
              t.test(auc_rf_white, auc_rf_pink)$p.value,
              t.test(auc_rf_white, auc_rf_red)$p.value,
              t.test(auc_rf_white, auc_rf_different)$p.value)
p_values <- c(1,t.test(auc_lr, auc_lr_violet)$p.value,
              t.test(auc_lr, auc_lr_blue)$p.value,
              t.test(auc_lr, auc_lr_white)$p.value,
              t.test(auc_lr, auc_lr_pink)$p.value,
              t.test(auc_lr, auc_lr_red)$p.value,
              t.test(auc_lr, auc_lr_different)$p.value,1,
              t.test(auc_rf, auc_rf_violet)$p.value,
              t.test(auc_rf, auc_rf_blue)$p.value,
              t.test(auc_rf, auc_rf_white)$p.value,
              t.test(auc_rf, auc_rf_pink)$p.value,
              t.test(auc_rf, auc_rf_red)$p.value,
              t.test(auc_rf, auc_rf_different)$p.value)
means = c(mean(na.omit(auc_lr)), mean(na.omit(auc_lr_violet)), mean(na.omit(auc_lr_blue)), mean(na.omit(auc_lr_white)),
          mean(na.omit(auc_lr_pink)), mean(na.omit(auc_lr_red)), mean(na.omit(auc_lr_different)), mean(na.omit(auc_rf)), mean(na.omit(auc_rf_violet)), 
          mean(na.omit(auc_rf_blue)), mean(na.omit(auc_rf_white)), mean(na.omit(auc_rf_pink)),
          mean(na.omit(auc_rf_red)), mean(na.omit(auc_rf_different)))
models <- c(rep("LR", 7), rep("RF", 7))
method <- c(rep(c("No DA", "Violet", "Blue", "White", "Pink", "Red", "Different"),2))
auc_p_values <- data.frame(model = models,
                           method = method,
                           p_values = p_values,
                           mean = round(means,3),
                           significance = rep("",14),
                           p_value_white = p_values_wn,
                           significance_wn = rep("",14))
for (index in 1:7) {
  if (auc_p_values$p_value[index] < 0.1 && auc_p_values$mean[1] < auc_p_values$mean[index]) {auc_p_values$significance[index] = "*"}
  if (auc_p_values$p_value[index] < 0.05 && auc_p_values$mean[1] < auc_p_values$mean[index]) {auc_p_values$significance[index] = "**"}
  if (auc_p_values$p_value[index] < 0.01 && auc_p_values$mean[1] < auc_p_values$mean[index]) {auc_p_values$significance[index] = "***"}
}
for (index in 8:14) {
  if (auc_p_values$p_value[index] < 0.1 && auc_p_values$mean[8] < auc_p_values$mean[index]) {auc_p_values$significance[index] = "*"}
  if (auc_p_values$p_value[index] < 0.05 && auc_p_values$mean[8] < auc_p_values$mean[index]) {auc_p_values$significance[index] = "**"}
  if (auc_p_values$p_value[index] < 0.01 && auc_p_values$mean[8] < auc_p_values$mean[index]) {auc_p_values$significance[index] = "***"}
}

for (index in 1:7) {
  if (auc_p_values$p_value_white[index] < 0.1 && auc_p_values$mean[4] < auc_p_values$mean[index]) {auc_p_values$significance_wn[index] = "*"}
  if (auc_p_values$p_value_white[index] < 0.05 && auc_p_values$mean[4] < auc_p_values$mean[index]) {auc_p_values$significance_wn[index] = "**"}
  if (auc_p_values$p_value_white[index] < 0.01 && auc_p_values$mean[4] < auc_p_values$mean[index]) {auc_p_values$significance_wn[index] = "***"}
}
for (index in 8:14) {
  if (auc_p_values$p_value_white[index] < 0.1 && auc_p_values$mean[11] < auc_p_values$mean[index]) {auc_p_values$significance_wn[index] = "*"}
  if (auc_p_values$p_value_white[index] < 0.05 && auc_p_values$mean[11] < auc_p_values$mean[index]) {auc_p_values$significance_wn[index] = "**"}
  if (auc_p_values$p_value_white[index] < 0.01 && auc_p_values$mean[11] < auc_p_values$mean[index]) {auc_p_values$significance_wn[index] = "***"}
}
auc_p_values = auc_p_values[,c(1,2,4,5,7)]


# AUPR
p_values <- c(1,t.test(aupr_lr, aupr_lr_violet)$p.value,
              t.test(aupr_lr, aupr_lr_blue)$p.value,
              t.test(aupr_lr, aupr_lr_white)$p.value,
              t.test(aupr_lr, aupr_lr_pink)$p.value,
              t.test(aupr_lr, aupr_lr_red)$p.value,
              t.test(aupr_lr, aupr_lr_different)$p.value,1,
              t.test(aupr_rf, aupr_rf_violet)$p.value,
              t.test(aupr_rf, aupr_rf_blue)$p.value,
              t.test(aupr_rf, aupr_rf_white)$p.value,
              t.test(aupr_rf, aupr_rf_pink)$p.value,
              t.test(aupr_rf, aupr_rf_red)$p.value,
              t.test(aupr_rf, aupr_rf_different)$p.value)
p_values_wn <- c(1,t.test(aupr_lr_white, aupr_lr_violet)$p.value,
                 t.test(aupr_lr_white, aupr_lr_blue)$p.value,1,
                 t.test(aupr_lr_white, aupr_lr_pink)$p.value,
                 t.test(aupr_lr_white, aupr_lr_red)$p.value,
                 t.test(aupr_lr_white, aupr_lr_different)$p.value,1,
                 t.test(aupr_rf_white, aupr_rf_violet)$p.value,
                 t.test(aupr_rf_white, aupr_rf_blue)$p.value,1,
                 t.test(aupr_rf_white, aupr_rf_pink)$p.value,
                 t.test(aupr_rf_white, aupr_rf_red)$p.value,
                 t.test(aupr_rf_white, aupr_rf_different)$p.value)
models <- c(rep("LR", 7), rep("RF", 7))
method <- c(rep(c("No DA", "Violet", "Blue", "White", "Pink", "Red", "Different"),2))
means = c(mean(na.omit(aupr_lr)), mean(na.omit(aupr_lr_violet)), mean(na.omit(aupr_lr_blue)), mean(na.omit(aupr_lr_white)),
          mean(na.omit(aupr_lr_pink)), mean(na.omit(aupr_lr_red)), mean(na.omit(aupr_lr_different)), mean(na.omit(aupr_rf)), mean(na.omit(aupr_rf_violet)), 
          mean(na.omit(aupr_rf_blue)), mean(na.omit(aupr_rf_white)), mean(na.omit(aupr_rf_pink)),
          mean(na.omit(aupr_rf_red)), mean(na.omit(aupr_rf_different)))
aupr_p_values <- data.frame(model = models,
                           method = method,
                           p_value = p_values,
                           mean = round(means,3),
                           significance = rep("",14),
                           p_value_white = p_values_wn,
                           significance_wn = rep("",14))
for (index in 1:7) {
  if (aupr_p_values$p_value[index] < 0.1 && aupr_p_values$mean[1] < aupr_p_values$mean[index]) {aupr_p_values$significance[index] = "*"}
  if (aupr_p_values$p_value[index] < 0.05 && aupr_p_values$mean[1] < aupr_p_values$mean[index]) {aupr_p_values$significance[index] = "**"}
  if (aupr_p_values$p_value[index] < 0.01 && aupr_p_values$mean[1] < aupr_p_values$mean[index]) {aupr_p_values$significance[index] = "***"}
}
for (index in 8:14) {
  if (aupr_p_values$p_value[index] < 0.1 && aupr_p_values$mean[8] < aupr_p_values$mean[index]) {aupr_p_values$significance[index] = "*"}
  if (aupr_p_values$p_value[index] < 0.05 && aupr_p_values$mean[8] < aupr_p_values$mean[index]) {aupr_p_values$significance[index] = "**"}
  if (aupr_p_values$p_value[index] < 0.01 && aupr_p_values$mean[8] < aupr_p_values$mean[index]) {aupr_p_values$significance[index] = "***"}
}

for (index in 1:7) {
  if (aupr_p_values$p_value_white[index] < 0.1) {aupr_p_values$significance_wn[index] = "*"}
  if (aupr_p_values$p_value_white[index] < 0.05) {aupr_p_values$significance_wn[index] = "**"}
  if (aupr_p_values$p_value_white[index] < 0.01) {aupr_p_values$significance_wn[index] = "***"}
}
for (index in 8:14) {
  if (aupr_p_values$p_value_white[index] < 0.1) {aupr_p_values$significance_wn[index] = "*"}
  if (aupr_p_values$p_value_white[index] < 0.05) {aupr_p_values$significance_wn[index] = "**"}
  if (aupr_p_values$p_value_white[index] < 0.01) {aupr_p_values$significance_wn[index] = "***"}
}
aupr_p_values = aupr_p_values[,c(1,2,4,5,7)]



# MCC
p_values <- c(1,t.test(mcc_lr, mcc_lr_violet)$p.value,
              t.test(mcc_lr, mcc_lr_blue)$p.value,
              t.test(mcc_lr, mcc_lr_white)$p.value,
              t.test(mcc_lr, mcc_lr_pink)$p.value,
              t.test(mcc_lr, mcc_lr_red)$p.value,
              t.test(mcc_lr, mcc_lr_different)$p.value,1,
              t.test(mcc_rf, mcc_rf_violet)$p.value,
              t.test(mcc_rf, mcc_rf_blue)$p.value,
              t.test(mcc_rf, mcc_rf_white)$p.value,
              t.test(mcc_rf, mcc_rf_pink)$p.value,
              t.test(mcc_rf, mcc_rf_red)$p.value,
              t.test(mcc_rf, mcc_rf_different)$p.value)
p_values_wn <- c(1,t.test(mcc_lr_white, mcc_lr_violet)$p.value,
                 t.test(mcc_lr_white, mcc_lr_blue)$p.value,1,
                 t.test(mcc_lr_white, mcc_lr_pink)$p.value,
                 t.test(mcc_lr_white, mcc_lr_red)$p.value,
                 t.test(mcc_lr_white, mcc_lr_different)$p.value,1,
                 t.test(mcc_rf_white, mcc_rf_violet)$p.value,
                 t.test(mcc_rf_white, mcc_rf_blue)$p.value,1,
                 t.test(mcc_rf_white, mcc_rf_pink)$p.value,
                 t.test(mcc_rf_white, mcc_rf_red)$p.value,
                 t.test(mcc_rf_white, mcc_rf_different)$p.value)
means = c(mean(na.omit(mcc_lr)),mean(na.omit(mcc_lr_violet)), mean(na.omit(mcc_lr_blue)), mean(na.omit(mcc_lr_white)),
          mean(na.omit(mcc_lr_pink)), mean(na.omit(mcc_lr_red)), mean(na.omit(mcc_lr_different)), mean(na.omit(mcc_rf)), mean(na.omit(mcc_rf_violet)), 
          mean(na.omit(mcc_rf_blue)), mean(na.omit(mcc_rf_white)), mean(na.omit(mcc_rf_pink)),
          mean(na.omit(mcc_rf_red)), mean(na.omit(mcc_rf_different)))
models <- c(rep("LR", 7), rep("RF", 7))
method <- c(rep(c("No DA", "Violet", "Blue", "White", "Pink", "Red", "Different"),2))
mcc_p_values <- data.frame(model = models,
                           method = method,
                           p_value = p_values,
                           mean = round(means,3),
                           significance = rep("",14),
                           p_value_white = p_values_wn,
                           significance_wn = rep("",14))
for (index in 1:7) {
  if (mcc_p_values$p_value[index] < 0.1 && mcc_p_values$mean[1] < mcc_p_values$mean[index]) {mcc_p_values$significance[index] = "*"}
  if (mcc_p_values$p_value[index] < 0.05 && mcc_p_values$mean[1] < mcc_p_values$mean[index]) {mcc_p_values$significance[index] = "**"}
  if (mcc_p_values$p_value[index] < 0.01 && mcc_p_values$mean[1] < mcc_p_values$mean[index]) {mcc_p_values$significance[index] = "***"}
}
for (index in 8:14) {
  if (mcc_p_values$p_value[index] < 0.1 && mcc_p_values$mean[8] < mcc_p_values$mean[index]) {mcc_p_values$significance[index] = "*"}
  if (mcc_p_values$p_value[index] < 0.05 && mcc_p_values$mean[8] < mcc_p_values$mean[index]) {mcc_p_values$significance[index] = "**"}
  if (mcc_p_values$p_value[index] < 0.01 && mcc_p_values$mean[8] < mcc_p_values$mean[index]) {mcc_p_values$significance[index] = "***"}
}

for (index in 1:7) {
  if (mcc_p_values$p_value_white[index] < 0.1 && mcc_p_values$mean[4] < mcc_p_values$mean[index]) {mcc_p_values$significance_wn[index] = "*"}
  if (mcc_p_values$p_value_white[index] < 0.05 && mcc_p_values$mean[4] < mcc_p_values$mean[index]) {mcc_p_values$significance_wn[index] = "**"}
  if (mcc_p_values$p_value_white[index] < 0.01 && mcc_p_values$mean[4] < mcc_p_values$mean[index]) {mcc_p_values$significance_wn[index] = "***"}
}
for (index in 8:14) {
  if (mcc_p_values$p_value_white[index] < 0.1 && mcc_p_values$mean[11] < mcc_p_values$mean[index]) {mcc_p_values$significance_wn[index] = "*"}
  if (mcc_p_values$p_value_white[index] < 0.05 && mcc_p_values$mean[11] < mcc_p_values$mean[index]) {mcc_p_values$significance_wn[index] = "**"}
  if (mcc_p_values$p_value_white[index] < 0.01 && mcc_p_values$mean[11] < mcc_p_values$mean[index]) {mcc_p_values$significance_wn[index] = "***"}
}
mcc_p_values = mcc_p_values[,c(1,2,4,5,7)]


# print all significance tables
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
      else if (j == 5 && i == 1){string_1 = paste(string_1, " & ", " --- ")}
      else if (j == 5 && i == 8){string_1 = paste(string_1, " & ", " --- ")}
      else if (j == 4 && i == 8){string_1 = paste(string_1, " & ", " --- ")}
      else if (j == 5 && i == 1){string_1 = paste(string_1, " & ", " --- ")}
      else if (j == 5 && i == 4){string_1 = paste(string_1, " & ", " --- ")}
      else if (j == 5 && i == 11){string_1 = paste(string_1, " & ", " --- ")}
      else{string_1 = paste(string_1, " & ", accuracy_p_values[i,j])}
      }
    if (j == 1) {string_1 = paste(string_1, accuracy_p_values[i,j])}
  }
  
  for (j in 1:ncol(accuracy_p_values)){
    if (j == 4 && i == 1){string_1 = paste(string_1, " & ", " --- ")}
    else if (j == 5 && i == 1){string_1 = paste(string_1, " & ", " --- ")}
    else if (j == 5 && i == 8){string_1 = paste(string_1, " & ", " --- ")}
    else if (j == 4 && i == 8){string_1 = paste(string_1, " & ", " --- ")}
    else if (j == 5 && i == 1){string_1 = paste(string_1, " & ", " --- ")}
    else if (j == 5 && i == 4){string_1 = paste(string_1, " & ", " --- ")}
    else if (j == 5 && i == 11){string_1 = paste(string_1, " & ", " --- ")}
    else{string_1 = paste(string_1, " & ", auc_p_values[i,j])}
  }
  string_1 = paste(string_1, " \\ ")
  
  for (j in 1:ncol(accuracy_p_values)){
    if (j != 1) {
      if (j == 4 && i == 1){string_3 = paste(string_3, " & ", " --- ")}
      else if (j == 5 && i == 1){string_3 = paste(string_3, " & ", " --- ")}
      else if (j == 5 && i == 8){string_3 = paste(string_3, " & ", " --- ")}
      else if (j == 4 && i == 8){string_3 = paste(string_3, " & ", " --- ")}
      else if (j == 5 && i == 1){string_3 = paste(string_3, " & ", " --- ")}
      else if (j == 5 && i == 4){string_3 = paste(string_3, " & ", " --- ")}
      else if (j == 5 && i == 11){string_3 = paste(string_3, " & ", " --- ")}
      else{string_3 = paste(string_3, " & ", aupr_p_values[i,j])}
    }
    if (j == 1) {string_3 = paste(string_3, aupr_p_values[i,j])}
  }
  
  for (j in 1:ncol(accuracy_p_values)){
    if (j == 4 && i == 1){string_3 = paste(string_3, " & ", " --- ")}
    else if (j == 5 && i == 1){string_3 = paste(string_3, " & ", " --- ")}
    else if (j == 5 && i == 8){string_3 = paste(string_3, " & ", " --- ")}
    else if (j == 4 && i == 8){string_3 = paste(string_3, " & ", " --- ")}
    else if (j == 5 && i == 1){string_3 = paste(string_3, " & ", " --- ")}
    else if (j == 5 && i == 4){string_3 = paste(string_3, " & ", " --- ")}
    else if (j == 5 && i == 11){string_3 = paste(string_3, " & ", " --- ")}
    else{string_3 = paste(string_3, " & ", mcc_p_values[i,j])}
  }
  string_3 = paste(string_3, " \\ ")
}

string = paste(string_1, "\\hline \\hline  \textbf{model} & \textbf{method} & \textbf{AUPR} &  & & \textbf{model} & \textbf{method} & \textbf{MCC} & & \\ \\hline", string_3,  "\\hline")
print(string)



  