source("DataAugmentationWithColoredNoise.R")
source('DAColNoise.R')

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
nrow_original = nrow(dataset)

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
nrow_original = nrow(dataset)

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
nrow_original = nrow(dataset)

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
nrow_original = nrow(dataset)

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
nrow_original = nrow(dataset)

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
nrow_original = nrow(dataset)

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
nrow_original = nrow(dataset)

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
nrow_original = nrow(dataset)

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
nrow_original = nrow(dataset)

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
nrow_original = nrow(dataset)

seed = 45367

# Mean and variance
differences = data.frame(matrix(NA, nrow = ncol(original_data), ncol = 6*6))
for (col in 1:5){
  if (col == 1){noise = -1.5}
  if (col == 2){noise = -1}
  if (col == 3){noise = 0}
  if (col == 4){noise = 1}
  if (col == 5){noise = 1.5}
  augmented_data <- DAColNoise(same_noise = TRUE,     # "same" = TRUE or "different" = FALSE
                               tbl_columns_selected = columns,   # Indices of the selected columns
                               dataset = dataset, 
                               tbl_classification_columns_selected = dependent_variable,   # Index of the column of the dependent variable
                               balanced_classes = balance_class,   # Name of the classes that should be balanced in vector 
                               balance_process = "balancing_with_noise",    # "oversampling" or "balancing_with_noise"
                               direction_of_generating = "vertical",    # "vertical" or "horizontal"
                               seed = seed,   # any seed 
                               noise_color = noise,   # -1.5, -1, 0, 1, 1.5
                               violet_noise = violet,  # names of the columns in a vector
                               blue_noise = blue,  # names of the columns in a vector
                               white_noise = white,   # names of the columns in a vector
                               pink_noise = pink,  # names of the columns in a vector
                               red_noise = red,   # names of the columns in a vector
                               new_datapoints = "double",   # "double", "triple", "quadruple" or "individual" 
                               amount_new_datapoints = 100,  # amount of new datapoints, if individual
                               balancing = FALSE,   # "balancing" = TRUE or "no_balancing" = FALSE
                               replacement = "no_replacement",
                               rounded_variables = c(),  # names of the columns in a vector
                               negative_values = c())   # names of the columns in a vector
  
  # divided the augmented dataset into original and fake
  original_data <- augmented_data[1:nrow_original,]
  fake_data <- augmented_data[nrow_original+1:nrow(augmented_data)-nrow_original,]
  
  
  for (i in 1:ncol(original_data)){
    differences[i,(col-1)*6+1] = colMeans(original_data)[[i]] - colMeans(fake_data)[[i]]
    differences[i,(col-1)*6+2] = t.test(original_data[, ..i], fake_data[, ..i])$p.value
    if (differences[i,(col-1)*6+2] < 0.1) {differences[i,(col-1)*6+3] = "*"}
    if (differences[i,(col-1)*6+2] < 0.05) {differences[i,(col-1)*6+3] = "**"}
    if (differences[i,(col-1)*6+2] < 0.01) {differences[i,(col-1)*6+3] = "***"}
    if (differences[i,(col-1)*6+2] >= 0.1) {differences[i,(col-1)*6+3] = " "}
    differences[i,(col-1)*6+4] = (apply(original_data, 2, var) - apply(fake_data, 2, var))[[i]]
    differences[i,(col-1)*6+5] = var.test(as.data.frame(original_data)[,i], as.data.frame(fake_data)[,i])$p.value
    if (differences[i,(col-1)*6+5] < 0.1) {differences[i,(col-1)*6+6] = "*"}
    if (differences[i,(col-1)*6+5] < 0.05) {differences[i,(col-1)*6+6] = "**"}
    if (differences[i,(col-1)*6+5] < 0.01) {differences[i,(col-1)*6+6] = "***"}
    if (differences[i,(col-1)*6+5] >= 0.1) {differences[i,(col-1)*6+6] = " "}
  }
  
}

augmented_data <- DAColNoise(same_noise = FALSE,     # "same" = TRUE or "different" = FALSE
                             tbl_columns_selected = columns,   # Indices of the selected columns
                             dataset = dataset, 
                             tbl_classification_columns_selected = dependent_variable,   # Index of the column of the dependent variable
                             balanced_classes = balance_class,   # Name of the classes that should be balanced in vector 
                             balance_process = "balancing_with_noise",    # "oversampling" or "balancing_with_noise"
                             direction_of_generating = "vertical",    # "vertical" or "horizontal"
                             seed = seed,   # any seed 
                             noise_color = noise,   # -1.5, -1, 0, 1, 1.5
                             violet_noise = violet,  # names of the columns in a vector
                             blue_noise = blue,  # names of the columns in a vector
                             white_noise = white,   # names of the columns in a vector
                             pink_noise = pink,  # names of the columns in a vector
                             red_noise = red,   # names of the columns in a vector
                             new_datapoints = "quadruple",   # "double", "triple", "quadruple" or "individual" 
                             amount_new_datapoints = 100,  # amount of new datapoints, if individual
                             balancing = FALSE,   # "balancing" = TRUE or "no_balancing" = FALSE
                             replacement = "no_replacement",
                             rounded_variables = c(),  # names of the columns in a vector
                             negative_values = c())   # names of the columns in a vector

# divided the augmented dataset into original and fake
original_data <- augmented_data[1:nrow_original,]
fake_data <- augmented_data[nrow_original+1:nrow(augmented_data)-nrow_original,]

col = 6
for (i in 1:ncol(original_data)){
  differences[i,(col-1)*6+1] = colMeans(original_data)[[i]] - colMeans(fake_data)[[i]]
  differences[i,(col-1)*6+2] = t.test(original_data[, ..i], fake_data[, ..i])$p.value
  if (differences[i,(col-1)*6+2] < 0.1) {differences[i,(col-1)*6+3] = "*"}
  if (differences[i,(col-1)*6+2] < 0.05) {differences[i,(col-1)*6+3] = "**"}
  if (differences[i,(col-1)*6+2] < 0.01) {differences[i,(col-1)*6+3] = "***"}
  if (differences[i,(col-1)*6+2] >= 0.1) {differences[i,(col-1)*6+3] = " "}
  differences[i,(col-1)*6+4] = (apply(original_data, 2, var) - apply(fake_data, 2, var))[[i]]
  differences[i,(col-1)*6+5] = var.test(as.data.frame(original_data)[,i], as.data.frame(fake_data)[,i])$p.value
  if (differences[i,(col-1)*6+5] < 0.1) {differences[i,(col-1)*6+6] = "*"}
  if (differences[i,(col-1)*6+5] < 0.05) {differences[i,(col-1)*6+6] = "**"}
  if (differences[i,(col-1)*6+5] < 0.01) {differences[i,(col-1)*6+6] = "***"}
  if (differences[i,(col-1)*6+5] >= 0.1) {differences[i,(col-1)*6+6] = " "}
}

differences = differences[,c(1,3,4,6,7,9,10,12,13,15,16,18,19,21,22,24,25,27,28,30,31,33,34,36)]
colnames(differences) = c("mean_vio", "p_value_vm", "variance_vio", "p_value_vv",
                          "mean_blu", "p_value_bm", "variance_blu", "p_value_bv",
                          "mean_whi", "p_value_wm", "variance_whi", "p_value_wv",
                          "mean_pin", "p_value_pm", "variance_pin", "p_value_pv",
                          "mean_red", "p_value_rm", "variance_red", "p_value_rv",
                          "mean_dif", "p_value_dm", "variance_dif", "p_value_dv")
differences = differences %>% 
  mutate_if(is.numeric, round, digits=3)

string = ""
for (row in 1:ncol(dataset)){
  k = row
  string_mean = gsub(" ","", paste("mean(",substr(colnames(dataset)[row],1,5),") &"))
  string_var = gsub(" ", "", paste("var(", substr(colnames(dataset)[row],1,5), ") &"))
  i = 1
  while (i < 25) {
    if (i != 1) {string_mean = paste(string_mean, " & ")}
    string_mean = paste(string_mean, differences[k,i], differences[k,i+1])
    i = i+2
    if (i == 23){string_mean = paste(string_mean, " \\ ")}
    
    if (i != 3) {string_var = paste(string_var, " & ")}
    string_var = paste(string_var, differences[k,i], differences[k,i+1])
    i = i+2
    if (i == 25){string_var = paste(string_var, " \\  ")}
  }
  
  string = paste(string, string_mean, string_var, "\\hline")
}


####################### correlation ###############################


seed = 45367

# correlation
differences_col = data.frame(matrix(NA, nrow = 2, ncol = 6))
for (col in 1:5){
  if (col == 1){noise = -1.5}
  if (col == 2){noise = -1}
  if (col == 3){noise = 0}
  if (col == 4){noise = 1}
  if (col == 5){noise = 1.5}
  
  augmented_data <- DAColNoise(same_noise = TRUE,     # "same" = TRUE or "different" = FALSE
                               tbl_columns_selected = columns,   # Indices of the selected columns
                               dataset = dataset, 
                               tbl_classification_columns_selected = dependent_variable,   # Index of the column of the dependent variable
                               balanced_classes = balance_class,   # Name of the classes that should be balanced in vector 
                               balance_process = "balancing_with_noise",    # "oversampling" or "balancing_with_noise"
                               direction_of_generating = "vertical",    # "vertical" or "horizontal"
                               seed = seed,   # any seed 
                               noise_color = noise,   # -1.5, -1, 0, 1, 1.5
                               violet_noise = violet,  # names of the columns in a vector
                               blue_noise = blue,  # names of the columns in a vector
                               white_noise = white,   # names of the columns in a vector
                               pink_noise = pink,  # names of the columns in a vector
                               red_noise = red,   # names of the columns in a vector
                               new_datapoints = "double",   # "double", "triple", "quadruple" or "individual" 
                               amount_new_datapoints = 100,  # amount of new datapoints, if individual
                               balancing = FALSE,   # "balancing" = TRUE or "no_balancing" = FALSE
                               replacement = "no_replacement",
                               rounded_variables = c(),  # names of the columns in a vector
                               negative_values = c())   # names of the columns in a vector
  
  # divided the augmented dataset into original and fake
  original_data_v <- augmented_data[1:nrow_original,]
  fake_data_v <- augmented_data[nrow_original+1:nrow(augmented_data)-nrow_original,]
  
  
  augmented_data <- DAColNoise(same_noise = TRUE,     # "same" = TRUE or "different" = FALSE
                               tbl_columns_selected = columns,   # Indices of the selected columns
                               dataset = dataset, 
                               tbl_classification_columns_selected = dependent_variable,   # Index of the column of the dependent variable
                               balanced_classes = balance_class,   # Name of the classes that should be balanced in vector 
                               balance_process = "balancing_with_noise",    # "oversampling" or "balancing_with_noise"
                               direction_of_generating = "horizontal",    # "vertical" or "horizontal"
                               seed = seed,   # any seed 
                               noise_color = noise,   # -1.5, -1, 0, 1, 1.5
                               violet_noise = violet,  # names of the columns in a vector
                               blue_noise = blue,  # names of the columns in a vector
                               white_noise = white,   # names of the columns in a vector
                               pink_noise = pink,  # names of the columns in a vector
                               red_noise = red,   # names of the columns in a vector
                               new_datapoints = "double",   # "double", "triple", "quadruple" or "individual" 
                               amount_new_datapoints = 100,  # amount of new datapoints, if individual
                               balancing = FALSE,   # "balancing" = TRUE or "no_balancing" = FALSE
                               replacement = "no_replacement",
                               rounded_variables = c(),  # names of the columns in a vector
                               negative_values = c())   # names of the columns in a vector
  
  # divided the augmented dataset into original and fake
  original_data_h <- augmented_data[1:nrow_original,]
  fake_data_h <- augmented_data[nrow_original+1:nrow(augmented_data)-nrow_original,]
  
  differences_col[1,col] = mean(abs(cor(original_data_v)-cor(fake_data_v)))
  differences_col[2,col] = mean(abs(cor(original_data_h)-cor(fake_data_h)))
  
}

augmented_data <- DAColNoise(same_noise = FALSE,     # "same" = TRUE or "different" = FALSE
                             tbl_columns_selected = columns,   # Indices of the selected columns
                             dataset = dataset, 
                             tbl_classification_columns_selected = dependent_variable,   # Index of the column of the dependent variable
                             balanced_classes = balance_class,   # Name of the classes that should be balanced in vector 
                             balance_process = "balancing_with_noise",    # "oversampling" or "balancing_with_noise"
                             direction_of_generating = "vertical",    # "vertical" or "horizontal"
                             seed = seed,   # any seed 
                             noise_color = noise,   # -1.5, -1, 0, 1, 1.5
                             violet_noise = violet,  # names of the columns in a vector
                             blue_noise = blue,  # names of the columns in a vector
                             white_noise = white,   # names of the columns in a vector
                             pink_noise = pink,  # names of the columns in a vector
                             red_noise = red,   # names of the columns in a vector
                             new_datapoints = "double",   # "double", "triple", "quadruple" or "individual" 
                             amount_new_datapoints = 100,  # amount of new datapoints, if individual
                             balancing = FALSE,   # "balancing" = TRUE or "no_balancing" = FALSE
                             replacement = "no_replacement",
                             rounded_variables = c(),  # names of the columns in a vector
                             negative_values = c())   # names of the columns in a vector

# divided the augmented dataset into original and fake
original_data_v <- augmented_data[1:nrow_original,]
fake_data_v <- augmented_data[nrow_original+1:nrow(augmented_data)-nrow_original,]

augmented_data <- DAColNoise(same_noise = FALSE,     # "same" = TRUE or "different" = FALSE
                             tbl_columns_selected = columns,   # Indices of the selected columns
                             dataset = dataset, 
                             tbl_classification_columns_selected = dependent_variable,   # Index of the column of the dependent variable
                             balanced_classes = balance_class,   # Name of the classes that should be balanced in vector 
                             balance_process = "balancing_with_noise",    # "oversampling" or "balancing_with_noise"
                             direction_of_generating = "horizontal",    # "vertical" or "horizontal"
                             seed = seed,   # any seed 
                             noise_color = noise,   # -1.5, -1, 0, 1, 1.5
                             violet_noise = violet,  # names of the columns in a vector
                             blue_noise = blue,  # names of the columns in a vector
                             white_noise = white,   # names of the columns in a vector
                             pink_noise = pink,  # names of the columns in a vector
                             red_noise = red,   # names of the columns in a vector
                             new_datapoints = "quadruple",   # "double", "triple", "quadruple" or "individual" 
                             amount_new_datapoints = 100,  # amount of new datapoints, if individual
                             balancing = FALSE,   # "balancing" = TRUE or "no_balancing" = FALSE
                             replacement = "no_replacement",
                             rounded_variables = c(),  # names of the columns in a vector
                             negative_values = c())   # names of the columns in a vector

# divided the augmented dataset into original and fake
original_data_h <- augmented_data[1:nrow_original,]
fake_data_h <- augmented_data[nrow_original+1:nrow(augmented_data)-nrow_original,]

differences_col[1,6] = mean(abs(cor(original_data_v)-cor(fake_data_v)))
differences_col[2,6] = mean(abs(cor(original_data_h)-cor(fake_data_h)))

differences_col = differences_col %>% 
  mutate_if(is.numeric, round, digits=3)

string = ""
string_v = "vertical & "
string_h = " & horizontal & "

for (i in 1:6) {
  if (i != 1) {string_v = paste(string_v, " & ")}
  string_v = paste(string_v, differences_col[1,i])
  if (i == 6){string_v = paste(string_v, " \\ ")}
  
  if (i != 1) {string_h = paste(string_h, " & ")}
  string_h = paste(string_h, differences_col[2,i])
  if (i == 6){string_h = paste(string_h, " \\  ")}
}

string = paste(string, string_v, string_h, "\\hline")
print(string)
