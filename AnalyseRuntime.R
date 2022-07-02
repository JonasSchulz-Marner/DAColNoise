source("DAColNoise.R")

library(smotefamily)

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


## runtime of different DA approaches

ptm <- Sys.time()
for (i in range(1:50)){
  augmented_data <- DAColNoise(same_noise = FALSE,     # "same" = TRUE or "different" = FALSE
                               tbl_columns_selected = columns,   # Indices of the selected columns
                               dataset = dataset, 
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
                               negative_values = c())   # names of the columns in a vector
}
time_cn <- (Sys.time()-ptm)/50


ptm <- Sys.time()
### SMOTE ### mit k = 5 und k = 3
for (i in range(1:50)){
  smote_data <- smotefamily::SMOTE(X = dataset[-dependent_variable], target = dataset[dependent_variable], K = 5, 
                                   dup_size = ceiling(table(dataset[,dependent_variable])[1]/table(dataset[,dependent_variable])[2])*smote_amount)$data
  smote_data <- smotefamily::SMOTE(X = smote_data[-max(ncol(smote_data))], target = smote_data$class, K = 5)$data
  
}
time_smote <- (Sys.time()-ptm)/50




ptm <- Sys.time()
for (i in range(1:50)){
  adasyn_data <- smotefamily::ADAS(X = dataset[-dependent_variable], target = dataset[dependent_variable], K = 5)$data
}

time_adasyn <- (Sys.time()-ptm)/50


ptm <- Sys.time()
for (i in range(1:50)){
  wn_data <- DAColNoise(same_noise = TRUE,     # "same" = TRUE or "different" = FALSE
                        tbl_columns_selected = columns,   # Indices of the selected columns
                        dataset = dataset, 
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
  
}

time_wn <- (Sys.time()-ptm)/50

print(time_wn)
print(time_cn)
print(time_smote)
print(time_adasyn)




## Analysis amount of data

ptm <- Sys.time()
for (i in range(1:50)){
  augmented_data <- DAColNoise(same_noise = FALSE,     # "same" = TRUE or "different" = FALSE
                               tbl_columns_selected = columns,   # Indices of the selected columns
                               dataset = dataset, 
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
                               new_datapoints = "double",   # "double", "triple", "quadruple" or "individual" 
                               amount_new_datapoints = 100,  # amount of new datapoints, if individual
                               balancing = TRUE,   # "balancing" = TRUE or "no_balancing" = FALSE
                               rounded_variables = c(),  # names of the columns in a vector
                               negative_values = c())   # names of the columns in a vector
}
time_double <- (Sys.time()-ptm)/50


ptm <- Sys.time()
for (i in range(1:50)){
  augmented_data <- DAColNoise(same_noise = FALSE,     # "same" = TRUE or "different" = FALSE
                               tbl_columns_selected = columns,   # Indices of the selected columns
                               dataset = dataset, 
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
                               negative_values = c())   # names of the columns in a vector
}
time_triple <- (Sys.time()-ptm)/50


ptm <- Sys.time()
for (i in range(1:50)){
  augmented_data <- DAColNoise(same_noise = FALSE,     # "same" = TRUE or "different" = FALSE
                               tbl_columns_selected = columns,   # Indices of the selected columns
                               dataset = dataset, 
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
                               new_datapoints = "quadruple",   # "double", "triple", "quadruple" or "individual" 
                               amount_new_datapoints = 100,  # amount of new datapoints, if individual
                               balancing = TRUE,   # "balancing" = TRUE or "no_balancing" = FALSE
                               rounded_variables = c(),  # names of the columns in a vector
                               negative_values = c())   # names of the columns in a vector
}
time_quadruple <- (Sys.time()-ptm)/50


print(time_double)
print(time_triple)
print(time_quadruple)


