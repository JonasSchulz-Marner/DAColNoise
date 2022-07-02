source("DatasetInput.R")
source("GenerateNoiseForBalancing.R")
source("GenerateVariableNoiseForBalancing.R")

###
# This function performs the balancing of the selected classes either 
# by oversampling or by balancing with noise. 
# The function returns the balanced dataset.
###

DatasetBalancing <- function (inFile, tbl_classification_columns_selected, balanced_classes,
                              balance_process, same_noise, direction_of_generating, tbl_columns_selected, seed,
                              noise_color, violet_noise, blue_noise, white_noise, pink_noise, red_noise, new_datapoints, amount_new_datapoints) {
  # omit NAs and give than correct rownames
  if (nrow(inFile) == 1 || is.character(inFile) && length(inFile) == 1) {
    dataset <- na.omit(DatasetInput(inFile))
  } else {
    dataset <- na.omit(inFile)
  }
  row.names(dataset) <- 1:nrow(dataset)
  
  all_classes <- sort(unique(dataset[,tbl_classification_columns_selected]))
  most_frequent_class <- all_classes[which.max(tabulate(match(dataset[,tbl_classification_columns_selected],all_classes)))]
  
  # calculate the frequencies of all classes
  frequencies_of_classes <- as.data.frame(table(dataset[,tbl_classification_columns_selected]))

  # calculate the amount of the most frequent class
  amount_of_most_frequent_class <- frequencies_of_classes[frequencies_of_classes$Var1 == most_frequent_class,"Freq"]
  
  # choose only the selected classes in the checkbox input
  frequencies_of_classes <- frequencies_of_classes[frequencies_of_classes$Var1 %in% balanced_classes, ]
  
  # calculate the amount of new data that is needed by each selected class in new_data_of_classes
  new_data_of_classes <- frequencies_of_classes

  #new_data_of_classes$Var1 <- lapply(new_data_of_classes$Var1, as.numeric)
  for (row in 1:nrow(new_data_of_classes)){
    new_data_of_classes$Freq[row] = amount_of_most_frequent_class - new_data_of_classes$Freq[row]
  }
  
  # sum the amount of new data that is needed to balance the dataset
  amount_new_data <- sum(new_data_of_classes$Freq)
  
  # generate the table with the balanced and noised data
  balance_table <- data.frame()
  for (row in 1:nrow(new_data_of_classes)) {
    
    #if-clause: if there are more than one datapoints of that class, do a sample, else multiply the one datapoint
    if (length(as.numeric(rownames(dataset[dataset[,tbl_classification_columns_selected] == new_data_of_classes$Var1[row],]))) > 1){
      set.seed(seed)
      balance_class <- dataset[sample(as.numeric(rownames(dataset[dataset[,tbl_classification_columns_selected] == new_data_of_classes$Var1[row],])), new_data_of_classes$Freq[row], replace = TRUE), ]
    } else {
      balance_class <- do.call("rbind", replicate(amount_of_most_frequent_class - 1, dataset[as.numeric(rownames(dataset[dataset[,tbl_classification_columns_selected] == new_data_of_classes$Var1[row],])), ], simplify = FALSE))
    }
    # append the sampled rows of the new class to_balance
    balance_table <- as.data.frame(tidyft::rbindlist(list(balance_table, balance_class), use.names = TRUE, fill = TRUE))
  }

  # select whether the data that is to be balanced, should be selected simply oversampled 
  # or if the data should additionally be added with noise. In that case, add noise in the same way,
  # as in the origin noise adding process, but with different noise. The data that is then to be noised
  # is in balance_table
  
  if (balance_process == "oversampling"){
    balanced_dataset <- as.data.frame(tidyft::rbindlist(list(dataset, balance_table), use.names = TRUE, fill = TRUE))
  } else if (balance_process == "balancing_with_noise"){
    # generate noise and estimate standard deviation
    if (same_noise == TRUE){
      noise_array <- GenerateNoiseForBalancing(seed = seed, noise_color = noise_color, new_datapoints = new_datapoints,
                                               tbl_columns_selected = tbl_columns_selected, amount_new_datapoints = amount_new_datapoints,
                                               inFile = inFile,
                                               tbl_classification_columns_selected = tbl_classification_columns_selected,
                                               balanced_classes = balanced_classes)
    } else if (same_noise == FALSE){
      noise_array <- GenerateVariableNoiseForBalancing(tbl_columns_selected = tbl_columns_selected, inFile = inFile, 
                                                       tbl_classification_columns_selected = tbl_classification_columns_selected,
                                                       balanced_classes = balanced_classes,
                                                       violet_noise = violet_noise, blue_noise = blue_noise,
                                                       white_noise = white_noise, pink_noise = pink_noise,
                                                       red_noise = red_noise, seed = seed,  
                                                       direction_of_generating = direction_of_generating)
    }
    #noise_array <- generate_noise_for_balancing()
    array_index = 1

    standard_deviation <- apply(dataset, 2, sd)
    standard_deviation[is.na(standard_deviation)] <- 0
    
    # distinguish between horizontal and vertical addition of the noise
    if (direction_of_generating == 'horizontal'){
      
      table_noise_balancing <- data.frame(matrix(0, ncol = ncol(balance_table), nrow = nrow(balance_table)))
      colnames(table_noise_balancing) <- colnames(balance_table)
      for (row_index in 1:nrow(balance_table)){
        for (col_index in 1:ncol(balance_table)){
          # if column is not selected or column contains non-numeric values
          if (!(col_index %in% tbl_columns_selected) || !(is.numeric(balance_table[row_index, col_index]))){
            table_noise_balancing[row_index, col_index] = balance_table[row_index, col_index]
          } else {
            table_noise_balancing[row_index, col_index] = balance_table[row_index, col_index] + noise_array[array_index] * standard_deviation[col_index]
            array_index = array_index + 1
          }
        }
      }
    } else if (direction_of_generating == 'vertical'){
      
      table_noise_balancing <- data.frame(matrix(0, ncol = ncol(balance_table), nrow = nrow(balance_table)))
      colnames(table_noise_balancing) <- colnames(balance_table)
      for (col_index in 1:ncol(balance_table)){
        for (row_index in 1:nrow(balance_table)){
          # if column is not selected or column contains non-numeric values
          if (!(col_index %in% tbl_columns_selected) || !(is.numeric(balance_table[row_index, col_index]))){
            table_noise_balancing[row_index, col_index] = balance_table[row_index, col_index]
          } else {
            table_noise_balancing[row_index, col_index] = balance_table[row_index, col_index] + noise_array[array_index] * standard_deviation[col_index]
            array_index = array_index + 1
          }
        }
      }
    }
    balanced_dataset <- as.data.frame(tidyft::rbindlist(list(dataset, table_noise_balancing), use.names = TRUE, fill = TRUE))
  }

  return(balanced_dataset)
}