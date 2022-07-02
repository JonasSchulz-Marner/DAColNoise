source("DatasetInput.R")

###
# This function gives the amount of new values that are generated and merged into 
# new datapoints. So it is the amount of new datapoints multiplied with the number
# of selected variables, where noise should be added to.
###

AmountOfNewData <- function (new_datapoints, tbl_columns_selected, amount_new_datapoints, inFile,
                             balancing, tbl_classification_columns_selected, balanced_classes) {
  if (!balancing) {  
    if (new_datapoints == 'double'){
      if (nrow(inFile) == 1 || is.character(inFile) && length(inFile) == 1) {
        return <- length(tbl_columns_selected)*nrow(DatasetInput(inFile))
      } else {
        return <- length(tbl_columns_selected)*nrow(inFile)
      }
    } else if (new_datapoints == 'triple'){
      if (nrow(inFile) == 1 || is.character(inFile) && length(inFile) == 1) {
        return <- length(tbl_columns_selected)*nrow(DatasetInput(inFile))*2
      } else {
        return <- length(tbl_columns_selected)*nrow(inFile)*2
      }
    } else if (new_datapoints == 'quadruple'){
      if (nrow(inFile) == 1 || is.character(inFile) && length(inFile) == 1) {
        return <- length(tbl_columns_selected)*nrow(DatasetInput(inFile))*3
      } else {
        return <- length(tbl_columns_selected)*nrow(inFile)*3
      }
    } else if(new_datapoints == 'individual'){
      return <- length(tbl_columns_selected)*as.integer(amount_new_datapoints)
    }
  } else if (balancing) {
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
    if (new_datapoints == 'double'){
      return <- length(tbl_columns_selected)*(amount_new_data+nrow(dataset))*2
    } else if (new_datapoints == 'triple'){
      return <- length(tbl_columns_selected)*(amount_new_data+nrow(dataset))*3
    } else if (new_datapoints == 'quadruple'){
      return <- length(tbl_columns_selected)*(amount_new_data+nrow(dataset))*4
    } else if(new_datapoints == 'individual'){
      return <- length(tbl_columns_selected)*(amount_new_data+as.integer(amount_new_datapoints))
    }
  }
  return(return)
}