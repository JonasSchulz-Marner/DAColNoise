source("DatasetInput.R")
source("DatasetBalancing.R")
source("NoiseTable.R")

library(dplyr) 

###
# This function performs the data augmentation process through calling 
# the other functions and returning the new augmented dataset.
# This is the function, that stands alone without the shiny app. 
# Thus it needs all the user input that is normally
# given in the app as input of the function.
###

DAColNoise <- function (same_noise, tbl_columns_selected, dataset, 
                                              violet_noise, blue_noise, white_noise, pink_noise,
                                              red_noise, seed, new_datapoints, amount_new_datapoints, 
                                              direction_of_generating = "vertical", noise_color, balancing, tbl_classification_columns_selected,
                                              balanced_classes, balance_process, replacement = "no_replacement", rounded_variables,
                                              negative_values) {
  if (!(balancing)){
    DT1 <- na.omit(dataset)
  } else if (balancing){
    DT1 <- DatasetBalancing(inFile = dataset, 
                            tbl_classification_columns_selected = tbl_classification_columns_selected, 
                            balanced_classes = balanced_classes, balance_process = balance_process, 
                            same_noise = same_noise, direction_of_generating = direction_of_generating, 
                            tbl_columns_selected = tbl_columns_selected, seed = seed, noise_color = noise_color,
                            violet_noise = violet_noise, blue_noise = blue_noise,
                            white_noise = white_noise, pink_noise = pink_noise,
                            red_noise = red_noise, new_datapoints = new_datapoints, amount_new_datapoints = amount_new_datapoints)

    # get the indices of the selected columns that should be rounded
    dataset <- na.omit(dataset)
    indices_round <- c()
    for (index in 1:length(rounded_variables)){
      indices_round[index] <- which(colnames(dataset) == rounded_variables[index])
    }
    indices_round <- sort(indices_round)
    
    cols <- names(DT1)[indices_round]
    DT1 <- mutate_at(DT1, cols, round)
    
    
    # get the indices of the selected columns that must not contain negative values
    indices_negative <- c()
    for (index in 1:length(negative_values)){
      indices_negative[index] <- which(colnames(dataset) == negative_values[index])
    }
    indices_negative <- sort(indices_negative)
    
    cols <- names(DT1)[indices_negative]
    DT1 <- mutate_at(DT1, cols, abs)
    
  }
  DT2 <- NoiseTable(same_noise = same_noise, tbl_columns_selected = tbl_columns_selected, 
                    inFile = dataset, 
                    tbl_classification_columns_selected = tbl_classification_columns_selected, 
                    balanced_classes = balanced_classes, balance_process = balance_process,
                    direction_of_generating = direction_of_generating, 
                    seed = seed, noise_color = noise_color,
                    violet_noise = violet_noise, blue_noise = blue_noise,
                    white_noise = white_noise, pink_noise = pink_noise,
                    red_noise = red_noise, new_datapoints = new_datapoints, 
                    amount_new_datapoints = amount_new_datapoints, balancing = balancing,
                    replacement = replacement)
  
  # get the indices of the selected columns that should be rounded
  dataset <- na.omit(dataset)
  indices_round <- c()
  for (index in 1:length(rounded_variables)){
    indices_round[index] <- which(colnames(dataset) == rounded_variables[index])
  }
  indices_round <- sort(indices_round)
  
  cols <- names(DT2)[indices_round]
  DT2 <- mutate_at(DT2, cols, round)

  # get the indices of the selected columns that must not contain negative values
  indices_negative <- c()
  for (index in 1:length(negative_values)){
    indices_negative[index] <- which(colnames(dataset) == negative_values[index])
  }
  indices_negative <- sort(indices_negative)
  
  cols <- names(DT2)[indices_negative]
  DT2 <- mutate_at(DT2, cols, abs)
  
  table <- tidyft::rbindlist(list(DT1,DT2), use.names = FALSE)

  return (table)
}