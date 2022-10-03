source("AmountOfNewData.R")

###
# This function generates the specific color of noise in the amount
# that is needed for augmenting the dataset and returns an array of noise.
###

GenerateNoise <- function(seed, noise_color, new_datapoints, tbl_columns_selected, amount_new_datapoints,
                          inFile, tbl_classification_columns_selected, balanced_classes, balancing) {

  amount_of_new_data <- AmountOfNewData(new_datapoints = new_datapoints, tbl_columns_selected = tbl_columns_selected,
                                        amount_new_datapoints = amount_new_datapoints, 
                                        inFile = inFile, balancing = balancing,
                                        tbl_classification_columns_selected = tbl_classification_columns_selected,
                                        balanced_classes = balanced_classes)
  noise_array <- c()
  while (length(noise_array) < amount_of_new_data) {
    set.seed(seed = seed)
    noise_array <- c(noise_array, noise(kind=("power"), alpha = as.numeric(noise_color))@left)
    seed = seed + 1
  }
  
  return(noise_array)
}