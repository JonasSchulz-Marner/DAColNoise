source("DatasetInput.R")

library(shiny)
library(tuneR)

###
# This function generates the noise array in case of different colors
# for different variables in the amount that is needed to augment the dataset.
# The function returns the noise array with the alternating selected colors of noise.
###

GenerateVariableNoise <- function (tbl_columns_selected, inFile, 
                                   violet_noise, blue_noise, white_noise, pink_noise, red_noise,
                                   seed, new_datapoints, amount_new_datapoints, direction_of_generating, balancing,
                                   tbl_classification_columns_selected, balanced_classes) {
  shiny::req(length(tbl_columns_selected) >= 1)
  if (nrow(inFile) == 1 || is.character(inFile) && length(inFile) == 1) {
    dataset <- na.omit(DatasetInput(inFile))
  } else {
    dataset <- na.omit(inFile)
  }
  selected_variables <- tbl_columns_selected

  # remove all non-numeric variables (for that we need the two auxiliary variables)
  selected_variables_copy <- selected_variables
  i <- 0
  for (variable in selected_variables_copy) {
    if (!(is.numeric(dataset[,variable]))) {
      selected_variables <- selected_variables[-(variable - i)]
      i <- i+1
    }
  }
  
  # get the column names of the choosen columns
  column_names <- names(dataset)[selected_variables]

  # get the indices of the selected columns for each noise color
  indices_violet <- c()
  for (index in 1:length(violet_noise)){
    indices_violet[index] <- which(colnames(dataset) == violet_noise[index])
  }
  indices_violet <- sort(indices_violet)
  
  indices_blue <- c()
  for (index in 1:length(blue_noise)){
    indices_blue[index] <- which(colnames(dataset) == blue_noise[index])
  }
  indices_blue <- sort(indices_blue)
  
  indices_white <- c()
  for (index in 1:length(white_noise)){
    indices_white[index] <- which(colnames(dataset) == white_noise[index])
  }
  indices_white <- sort(indices_white)
  
  indices_pink <- c()
  for (index in 1:length(pink_noise)){
    indices_pink[index] <- which(colnames(dataset) == pink_noise[index])
  }
  indices_pink <- sort(indices_pink)
  
  indices_red <- c()
  for (index in 1:length(red_noise)){
    indices_red[index] <- which(colnames(dataset) == red_noise[index])
  }
  indices_red <- sort(indices_red)
  
  # vector with the color of noise for each variable that is numeric
  # colors_of_variables has the full length of all column, also with non-numeric variables,
  # because the indices are the column indices of the dataset.
  colors_of_variables <- rep(0, ncol(dataset))
  for (element in indices_violet) {
    colors_of_variables[element] <- "violet"
  }
  for (element in indices_blue) {
    colors_of_variables[element] <- "blue"
  }
  for (element in indices_white) {
    colors_of_variables[element] <- "white"
  }
  for (element in indices_pink) {
    colors_of_variables[element] <- "pink"
  }
  for (element in indices_red) {
    colors_of_variables[element] <- "red"
  }

  
  # determine the amount of new rows depending on the amount of new data and whether the dataset is balanced or not.
  # Total amount of new data is amount_of_new_rows*length(selected_variables)
  new_data_balancing <- 0
  if (balancing == TRUE) {
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
    new_data_balancing <- sum(new_data_of_classes$Freq)
  }
  
  if (new_datapoints == "double"){
    amount_of_new_rows <- nrow(dataset) + new_data_balancing
  } else if (new_datapoints == "triple") {
    amount_of_new_rows <- 2*(nrow(dataset) + new_data_balancing)
  } else if (new_datapoints == "quadruple") {
    amount_of_new_rows <- 3*(nrow(dataset) + new_data_balancing)
  } else if (new_datapoints == "individual") {
    amount_of_new_rows <- amount_new_datapoints + new_data_balancing
  }

  
  # generate the noises for each color
  if (length(indices_violet) > 0){
    violet_array <- c()
    while (length(violet_array) < amount_of_new_rows*length(indices_violet)) {
      set.seed(seed = seed)
      violet_array <- c(violet_array, tuneR::noise(kind=("power"), alpha = -1.5)@left)
    }
  }
  if (length(indices_blue) > 0){
    blue_array <- c()
    while (length(blue_array) < amount_of_new_rows*length(indices_blue)) {
      set.seed(seed = seed)
      blue_array <- c(blue_array, tuneR::noise(kind=("power"), alpha = -1)@left)
    }
  }
  if (length(indices_white) > 0){
    white_array <- c()
    while (length(white_array) < amount_of_new_rows*length(indices_white)) {
      set.seed(seed = seed)
      white_array <- c(white_array, tuneR::noise(kind=("power"), alpha = 0)@left)
    }
  }
  if (length(indices_pink) > 0){
    pink_array <- c()
    while (length(pink_array) < amount_of_new_rows*length(indices_pink)) {
      set.seed(seed = seed)
      pink_array <- c(pink_array, tuneR::noise(kind=("power"), alpha = 1)@left)
    }
  }
  if (length(indices_red) > 0){
    red_array <- c()
    while (length(red_array) < amount_of_new_rows*length(indices_red)) {
      set.seed(seed = seed)
      red_array <- c(red_array, tuneR::noise(kind=("power"), alpha = 1.5)@left)
    }
  }
  
  
  # generate the noise_array denpending on the direction of adding noise (horizontal/vertical)
  # if it is vertical, generate noise of the length of amount_of_new_rows and then fill the noise_array
  # with these noises
  if (direction_of_generating == "vertical"){
    noise_array <- c()
    violet_i <- 0
    blue_i <- 0
    white_i <- 0
    pink_i <- 0
    red_i <- 0
    for (element in colors_of_variables) {
      if (element == "violet") {
        noise_array <- c(noise_array, violet_array[(violet_i*amount_of_new_rows + 1):((violet_i+1)*amount_of_new_rows)])
        violet_i <- violet_i+1
      } else if (element == "blue") {
        noise_array <- c(noise_array, blue_array[(blue_i*amount_of_new_rows + 1):((blue_i+1)*amount_of_new_rows)])
        blue_i <- blue_i+1
      } else if (element == "white") {
        noise_array <- c(noise_array, white_array[(white_i*amount_of_new_rows + 1):((white_i+1)*amount_of_new_rows)])
        white_i <- white_i+1
      } else if (element == "pink") {
        noise_array <- c(noise_array, pink_array[(pink_i*amount_of_new_rows + 1):((pink_i+1)*amount_of_new_rows)])
        pink_i <- pink_i+1
      } else if (element == "red") {
        noise_array <- c(noise_array, red_array[(red_i*amount_of_new_rows + 1):((red_i+1)*amount_of_new_rows)])
        red_i <- red_i+1
      }
    }
    # if direction is horizontal, generate first a data frame containing the different noises in the columns
    # then read row wise this data frame in the noise_array
  } else if (direction_of_generating == "horizontal") {
    noise_array <- c()
    violet_i <- 1
    blue_i <- 1
    white_i <- 1
    pink_i <- 1
    red_i <- 1
    noise_table <- data.frame(row.names = 1:amount_of_new_rows)
    # write in noise_table the color of noise in each column as string
    for (column in 1:length(colors_of_variables)) {
      noise_table[,column] <- rep(colors_of_variables[column], amount_of_new_rows)
    }

    # go through the noise_table row wise and replace every color by the entry of the noise_array of that color
    # in that way, we get a noise_array for horizontal addition
    for (row_index in 1:nrow(noise_table)) {
      for (col_index in 1:ncol(noise_table)){
        if (noise_table[row_index, col_index] == "violet"){
          noise_table[row_index, col_index] <- violet_array[violet_i]
          violet_i <- violet_i + 1
        } else if (noise_table[row_index, col_index] == "blue"){
          noise_table[row_index, col_index] <- blue_array[blue_i]
          blue_i <- blue_i + 1
        } else if (noise_table[row_index, col_index] == "white"){
          noise_table[row_index, col_index] <- white_array[white_i]
          white_i <- white_i + 1
        } else if (noise_table[row_index, col_index] == "pink"){
          noise_table[row_index, col_index] <- pink_array[pink_i]
          pink_i <- pink_i + 1
        } else if (noise_table[row_index, col_index] == "red"){
          noise_table[row_index, col_index] <- red_array[red_i]
          red_i <- red_i + 1
        } else if (noise_table[row_index, col_index] == "0"){
          noise_table[row_index, col_index] <- 0
        }
        
      }
     
    }
    noise_table[] <- lapply(noise_table, function(x) {
      if(!(is.numeric(x))) as.numeric(as.character(x)) else x
    })
    
    # delete the "0"-columns
    noise_table <- noise_table[,colSums(abs(noise_table) != 0) > 0]
    
    noise_array <- as.vector(t(noise_table))

  }
  
  return(noise_array)
}