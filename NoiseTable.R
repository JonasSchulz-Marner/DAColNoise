source("GenerateNoise.R")
source("GenerateVariableNoise.R")
source("DatasetInput.R")
source("DatasetBalancing.R")

###
# This function generates the new datapoints based on the noise arrays
# of the other functions. The return is a data table with all new datapoints.
###

NoiseTable <- function (same_noise, tbl_columns_selected, inFile, 
                        violet_noise, blue_noise, white_noise, pink_noise,
                        red_noise, seed, new_datapoints, amount_new_datapoints, 
                        direction_of_generating, noise_color, balancing, tbl_classification_columns_selected,
                        balanced_classes, balance_process, replacement) {
  
  if (same_noise == TRUE){
    noise_array <- GenerateNoise(seed = seed, noise_color = noise_color, new_datapoints = new_datapoints,
                                 tbl_columns_selected = tbl_columns_selected, amount_new_datapoints = amount_new_datapoints,
                                 inFile = inFile, balancing = balancing,
                                 tbl_classification_columns_selected = tbl_classification_columns_selected,
                                 balanced_classes = balanced_classes)
  } else if (same_noise == FALSE){
    noise_array <- GenerateVariableNoise(tbl_columns_selected = tbl_columns_selected, inFile = inFile, 
                                         violet_noise = violet_noise, blue_noise = blue_noise,
                                         white_noise = white_noise, pink_noise = pink_noise,
                                         red_noise = red_noise, seed = seed, new_datapoints = new_datapoints,
                                         amount_new_datapoints = amount_new_datapoints, 
                                         direction_of_generating = direction_of_generating, balancing = balancing,
                                         tbl_classification_columns_selected = tbl_classification_columns_selected,
                                         balanced_classes = balanced_classes)
  }

  array_index = 1
  # datatable only with the selected variables
  if (nrow(inFile) == 1 || is.character(inFile) && length(inFile) == 1) {
    dataset <- na.omit(DatasetInput(inFile))
  } else {
    dataset <- na.omit(inFile)
  }
  datatable_variables <- colnames(dataset[,tbl_columns_selected])
  
  
  
  #dataset_nan is the dataset with all columns but without NaN
  if (balancing == FALSE){
    dataset_nan <- na.omit(dataset)
  } else if (balancing == TRUE){
    dataset_nan <- DatasetBalancing(inFile = inFile, new_datapoints = new_datapoints,
                                    tbl_classification_columns_selected = tbl_classification_columns_selected, 
                                    balanced_classes = balanced_classes, balance_process = balance_process, 
                                    same_noise = same_noise, direction_of_generating = direction_of_generating, 
                                    tbl_columns_selected = tbl_columns_selected, seed = seed, noise_color = noise_color,
                                    violet_noise = violet_noise, blue_noise = blue_noise,
                                    white_noise = white_noise, pink_noise = pink_noise,
                                    red_noise = red_noise)
  }

  # calculate standard deviation for each column, if NA (e.g. because of strings) -> 0
  standard_deviation <- apply(dataset_nan, 2, sd)
  standard_deviation[is.na(standard_deviation)] <- 0
  standard_deviation
  
  # distinguish in four cases
  ### FIRST CASE: horizontal and without replacement ###
  if (direction_of_generating == 'horizontal' && replacement == 'no_replacement'){
    # distinguish between double/triple/quadruple and individual
    if (new_datapoints == 'double') {
      table_noise <- data.frame(matrix(0, ncol = ncol(dataset_nan), nrow = nrow(dataset_nan)))
      for (row_index in 1:nrow(dataset_nan)){
        for (col_index in 1:ncol(dataset_nan)){
          # if column is not selected or column contains non-numeric values
          if (!(col_index %in% tbl_columns_selected) || !(is.numeric(dataset_nan[row_index, col_index]))){
            table_noise[row_index, col_index] = dataset_nan[row_index, col_index]
          } else {
            table_noise[row_index, col_index] = dataset_nan[row_index, col_index] + noise_array[array_index] * standard_deviation[col_index]
            array_index = array_index + 1
          }
          
        }
        
      }
    } else if (new_datapoints == 'triple'){
      dataset_triple_h_no <- rbind(dataset_nan, dataset_nan)
      table_noise <- data.frame(matrix(0, ncol = ncol(dataset_triple_h_no), nrow = nrow(dataset_triple_h_no)))
      for (row_index in 1:nrow(dataset_triple_h_no)){
        for (col_index in 1:ncol(dataset_triple_h_no)){
          # if column is not selected or column contains non-numeric values
          if (!(col_index %in% tbl_columns_selected) || !(is.numeric(dataset_triple_h_no[row_index, col_index]))){
            table_noise[row_index, col_index] = dataset_triple_h_no[row_index, col_index]
          } else {
            table_noise[row_index, col_index] = dataset_triple_h_no[row_index, col_index] + noise_array[array_index] * standard_deviation[col_index]
            array_index = array_index + 1
          }
          
        }
        
      }
    } else if (new_datapoints == 'quadruple'){
      dataset_quadruple_h_no <- rbind(dataset_nan, dataset_nan, dataset_nan)
      table_noise <- data.frame(matrix(0, ncol = ncol(dataset_quadruple_h_no), nrow = nrow(dataset_quadruple_h_no)))
      for (row_index in 1:nrow(dataset_quadruple_h_no)){
        for (col_index in 1:ncol(dataset_quadruple_h_no)){
          # if column is not selected or column contains non-numeric values
          if (!(col_index %in% tbl_columns_selected) || !(is.numeric(dataset_quadruple_h_no[row_index, col_index]))){
            table_noise[row_index, col_index] = dataset_quadruple_h_no[row_index, col_index]
          } else {
            table_noise[row_index, col_index] = dataset_quadruple_h_no[row_index, col_index] + noise_array[array_index] * standard_deviation[col_index]
            array_index = array_index + 1
          }
          
        }
        
      }
    } else if (new_datapoints == 'individual'){
      # if amount of new datapoints is higher than rows in dataset, add the dataset x-times
      # and then do a sampling
      if (amount_new_datapoints >= nrow(dataset_nan)) {
        dataset_individual_h_no <- dataset_nan
        if (floor(amount_new_datapoints / nrow(dataset_nan)) > 2) {
          for (index in range(floor(amount_new_datapoints / nrow(dataset_nan)) - 1)) {
            dataset_individual_h_no <- rbind(dataset_individual_h_no, dataset_nan)
          }
        } else if (floor(amount_new_datapoints / nrow(dataset_nan))== 2){
          dataset_individual_h_no <- rbind(dataset_individual_h_no, dataset_nan)
        }
        set.seed(seed)
        dataset_individual_h_no <- rbind(dataset_individual_h_no, dataset_nan[sample(nrow(dataset_nan), amount_new_datapoints - nrow(dataset_nan) * floor(amount_new_datapoints / nrow(dataset_nan))), ])
      } else {
        set.seed(seed)
        dataset_individual_h_no <- dataset_nan[sample(nrow(dataset_nan), amount_new_datapoints), ]
      }
      table_noise <- data.frame(matrix(0, ncol = ncol(dataset_individual_h_no), nrow = nrow(dataset_individual_h_no)))
      for (row_index in 1:nrow(dataset_individual_h_no)){
        for (col_index in 1:ncol(dataset_individual_h_no)){
          # if column is not selected or column contains non-numeric values
          if (!(col_index %in% tbl_columns_selected) || !(is.numeric(dataset_individual_h_no[row_index, col_index]))){
            table_noise[row_index, col_index] = dataset_individual_h_no[row_index, col_index]
          } else {
            table_noise[row_index, col_index] = dataset_individual_h_no[row_index, col_index] + noise_array[array_index] * standard_deviation[col_index]
            array_index = array_index + 1
          }
          
        }
        
      }
    }
  } ### SECOND CASE: horizontal and with replacement ###
  else if (direction_of_generating == 'horizontal' && replacement == 'replacement') {
    if (new_datapoints == 'double') {
      set.seed(seed)
      dataset_nan_h_re <- dataset_nan[sample(nrow(dataset_nan), nrow(dataset_nan), replace = TRUE), ]
      table_noise <- data.frame(matrix(0, ncol = ncol(dataset_nan_h_re), nrow = nrow(dataset_nan_h_re)))
      for (row_index in 1:nrow(dataset_nan_h_re)){
        for (col_index in 1:ncol(dataset_nan_h_re)){
          # if column is not selected or column contains non-numeric values
          if (!(col_index %in% tbl_columns_selected) || !(is.numeric(dataset_nan_h_re[row_index, col_index]))){
            table_noise[row_index, col_index] = dataset_nan_h_re[row_index, col_index]
          } else {
            table_noise[row_index, col_index] = dataset_nan_h_re[row_index, col_index] + noise_array[array_index] * standard_deviation[col_index]
            array_index = array_index + 1
          }
          
        }
        
      }
    } else if (new_datapoints == 'triple') {
      set.seed(seed)
      dataset_triple_h_re <- dataset_nan[sample(nrow(dataset_nan), 2*nrow(dataset_nan), replace = TRUE), ]
      table_noise <- data.frame(matrix(0, ncol = ncol(dataset_triple_h_re), nrow = nrow(dataset_triple_h_re)))
      for (row_index in 1:nrow(dataset_triple_h_re)){
        for (col_index in 1:ncol(dataset_triple_h_re)){
          # if column is not selected or column contains non-numeric values
          if (!(col_index %in% tbl_columns_selected) || !(is.numeric(dataset_triple_h_re[row_index, col_index]))){
            table_noise[row_index, col_index] = dataset_triple_h_re[row_index, col_index]
          } else {
            table_noise[row_index, col_index] = dataset_triple_h_re[row_index, col_index] + noise_array[array_index] * standard_deviation[col_index]
            array_index = array_index + 1
          }
          
        }
        
      }
    } else if (new_datapoints == 'quadruple') {
      set.seed(seed)
      dataset_quadruple_h_re <- dataset_nan[sample(nrow(dataset_nan), 3*nrow(dataset_nan), replace = TRUE), ]
      table_noise <- data.frame(matrix(0, ncol = ncol(dataset_quadruple_h_re), nrow = nrow(dataset_quadruple_h_re)))
      for (row_index in 1:nrow(dataset_quadruple_h_re)){
        for (col_index in 1:ncol(dataset_quadruple_h_re)){
          # if column is not selected or column contains non-numeric values
          if (!(col_index %in% tbl_columns_selected) || !(is.numeric(dataset_quadruple_h_re[row_index, col_index]))){
            table_noise[row_index, col_index] = dataset_quadruple_h_re[row_index, col_index]
          } else {
            table_noise[row_index, col_index] = dataset_quadruple_h_re[row_index, col_index] + noise_array[array_index] * standard_deviation[col_index]
            array_index = array_index + 1
          }
          
        }
        
      }
    } else if (new_datapoints == 'individual') {
      set.seed(seed)
      dataset_individual_h_re <- dataset_nan[sample(nrow(dataset_nan), amount_new_datapoints, replace = TRUE), ]
      table_noise <- data.frame(matrix(0, ncol = ncol(dataset_individual_h_re), nrow = nrow(dataset_individual_h_re)))
      for (row_index in 1:nrow(dataset_individual_h_re)){
        for (col_index in 1:ncol(dataset_individual_h_re)){
          # if column is not selected or column contains non-numeric values
          if (!(col_index %in% tbl_columns_selected) || !(is.numeric(dataset_individual_h_re[row_index, col_index]))){
            table_noise[row_index, col_index] = dataset_individual_h_re[row_index, col_index]
          } else {
            table_noise[row_index, col_index] = dataset_individual_h_re[row_index, col_index] + noise_array[array_index] * standard_deviation[col_index]
            array_index = array_index + 1
          }
          
        }
        
      }
    }
  } ### THIRD CASE: vertical and without replacement ###
  else if (direction_of_generating == 'vertical' && replacement == 'no_replacement') {
    if (new_datapoints == 'double') {
      table_noise <- data.frame(matrix(0, ncol = ncol(dataset_nan), nrow = nrow(dataset_nan)))
      for (col_index in 1:ncol(dataset_nan)){
        for (row_index in 1:nrow(dataset_nan)){
          # if column is not selected or column contains non-numeric values
          if (!(col_index %in% tbl_columns_selected) || !(is.numeric(dataset_nan[row_index, col_index]))){
            table_noise[row_index, col_index] = dataset_nan[row_index, col_index]
          } else {
            table_noise[row_index, col_index] = dataset_nan[row_index, col_index] + noise_array[array_index] * standard_deviation[col_index]
            array_index = array_index + 1
          }
          
        }
        
      }
    } else if (new_datapoints == 'triple'){
      dataset_triple_v_re <- rbind(dataset_nan, dataset_nan)
      table_noise <- data.frame(matrix(0, ncol = ncol(dataset_triple_v_re), nrow = nrow(dataset_triple_v_re)))
      for (col_index in 1:ncol(dataset_triple_v_re)){
        for (row_index in 1:nrow(dataset_triple_v_re)){
          # if column is not selected or column contains non-numeric values
          if (!(col_index %in% tbl_columns_selected) || !(is.numeric(dataset_triple_v_re[row_index, col_index]))){
            table_noise[row_index, col_index] = dataset_triple_v_re[row_index, col_index]
          } else {
            table_noise[row_index, col_index] = dataset_triple_v_re[row_index, col_index] + noise_array[array_index] * standard_deviation[col_index]
            array_index = array_index + 1
          }
          
        }
        
      }
    } else if (new_datapoints == 'quadruple'){
      dataset_quadruple_v_re <- rbind(dataset_nan, dataset_nan, dataset_nan)
      table_noise <- data.frame(matrix(0, ncol = ncol(dataset_quadruple_v_re), nrow = nrow(dataset_quadruple_v_re)))
      for (col_index in 1:ncol(dataset_quadruple_v_re)){
        for (row_index in 1:nrow(dataset_quadruple_v_re)){
          # if column is not selected or column contains non-numeric values
          if (!(col_index %in% tbl_columns_selected) || !(is.numeric(dataset_quadruple_v_re[row_index, col_index]))){
            table_noise[row_index, col_index] = dataset_quadruple_v_re[row_index, col_index]
          } else {
            table_noise[row_index, col_index] = dataset_quadruple_v_re[row_index, col_index] + noise_array[array_index] * standard_deviation[col_index]
            array_index = array_index + 1
          }
          
        }
        
      }
    } else if (new_datapoints == 'individual'){
      # if amount of new datapoints is higher than rows in dataset, add the dataset x-times
      # and then do a sampling
      if (amount_new_datapoints >= nrow(dataset_nan)) {
        dataset_individual_v_re <- dataset_nan
        if (floor(input$amount_new_datapoints / nrow(dataset_nan)) > 2){
          for (index in range(floor(amount_new_datapoints / nrow(dataset_nan)) - 1)) {
            dataset_individual_v_re <- rbind(dataset_individual_v_re, dataset_nan)
          }
        } else if (floor(amount_new_datapoints / nrow(dataset_nan))== 2){
          dataset_individual_v_re <- rbind(dataset_individual_v_re, dataset_nan)
        }
        set.seed(seed)
        dataset_individual_v_re <- rbind(dataset_individual_v_re, dataset_nan[sample(nrow(dataset_nan), amount_new_datapoints - nrow(dataset_nan) * floor(amount_new_datapoints / nrow(dataset_nan))), ])
      } else {
        set.seed(seed)
        dataset_individual_v_re <- dataset_nan[sample(nrow(dataset_nan), amount_new_datapoints), ]
      }
      table_noise <- data.frame(matrix(0, ncol = ncol(dataset_individual_v_re), nrow = nrow(dataset_individual_v_re)))
      for (col_index in 1:ncol(dataset_individual_v_re)){
        for (row_index in 1:nrow(dataset_individual_v_re)){
          # if column is not selected or column contains non-numeric values
          if (!(col_index %in% tbl_columns_selected) || !(is.numeric(dataset_individual_v_re[row_index, col_index]))){
            table_noise[row_index, col_index] = dataset_individual_v_re[row_index, col_index]
          } else {
            table_noise[row_index, col_index] = dataset_individual_v_re[row_index, col_index] + noise_array[array_index] * standard_deviation[col_index]
            array_index = array_index + 1
          }
          
        }
        
      }
    }
  } ### FOURTH CASE: vertical and with replacement ###
  else if (direction_of_generating == 'vertical' && replacement == 'replacement') {
    if (new_datapoints == 'double') {
      set.seed(seed)
      dataset_nan_v_re <- dataset_nan[sample(nrow(dataset_nan), nrow(dataset_nan), replace = TRUE), ]
      table_noise <- data.frame(matrix(0, ncol = ncol(dataset_nan_v_re), nrow = nrow(dataset_nan_v_re)))
      for (col_index in 1:ncol(dataset_nan_v_re)){
        for (row_index in 1:nrow(dataset_nan_v_re)){
          # if column is not selected or column contains non-numeric values
          if (!(col_index %in% tbl_columns_selected) || !(is.numeric(dataset_nan_v_re[row_index, col_index]))){
            table_noise[row_index, col_index] = dataset_nan_v_re[row_index, col_index]
          } else {
            table_noise[row_index, col_index] = dataset_nan_v_re[row_index, col_index] + noise_array[array_index] * standard_deviation[col_index]
            array_index = array_index + 1
          }
          
        }
        
      }
    } else if (new_datapoints == 'triple') {
      set.seed(seed)
      dataset_triple_v_re <- dataset_nan[sample(nrow(dataset_nan), 2*nrow(dataset_nan), replace = TRUE), ]
      table_noise <- data.frame(matrix(0, ncol = ncol(dataset_triple_v_re), nrow = nrow(dataset_triple_v_re)))
      for (col_index in 1:ncol(dataset_triple_v_re)){
        for (row_index in 1:nrow(dataset_triple_v_re)){
          # if column is not selected or column contains non-numeric values
          if (!(col_index %in% tbl_columns_selected) || !(is.numeric(dataset_triple_v_re[row_index, col_index]))){
            table_noise[row_index, col_index] = dataset_triple_v_re[row_index, col_index]
            
          } else {
            table_noise[row_index, col_index] = dataset_triple_v_re[row_index, col_index] + noise_array[array_index] * standard_deviation[col_index]
            array_index = array_index + 1
            
          }
          
        }
        
      }
    } else if (new_datapoints == 'quadruple') {
      set.seed(seed)
      dataset_quadruple_v_re <- dataset_nan[sample(nrow(dataset_nan), 3*nrow(dataset_nan), replace = TRUE), ]
      table_noise <- data.frame(matrix(0, ncol = ncol(dataset_quadruple_v_re), nrow = nrow(dataset_quadruple_v_re)))
      for (col_index in 1:ncol(dataset_quadruple_v_re)){
        for (row_index in 1:nrow(dataset_quadruple_v_re)){
          # if column is not selected or column contains non-numeric values
          if (!(col_index %in% tbl_columns_selected) || !(is.numeric(dataset_quadruple_v_re[row_index, col_index]))){
            table_noise[row_index, col_index] = dataset_quadruple_v_re[row_index, col_index]
          } else {
            table_noise[row_index, col_index] = dataset_quadruple_v_re[row_index, col_index] + noise_array[array_index] * standard_deviation[col_index]
            array_index = array_index + 1
          }
          
        }
        
      }
    } else if (new_datapoints == 'individual') {
      set.seed(seed)
      dataset_individual_v_re <- dataset_nan[sample(nrow(dataset_nan), amount_new_datapoints, replace = TRUE), ]
      table_noise <- data.frame(matrix(0, ncol = ncol(dataset_individual_v_re), nrow = nrow(dataset_individual_v_re)))
      for (col_index in 1:ncol(dataset_individual_v_re)){
        for (row_index in 1:nrow(dataset_individual_v_re)){
          # if column is not selected or column contains non-numeric values
          if (!(col_index %in% tbl_columns_selected) || !(is.numeric(dataset_individual_v_re[row_index, col_index]))){
            table_noise[row_index, col_index] = dataset_individual_v_re[row_index, col_index]
          } else {
            table_noise[row_index, col_index] = dataset_individual_v_re[row_index, col_index] + noise_array[array_index] * standard_deviation[col_index]
            array_index = array_index + 1
          }
          
        }
        
      }
    }
  }
  # return the datatable with all variables and selected are noised
  
  return(table_noise)
  
}