###
# This function reads the dataset, regardless of file type, separator
# and decimal point and returns the dataset.
###

DatasetInput <- function(inFile) {
  # check automatically whether the separator is ";" or "," and whether the decimal is "." or ","
  if (typeof(inFile) == "list") {
    if(is.null(inFile))
    return(NULL)
    if (inFile$type == "text/csv") {
      tryCatch(expr = {
        # assumption: header = TRUE, sep = ,
        dataset <- read.csv(inFile$datapath, header = TRUE, sep = "," , dec = ".")
        dataset_comma <- read.csv(inFile$datapath, header = TRUE, sep = "," , dec = ",")
        dataset_dot <- read.csv(inFile$datapath, header = TRUE, sep = "," , dec = ".")
        types_comma <- sapply(dataset_comma[1,], typeof)
        names(types_comma) <- NULL
        types_dot <- sapply(dataset_dot[1,], typeof)
        names(types_dot) <- NULL
        # check the correct decimal point
        if (length(types_comma[which(types_comma == "character")]) < length(types_dot[which(types_dot == "character")])) {
          dataset <- read.csv(inFile$datapath, header = TRUE, sep = "," , dec = ",")
          
          # if the amount of different types changes -> Header = TRUE
          dataset_head <- read.csv(inFile$datapath, header = TRUE, sep = "," , dec = ",")
          dataset_no_head <- read.csv(inFile$datapath, header = FALSE, sep = "," , dec = ",")
          types_with_head <- sapply(dataset_head[1,], typeof)
          types_without_head <- sapply(dataset_no_head[1,], typeof)
          different_types <- types_with_head == types_without_head
          names(different_types) <- NULL
          if (FALSE %in% different_types) {
            dataset <- read.csv(inFile$datapath, header = TRUE, sep = "," , dec = ",")
          } else {
            dataset <- read.csv(inFile$datapath, header = FALSE, sep = "," , dec = ",")
          }
        } else {
          dataset <- read.csv(inFile$datapath, header = TRUE, sep = "," , dec = ".")
          
          # if the amount of different types changes -> Header = TRUE
          dataset_head <- read.csv(inFile$datapath, header = TRUE, sep = "," , dec = ".")
          dataset_no_head <- read.csv(inFile$datapath, header = FALSE, sep = "," , dec = ".")
          types_with_head <- sapply(dataset_head[1,], typeof)
          types_without_head <- sapply(dataset_no_head[1,], typeof)
          different_types <- types_with_head == types_without_head
          names(different_types) <- NULL
          if (FALSE %in% different_types) {
            dataset <- read.csv(inFile$datapath, header = TRUE, sep = "," , dec = ".")
          } else {
            dataset <- read.csv(inFile$datapath, header = FALSE, sep = "," , dec = ".")
          }
        }
        # check for the correct separator
        if (ncol(dataset) <= 1) {
          dataset <- read.csv(inFile$datapath, header = TRUE, sep = ";" , dec = ".")
          dataset_comma <- read.csv(inFile$datapath, header = TRUE, sep = ";" , dec = ",")
          dataset_dot <- read.csv(inFile$datapath, header = TRUE, sep = ";" , dec = ".")
          types_comma <- sapply(dataset_comma[1,], typeof)
          names(types_comma) <- NULL
          types_dot <- sapply(dataset_dot[1,], typeof)
          names(types_dot) <- NULL
          if (length(types_comma[which(types_comma == "character")]) < length(types_dot[which(types_dot == "character")])) {
            dataset <- read.csv(inFile$datapath, header = TRUE, sep = ";" , dec = ",")
            
            # if the amount of different types changes -> Header = TRUE
            dataset_head <- read.csv(inFile$datapath, header = TRUE, sep = ";" , dec = ",")
            dataset_no_head <- read.csv(inFile$datapath, header = FALSE, sep = ";" , dec = ",")
            types_with_head <- sapply(dataset_head[1,], typeof)
            types_without_head <- sapply(dataset_no_head[1,], typeof)
            different_types <- types_with_head == types_without_head
            names(different_types) <- NULL
            if (FALSE %in% different_types) {
              dataset <- read.csv(inFile$datapath, header = TRUE, sep = ";" , dec = ",")
            } else {
              dataset <- read.csv(inFile$datapath, header = FALSE, sep = ";" , dec = ",")
            }
          } else {
            dataset <- read.csv(inFile$datapath, header = TRUE, sep = ";" , dec = ".")
            
            # if the amount of different types changes -> Header = TRUE
            dataset_head <- read.csv(inFile$datapath, header = TRUE, sep = ";" , dec = ".")
            dataset_no_head <- read.csv(inFile$datapath, header = FALSE, sep = ";" , dec = ".")
            types_with_head <- sapply(dataset_head[1,], typeof)
            types_without_head <- sapply(dataset_no_head[1,], typeof)
            different_types <- types_with_head == types_without_head
            names(different_types) <- NULL
            if (FALSE %in% different_types) {
              dataset <- read.csv(inFile$datapath, header = TRUE, sep = ";" , dec = ".")
            } else {
              dataset <- read.csv(inFile$datapath, header = FALSE, sep = ";" , dec = ".")
            }
          }
        }

        return(dataset)
      }, error = function(e) {
        # assumption: header = TRUE, sep = ;
        dataset <- read.csv(inFile$datapath, header = TRUE, sep = ";" , dec = ".")
        dataset_comma <- read.csv(inFile$datapath, header = TRUE, sep = ";" , dec = ",")
        dataset_dot <- read.csv(inFile$datapath, header = TRUE, sep = ";" , dec = ".")
        types_comma <- sapply(dataset_comma[1,], typeof)
        names(types_comma) <- NULL
        types_dot <- sapply(dataset_dot[1,], typeof)
        names(types_dot) <- NULL
        if (length(types_comma[which(types_comma == "character")]) < length(types_dot[which(types_dot == "character")])) {
          dataset <- read.csv(inFile$datapath, header = TRUE, sep = ";" , dec = ",")
          
          # if the amount of different types changes -> Header = TRUE
          dataset_head <- read.csv(inFile$datapath, header = TRUE, sep = ";" , dec = ",")
          dataset_no_head <- read.csv(inFile$datapath, header = FALSE, sep = ";" , dec = ",")
          types_with_head <- sapply(dataset_head[1,], typeof)
          types_without_head <- sapply(dataset_no_head[1,], typeof)
          different_types <- types_with_head == types_without_head
          names(different_types) <- NULL
          if (FALSE %in% different_types) {
            read.csv(inFile$datapath, header = TRUE, sep = ";" , dec = ",")
          } else {
            read.csv(inFile$datapath, header = FALSE, sep = ";" , dec = ",")
          }
        } else {
          dataset <- read.csv(inFile$datapath, header = TRUE, sep = ";" , dec = ".")
          
          # if the amount of different types changes -> Header = TRUE
          dataset_head <- read.csv(inFile$datapath, header = TRUE, sep = ";" , dec = ".")
          dataset_no_head <- read.csv(inFile$datapath, header = FALSE, sep = ";" , dec = ".")
          types_with_head <- sapply(dataset_head[1,], typeof)
          types_without_head <- sapply(dataset_no_head[1,], typeof)
          different_types <- types_with_head == types_without_head
          names(different_types) <- NULL
          if (FALSE %in% different_types) {
            read.csv(inFile$datapath, header = TRUE, sep = ";" , dec = ".")
          } else {
            read.csv(inFile$datapath, header = FALSE, sep = ";" , dec = ".")
          }
        }
        return(dataset)
      })
  }
    else {
      tryCatch(expr = {
        # assumption: header = TRUE, sep = ,
        dataset <- read.delim(inFile$datapath, header = TRUE, sep = "," , dec = ".")
        dataset_comma <- read.delim(inFile$datapath, header = TRUE, sep = "," , dec = ",")
        dataset_dot <- read.delim(inFile$datapath, header = TRUE, sep = "," , dec = ".")
        types_comma <- sapply(dataset_comma[1,], typeof)
        names(types_comma) <- NULL
        types_dot <- sapply(dataset_dot[1,], typeof)
        names(types_dot) <- NULL
        if (length(types_comma[which(types_comma == "character")]) < length(types_dot[which(types_dot == "character")])) {
          dataset <- read.delim(inFile$datapath, header = TRUE, sep = "," , dec = ",")
          
          # if the amount of different types changes -> Header = TRUE
          dataset_head <- read.delim(inFile$datapath, header = TRUE, sep = "," , dec = ",")
          dataset_no_head <- read.delim(inFile$datapath, header = FALSE, sep = "," , dec = ",")
          types_with_head <- sapply(dataset_head[1,], typeof)
          types_without_head <- sapply(dataset_no_head[1,], typeof)
          different_types <- types_with_head == types_without_head
          names(different_types) <- NULL
          if (FALSE %in% different_types) {
            dataset <- read.delim(inFile$datapath, header = TRUE, sep = "," , dec = ",")
          } else {
            dataset <- read.delim(inFile$datapath, header = FALSE, sep = "," , dec = ",")
          }
        } else {
          dataset <- read.delim(inFile$datapath, header = TRUE, sep = "," , dec = ".")
          
          # if the amount of different types changes -> Header = TRUE
          dataset_head <- read.delim(inFile$datapath, header = TRUE, sep = "," , dec = ".")
          dataset_no_head <- read.delim(inFile$datapath, header = FALSE, sep = "," , dec = ".")
          types_with_head <- sapply(dataset_head[1,], typeof)
          types_without_head <- sapply(dataset_no_head[1,], typeof)
          different_types <- types_with_head == types_without_head
          names(different_types) <- NULL
          if (FALSE %in% different_types) {
            dataset <- read.delim(inFile$datapath, header = TRUE, sep = "," , dec = ".")
          } else {
            dataset <- read.delim(inFile$datapath, header = FALSE, sep = "," , dec = ".")
          }
        }
        # check for the correct separator
        if (ncol(dataset) <= 1) {
          dataset <- read.delim(inFile$datapath, header = TRUE, sep = ";" , dec = ".")
          dataset_comma <- read.delim(inFile$datapath, header = TRUE, sep = ";" , dec = ",")
          dataset_dot <- read.delim(inFile$datapath, header = TRUE, sep = ";" , dec = ".")
          types_comma <- sapply(dataset_comma[1,], typeof)
          names(types_comma) <- NULL
          types_dot <- sapply(dataset_dot[1,], typeof)
          names(types_dot) <- NULL
          if (length(types_comma[which(types_comma == "character")]) < length(types_dot[which(types_dot == "character")])) {
            dataset <- read.delim(inFile$datapath, header = TRUE, sep = ";" , dec = ",")
            
            # if the amount of different types changes -> Header = TRUE
            dataset_head <- read.delim(inFile$datapath, header = TRUE, sep = ";" , dec = ",")
            dataset_no_head <- read.delim(inFile$datapath, header = FALSE, sep = ";" , dec = ",")
            types_with_head <- sapply(dataset_head[1,], typeof)
            types_without_head <- sapply(dataset_no_head[1,], typeof)
            different_types <- types_with_head == types_without_head
            names(different_types) <- NULL
            if (FALSE %in% different_types) {
              dataset <- read.delim(inFile$datapath, header = TRUE, sep = ";" , dec = ",")
            } else {
              dataset <- read.delim(inFile$datapath, header = FALSE, sep = ";" , dec = ",")
            }
          } else {
            dataset <- read.delim(inFile$datapath, header = TRUE, sep = ";" , dec = ".")
            
            # if the amount of different types changes -> Header = TRUE
            dataset_head <- read.delim(inFile$datapath, header = TRUE, sep = ";" , dec = ".")
            dataset_no_head <- read.delim(inFile$datapath, header = FALSE, sep = ";" , dec = ".")
            types_with_head <- sapply(dataset_head[1,], typeof)
            types_without_head <- sapply(dataset_no_head[1,], typeof)
            different_types <- types_with_head == types_without_head
            names(different_types) <- NULL
            if (FALSE %in% different_types) {
              dataset <- read.delim(inFile$datapath, header = TRUE, sep = ";" , dec = ".")
            } else {
              dataset <- read.delim(inFile$datapath, header = FALSE, sep = ";" , dec = ".")
            }
          }
        }
        
        return(dataset)
      }, error = function(e) {
        # assumption: header = TRUE, sep = ;
        dataset <- read.delim(inFile$datapath, header = TRUE, sep = ";" , dec = ".")
        dataset_comma <- read.delim(inFile$datapath, header = TRUE, sep = ";" , dec = ",")
        dataset_dot <- read.delim(inFile$datapath, header = TRUE, sep = ";" , dec = ".")
        types_comma <- sapply(dataset_comma[1,], typeof)
        names(types_comma) <- NULL
        types_dot <- sapply(dataset_dot[1,], typeof)
        names(types_dot) <- NULL
        if (length(types_comma[which(types_comma == "character")]) < length(types_dot[which(types_dot == "character")])) {
          dataset <- read.delim(inFile$datapath, header = TRUE, sep = ";" , dec = ",")
          
          # if the amount of different types changes -> Header = TRUE
          dataset_head <- read.delim(inFile$datapath, header = TRUE, sep = ";" , dec = ",")
          dataset_no_head <- read.delim(inFile$datapath, header = FALSE, sep = ";" , dec = ",")
          types_with_head <- sapply(dataset_head[1,], typeof)
          types_without_head <- sapply(dataset_no_head[1,], typeof)
          different_types <- types_with_head == types_without_head
          names(different_types) <- NULL
          if (FALSE %in% different_types) {
            read.delim(inFile$datapath, header = TRUE, sep = ";" , dec = ",")
          } else {
            read.delim(inFile$datapath, header = FALSE, sep = ";" , dec = ",")
          }
        } else {
          dataset <- read.delim(inFile$datapath, header = TRUE, sep = ";" , dec = ".")
          
          # if the amount of different types changes -> Header = TRUE
          dataset_head <- read.delim(inFile$datapath, header = TRUE, sep = ";" , dec = ".")
          dataset_no_head <- read.delim(inFile$datapath, header = FALSE, sep = ";" , dec = ".")
          types_with_head <- sapply(dataset_head[1,], typeof)
          types_without_head <- sapply(dataset_no_head[1,], typeof)
          different_types <- types_with_head == types_without_head
          names(different_types) <- NULL
          if (FALSE %in% different_types) {
            read.delim(inFile$datapath, header = TRUE, sep = ";" , dec = ".")
          } else {
            read.delim(inFile$datapath, header = FALSE, sep = ";" , dec = ".")
          }
        }
        return(dataset)
      })
    }
    
  } else {
    if(is.null(inFile))
      return(NULL)
    inFile_type <- tail(strsplit(inFile, ".", fixed = TRUE)[[1]], n=1)
    if (inFile_type == "csv") {
      tryCatch(expr = {
        # assumption: header = TRUE, sep = ,
        dataset <- read.csv(inFile, header = TRUE, sep = "," , dec = ".")
        dataset_comma <- read.csv(inFile, header = TRUE, sep = "," , dec = ",")
        dataset_dot <- read.csv(inFile, header = TRUE, sep = "," , dec = ".")
        types_comma <- sapply(dataset_comma[1,], typeof)
        names(types_comma) <- NULL
        types_dot <- sapply(dataset_dot[1,], typeof)
        names(types_dot) <- NULL
        if (length(types_comma[which(types_comma == "character")]) < length(types_dot[which(types_dot == "character")])) {
          dataset <- read.csv(inFile, header = TRUE, sep = "," , dec = ",")
          
          # if the amount of different types changes -> Header = TRUE
          dataset_head <- read.csv(inFile, header = TRUE, sep = "," , dec = ",")
          dataset_no_head <- read.csv(inFile, header = FALSE, sep = "," , dec = ",")
          types_with_head <- sapply(dataset_head[1,], typeof)
          types_without_head <- sapply(dataset_no_head[1,], typeof)
          different_types <- types_with_head == types_without_head
          names(different_types) <- NULL
          if (FALSE %in% different_types) {
            dataset <- read.csv(inFile, header = TRUE, sep = "," , dec = ",")
          } else {
            dataset <- read.csv(inFile, header = FALSE, sep = "," , dec = ",")
          }
        } else {
          dataset <- read.csv(inFile, header = TRUE, sep = "," , dec = ".")
          
          # if the amount of different types changes -> Header = TRUE
          dataset_head <- read.csv(inFile, header = TRUE, sep = "," , dec = ".")
          dataset_no_head <- read.csv(inFile, header = FALSE, sep = "," , dec = ".")
          types_with_head <- sapply(dataset_head[1,], typeof)
          types_without_head <- sapply(dataset_no_head[1,], typeof)
          different_types <- types_with_head == types_without_head
          names(different_types) <- NULL
          if (FALSE %in% different_types) {
            dataset <- read.csv(inFile, header = TRUE, sep = "," , dec = ".")
          } else {
            dataset <- read.csv(inFile, header = FALSE, sep = "," , dec = ".")
          }
        }
        # check for the correct separator
        if (ncol(dataset) <= 1) {
          dataset <- read.csv(inFile, header = TRUE, sep = ";" , dec = ".")
          dataset_comma <- read.csv(inFile, header = TRUE, sep = ";" , dec = ",")
          dataset_dot <- read.csv(inFile, header = TRUE, sep = ";" , dec = ".")
          types_comma <- sapply(dataset_comma[1,], typeof)
          names(types_comma) <- NULL
          types_dot <- sapply(dataset_dot[1,], typeof)
          names(types_dot) <- NULL
          if (length(types_comma[which(types_comma == "character")]) < length(types_dot[which(types_dot == "character")])) {
            dataset <- read.csv(inFile, header = TRUE, sep = ";" , dec = ",")
            
            # if the amount of different types changes -> Header = TRUE
            dataset_head <- read.csv(inFile, header = TRUE, sep = ";" , dec = ",")
            dataset_no_head <- read.csv(inFile, header = FALSE, sep = ";" , dec = ",")
            types_with_head <- sapply(dataset_head[1,], typeof)
            types_without_head <- sapply(dataset_no_head[1,], typeof)
            different_types <- types_with_head == types_without_head
            names(different_types) <- NULL
            if (FALSE %in% different_types) {
              dataset <- read.csv(inFile, header = TRUE, sep = ";" , dec = ",")
            } else {
              dataset <- read.csv(inFile, header = FALSE, sep = ";" , dec = ",")
            }
          } else {
            dataset <- read.csv(inFile, header = TRUE, sep = ";" , dec = ".")
            
            # if the amount of different types changes -> Header = TRUE
            dataset_head <- read.csv(inFile, header = TRUE, sep = ";" , dec = ".")
            dataset_no_head <- read.csv(inFile, header = FALSE, sep = ";" , dec = ".")
            types_with_head <- sapply(dataset_head[1,], typeof)
            types_without_head <- sapply(dataset_no_head[1,], typeof)
            different_types <- types_with_head == types_without_head
            names(different_types) <- NULL
            if (FALSE %in% different_types) {
              dataset <- read.csv(inFile, header = TRUE, sep = ";" , dec = ".")
            } else {
              dataset <- read.csv(inFile, header = FALSE, sep = ";" , dec = ".")
            }
          }
        }
        
        return(dataset)
      }, error = function(e) {
        # assumption: header = TRUE, sep = ;
        dataset <- read.csv(inFile, header = TRUE, sep = ";" , dec = ".")
        dataset_comma <- read.csv(inFile, header = TRUE, sep = ";" , dec = ",")
        dataset_dot <- read.csv(inFile, header = TRUE, sep = ";" , dec = ".")
        types_comma <- sapply(dataset_comma[1,], typeof)
        names(types_comma) <- NULL
        types_dot <- sapply(dataset_dot[1,], typeof)
        names(types_dot) <- NULL
        if (length(types_comma[which(types_comma == "character")]) < length(types_dot[which(types_dot == "character")])) {
          dataset <- read.csv(inFile, header = TRUE, sep = ";" , dec = ",")
          
          # if the amount of different types changes -> Header = TRUE
          dataset_head <- read.csv(inFile, header = TRUE, sep = ";" , dec = ",")
          dataset_no_head <- read.csv(inFile, header = FALSE, sep = ";" , dec = ",")
          types_with_head <- sapply(dataset_head[1,], typeof)
          types_without_head <- sapply(dataset_no_head[1,], typeof)
          different_types <- types_with_head == types_without_head
          names(different_types) <- NULL
          if (FALSE %in% different_types) {
            read.csv(inFile, header = TRUE, sep = ";" , dec = ",")
          } else {
            read.csv(inFile, header = FALSE, sep = ";" , dec = ",")
          }
        } else {
          dataset <- read.csv(inFile, header = TRUE, sep = ";" , dec = ".")
          
          # if the amount of different types changes -> Header = TRUE
          dataset_head <- read.csv(inFile, header = TRUE, sep = ";" , dec = ".")
          dataset_no_head <- read.csv(inFile, header = FALSE, sep = ";" , dec = ".")
          types_with_head <- sapply(dataset_head[1,], typeof)
          types_without_head <- sapply(dataset_no_head[1,], typeof)
          different_types <- types_with_head == types_without_head
          names(different_types) <- NULL
          if (FALSE %in% different_types) {
            read.csv(inFile, header = TRUE, sep = ";" , dec = ".")
          } else {
            read.csv(inFile, header = FALSE, sep = ";" , dec = ".")
          }
        }
        return(dataset)
      })
    }
    else {
      tryCatch(expr = {
        # assumption: header = TRUE, sep = ,
        dataset <- read.delim(inFile, header = TRUE, sep = "," , dec = ".")
        dataset_comma <- read.delim(inFile, header = TRUE, sep = "," , dec = ",")
        dataset_dot <- read.delim(inFile, header = TRUE, sep = "," , dec = ".")
        types_comma <- sapply(dataset_comma[1,], typeof)
        names(types_comma) <- NULL
        types_dot <- sapply(dataset_dot[1,], typeof)
        names(types_dot) <- NULL
        if (length(types_comma[which(types_comma == "character")]) < length(types_dot[which(types_dot == "character")])) {
          dataset <- read.delim(inFile, header = TRUE, sep = "," , dec = ",")
          
          # if the amount of different types changes -> Header = TRUE
          dataset_head <- read.delim(inFile, header = TRUE, sep = "," , dec = ",")
          dataset_no_head <- read.delim(inFile, header = FALSE, sep = "," , dec = ",")
          types_with_head <- sapply(dataset_head[1,], typeof)
          types_without_head <- sapply(dataset_no_head[1,], typeof)
          different_types <- types_with_head == types_without_head
          names(different_types) <- NULL
          if (FALSE %in% different_types) {
            dataset <- read.delim(inFile, header = TRUE, sep = "," , dec = ",")
          } else {
            dataset <- read.delim(inFile, header = FALSE, sep = "," , dec = ",")
          }
        } else {
          dataset <- read.delim(inFile, header = TRUE, sep = "," , dec = ".")
          
          # if the amount of different types changes -> Header = TRUE
          dataset_head <- read.delim(inFile, header = TRUE, sep = "," , dec = ".")
          dataset_no_head <- read.delim(inFile, header = FALSE, sep = "," , dec = ".")
          types_with_head <- sapply(dataset_head[1,], typeof)
          types_without_head <- sapply(dataset_no_head[1,], typeof)
          different_types <- types_with_head == types_without_head
          names(different_types) <- NULL
          if (FALSE %in% different_types) {
            dataset <- read.delim(inFile, header = TRUE, sep = "," , dec = ".")
          } else {
            dataset <- read.delim(inFile, header = FALSE, sep = "," , dec = ".")
          }
        }
        # check for the correct separator
        if (ncol(dataset) <= 1) {
          dataset <- read.delim(inFile, header = TRUE, sep = ";" , dec = ".")
          dataset_comma <- read.delim(inFile, header = TRUE, sep = ";" , dec = ",")
          dataset_dot <- read.delim(inFile, header = TRUE, sep = ";" , dec = ".")
          types_comma <- sapply(dataset_comma[1,], typeof)
          names(types_comma) <- NULL
          types_dot <- sapply(dataset_dot[1,], typeof)
          names(types_dot) <- NULL
          if (length(types_comma[which(types_comma == "character")]) < length(types_dot[which(types_dot == "character")])) {
            dataset <- read.delim(inFile, header = TRUE, sep = ";" , dec = ",")
            
            # if the amount of different types changes -> Header = TRUE
            dataset_head <- read.delim(inFile, header = TRUE, sep = ";" , dec = ",")
            dataset_no_head <- read.delim(inFile, header = FALSE, sep = ";" , dec = ",")
            types_with_head <- sapply(dataset_head[1,], typeof)
            types_without_head <- sapply(dataset_no_head[1,], typeof)
            different_types <- types_with_head == types_without_head
            names(different_types) <- NULL
            if (FALSE %in% different_types) {
              dataset <- read.delim(inFile, header = TRUE, sep = ";" , dec = ",")
            } else {
              dataset <- read.delim(inFile, header = FALSE, sep = ";" , dec = ",")
            }
          } else {
            dataset <- read.delim(inFile, header = TRUE, sep = ";" , dec = ".")
            
            # if the amount of different types changes -> Header = TRUE
            dataset_head <- read.delim(inFile, header = TRUE, sep = ";" , dec = ".")
            dataset_no_head <- read.delim(inFile, header = FALSE, sep = ";" , dec = ".")
            types_with_head <- sapply(dataset_head[1,], typeof)
            types_without_head <- sapply(dataset_no_head[1,], typeof)
            different_types <- types_with_head == types_without_head
            names(different_types) <- NULL
            if (FALSE %in% different_types) {
              dataset <- read.delim(inFile, header = TRUE, sep = ";" , dec = ".")
            } else {
              dataset <- read.delim(inFile, header = FALSE, sep = ";" , dec = ".")
            }
          }
        }
        
        return(dataset)
      }, error = function(e) {
        # assumption: header = TRUE, sep = ;
        dataset <- read.delim(inFile, header = TRUE, sep = ";" , dec = ".")
        dataset_comma <- read.delim(inFile, header = TRUE, sep = ";" , dec = ",")
        dataset_dot <- read.delim(inFile, header = TRUE, sep = ";" , dec = ".")
        types_comma <- sapply(dataset_comma[1,], typeof)
        names(types_comma) <- NULL
        types_dot <- sapply(dataset_dot[1,], typeof)
        names(types_dot) <- NULL
        if (length(types_comma[which(types_comma == "character")]) < length(types_dot[which(types_dot == "character")])) {
          dataset <- read.delim(inFile, header = TRUE, sep = ";" , dec = ",")
          
          # if the amount of different types changes -> Header = TRUE
          dataset_head <- read.delim(inFile, header = TRUE, sep = ";" , dec = ",")
          dataset_no_head <- read.delim(inFile, header = FALSE, sep = ";" , dec = ",")
          types_with_head <- sapply(dataset_head[1,], typeof)
          types_without_head <- sapply(dataset_no_head[1,], typeof)
          different_types <- types_with_head == types_without_head
          names(different_types) <- NULL
          if (FALSE %in% different_types) {
            read.delim(inFile, header = TRUE, sep = ";" , dec = ",")
          } else {
            read.delim(inFile, header = FALSE, sep = ";" , dec = ",")
          }
        } else {
          dataset <- read.delim(inFile, header = TRUE, sep = ";" , dec = ".")
          
          # if the amount of different types changes -> Header = TRUE
          dataset_head <- read.delim(inFile, header = TRUE, sep = ";" , dec = ".")
          dataset_no_head <- read.delim(inFile, header = FALSE, sep = ";" , dec = ".")
          types_with_head <- sapply(dataset_head[1,], typeof)
          types_without_head <- sapply(dataset_no_head[1,], typeof)
          different_types <- types_with_head == types_without_head
          names(different_types) <- NULL
          if (FALSE %in% different_types) {
            read.delim(inFile, header = TRUE, sep = ";" , dec = ".")
          } else {
            read.delim(inFile, header = FALSE, sep = ";" , dec = ".")
          }
        }
        return(dataset)
      })
    }
  }
  
  }
  
  