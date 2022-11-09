# Reformat Data Function

# This functions ensures that the data file that is used to create the commands
# in the command file is in the correct format

# Inputs:
# data - the original dataframe to be reformatted
# dsmodel - the model specifying the covariates to be included for analysis

reformat_data <- function(data, dsmodel){
  # firstly, identifying if the data has multiple observers
  if(TRUE %in% grepl("^detected$",tolower(colnames(data)))){
    # combine data from multiple observers
    data <- data[data$detected==1,]
    if(TRUE %in% grepl("^object$",tolower(colnames(data)))){
      # in case the object column isn't named 'object', to remove case sensitivity
      obj_col <- grep("^object$",tolower(colnames(data)))
      colnames(data)[obj_col] <- "object"
      # identifying all objects and taking the first data point for each
      obj_nums <- unique(data$object)
      for(i in 1:length(obj_nums)){
        entries <- grep(TRUE,data$object==i)
        if(length(entries)>1){
          remove <- entries[-1]
          data <- data[-remove,]
        }
      }
    }
  }
  
  # matching the names to those used in MCDS
  # create a list of recognised fields along with likely alternative names and
  # whether the are required
  req_fields <- list(list("SMP_LABEL", c("SMP_LABEL","sample.label"), TRUE),
                     list("SMP_EFFORT", c("SMP_EFFORT","effort","Search.time"), TRUE),
                     list("DISTANCE", c("distance"), TRUE), 
                     list("STR_LABEL", c("STR_LABEL","region.label"), FALSE),
                     list("STR_AREA", c("STR_AREA","area"), FALSE),
                     list("size", c("size"), FALSE))
  
  # create a vector to store the indices of the recognised fields
  fields_index <- rep("", length(req_fields))
  # for each of the required fields, find the column and add in if not present
  for(i in 1:length(req_fields)) {
    # record which column the ith required field is found in the dataframe
    fields_index[i] <- id_fields(colnames(data),req_fields[[i]][[1]],req_fields[[i]][[2]])
    # if the field is not present but is required, add in a dummy variable
    if(fields_index$index[i] == FALSE && req_fields[[i]][[3]] == TRUE) {
      data$new <- rep(1,nrow(data))
      tail(colnames(data),1) <- req_fields[[i]][[1]]
    } else {
      # rename the column to match the name required by MCDS
      colnames(data)[fields_index[i]] <- req_fields[[i]][[1]]
    }
  }
  
  # specifying covariates in the model
  if(identical(all.vars(dsmodel),character(0)) == FALSE){
    covar_pres <- TRUE
    # extracting the list of covariates
    covars <- all.vars(dsmodel)
    # creating a list of the fields for each covariate
    covar_fields <- rep("",length(covars))
    
    # finding the index of each covariate
    for(i in 1:length(covars)){
      # identifying the field name for each covariate
      index <- id_fields(colnames(data), covars[i], covars[i])
      covar_fields[i] <- colnames(data)[index]
    }
    # the required fields cannot be covariates in the model, with the exception of size
    if(length(intersect(tolower(req_fields),tolower(covar_fields))) > 0){
      if(TRUE %in% grepl("size",tolower(covar_fields))){
        # specify whether SIZE is a covariate
        size_cov <- TRUE
      }else{
        size_cov <- FALSE
      }
      # remove any required fields from the list of covariates
      covar_fields <- covar_fields[! covar_fields %in% intersect(req_fields,covar_fields)]
    }
    # add covariates to the fields that are kept for analysis
    req_fields <- c(req_fields,covar_fields)
    # if SIZE is a covariate, add it back to the list of covariates
    if(size_cov == TRUE){
      covar_fields <- append(covar_fields,"SIZE")
    }
  }else{
    covar_pres <- FALSE
  }
  
  # remove all non-essential columns from the dataset
  data <- data[req_fields]
  
  # return reformatted dataframe
  return(data)
}

# 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