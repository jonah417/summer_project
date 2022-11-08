# Reformat Data Function

# This functions ensures that the data file that is used to create the commands
# in the command file is in the correct format

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
  # create a list of required fields and likely alternative names
  req_fields <- list(list("SMP_LABEL", c("SMP_LABEL","sample.label")),"SMP_EFFORT","DISTANCE")
  # create a vector of optional specified fields
  opt_fields <- c("STR_LABEL","STR_AREA","size")
  
  # change the distance column name to upper case
  colnames(data)[grep("^distance$",tolower(colnames(data)))] <- "DISTANCE"
  
  # function for field names
  # rename columns and deal with required variables
  # check if covars present
    # function for covars
  # creating the dataframe
  # output new dataframe
}

# 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