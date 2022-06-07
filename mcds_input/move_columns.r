# a function to ensure that the fields of the inputted data are all
# present and in the correct order

move_columns <- function(data,required_cols,cluster=FALSE) {
  #print("run")
  # check if all required columns are present
  if(length(required_cols[! required_cols %in% colnames(data)]) > 0){
    stop("Data does not have all required fields")
  }else{
    # check if there is a field for effort
    if(! TRUE %in% grepl("^Effort$",colnames(data))){
      # sometimes Search.time is used rather than Effort
      if(! TRUE %in% grepl("^Search.time$",colnames(data))){
        # if there aren't fields for either Effort or Search.time, then
        # there is no field that can be used for effort
        stop("Data does not have all required fields")
      }
    }
  }
  # if the data is in clusters, size is a required field
  if(cluster == TRUE){
    if(! TRUE %in% grepl("^size$",colnames(data))){
      stop("Data does not have all required fields")
    }
  }
  print("valid")
  
  # once we have checked that all required fields are present, we can
  # rearrange them
  
  # adding the size columns if there are clusters
  if(cluster == TRUE){
    required_cols <- append(required_cols,"size")
  }
  print("size")
  
  # find which field will be used for the effort and change the name 
  # to match field name in mcds
  if(TRUE %in% grepl("^Effort$",colnames(data))){
    colnames(data)[colnames(data)=="Effort"] <- "SMP_EFFORT"
  }else if(TRUE %in% grepl("^Search.time$",colnames(data))){
    colnames(data)[colnames(data)=="Search.time"] <- "SMP_EFFORT"
  }
  # add effort field to required fields
  required_cols <- append(required_cols,"SMP_EFFORT")
  print("effort")
  
  # check if other defined fields are columns in the dataset
  if(TRUE %in% grepl("^Region.Label$",colnames(data))){
    colnames(data)[colnames(data)=="Region.Label"] <- "STR_LABEL"
    required_cols <- append(required_cols,"STR_LABEL")
  }
  if(TRUE %in% grepl("^Area$",colnames(data))){
    colnames(data)[colnames(data)=="Area"] <- "STR_AREA"
    required_cols <- append(required_cols,"STR_AREA")
  }
  print("extra")
  
  # change sample label field name to match mcds
  # col_index <- grep("Sample.Label",colnames(data))
  # print(colnames(data)[col_index])
  # print(col_index)
  # print(colnames(data))
  # colnames(data)[col_index] <- "SMP_LABEL"
  colnames(data)[colnames(data)=="Sample.Label"] <- "SMP_LABEL"
  required_cols[required_cols=="Sample.Label"] <- "SMP_LABEL"
  
  # separate all remaining fields
  extra_cols <- colnames(data)
  extra_cols <- extra_cols[! extra_cols %in% required_cols]
  print("covars")
  
  # recombine the fields in required order
  new_cols <- append(required_cols,extra_cols)
  print(colnames(data))
  print(new_cols)
  
  # rearranging the columns in the dataset
  data <- data[, new_cols]
  print("reorder")
  
  # return the dataset with rearranged columns
  return(data)
}