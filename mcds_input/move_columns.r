# a function to ensure that the fields of the inputted data are all
# present and in the correct order

move_fields <- function(data,required_cols,cluster=FALSE) {
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
  
  # once we have checked that all required fields are present, we can
  # rearrange them
  
  # adding the size columns if there are clusters
  if(cluster == TRUE){
    required_cols <- append(required_cols,"size")
  }
  
  # add effort column in some form
  if(TRUE %in% grepl("^Effort$",colnames(data))){
    required_cols <- append(required_cols,"Effort")
  }else if(TRUE %in% grepl("^Search.time$",colnames(data))){
    required_cols <- append(required_cols,"Search.time")
  }
  
  # check if other defined fields are columns in the dataset
  if(TRUE %in% grepl("^Region.Label$",colnames(data))){
    required_cols <- append(required_cols,"Region.Label")
  }
  if(TRUE %in% grepl("^Area$",colnames(data))){
    required_cols <- append(required_cols,"Area")
  }
  
  # separate all remaining fields
  extra_cols <- colnames(data)
  extra_cols <- extra_cols[! extra_cols %in% required_cols]
  
  # recombine the fields in required order
  new_cols <- append(required_cols,extra_cols)
  
  # rearranging the columns in the dataset
  data <- data[new_cols]
  
  # return the dataset with rearranged columns
  return(data)
}