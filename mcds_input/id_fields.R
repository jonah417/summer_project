# Identify Field Names Function

# parameters:
# data_cols - vector of original column names
# fin_name - string of desired final name of the column
# pot_names - vector of potential names the column may originally have

id_fields <- function(data_cols, fin_name, pot_names) {
  # first, identify which column contains the relevant data
  # we want to check through each of the potential names to see which appears
  # in the column names, so we need to know the number of potential names and
  # keep track of whether it is present
  name_pres <- FALSE
  # convert vector of potential names to lower case for ease of comparison
  pot_names <- tolower(pot_names)
  # check through each of the potential names to identify if it is used for a
  # column
  for(i in 1:length(pot_names)) {
    if(TRUE %in% grepl(pot_names[i],tolower(data_cols))) {
      # identify the index of the column that needs renamed
      name_pres <- grep(pot_names[i],tolower(data_cols))
    }
  }
  # return either that the name wasn't found, or the index it was found at
  return(name_pres)
}





