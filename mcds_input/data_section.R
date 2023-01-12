# Data Section Function

data_section <- function(data, covar_pres, covar_fields) {
  cat_file("DATA /STRUCTURE=FLAT;")
  
  # change all fields to upper case and combine to one string
  fields_comb <- paste(toupper(colnames(data)), collapse=",")
  cat_file(paste("FIELDS=", fields_comb, ";", sep=""))
  
  # specifying which fields are factor covariates
  if(covar_pres == TRUE){
    factor_fields <- c()
    for(i in 1:length(colnames(data))){
      # for each covariate, check if it is a factor
      if(is.factor(data[,i]) && (TRUE %in% grepl(colnames(data)[i],covar_fields))){
        # if the covariate is a factor, specify its name, levels, and level labels
        factor_fields <- append(factor_fields,colnames(data)[i])
        labels <- paste(levels(data[,i]), collapse=",")
        cat_file(paste("FACTOR /NAME=", toupper(colnames(data)[i]), 
                  " /LEVELS=", length(levels(data[,i])), " /LABELS=", 
                  labels, sep=""))
      }
    }
  }
}