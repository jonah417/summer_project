# Estimate section function

# Inputs
#  - dsmodel
#  - covar_pres (?may need to be made into a global variable from reformat_data)
#  - covar_fields
#  - meta.data

estimate <- function(dsmodel, covar_pres, covar_fields, meta.data) {
  # starting the estimator section
  cat("ESTIMATE;", file=command.file.name, "\n", append=TRUE)
  
  # we are only interested in the estimates for detection probability
  cat("DETECTION ALL;", file=command.file.name, "\n", append=TRUE)
  
  # a messy way of accessing the model parameters
  mod_paste <- paste(dsmodel)
  mod_vals <- try(eval(parse(text=mod_paste[2:length(modpaste)])))
  
  # create a list for the potential dsmodel inputs
  key_opts <- list(var=mod_vals$key,
                   conditions=list("hn","hr","unif","gamma"),
                   results=list("HNORMAL","HAZARD","UNIFORM","NEXPON"))
  adj_opts <- list(var=mod_vals$adj.series,
                   conditions=list("cos","herm","poly"),
                   results=list(" /ADJUST=COSINE"," /ADJUST=HERMITE"," /ADJUST=POLY"))
  scale_opts <- list(var=mod_vals$adj.scale,
                     conditions=list("width","scale"),
                     results=list(" /ADJSTD=W"," /ADJSTD=SIGMA"))
  ds_opts <- list(key_opts,adj_opts,scale_opts)
  
  # start the estimator line and specify the key function
  cat("ESTIMATOR /KEY=", file=command.file.name, append=TRUE)
  # loop through and concatenate the correct line based upon the input
  for(i in 1:length(ds_opts)){
    cat_conditions(opt_list[[i]][[1]],opt_list[[i]][[3]],opt_list[[i]][[2]],FALSE)
  }
  
  # add additional information about adjustment parameters if they are used
  if(is.null(mod_vals$adj.series) == FALSE){
    adj_pres <- TRUE
    # specify the order of adjustment terms
    cat(paste(" /ORDER=", paste(mod_vals$adj.order,collapse=","),sep=""), 
        file=command.file.name, append=TRUE)
    # specify the number of adjustment parameters
    cat(paste(" /NAP=", length(mod_vals$adj.order), sep=""), 
        file=command.file.name, append=TRUE)
  }
  
  # specify which fields are covariates
  if(covar_pres == TRUE){
    cat(paste(" /COVARIATES=", paste(covar_fields,collapse=","), sep=""), 
        file=command.file.name, append=TRUE)
  }
  
  # ending the ESTIMATOR line
  cat(";", file=command.file.name, "\n", append=TRUE)
  
  # specifying monotonicity constraint
  if(is.null(meta.data$mono.strict)){
    meta.data$mono.strict <- FALSE
  }
  if(is.null(meta.data$mono)){
    meta.data$mono <- FALSE
  }
  
  if(meta.data$mono.strict == TRUE){
    cat("MONOTONE=STRICT;", file=command.file.name, "\n", append=TRUE)
  }else if(meta.data$mono == TRUE){
    cat("MONOTONE=WEAK;", file=command.file.name, "\n", append=TRUE)
  }else{
    cat("MONOTONE=NONE;", file=command.file.name, "\n", append=TRUE)
  }
  
  # specifying the truncation distance
  cat(paste("DISTANCE /WIDTH=",meta.data$width,sep=""), 
      file=command.file.name, append=TRUE)
  # dealing with grouped data
  if(is.null(meta.data$binned) == FALSE){
    if(meta.data$binned == TRUE){
      cat(paste(" /INTERVALS=", paste(meta.data$breaks, collapse=","),
                sep=""), file=command.file.name, append=TRUE)
    }
  }
  
  # specifying left trunction distance, if included in the input
  if(is.null(meta.data$left) == FALSE){
    cat(paste(" /LEFT=", meta.data$left, sep=""), 
        file=command.file.name, append=TRUE)
  }
  cat(";", file=command.file.name, "\n", append=TRUE)
  cat("END;", file=command.file.name, append=TRUE)
}