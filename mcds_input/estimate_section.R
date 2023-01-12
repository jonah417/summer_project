# Estimate section function

# Inputs
#  - dsmodel
#  - covar_pres
#  - covar_fields
#  - meta.data

estimate_section <- function(dsmodel, covar_pres, covar_fields, meta.data) {
  # starting the estimator section
  cat_file("ESTIMATE")
  
  # we are only interested in the estimates for detection probability
  cat_file("DETECTION ALL;")
  
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
  cat_file("ESTIMATOR /KEY=", new_line = FALSE)
  # loop through and concatenate the correct line based upon the input
  for(i in 1:length(ds_opts)){
    cat_conditions(opt_list[[i]][[1]],opt_list[[i]][[3]],opt_list[[i]][[2]],FALSE)
  }
  
  # add additional information about adjustment parameters if they are used
  if(is.null(mod_vals$adj.series) == FALSE){
    adj_pres <- TRUE
    # specify the order of adjustment terms
    cat_file(paste(" /ORDER=", paste(mod_vals$adj.order,collapse=","),sep=""), 
        new_line=FALSE)
    # specify the number of adjustment parameters
    cat_file(paste(" /NAP=", length(mod_vals$adj.order), sep=""), 
        new_line=FALSE)
  }
  
  # specify which fields are covariates
  if(covar_pres == TRUE){
    cat_file(paste(" /COVARIATES=", paste(covar_fields,collapse=","), sep=""), 
        new_line=FALSE)
  }
  
  # ending the ESTIMATOR line
  cat_file(";")
  
  # specifying monotonicity constraint
  if(is.null(meta.data$mono.strict)){
    meta.data$mono.strict <- FALSE
  }
  if(is.null(meta.data$mono)){
    meta.data$mono <- FALSE
  }
  
  if(meta.data$mono.strict == TRUE){
    cat_file("MONOTONE=STRICT;")
  }else if(meta.data$mono == TRUE){
    cat_file("MONOTONE=WEAK;")
  }else{
    cat_file("MONOTONE=NONE;")
  }
  
  # specifying the truncation distance
  cat_file(paste("DISTANCE /WIDTH=",meta.data$width,sep=""), 
      new_line=FALSE)
  # dealing with grouped data
  if(is.null(meta.data$binned) == FALSE){
    if(meta.data$binned == TRUE){
      cat_file(paste(" /INTERVALS=", paste(meta.data$breaks, collapse=","),
                sep=""), new_line=FALSE)
    }
  }
  
  # specifying left trunction distance, if included in the input
  if(is.null(meta.data$left) == FALSE){
    cat_file(paste(" /LEFT=", meta.data$left, sep=""), 
        new_line=FALSE)
  }
  cat_file(";")
  cat_file("END;")
}