# assuming the inputs are in the same form as those used in ddf.R

# data must have the following fields:
# object, observer, detected, distance (matches mrds input)
# SMP_LABEL, SMP_EFFORT (required for mcds)
# optionally

create_command_file <- function(dsmodel=call(),mrmodel=call(),data,
                                method,meta.data,control) {
  # create a temporary directory
  directory <- tempdir()
  # create data file to pass to mcds
  data.file.name <- tempfile(pattern="data", tmpdir=directory,
                             fileext=".csv")
  file.create(data.file.name)
  write.csv(data, data.file.name, row.names=FALSE)
  
  # create command file
  command.file.name <- tempfile(pattern="cmdtmp", tmpdir=directory,
                                fileext=".txt")
  file.create(command.file.name)
  # output commands to it
  cat("out.txt", file=command.file.name, "\n", append=TRUE)
  cat("log.txt", file=command.file.name, "\n", append=TRUE)
  cat("stat.txt", file=command.file.name, "\n", append=TRUE)
  cat("plot.txt", file=command.file.name, "\n", append=TRUE)
  cat("None", file=command.file.name, "\n", append=TRUE)
  cat("None", file=command.file.name, "\n", append=TRUE)
  cat("OPTIONS;", file=command.file.name, "\n", append=TRUE)
  
  # if analysis is restricted to just detected observations
  if(control$limit){
    data <- data[data$detected==1,]
  }
  
  # OPTION section
  
  # !consider the case where TYPE="LINE" but DISTANCE="RADIAL"?
  if(meta.data$point == TRUE){
    cat("DISTANCE=RADIAL /UNITS='Meters' /WIDTH=", 
        meta.data$width, ";", file=command.file.name, "\n", 
        append=TRUE)
  }else{
    cat("DISTANCE=PERP /UNITS='Meters' /WIDTH=", 
        meta.data$width, ";", file=command.file.name, "\n", 
        append=TRUE)
  }
  
  # define whether there are clusters
  if(TRUE %in% grepl("^size$",colnames(data))){
    cluster <- TRUE
    cat("OBJECT=CLUSTER;", file=command.file.name, "\n", append=TRUE)
  }else{
    cat("OBJECT=SINGLE;", file=command.file.name, "\n", append=TRUE)
  }
  
  # managing the output options
  if(is.null(control$debug) == FALSE){
    if(control$debug == TRUE){
      cat("DEBUG=TRUE;", file=command.file.name, "\n", append=TRUE)
    }
  }
  
  # !not sure if the output levels match completely
  if(is.null(control$showit) == FALSE){
    output_info_levels <- c("SUMMARY","RESULTS","SELECTION","ALL")
    specified_output_level <- output_info_levels[control$showit+1]
    cat("PRINT=", specified_output_level, ";", file=command.file.name, 
      "\n", append=TRUE)
  }
  
  # the user will specify the adjustment term selection
  cat("SELECTION=SPECIFY;", file=command.file.name, "\n", 
      append=TRUE)
  cat("END;", file=command.file.name, "\n", append=TRUE)
  
  # DATA section
  
  cat("DATA /STRUCTURE=FLAT;", file=command.file.name, "\n", 
      append=TRUE)
  
  # create a vector of fields, renamed to match mcds
  fields <- colnames(data)
  # find which field will be used for the effort and change the name 
  # to match field name in mcds
  if(TRUE %in% grepl("^Effort$",colnames(data))){
    fields[colnames(data)=="Effort"] <- "SMP_EFFORT"
  }else if(TRUE %in% grepl("^Search.time$",colnames(data))){
    # !this may be a bit too specific to the example data in Distance
    fields[colnames(data)=="Search.time"] <- "SMP_EFFORT"
  }else{
    data$EFFORT <- rep(1,nrow(data))
    append(fields,"EFFORT")
  }
  
  # find if Sample.Label is a field; if not, add it
  if(TRUE %in% grepl("^Sample.Label$",colnames(data))){
    fields[colnames(data)=="Sample.Label"] <- "SMP_LABEL"
  }else{
    data$SMP_LABEL <- rep(1,nrow(data))
    append(fields,"SMP_LABEL")
  }
  
  # check if other defined fields are columns in the dataset
  if(TRUE %in% grepl("^Region.Label$",colnames(data))){
    fields[colnames(data)=="Region.Label"] <- "STR_LABEL"
  }
  if(TRUE %in% grepl("^Area$",colnames(data))){
    fields[colnames(data)=="Area"] <- "STR_AREA"
  }
  
  # change all fields to upper case and combine to one string
  fields_comb <- paste(toupper(fields), collapse=",")
  cat("FIELDS=", fields_comb, file=command.file.name, "\n", append=TRUE)
  
  # !how to define a cluster size covariate?
  
  # !deal with factors
  # !how do we deal with numeric factors accurately?
  factor_fields <- c()
  for(i in 1:length(colnames(data))){
    if(is.factor(data[,i])){
      append(factor_fields,fields[i])
    }
  }
  
  cat("INFILE=", data.file.name, "/NOECHO;", file=command.file.name, 
      "\n", append=TRUE)
  cat("END;", file=command.file.name, "\n", append=TRUE)
  
  # ESTIMATE section
  
  cat("ESTIMATE;", file=command.file.name, "\n", append=TRUE)
  
  # we are only interested in the estimates for detection probability
  cat("DETECTION ALL;", file=command.file.name, "\n", append=TRUE)
  
  # a messy way of accessing the model parameters
  mod_paste <- paste(dsmodel)
  mod_vals <- try(eval(parse(text=mod_paste[2:length(modpaste)])))
  
  cat("ESTIMATOR /KEY=", file=command.file.name, append=TRUE)
  if(mod_vals$key == "hn"){
    cat("HNORMAL", file=command.file.name, append=TRUE)
  }else if(mod_vals$key == "hr"){
    cat("HAZARD", file=command.file.name, append=TRUE)
  }else if(mod_vals$key == "unif"){
    cat("UNIFORM", file=command.file.name, append=TRUE)
  }else{
    cat("NEXPON", file=command.file.name, append=TRUE)
  }
  
  if(mod_vals$adj.series == "cos"){
    cat(" /ADJUST=COSINE", file=command.file.name, append=TRUE)
  }else if(mod_vals$adj.series == "herm"){
    cat(" /ADJUST=HERMITE", file=command.file.name, append=TRUE)
  }else if(mod_vals$adj.series == "poly"){
    cat(" /ADJUST=POLY", file=command.file.name, append=TRUE)
  }
  
  cat(" /ORDER=", paste(dsmodel$adj.order,collapse=","), 
      file=command.file.name, append=TRUE)
  
  if(mod_vals$adj.scale == "width"){
    cat(" /ADJSTD=W", file=command.file.name, append=TRUE)
  }else{
    cat(" /ADJSTD=SIGMA", file=command.file.name, append=TRUE)
  }
  
  cat(" /NAP=", length(mod_vals$adj.order), file=command.file.name, 
      append=TRUE)
  
  # allowing for initial values for the parameters
  if(is.null(control$initial) == FALSE){
    cat(" /START=", file=command.file.name, append=TRUE)
    if(mod_vals$key == "hr"){
      cat(control$initial$scale, ",", control$initial$shape, 
          file=command.file.name, append=TRUE)
    }else if(mod_vals$key == "hn"){
      cat(control$initial$scale, file=command.file.name, append=TRUE)
    }
    # !find out how initial values for adjustment parameters are given
    if(mod_vals$adj.order > 0){
      for(i in 1:length(mod_vals$adj.order)){
        cat(",", control$initial$adjustment[i], file=command.file.name, 
            append=TRUE)
      }
    }
  }
  
  # defining upper and lower bounds for parameters
  if(is.null(control$lowerbounds) == FALSE){
    cat(" /LOWER=", paste(control$lowerbounds,collapse=","), 
        file=command.file.name, append=TRUE)
  }
  if(is.null(control$upperbounds) == FALSE){
    cat(" /UPPER=", paste(control$upperbounds,collapse=","), 
        file=command.file.name, append=TRUE)
  }
  
  # specifying covariates in the model
  covars <- all.vars(dsmodel)
  covar_fields <- rep("",length(covars))
  for(i in 1:length(covars)){
    index <- grep(covars[i],colnames(data))
    covar_fields[i] <- toupper(fields[index])
  }
  cat(" /COVARIATES=", paste(covar_fields,collapse=","), 
      file=command.file.name, append=TRUE)
  
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
  
  # dealing with grouped data
  if(is.null(meta.data$binned) == FALSE){
    if(meta.data$binned == TRUE){
      cat("DISTANCE /INTERVALS=", paste(meta.data$breaks, collapse=","), 
         file=command.file.name, append=TRUE)
    }
  }
  
  if(is.null(meta.data$left) == FALSE){
    cat(" /LEFT=", meta.data$left, ";", file=command.file.name, 
       "\n", append=TRUE)
  }
  cat("END;", file=command.file.name, "\n", append=TRUE)
  
  return(command.file.name)
}
