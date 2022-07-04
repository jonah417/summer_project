# assuming the inputs are in the same form as those used in ddf.R

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
  if(is.null(control$limit) == FALSE){
    if(control$limit){
      data <- data[data$detected==1,]
    }
  }
  
  # OPTION section
  
  # !consider the case where TYPE="LINE" but DISTANCE="RADIAL"?
  if(meta.data$point == TRUE){
    cat(paste("DISTANCE=RADIAL /UNITS='Meters' /WIDTH=", 
        meta.data$width, sep=""), ";", file=command.file.name, "\n", 
        append=TRUE)
  }else{
    cat(paste("DISTANCE=PERP /UNITS='Meters' /WIDTH=", 
        meta.data$width, sep=""), ";", file=command.file.name, "\n", 
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
    cat(paste("PRINT=", specified_output_level, sep=""), ";", 
        file=command.file.name, "\n", append=TRUE)
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
    fields <- append(fields,"EFFORT")
  }
  
  # find if Sample.Label is a field; if not, add it
  if(TRUE %in% grepl("^Sample.Label$",colnames(data))){
    fields[colnames(data)=="Sample.Label"] <- "SMP_LABEL"
  }else{
    data$SMP_LABEL <- rep(1,nrow(data))
    fields <- append(fields,"SMP_LABEL")
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
  cat(paste("FIELDS=", fields_comb, sep=""), file=command.file.name, 
      "\n", append=TRUE)
  
  # specifying which fields are factors
  factor_fields <- c()
  for(i in 1:length(colnames(data))){
    if(is.factor(data[,i])){
      factor_fields <- append(factor_fields,fields[i])
      labels <- paste(levels(data[,i]), collapse=",")
      cat(paste("FACTOR /NAME=", toupper(fields[i]), " /LEVELS=", 
          length(levels(data[,i])), " /LABELS=", labels, sep=""), 
          file=command.file.name, "\n", append=TRUE)
    }
  }
  
  cat(paste("INFILE=", data.file.name, "/NOECHO;", sep=""), 
      file=command.file.name, "\n", append=TRUE)
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
  
  cat(" /ORDER=", mod_vals$adj.order, 
      file=command.file.name, append=TRUE)
  
  if(mod_vals$adj.scale == "width"){
    cat(" /ADJSTD=W", file=command.file.name, append=TRUE)
  }else{
    cat(" /ADJSTD=SIGMA", file=command.file.name, append=TRUE)
  }
  
  cat(paste(" /NAP=", length(mod_vals$adj.order), sep=""), 
      file=command.file.name, append=TRUE)
  
  # specifying covariates in the model
  covars <- all.vars(dsmodel)
  covar_fields <- rep("",length(covars))
  for(i in 1:length(covars)){
    index <- grep(covars[i],colnames(data))
    covar_fields[i] <- toupper(fields[index])
  }
  cat(paste(" /COVARIATES=", paste(covar_fields,collapse=","), sep=""), 
      file=command.file.name, append=TRUE)
  
  # allowing for initial values for the parameters
  inits <- c()
  if(is.null(control$initial) == FALSE){
    # go through covariates in order
    for(i in 1:length(covars)){
      index <- grep(covar_fields[i],toupper(fields))
      if(TRUE %in% grepl(covar_fields[i],factor_fields)){
        for(j in 2:length(levels(data[,index]))){
          access_covar <- paste("control$initial$scale$",
                                colnames(data)[index],"[",j,"]",sep="")
          inits <- append(inits,eval(parse(text=access_covar)))
        }
        access_covar <- paste("control$initial$scale$",
                              colnames(data)[index],"[1]",sep="")
        inits <- append(inits,eval(parse(text=access_covar)))
      }else{
        access_covar <- paste("control$initial$scale$",
                              colnames(data)[index],sep="")
        inits <- append(inits,eval(parse(text=access_covar)))
      }
    }
    # add in shape parameter if hazard-rate used
    if(mod_vals$key == "hr"){
      inits <- append(inits,control$initial$shape)
    }
    # add in adjustment initial values
    for(i in 1:length(mod_vals$adj.order)){
      inits <- append(inits,control$initial$adjustment[i])
    }
    print(inits)
    cat(paste(" /START=", paste(inits,collapse=","), sep=""), 
        file=command.file.name, append=TRUE)
  }
  
  # defining upper and lower bounds for parameters
  if(is.null(control$lowerbounds) == FALSE){
    cat(paste(" /LOWER=", paste(control$lowerbounds,collapse=","), sep=""), 
        file=command.file.name, append=TRUE)
  }
  if(is.null(control$upperbounds) == FALSE){
    cat(paste(" /UPPER=", paste(control$upperbounds,collapse=","), sep=""), 
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
  
  # dealing with grouped data
  if(is.null(meta.data$binned) == FALSE){
    if(meta.data$binned == TRUE){
      cat("DISTANCE /INTERVALS=", paste(meta.data$breaks, collapse=","), 
         file=command.file.name, append=TRUE)
    }
  }
  
  if(is.null(meta.data$left) == FALSE){
    cat(paste(" /LEFT=", meta.data$left, sep=""), ";", 
        file=command.file.name, "\n", append=TRUE)
  }
  cat("END;", file=command.file.name, "\n", append=TRUE)
  
  return(command.file.name)
}
