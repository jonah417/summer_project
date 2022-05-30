get_mcds_results <- function(this_spec, dat, covariates=NULL){

  # write out the data file
  dist_data_filename <- tempfile(pattern="dattmp", tmpdir="tmp_files",
                                 fileext=".tsv")
  write.table(dat, file=dist_data_filename, col.names=FALSE, row.names=FALSE,
              quote=FALSE, sep="\t")

  # detection function options object
  detfunc.options <- this_spec
  if(is.na(detfunc.options$truncation)){
    detfunc.options$truncation <- max(dat$distance)
  }


  # setup order of adjustments
  if(detfunc.options$nap > 0){
    detfunc.options$order <- paste(adj_order(detfunc.options$key,
                                             detfunc.options$adj,
                                             detfunc.options$nap), collapse=",")
  }

  # handle covariates
  if(!is.null(covariates)){
    factors <- lapply(covariates, function(x){
      if(grepl("as.factor", x)){
        x <- sub("as.factor\\((.+)\\)", "\\1", x)
        return(paste0("FACTOR /NAME=", x,
                      " /LEVELS=", length(levels(dat[[x]])),
                      " /LABELS=", paste(levels(dat[[x]]), collapse=","),
                      ";\n"))
      }else{
        return("")
      }
    })
    detfunc.options$factors <- paste(unlist(factors), collapse="")
    detfunc.options$fieldnames <- paste(sub("as.factor\\((.+)\\)", "\\1",
                                            covariates), collapse=",")
  }else{
    detfunc.options$fieldnames <- ""
    detfunc.options$factors <- ""
  }

  detfunc.options$intervals <- this_spec$cutpoints


  # generate the MCDS command file and return the file name
  command_filename <- create.mcds.command.file(detfunc.options, dist_data_filename)

  # run MCDS.exe using wine (on windows you can probably just remove wine at the
  #  start of the command
  wine_call <- paste0("wine32on64 MCDS.exe 0, ", command_filename)
  call_status <- system(wine_call, intern=TRUE,
                        ignore.stdout=TRUE, ignore.stderr=TRUE)

  # okay files have been generated so we can delete the files
  unlink(command_filename)
  unlink(dist_data_filename)

  # now grab the stats file
  if(length(readLines("stat.txt")) > 1){
    stats <- read.table("stat.txt", row.names=NULL)
    colnames(stats) <- c("Stratum", "Sample", "Estimator", "Module", "Statistic",
                         "Value", "CV", "Lcl", "Ucl", "Df")

    # based on the cuniform tablets, we need
    # module 2
    # statistics:
    #   9 -- log likelihood
    #   101+ -- parameter estimates ("Statistic 101 corresponds with the parameter identified as A(1) in the results, 102 with A(2), etc.")

    stats <- subset(stats, Module==2)
    stats <- subset(stats, Statistic==9 | Statistic>100 | Statistic==5)
    stats$Name <- ""
    stats$Name[stats$Statistic==9] <- "loglik"
    stats$Name[stats$Statistic==5] <- "p"
    stats$Name[stats$Statistic>100] <- paste0("A(",
                                         stats$Statistic[stats$Statistic>100]-100,
                                         ")")
    stats <- stats[, c("Name", "Value", "CV")]

    # parse variable names/meanings
    if(!is.null(covariates)){
      parnames <- readLines("out.txt")
      parnames <- unique(parnames[grepl("Parameter A\\(", parnames)])
      parnames <- sub(" is the intercept of the scale parameter s.",
                      "\tscale_intercept", parnames)
      parnames <- sub(" is the power parameter.", "\tshape", parnames)
      parnames <- sub(".+Parameter ", "", parnames)
      parnames <- sub("is the coefficient of ", "", parnames)
      parnames <- sub(" level (.+) of factor covariate (.+)\\.",
                      "\t\\2=\\1", parnames)
      parnames <- sub(" covariate (.+)\\.", "\t\\1", parnames)
      parnames <- t(as.data.frame(strsplit(parnames,"\t")))
      rownames(parnames) <- NULL
      colnames(parnames) <- c("Name", "newName")
      stats <- merge(stats, parnames, all.x=TRUE)
      stats$newName[stats$Name=="loglik"] <- "loglik"
      stats$newName[stats$Name=="p"] <- "p"
      stats$Name <- stats$newName
      stats$newName <- NULL

    }else{
      parnames <- c()
      if(detfunc.options$key=="HA"){
        parnames <- c(parnames, "shape")
      }
      if(detfunc.options$key %in% c("HN", "HA")){
        parnames <- c(parnames, "scale")
      }
      if(detfunc.options$nap > 0){
        parnames <- c(parnames, paste0(detfunc.options$adj,
                                       "(", unlist(strsplit(detfunc.options$order,
                                                            ",")), ")"))
      }
      stats <- stats[order(stats$Name), ]
      stats$Name[!(stats$Name %in% c("loglik", "p"))] <- parnames

      if(detfunc.options$nap == 0){
        stats$Value[stats$Name=="scale"] <- log(stats$Value[stats$Name=="scale"])
      }
    }
  }else{
    # if there was an error, no stats written, return NA ll
    stats <- data.frame(Name="loglik", Value=NA, CV=NA)
  }
  unlink("boot.txt")
  unlink("bootprog.txt")
  unlink("stat.txt")
  unlink("plot.txt")
  unlink("out.txt")
  unlink("log.txt")

  return(unique(stats))
}
