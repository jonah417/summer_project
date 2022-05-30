# based on code from trendest6.r
# based on code written by Len Thomas

create.mcds.command.file <- function(detfunc.options, data.file.name){
#Purpose: Creates a command file for MCDS.exe
# Currently just does an analysis of the detection function
#Inputs:
# detfunc.options a list:
#   key - vector of key functions
#   adj - vector of adjustment terms
#   covars - character vector of covariates (must be the same for all analyses)
#   w - truncation distance
#   cutpoints - vector of truncation cutpoints

  # windows-ise the path
  data.file.name <- sub("/", "\\\\", data.file.name)

  command.file.name <- tempfile(pattern="cmdtmp", tmpdir="tmp_files",
                                fileext=".txt")

  #create the command file
  file.create(command.file.name)

  #output commands to it
  cat("out.txt", file=command.file.name, "\n", append=TRUE)
  cat("log.txt", file=command.file.name, "\n", append=TRUE)
  cat("stat.txt", file=command.file.name, "\n", append=TRUE)
  cat("plot.txt", file=command.file.name, "\n", append=TRUE)
  cat("None", file=command.file.name, "\n", append=TRUE)
  cat("None", file=command.file.name, "\n", append=TRUE)
  cat("OPTIONS;", file=command.file.name, "\n", append=TRUE)
  cat("TYPE=", detfunc.options$transect, ";",
      file=command.file.name, "\n", append=TRUE)

  if(detfunc.options$transect=="POINT"){
    cat("DISTANCE=RADIAL /UNITS='Meters' /WIDTH=", detfunc.options$truncation,
        ";", file=command.file.name, "\n", append=TRUE)
  }else{
    cat("DISTANCE=PERP /UNITS='Meters' /WIDTH=", detfunc.options$truncation,
        ";", file=command.file.name, "\n", append=TRUE)
  }

  # if size is a covariate, need to tell MCDS that we have clusters, idk
  if(grepl("(^|,)size($|,)", detfunc.options$fieldnames)){
    cat("OBJECT=CLUSTER;", file=command.file.name, "\n", append=TRUE)
  }else{
    cat("OBJECT=SINGLE;", file=command.file.name, "\n", append=TRUE)
  }

  cat("END;", file=command.file.name, "\n", append=TRUE)
  cat("DATA /STRUCTURE=FLAT;", file=command.file.name, "\n", append=TRUE)

  if(grepl("(^|,)size($|,)", detfunc.options$fieldnames)){
    sizename <- sub("size", "r\\1", detfunc.options$fieldnames)
    detfunc.options$fieldnames <- sub("size", "r\\1",
                                      detfunc.options$fieldnames)
  }

  cat("FIELDS=STR_LABEL, STR_AREA, SMP_LABEL, SMP_EFFORT, DISTANCE",
    ifelse(detfunc.options$fieldnames=="",
           ";",
           paste(", ",detfunc.options$fieldnames,";",sep="")),
    sep="", file=command.file.name, "\n", append=TRUE)

  if(grepl("(^|,)size($|,)", detfunc.options$fieldnames)){
    cat("SIZEC=", sizename, ";", file=command.file.name, "\n", append=TRUE)
  }

  # deal with factors
  if(detfunc.options$factors != ""){
     cat(detfunc.options$factors, file=command.file.name, sep="", append=TRUE)
  }

  cat("INFILE=", data.file.name, "/NOECHO;", file=command.file.name, "\n",
      append=TRUE)
  cat("END;", file=command.file.name, "\n", append=TRUE)
  cat("ESTIMATE;", file=command.file.name, "\n", append=TRUE)

  if(detfunc.options$cutpoints != ""){
    cat("DISTANCE",  "/INTERVALS=",
        paste(detfunc.options$intervals,collapse=","), ";",
        file=command.file.name, "\n", append=TRUE)
  }

  cat("DETECTION=ALL;", file=command.file.name, "\n", append=TRUE)
  cat("DETECTION=STRATUM;", file=command.file.name, "\n", append=TRUE)

  # what is the form of the detection function?
  if(detfunc.options$nap!=0){
    # if we specify a number of adjustment terms
    cat("ESTIMATOR",  "/KEY=", detfunc.options$key,
                      "/ADJUST=", detfunc.options$adj,
        " /NAP=", detfunc.options$nap,
        " /ADJSTD=", detfunc.options$adjstd, " SELECT=SPECIFY;",
        file=command.file.name, "\n", append=TRUE)
  }else{
    # no adjustments, we're fitting a covariate model or a key only model
    cat("ESTIMATOR",  "/KEY=", detfunc.options$key ,
                      "/ADJUST=", detfunc.options$adj,
        ifelse(detfunc.options$fieldnames=="",
               # key only
               paste0(" /NAP=", detfunc.options$nap,
                      " /ADJSTD=", detfunc.options$adjstd, " SELECT=SPECIFY"),
               # covariate
               paste0("/COVARIATES=", detfunc.options$fieldnames,
                     " /NAP=0",
                     " /ADJSTD=", detfunc.options$adjstd, " SELECT=SPECIFY")),
        ";",
        file=command.file.name, "\n", append=TRUE)
  }

  cat("MONOTONE=NONE;", file=command.file.name, "\n", append=TRUE)
  cat("END;", file=command.file.name, "\n", append=TRUE)

  return(command.file.name)
}
