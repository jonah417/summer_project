get_data <- function(this_spec, covariates=NULL){

  # setup the variables
  fn <- this_spec$filename

  # what to do if we have a Distance project file
  if(grepl("^dst_files", fn)){
    project_number <- this_spec$project_number

    this_spec$filename <- this_spec$project_number <- NULL

    # get the data from the project file
    cc <- convert_project(fn)
    # this is already a flatfile format, which Distance wants
    if(!is.na(project_number)){
      dat <- cc[[project_number]]$env$data
    }else{
      # if there are no analyses in this project just grab the flat data
      dat <- attr(cc, "flatfile")
    }

  }else if(grepl("^RData_files", fn)){
    load(fn)
  }else{
    stop(paste0("Don't know what to do with: ", fn))
  }

  # do subsetting if necessary
  if(this_spec$subset!=""){
    dat <- eval(parse(text = paste0("subset(dat, dat$", this_spec$subset, ")")))
  }

  # grab the columns we need for MCDS.exe to work
  # these get ignored so set them to whatever
  if(is.null(dat$Area)) dat$Area <- 0
  if(is.null(dat$Effort)) dat$Effort <- 1
  if(is.null(dat$Region.Label)) dat$Region.Label <- 1
  if(is.null(dat$Sample.Label)) dat$Sample.Label <- 1

  # handle covariates
  factors <- which(grepl("as.factor\\((.+)\\)", covariates))
  covariates <- sub("as.factor\\((.+)\\)", "\\1", covariates)
  for(i in seq_along(factors)){
    dat[[covariates[[i]]]] <- as.factor(dat[[covariates[[i]]]])
  }

  # ordering as in trendest6.r files
  dat <- dat[, c("Region.Label", "Area", "Sample.Label",
                 "Effort", "distance", covariates)]

  # remove NA distances
  dat <- dat[!is.na(dat$distance), ]


  return(dat)
}
