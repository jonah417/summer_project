mrds_runone <- function(this_spec, dat, covariates=NULL, debug=0, start=NULL, refit=TRUE, method=NULL){

  # add object number
  dat$object <- 1:nrow(dat)

  # detection function options object
  detfunc.options <- this_spec
  if(is.na(detfunc.options$truncation)){
    detfunc.options$truncation <- max(dat$distance)
  }


  # need to fix this!!
  detfunc.options$covariates <- ""
  detfunc.options$intervals <- this_spec$cutpoints

  detfunc.options$key <- switch(detfunc.options$key,
                                HN = "hn",
                                HA = "hr",
                                UN = "unif")

  modform <- paste0("~mcds(key = \"", detfunc.options$key, "\"")
  # NAP is number of adjustment parameters if zero then no
  # adjustments are to be fitted
  if(detfunc.options$nap > 0){
    adj <- adj_order(detfunc.options$key, detfunc.options$adj,
                     detfunc.options$nap)

    adjparnames <- paste0(detfunc.options$adj, "(", adj, ")")
    detfunc.options$adj <- switch(detfunc.options$adj,
                                  CO = "cos",
                                  HE = "herm",
                                  PO = "poly",
                                  NULL)
    detfunc.options$adjstd <- switch(detfunc.options$adjstd,
                                     W = "width",
                                     SIGMA = "scale")
    modform <- paste0(modform, ",",
                      "adj.series = \"", detfunc.options$adj, "\",",
                      "adj.order  = c(", paste(adj, collapse=","), "),",
                      "adj.scale  = \"", detfunc.options$adjstd, "\"")
  }

  # add in the formula
  if(is.null(covariates)){
    modform <- paste0(modform, ", formula = ~1)")
  }else{
    modform <- paste0(modform, ", formula = ~",
                      paste(covariates, collapse="+"), ")")
  }


  # setup meta.data
  meta.data <- list(width = detfunc.options$truncation,
                    point = detfunc.options$transect=="POINT")
  if(detfunc.options$cutpoints!=""){
    meta.data$breaks <- unlist(strsplit(detfunc.options$cutpoints, ","))
    meta.data$binned <- TRUE
  }

  ctrl <- list(showit=debug, refit=refit, optimx.method=method)
  if(!is.null(start)){
    ctrl$initial <- start
  }
  # fit that model
  result <- try(ddf(dsmodel = as.formula(modform),
                data = dat, method = "ds", meta.data=meta.data,
                control=ctrl))


  return(result)
}
