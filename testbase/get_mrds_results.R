get_mrds_results <- function(this_spec, dat, covariates=NULL){

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
  control <- list(optimx.method="nlminb")

  # setup meta.data
  meta.data <- list(width = detfunc.options$truncation,
                    point = detfunc.options$transect=="POINT")
  if(detfunc.options$cutpoints!=""){
    meta.data$breaks <- as.numeric(unlist(strsplit(detfunc.options$cutpoints, ",")))
    meta.data$binned <- TRUE
    dat <- Distance::create.bins(dat, meta.data$breaks)
  }
  # fit that model
  result <- try(ddf(dsmodel = as.formula(modform),
                    data = dat, method = "ds",
                    meta.data=meta.data, control=control))

  if(any(class(result) == "try-error")){
    return(data.frame(Name  = "loglik",
                      Value = NA,
                      CV    = NA))
  }


  # extract lnl
  stats <- data.frame(Name  = "loglik",
                      Value = result$lnl,
                      CV    = NA)

  # try to get pars, things can still fail here
  # because vcov is not invertable
  summary_result <- tryCatch(summary(result), error=function(x) NA)
  if(all(is.na(summary_result))){
    return(data.frame(Name  = "loglik",
                      Value = NA,
                      CV    = NA))
  }
  pars <- summary_result$coeff

  # add in average p
  if(length(summary_result$average.p)==0){
    # if we got numeric(0) then something went wrong!
    return(data.frame(Name  = "loglik",
                      Value = NA,
                      CV    = NA))
  }
  if(is.matrix(summary_result$average.p.se)){
    summary_result$average.p.se <- summary_result$average.p.se[1,1]
  }
  stats <- rbind(stats, data.frame(Name  = "p",
                                   Value = summary_result$average.p,
                                   CV    = summary_result$average.p.se/
                                           summary_result$average.p))

  # concatenate this all together
  pars <- rbind(pars$key.shape, pars$key.scale, pars$adj.parm)
  rownames(pars) <- NULL
  pars$Value <- pars$estimate
  pars$CV <- pars$se/pars$estimate
  parnames <- c()
  if(detfunc.options$key=="hr"){
    parnames <- c(parnames, "shape")
  }
  if(detfunc.options$key %in% c("hn", "hr")){
    parnames <- c(parnames, "scale")
    if(!is.null(covariates)){
      covnames <- rownames(summary(result)$coeff$key.scale)[-1]
      covnames <- sub("as.factor\\((.+)\\)(.+)", "\\1=\\2", covnames)
      covnames <- toupper(covnames)
      parnames <- c(parnames, covnames)
    }
  }
  if(detfunc.options$nap > 0){
    parnames <- c(parnames, adjparnames)
  }
  pars$Name <- parnames


  pars <- rbind(stats, pars[, c("Name", "Value", "CV")])

  return(pars)
}
