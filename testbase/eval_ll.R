# evaluate the log-likelihood at a point
eval_ll <- function(fit, pars){

  # build misc options
  misc.options <- list(point=fit$meta.data$point,
                       int.range=fit$meta.data$int.range,
                       showit=fit$control$showit,
                       integral.numeric=fit$control$integral.numeric,
                       breaks=NULL,
                       maxiter=fit$control$maxiter,
                       refit=fit$control$refit,
                       nrefits=fit$control$nrefits,
                       parscale=fit$control$parscale,
                       mono=fit$meta.data$mono,
                       mono.strict=fit$meta.data$mono.strict,
                       binned=fit$meta.data$binned,
                       width=fit$meta.data$width,
                       standardize=fit$control$standardize,
                       debug=fit$control$debug,
                       nofit=fit$control$nofit,
                       left=fit$meta.data$left,
                       silent=fit$control$silent
                      )


  return(-1*flnl(fpar=pars, ddfobj=fit$ds$aux$ddfobj, misc.options=misc.options,
                 fitting="all"))
}
