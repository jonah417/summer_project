# construct the data

#library(mrds)
devtools::load_all("~/current/mrds")

library(Hmisc)
library(stringr)
library(readdst)

# based on files from from LJT
source("create.mcds.command.file.r")
source("get_data.R")
source("get_mcds_results.R")
source("get_mrds_results.R")
source("adj_order.R")

# get all the datasets and their settings
dsts <- read.csv("data_sources.csv")

# options to fiddle with!
# what to append to engine name and output filename
runname <- "integrate-like-distance"
# what should the "engine" title be?
mrds_engine_name <- paste0("MRDS-", runname)
# should we re-run MCDS.exe models (no we have them in original results file)
run_MCDS.exe <- FALSE
# make a subset of the datasets if we only want to investigate certain things?
#dsts <- subset(dsts, formula=="")

# get all key+adjustment model combinations
# note we specify the number of adjustments here, not their order
ka_model_combs <- expand.grid(key = c("HN", "HA"),
                              adj = c("CO", "HE", "PO"),
                              nap = 1:3,
                              adjstd = c("W", "SIGMA"),
                              stringsAsFactors=FALSE)
# add in the no adjustment versions
ka_model_combs <- rbind(ka_model_combs,
                        c("HN", "CO", 0, "W"),
                        c("HA", "CO", 0, "W"))

#ka_model_combs <- ka_model_combs[-1,]


results <- c()
# get the current set of specifications
for(i in 1:nrow(dsts)){
  this_spec <- dsts[i, ]

  this_dat <- get_data(this_spec)

  # loop over k+a models
  for(j in 1:nrow(ka_model_combs)){

    # setup the model
    this_ka_model <- ka_model_combs[j, ]
    this_spec$key <- this_ka_model$key
    this_spec$adj <- this_ka_model$adj
    this_spec$nap <- this_ka_model$nap
    this_spec$adjstd <- this_ka_model$adjstd

    # run the MCDS model for this model spec
    if(run_MCDS.exe){
      stats <- get_mcds_results(this_spec, this_dat)
      # bind the spec onto the results
      rownames(stats) <- rownames(this_spec) <- NULL
      stats <- cbind(stats, this_spec)
      stats$engine <- "MCDS"
      stats$model <- paste(this_spec$key, this_spec$adj, this_spec$nap, sep="-")
      stats$formula <- stats$filename <- stats$project_number <- NULL
      # bind the results onto the others
      results <- rbind(results, stats)
    }

    # run mrds model
    stats <- get_mrds_results(this_spec, this_dat)
    # bind the spec onto the results
    stats <- cbind(stats, this_spec)
    stats$engine <- mrds_engine_name
    stats$model <- paste(this_spec$key, this_spec$adj, this_spec$nap, sep="-")
    stats$formula <- stats$filename <- stats$project_number <- NULL
    # bind the results onto the others
    results <- rbind(results, stats)
  }

  # if there is a covariate spec, deal with that
  if(this_spec$formula != ""){
    covars <- strsplit(this_spec$formula, "\\+")[[1]]
    covar_combs <- lapply(1:length(covars), combn, x=covars)
    # format that
    covar_combs <- lapply(covar_combs, function(x){
      apply(x, 2, list)
    })
    covar_combs <- unlist(unlist(covar_combs, recursive=FALSE), recursive=FALSE)

    # same for covariate combinations
    # now build the model combinations
    model_combs <- expand.grid(key = c("HN", "HA"),
                               adj = c("CO"),
                               nap = 0,
                               adjstd = "W",
                               stringsAsFactors=FALSE)


    # go through all those combinations, fitting models
    for(covar_comb_i in seq_along(covar_combs)){
      # get the data with covariates included
      this_dat <- get_data(this_spec, covar_combs[[covar_comb_i]])

      for(j in 1:nrow(model_combs)){
        # setup the model
        this_model <- model_combs[j, ]
        this_spec$key <- this_model$key
        this_spec$adj <- this_model$adj
        this_spec$nap <- this_model$nap
        this_spec$adjstd <- this_model$adjstd

        # run the MCDS model for this model spec
        if(run_MCDS.exe){
          stats <- get_mcds_results(this_spec, this_dat,
                                    covar_combs[[covar_comb_i]])
          # bind the spec onto the results
          rownames(stats) <- rownames(this_spec) <- NULL
          stats <- cbind(stats, this_spec)
          stats$engine <- "MCDS"
          stats$model <- paste(this_spec$key,
                               paste(covar_combs[[covar_comb_i]], collapse="+"),
                               sep="-")
          stats$formula <- stats$filename <- stats$project_number <- NULL
          # bind the results onto the others
          results <- rbind(results, stats)
        }

        # run mrds model
        stats <- get_mrds_results(this_spec, this_dat,
                                  covar_combs[[covar_comb_i]])
        # bind the spec onto the results
        stats <- cbind(stats, this_spec)
        stats$engine <- mrds_engine_name
        stats$model <- paste(this_spec$key,
                             paste(covar_combs[[covar_comb_i]], collapse="+"),
                             sep="-")
        stats$formula <- stats$filename <- stats$project_number <- NULL
        # bind the results onto the others
        results <- rbind(results, stats)

      } # end covar model combinations
    } # end covar covariate combinations
  } # end covariate conditional


} # end loop over datasets


save(results, file=paste0("results-", runname, ".RData"))



