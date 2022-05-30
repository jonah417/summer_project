# where are the problems

library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)

# load the base MCDS data
load("../testbase/results-BFGS.RData")
results$engine[results$engine=="MRDS-BFGS"] <- "MRDS"
results_mcds <- subset(results, engine=="MCDS")


# got get all the data we've ever recorded
all_rdata <- list.files("../testbase/", "\\.RData", full.names=TRUE)

res <- lapply(all_rdata, function(fn, results_mcds){

  # get the data
  load(fn)
  # get the MRDS results
  results <- subset(results, engine!="MCDS")
  results$engine <- "MRDS"

  # bind the other results
  results <- rbind(results_mcds, results)

  results$model_desc <- paste0(results$model, results$adjstd)

  results_ll <- results %>%
    filter(Name=="loglik") %>%
    select(Value, name, model_desc, engine)
  results_ll <- unique(results_ll)

  # count NAs, total models
  mcds_models <- nrow(results_mcds)
  mrds_models <- nrow(results) - nrow(results_mcds)
  mrds_NAs <- sum(is.na(subset(results_ll, engine=="MRDS")$Value))
  mcds_NAs <- sum(is.na(subset(results_ll, engine=="MCDS")$Value))


  results_ll <- results_ll %>%
    pivot_wider(id_cols=c("name", "model_desc", "engine"),
                names_from="engine", values_from="Value")


  results_ll$MRDS[is.na(results_ll$MRDS)] <- 0
  results_ll$MCDS[is.na(results_ll$MCDS)] <- 0

  data.frame(time  = file.info(fn)$ctime,
             score = sqrt(sum((results_ll$MCDS-results_ll$MRDS)^2)),
             mcds_models = mcds_models,
             mrds_models = mrds_models,
             mrds_NAs = mrds_NAs,
             mcds_NAs = mcds_NAs)

}, results_mcds=results_mcds)


results <- do.call(rbind.data.frame, res)
results <- results[order(results$time), ]

results$NAprop <- results$mrds_NAs/results$mrds_models

par(mfrow=c(1,2))
plot(results[,c("time", "score")], type="l", main="total RMSE")
plot(results[,c("time", "NAprop")], type="l", main="NA proportion")

