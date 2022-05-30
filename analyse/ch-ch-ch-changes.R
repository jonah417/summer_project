# plot changes in likelihood over time

library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)

format_ll_data <- function(results){

  # make MRDS/MCDS columns:
  results_ll <- results %>%
    filter(Name=="loglik") %>%
    select(Value, name, model, engine, adjstd)

  # recode model specs for plotting
  results_ll$model_desc <- results_ll$model
  results_ll$model_desc[!grepl("(H[AN])-(\\w+)-\\d",
                               results_ll$model_desc)] <- "covar"
  results_ll$model_desc <- sub("(H[AN])-\\w+-0", "\\1",
                               results_ll$model_desc)
  #results_ll$model_desc <- sub("(H[AN])-(\\w+)-\\d", "\\1-\\2",
  #                             results_ll$model_desc)
  # add adjustment scaling info
  results_ll$model_desc[!grepl("covar", results_ll$model_desc)] <-
    paste0(results_ll$model_desc[!grepl("covar", results_ll$model_desc)], "-",
           results_ll$adjstd[!grepl("covar", results_ll$model_desc)])

  results_ll$adjstd <- NULL
  results_ll <- unique(results_ll)

  results_ll <- results_ll %>%
    pivot_wider(id_cols=c("name", "model", "model_desc", "engine"),
                names_from="engine", values_from="Value")
  return(results_ll)
}

# get first run data
runname <- "BFGS"
load(paste0("../testbase/results-", runname, ".RData"))
first <- format_ll_data(results)
first$MRDS1 <- first[[paste0("MRDS-", runname)]]
first[[paste0("MRDS-", runname)]] <- NULL

# second run
runname <- "all-intfail"
load(paste0("../testbase/results-", runname, ".RData"))
second <- format_ll_data(results)
second$MRDS2 <- second[[paste0("MRDS-", runname)]]
second[[paste0("MRDS-", runname)]] <- NULL



#first <- first[grepl("HE", first$model_desc),]
#second <- second[grepl("HE", second$model_desc),]

all <- merge(first, second)
#all <- all[!is.na(all$MRDS1), ]
#all <- all[!is.na(all$MRDS2), ]


# where do bad things happen?
p_ll <- ggplot(all, aes(text=name)) +
  geom_abline(slope=1, intercept=0) +
  geom_point(aes(x=MCDS, y=MRDS1), size=0.5) +
  geom_point(aes(x=MCDS, y=MRDS2), col="red", size=0.5) +
  geom_segment(aes(x=MCDS, y=MRDS1, xend=MCDS, yend=MRDS2), col="grey80") +
  facet_wrap(~model_desc, scale="free") +
  theme_minimal()
p_ll

ggplotly(p_ll)


# image map of differences
esults_im <- all
results_im$diff <- results_im$MCDS-results_im$MRDS2

ggplot(results_im) +
  geom_tile(aes(x=name, y=model_desc, fill=diff)) +
  labs(x="Dataset", y="Model", fill="log-lik\ndifference") +
  scale_fill_gradient2() +
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45))




