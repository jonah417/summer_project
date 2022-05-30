# look at what the testing database tells us


library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)

# one dataset for MCDS analyses, since we try not to run them multiple times
# (since they don't change)
# load("../testbase/results-orig.rdata")
load("../testbase/results-BFGS.RData")
results_orig <- results
results_orig <- subset(results_orig, engine=="MCDS")
# "new" data with mrds results
newname <- "integrate-like-distance"
load(paste0("../testbase/results-", newname, ".RData"))
results_new <- results
results_new$engine <- "MRDS"
results <- rbind(results_orig, results_new)

## "new" data with all results
#load("../testbase/results-BFGS.RData")
#results$engine[results$engine=="MRDS-BFGS"] <- "MRDS"


# what if we just plot the likelihoods

# make MRDS/MCDS columns:
results_ll <- results %>%
  filter(Name=="loglik") %>%
  select(Value, name, model, engine, adjstd)

# recode model specs for plotting
results_ll$model_desc <- results_ll$model

# select only covar models
#results_ll <- results_ll[!grepl("(H[AN])-(\\w+)-\\d",
#                               results_ll$model_desc), ]


results_ll$model_desc[!grepl("(H[AN])-(\\w+)-\\d",
                             results_ll$model_desc)] <- "covar"
results_ll$model_desc <- sub("(H[AN])-\\w+-0", "\\1",
                             results_ll$model_desc)

# add adjustment scaling info
results_ll$model_desc[!grepl("covar", results_ll$model_desc)] <-
  paste0(results_ll$model_desc[!grepl("covar", results_ll$model_desc)], "-",
         results_ll$adjstd[!grepl("covar", results_ll$model_desc)])

results_ll$adjstd <- NULL
results_ll <- unique(results_ll)

results_ll <- results_ll %>%
  pivot_wider(id_cols=c("name", "model", "model_desc", "engine"),
              names_from="engine", values_from="Value")

# where do bad things happen?
p_ll <- ggplot(results_ll, aes(text=name)) +
  geom_abline(slope=1, intercept=0) +
  geom_point(aes(x=MCDS, y=MRDS)) +
  facet_wrap(~model_desc, scale="free") +
  theme_minimal()

ggsave(p_ll, file=paste0("loglik-", newname, ".pdf"), width=12, height=9)


ggplotly(p_ll)



# now for parameters

# make MRDS/MCDS columns:
results_pars <- results %>%
  filter(Name!="loglik" & Name!="p") %>%
  select(Value, name, model, engine, adjstd, Name)

# recode model specs for plotting
results_pars$model_desc <- results_pars$model
results_pars$model_desc[!grepl("(H[AN])-(\\w+)-\\d",
                             results_pars$model_desc)] <- "covar"
results_pars$model_desc <- sub("(H[AN])-(\\w+)-\\d", "\\1-\\2",
                             results_pars$model_desc)
# add adjustment scaling info
results_pars$model_desc[!grepl("covar", results_pars$model_desc)] <-
  paste0(results_pars$model_desc[!grepl("covar", results_pars$model_desc)], "-",
         results_pars$adjstd[!grepl("covar", results_pars$model_desc)])

results_pars$adjstd <- NULL
results_pars <- unique(results_pars)
results_pars <- results_pars %>%
  pivot_wider(id_cols=c("name", "model", "model_desc", "engine", "Name"),
              names_from="engine", values_from="Value")

# where do bad things happen?
pp_pars <- ggplot(results_pars, aes(text=name)) +
  geom_abline(slope=1, intercept=0, colour="grey60") +
  geom_point(aes(x=MCDS, y=MRDS)) +
  facet_wrap(~Name, scale="free") +
  theme_minimal()

ggsave(pp_pars, file=paste0("pars-", newname, ".pdf"), width=12, height=9)
ggplotly(pp_pars)

# where did things go wrong?

results_bad <- results %>%
  filter(Name=="loglik" & is.na(Value)) %>%
  select(name, model, adjstd, engine)



# image map of differences
results_im <- results_ll
results_im$diff <- results_im$MCDS-results_im$MRDS

ll_im <- ggplot(results_im) +
  geom_tile(aes(x=name, y=model_desc, fill=diff)) +
  labs(x="Dataset", y="Model", fill="log-lik\ndifference") +
  scale_fill_gradient2() +
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45))
ll_im

ggsave(ll_im, file=paste0("ll_image-", newname, ".pdf"), width=12, height=12)





# plots for average probability of detection
results_p <- results %>%
  filter(Name=="p") %>%
  select(Value, name, model, engine, adjstd)

# recode model specs for plotting
results_p$model_desc <- results_p$model
results_p$model_desc[!grepl("(H[AN])-(\\w+)-\\d",
                             results_p$model_desc)] <- "covar"
results_p$model_desc <- sub("(H[AN])-\\w+-0", "\\1",
                             results_p$model_desc)
#results_p$model_desc <- sub("(H[AN])-(\\w+)-\\d", "\\1-\\2",
#                             results_p$model_desc)

# drop covar models for now
results_p <- subset(results_p, model_desc!="covar")

# add adjustment scaling info
results_p$model_desc[!grepl("covar", results_p$model_desc)] <-
  paste0(results_p$model_desc[!grepl("covar", results_p$model_desc)], "-",
         results_p$adjstd[!grepl("covar", results_p$model_desc)])

results_p$adjstd <- NULL
results_p <- unique(results_p)

results_p <- results_p %>%
  pivot_wider(id_cols=c("name", "model", "model_desc", "engine"),
              names_from="engine", values_from="Value")

# where do bad things happen?
p_p <- ggplot(results_p, aes(text=name)) +
  geom_abline(slope=1, intercept=0) +
  geom_point(aes(x=MCDS, y=MRDS)) +
  facet_wrap(~model_desc, scale="free") +
  theme_minimal()

ggplotly(p_p)

#ggsave(p_p, file="p.pdf", width=12, height=9)



# image map of differences in p
results_imp <- results_p
results_imp$diff <- results_imp$MCDS-results_imp$MRDS

# there are some really weird values here from MCDS
results_imp <- subset(results_imp, is.na(diff) | abs(diff) < 2)

ll_imp <- ggplot(results_imp) +
  geom_tile(aes(x=name, y=model, fill=diff)) +
  labs(x="Dataset", y="Model", fill="p difference") +
  scale_fill_gradient2() +
  theme_minimal()+
  theme(axis.text.x=element_text(angle=90))
ggplotly(ll_imp)

ggsave(ll_imp, file=paste0("ll_impage-", newname,".pdf"), width=14, height=12)
