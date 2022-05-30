# what do results look like if we get the best model by AIC


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
newname <- "all-intfail"
load(paste0("../testbase/results-", newname, ".RData"))
results_new <- results
results_new$engine <- "MRDS"
results <- rbind(results_orig, results_new)


# what if we just plot the likelihoods

# make MRDS/MCDS columns:
results_ll <- results %>%
  filter(Name=="loglik") %>%
  select(Value, name, model, engine, adjstd) %>%
  # recode model specs for plotting
  mutate(model_desc = model) %>%
  # get only non-covariate models
  filter(grepl("(H[AN])-(\\w+)-\\d", model_desc)) %>%
  # add adjustment scaling info
  mutate(model_desc = paste0(model_desc, "-", adjstd)) %>%
  # generate model class and number of parameters
  mutate(model_class = sub("(\\w+-\\w+-)\\d+-(\\w+)", "\\1\\2", model_desc),
         npars       = as.numeric(sub("\\w+-\\w+-(\\d+)-\\w+", "\\1",
                                  model_desc)) +
                       c(1,2)[grepl("^HA", model_desc)+1],
         model_desc  = sub("(H[AN])-\\w+-0", "\\1", model_desc)) %>%
  # calculate AIC
  mutate(AIC = -2*Value + 2*npars) %>%
  select(-adjstd) %>%
  distinct()

# now find the best models
results_aic <- results_ll %>%
  group_by(model_class, name, engine) %>%
  slice(which.min(AIC)) %>%
  ungroup() %>%
  select(name, model_class, model_desc, engine, AIC)

# make columns for model name for each engine and another for their AICs
results_aic <- results_aic %>%
  pivot_wider(id_cols=c("name", "model_class", "engine"),
              names_from=c("engine", "engine"),
              names_prefix=c("", "AIC_"),
              values_from=c("model_desc", "AIC"))

# I'm bad at dplyr, so rename the columns manually
names(results_aic) <- c("name", "model_class", "MCDS", "MRDS",
                        "AIC_MCDS", "AIC_MRDS")
# difference in AIC
results_aic$delta_AIC <- abs(results_aic$AIC_MCDS - results_aic$AIC_MRDS)
# only get the models where there is a difference in model form and sort
# by AIC diff, largest first
results_aic <- results_aic %>%
  filter(MCDS!=MRDS) %>%
  arrange(desc(delta_AIC))

write.csv(results_aic, file="unmatched.csv")
