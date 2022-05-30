# where are the problems

library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)

# load the data
load("../testbase/results-BFGS.RData")
results$engine[results$engine=="MRDS-BFGS"] <- "MRDS"
results_mcds <- subset(results, engine=="MCDS")

load("../testbase/results-all-intfail.RData")
results <- subset(results, engine!="MCDS")
results$engine <- "MRDS"

results <- rbind(results_mcds, results)


# pre-process
# recode model specs for plotting
results$model_desc <- results$model
#results$model_desc[!grepl("(H[AN])-(\\w+)-\\d",
#                             results$model_desc)] <- "covar"
results$model_desc <- sub("(H[AN])-\\w+-0", "\\1",
                             results$model_desc)

# add adjustment scaling info
results$model_desc[!grepl("covar", results$model_desc)] <-
  paste0(results$model_desc[!grepl("covar", results$model_desc)], "-",
         results$adjstd[!grepl("covar", results$model_desc)])

results$adjstd <- NULL

# information about likelihoods


# make MRDS/MCDS columns:
results_ll <- results %>%
  filter(Name=="loglik") %>%
  select(Value, name, model_desc, engine)

results_ll <- unique(results_ll)

results_ll <- results_ll %>%
  pivot_wider(id_cols=c("name", "model_desc", "engine"),
              names_from="engine", values_from="Value")

# failures
table(data.frame(MCDS=is.na(results_ll$MCDS),
                 MRDS=is.na(results_ll$MRDS)))

sort(table(results_ll$model_desc[is.na(results_ll$MRDS)]), decreasing=TRUE)
sort(table(results_ll$name[is.na(results_ll$MRDS)]), decreasing=TRUE)

failures <- results_ll[, c("model_desc", "name")][is.na(results_ll$MRDS), ]
write.csv(failures, file="failures.csv")

# did any of those have weird values for MCDS?
# like a bounds issue for the shape par?
failure_mcds <- results
failure_mcds <- subset(failure_mcds, engine=="MCDS" & Name=="shape")
failure_mcds <- subset(failure_mcds, model_desc %in% failures$model_desc)
sum(abs(failure_mcds$Value-1) < 1e-2)
# 4?

# which is better?
results_ll$diff <- results_ll$MCDS-results_ll$MRDS
data.frame(MCDS=sum(results_ll$diff>0, na.rm=TRUE),
           MRDS=sum(results_ll$diff<0, na.rm=TRUE),
           isNA=sum(is.na(results_ll$diff)))


# which datasets or models?

# where MRDS was better
sort(table(results_ll$model_desc[results_ll$diff<0]), decreasing=TRUE)
sort(table(results_ll$name[results_ll$diff<0]), decreasing=TRUE)

# where MCDS was better
sort(table(results_ll$model_desc[results_ll$diff>0]), decreasing=TRUE)
sort(table(results_ll$name[results_ll$diff>0]), decreasing=TRUE)


# where MCDS was better by 1
sort(table(results_ll$model_desc[results_ll$diff>0 &
                                 abs(results_ll$diff)>1]), decreasing=TRUE)
sort(table(results_ll$name[results_ll$diff>0 &
                           abs(results_ll$diff)>1]), decreasing=TRUE)



# where do bad things happen?
p_ll <- ggplot(results_ll, aes(text=name)) +
  geom_abline(slope=1, intercept=0) +
  geom_point(aes(x=MCDS, y=MRDS)) +
  facet_wrap(~model_desc, scale="free") +
  theme_minimal()
ggplotly(p_ll)

