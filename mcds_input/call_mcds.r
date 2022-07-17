
dsmodel <- ~cds(key="hr",formula=~exposure+sex, adj.series="cos",
                adj.order=c(2,4))
mrmodel <- ~1
method <- "ds"
meta.data <- list(point=FALSE, width=5.0)
control <- list()

test_file <- create_command_file(dsmodel,mrmodel,golftees,method,
                                 meta.data,control)

wine_call <- paste0("MCDS.exe 0, ", test_file)
call_status <- system(wine_call, intern=TRUE,
                      ignore.stdout=TRUE, ignore.stderr=TRUE)

mod_paste <- paste(dsmodel)
mod_vals <- try(eval(parse(text=mod_paste[2:length(modpaste)])))


