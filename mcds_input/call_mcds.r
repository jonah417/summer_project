
dsmodel <- ~cds(key="hn",formula=~distance+sex, adj.series="cos",
                adj.order=c(2,4))
mrmodel <- ~1
method <- "ds"
meta.data <- list(point=FALSE, width=5.0, left=0.0, binned=FALSE,
                  mono.strict=FALSE)
control <- list(showit=3)

test_file <- create_command_file(dsmodel,mrmodel,golftees,method,
                                 meta.data,control)

wine_call <- paste0("MCDS.exe 0, ", test_file)
call_status <- system(wine_call, intern=TRUE,
                      ignore.stdout=TRUE, ignore.stderr=TRUE)

mod_paste <- paste(dsmodel)
mod_vals <- try(eval(parse(text=mod_paste[2:length(modpaste)])))
