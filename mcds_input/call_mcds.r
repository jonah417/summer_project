# a test space for creating the command file with various different inputs

dsmodel <- ~cds(key="hr",formula=~sex,adj.series="cos",adj.order=c(2,4),
                adj.scale="width")
mrmodel <- ~1
method <- "ds"
meta.data <- list(point=FALSE,width=5.0)
control <- list(initial=list(scale=list(sex=1),adjustment=c(0,3)),
                showit=0)

test_file <- create_command_file(dsmodel,mrmodel,golftees,method,
                                 meta.data,control)


