# a test space for functions

library(Distance)

# we need a command file to write to:

# create a temporary directory
directory <- tempdir()

# create command file
command.file.name <- tempfile(pattern="cmdtmp", tmpdir=directory,
                              fileext=".txt")
command.file.name <- gsub("/","\\\\",command.file.name)
file.create(command.file.name)

# TESTS
# check that id_fields works
#   - test with simple strings and one iteration
#   - test with loops
# check that cat_file works
#   - create a testing space where a command file is generated and written to
#   - test with one piece of text
#   - test with a vector of text
#   - check the new line switch
# check that cat_conditions works
#   - create a testing environment with sample meta.data and a command file
#   - test the simple if statements

# TESTS
# testing the id_fields function works
#   - test with simple strings and one iteration

test_cols <- c("cat","dog","horse","sheep","duck")
cat_alt <- c("cat","feline")
test1_1 <- id_fields(test_cols,cat_alt)
test1_1

#   - test with loops

# loading in data to use in the tests
data("CueCountingExample")
fields <- colnames(CueCountingExample)

req_fields <- list(list("SMP_LABEL", c("SMP_LABEL","sample.label"), TRUE),
                   list("SMP_EFFORT", c("SMP_EFFORT","effort","Search.time"), TRUE),
                   list("DISTANCE", c("distance"), TRUE), 
                   list("STR_LABEL", c("STR_LABEL","region.label"), FALSE),
                   list("STR_AREA", c("STR_AREA","area"), FALSE),
                   list("size", c("size"), FALSE))

fields
# create a vector to store the indices of the recognised fields
fields_index <- rep("", length(req_fields))
# for each of the required fields, find the column and add in if not present
for(i in 1:length(req_fields)) {
  # record which column the ith required field is found in the dataframe
  print(fields_index[i])
  print(id_fields(fields,req_fields[[i]][[2]]))
  fields_index[i] <- id_fields(fields,req_fields[[i]][[2]])
  # if the field is not present but is required, add in a dummy variable
  if(fields_index[i] == FALSE && req_fields[[i]][[3]] == TRUE) {
    data$new <- rep(1,nrow(data))
    tail(fields,1) <- req_fields[[i]][[1]]
  } else {
    # rename the column to match the name required by MCDS
    fields[strtoi(fields_index[i])] <- req_fields[[i]][[1]]
  }
}
fields

# test cat_file
cat_file(c("good thanks ","muy bien"))
read.delim(command.file.name)

# bear in mind that the new line code relates to the line after it

# test cat_conditions

# creating test meta.data
test_meta <- list(point = TRUE, width = "34")
test_cat_res <- list(c("DISTANCE=RADIAL /UNITS='Meters' /WIDTH=", 
                                   test_meta$width, ";"), 
                  c("DISTANCE=PERP /UNITS='Meters' /WIDTH=", 
                    test_meta$width, ";"))

cat_conditions(switch_input = test_meta$point, results = test_cat_res)
read.delim(command.file.name)

if(meta.data$point == TRUE){
  cat(paste("DISTANCE=RADIAL /UNITS='Meters' /WIDTH=", 
            meta.data$width, ";", sep=""), file=command.file.name, "\n", 
      append=TRUE)
  cat("TYPE=POINT;", file=command.file.name, "\n", append=TRUE)
}else{
  cat(paste("DISTANCE=PERP /UNITS='Meters' /WIDTH=", 
            meta.data$width, ";", sep=""), file=command.file.name, "\n", 
      append=TRUE)
  cat("TYPE=LINE;", file=command.file.name, "\n", append=TRUE)
}




opt_list <- list(list(var=meta.data$point,
                             results=list("TYPE=POINT;","TYPE=LINE;")),
                 point2=list(var=meta.data$point,
                             results=list(c("DISTANCE=RADIAL /UNITS='Meters' /WIDTH=", 
                                            meta.data$width, ";"), 
                                          c("DISTANCE=PERP /UNITS='Meters' /WIDTH=", 
                                            meta.data$width, ";"))),
                 cluster=list(var=cluster,
                              results=list("OBJECT=CLUSTER;","OBJECT=SINGLE;")),
                 debug=list(var=control$debug,
                            results=list("DEBUG=ON;","")))



