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
