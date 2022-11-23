# Options Section

# rewriting the following code with functions I've developed
if(cluster == TRUE){
  cat("OBJECT=CLUSTER;", file=command.file.name, "\n", append=TRUE)
}else{
  cat("OBJECT=SINGLE;", file=command.file.name, "\n", append=TRUE)
}

# define the value being checked
input <- cluster
# define the options to be concatenated
cat_options <- c("OBJECT=CLUSTER", "OBJECT=SINGLE")
# figure out which thing to concatenate
switch_options(switch_input = input, results = cat_options)

# next steps:
# test these functions
# try lapply across a list of conditions to be checked
# figure out if there's a way to remove the repetition of "OBJECT=..." etc

# TESTS
# check that id_fields works
#   - test with simple strings and one iteration
#   - test with loops
#   - test with real data
#   - test the required switch works
# check that cat_file works
#   - create a testing space where a command file is generated and written to
#   - test with one piece of text
#   - test with a vector of text
#   - check the new line switch
# check that input_options works
#   - create a testing environment with sample meta.data and a command file
#   - test the simple if statements

