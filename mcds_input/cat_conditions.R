# Concatenate with Conditions Function

# A function which allows different lines to be written to the command file
# depending on the mrds input specifications

# Inputs:
# switch_input - the variable to have the options of considered
# conditions - a vector containing the potential values switch_input may take;
# default is TRUE and FALSE
# results - a list of results given each of the conditions
# new_line - specifies whether a new line should be started after the command
# line; corresponds to the new_line input of the cat_file() function


cat_conditions <- function(switch_input, results, conditions=c(TRUE,FALSE), new_line=TRUE) {
  # checking that the input is present
  if(!is.null(switch_input)){
    # find the index of the condition which is found to be met
    result_index <- grep(switch_input, conditions)
    # find the corresponding response
    fin_result <- results[[result_index]]
    # write the correct command to the command file
    cat_file(text=fin_result, new_line=new_line)
  }
}