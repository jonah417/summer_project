# Concatenate to file function

# This function adds lines to the specified command file

# Inputs:
# text - the string to be concatinated to the file
# new_line - specifies whether there should be a line break after the input text
# default is TRUE as most concatenations are already complete lines

cat_file <- function(text, new_line=TRUE) {
  # in case the text input is a vector, paste the elements to create the final string
  text <- paste(text, collapse="")
  print(text)
  # add the text to the command file, adding a line break when specified
  if(new_line == TRUE) {
    cat(text, file=command.file.name, "\n", append=TRUE)
  }else{
    cat(text, file=command.file.name, sep="", append=TRUE)
  }
}