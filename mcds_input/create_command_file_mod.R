# a modularised version of the original create_command_file function

# a function to take the input as required for the ddf function in mrds and
# create a command file for the mcds engine, which will perform an equivalent
# analysis.

# the input for this function is the same as the input for ddf

create_command_file_mod <- function(dsmodel=call(),mrmodel=call(),data,
                                method,meta.data,control) {
  print(environment())
  
  # create a temporary directory
  directory <- tempdir()
  
  # create command file
  command.file.name <- tempfile(pattern="cmdtmp", tmpdir=directory,
                                fileext=".txt")
  command.file.name <- gsub("/","\\\\",command.file.name)
  file.create(command.file.name)
  
  # HEADER section
  
  # specify the location of output files
  cat_file(paste(directory,"out.txt",sep=""))
  cat_file(paste(directory,"log.txt",sep=""))
  cat_file(paste(directory,"stat.txt",sep=""))
  cat_file(paste(directory,"plot.txt",sep=""))
  cat_file("None")
  cat_file("None")
  
  # Reformatting the data
  data_info <- reformat_data(data, dsmodel)
  data <- data_info[1]
  
  # extracting the information about covariates so that it can be used in other
  # subfunctions
  covar_pres <- data_info[2]
  covar_fields <- data_info[3]
  
  # create data file to pass to mcds
  data.file.name <- tempfile(pattern="data", tmpdir=directory,
                             fileext=".txt")
  data.file.name <- gsub("/","\\\\",data.file.name)
  file.create(data.file.name)
  write.table(data, file=data.file.name, col.names=FALSE, 
              row.names=FALSE, sep="\t")
  
  # OPTION section
  options_section(meta.data, control)
  
  # DATA section
  data_section(data, covar_pres, covar_fields)
  
  # input the absolute path to the data file
  gsub("/","\\\\",data.file.name)
  cat_file(paste("INFILE=", data.file.name, " /NOECHO;", sep=""))
  cat_file("END;")
  
  # ESTIMATE section
  estimate_section(dsmodel, covar_pres, covar_fields, meta.data)
  
  # output final command file
  return(command.file.name)
}