# creating a set of test inputs, in the same form as those used in ddf.R


create_command_file(dsmodel=call(),mrmodel=call(),data,method,
                    meta.data,control) {
  # !create file name
  
  command.file.name <- tempfile(pattern="cmdtmp", tmpdir="tmp_files",
                                fileext=".txt")
  # create the command file
  file.create(command.file.name)
  # output commands to it
  cat("out.txt", file=command.file.name, "\n", append=TRUE)
  cat("log.txt", file=command.file.name, "\n", append=TRUE)
  cat("stat.txt", file=command.file.name, "\n", append=TRUE)
  cat("plot.txt", file=command.file.name, "\n", append=TRUE)
  cat("None", file=command.file.name, "\n", append=TRUE)
  cat("None", file=command.file.name, "\n", append=TRUE)
  cat("OPTIONS;", file=command.file.name, "\n", append=TRUE)
  
  # fill in option section
  # !find whether cue counting was used, in which case TYPE=CUE
  
  transect_type <- toupper(data$transect)
  cat("TYPE=", transect_type, ";",
      file=command.file.name, "\n", append=TRUE)

  if(grepl("LINE", transect_type)){
    cat("DISTANCE=PERP /UNITS='Meters' /WIDTH=", meta.data$width,
        ";", file=command.file.name, "\n", append=TRUE)
  }else{
    cat("DISTANCE=RADIAL /UNITS='Meters' /WIDTH=", meta.data$width,
        ";", file=command.file.name, "\n", append=TRUE)
  }
  
  # !if cue counts are used, state cue rate and SE
  
  # define whether there are clusters
  if(grepl("(^|,)size($|,)", data$fieldnames)){
    cat("OBJECT=CLUSTER;", file=command.file.name, "\n", append=TRUE)
  }else{
    cat("OBJECT=SINGLE;", file=command.file.name, "\n", append=TRUE)
  }
  
  # moving onto the data section
  
  cat("END;", file=command.file.name, "\n", append=TRUE)
  cat("DATA /STRUCTURE=FLAT;", file=command.file.name, "\n", append=TRUE)
  
}