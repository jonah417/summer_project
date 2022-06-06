# assuming the inputs are in the same form as those used in ddf.R

# data must have the following fields:
# object, observer, detected, distance (matches mrds input)
# SMP_LABEL, SMP_EFFORT (required for mcds)
# optionally

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
  
  # !this needs redone with meta.data
  # !consider the case where TYPE="LINE" but DISTANCE="RADIAL"?
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
  if(TRUE %in% grepl("^size$",colnames(data))){
    # the fact that data is clustered is used later on, so this
    # boolean prevents us from having to check each time
    cluster <- TRUE
    cat("OBJECT=CLUSTER;", file=command.file.name, "\n", append=TRUE)
  }else{
    cat("OBJECT=SINGLE;", file=command.file.name, "\n", append=TRUE)
  }
  
  # managing the output options
  if(control$debug == TRUE){
    cat("DEBUG=TRUE;", file=command.file.name, "\n", append=TRUE)
  }
  
  # !not sure if the output levels match completely
  output_info_levels <- c("SUMMARY","RESULTS","SELECTION","ALL")
  specified_output_level <- output_info_levels[data$showit-1]
  cat(specified_output_level, file=command.file.name, "\n", append=TRUE)
  
  # !the selection section for model fitting; is that specified in ddf?
  
  # moving onto the data section
  
  cat("END;", file=command.file.name, "\n", append=TRUE)
  cat("DATA /STRUCTURE=FLAT;", file=command.file.name, "\n", append=TRUE)
  
  
  
}