# assuming the inputs are in the same form as those used in ddf.R

# data must have the following fields:
# object, observer, detected, distance (matches mrds input)
# SMP_LABEL, SMP_EFFORT (required for mcds)
# optionally

create_command_file <- function(dsmodel=call(),mrmodel=call(),data,
                                method,meta.data,control) {
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
  cat("PRINT=", specified_output_level, file=command.file.name, "\n", 
      append=TRUE)
  
  # !the selection section for model fitting; is that specified in ddf?
  
  # moving onto the data section
  
  cat("END;", file=command.file.name, "\n", append=TRUE)
  cat("DATA /STRUCTURE=FLAT;", file=command.file.name, "\n", append=TRUE)
  
  # !this is only necessary if fields appear as they do in example data
  # create a vector of fields, renamed to match mcds
  fields <- colnames(data)
  # find which field will be used for the effort and change the name 
  # to match field name in mcds
  if(TRUE %in% grepl("^Effort$",colnames(data))){
    fields[colnames(data)=="Effort"] <- "SMP_EFFORT"
  }else if(TRUE %in% grepl("^Search.time$",colnames(data))){
    fields[colnames(data)=="Search.time"] <- "SMP_EFFORT"
  }
  # check if other defined fields are columns in the dataset
  if(TRUE %in% grepl("^Region.Label$",colnames(data))){
    fields[colnames(data)=="Region.Label"] <- "STR_LABEL"
  }
  if(TRUE %in% grepl("^Area$",colnames(data))){
    fields[colnames(data)=="Area"] <- "STR_AREA"
  }
  # change sample label field name to match mcds
  fields[colnames(data)=="Sample.Label"] <- "SMP_LABEL"
  
  # change all fields to upper case and combine to one string
  fields <- paste(toupper(fields), collapse=", ")
  cat("FIELDS=", fields, file=command.file.name, "\n", append=TRUE)
  
  # !how to define a cluster size covariate?
  
  # deal with factors
  # !would have to add factors as a part of meta.data
  if(meta.data$factors != ""){
    # run through each of the factors
    for(i in 1:length(meta.data$factors)){
      # find which field of the data the ith factor is
      field_index <- grep(meta.data$factors[i]$field_name,colnames(data))
      # find the corresponding renamed field
      field_name <- fields[field_index]
      cat("FACTOR /NAME=", toupper(field_name), "/LEVELS=",
          meta.data$factors[field_index]$levels, "/LABELS=", 
          meta.data$factors[field_index]$labels, file=command.file.name, 
          "\n", append=TRUE)
    }
  }
  
  cat("INFILE=", data.file.name, "/NOECHO;", file=command.file.name, 
      "\n", append=TRUE)
  cat("END;", file=command.file.name, "\n", append=TRUE)
}


test_data <- golftees
core_cols <- c("object","observer","detected","distance","Sample.Label")
rearrange_tees <- move_columns(test_data,core_cols)
