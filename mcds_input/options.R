# Options Section

# A function which writes the OPTIONS section to the command file, based upon
# the mrds input

# Inputs:
#  - meta.data: a list as found in mrds
#  - control: a list as found in mrds

options <- function(meta.data, control) {
  # starting the options section within the command file
  cat_file("OPTIONS;")
  
  # creating a list with all the options and results
  opt_list <- list(point1=list(var=meta.data$point,
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
  
  # looping through each of the options and concatenating the relevant commands
  for(i in 1:length(opt_list)) {
    # inputting the relevant data to the cat_conditions function
    cat_conditions(opt_list[[i]][[1]],opt_list[[i]][[2]])
  }
  
  # the user will specify the adjustment term selection
  cat_file("SELECTION=SPECIFY;")
  # ending the options section
  cat_file("END;")
}