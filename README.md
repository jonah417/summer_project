# Summer Project

This repo contains all of the code used to write the function that translates inputs from mrds to a command file that can be run in MCDS.exe. The file containing this function is create_command_file.r, which can be found in the mcds_input folder. Also in that folder is
- MCDS.exe: the MCDS engine
- call_mcds.r: a file used to test inputs to the create_command_file function
- move_columns.r: a function written to reorder the columns, which I realised wasn't required but was neat anyway, though still has some bugs
- Report.Rmd: my report of the project in rmarkdown

There are three other folders in the repo; analyse and testbase were taken directly from the optimist repo, and mrds contains relevant files from the mrds package.

Floating about in the repo is also:
- golftees.csv: the data file I used in most tests
- \tmp\RtmpFIUMuR\cmdtmpf75967b773.txt: the command file created by the code currently in call_mcds.r
- \tmp\RtmpFIUMuR\dataf716df542f.txt: the data file created by the code currently in call_mcds.r
