# a test space for functions

# ensuring we can use all of the functions in this file


# we need a command file to write to:

# create a temporary directory
directory <- tempdir()

# create command file
command.file.name <- tempfile(pattern="cmdtmp", tmpdir=directory,
                              fileext=".txt")
command.file.name <- gsub("/","\\\\",command.file.name)
file.create(command.file.name)

