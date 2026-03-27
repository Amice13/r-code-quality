# Initialise renv in the project
 #install.packages("renv")

# load package
library(renv)

# init() scans your project for package calls and installs 
# all detected packages with their dependencies into an 
# isolated

renv::init() 

# Snapshot the state of the library to renv.lock
renv::snapshot()

# - Lockfile written to "~/../renv.lock".