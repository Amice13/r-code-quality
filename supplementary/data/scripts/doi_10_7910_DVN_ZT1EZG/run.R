# get script location on computer
scriptPath = sub(".*=", "", commandArgs()[4])
if ( !file.exists(scriptPath) ) {
   scriptPath = paste0( getwd(), .Platform$file.sep, scriptPath)

   if ( !file.exists(scriptPath) ) {
      stop("\nERROR: Failed to determine location of run.R. Try to execute it within its directory.\n\n");
   }
}
scriptDir = dirname(scriptPath)
scriptName = basename(scriptPath)

# start browser
shiny::runApp(scriptDir, launch.browser = T)
