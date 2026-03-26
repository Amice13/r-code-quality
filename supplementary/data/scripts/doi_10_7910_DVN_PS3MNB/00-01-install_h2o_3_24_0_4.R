#!/usr/bin/Rscript
##################################################################################################
# Social Media and Political Agenda Setting
##################################################################################################
# Description:
# Installation Script for the right version of h2o.
##################################################################################################
# Directories:
##################################################################################################
args = commandArgs()

scriptName = args[substr(args,1,7) == '--file=']

if (length(scriptName) == 0) {
  scriptName <- rstudioapi::getSourceEditorContext()$path
} else {
  scriptName <- substr(scriptName, 8, nchar(scriptName))
}

pathName = substr(
  scriptName, 
  1, 
  nchar(scriptName) - nchar(strsplit(scriptName, '.*[/|\\]')[[1]][2])
)

setwd(pathName)
parent_path <- getwd()
##################################################################################################
# Install H2O Version 3.24.0.4
##################################################################################################
install.packages("../h2o/h2o_3.24.0.4.tar.gz",
                   repos = NULL, type = "source")
##################################################################################################