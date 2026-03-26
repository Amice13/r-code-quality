#!/usr/bin/env Rscript
#
# Code for transposing file of numbers
#
# Ona Wu, Massachusetts General Hospital
# Version 2025-12-09
#
# Copyright (c) 2025 Massachusetts General Hospital
# License: CC BY-NC-SA 4.0
#

args = commandArgs(trailingOnly=TRUE)
if (length(args)<3) {
  stop("At least three arguments must be supplied (input/output file sep).n", call.=FALSE)
} 

data=read.csv(args[1],sep=args[3], header=F)
write.table(t(data),file=args[2],row.names=F,col.names=F)
q()
