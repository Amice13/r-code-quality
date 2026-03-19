#!/usr/bin/Rscript

library(knitr)
library(rmarkdown)
library(tikzDevice)

content <- "liberty_rules-cps"
content.file <- paste(content, "Rmd", sep=".")
markdown.file <- paste(content, "md", sep=".")

# Uncomment for generating both output formats
#render(content.file, output_format="all", clean=FALSE)

# Uncomment for only pdf
render(content.file, output_format="bookdown::tufte_book2", clean=FALSE)

# Uncomment for only html
#render(content.file, output_format="bookdown::tufte_html2", clean=FALSE)


print(proc.time())
