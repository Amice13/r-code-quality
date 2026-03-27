rm(list=ls())

library(data.table)  # all of our R data are in the data.table format
library(haven)       # data were haven-labelled by the ipumsr package
library(sjlabelled)  # additional helpful functions for labelled data
library(sjmisc)      # "  "  " functions for labelled data

library(sjPlot)      # "  "  " functions for labelled data (not used here)

Root <- "D:/Research"  # Root directory on Mark's computer

# On your computer, put the data downloaded from the Harvard Dataverse in this directory:
PathIn <- file.path(Root, "IPUMS", "R_Data_LowIncome_Merged_2021")

ISO3 <- "GTM"
Year <- 2002
File <- paste0(ISO3,"_",Year,"_Merged.RData") # compose the name of the file

#---------------------------------------------------------------------------------------------------
# PART ONE: Load a single dataset into memory
#---------------------------------------------------------------------------------------------------
load( file=file.path(PathIn, File) ) # All our data.tables are named "Merged"
class(Merged) # class() shows the type of object we're dealing with

#---------------------------------------------------------------------------------------------------
# PART TWO: Use functions from sjlabelled and sjmisc to explore variable and value labels, and to
# create a codebook
#---------------------------------------------------------------------------------------------------
# Variable names:
Nm <- names(Merged)
class(Nm)
# A character vector of variable labels; the get_label() function is from the sjlabelled package:
X <- get_label(Merged, def.value="no var label") 
class(X)

# Make a codebook, that is, an easier-to-read data.table of variable names and labels. Once we've
# extracted value labels (see below), we'll edit the last column of the codebook:
Codebook <- data.table(Name=Nm, VarLabel=X, HasValueLabel = FALSE)
View( Codebook )

# Value labels are also easy to access, but since (for many variables) IPUMS-I has assigned value
# labels that are global in scope---we'll see an example for the COUNTRY variable---it's probably
# best to keep the value labels in a separate object and access them only when they are clearly of
# interest.

# Using the get_labels() function from sjlabelled, we create a list (1 element per variable) with
# the value labels specific to each variable (if the variable has value labels. If the variable has
# no value labels, the contents will be NULL:
ValueLabels <- get_labels(Merged, non.labelled = TRUE, values="as.name")
class(ValueLabels)  # ValueLabels is of type "list", which is R's most general data structure
names(ValueLabels)  # Each element of the list is named, with the variable name
str(ValueLabels)    # Too much output, but should give you an idea of what's here

ValueLabels$COUNTRY  # An example of IPUMS-I global value labels
ValueLabels$LIT      # The NIU code is probably for children too young to have acquired literacy
ValueLabels$AGE2     # Value labels for age groups (note provision for alternative age groups)
ValueLabels$OCC      # An example of a variable that lacks value labels

# Fill in the last column of Codebook (i.e., the HasValueLabels variable) using a loop that goes
# from the first variable (row) to the last variable (last row):
for(I in 1:nrow(Codebook)){
  if(  !is.null( ValueLabels[[I]] )  ){ Codebook[I, HasValueLabel := TRUE] }
}

View(Codebook)

# Now we can get value labels for any variable whose HasValueLabels=TRUE. For example:
ValueLabels[[ Codebook[10,Name] ]] # The 10th variable is Group Quarters

#---------------------------------------------------------------------------------------------------
# PART THREE: Use functions from sjlabelled and sjmisc for tables and cross-tabulations using the
# value labels
#---------------------------------------------------------------------------------------------------

# The frq() function is from the sjmisc package. A good tool for univariate summaries:
with(Merged, frq(LIT)) 

# The flat_table() function is also from sjmisc:
flat_table(data=Merged, AGE2, LIT, show.values=TRUE) # basic cross-tabulation, counts
flat_table(data=Merged, AGE2, LIT, show.values=TRUE, margin="col") # cross-tabulation, column prop.

