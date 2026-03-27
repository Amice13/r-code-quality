#~~~~~  Qualitative Comparative Analysis (QCA) Using R: A Beginner’s Guide     ~~~~~

#            Ioana-Elena Oana, Carsten Q. Schneider, Eva Thomann 
#                

#~~~~~   Getting Started with R  ~~~~~


#### 1. INTRODUCTION ====

# R is freely available and can be downloaded directly from:
# https://cran.r-project.org/
# You can find detailed installation instructions online. 
# We will use R through the open source RStudio interface.
# Rstudio can be downloaded for free - after R was installed - from:
# http://www.rstudio.com/products/rstudio/download/
# Once both are installed, it will suffice to open Rstudio.  

# RStudio Interface:

# Upper right 
# 'Environment' tab: shows all currently active objects in your working environment.
# 'History' tab: shows a list of commands used so far. 

# Lower right 
# 'Files' tab: shows all the files and folders in the default workspace. 
# 'Plots' tab: displays all graphs that are produced.
# 'Packages' tab: lists a series of packages or add-ons needed to run certain processes. 
# 'Help' tab: offers additional information. 

# Left-hand side 
# Contains the console, where we see the so-called 'output'
# The lower left contains the output with our activities and results. 
# It is also possible to type and execute commands in the console. 
# The upper left contains the editor. We use this upper left pane to enter commands,
# our documentation, and save all in an R script. 

# Be aware: 
# R has no tolerance for typos or missing symbols/letters.
# R gets confused if we use the wrong symbols (e.g., dots instead of commas)
# R is case sensitive; 'A' has a different meaning than 'a'. 
 

#### 2. WORKING IN R =====

#### 2.1 Scripts and Comments =====

# To open the editor and start a new script: 
# File -> New File -> R Script 

# Editor window appears in upper left-hand side of the interface;
# Console appears below the editor. 

# Too save a script: 
# File −> 'Save' or 'Save As'

# Comments in the script start with # hashtag. 
# R ignores everything after # in the line.
# Use comments extensively in the script to explain what was done and why. 

#Clear the workspace: 
rm(list=ls()) 

#To execute a command, put the cursor in the line where command is and either
# hit STRG and Enter, or
# hit "STRG + R" (Windows), or 
# hit "cmd + Enter" (Mac).


#### 2.2 Working Directory, Packages & Help Function =====

# We always need to specify the 'working directory'
# This tells R where to read files from and save them.
# The working directory is simply a specific location on our computer. 
# Typically we set the working directory in the folder where the datasets we use 
# for the analysis are stored. 

# Identify current working directory:
getwd ()

# Set a new working directory:
setwd("/cloud/project/Data")

# Attention: this might not work for you, the WD needs to be a specific file path 
# on your computer where you place your data!


# Alternatively, we can set the working directory by clickling:
# Session −> Set Working Directory −> Choose Directory

# One can then browse to the file location that should be the working directory.
# After doing this, the path to the working directory will appear in the console. 

# Display the content of (= the files in) the working directory:
dir()

# We can install the packages with the function:
# install.packages() 

# We can list several packages to install. 
# the c() function combines its arguments (c stands for ’concatenate’) 
# With more than one package, each package needs to be placed in quotation marks.

# Install the packages QCA and SetMethods:

install.packages(c("QCA", "SetMethods"), dependencies = TRUE)

# We specify the option to also install “dependencies”, that is, 
# all other packages that might be required for performing the functions in QCA 
# and SetMethods. 

# We need to install a package only once. Once we successfully installed all 
# required packages, we just need to load them when starting the R session.

# Load the packages
library(QCA)
library(SetMethods)

# To see which packages are loaded:
search()

# To get help for a specific function:
?getwd

# To get help for a keyword (e.g. QCA):
??qca

# To get help about a topic:
??"descriptive statistics"


#### 2.3 Objects and Operators =====
 
# R is an object-oriented language.
# Functions use the content of an object and produce results. 
# Datasets are objects, Variables are objects, and we can even 
# save results as objects to recall them any time for further use. 

# To generate an object, we give it a label and use a backward arrow <- to define 
# its content and store it in the workspace. As soon as phrases are involved, we 
# need to use quotation marks.

# Create an object "salute" with the phrase "hi there":
salute <- "hi there"

# Return the object:
salute

# Create an object containing a list of phrases:
conversation <- c("hi", "what's up?", "coffee please.")
conversation

# Create a numerical object
numericalobject <-37
numericalobject

# Create an object containing a list of numbers:
count <-  c(1,2,3)
count

# We can delete specific objects with the rm() command:
rm(count)

# Create a logical object:
logicalobject <- numericalobject>36
logicalobject

# Return a logical object as numerical:
as.numeric(logicalobject)

# Identify the class type of the object:
class(numericalobject)

# Delete all content (clear) in the whole workspace:
rm(list = ls())

# R integrates a variety of arithmetic and logical operators. 
# This also allows us to use R as a calculator. 

# Perform arithmetic operations:
z <- 2+7
z
x <- ((1+3)/7*18)^2
x

# Perform operations on objects:
y <- z+x
y
z ==12
(z!=x)&(z<=x)

# Perform functions on objects:
sqrt(z)


#### 3. HANDLING DATASETS =====

# In R, we can easily import data, store it as an object, and use it for various 
# analyses in R. We usually import datasets as so-called data frame objects into R. 

# Data frames are are 2-dimensional objects in that they look like a table with 
# columns and rows on which values are stored. In a data frame the columns represent 
# different variables, while the rows represent different units of analysis 
# (or cases) on which information is collected.


#### 3.1 Opening and Saving Datasets =====

# A popular format for dataset files is ’.csv’ (comma-separated values). 
# Depending on the settings, this format uses a comma or a semicolon to separate 
# values in a delimited, table-like text file. 
# We will need to import the dataset into R before starting the analysis.

# To import a dataset, we should first make sure to set the working directory
# to the folder where we have stored the dataset 

# To load a .csv file from that folder we use the function:
# read.csv()
# Inside the brackets, we input the name of the file and its extension 
# using quotation marks (e.g. "data.csv"). 

# Note: The data.csv file needs to be in your working directory set above:

# For example:
mydata <- read.csv("asylumraw.csv", header = TRUE, row.names = 1, sep = ",", dec = ".")

# We can ask for help with the read.csv function:

?read.csv

# Or we can load data directly saved into packages
# Here we load the LIPC data saved in the SetMethods package

data("LIPC")

# To importing dataset files in other format, we can install the "foreign" package,
# which contains specific functions for each file type:
#install.packages("foreign") 
library(foreign)

# Octave: read.octave()
# STATA: read.dta() 
# SAS: read.ssd() 
# PSS: read.spss() 
# Minitab: read.mtp() 
# Systat: read.systat()


#### 3.2 Inspecting and Describing Data =====

# We start by creating an artifical dataset 'newdt':

# We create 2 numerical vectors:
x <- c(1,2,3,4)
y <- c(4, 7, 100, -2)

# We create 1 logical vector:
z <- c(TRUE, FALSE,FALSE, TRUE)

# We create the dataframe 'newdt' containing the three vectors:
newdt <- data.frame(x,y,z)

# We can look up the current lables of the columns and rows:
names(newdt)
rownames(newdt)

# We can rename the columns:
names(newdt) <-  c("vector1", "vector2", "vector3")

# We can rename the rows:
rownames(newdt) <- c("c1", "c2", "c3", "c4")

# We can save the data frame as a .csv file:
write.csv(newdt, "my3vectors.csv")

# Checking the first 5 rows of the data:
head(newdt)

# Checking the names of the columns:
names(newdt)

# Checking the names of the rows:
rownames(newdt)

# Checking the dimensions of the dataset:
dim(newdt)

# Checking the structure of the dataframe object:
str(newdt)

# We can use the dollar sign to access parts of a more complex object

# For example, looking at variable 'vector1' in our data frame:
newdt$vector1

# But we can also use square brackets

# For example, checking the values of case "c1" on all variables:
newdt["c1", ]

# Checcking the values of variable 'vector1' on all cases:
newdt[,"vector1"]

# Checking the value in the fourth row and second column:
newdt[4,2]

# Checking the values on the second row of the data:
newdt[2,]

# Checking the values of the third column of the data:
newdt[,3]

# Checking the value on the first and the third column of the data:
newdt[,c(1,3)]

# Checking the values from the first to the third column:
newdt[,c(1:3)]

# Subsetting the data
subset(mydata, (L>0.5) & (RM> 0.5), select=INT)

# We can also do some descriptive statistics:

summary(newdt) # for the entire dataframe
summary(newdt$vector1)  # for the column vector1
mean(newdt$vector1) # mean of column vector1
sd(newdt$vector1)  # standard deviation of column vector1
sum(newdt$vector1) # sum of the values in column vector1
table(newdt$vector1) # frequency of each value in column vector1


#### 3.3 Adding, Renaming, Deleting, and Recoding  =====

# After having a dataset imported into R we may want to 
# add, recode, rename, or delete variables or cases.

# Adding a column called 'vector4' to the 'newdt' data:
newdt$vector4 <- c("red", "blue", "green", "yellow")
head(newdt)

# Rename the 4th column in newdt as "colours":
names(newdt)[4] <- "colours"

# Deleting the 4th column in newdt:
newdt <-newdt[,-4]

# Recoding FALSE to TRUE in column vector3 :
newdt[newdt$vector3==FALSE, "vector3"] <- TRUE

# Recoding numbers >= 5 in 'vector2' with the expression 'bignumber':
newdt[newdt$vector2>=5, "vector2"] <- "bignumber"
newdt

# Recoding a particular value:
newdt[2,3] <- 'anewvalue'
newdt["c1", "vector1"] <- 'anewvalue'

# Recoding a variable using the recode() function
mydata$LABRES2 <- recode(mydata$LABRES, "1,3,5:7=1; 4,8,9=2; else=copy")
head(mydata)

# Adding a new row named 'c5' to newdt:
newdt <- rbind(newdt, "c5" = c(1,2,FALSE))
head(newdt)

# Deleting the 5th row from newdt:
newdt <- newdt [-5,]

#### 3.3 Dealing with missing data  =====

# Number of missing values per row:
rowSums(is.na(newdt))

# Number of missing values per column/variable:
colSums(is.na(newdt))

# Convert to missing data:
newdt[newdt$vector2==4,"vector2"] <- NA

# List rows that have missing values:
newdt[!complete.cases(newdt),]

# Create a new dataset without missing data by listwise deletion:
newdt <- na.omit(newdt)

# Check for presence of missing data in dataset:
is.na(newdt)

# Check for the presence of missing data in specific variable:
is.na(newdt$vector1)

# Check for the absence of missing data in dataset or variable:
complete.cases(newdt)
complete.cases(newdt$vector1)

# And now with the Sager and Thomann dataset:

# Recode missing values (here: -99) into format suitable for R (NA)
mydata[mydata==-99] <- NA

# Check for the presence of missing data in dataset
is.na(mydata)

# Check for the presence of missing data in specific variable of dataset
is.na(mydata$SPPMAJ)

# Check for the absence of missing data in dataset or variable
complete.cases(mydata)
complete.cases(mydata$SPPMAJ)

# Obtain the number of cases with missing values (0) and with no missing values (1)
nomissings <- as.numeric(complete.cases(mydata))
table(nomissings)

# Based on this, obtain the percentage of cases with 
# missing values (left-hand side of crosstable) and 
# without missing values (right-hand side) 
prop.table(table(nomissings))

# Identify the names of the cases that have missing values on a specific variable:
varmiss <- as.numeric(is.na(mydata$SPPMAJ))
rownames(subset(mydata, varmiss==1))

# Include only complete cases in dataset
nomissings <- as.numeric(complete.cases(mydata))
mydata = mydata[nomissings== 1,]

