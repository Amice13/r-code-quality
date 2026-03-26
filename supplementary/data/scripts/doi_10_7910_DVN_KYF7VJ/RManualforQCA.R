### R Manual for QCA ####

# Version: 3.0, October 2023
# by Patrick A. Mello (p.a.mello@vu.nl)
# https://patrickmello.com

# This manual is the online appendix to the book: 
# Mello, Patrick A. (2021) Qualitative Comparative Analysis: An Introduction to 
# Research Design and Application, Washington, DC: Georgetown University Press.

# The online appendix is available at: https://doi.org/10.7910/DVN/KYF7VJ
# Consult the "R Manual for QCA" PDF file for additional documentation.

### Introduction to R ####

# Welcome to RStudio and R! R is an open-source and platform-independent
# programming language. RStudio is an integrated development
# environment and a widely-used R editor that helps you work with R 
# more efficiently. You can customize RStudio under "Preferences".

# For the latest versions of R and RStudio see:
# https://cran.r-project.org/ and www.rstudio.com

# Note that the "#" symbol allows for "commentary" because anything you write 
# after this symbol will not be computed by R. Use comments to remember the
# function of specific commands and analytical steps.

# Also note that R is case-sensitive: "Hi" and "hi" are NOT the same in R.

# When you make a mistake, RStudio will often point you to the source of the
# error with a red sign, e.g. because of missing brackets, spelling errors, 
# and unknown commands.

# You can ask R for help in case you don't know what a command does.
# Just type "?" and the command. Or type "help()"

# Examples using the commands "library" and "read.csv":
# To run a command, position the cursor in the respective line
# and press "run" in the upper menu (or press command/control + enter).
# You can also select multiple lines or an entire script of code:
?library
help(read.csv)

# R can be used for basic math:
2 + 2
3 * 30
4 / 3
(30 - 4) / (60 + 20)

# R is object based. To create an object, use the arrow "<-" sign. Once an 
# object is created, it appears in your "environment" (top right window). 
# It can then be modified and used for your analysis. 
# You can list your environment with "ls()".
A <- 2 + 2
B <- 3 * 14
City <- "Amsterdam"
ls()

# Now type the objects' names:
A
City

# Be careful not to use the same name twice - it will be overwritten:
A <- 3 * 3

# Objects can be used for computation:
A * B
A - B

# Remove objects from your workspace with "rm()":
rm(City)

# You can also remove everything (to start again):
rm(list = ls())

### Working Directory ####

# To start working with R, we have to set our "working directory" so that
# everything is readily accessible (data, R scripts, exercises).

# Create a new folder on your computer where all these files will be placed.
# Because R does not recognize subfolders, everything should be in the same
# folder.

# Check your current working directory with "getwd()":
getwd()

# Set a new working directory with "setwd()".
# Example (adapt to your own settings):
setwd("/mydirectory/mydata")

# Check what is inside the folder with "list.files()":
list.files()

### Reading Data ####

# To work with R, we need to import data. R can read various formats, 
# but we will mainly work with comma-separated files (.csv).
# For this, use the command "read.csv()":
fuzzy.data <- read.csv("FuzzyData.csv") # reads as 1 variable
semi.colon.data <- read.csv("FuzzyData.csv", sep = ";") # no row names
fuzzy.data <- read.csv("FuzzyData.csv", sep = ";", row.names = 1) # correct

# read.csv() can be further specified, depending on your data:
# "row.names" specifies the column that contains row names (your case names)
# "header" specifies whether the first row contains variable names
# "sep" specifies the separator used in the csv file (comma, semicolon, etc.) 
# For additional options, see:
?read.csv

### Installing Packages ####

# Many functions are included in R, for others you first need to install
# "packages" of specific functions. 

# We will mainly work with the R packages "QCA" (Duşa 2019),
# "SetMethods" (Oana & Schneider 2018), and "ggplot2" (Wickham 2016)

# To install a package, use "install.packages()".
install.packages("ggplot2")
install.packages("QCA")
install.packages("SetMethods")

# Once installed, load the respective packages using "library()":
library("ggplot2")
library("QCA")
library("SetMethods")

# You can always look up help and documentation on a package:
help(package = "QCA")

# Citing Packages
# There is also a function to gain citation info:
citation()
citation("QCA")
citation("SetMethods")
citation("ggplot2")

# Finally, we can gain summary information on our R current session: 
sessionInfo()


### Data Management ####

# Examining pre-existing data sets (entailed in R packages):
data(package="SetMethods")

# Reading the data set "VISF" (Vis 2009) entailed in the "SetMethods" package:
data(VISF)

# Examining the first six lines of the data set:
head(VISF)

# Creating New Data Sets
# Remove everything from your R workspace
rm(list = ls())

# Create vectors
a <- c(1, 3, 17)
b <- c(24.1, 18.0, 10.3)
c <- c(6.34, 3.78, 0.61)

# Create data frame
our.data <- data.frame(a, b, c)

# Assign names to case rows
row.names(our.data) <- c("Shanghai", "Karachi", "Seoul")

# Assign names to columns
columns <- c("rank", "population", "size")
colnames(our.data) <- columns

# Examine the data set
our.data

# Modifying Data Sets
# Change the name of a column
colnames(our.data)[2] <- "new name"

# Add a case with certain values for the three conditions
our.data[nrow(our.data) + 1,] = list(27, 8.7, 1.57)

# Assign a name to the new case
row.names(our.data)[4] <- "London"

# Change a specific value (4th row, 2nd column)
our.data[4,2] <- 11.2

# Delete a case (row 2)
new.data <- our.data[-2,]

# Deleting several conditions (columns 2 and 3)
new.data <- our.data[,-(2:3)]

# Deleting a single condition (column 2)
our.data[,2] <- NULL

# Saving Data Sets
write.csv(our.data, "mydata.csv")

# Check contents of the working directory
list.files()


### Descriptive Statistics ####

# We work with the "VISF" data set, loaded above
data(VISF)

# Calculate the mean for the condition p
mean(VISF$p)

# Calculate the median for the condition s
median(VISF$s)

# Plot the condition p against the condition u
plot(VISF$p, VISF$u)

# Saving the plot as PDF file
pdf(file = "XYplot.pdf")
plot(VISF$p, VISF$u)
dev.off()

# For further customization, see the documentation
help("pdf")

# Draw a histogram for the condition u
hist(VISF$u)

# Histograms, side by side (one row, two columns),
# with thicker container lines
par(mfrow = c(1, 2), lwd = 2)
hist(VISF$p)
hist(VISF$s)

# Reset graphical parameters
par(mfrow=c(1,1))

### Histogram (Template) ####

# Histogram, further customized
hist(
  VISF$u,
  col = "white",
  xlim = c(0, 1),
  xlab = "Condition U",
  font = 2,
  font.lab = 2,
  lty = 1,
  lwd = 2,
  main = ""
)

### Set Operations ####

# Create data frame "Boole", values from Table 3.3 (Mello 2021: 51)
A <- c(1, 1, 0)
B <- c(0, 1, 0)
C <- c(0.9, 0.7, 0.2)
D <- c(0.3, 0.8, 0.4)
Boole <- data.frame(A, B, C, D)

# Boolean AND (minimum)
Boole$AandB <- pmin(A, B)
Boole$CandD <- pmin(C, D)

# Boolean OR (maximum)
Boole$AorB <- pmax(A, B)
Boole$CorD <- pmax(C, D)

# Boolean negation (NOT)
Boole$notA <- 1-A
Boole$notC <- 1-C

# Complete table with Boolean operations
Boole

### Venn Diagrams ####

# We work with the "venn" package (Duşa 2022)
install.packages("venn")
library("venn")

# Venn diagram with two conditions
venn(2)

# Intersection and Union of A and B
par(mfrow = c(1, 2))
venn("A*B", zcol = "red")
venn("A+B", zcol = "green")

### Calibrating Sets ####

# Creating Raw Data
Raw1 <- runif(30, min = 0, max = 60)
Raw2 <- runif(30, min = 0, max = 60)
DT <- data.frame(Raw1, Raw2)

# Round to two digits
DT <- round(DT, digits = 2)

# Examine the data
head(DT)

# Direct method of calibration
# Create the fuzzy-set condition "Fuzzy 1", 
# using the empirical anchors 10, 30, and 50 for full
# set non-membership, cross-over, and full set membership
DT$Fuzzy1 <-calibrate(DT$Raw1, 
                      type = "fuzzy", 
                      method = "direct", 
                      thresholds = "10, 30, 50")

# Round to two digits
DT <- round(DT, digits = 2)

# Create a crisp-set condition using 30 as cross-over
DT$Crisp1 <- calibrate(DT$Raw1, 
                       type = "crisp", 
                       method = "direct", 
                       thresholds = "30")

# Create a 'qualitative' condition, assigning specific scores to ranges
# of values in the raw data
Qual1 <- NA 

Qual1[DT$Raw1>=50] <- 1 				        # Full inclusion
Qual1[DT$Raw1<50 & DT$Raw1>30] <- 0.67  # "More in than out"
Qual1[DT$Raw1==30] <- 0.50 			        # Cross-over
Qual1[DT$Raw1<30 & DT$Raw1>10] <- 0.33 	# "More out than in"
Qual1[DT$Raw1<=10] <- 0                 # Full exclusion

DT$Qual1 <- Qual1                       # Place inside data frame

# Examine data frame
head(DT)

## Necessary Conditions ####

# Read data
FD <- read.csv("FuzzyData.csv", row.names = 1, sep = ";")

# Columns 1-3 (C1, C2, C3), their negations and the outcome
QCAfit(FD[,1:3], 
       FD$OUT, 
       necessity = TRUE, 
       neg.out = FALSE)

# Columns 1-3 (C1, C2, C3), their negations and the non-outcome 
QCAfit(FD[,1:3], 
       FD$OUT, 
       necessity = TRUE, 
       neg.out = TRUE)

## Truth Table Analysis ####

# (This uses the FD data loaded above)

# Create a truth table from the FD data
TT <- truthTable(FD, 
                 "OUT", 
                 complete = TRUE, 
                 show.cases = TRUE, 
                 incl.cut = 0.75,
                 sort.by = "incl, n")

TT                         # Display the truth table TT
str(TT)                    # Display the structure of the TT object

write.csv(TT$tt, "TT.csv") # Save the truth table element in a new file

# Create a truth table and specify the conditions to be included
TT2 <- truthTable(FD, 
                  "OUT", 
                  complete = TRUE, 
                  show.cases = TRUE, 
                  incl.cut = 0.75,
                  conditions = c("C1", "C2"),
                  sort.by = "incl, n")
TT2

### Solution Types ####

## Minimizing the Truth Table
# Conservative solution
sol.cons <- minimize(TT, 
                     include = "1", 
                     details = TRUE, 
                     use.tilde = TRUE)

sol.cons   # Examine the solution

# Parsimonious solution
sol.pars <- minimize(TT, 
                     include = "?",
                     details = TRUE, 
                     use.tilde = TRUE)
sol.pars

# Examine simplifying assumptions (SAs) of the parsimonious solution
sol.pars$SA       # Logical remainder rows 3 and 8 were used as SAs

# Intermediate solution #1
TT                # First, examine the complete truth table

# Exlude a single logical remainder row (row 3), for example's sake
sol.int1 <- minimize(TT, 
                     include = "?", 
                     details = TRUE, 
                     use.tilde = TRUE,
                     omit = (3))
sol.int1

sol.int1$SA       # Examine simplifying assumptions (three LRs were used)

# Intermediate solution #2
# Exclude several logical remainder rows (rows 5 & 8, for example's sake)
sol.int2 <- minimize(TT, 
                     include = "?", 
                     details = TRUE, 
                     use.tilde = TRUE,
                     exclude = c(5, 8))
sol.int2

sol.int2$SA       # Examine simplifying assumptions (row 3 has been used)

# Intermediate solution #3
# Introduce directional expectations for conditions C1 and C2
sol.int3 <- minimize(TT, 
                     include = "?", 
                     details = TRUE, 
                     use.tilde = TRUE,
                     dir.exp = c(C1, ~C2))
sol.int3

# When introducing directional expectations, the simplifying assumptions
# need to be checked in a different manner (due to how the QCA package works).
# These are entailed in the component "i.sol" that is created for any 
# intermediate solution based on directional expectations.

# i.sol entails the combination of the conservative and parsimonious solutions 
# in the object ("C1P1"). This further entails easy counterfactuals ("EC"),
# diffifult counterfactuals ("DC"), and non-simplifying easy counterfactuals 
# ("NSEC"). To find out which logical remainders have been used, we need to 
# check the EC component:

sol.int3$i.sol$C1P1$EC    # Row 8 has been used as simplifying assumption

# Intermediate solution #4
# Introduce conjunctural directional expectations for C1*C3 and C2
sol.int4 <- minimize(TT, 
                     include = "?", 
                     details = TRUE, 
                     use.tilde = TRUE,
                     dir.exp = "C1*C3, C2")
sol.int4

sol.int4$i.sol$C1P1$EC  # Rows 3 and 8 have been used as simplifying 
# assumptions. This solution is identical to the parsimonious solution.

### Visualizing Results ####

# This part works with the ggplot2 package (Wickham 2016)
# Check whether it has been installed (see "Installing Packages" above)

# Read sample data
plot.data <- read.csv("plot-data.csv", row.names = 1)

# Basic XY plot with ggplot2
our.plot <- ggplot(plot.data,
                   aes(x = SOL, y = OUT)) +
  geom_point(
    fill = "dodgerblue",
    size = 7,
    shape = 21,
    color = "black",
    stroke = 1
  ) +
  theme_classic() +
  theme(panel.border = element_rect
        (
          fill = NA,
          color = "black",
          linewidth = 1,
          linetype = 1
        )) +
  scale_x_continuous(name = "Solution Term",
                     breaks = seq(0.0, 1.0, 0.1)) +
  scale_y_continuous(name = "Outcome",
                     breaks = seq(0.0, 1.0, 0.1)) +
  coord_cartesian(xlim = c(-0.02, 1.02),
                  ylim = c(-0.02, 1.02)) +
  geom_abline(intercept = c(0, 0),
              slope = 1)
our.plot

# Save plot as PDF
ggsave(
  "xyplot.pdf",
  plot = our.plot,
  path = NULL,
  scale = 1,
  width = 7,
  height = 7,
  dpi = 300
)

### XY Plot (Template) ####

# Enhanced XY plot with ggplot2

# Create subsets with unaccounted cases and deviant cases
# (In a similar way, you could also create a subset for typical cases)

Unacc <- subset(plot.data,
                OUT > 0.5 & SOL < 0.5) # Subset of unaccounted cases
Devia <- subset(plot.data,
                OUT < 0.5 & SOL > 0.5) # Subset of deviant cases

# Create triangular (polygon) shape for upper right triangle
poly <- data.frame(x = c(0.5, 0.5, 1.1),
                   y = c(0.5, 1.1, 1.1))
# Plot
our.plot2 <- ggplot(plot.data, aes(x = SOL, y = OUT)) +
  geom_polygon(
    inherit.aes = FALSE,
    aes(x = x, y = y),
    data = poly,
    fill = "gray95"
  ) +
  geom_point(
    fill = "dodgerblue",
    size = 7,
    shape = 21,
    color = "black",
    stroke = 1
  ) +
  theme_classic() +
  theme(panel.border = element_rect(
                                    fill = NA,
                                    color = "black",
                                    linewidth = 1,
                                    linetype = 1)
        ) +
  theme(
    axis.text.y = element_text(size = 18,
                               color = "black"),
    axis.title = element_text(face = "plain",
                              size = 20)
  ) +
  theme(
    axis.text.x = element_text(size = 18,
                               color = "black"),
    axis.title = element_text(face = "plain",
                              size = 20)
  ) +
  scale_x_continuous(name = "Solution Term",
                     breaks = seq(0.0, 1.0, 0.1)) +
  scale_y_continuous(name = "Outcome",
                     breaks = seq(0.0, 1.0, 0.1)) +
  coord_cartesian(xlim = c(-0.02, 1.02),
                  ylim = c(-0.02, 1.02)) +
  geom_hline(aes(yintercept = 0.5),
             color = "black",
             linetype = 5) +
  geom_vline(aes(xintercept = 0.5),
             color = "black",
             linetype = 5) +
  geom_abline(intercept = c(0, 0),
              slope = 1) +
  geom_text_repel(data = Unacc,
                  aes(label = row.names(Unacc)),
                  box.padding = 0.5,
                  size = 5) +
  geom_text_repel(data = Devia,
                  aes(label = row.names(Devia)),
                  box.padding = 0.5,
                  size = 5)
our.plot2

# Save the plot
ggsave(
  "xyplot2.pdf",
  plot = our.plot2,
  path = NULL,
  scale = 1,
  width = 7,
  height = 7,
  dpi = 300
)

# End!
#
# In case of errors, please let me know: p.a.mello@vu.nl
# https://patrickmello.com
#