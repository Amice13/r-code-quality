# CFA_2nd_configural.R

# observed data
data <- readxl::read_excel("data.xlsx")
data$Group <- factor(data$Group, 
                     levels = c("less or equal to 48","more than 48"))
responses <- paste0("Q",1:14)
data[,responses] <- lapply(data[,responses],as.numeric)

filename <- NA # screen output, otherwise
# filename <- "CFA_2nd_configural" # base filename without .ext

# Second order model
model <- "
H =~ Q7+Q8+Q9
P =~ Q1+Q2+Q12+Q13
S =~ Q3+Q4+Q6+Q10+Q11
G =~ H+P+S
"
title <- paste0("Configural invariance\n(second order model)")
group <- "Group"
group.equal <- ""
bifactor <- ""

# CFA
source("CFA.R")