# CFA_unifactor_previous.R

# observed data
data <- readxl::read_excel("data.xlsx")
data$Group <- factor(data$Group, 
                     levels = c("less or equal to 48","more than 48"))
responses <- paste0("Q",1:14)
data[,responses] <- lapply(data[,responses],as.numeric)

filename <- NA # screen output, otherwise
# filename <- "CFA_unifactor_previous" # base filename without .ext

# Unifactor model
model <- "
G =~ Q1+Q2+Q3+Q4+Q5+Q6+Q7+Q8+Q9+Q10+Q11+Q12+Q13+Q14
"
title <- paste0("Unifactor model")
group <- NULL
invariance <- "preliminary"
bifactor <- ""
what <- "std" #"est"

# CFA
source("CFA.R")