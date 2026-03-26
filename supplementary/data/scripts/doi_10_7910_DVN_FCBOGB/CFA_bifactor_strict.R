# CFA_bifactor_strict.R

# observed data
data <- readxl::read_excel("data.xlsx")
data$Group <- factor(data$Group, 
                     levels = c("less or equal to 48","more than 48"))
responses <- paste0("Q",1:14)
data[,responses] <- lapply(data[,responses],as.numeric)

filename <- NA # screen output, otherwise
# filename <- "CFA_bifactor_strict" # base filename without .ext

# Bifactor model
model <- "
G =~ Q1 + Q2 + Q7 + Q8 + Q9 + Q12 + Q13 +  Q3 + Q4 + Q6 + Q10 + Q11 + Q14
P =~ Q1 + Q2 + Q12 + Q13
S =~ Q3 + Q4 + Q6 + Q10 + Q11 + Q14
H =~ Q7 + Q8 + Q9
H ~~ 0*P
H ~~ 0*S
P ~~ 0*S
P ~~ 0*G
S ~~ 0*G
H ~~ 0*G
"
title <- paste0("Strict invariance\n(bifactor model without Q5)")
group <- "Group"
invariance <- "strict"
bifactor <- "G"
what <- "std" # "std" or "est"

# CFA
source("CFA.R")
