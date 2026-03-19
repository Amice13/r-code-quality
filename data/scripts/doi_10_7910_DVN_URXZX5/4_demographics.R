# Demographics

# Import libraries
library(reporttools)
library(data.table)

# Define function that returns prop table
tblFun <- function(x,orderbycount){
  tbl <- table(x)
  res <- cbind(tbl,round(prop.table(tbl)*100,2))
  colnames(res) <- c('Count','Percentage')
  res <- as.data.frame(res)
  if (orderbycount) res <- setorder(res, -Count)
  res
}

# Load clean demograpics data
load("data/Demographics_Data_Clean.Rda")
demographic_data <- data
rm(data)

# Remove UUID
demographic_data <- demographic_data[-6]

## Let's create an overview of our participants!

# Where do you currently reside?
prop.table.reside <- tblFun(demographic_data$Where.do.you.currently.reside.,TRUE)
print(prop.table.reside)

# What's your gender?
prop.table.gender <- tblFun(demographic_data$What.s.your.gender.,TRUE)
print(prop.table.gender)

# What's your age?
prop.table.age <- tblFun(demographic_data$What.s.your.age.,FALSE)
print(prop.table.age)

# What is the highest level of education you have received?
prop.table.edu <- tblFun(demographic_data$What.is.the.highest.level.of.education.you.have.received.,FALSE)
print(prop.table.edu)

# Rename the column names to make sure our latex table will fit on a page
cols <- c("Survey","Country of residence","Gender","Age","Education")
colnames(demographic_data) <- cols
rm(cols)

# Display descriptive statistics for our nominal variables in a nice looking graph
tableNominal(vars = demographic_data, cap = "Demographics.", lab = "tab: nominal")
