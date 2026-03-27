# CFA_descriptive.R

# data
data <- readxl::read_excel("data.xlsx")
data$ID <- as.character(data$ID)
data$Group <- factor(data$Group)
responses <- paste0("Q",1:14)
data[,responses] <- lapply(data[,responses],as.numeric)

# sink("CFA_descriptive.txt")

cat("\n*** Descriptive statistics ***\n")

# n
print(table(data$Group))

# Sex
data$Sex <- factor(data$Sex)
print(table(data$Sex))

# BMI
cat("\nBMI\n")
cat("\n\tless or equal to 48\n")
print(summary(data$BMI[data$Group=="less or equal to 48"]))
print(sd(data$BMI[data$Group=="less or equal to 48"]))
cat("\n\tmore than 48\n")
print(summary(data$BMI[data$Group=="more than 48"]))
print(sd(data$BMI[data$Group=="more than 48"]))
# Schooling
cat("\nStatus\n")
data$Status <- factor(data$Status,
                         levels=c("Single",
                                  "Married",
                                  "Common-law",
                                  "Separated",
                                  "Divorced",
                                  "Widowed")
                        )
print(table(data$Group,data$Status))
# Schooling
cat("\nSchool\n")
data$Schooling <- factor(data$Schooling,
                         levels=c("Elementary",
                                  "High",
                                  "Technical",
                                  "College/University")
)
print(table(data$Group,data$Schooling))
# Working
cat("\nWorking\n")
data$Working <- factor(data$Working,
                         levels=c("Unemployed",
                                  "Medical leave",
                                  "Homemaker",
                                  "Currently working",
                                  "Retired")
)
print(table(data$Group,data$Working))
# Diabetes
cat("\nDiabetes\n")
data$Diabetes <- factor(data$Diabetes)
print(table(data$Group,data$Diabetes))
# Hypertension
cat("\nHypertension\n")
data$Hypertension <- factor(data$Hypertension)
print(table(data$Group,data$Hypertension))

# Checking
cat("\nNumber of valid answers (checking for missing data)\n")
dt <- data.frame(sapply(data,function(x){sum(!is.na(x))}))
names(dt) <- NULL
print(dt)

# Descriptive
responses <- paste0("Q",1:14)
cat("\nDescriptive statistics (psych::describe)\n")
dt <- as.data.frame(psych::describe(data[,c(responses)]))
print(dt)

# Correlation matrix
cat("\nCorrelation matrix (polycor::hetcor)\n")
Items <- data.frame(data[,responses])
r.het <- polycor::hetcor(Items,use="complete.obs")
print(round(r.het[[1]],3))

# sink()

