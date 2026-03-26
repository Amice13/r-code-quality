
# Omitted variable bias example in R
# Henrique Castro Martins - hcm@iag.puc-rio.br
# If you find any mistake, please let me know


library(readxl)

data <- read_excel("data.xlsx")

# short regression in groups A and B
short <- lm(performance ~ bad_decision  , data = data)
summary(short)

# Long regression in groups A and B
long <- lm(performance ~ bad_decision  + risky_firm, data = data)
summary(long)


# The OVB is short    = long     + bias
# The OVB is 0.44535  = -0.38389 + bias
# The OVB is 0.44535  = -0.38389 + 0.82924
# The OVB is 0.44535  = -0.38389 + phi (which is omitted = f(non-omitted)) * omega (beta of omitted in long)


# Phi in omitted = f(non-omitted) * IN YOUTUBE VIDEO, THIS LINE WAS INCORRECT. NOW IT IS CORRECT.

ovbmodel <- lm(risky_firm ~bad_decision , data = data )
summary(ovbmodel)

# The OVB is 0.44535  = -0.38389 + 1.25146 * 0.66262


matrix1<- summary(long)$coefficients
matrix2<- summary(ovbmodel)$coefficients

# Calculating OVB
sum(matrix1[3,1] * matrix2[2,1]) 
    

#summary
tapply(data$performance, data$risky_firm, summary)

tapply(data$bad_decision, data$risky_firm, summary)





