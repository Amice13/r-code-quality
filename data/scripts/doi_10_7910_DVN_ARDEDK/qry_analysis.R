library(stargazer)
library(lme4)

data <- read.csv("qry_data.csv")

m1 <- lmer(
    "word_Female ~ is_Male + is_Black + is_Male*is_Black + (1|user_no)", data=data,
)
m2 <- lmer(
    "word_Male ~ is_Male + is_Black + is_Male*is_Black + (1|user_no)", data=data, 
)
m3 <- lmer(
    "word_Black ~ is_Male + is_Black + is_Male*is_Black + (1|user_no)", data=data,
)

table <- stargazer(
    m1, m2, m3,     
    align=TRUE, 
    no.space=TRUE, 
    omit.stat=c("LL", "AIC", "BIC"),
    covariate.labels=c("Male", "Black", "Male:Black"),
    dep.var.labels=c("'Female'", "'Male'", "'Black'"),
    star.cutoffs = c(0.05, 0.01, 0.001)
)
write(table, "tables/word_regressions.tex")

print(length(unique(data$user_id)))
