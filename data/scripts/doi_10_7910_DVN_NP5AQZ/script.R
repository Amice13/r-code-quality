library(psych)
library(texreg)

survey <- read.csv('factchecks_HKSMR_data.csv')

#___________________________________________________________________treatment variable
# a source variable with informative values for source effects
survey$t <- ifelse(survey$pTreatmentr1==1, '-fact', 
                   ifelse(survey$pTreatmentr2==1, '-telegraph',
                          ifelse(survey$pTreatmentr3==1, '-guardian',
                                 ifelse(survey$pTreatmentr4==1, 'control', NA))))


# order factor levels
survey$t <- factor(survey$t, levels=c("control","-fact","-guardian","-telegraph"))


#_____________________________________________________covariates/moderators
# referendum vote
survey$referendum <- as.factor(
  ifelse(
    survey$Q3b == 1, '-leave', '-remain'
  )
)

survey$referendum <- factor(survey$referendum, levels = c("-remain","-leave"))

# GE vote intention
survey$conservative <- survey$Q17c == 1

# pre-exposure to (incorrect) campaign claim
survey$exposure <- survey$Q24ar2

#__________________________________________________________dependent variables

# repeated measures of belief in claim, pre, post, followup:
survey$pre <- reverse.code(-1,survey$Q24br2)[,1]
survey$post <- reverse.code(-1,survey$Q22)[,1]

survey$w4_Q2r2 <- ifelse(
  survey$w4_Q2r2 == "Strongly disbelieve", 1, ifelse(
    survey$w4_Q2r2 == "Mostly disbelieve", 2, ifelse(
      survey$w4_Q2r2 == "Neither believe nor disbelieve", 3, ifelse(
        survey$w4_Q2r2 == "Mostly believe", 4, ifelse(
          survey$w4_Q2r2 == "Strongly believe", 5, NA
        )
      )
    )
  )
)

survey$followup <- reverse.code(-1,survey$w4_Q2r2)[,1]

# subset of respondents who filled out all waves will be used in all analyses:
survey <- survey[!is.na(survey$followup),]

#__________________________________________________________regression models

# correction effects averaged across source (Table 1)
m <- list()
m[[1]] <- lm(post ~ pre + t, data = survey)
m[[2]] <- lm(followup ~ pre + t, data = survey)

# to reproduce Table 1:
screenreg(m, stars = c(0.01, 0.05, 0.1))

# correction effects by predisposition (Table A3)
s <- list()
s[[1]] <- lm(post ~ (pre==1) * t, data = survey) # column 1
s[[2]] <- lm(post ~ (pre < 3) * t, data = survey) # column 2 and so on
s[[3]] <- lm(post ~ pre + (t * referendum), data = survey)
s[[4]] <- lm(post ~ pre + (t * conservative), data = survey)
s[[5]] <- lm(post ~ pre + (t * exposure), data = survey)

# to reproduce Table A3
order = c(1,3:5,2,6:25)
screenreg(s, stars = c(0.001 ,0.01, 0.05, 0.1),reorder.coef = order)
