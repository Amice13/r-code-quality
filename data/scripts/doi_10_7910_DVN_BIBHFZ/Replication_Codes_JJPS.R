#######################################
#title: "Replication_Codes_JJPS
#subtitle: "Do FIFA World Cup Matches Affect Outgroup Bias? Evidence from South Korea"
#authors: "Jong Hee Park, Anakiz Elif Senturk"
#date: 14/07/2024
################################################

#Clear the workspace and source helper functions
#P.S. you may change the file path according to you directory, if needed.

rm(list=ls())

##############################
#Helper functions
##############################

difftest_lm <- function(model1, model2, term.choice = "Outgroup", df = length(ict.results.R0[[5]]$y)){
  diffest <- model2%>%filter(term==term.choice) %>% pull(estimate)-
    model1%>%filter(term==term.choice) %>% pull(estimate)
  vardiff <- (model2%>%filter(term==term.choice) %>% pull('std.error'))^2 + 
    (model1%>%filter(term==term.choice) %>% pull('std.error'))^2 
  ## variance of x1 + variance of x2 - 2*covariance of x1 and x2
  diffse <- sqrt(vardiff)
  tdiff <- (diffest)/(diffse)
  ptdiff <- 2*(1-pt(abs(tdiff), df, lower.tail=T))
  upr <- diffest + qt(.95, df = df)*diffse # will usually be very close to 1.96
  lwr <- diffest + qt(.05, df = df)*diffse
  return(data.frame(est=round(diffest, digits =2), 
                    t=round(tdiff, digits = 2), 
                    p=round(ptdiff, digits = 4), 
                    lwr=round(lwr, digits = 2), 
                    upr=round(upr, digits = 2),
                    df = df))
}

ate.compute <- function(y, treatment, group=NULL, 
                        robust=TRUE, cluster=FALSE, x=NULL){
  ## n.treat = length(treat)
  ## n.control = length(control)
  ## if(regression){
  small.clean <- data.frame(y = y,
                            treatment = treatment)            
  if(is.null(x)){
    ## small <- cbind(treat, control)
    fit <- lm(y ~ treatment, data=small.clean)
  }else{
    x <- as.matrix(x)
    small.clean <- cbind(small.clean, x)
    colnames(small.clean) <- c("y", "treatmen", colnames(x))      
    fit <- lm(y ~ ., data=small.clean)
  }
  if(robust){
    out <- lmtest::coeftest(fit, df = fit$df,
                            vcov = vcovHC(fit, type = "HC2"))
  }else if(cluster){
    out <- lmtest::coeftest(fit, vcov=vcovHC(fit, type="HC0", cluster=group))
    
  }else{
    out <- lmtest::coeftest(fit, df = fit$df) 
  }
  return(out)
}

data_summary <- function(data, varname, groupnames, title=""){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE)/sqrt(length(x)))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  colnames(data_sum) <- c("gb",           "y", "sd"   )
  ## Default bar plot
  g <- ggplot(data_sum, aes(x=gb, y=y, fill=as.factor(gb))) + 
    geom_bar(stat="identity", color="black", 
             position=position_dodge()) +
    geom_errorbar(aes(ymin=y-sd, ymax=y+sd), width=.2,
                  position=position_dodge(.9)) +
    labs(title=title, x="", y = "", fill="Round")+
    theme_classic()
  print(g)
  return(list(data_sum, g))
}

### Create a Summary Data Frame for Model Coefficients and Statistics
model.maker <- function(input){
    data <- rbind(cbind(coef(input)[2], sqrt(diag(vcov(input))[2])),
                     cbind(coef(input)[1], sqrt(diag(vcov(input))[1])))
    data.frame("term" = c("Control", "Outgroup"),
               "estimate" = data[, 1],
               "std.error" = data[, 2],
               "statistic" = data[, 1]/data[, 2],
               "p.value" = pt(data[, 1]/data[, 2], df = c(length(input$y), length(input$y)), lower.tail=FALSE), 
               "model" = c("Control", "Outgroup"))%>% as_tibble()
}

### Load libraries
library(tidyverse)
library(haven)
library(sjlabelled)
library(labelled) 
library(surveytoolbox) # install with devtools::install_github("martinctc/surveytoolbox")
library(dplyr)
library(list)
library(dotwhisker)
require(gtsummary)
require(stargazer)
require(NetworkChange)
require(lmtest)
require(tidyr)
library(broom)
library(sandwich)
require(xtable)

select <- dplyr::select
filter <- dplyr::filter
mutate <- dplyr::mutate
summarise <- dplyr::summarise
group_by <- dplyr::group_by
ungroup <- dplyr::ungroup
arrange <- dplyr::arrange
rename <- dplyr::rename

### Data Set Creation
# Importing main experimental data set with outcome
df <- readRDS("replication_data.rds")

#Other alternatives
#df <- readxl::read_xlsx("main_dataset.xlsx")
#df <- read.csv("main_dataset.csv")

################################################
## Table 1
################################################
df <- df %>%
  mutate(
    ideology = case_when(
      political_ideology > 3 ~ "conservative",
      political_ideology < 3 ~ "liberal",
      political_ideology == 3 ~ "moderate"
    ))

summary_table <-
  df %>%
  dplyr::select(sex, age.cat, residence7, 
                ideology, 
                education, gb) %>%
  gtsummary::tbl_strata(
                 strata = gb,
                 .tbl_fun =
                     ~ .x %>%
                         tbl_summary() %>% ## , missing = "no") %>% by = source
                         add_n(),
                 .header = "**{strata}**, N = {n}"
             )
summary_table
####################################################
## List Experiment Data Cleanup
####################################################
#Prepare Data and Define Treatment Variables
df.A <- df %>%
    mutate(treat = ifelse(gb==1, 0, 1)) %>%
    ## treatment 1: south american
    mutate(y1 = ifelse(!is.na(y_listexp_control), y_listexp_control,
                ifelse(!is.na(y_stereotype_samerican), y_stereotype_samerican,
                       NA))) %>%
    mutate(treatment1 = ifelse(!is.na(y_listexp_control), 0,
                        ifelse(!is.na(y_stereotype_samerican), 1,
                               NA)))%>%
    ## treatment 2: african
    mutate(y2 = ifelse(!is.na(y_listexp_control), y_listexp_control,
                ifelse(!is.na(y_stereotype_african), y_stereotype_african,
                       NA))) %>%
    mutate(treatment2 = ifelse(!is.na(y_listexp_control), 0,
                        ifelse(!is.na(y_stereotype_african), 1,
                               NA)))%>%
    
    ## treatment 3: white
    mutate(y3 = ifelse(!is.na(y_listexp_control), y_listexp_control,
                ifelse(!is.na(y_stereotype_white), y_stereotype_white,
                       NA))) %>%
    mutate(treatment3 = ifelse(!is.na(y_listexp_control), 0,
                        ifelse(!is.na(y_stereotype_white), 1,
                               NA)))%>%

    ## treatment: In-group bias
    mutate(y.ingroup = ifelse(!is.na(y_listexp_control), y_listexp_control,
                ifelse(!is.na(y_stereotype_korean), y_stereotype_korean,
                       NA))) %>%
    mutate(treatment.ingroup = ifelse(!is.na(y_listexp_control), 0,
                        ifelse(!is.na(y_stereotype_korean), 1,
                               NA)))%>%
    
    ## treatment: outgroup bias
    mutate(y.binary = ifelse(!is.na(y_listexp_control), y_listexp_control,
               ifelse(!is.na(y_stereotype_samerican), y_stereotype_samerican,
               ifelse(!is.na(y_stereotype_african), y_stereotype_african,
               ifelse(!is.na(y_stereotype_white), y_stereotype_white,
                      NA))))) %>%
    mutate(treatment.binary = ifelse(!is.na(y_listexp_control), 0,
                              ifelse(!is.na(y_stereotype_samerican), 1,
                              ifelse(!is.na(y_stereotype_african), 1,
                              ifelse(!is.na(y_stereotype_white), 1,
                                     NA)))))

################################
## Double Differences
################################
df.A0 <- data.frame(df.A %>% 
                    mutate(fifa = ifelse(gb > 1, 1, 0)))

df.A1 <- data.frame(df.A %>% filter(gb==1| gb == 2) %>%
                    mutate(fifa = ifelse(gb ==2, 1, 0)))

df.A2 <- data.frame(df.A %>% filter(gb==1| gb == 3) %>%
                    mutate(fifa = ifelse(gb ==3, 1, 0)))

df.A3 <- data.frame(df.A %>% filter(gb==1| gb == 4) %>%
                    mutate(fifa = ifelse(gb ==4, 1, 0)))

treat.report <- function(lm.obj, conf.level = 0.95){
    (m0 <- coeftest(lm.obj, vcov = vcovHC(lm.obj, type = "HC1")))
    out <- tidy(m0, conf.int = TRUE, conf.level = conf.level) %>% filter(grepl("fifa:", term)) %>%
        select(estimate, std.error, conf.low, conf.high)
    return(out)
}
treat.report.pre <- function(lm.obj, conf.level = 0.95){
    (m0 <- coeftest(lm.obj, vcov = vcovHC(lm.obj, type = "HC1")))
    out <- tidy(m0, conf.int = TRUE, conf.level = conf.level) %>% filter(grepl("treatment", term)) %>%
        select(estimate, std.error, conf.low, conf.high)
    return(out)
}

################################################
### Creating Table 4
#Calculte changes in bias toward referent outgroups and ingroup
################################################
#outgroup
## post match effect: outgroup
m1 <- lm(y1 ~ fifa*treatment1, df.A1); treat.report.pre(m1)
m2 <- lm(y2 ~ fifa*treatment2, df.A2); treat.report.pre(m2)
m3 <- lm(y3 ~ fifa*treatment3, df.A3); treat.report.pre(m3)

## post-game treatment effect: outgroup
m1.a <- lm(y1 ~ treatment1, df.A%>% filter(gb==2)); treat.report.pre(m1.a)
m2.a <- lm(y2 ~ treatment2, df.A%>% filter(gb==3)); treat.report.pre(m2.a)
m3.a <- lm(y3 ~ treatment3, df.A%>% filter(gb==4)); treat.report.pre(m3.a)

## Generate report table for outgroup
out <- rbind(cbind(treat.report.pre(m1.a)[, 1:2], treat.report.pre(m1)[1,1:2], treat.report.pre(m1)[2,]),
             cbind(treat.report.pre(m2.a)[, 1:2], treat.report.pre(m2)[1,1:2], treat.report.pre(m2)[2,]),
             cbind(treat.report.pre(m3.a)[, 1:2], treat.report.pre(m3)[1,1:2], treat.report.pre(m3)[2,]))

rownames(out) <- c("R1", "R2", "R3")
colnames(out) <- c("Est-Post", "SE-Post", "Est-Pre", "SE-Pre", "Est-Diff", "SE-Diff", "Lower", "Upper")

print(xtable(out, digits=3), 
      file="Table4top.tex")


################################################
## Ingroup
################################################
## post match effect: ingroup
m1.ingroup <- lm(y.ingroup ~ fifa*treatment.ingroup, df.A1); treat.report.pre(m1.ingroup)
m2.ingroup <- lm(y.ingroup ~ fifa*treatment.ingroup, df.A2); treat.report.pre(m2.ingroup)
m3.ingroup <- lm(y.ingroup ~ fifa*treatment.ingroup, df.A3); treat.report.pre(m3.ingroup)
## pre-game treatment effect: outgroup
m1.a.ingroup <- lm(y.ingroup ~ treatment.ingroup, df.A%>% filter(gb==2)); treat.report.pre(m1.a.ingroup)
m2.a.ingroup <- lm(y.ingroup ~ treatment.ingroup, df.A%>% filter(gb==3)); treat.report.pre(m2.a.ingroup)
m3.a.ingroup <- lm(y.ingroup ~ treatment.ingroup, df.A%>% filter(gb==4)); treat.report.pre(m3.a.ingroup)

## Generate report table for ingroup
out <- rbind(cbind(treat.report.pre(m1.a.ingroup)[, 1:2], treat.report.pre(m1.ingroup)[1,1:2], treat.report.pre(m1.ingroup)[2,]),
             cbind(treat.report.pre(m2.a.ingroup)[, 1:2], treat.report.pre(m2.ingroup)[1,1:2], treat.report.pre(m2.ingroup)[2,]),
             cbind(treat.report.pre(m3.a.ingroup)[, 1:2], treat.report.pre(m3.ingroup)[1,1:2], treat.report.pre(m3.ingroup)[2,]))

rownames(out) <- c("R1", "R2", "R3")
colnames(out) <- c("Est-Post", "SE-Post", "Est-Pre", "SE-Pre", "Est-Diff", "SE-Diff", "Lower", "Upper")

print(xtable(out, digits=3), 
      file="Table4bottom.tex")

################################################
### Creating Table 5
## Calculte Changes in bias toward non-referent outgroups
## treatment = South America
## non-referent = Africa, Europe
################################################
## post match placebo effect: outgroup
## placebo for R1
m11 <- lm(y2 ~ fifa*treatment2, df.A1); treat.report.pre(m11)
m12 <- lm(y3 ~ fifa*treatment3, df.A1); treat.report.pre(m12)

## placebo for R2
m21 <- lm(y1 ~ fifa*treatment1, df.A2); treat.report.pre(m21)
m22 <- lm(y3 ~ fifa*treatment3, df.A2); treat.report.pre(m22)

## placebo for R3
m31 <- lm(y1 ~ fifa*treatment1, df.A3); treat.report.pre(m31)
m32 <- lm(y2 ~ fifa*treatment2, df.A3); treat.report.pre(m32)

## pre-game treatment effect: outgroup
m11.a <- lm(y2 ~ treatment2, df.A%>% filter(gb==2)); treat.report.pre(m11.a)
m12.a <- lm(y3 ~ treatment3, df.A%>% filter(gb==2)); treat.report.pre(m12.a)

m21.a <- lm(y1 ~ treatment1, df.A%>% filter(gb==3)); treat.report.pre(m21.a)
m22.a <- lm(y3 ~ treatment3, df.A%>% filter(gb==3)); treat.report.pre(m22.a)

m31.a <- lm(y1 ~ treatment1, df.A%>% filter(gb==4)); treat.report.pre(m31.a)
m32.a <- lm(y2 ~ treatment2, df.A%>% filter(gb==4)); treat.report.pre(m32.a)

## report
out <- rbind(cbind(treat.report.pre(m11.a)[, 1:2], treat.report.pre(m11)[1,1:2], treat.report.pre(m11)[2,]),
             cbind(treat.report.pre(m21.a)[, 1:2], treat.report.pre(m21)[1,1:2], treat.report.pre(m21)[2,]),
             cbind(treat.report.pre(m31.a)[, 1:2], treat.report.pre(m31)[1,1:2], treat.report.pre(m31)[2,]),
             cbind(treat.report.pre(m12.a)[, 1:2], treat.report.pre(m12)[1,1:2], treat.report.pre(m12)[2,]),
             cbind(treat.report.pre(m22.a)[, 1:2], treat.report.pre(m22)[1,1:2], treat.report.pre(m22)[2,]),
             cbind(treat.report.pre(m32.a)[, 1:2], treat.report.pre(m32)[1,1:2], treat.report.pre(m32)[2,]))


colnames(out) <- c("Est-Post", "SE-Post", "Est-Pre", "SE-Pre", "Est-Diff", "SE-Diff", "Lower", "Upper")

print(xtable(out, digits=3), 
      file="Table5.tex")

################################################
## Table 6
## Calculte triple difference estimates of gender gap in referent outgroup
################################################
## post match effect: outgroup
m1 <- lm(y1 ~ fifa*treatment1*sex, df.A1); treat.report.pre(m1, conf.level = 0.90)
m2 <- lm(y2 ~ fifa*treatment2*sex, df.A2); treat.report.pre(m2, conf.level = 0.90)
m3 <- lm(y3 ~ fifa*treatment3*sex, df.A3); treat.report.pre(m3, conf.level = 0.90)

## post match effect: ingroup
m11 <- lm(y.ingroup ~ fifa*treatment.ingroup*sex, df.A1); treat.report.pre(m1, conf.level = 0.90)
m21 <- lm(y.ingroup ~ fifa*treatment.ingroup*sex, df.A2); treat.report.pre(m2, conf.level = 0.90)
m31 <- lm(y.ingroup ~ fifa*treatment.ingroup*sex, df.A3); treat.report.pre(m3, conf.level = 0.90)


## report
out <- rbind(cbind(treat.report.pre(m1, conf.level = 0.90)[4,],
                   treat.report.pre(m2, conf.level = 0.90)[4,],
                   treat.report.pre(m3, conf.level = 0.90)[4,]),
             cbind(treat.report.pre(m11, conf.level = 0.90)[4,],
                   treat.report.pre(m21, conf.level = 0.90)[4,],
                   treat.report.pre(m31, conf.level = 0.90)[4,]))


rownames(out) <- c("Outgroup", "Ingroup")
colnames(out) <- rep(c("Est-Diff", "SE-Diff", "Lower", "Upper"), 3)

print(xtable(out, digits=3), 
      file="Table6top_90.tex")


################################################
## triple difference of gender for non-referent group
################################################
## post match effect: outgroup
m11 <- lm(y1 ~ fifa*treatment1*sex, df.A2); treat.report.pre(m11)
m12 <- lm(y1 ~ fifa*treatment1*sex, df.A3); treat.report.pre(m12)

m21 <- lm(y2 ~ fifa*treatment2*sex, df.A1); treat.report.pre(m21)
m22 <- lm(y2 ~ fifa*treatment2*sex, df.A3); treat.report.pre(m22)

m31 <- lm(y3 ~ fifa*treatment3*sex, df.A1); treat.report.pre(m31)
m32 <- lm(y3 ~ fifa*treatment3*sex, df.A2); treat.report.pre(m32)

## report
out <- cbind(rbind(treat.report.pre(m21, conf.level = 0.90)[4,], treat.report.pre(m31, conf.level = 0.90)[4,]),
             rbind(treat.report.pre(m11, conf.level = 0.90)[4,], treat.report.pre(m32, conf.level = 0.90)[4,]),
             rbind(treat.report.pre(m12, conf.level = 0.90)[4,], treat.report.pre(m22, conf.level = 0.90)[4,]))

rownames(out) <- c("Non-referent Outgroup 1", "Non-referent Outgroup 2")
colnames(out) <- rep(c("Est-Diff", "SE-Diff", "Lower", "Upper"), 3)

print(xtable(out, digits=3), 
      file="Table6bottom_90.tex")

############################################
## Table 7 
# Checking for the Ceiling and Floor Effects
############################################
# Preprocess the data
# Create new binary variables for age, political ideology, gender, and education
df.A$age.young <- ifelse(df.A$age.cat < 4, 1, 0)
df.A$conservative <- ifelse(df.A$political_ideology>3, 1, 0)
df.A$male <- ifelse(df.A$sex =="Male", 1, 0)
df.A$college <- ifelse(df.A$education >2, 1, 0)
df.A$treat <- df.A$treatment.binary == 1

# Select relevant columns and drop missing values
df.A1 <- df.A %>% dplyr::select(y.binary, male, college, age.young, conservative, treatment.binary, gb) %>% drop_na()

# Convert the data frame to a standard format
df.A1 <- data.frame(df.A1)


# Fit standard design ML model (No Boundary Effects)
# Replicates Table 7 Columns 1-2 in Blair and Imai (2010)
noboundary.results <- ictreg(y.binary ~ male + college + age.young + conservative, 
                             data = df.A1, 
                             treat = "treatment.binary", J=5, method = "ml")
# Display the summary of the no boundary effects model
summary(noboundary.results)

# Fit standard design ML model with ceiling effects
# Replicates Table 7 Columns 3-4 in Blair and Imai (2010)
ceiling.results <- ictreg(y.binary ~ male + college + age.young + conservative, 
                          data = df.A1, 
                          treat = "treatment.binary", J=5, method = "ml", ## ffit.start = "nls",
                          ceiling = TRUE, ceiling.fit = "bayesglm",
                          ceiling.formula = ~ male + college + age.young + conservative)

# Display the summary of the ceiling effects model with boundary proportions
summary(ceiling.results, boundary.proportions=TRUE)

# Fit standard design ML model with floor effects
# Replicates Table 7 Columns 5-6 in Blair and Imai (2010)

floor.results <- ictreg(y.binary ~ male + college + age.young + conservative, 
                        data = df.A1, 
                        treat = "treatment.binary", J=5, method = "ml", ## fit.start = "nls",
                        floor = TRUE, floor.fit = "bayesglm",
                        floor.formula = ~ male + college + age.young + conservative)

# Display the summary of the floor effects model with boundary proportions
summary(floor.results, boundary.proportions=TRUE)

# Fit standard design ML model with both floor and ceiling effects
# Replicates Table 7 Columns 7-8 in Blair and Imai (2010)
both.results <- ictreg(y.binary ~ male + college + age.young + conservative, 
                       data = df.A1, 
                       treat = "treatment.binary", J=5, method = "ml", ## ffit.start = "nls",
                       floor = TRUE, ceiling = TRUE, 
                       floor.fit = "bayesglm", ceiling.fit = "bayesglm",
                       floor.formula = ~ male + college + age.young + conservative,
                       ceiling.formula = ~ male + college + age.young + conservative)

# Display the summary of the model with both floor and ceiling effects and boundary proportions
summary(both.results, boundary.proportions=TRUE)

# Helper function to extract coefficients and standard errors from ictreg results
ict.table.maker <- function(input){
  res <- cbind(input$par.treat, input$se.treat)
  colnames(res) <- c("coef", "se")
  return(res)
}

# Extract results from each model
ict_model.R0 <- ict.table.maker(noboundary.results)
ict_model.R1 <- ict.table.maker(ceiling.results)
ict_model.R2 <- ict.table.maker(floor.results)
ict_model.R3 <- ict.table.maker(both.results)

# Combine results into a single table
ict.all <- bind_cols(ict_model.R0,
                     ict_model.R1,
                     ict_model.R2,
                     ict_model.R3) 

# Save the results table as a text file
sink("Table7.txt")
print(xtable(ict.all, digits=3))
sink()

################################################
## Supporting Information Document
################################################

################################################
## Creating the Table 4 and Figure 1 in Supporting Information Document
## Balance check across treatment conditions
################################################
xtable(cbind(matrix(c(summary(m1.out[[1]])[[3]][1,4], summary(m1.out[[1]])[[4]][1,4],
                      summary(m1.out[[2]])[[3]][1,4], summary(m1.out[[2]])[[4]][1,4],
                      summary(m1.out[[3]])[[3]][1,4], summary(m1.out[[3]])[[4]][1,4],
                      summary(m1.out[[4]])[[3]][1,4], summary(m1.out[[4]])[[4]][1,4]), 4, 2, byrow=TRUE),

             matrix(c(summary(m2.out[[1]])[[3]][1,4], summary(m2.out[[1]])[[4]][1,4],
                      summary(m2.out[[2]])[[3]][1,4], summary(m2.out[[2]])[[4]][1,4],
                      summary(m2.out[[3]])[[3]][1,4], summary(m2.out[[3]])[[4]][1,4],
                      summary(m2.out[[4]])[[3]][1,4], summary(m2.out[[4]])[[4]][1,4]), 4, 2, byrow=TRUE),

             matrix(c(summary(m3.out[[1]])[[3]][1,4], summary(m3.out[[1]])[[4]][1,4],
                      summary(m3.out[[2]])[[3]][1,4], summary(m3.out[[2]])[[4]][1,4],
                      summary(m3.out[[3]])[[3]][1,4], summary(m3.out[[3]])[[4]][1,4],
                      summary(m3.out[[4]])[[3]][1,4], summary(m3.out[[4]])[[4]][1,4]), 4, 2, byrow=TRUE)), 3)


g1 <- love.plot(m.out[[1]], binary = "std", title="Pre-game")
g2 <- love.plot(m.out[[2]], binary = "std", title="Round 1 (draw)")
g3 <- love.plot(m.out[[3]], binary = "std", title="Round 2 (loss)")
g4 <- love.plot(m.out[[4]], binary = "std", title="Round 3 (win)")

pdf("SI_Figure1.pdf", family="sans",
    width     = 12, height    =10)
NetworkChange:::multiplot(g1, g2, g3, g4, cols=2)
dev.off()

## Creating Table 5 and 6 (Subgroup analysis)
################################################
sub.analysis <- function(df.sub){
    df.sub0 <- data.frame(df.sub %>% 
                        mutate(fifa = ifelse(gb > 1, 1, 0)))

    df.sub1 <- data.frame(df.sub %>% filter(gb==1| gb == 2) %>%
                        mutate(fifa = ifelse(gb ==2, 1, 0)))

    df.sub2 <- data.frame(df.sub %>% filter(gb==1| gb == 3) %>%
                        mutate(fifa = ifelse(gb ==3, 1, 0)))

    df.sub3 <- data.frame(df.sub %>% filter(gb==1| gb == 4) %>%
                        mutate(fifa = ifelse(gb ==4, 1, 0)))

    ## outgroup
    m1 <- lm(y1 ~ fifa*treatment1, df.sub1); treat.report.pre(m1)
    m2 <- lm(y2 ~ fifa*treatment2, df.sub2); treat.report.pre(m2)
    m3 <- lm(y3 ~ fifa*treatment3, df.sub3); treat.report.pre(m3)
    ## pre-game treatment effect: outgroup
    m1.a <- lm(y1 ~ treatment1, df.sub%>% filter(gb==2)); treat.report.pre(m1.a)
    m2.a <- lm(y2 ~ treatment2, df.sub%>% filter(gb==3)); treat.report.pre(m2.a)
    m3.a <- lm(y3 ~ treatment3, df.sub%>% filter(gb==4)); treat.report.pre(m3.a)

    ## report
    out1 <- rbind(cbind(treat.report.pre(m1.a)[, 1:2], treat.report.pre(m1)[1,1:2], treat.report.pre(m1)[2,]),
                 cbind(treat.report.pre(m2.a)[, 1:2], treat.report.pre(m2)[1,1:2], treat.report.pre(m2)[2,]),
                 cbind(treat.report.pre(m3.a)[, 1:2], treat.report.pre(m3)[1,1:2], treat.report.pre(m3)[2,]))

    rownames(out1) <- c("R1", "R2", "R3")
    colnames(out1) <- c("Est-Post", "SE-Post", "Est-Pre", "SE-Pre", "Est-Diff", "SE-Diff", "Lower", "Upper")


    ## ingroup
    m1.ingroup <- lm(y.ingroup ~ fifa*treatment.ingroup, df.sub1); treat.report.pre(m1.ingroup)
    m2.ingroup <- lm(y.ingroup ~ fifa*treatment.ingroup, df.sub2); treat.report.pre(m2.ingroup)
    m3.ingroup <- lm(y.ingroup ~ fifa*treatment.ingroup, df.sub3); treat.report.pre(m3.ingroup)
    ## pre-game treatment effect: outgroup
    m1.a.ingroup <- lm(y.ingroup ~ treatment.ingroup, df.sub%>% filter(gb==2)); treat.report.pre(m1.a.ingroup)
    m2.a.ingroup <- lm(y.ingroup ~ treatment.ingroup, df.sub%>% filter(gb==3)); treat.report.pre(m2.a.ingroup)
    m3.a.ingroup <- lm(y.ingroup ~ treatment.ingroup, df.sub%>% filter(gb==4)); treat.report.pre(m3.a.ingroup)

    ## report
    out2 <- rbind(cbind(treat.report.pre(m1.a.ingroup)[, 1:2], treat.report.pre(m1.ingroup)[1,1:2], treat.report.pre(m1.ingroup)[2,]),
                 cbind(treat.report.pre(m2.a.ingroup)[, 1:2], treat.report.pre(m2.ingroup)[1,1:2], treat.report.pre(m2.ingroup)[2,]),
                 cbind(treat.report.pre(m3.a.ingroup)[, 1:2], treat.report.pre(m3.ingroup)[1,1:2], treat.report.pre(m3.ingroup)[2,]))

    rownames(out2) <- c("R1", "R2", "R3")
    colnames(out2) <- c("Est-Post", "SE-Post", "Est-Pre", "SE-Pre", "Est-Diff", "SE-Diff", "Lower", "Upper")

    ## placebo
    ## post match placebo effect: outgroup
    ## placebo for R1
    m11 <- lm(y2 ~ fifa*treatment2, df.sub1); treat.report.pre(m11)
    m12 <- lm(y3 ~ fifa*treatment3, df.sub1); treat.report.pre(m12)

    ## placebo for R2
    m21 <- lm(y1 ~ fifa*treatment1, df.sub2); treat.report.pre(m21)
    m22 <- lm(y3 ~ fifa*treatment3, df.sub2); treat.report.pre(m22)

    ## placebo for R3
    m31 <- lm(y1 ~ fifa*treatment1, df.sub3); treat.report.pre(m31)
    m32 <- lm(y2 ~ fifa*treatment2, df.sub3); treat.report.pre(m32)

    ## pre-game treatment effect: outgroup
    m11.a <- lm(y2 ~ treatment2, df.sub%>% filter(gb==2)); treat.report.pre(m11.a)
    m12.a <- lm(y3 ~ treatment3, df.sub%>% filter(gb==2)); treat.report.pre(m12.a)

    m21.a <- lm(y1 ~ treatment1, df.sub%>% filter(gb==3)); treat.report.pre(m21.a)
    m22.a <- lm(y3 ~ treatment3, df.sub%>% filter(gb==3)); treat.report.pre(m22.a)

    m31.a <- lm(y1 ~ treatment1, df.sub%>% filter(gb==4)); treat.report.pre(m31.a)
    m32.a <- lm(y2 ~ treatment2, df.sub%>% filter(gb==4)); treat.report.pre(m32.a)

    ## report
    out3 <- rbind(cbind(treat.report.pre(m11.a)[, 1:2], treat.report.pre(m11)[1,1:2], treat.report.pre(m11)[2,],
                       treat.report.pre(m12.a)[, 1:2], treat.report.pre(m12)[1,1:2], treat.report.pre(m12)[2,]),
                 cbind(treat.report.pre(m21.a)[, 1:2], treat.report.pre(m21)[1,1:2], treat.report.pre(m21)[2,],
                       treat.report.pre(m22.a)[, 1:2], treat.report.pre(m22)[1,1:2], treat.report.pre(m22)[2,]),
                 cbind(treat.report.pre(m31.a)[, 1:2], treat.report.pre(m31)[1,1:2], treat.report.pre(m31)[2,],
                       treat.report.pre(m32.a)[, 1:2], treat.report.pre(m32)[1,1:2], treat.report.pre(m32)[2,]))


    rownames(out3) <- c("R1", "R2", "R3")
    colnames(out3) <- c("Est-Post", "SE-Post", "Est-Pre", "SE-Pre", "Est-Diff", "SE-Diff", "Lower", "Upper",
                       "Est-Post", "SE-Post", "Est-Pre", "SE-Pre", "Est-Diff", "SE-Diff", "Lower", "Upper")

    return(list(out1, out2, out3))
}

## ideology
## liberal
df.liberal <- df.A %>% filter(political_ideology<3)
## Moderate
df.moderate <- df.A %>% filter(political_ideology==3)
## conservative
df.conservative <- df.A %>% filter(political_ideology>3)
## young
df.young <- df.A %>% filter(age.cat < 4)
## Old
df.old <- df.A %>% filter(age.cat > 3)


## analysis
out.liberal <- sub.analysis(df.liberal)
out.moderate <- sub.analysis(df.moderate)
out.conservative <- sub.analysis(df.conservative)
out.young <- sub.analysis(df.young)
out.old <- sub.analysis(df.old)


## ideology
print(xtable(cbind(out.liberal[[1]], out.moderate[[1]], out.conservative[[1]]), digits=3),  
      file="double_jjps_revision_outgroup_table_ideology.tex")
print(xtable(cbind(out.liberal[[2]], out.moderate[[2]], out.conservative[[2]]), digits=3),  
      file="double_jjps_revision_ingroup_table_ideology.tex")
print(xtable(cbind(out.liberal[[3]], out.moderate[[3]], out.conservative[[2]]), digits=3),  
      file="placebo_jjps_revision_ingroup_table_ideology.tex")

## age
print(xtable(cbind(out.young[[1]], out.old[[1]]), digits=3),  
      file="double_jjps_revision_outgroup_table_age.tex")
print(xtable(cbind(out.young[[2]], out.old[[2]]), digits=3),  
      file="double_jjps_revision_ingroup_table_age.tex")
print(xtable(cbind(out.young[[3]], out.old[[3]]), digits=3),  
      file="placebo_jjps_revision_ingroup_table_age.tex")

