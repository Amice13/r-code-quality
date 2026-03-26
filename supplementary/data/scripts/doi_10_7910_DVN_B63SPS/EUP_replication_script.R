# Joint bodies in the EU's international agreements: Delegating powers to the European Commission in EU external relations
# Markus Gastinger and Andreas Dür (University of Salzburg, Austria)
# European Union Politics, issue 22(4), 2021


# Header ------------------------------------------------------------------


rm(list=ls())
cat("\014")
dev.off()
options(max.print=100000, "scipen" = 100, warn = 1)
Sys.setenv(LANG = "en")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
path1 <- paste0(getwd(),"/")
path1

library(pacman)
p_load(MASS, stargazer, clipr, ggplot2, correlation, ltm, estimatr)


# Load data ---------------------------------------------------------------


df <- readRDS(paste0(path1, "/EUP_replication_data.RDS"))


# Calculate dependent variables -------------------------------------------


#additive index of Commission discretion in JBs
indicators <- c("representation", "no.police.patrol", "no.joint.council", "majority.decides", "no.special.committee")
df$discretion <- rowSums(df[,indicators], na.rm=TRUE)

#Latent trait analysis
r_fit <- ltm::rasch(df[,indicators], start.val = NULL, Hessian=TRUE, constraint = cbind(ncol(df[,indicators]) + 1, 1))
ra_coef <- coef(r_fit, prob = TRUE, order = TRUE)
ra_coef[,1:2]
r_fs <- ltm::factor.scores(r_fit, resp.patterns=as.matrix(df[,indicators]), method = "EB")
df$discretion_rasch <- r_fs$score.dat$z1


# Summary statistics ------------------------------------------------------


stargazer(df[,c("discretion_rasch", "discretion", "wc_dif_ln", "council.unanimity", 
                "council.policy.range", "council.commissioner.policy.distance", 
                "pa.exclusive", "ep.assent", "mixed")],
          covariate.labels = c("Discretion (Rasch)", "Discretion", "Complexity", "Unanimity", 
                               "Council conflict", "Council Commissioner distance",  
                               "Core competence", "EP assent", "Mixed agreement"),
          digits=2, type = "html")
write_last_clip() # copy-paste into HTML editor, e.g. https://htmledit.squarefree.com/


# Multivariate models ------------------------------------------------------------


#robust SEs calculated at time of exporting models below
m1 <- lm(
  discretion_rasch ~
    log(wc_dif+1) + 
    council.unanimity +
    council.policy.range +
    council.commissioner.policy.distance +
    pa.exclusive +
    ep.assent +                  
    mixed +
    treaty +
    NULL,  
  data = df)

m2 <- polr(
  factor(discretion, ordered = TRUE) ~          
    log(wc_dif+1) +             
    council.unanimity +
    council.policy.range +
    council.commissioner.policy.distance + 
    pa.exclusive +
    ep.assent +
    mixed +
    treaty +
    NULL,  
  data = df,
  Hess = TRUE)


# Export models -----------------------------------------------------------


stargazer(m1, m2, type = "html", 
          covariate.labels = c("Complexity", "Unanimity", "Council policy range", "Council Commissioner distance", "Core competences", 
                               "EP assent", "Mixed agreement"),
          dep.var.labels.include=FALSE,
          dep.var.caption = "",
          digits=2,
          digits.extra=2,
          omit=c("year", "treaty"),
          keep.stat = c("n", "adj.rsq"),
          se = starprep(m1, se_type="HC1"))  ##robust standard errors from estimatr package
write_last_clip()


# Substantive effects -----------------------------------------------------


(m1$coefficients["log(wc_dif + 1)"] * (summary(log(df$wc_dif+1))[6] - summary(log(df$wc_dif+1))[1]))/
  (summary(df$discretion_rasch)[6]-summary(df$discretion_rasch)[1])

m1$coefficients["council.unanimity"]/
  (summary(df$discretion_rasch)[6]-summary(df$discretion_rasch)[1])

(m1$coefficients["council.policy.range"] * (summary(df$council.policy.range)[6] - summary(df$council.policy.range)[1]))/
  (summary(df$discretion_rasch)[6]-summary(df$discretion_rasch)[1])


# Online appendix ----------------------------------------------------------------


#Figure A1
tmp <- as.data.frame(table(df["discretion"]))
tmp$perc <- round((tmp$Freq/(sum(tmp$Freq)))*100, digits = 1)
ggplot(data=tmp, aes(x=Var1, y=perc)) +
  geom_bar(stat="identity") +
  labs(title = "", x = "Discretion", y = "Percent") +
  theme_bw()

#Table A1
apply(df[,indicators], 2, table)

#Table A2
cor <- correlation(df[,indicators], method = "auto", ci = 0.95)
summary(cor)

#Table A5
#alternative operationalizations
m3 <- update(m1, . ~ . - council.policy.range + council.integration.range)
m4 <- update(m1, . ~ . - council.commissioner.policy.distance + council.commissioner.integration.distance)
m5 <- update(m1, . ~ . + pa.cfsp)
m6 <- update(m1, . ~ . + log(wc_noanx) + pa.association.agreement)
m7 <- update(m1, . ~ . + year - treaty)

#drop not yet ratified agreements
m8 <- update(m1, . ~ . , data=df[df$draft.decision==0,])

stargazer(m3, m4, m5, m6, m7, m8, type = "html", 
          covariate.labels = c("Complexity", "Unanimity", "Council Commissioner distance", "Council policy range", "Core competences", 
                               "EP assent", "Mixed agreement", 
                               "Council integration range", "Council Commissioner integration distance", "CFSP",  
                               "Word count", "Association agreement"),
          dep.var.labels.include=FALSE,
          dep.var.caption = "",
          digits=2,
          digits.extra=2,
          omit=c("year", "treaty"),
          keep.stat = c("n", "adj.rsq"),
          se = starprep(m3, m4, m5, m6, m7, m8, se_type="HC1"))  ##robust standard errors from estimatr package
write_last_clip()


# Session information -----------------------------------------------------


writeLines(capture.output(sessionInfo()), "sessionInfo.txt")