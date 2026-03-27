# Title -------------------------------------------------------------------

# Replication script for "How to distinguish human error from election fraud: Evidence from the 2019 Malawi election"   
# Authors Johan Ahlback and Ryan Jablonski               


# Description -------------------------------------------------------------

# Use this script to replicate tables and figures presented in section 4 and the online appendix

# Section 4 includes the following:

# Descriptive statistics on result-sheet edits, presented in table S1 and figure 2
# Correlation between incorrect result-sheet and edits in table S2
# Correlation between edits and other covariates in table S3

# Prerequisites -----------------------------------------------------------
# run codes in data_preparation.R prior to running the codes below

################################################################################

# Table S1: Descriptive statistics of result-sheet irregularities  --------
# This code generates table S1 that provides descriptive data of edits and other irregularities

ed1 <- data.frame(edits$pres_alt, edits$combine, edits$pres_inc,edits$parl_alt, edits$lc_alt, edits$pres_alt2014)
stargazer(ed1, type = "latex", digits = 3, 
          title = "Descriptive statistics of result-sheet irregularities in 2019 and 2014",
          covariate.labels = c("Edits in presidential results (2019", 
                               "Number of rows edited in presidential result-sheet (2019)", 
                               "Incorrect presidential result-sheet (2019)", 
                               "Edits in parliamentary result-sheet (2019)", 
                               "Edits in local government result-sheet (2019)", 
                               "Edits in presidential result-sheet (2014)"))


# Figure 2: Distribution of edits in the 2019 presidential election -------
# This code generates the bar-graphs presented in figure 2 

mytab1 <- prop.table(table(edits$combine))
my.df1 <- data.frame(mytab1)
names(my.df1) <- c("rows", "edits")
my.df1$edits <- my.df1$edits*100

figure2a <- ggplot(my.df1, aes(x=fct_rev(rows), y=edits)) + 
  geom_bar(stat = "identity", fill="darkblue") +
  ylim(0,30) +
  xlab("Number of rows") +
  ylab("Result-sheets affected (%)") +
  theme_classic() +
  coord_flip()

figure2a

eds <- c(mean(edits$pres_a, na.rm=T),mean(edits$pres_b, na.rm=T),mean(edits$pres_c, na.rm=T),mean(edits$pres_d, na.rm=T),mean(edits$pres_e, na.rm=T),mean(edits$pres_f, na.rm=T),mean(edits$pres_1, na.rm=T), mean(edits$pres_2, na.rm=T), mean(edits$pres_3, na.rm=T), mean(edits$pres_4, na.rm=T), mean(edits$pres_5, na.rm=T), mean(edits$pres_6, na.rm=T), mean(edits$pres_7, na.rm=T))
cat <- c("Received Ballots", "Unused Ballots", "Cancelled/Spoilt", "Null and Void", "Valid", "Total", "Chakwera", "Chilma", "Chisi", "Kaliya", "Kuwani", "Muluzi", "Mutharika")
my.df2 <- data.frame(eds, cat)
my.df2$eds <- my.df2$eds*100
my.df2$cat <- factor(my.df2$cat, levels = c("Received Ballots", "Unused Ballots", "Cancelled/Spoilt", "Null and Void", "Valid", "Total", "Chakwera", "Chilma", "Chisi", "Kaliya", "Kuwani", "Muluzi", "Mutharika"))

figure2b <- ggplot(my.df2, aes(x=fct_rev(cat), y=eds)) + 
  geom_bar(stat = "identity", fill="darkblue") +
  ylim(0,50) +
  xlab("") +
  ylab("Result-sheets affected (%)") +
  theme_classic() +
  coord_flip()

figure2b

# Table S2: correlation between incorrect result-sheet and result- --------
# This code generates the regressions presented in table S2

mod1 <- felm(pres_alt ~ pres_inc | 0 | 0 | const_code, data=edits)
mod1fe <- felm(pres_alt ~ pres_inc | const_code | 0 | const_code, data=edits)
mod2 <- felm(combine ~ pres_inc  | 0 | 0 | const_code, data=edits)
mod2fe <- felm(combine ~ pres_inc  | const_code | 0 | const_code, data=edits)

stargazer(mod1, mod1fe, mod2, mod2fe, 
          title = "Correlation between incorrect result-sheets and result-sheet edits", 
          covariate.labels = "Incorrect presidential result-sheet", 
          dep.var.labels = c("Edit (dummy)", "Number of rows edited"), 
          dep.var.caption = "Edits in presidential result-sheet"
          )


# Table S3: Correlates of result-sheet edits ------------------------------
# This code generates the regressions presented in table S3 

m1a <- felm(pres_alt ~ registered + factor(district_2014_winner) + parl_margin  | 0 | 0 | const_code, data = edits)
m1b <- felm(cand_alt ~ registered + factor(district_2014_winner) + parl_margin | 0 | 0 | const_code, data = edits)
m2a <- felm(pres_alt ~ registered + nightlights_2014a + dep_ratio100 | 0 | 0 | const_code, data = edits)
m2b <- felm(cand_alt ~ registered + nightlights_2014a + dep_ratio100  | 0 | 0 | const_code, data = edits)
m3a <- felm(pres_alt ~ registered + factor(district_2014_winner) + parl_margin + nightlights_2014a + dep_ratio100 | 0 | 0 | const_code, data = edits)
m3b <- felm(cand_alt ~ registered + factor(district_2014_winner) + parl_margin + nightlights_2014a + dep_ratio100  | 0 | 0 | const_code, data = edits)
m4a <- felm(pres_alt ~ registered + nightlights_2014a + dep_ratio100 | const_code | 0 | const_code, data = edits)
m4b <- felm(cand_alt ~ registered + nightlights_2014a + dep_ratio100  | const_code | 0 | const_code, data = edits)

stargazer(m1a,  m2a, m3a, m4a, m1b, m2b, m3b, m4b,
          type="latex", 
          title = "Correlates of result-sheet edits",
          covariate.labels = c("Registered voters", "MCP", "PP", "UDF", 
                               "Parliamentary margin of victory (pp)", 
                               "Nightlights (0-100)", "Dependency ratio 2011 (0-100)"), 
          dep.var.labels = c("Result sheet with any edit", "Edits in candidate row")
          )

