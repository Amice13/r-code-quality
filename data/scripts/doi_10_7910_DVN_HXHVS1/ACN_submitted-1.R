
#Replication Code for 
#Djupe, Paul A. and Brooklyn Walker. 
#"The Weakness of Anti-Christian Nationalism: When Religiously-Inclusive Orientations Can’t Increase Tolerance"
#Politics & Religion

#Installs####
# install.packages("tidyverse")
# install.packages("remotes")
# remotes::install_github("ryanburge/socsci")
# install.packages("modelsummary")
# install.packages("marginaleffects")
# install.packages("jtools")
# install.packages("patchwork")
# install.packages("fastDummies")
# install.packages("rio")
#install.packages("correlation")
#install.packages("pandoc")
#install.packages("psych")
#install.packages("showtext")

#Libraries####
library(tidyverse)
library(socsci)
library(modelsummary)
library(marginaleffects)
library(jtools)
library(patchwork)
library(fastDummies)
library(rio)
library(correlation)
library(psych)
library(showtext)
font_add_google("EB Garamond", "G", regular.wt = 400)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

theme_plot <- function() {
  theme_minimal() %+replace%
    theme(text=element_text(family="G", size=12),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.position = "bottom",
          plot.title.position = "plot")
}

#Setting the directory, reading the data####
apdir <- "SET_DATA_DIRECTORY_HERE"
ap <- import(paste0(apdir, "acn.csv"))

#Table A1 - Summary Statistics of included variables -  weighted vs nonweighted####
ap %>% filter(wgt!="NA") %>% dplyr::select(wgt, tol, cp, threat, acn3, cn5,separationr,individ,age,ed,female,race,attend5,llg,pid7,ba) %>% 
  datasummary(All(.) ~ Mean+ weighted.mean * Arguments(w = wgt, na.rm=T) + SD + Min + Max, 
              data=., output = paste0(apdir, 'acn_summtable1_both.docx') )


ap %>% filter(wgt!="NA") %>% 
  dplyr::select(race, llg, wgt) %>% dummy_cols(select_columns = c("race", "llg")) %>% 
  datasummary(All(.) ~ Mean + weighted.mean * Arguments(w = wgt, na.rm=T) + SD + Min + Max, 
              data=., output = paste0(apdir, 'acn_summtable2_both.docx'))

#Table 2 - Correlation Matrix of Christian Nationalist and Anti-Christian Nationalist Worldview Components####
corfun <- function(x) {
  out <- correlation(x) |>
    summary() |>
    format(2) |> 
    as.matrix()
  row.names(out) <- out[, 1]
  out <- out[, 2:ncol(out)]
  return(out)
}

ap %>% dplyr::select(q57_1, q57_2, q57_3, q57_4, q57_5, q57_6, q58_1, q58_2, q58_3, q58_4, q58_5) %>% 
  rename("Declare"="q57_1", "Values"="q57_2", "Separate"="q57_3", "Plan"="q57_4", "Prayer"="q57_5", "Symbols"="q57_6", 
         "NoFavor"="q58_1", "Love"="q58_2", "Regulation"="q58_3", "Believe"="q58_4", "RelFree"="q58_5") %>% 
  datasummary_correlation(., method=corfun, output = paste0(apdir, 'acn_table2.docx'))

#Table 3 - Factor loadings of Christian nationalism and ACN items####
pca_ap <- ap %>% dplyr::select(q57_1, q57_2, q57_3, q57_4, q57_5, 
                               q57_6, q58_1, q58_2, q58_3, q58_4, q58_5)

PCA_matrix <- cor(pca_ap, use='pairwise.complete.obs') 
round(PCA_matrix, 2)

# cortest.bartlett(pca_ap)
# KMO(r=pca_ap)
# det(PCA_matrix)

pca1 <- principal(pca_ap, nfactors = length(pca_ap), rotate="none")
pca1
names(pca1)
plot(pca1$values, type = "b") 

pca_4 <- psych::principal(pca_ap, nfactors=4, rotate='none')
pca_4
mean(pca_4$communality)  # must be greater than .6 to be a good solution- this is OK
round(psych::factor.model(pca_4$loadings), 3)
round(psych::factor.residuals(PCA_matrix, pca_4$loadings), 3)
# first extract the residuals
pca_4_resids <- psych::factor.residuals(PCA_matrix, pca_4$loadings)
# the object has the residuals in a single column
pca_4_resids <- as.matrix(pca_4_resids[upper.tri(pca_4_resids)])
# display the first 6 rows of the residuals
head(pca_4_resids)
large.resid_pca_4 <- abs(pca_4_resids)>0.05
sum(large.resid_pca_4)
round(sum(large.resid_pca_4)/nrow(pca_4_resids), 3) # this should be below 50%, it's .509
round(sqrt(mean(pca_4_resids^2)), 3) #this should be less than .08. It is 0.074
hist(pca_4_resids) # these should be normally distributed - they look decent
pca4ORTH <- psych::principal(pca_ap, nfactors=4, rotate="varimax", scores=TRUE)
pca4ORTH
pca4_table <- psych::print.psych(pca4ORTH, cut=0.3, sort=TRUE)
pca4ORTH_table <- round(pca4ORTH$loadings, 3)
pca4ORTH_table

write.table(pca4ORTH_table, file = paste0(apdir, "pca8ORTH_table.csv"), sep = ",", col.names = TRUE,
            row.names = FALSE)
pca4ORTH_table
head(pca4ORTH$scores, 10)


#Table 4 - Correlation Matrix between Four Religious and Public Life Concepts####
ap %>% dplyr::select(acn3, cn5, separationr, individ) %>% 
  rename("Anti-CN"="acn3", "CN"="cn5", "Church-State"="separationr", "Indiv."="individ") %>% 
  datasummary_correlation(., method=corfun, output = paste0(apdir, 'acn_table4.docx'))


#Figure A1 scatterplot####
ap %>% ggplot(aes(x=cn5, y=acn3)) + 
  geom_jitter(height=.03, width=.03, alpha=.3) +
  geom_smooth(method="lm") +
  scale_x_continuous(breaks=seq(0,1,.25)) +
  theme_minimal() +
  labs(x="Christian Nationalism",
       y="Anti-Christian Nationalism") +
  theme(text=element_text(family="G", size=12)) +
  annotate("rect", xmin=.82, xmax=.92, ymin=.11, ymax=.18, fill="white", color="black") +
  annotate("text", x=.87, y=.15, label="r=.36", family="G")

ap <- ap |> mutate(llg=as.factor(llg))

#Figure 1 and Figure A2 - four worldviews models
Bivariate <- lm(tol ~ acn3, data=ap, weight=wgt)
summ(Bivariate)

`W Demographics` <- lm(tol ~ acn3+llg+race+ed+age+female+pid7+attend5+ba, data=ap, weight=wgt)
summ(`W Demographics`)

`W Sep of C&S` <- lm(tol ~ acn3+separationr+llg+race+ed+age+female+pid7+attend5+ba, data=ap, weight=wgt)
summ(`W Sep of C&S`)

`W Individualism` <- lm(tol ~ acn3+separationr+individ+llg+race+ed+age+female+pid7+attend5+ba, data=ap, weight=wgt)
summ(`W Individualism`)

`W CN` <- lm(tol ~ acn3+separationr+individ+cn5+llg+race+ed+age+female+pid7+attend5+ba, data=ap, weight=wgt)
summ(`W CN`)

#Figure 1 - Four religious worldview variables####
#Figure 1 and Figure A2 - four worldviews models####
Bivariate <- lm(tol ~ acn3, data=ap, weight=wgt)
summ(Bivariate)

`W Demographics` <- lm(tol ~ acn3+llg+race+ed+age+female+pid7+attend5+ba, data=ap, weight=wgt)
summ(`W Demographics`)

`W Sep of C&S` <- lm(tol ~ acn3+separationr+llg+race+ed+age+female+pid7+attend5+ba, data=ap, weight=wgt)
summ(`W Sep of C&S`)

`W Individualism` <- lm(tol ~ acn3+separationr+individ+llg+race+ed+age+female+pid7+attend5+ba, data=ap, weight=wgt)
summ(`W Individualism`)

`W CN` <- lm(tol ~ acn3+separationr+individ+cn5+llg+race+ed+age+female+pid7+attend5+ba, data=ap, weight=wgt)
summ(`W CN`)

    #Figure 1
plot_coefs(`W CN`, coefs=c("ACN"="acn3",
                           "CN" = "cn5",
                           "Separation"="separationr",
                           "Individualization"="individ"),
           inner_ci_level = .9) +
  theme_minimal() + 
  theme(panel.grid.major.x = element_line(),
        panel.grid.major.y = element_blank(),
        text=element_text(family="G", size=12)) +
  labs(x="Estimated Effect of Worldviews", y="")

#Figure A2 -- Checking for suppression/enhancement effects####
plot_coefs(Bivariate, `W Demographics`, `W Sep of C&S`, `W Individualism`, `W CN` ,
           coefs=c("ACN"="acn3"),
           model.names = c("Bivariate",
                           "W/ Demographics",
                           "W/ Sep of C&S",
                           "W/ Individualization",
                           "W/ CN"),
           inner_ci_level = .9) +
  theme_minimal() + 
  theme(panel.grid.major.x = element_line(),
        panel.grid.major.y = element_blank(),
        text=element_text(family="G", size=12)) +
  labs(x="Estimated Effect of ACN", y="")

#Table A3 - results behind Figure 1 and Figure A2####
export_summs(Bivariate, `W Demographics`, `W Sep of C&S`, `W Individualism`, `W CN`,
             coefs=c("ACN"="acn3",
                     "Separation of C&S" = "separationr",
                     "Individualization"="individ",
                     "Christian nationalism"="cn5",
                     "Trump supporters" = "llg2", 
                     "Immigrants" = "llg3", 
                     "Atheists" = "llg4", 
                     "Christian fundamentalists" = "llg5", 
                     "Socialists" = "llg6", 
                     "Environmental activists" = "llg7", 
                     "Pro-Palestinian protestors" = "llg8", 
                     "Pro-Israel protestors" = "llg9", 
                     "Black"="raceBlack",
                     "Latino"="raceLatino",
                     "Other"="raceOther",
                     "White"="raceWhite",
                     "Education"="ed",
                     "Age"="age",
                     "Women"="female",
                     "Partisanship"="pid7",
                     "Worship attendance"="attend5",
                     "Intercept"="(Intercept)"),
             model.names = c("Bivariate",
                             "W/ Demographics",
                             "W/ Sep of C&S",
                             "W/ Individualization",
                             "W/ CN"),
             error_format = "({p.value})", error_pos = "below",
             to.file="docx", file.name = paste0(apdir, "acn_online_table_A2a.docx"))

#Figure 2 - interaction with CN and ACN####

lmt4 <- lm(tol ~ cn5*acn3+separationr+individ+age+ed+female+race+attend5+llg+pid7+ba, data=ap, weight=wgt)
summ(lmt4)
plot_predictions(lmt4, condition=list("acn3", "cn5"="minmax"), conf_level = .84) +
  labs(x="Anti-CN", y="Tolerance") +
  theme_plot() + 
  scale_x_continuous(breaks=c(0,1), labels=c("Least", "Most")) +
  scale_color_manual(name="Christian\nNationalism", labels=c("Least", "Most"),
                       values=c("skyblue3", "darkorange3")) +
  scale_fill_manual(name="Christian\nNationalism", labels=c("Least", "Most"),
                      values=c("skyblue3", "darkorange3"))

#Table A3 - results behind Figure 2####
export_summs(lmt4,
             coefs=c("ACN"="acn3",
                     "Chr nationalism * ACN" = "cn5:acn3",
                     "Separation of C&S" = "separationr",
                     "Individualization"="individ",
                     "Christian nationalism"="cn5",
                     "Trump supporters" = "llg2", 
                     "Immigrants" = "llg3", 
                     "Atheists" = "llg4", 
                     "Christian fundamentalists" = "llg5", 
                     "Socialists" = "llg6", 
                     "Environmental activists" = "llg7", 
                     "Pro-Palestinian protestors" = "llg8", 
                     "Pro-Israel protestors" = "llg9", 
                     "Black"="raceBlack",
                     "Latino"="raceLatino",
                     "Other"="raceOther",
                     "White"="raceWhite",
                     "Education"="ed",
                     "Age"="age",
                     "Women"="female",
                     "Partisanship"="pid7",
                     "Worship attendance"="attend5",
                     "Evangelical"="ba",
                     "Intercept"="(Intercept)"),
             error_format = "({p.value})", error_pos = "below",
             to.file="docx", file.name = paste0(apdir, "acn_online_table_A3.docx"))


#Figure 3 -- Predicting Two types of Threat####
lmthr <- lm(threat ~ cn5*acn3+separationr+individ+age+ed+female+race+attend5+llg+pid7+ba, data=ap, weight=wgt)
summ(lmthr)
pred_thr <- plot_predictions(lmthr, condition=list("acn3", "cn5"="minmax"), conf_level = .84) +
  labs(x="Anti-CN", y="Perceived LLG Threat") +
  theme_plot() + 
  scale_x_continuous(breaks=c(0,1), labels=c("Least", "Most")) +
  scale_color_manual(name="Christian\nNationalism", labels=c("Least", "Most"),
                     values=c("skyblue3", "darkorange3")) +
  scale_fill_manual(name="Christian\nNationalism", labels=c("Least", "Most"),
                    values=c("skyblue3", "darkorange3"))

lmthrcp <- lm(cp ~ cn5*acn3+separationr+individ+age+ed+female+race+attend5+llg+pid7+ba, data=ap, weight=wgt)
summ(lmthrcp)
pred_cp <- plot_predictions(lmthrcp, condition=list("acn3", "cn5"="minmax"), conf_level = .84) +
  labs(x="Anti-CN", y="Perceived Christian Persecution") +
  theme_plot() + 
  scale_x_continuous(breaks=c(0,1), labels=c("Least", "Most")) +
  scale_color_manual(name="Christian\nNationalism", labels=c("Least", "Most"),
                     values=c("skyblue3", "darkorange3")) +
  scale_fill_manual(name="Christian\nNationalism", labels=c("Least", "Most"),
                    values=c("skyblue3", "darkorange3"))

  #Patchwork them together
pred_cp + pred_thr + plot_layout(guides='collect',
                                 axes='collect') &
  theme(legend.position = "bottom")

#Table A4 - results behind Figure 3 ####
export_summs(lmthrcp, lmthr,
             model.names = c("Christian Persecution", "LLG Threat"),
             coefs=c("ACN"="acn3",
                     "Chr nationalism * ACN" = "cn5:acn3",
                     "Separation of C&S" = "separationr",
                     "Individualization"="individ",
                     "Christian nationalism"="cn5",
                     "Trump supporters" = "llg2", 
                     "Immigrants" = "llg3", 
                     "Atheists" = "llg4", 
                     "Christian fundamentalists" = "llg5", 
                     "Socialists" = "llg6", 
                     "Environmental activists" = "llg7", 
                     "Pro-Palestinian protestors" = "llg8", 
                     "Pro-Israel protestors" = "llg9", 
                     "Black"="raceBlack",
                     "Latino"="raceLatino",
                     "Other"="raceOther",
                     "White"="raceWhite",
                     "Education"="ed",
                     "Age"="age",
                     "Women"="female",
                     "Partisanship"="pid7",
                     "Worship attendance"="attend5",
                     "Evangelical"="ba",
                     "Intercept"="(Intercept)"),
             error_format = "({p.value})", error_pos = "below",
             to.file="docx", file.name = paste0(apdir, "acn_online_table_A4.docx"))

#Removes####
rm(list=ls(pattern="lm"))
rm(list=ls(pattern="pred"))
rm(list=ls(pattern="W "))
rm(list=ls(pattern="pca"))
rm(ap, Bivariate, PCA_matrix)
