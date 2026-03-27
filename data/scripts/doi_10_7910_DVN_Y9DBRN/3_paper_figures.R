# PLOTS

require(reshape2)
library(tidyverse)
library(grid)
library(gridExtra)
library(png)
require(cowplot)
library(scales)
library(tidyr)
library(haven)
library(MASS)
library(ggplot2)
library(scales)
library(multicon)
library(extrafont)

setwd(dir = "XXX/Replication/")
my_path <- "XXX/Replication/Plots" 

df <- read_dta("intermediate_files/2d_brier_log.dta")
df$partisan[df$partisan=="NA"] <- NA
df <- df[!is.na(df$partisan),]


#-------------------------------------------------------------------------------------------------------------------------------

### FIGURE 1 ###


## Figure 1 Absolute ##
df.long <- pivot_longer(df, cols=c(err_us_abs, err_state_abs), names_to= "issues", values_to="dv")
df.long$partisan <- as.factor(df.long$partisan)
df.long <- within(df.long, partisan <- relevel(partisan, ref = "Other"))
df.long$elite <-as.factor(df.long$elite)
df.long <- within(df.long, elite <- relevel(elite, ref="public"))



a = "Mobility (U.S.)" 
b = "Mobility (state)"

issues = c("err_us_abs", "err_state_abs")
results = data.frame(matrix(,12,4))
colnames(results) = c("issue","party","type","mean")
results$party = factor(rep(c("Democrat","Republican", "Other"),2),
                       levels = c("Democrat", "Republican", "Other"))
results$type = factor(rep(c("Public","Public","Public", "Local officials", "Local officials","Local officials"),1),
                      levels = c("Public","Local officials"))


for (i in 1:2){
  k = 6*i - 5
  
  results[k:(k + 5),"issue"] = paste(rep(issues[i],1))
  reg = lm(dv ~ partisan * elite, #+ female + education + nonwhite + age3044 + age4564 + age65plus, 
           data=df.long[df.long$issues==issues[i],])
  summary(reg)
  results[k,"mean"] =  summary(reg)$coefficients["partisanDemocrat", 1] + summary(reg)$coefficients["(Intercept)", 1]
  results[k + 1,"mean"] = summary(reg)$coefficients["partisanRepublican",1] + summary(reg)$coefficients["(Intercept)", 1]
  results[k+2, "mean"] = summary(reg)$coefficients["(Intercept)", 1]
  results[k+3, "mean"] = summary(reg)$coefficients["partisanDemocrat:eliteelite", 1] + summary(reg)$coefficients["(Intercept)", 1] + summary(reg)$coefficients["eliteelite", 1] + summary(reg)$coefficients["partisanDemocrat", 1] 
  results[k+4, "mean"] = summary(reg)$coefficients["partisanRepublican:eliteelite", 1] + summary(reg)$coefficients["(Intercept)", 1] + summary(reg)$coefficients["eliteelite", 1] + summary(reg)$coefficients["partisanRepublican", 1] 
  results[k+5, "mean"] = summary(reg)$coefficients["eliteelite", 1] + summary(reg)$coefficients["(Intercept)", 1]
}


results$issue[results$issue=="err_us_abs"] = a
results$issue[results$issue=="err_state_abs"] =  b


issues = c(b,a)
results$issue = factor(results$issue,levels = issues)
results <- results[-c(3, 6, 9, 12), ]

results$size = ifelse(results$type=="Local officials",2.4,2.6)
ggplot(data=results, aes(y=as.numeric(mean),x = issue,shape = type,color=party))+ 
  geom_point(position = position_dodge(width = 0, preserve = c("total")), stat='identity',size=5)+
  # geom_errorbar(aes(ymin=mean - ci,ymax=mean + ci),position = position_dodge(width = 0, preserve = c("total")), width=0.1,alpha=1)+
  geom_hline(yintercept=0,lwd=0.2,lty=2)+
  scale_color_manual(name = "", values = c("Republican" = ("red4"), "Democrat"  = "skyblue1"),
                     guide=guide_legend(nrow=1))+
  scale_shape_manual(name = "", values = c("Public"  = 2,"Local officials" = 15))+
  theme_minimal()+#theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())+
  theme(legend.position="bottom",legend.box = "vertical",
        legend.justification =  0.5, legend.spacing.x = unit(0.1,"cm"),
        legend.spacing.y = unit(-0.5,"cm"))+xlab('')+ 
  ylab('\nPerceived mobility rate versus true value')+
  coord_flip()+
  ylim(-0.09,0.12) +
  theme(axis.title.x = element_text(size=14)) +
  theme(axis.text.y=element_text(face="bold", size=14)) +
  theme(legend.text=element_text(size=14))
  ggsave(path= my_path, filename="fig_1_abs.pdf",width=7.5, height=6)


#--------------------------------------------------------------------------------------------------------
## Figure 1 Relative with only q1-q5##

df.long <- pivot_longer(df, cols=c(err_us_q5q1, err_cz_q5q1), names_to= "issues", values_to="dv")
df.long$partisan <- as.factor(df.long$partisan)
df.long <- within(df.long, partisan <- relevel(partisan, ref = "Other"))
df.long$elite <-as.factor(df.long$elite)
df.long <- within(df.long, elite <- relevel(elite, ref="public"))

a = "Q1 -> Q5 mobility (U.S.)"
b = "Q1 -> Q5 mobility (local)" 


issues = c("err_us_q5q1", "err_cz_q5q1")
results = data.frame(matrix(,24,4))
colnames(results) = c("issue","party","type","mean")
results$party = factor(rep(c("Democrat","Republican", "Other"),2),
                       levels = c("Democrat", "Republican", "Other"))
results$type = factor(rep(c("Public","Public","Public", "Local officials", "Local officials","Local officials"),1),
                      levels = c("Public","Local officials"))


for (i in 1:2){
  k = 6*i - 5
  
  results[k:(k + 5),"issue"] = paste(rep(issues[i],1))
  reg = lm(dv ~ partisan * elite, #+ female + education + nonwhite + age3044 + age4564 + age65plus, 
           data=df.long[df.long$issues==issues[i],])
  summary(reg)
  results[k,"mean"] =  summary(reg)$coefficients["partisanDemocrat", 1] + summary(reg)$coefficients["(Intercept)", 1]
  results[k + 1,"mean"] = summary(reg)$coefficients["partisanRepublican",1] + summary(reg)$coefficients["(Intercept)", 1]
  results[k+2, "mean"] = summary(reg)$coefficients["(Intercept)", 1]
  results[k+3, "mean"] = summary(reg)$coefficients["partisanDemocrat:eliteelite", 1] + summary(reg)$coefficients["(Intercept)", 1] + summary(reg)$coefficients["eliteelite", 1] + summary(reg)$coefficients["partisanDemocrat", 1] 
  results[k+4, "mean"] = summary(reg)$coefficients["partisanRepublican:eliteelite", 1] + summary(reg)$coefficients["(Intercept)", 1] + summary(reg)$coefficients["eliteelite", 1] + summary(reg)$coefficients["partisanRepublican", 1] 
  results[k+5, "mean"] = summary(reg)$coefficients["eliteelite", 1] + summary(reg)$coefficients["(Intercept)", 1]
}


results$issue[results$issue=="err_us_q5q1"] = a
results$issue[results$issue=="err_cz_q5q1"] =  b


issues = c(b,a)
results$issue = factor(results$issue,levels = issues)
results <- results[-c(3, 6, 9, 12,13,14,15,16,17,18,19,20,21,22,23,24), ]


results$size = ifelse(results$type=="Local officials",2.4,2.6)
ggplot(data=results, aes(y=as.numeric(mean),x = issue,shape = type,color=party))+ 
  geom_point(position = position_dodge(width = 0, preserve = c("total")), stat='identity',size=5)+
  # geom_errorbar(aes(ymin=mean - ci,ymax=mean + ci),position = position_dodge(width = 0, preserve = c("total")), width=0.1,alpha=1)+
  geom_hline(yintercept=0,lwd=0.2,lty=2)+
  scale_color_manual(name = "", values = c("Republican" = ("red4"), "Democrat"  = "skyblue1"),
                     guide=guide_legend(nrow=1))+
  scale_shape_manual(name = "", values = c("Public"  = 2,"Local officials" = 15))+
  theme_minimal()+#theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())+
  theme(legend.position="bottom",legend.box = "vertical",
        legend.justification =  0.5, legend.spacing.x = unit(0.1,"cm"),
        legend.spacing.y = unit(-0.5,"cm"))+xlab('')+ 
  ylab('\nPerceived mobility rate versus true value')+
  coord_flip()+
  ylim(-0.15,0.12) +
  theme(axis.title.x = element_text(size=14)) +
  theme(axis.text.y=element_text(face="bold", size=14)) +
  theme(legend.text=element_text(size=14))
  ggsave(path= my_path, filename="fig_1_rel_new.pdf",width=7.5, height=6)
  

#-------------------------------------------------------------------------------
## Figure 1 Relative with both Q1-Q5 and Q1-Q1 ##
  
  ## Figure 1 Relative ##
  
  df.long <- pivot_longer(df, cols=c(err_us_q5q1, err_cz_q5q1, err_us_q1q1, err_cz_q1q1), names_to= "issues", values_to="dv")
  df.long$partisan <- as.factor(df.long$partisan)
  df.long <- within(df.long, partisan <- relevel(partisan, ref = "Other"))
  df.long$elite <-as.factor(df.long$elite)
  df.long <- within(df.long, elite <- relevel(elite, ref="public"))
  
  
  a = "Q1 -> Q5 mobility (U.S.)"
  b = "Q1 -> Q5 mobility (local)" 
  c = "Q1 -> Q1 mobility (U.S.)"
  d ="Q1 -> Q1 mobility (local)"
  
  issues = c("err_us_q5q1", "err_cz_q5q1", "err_us_q1q1", "err_cz_q1q1")
  results = data.frame(matrix(,24,4))
  colnames(results) = c("issue","party","type","mean")
  results$party = factor(rep(c("Democrat","Republican", "Other"),2),
                         levels = c("Democrat", "Republican", "Other"))
  results$type = factor(rep(c("Public","Public","Public", "Local officials", "Local officials","Local officials"),1),
                        levels = c("Public","Local officials"))
  
  
  for (i in 1:4){
    k = 6*i - 5
    
    results[k:(k + 5),"issue"] = paste(rep(issues[i],1))
    reg = lm(dv ~ partisan * elite, #+ female + education + nonwhite + age3044 + age4564 + age65plus, 
             data=df.long[df.long$issues==issues[i],])
    summary(reg)
    results[k,"mean"] =  summary(reg)$coefficients["partisanDemocrat", 1] + summary(reg)$coefficients["(Intercept)", 1]
    results[k + 1,"mean"] = summary(reg)$coefficients["partisanRepublican",1] + summary(reg)$coefficients["(Intercept)", 1]
    results[k+2, "mean"] = summary(reg)$coefficients["(Intercept)", 1]
    results[k+3, "mean"] = summary(reg)$coefficients["partisanDemocrat:eliteelite", 1] + summary(reg)$coefficients["(Intercept)", 1] + summary(reg)$coefficients["eliteelite", 1] + summary(reg)$coefficients["partisanDemocrat", 1] 
    results[k+4, "mean"] = summary(reg)$coefficients["partisanRepublican:eliteelite", 1] + summary(reg)$coefficients["(Intercept)", 1] + summary(reg)$coefficients["eliteelite", 1] + summary(reg)$coefficients["partisanRepublican", 1] 
    results[k+5, "mean"] = summary(reg)$coefficients["eliteelite", 1] + summary(reg)$coefficients["(Intercept)", 1]
  }
  
  
  results$issue[results$issue=="err_us_q5q1"] = a
  results$issue[results$issue=="err_cz_q5q1"] =  b
  results$issue[results$issue=="err_us_q1q1"] =  c
  results$issue[results$issue=="err_cz_q1q1"] =  d
  
  issues = c(d,c,b,a)
  results$issue = factor(results$issue,levels = issues)
  results <- results[-c(3, 6, 9, 12, 15, 18, 21, 24), ]
  
  results$size = ifelse(results$type=="Local officials",2.4,2.6)
  ggplot(data=results, aes(y=as.numeric(mean),x = issue,shape = type,color=party))+ 
    geom_point(position = position_dodge(width = 0, preserve = c("total")), stat='identity',size=5)+
    # geom_errorbar(aes(ymin=mean - ci,ymax=mean + ci),position = position_dodge(width = 0, preserve = c("total")), width=0.1,alpha=1)+
    geom_hline(yintercept=0,lwd=0.2,lty=2)+
    scale_color_manual(name = "", values = c("Republican" = ("red4"), "Democrat"  = "skyblue1"),
                       guide=guide_legend(nrow=1))+
    scale_shape_manual(name = "", values = c("Public"  = 2,"Local officials" = 15))+
    theme_minimal()+#theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())+
    theme(legend.position="bottom",legend.box = "vertical",
          legend.justification =  0.5, legend.spacing.x = unit(0.1,"cm"),
          legend.spacing.y = unit(-0.5,"cm"))+xlab('')+ 
    ylab('\nPerceived mobility rate versus true value')+
    coord_flip()+
    ylim(-0.15,0.12) +
    theme(axis.title.x = element_text(size=14)) +
    theme(axis.text.y=element_text(face="bold", size=14)) +
    theme(legend.text=element_text(size=14))
  ggsave(path= my_path, filename="fig_1_rel.pdf",width=7.5, height=6)
  
  
#------------------------------------------------------------------------------------------------
## Figure 2 ##
  
## Figure 2 Absolute ##
df.long <- pivot_longer(df, cols=c(absval_err_us_abs, absval_err_state_abs), names_to= "issues", values_to="dv")
df.long$partisan <- as.factor(df.long$partisan)
df.long <- within(df.long, partisan <- relevel(partisan, ref = "Other"))
df.long$elite <-as.factor(df.long$elite)
df.long <- within(df.long, elite <- relevel(elite, ref="public"))
  
  
  
  a = "Mobility (U.S.)" 
  b = "Mobility (state)"
  
  issues = c("absval_err_us_abs", "absval_err_state_abs")
  results = data.frame(matrix(,12,4))
  colnames(results) = c("issue","party","type","mean")
  results$party = factor(rep(c("Democrat","Republican", "Other"),2),
                         levels = c("Democrat", "Republican", "Other"))
  results$type = factor(rep(c("Public","Public","Public", "Local officials", "Local officials","Local officials"),1),
                        levels = c("Public","Local officials"))
  
  
  for (i in 1:2){
    k = 6*i - 5
    
    results[k:(k + 5),"issue"] = paste(rep(issues[i],1))
    reg = lm(dv ~ partisan * elite, #+ female + education + nonwhite + age3044 + age4564 + age65plus, 
             data=df.long[df.long$issues==issues[i],])
    summary(reg)
    results[k,"mean"] =  summary(reg)$coefficients["partisanDemocrat", 1] + summary(reg)$coefficients["(Intercept)", 1]
    results[k + 1,"mean"] = summary(reg)$coefficients["partisanRepublican",1] + summary(reg)$coefficients["(Intercept)", 1]
    results[k+2, "mean"] = summary(reg)$coefficients["(Intercept)", 1]
    results[k+3, "mean"] = summary(reg)$coefficients["partisanDemocrat:eliteelite", 1] + summary(reg)$coefficients["(Intercept)", 1] + summary(reg)$coefficients["eliteelite", 1] + summary(reg)$coefficients["partisanDemocrat", 1] 
    results[k+4, "mean"] = summary(reg)$coefficients["partisanRepublican:eliteelite", 1] + summary(reg)$coefficients["(Intercept)", 1] + summary(reg)$coefficients["eliteelite", 1] + summary(reg)$coefficients["partisanRepublican", 1] 
    results[k+5, "mean"] = summary(reg)$coefficients["eliteelite", 1] + summary(reg)$coefficients["(Intercept)", 1]
  }
  
  
  results$issue[results$issue=="absval_err_us_abs"] = a
  results$issue[results$issue=="absval_err_state_abs"] =  b
  
  
  issues = c(b,a)
  results$issue = factor(results$issue,levels = issues)
  results <- results[-c(3, 6, 9, 12), ]
  
  results$size = ifelse(results$type=="Local officials",2.4,2.6)
  ggplot(data=results, aes(y=as.numeric(mean),x = issue,shape = type,color=party))+ 
    geom_point(position = position_dodge(width = 0, preserve = c("total")), stat='identity',size=5)+
    # geom_errorbar(aes(ymin=mean - ci,ymax=mean + ci),position = position_dodge(width = 0, preserve = c("total")), width=0.1,alpha=1)+
    geom_hline(yintercept=0,lwd=0.2,lty=2)+
    scale_color_manual(name = "", values = c("Republican" = ("red4"), "Democrat"  = "skyblue1"),
                       guide=guide_legend(nrow=1))+
    scale_shape_manual(name = "", values = c("Public"  = 2,"Local officials" = 15))+
    theme_minimal()+#theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())+
    theme(legend.position="bottom",legend.box = "vertical",
          legend.justification =  0.5, legend.spacing.x = unit(0.1,"cm"),
          legend.spacing.y = unit(-0.5,"cm"))+xlab('')+ 
    ylab('\nInaccuracy of perceived mobility rate')+
    coord_flip()+
    ylim(0,0.25) +
    theme(axis.title.x = element_text(size=14)) +
    theme(axis.text.y=element_text(face="bold", size=14)) +
    theme(legend.text=element_text(size=14))
  ggsave(path= my_path, filename="fig_2_abs.pdf",width=7.5, height=6)
  
  
  #------------------------------------------------------------------------------------------------  
  ## Figure 2 Relative only q1-q5##
  
  df.long <- pivot_longer(df, cols=c(absval_err_us_q5q1, absval_err_cz_q5q1), names_to= "issues", values_to="dv")
  df.long$partisan <- as.factor(df.long$partisan)
  df.long <- within(df.long, partisan <- relevel(partisan, ref = "Other"))
  df.long$elite <-as.factor(df.long$elite)
  df.long <- within(df.long, elite <- relevel(elite, ref="public"))
  
  
  a = "Q1 -> Q5 mobility (U.S.)"
  b = "Q1 -> Q5 mobility (local)" 
 
  
  issues = c("absval_err_us_q5q1", "absval_err_cz_q5q1")
  results = data.frame(matrix(,24,4))
  colnames(results) = c("issue","party","type","mean")
  results$party = factor(rep(c("Democrat","Republican", "Other"),2),
                         levels = c("Democrat", "Republican", "Other"))
  results$type = factor(rep(c("Public","Public","Public", "Local officials", "Local officials","Local officials"),1),
                        levels = c("Public","Local officials"))
  
  
  for (i in 1:2){
    k = 6*i - 5
    
    results[k:(k + 5),"issue"] = paste(rep(issues[i],1))
    reg = lm(dv ~ partisan * elite, #+ female + education + nonwhite + age3044 + age4564 + age65plus, 
             data=df.long[df.long$issues==issues[i],])
    summary(reg)
    results[k,"mean"] =  summary(reg)$coefficients["partisanDemocrat", 1] + summary(reg)$coefficients["(Intercept)", 1]
    results[k + 1,"mean"] = summary(reg)$coefficients["partisanRepublican",1] + summary(reg)$coefficients["(Intercept)", 1]
    results[k+2, "mean"] = summary(reg)$coefficients["(Intercept)", 1]
    results[k+3, "mean"] = summary(reg)$coefficients["partisanDemocrat:eliteelite", 1] + summary(reg)$coefficients["(Intercept)", 1] + summary(reg)$coefficients["eliteelite", 1] + summary(reg)$coefficients["partisanDemocrat", 1] 
    results[k+4, "mean"] = summary(reg)$coefficients["partisanRepublican:eliteelite", 1] + summary(reg)$coefficients["(Intercept)", 1] + summary(reg)$coefficients["eliteelite", 1] + summary(reg)$coefficients["partisanRepublican", 1] 
    results[k+5, "mean"] = summary(reg)$coefficients["eliteelite", 1] + summary(reg)$coefficients["(Intercept)", 1]
  }
  
  
  results$issue[results$issue=="absval_err_us_q5q1"] = a
  results$issue[results$issue=="absval_err_cz_q5q1"] =  b
 
  
  issues = c(b,a)
  results$issue = factor(results$issue,levels = issues)
  results <- results[-c(3, 6, 9, 12,13,14,15,16,17,18,19,20,21,22,23,24), ]
  
  
  results$size = ifelse(results$type=="Local officials",2.4,2.6)
  ggplot(data=results, aes(y=as.numeric(mean),x = issue,shape = type,color=party))+ 
    geom_point(position = position_dodge(width = 0, preserve = c("total")), stat='identity',size=5)+
    # geom_errorbar(aes(ymin=mean - ci,ymax=mean + ci),position = position_dodge(width = 0, preserve = c("total")), width=0.1,alpha=1)+
    geom_hline(yintercept=0,lwd=0.2,lty=2)+
    scale_color_manual(name = "", values = c("Republican" = ("red4"), "Democrat"  = "skyblue1"),
                       guide=guide_legend(nrow=1))+
    scale_shape_manual(name = "", values = c("Public"  = 2,"Local officials" = 15))+
    theme_minimal()+#theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())+
    theme(legend.position="bottom",legend.box = "vertical",
          legend.justification =  0.5, legend.spacing.x = unit(0.1,"cm"),
          legend.spacing.y = unit(-0.5,"cm"))+xlab('')+ 
    ylab('\nInaccuracy of perceived mobility rate')+
    coord_flip()+
    ylim(0,0.3) +
    theme(axis.title.x = element_text(size=14)) +
    theme(axis.text.y=element_text(face="bold", size=14)) +
    theme(legend.text=element_text(size=14))
  ggsave(path= my_path, filename="fig_2_rel_new.pdf",width=7.5, height=6)
  
#--------------------------------------------------------------------------------
  ## Figure 2 Relative both Q1-Q1 and Q1-Q5

  df.long <- pivot_longer(df, cols=c(absval_err_us_q5q1, absval_err_cz_q5q1, absval_err_us_q1q1, absval_err_cz_q1q1), names_to= "issues", values_to="dv")
  df.long$partisan <- as.factor(df.long$partisan)
  df.long <- within(df.long, partisan <- relevel(partisan, ref = "Other"))
  df.long$elite <-as.factor(df.long$elite)
  df.long <- within(df.long, elite <- relevel(elite, ref="public"))
  
  
  a = "Q1 -> Q5 mobility (U.S.)"
  b = "Q1 -> Q5 mobility (local)" 
  c = "Q1 -> Q1 mobility (U.S.)"
  d ="Q1 -> Q1 mobility (local)"
  
  issues = c("absval_err_us_q5q1", "absval_err_cz_q5q1", "absval_err_us_q1q1", "absval_err_cz_q1q1")
  results = data.frame(matrix(,24,4))
  colnames(results) = c("issue","party","type","mean")
  results$party = factor(rep(c("Democrat","Republican", "Other"),2),
                         levels = c("Democrat", "Republican", "Other"))
  results$type = factor(rep(c("Public","Public","Public", "Local officials", "Local officials","Local officials"),1),
                        levels = c("Public","Local officials"))
  
  
  for (i in 1:4){
    k = 6*i - 5
    
    results[k:(k + 5),"issue"] = paste(rep(issues[i],1))
    reg = lm(dv ~ partisan * elite, #+ female + education + nonwhite + age3044 + age4564 + age65plus, 
             data=df.long[df.long$issues==issues[i],])
    summary(reg)
    results[k,"mean"] =  summary(reg)$coefficients["partisanDemocrat", 1] + summary(reg)$coefficients["(Intercept)", 1]
    results[k + 1,"mean"] = summary(reg)$coefficients["partisanRepublican",1] + summary(reg)$coefficients["(Intercept)", 1]
    results[k+2, "mean"] = summary(reg)$coefficients["(Intercept)", 1]
    results[k+3, "mean"] = summary(reg)$coefficients["partisanDemocrat:eliteelite", 1] + summary(reg)$coefficients["(Intercept)", 1] + summary(reg)$coefficients["eliteelite", 1] + summary(reg)$coefficients["partisanDemocrat", 1] 
    results[k+4, "mean"] = summary(reg)$coefficients["partisanRepublican:eliteelite", 1] + summary(reg)$coefficients["(Intercept)", 1] + summary(reg)$coefficients["eliteelite", 1] + summary(reg)$coefficients["partisanRepublican", 1] 
    results[k+5, "mean"] = summary(reg)$coefficients["eliteelite", 1] + summary(reg)$coefficients["(Intercept)", 1]
  }
  
  
  results$issue[results$issue=="absval_err_us_q5q1"] = a
  results$issue[results$issue=="absval_err_cz_q5q1"] =  b
  results$issue[results$issue=="absval_err_us_q1q1"] =  c
  results$issue[results$issue=="absval_err_cz_q1q1"] =  d
  
  issues = c(d,c,b,a)
  results$issue = factor(results$issue,levels = issues)
  results <- results[-c(3, 6, 9, 12, 15, 18, 21, 24), ]
  
  results$size = ifelse(results$type=="Local officials",2.4,2.6)
  ggplot(data=results, aes(y=as.numeric(mean),x = issue,shape = type,color=party))+ 
    geom_point(position = position_dodge(width = 0, preserve = c("total")), stat='identity',size=5)+
    # geom_errorbar(aes(ymin=mean - ci,ymax=mean + ci),position = position_dodge(width = 0, preserve = c("total")), width=0.1,alpha=1)+
    geom_hline(yintercept=0,lwd=0.2,lty=2)+
    scale_color_manual(name = "", values = c("Republican" = ("red4"), "Democrat"  = "skyblue1"),
                       guide=guide_legend(nrow=1))+
    scale_shape_manual(name = "", values = c("Public"  = 2,"Local officials" = 15))+
    theme_minimal()+#theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())+
    theme(legend.position="bottom",legend.box = "vertical",
          legend.justification =  0.5, legend.spacing.x = unit(0.1,"cm"),
          legend.spacing.y = unit(-0.5,"cm"))+xlab('')+ 
    ylab('\nInaccuracy of perceived mobility rate')+
    coord_flip()+
    ylim(0,0.3) +
    theme(axis.title.x = element_text(size=14)) +
    theme(axis.text.y=element_text(face="bold", size=14)) +
    theme(legend.text=element_text(size=14))
  ggsave(path= my_path, filename="fig_2_rel.pdf",width=7.5, height=6)
  
  
  

#------------------------------------------------------------------------------------------------
## Figure 3 ##

addline_format <- function(x,...){
  gsub('\\s','\n',x)
}
  
  # test
  # elites
  df_party <- subset(df, partisan=="Republican"|partisan=="Democrat", select=c(opp_race, opp_wealth, opp_edu, opp_work, opp_nochild, elitenum, partisan))
  elites <- subset(df_party, elitenum==1, select=c(opp_race, opp_wealth, opp_edu, opp_work, opp_nochild, elitenum, partisan))
  republicans <- subset(elites, partisan=="Republican", select=c(opp_race, opp_wealth, opp_edu, opp_work, opp_nochild, elitenum, partisan))
  rep_race <- mean(republicans$opp_race, na.rm = TRUE)
  rep_wealth <- mean(republicans$opp_wealth, na.rm = TRUE)
  rep_edu <- mean(republicans$opp_edu, na.rm = TRUE)
  rep_work <- mean(republicans$opp_work, na.rm = TRUE)
  rep_nochild <- mean(republicans$opp_nochild, na.rm = TRUE)
  
  democrats <- subset(elites, partisan=="Democrat", select=c(opp_race, opp_wealth, opp_edu, opp_work, opp_nochild, elitenum, partisan))
  dem_race <- mean(democrats$opp_race, na.rm = TRUE)
  dem_wealth <- mean(democrats$opp_wealth, na.rm = TRUE)
  dem_edu <- mean(democrats$opp_edu, na.rm = TRUE)
  dem_work <- mean(democrats$opp_work, na.rm = TRUE)
  dem_nochild <- mean(democrats$opp_nochild, na.rm = TRUE)
  
  diff_race = abs(dem_race - rep_race)
  diff_wealth = abs(dem_wealth - rep_wealth)
  diff_edu = abs(dem_edu - rep_edu)
  diff_work = abs(dem_work - rep_work)
  diff_nochild = abs(dem_nochild - rep_nochild)
  
  print(diff_race)
  print(diff_wealth)
  print(diff_edu)
  print(diff_work)
  print(diff_nochild)
  
  # public
  df_party <- subset(df, partisan=="Republican"|partisan=="Democrat", select=c(opp_race, opp_wealth, opp_edu, opp_work, opp_nochild, elitenum, partisan))
  public <- subset(df_party, elitenum=0, select=c(opp_race, opp_wealth, opp_edu, opp_work, opp_nochild, elitenum, partisan))
  republicans <- subset(public, partisan=="Republican", select=c(opp_race, opp_wealth, opp_edu, opp_work, opp_nochild, elitenum, partisan))
  rep_race <- mean(republicans$opp_race, na.rm = TRUE)
  rep_wealth <- mean(republicans$opp_wealth, na.rm = TRUE)
  rep_edu <- mean(republicans$opp_edu, na.rm = TRUE)
  rep_work <- mean(republicans$opp_work, na.rm = TRUE)
  rep_nochild <- mean(republicans$opp_nochild, na.rm = TRUE)
  
  democrats <- subset(elites, partisan=="Democrat", select=c(opp_race, opp_wealth, opp_edu, opp_work, opp_nochild, elitenum, partisan))
  dem_race <- mean(democrats$opp_race, na.rm = TRUE)
  dem_wealth <- mean(democrats$opp_wealth, na.rm = TRUE)
  dem_edu <- mean(democrats$opp_edu, na.rm = TRUE)
  dem_work <- mean(democrats$opp_work, na.rm = TRUE)
  dem_nochild <- mean(democrats$opp_nochild, na.rm = TRUE)
  
  diff_race = abs(dem_race - rep_race)
  diff_wealth = abs(dem_wealth - rep_wealth)
  diff_edu = abs(dem_edu - rep_edu)
  diff_work = abs(dem_work - rep_work)
  diff_nochild = abs(dem_nochild - rep_nochild)
  
  print(diff_race)
  print(diff_wealth)
  print(diff_edu)
  print(diff_work)
  print(diff_nochild)
  
  

# elite
# with all 5
df.long <- pivot_longer(df, cols=c(  opp_edu, opp_work,opp_wealth, opp_race, opp_nochild), names_to= "reasons", values_to="dv")
without_other <- subset(df.long, partisan=="Republican"|partisan=="Democrat", select=c(reasons, dv, elitenum, partisan))
elites <- subset(without_other, elitenum==1, select=c(reasons, dv, elitenum, partisan))
elites$reasons <- factor(elites$reasons, levels=c("opp_race", "opp_wealth",  "opp_edu", "opp_nochild","opp_work"), ordered=TRUE)
p <- ggplot(elites, aes(x=reasons, y=dv, fill=partisan))+
  stat_summary(fun.y=mean, geom='bar', position=position_dodge(width=0.8)) +
  stat_summary(fun.data=mean_cl_normal, geom="linerange",fun.args=list(mult=1),
               color='black', position=position_dodge(0.8)) 
  

pdf("Plots/reasons_elites_5.pdf")


p+xlab("\nImportance of factors in getting ahead") +
  theme(legend.title=element_blank(), axis.title.y=element_blank()) +
  scale_y_continuous(labels=c("Not important at all", "Not very important", 
                              "Fairly important", "Very important", "Essential"), breaks=c(1,2,3,4,5)) +
  coord_cartesian(ylim=c(1,5))+
  scale_x_discrete(labels=addline_format(c("Race", "Parent wealth", "Education", "Delay kids", "Hard work"))) +
  scale_fill_manual(values=c("blue", "red")) +
  theme(axis.text=element_text(size=12)) +
  theme(axis.title=element_text(size=12))



dev.off()


# public
# with 5
df.long <- pivot_longer(df, cols=c(opp_wealth, opp_race, opp_edu, opp_work, opp_nochild), names_to= "reasons", values_to="dv")
without_other <- subset(df.long, partisan=="Republican"|partisan=="Democrat", select=c(reasons, dv, elitenum, partisan))
public <- subset(without_other, elitenum==0, select=c(reasons, dv, elitenum, partisan))
public$reasons <- factor(public$reasons, levels=c("opp_race", "opp_wealth",  "opp_edu", "opp_nochild","opp_work"), ordered=TRUE)
p <- ggplot(public, aes(x=reasons, y=dv, fill=partisan))+
  stat_summary(fun.y=mean, geom='bar', position=position_dodge(width=0.8)) +
  stat_summary(fun.data=mean_cl_normal, geom="linerange",fun.args=list(mult=1),
               color='black', position=position_dodge(0.8)) 

pdf("Plots/reasons_public_5.pdf")


p+xlab("\nImportance of factors in getting ahead") +
  theme(legend.title=element_blank(), axis.title.y=element_blank()) +
  scale_y_continuous(labels=c("Not important at all", "Not very important", 
                              "Fairly important", "Very important", "Essential"), breaks=c(1,2,3,4,5)) +
  coord_cartesian(ylim=c(1,5))+
  scale_x_discrete(labels=addline_format(c("Race", "Parent wealth", "Education", "Delay kids", "Hard work"))) +
  scale_fill_manual(values=c("blue", "red")) +
  theme(axis.text=element_text(size=12)) +
  theme(axis.title=element_text(size=12))


dev.off()



