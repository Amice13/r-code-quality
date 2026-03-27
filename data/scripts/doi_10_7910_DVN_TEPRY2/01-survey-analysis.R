## Preamble --------------------------------------------------------------------
library(foreign)
library(tidyverse)
library(janitor)


# plotting w/ custom colors (optional)
red_mit = '#A31F34'
blue_mit = '#315485'

## create subfolder for survey results:
if(!dir.exists("survey")){
  dir.create("survey")
}


## Load data -------------------------------------------------------------------
## this file only includes the relevant columns to reproduce this figure: the respondent ID column created by Qualtrics, their self-reported partisanship, and their response to the question about multi-family housing, which is 0 if they did not support multi-family housing everywhere and 1 if they did.
dat <- read_rds(file = "survey_multifamily_recoded.rds")

## Output overall sample size
writeLines(prettyNum(nrow(dat),big.mark = ","),"survey/n_national.tex",sep="%") #

## Figure 1 ##
multifamily_bypid_tab <- filter(dat,!is.na(pid) & pid!="Independent") %>%
  group_by(pid) %>%
  summarize(multifamily = mean(housing_tradeoff<0.5,na.rm=T))
dem_avg <- filter(multifamily_bypid_tab,pid=="Democratic") %>% select(multifamily)
rep_avg <- filter(multifamily_bypid_tab,pid=="Republican") %>% select(multifamily)
writeLines(as.character(abs(100*round(rep_avg-dem_avg,2))),
           con = "survey/multifamily_diffpid.tex",sep="%")

pid_diff_test <- t.test(dat$housing_tradeoff[dat$pid=="Democratic"]<0.5,dat$housing_tradeoff[dat$pid=="Republican"]<0.5)
writeLines(as.character(round(pid_diff_test$p.value,2)),
           con = "survey/multifamily_diffpid_pval.tex",sep="%")

housingpol_sep_bypid_tab <- multifamily_bypid_tab %>%
  pivot_longer(cols=c(multifamily), names_to = "question") %>%
  mutate(pid2 = recode(pid,"Democratic"="Democrats","Republican"="Republicans"))


## Figure 1:
(multifamily_diffs_bypid <- ggplot(filter(housingpol_sep_bypid_tab,question=="multifamily")) + 
    geom_line(aes(y=question,x=value,group=question),lwd=3,col="grey") + 
    geom_point(aes(x=value,y=question,col=pid),size=4) +
    geom_text(aes(x=value+ifelse(pid=="Democratic",0.08,-0.08),y=question,label=paste0(100*round(value,2),"%"))) + 
    geom_text(data=filter(housingpol_sep_bypid_tab,question=="multifamily"),aes(x=value+ifelse(pid=="Democratic",0.08,-0.08),y=question,label=paste0(100*round(value,2),"%"))) + 
    geom_text(data=filter(housingpol_sep_bypid_tab,question=="multifamily"),aes(x=value+ifelse(pid=="Democratic",0.08,-0.08),y=question,label=paste0(pid2,":")),nudge_y=0.2) + 
    scale_color_manual("Party ID:",breaks=c("Democratic","Independent","Republican"),
                       values = c(blue_mit,"black",red_mit)) + 
    scale_y_discrete("",labels="") + 
    scale_x_continuous("Percent support allowing multifamily housing everywhere",labels=scales::percent_format(accuracy=1),limits=c(0,1)) + 
    theme_minimal() + 
    theme(legend.pos="none",legend.direction = "horizontal",panel.grid = element_blank(),axis.line.x = element_line(),axis.ticks.x = element_line())
)
ggsave(multifamily_diffs_bypid,filename = "survey/multifamily_diffs_bypid_national.pdf",height=1.5,width=5.5)
