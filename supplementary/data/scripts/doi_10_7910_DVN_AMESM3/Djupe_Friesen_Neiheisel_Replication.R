# The Political Engagement of Political Scientists: Partisans, Public Scholars, and Teachers/Pedagogues
# 
# Paul A. Djupe, Denison University
# Amanda Friesen, Western University
# Jacob R. Neiheisel, SUNY-Buffalo

#Intalls####
install.packages("tidyverse")
install.packages("rio")
install.packages("remotes")
remotes::install_github("ryanburge/socsci")
install.packages("car")
install.packages("showtext")
install.packages("jtools")
install.packages("psych")
install.packages("modelsummary")
install.packages("ggridges")
install.packages("huxtable")
install.packages("officer")
install.packages("patchwork")
install.packages("paletteer")

#Libraries####
library(rio)
library(socsci)
library(tidyverse)
library(MASS)
library(car)
library(psych)
library(modelsummary)
library(ggridges)
library(showtext)
font_add_google("EB Garamond", "G", regular.wt = 400)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)
library(jtools)
library(patchwork)
library(paletteer)

theme_ps <- function() {
  theme_minimal() %+replace%
    theme(text=element_text(family="G", size=12),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.position = "bottom",
          plot.title.position = "plot")
}


#Loading Data -- specify the directory of the data
psdir <- "REPLACE WITH YOUR FILE DIRECTORY"

ps <- import(paste0(psdir,"ps_activity_data.csv"))


#Figure 1 – The Distribution of Involvement in Politics Among Political Scientists####
ps %>% gather(key="acts", value="actans", q14_1, q14_2, q14_3, q14_4, q14_5, q14_6, q14_7, q14_8, q14_9, q14_10, q14_11, q14_12, q14_13, q14_14, q14_15, na.rm=T) %>% 
  mutate(acts=frcode(acts=="q14_3" ~ "Register others",
                     acts=="q14_5" ~ "Work for candidate",
                     acts=="q14_8" ~ "Stickers/buttons",
                     acts=="q14_4" ~ "GOTV",
                     acts=="q14_2" ~ "Register students",
                     acts=="q14_10" ~ "Post stance on SM",
                     acts=="q14_1" ~ "Protest/Rally",
                     acts=="q14_9" ~ "Yard signs",
                     acts=="q14_15" ~ "Blog",
                     acts=="q14_6" ~ "Op-ed writing",
                     acts=="q14_14" ~ "Advice to govt",
                     acts=="q14_11" ~ "Host campus event",
                     acts=="q14_7" ~ "Donate",
                     acts=="q14_13" ~ "Give community talk",
                     acts=="q14_12" ~ "Media interviews")) %>% 
  mutate(actans=frcode(actans==1 ~ "Already Done",
                       actans==2 ~ "Plan to do",
                       actans==3 ~ "Have done, will do",
                       actans==4 ~ "Neither/won't")) %>% 
  group_by(acts) %>% ct(actans, show_na = F, wt=wgt) %>% 
  ggplot(aes(x=acts, y=pct, fill=as.factor(actans))) + 
  geom_col(position=position_stack(reverse=T), color="white", alpha=.6) +
  geom_text(aes(label=round(pct*100, 0)), position=position_stack(vjust=.5, reverse=T), family="G") +
  theme_ps() +
  scale_fill_manual(name="", values=c("#4C3425FF","burlywood4", "burlywood3", "#9FBAD3FF")) +
  coord_flip() +
  labs(x="", y="") +
  scale_y_continuous(labels=percent_format(accuracy = 1)) +
  theme(panel.grid.major.x = element_line(size=.25, color="gray80"),
        panel.grid.major.y = element_blank())

ggsave(file=paste0(psdir, "Fig_1.tiff"), height=5, width=6, dpi=300)

#Figure 2 – Distribution of Political Engagement####

ps <- ps %>% mutate(polact=rowSums(dplyr::select(., starts_with("q14_c")), na.rm = TRUE))

ps %>% filter(wgt!="NA") %>% mean_ci(polact, wt=wgt) #wtd is 5.9

ps %>% ct(polact, show_na=F, wt=wgt) %>% 
  ggplot(aes(x=polact, y=pct)) + 
    geom_vline(xintercept = 5.91, color="red") +
  geom_col() +
  geom_text(aes(label=paste0(round(pct*100,0), "%")), 
            color="white", family="G", nudge_y=-.005) +
  theme_ps() +
  labs(x="Political Activities Distribution", y="") +
  scale_y_continuous(labels=percent_format(accuracy = 1)) +
  annotate("text", family="G", label="Mean=5.9", x=7.2, y=.125)

ggsave(file=paste0(psdir, "Fig_2.tiff"), height=3.5, width=5, dpi=300)

#Comments about the correlation between q15_10r and q15_11r - ####
# the politics of state oversight of higher ed.
ps %>% corr(q15_10r, q15_11r)

#Figure 3 – Political Scientist Beliefs about the Place of Politics in the Profession####
psmeans <- ps %>% gather(key="q15qs", value="q15ans", q15_1r, q15_2r, q15_3r, q15_4r, q15_5r, q15_6r, 
              q15_7r, q15_8r, q15_9r, q15_10r, q15_11r, na.rm=T) %>% 
  group_by(q15qs) %>% mean_ci(q15ans) %>% mutate(rank=rank(-mean))

pscts <- ps %>% gather(key="q15qs", value="q15ans", q15_1r, q15_2r, q15_3r, q15_4r, q15_5r, q15_6r, 
              q15_7r, q15_8r, q15_9r, q15_10r, q15_11r, na.rm=T) %>% 
  group_by(q15qs) %>% ct(q15ans, show_na = F)

pss <- left_join(pscts, psmeans, by="q15qs")

pss <- pss %>% mutate(factor=frcode((q15qs=="q15_1r" | q15qs=="q15_5r" | q15qs=="q15_6r") ~ "Beliefs about mobilizing the public",
                                    (q15qs=="q15_2r" | q15qs=="q15_3r" | q15qs=="q15_4r" | q15qs=="q15_10r" | q15qs=="q15_11r") ~ "Beliefs about the value of impartiality",
                                    (q15qs=="q15_7r" | q15qs=="q15_8r" | q15qs=="q15_9r") ~ "Beliefs about being a political role model"))


#Political Example - Beliefs about being a political role model
rolemodel <- pss %>% filter(factor=="Beliefs about being a political role model") %>% 
  mutate(q15qs=frcode(q15qs=="q15_7r" ~ "7. My students know how politically\n engaged I am",
                            q15qs=="q15_8r" ~ "8. My students know my partisan affiliation",
                            q15qs=="q15_9r" ~ "9. I feel pressure to stay away from\ncertain topics given the\npolitical climate in my state")) %>% 
  ggplot(aes(x=reorder(q15qs, mean), y=pct, fill=as.factor(q15ans))) +
  geom_col(position=position_stack(reverse=T), color="black", alpha=.7) +
  geom_text(aes(label=round(pct*100, 0)), position=position_stack(vjust=.5, reverse=T), family="G") +
  coord_flip() +
  facet_wrap(~factor, ncol=1) +
  theme_ps() +
  scale_fill_manual(name="", 
                    values=c("#4C3425FF", "#7F6552FF","gray90", "#9FBAD3FF", "#5A8093FF"),
                    labels=c("Strongly\nDisagree", "Disagree", "Neither", 
                             "Agree", "Strongly\nAgree")) +
  labs(x="", y="") +
  scale_y_continuous(labels=percent_format(accuracy = 1)) +
  theme(panel.grid.major.x = element_line(size=.25, color="gray80"),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(size=16))

#"Beliefs about mobillizing the public"
mobilize <- pss %>% filter(factor=="Beliefs about mobilizing the public") %>% 
  mutate(q15qs=frcode(q15qs=="q15_1r" ~ "1. It is my duty to get involved as\nan example to other citizens",
                      q15qs=="q15_5r" ~ "5. Educating the public is an essential\npart of my career",
                      q15qs=="q15_6r" ~ "6. As part of my job, I should be\nencouraging my students to be\npolitically active")) %>% 
  ggplot(aes(x=reorder(q15qs, mean), y=pct, fill=as.factor(q15ans))) +
  geom_col(position=position_stack(reverse=T), color="black", alpha=.7) +
  geom_text(aes(label=round(pct*100, 0)), position=position_stack(vjust=.5, reverse=T), family="G") +
  coord_flip() +
  facet_wrap(~factor, ncol=1) +
  theme_ps() +
  scale_fill_manual(name="", 
                    values=c("#4C3425FF", "#7F6552FF","gray90", "#9FBAD3FF", "#5A8093FF"),
                    labels=c("Strongly\nDisagree", "Disagree", "Neither", 
                             "Agree", "Strongly\nAgree")) +
  labs(x="", y="") +
  scale_y_continuous(labels=percent_format(accuracy = 1)) +
  theme(panel.grid.major.x = element_line(size=.25, color="gray80"),
        panel.grid.major.y = element_blank(),
        legend.position = "",
        axis.text.x = element_blank(),
        axis.title=element_blank(),
        plot.margin=grid::unit(c(0,0,0,0),"cm"),
        plot.title = element_text(size=16))


#"Beliefs about the value of impartiality"
 
impartial <- pss %>% filter(factor=="Beliefs about the value of impartiality") %>% 
  mutate(q15qs=frcode(q15qs=="q15_2r" ~ "2. It is my duty to remain impartial\nin political matters to\nmaintain our objectivity",
                                   q15qs=="q15_3r" ~ "3. The public trusts academics less\n when they take political\nstands in public",
                                   q15qs=="q15_4r" ~ "4. The public trusts academics more\n when they take stands on\nsalient political issues",
                                   q15qs=="q15_10r" ~ "10. Activist academics have encouraged\nstate legislatures to clamp\ndown on higher ed",
                                   q15qs=="q15_11r" ~ "11. Conservatives are unfairly targeting\ncolleges and universities")) %>% 
  ggplot(aes(x=reorder(q15qs, mean), y=pct, fill=as.factor(q15ans))) +
  geom_col(position=position_stack(reverse=T), color="black", alpha=.7) +
  geom_text(aes(label=round(pct*100, 0)), position=position_stack(vjust=.5, reverse=T), family="G") +
  coord_flip() +
  facet_wrap(~factor, ncol=1) +
  theme_ps() +
  scale_fill_manual(name="", 
                    values=c("#4C3425FF", "#7F6552FF","gray90", "#9FBAD3FF", "#5A8093FF"),
                    labels=c("Strongly\nDisagree", "Disagree", "Neither", 
                             "Agree", "Strongly\nAgree")) +
  labs(x="", y="") +
  scale_y_continuous(labels=percent_format(accuracy = 1)) +
  theme(panel.grid.major.x = element_line(size=.25, color="gray80"),
        panel.grid.major.y = element_blank(),
        legend.position = "",
        plot.title=element_text(size=16),
        axis.text.x = element_blank(),
        axis.title=element_blank(),
        plot.margin=grid::unit(c(0,0,0,0),"cm"))

mobilize + impartial + rolemodel + plot_layout(ncol=1, heights=c(.25, .5, .25))
ggsave(file=paste0(psdir, "Fig_3.tiff"), height=8, width=6.5, dpi=300)


#Table A1 - PCA of Belief Dimensions####
beliefs <- ps %>% dplyr::select(q15_1r,q15_2r,q15_3r,q15_4r,q15_5r,q15_6r,q15_7r,q15_8r,q15_9r,q15_10r,q15_11r)
pca_beliefs <- principal(beliefs, nfactors=3, rotate = "varimax", cor="cor")

ps <- ps %>% mutate(beliefs1=(q15_2r+q15_3r+(4-q15_4r)+q15_10r+q15_11r)/20) #impartiality
ps <- ps %>% mutate(beliefs2=(q15_1r+q15_5r+q15_6r)/12)  #mobilizing
ps <- ps %>% mutate(beliefs3=(q15_7r+q15_8r+(4-q15_9r))/12) #role model

rm(pca_beliefs, beliefs)

#Figure A1
ps %>% gather(key="belqs", value="belans", beliefs1, beliefs2, beliefs3, na.rm=T) %>% 
  mutate(belqs=frcode(belqs=="beliefs1" ~ "Beliefs about the value\nof impartiality",
                      belqs=="beliefs2" ~ "Beliefs about mobilizing\nthe public",
                      belqs=="beliefs3" ~ "Beliefs about being a\npolitical role model")) %>% 
  ggplot(aes(x=belans, y=belqs, fill=belqs)) + 
  geom_density_ridges(rel_min_height=.02, quantiles=2, quantile_lines=T,
                      alpha=.7, scale=1.25) +
  scale_x_continuous(breaks=seq(0,1,.25)) +
  scale_fill_manual(values=c("#4C3425FF","#5A8093FF", "#7F6552FF")) +
  theme_ps() +
  theme(panel.grid.major.x = element_line(linewidth=.5),
        panel.grid.major.y = element_blank(),
        legend.position = "") +
  labs(x="Belief Agreement", y="")
ggsave(file=paste0(psdir, "Fig_A1.tiff"), height=5, width=5.5, dpi=300)


#Table A2 - Models of Belief Dimensions

lmb1 <- lm(beliefs1 ~ rank+white+female+phd+reviews20sd+pid7+quant+fields+q16_1r, data=ps, weight=wgt)
summ(lmb1)
lmb2 <- lm(beliefs2 ~ rank+white+female+phd+reviews20sd+pid7+quant+fields+q16_1r, data=ps, weight=wgt)
summ(lmb2)
lmb3 <- lm(beliefs3 ~ rank+white+female+phd+reviews20sd+pid7+quant+fields+q16_1r, data=ps, weight=wgt)
summ(lmb3)

export_summs(lmb1, lmb2, lmb3, 
             error_format = "({p.value})", error_pos = "below",
             model.names = c("Public Trust", "Individual\nExample", "Individual/nReputation"), 
             to.file="docx", file.name = paste0(psdir, "Table_A2.docx"))

#Figure A2 – Beliefs about Political Engagement, Mean Scores by Field ####

ps %>% filter(wgt!="NA") %>% mean_ci(beliefs1, wt=wgt)
ps %>% filter(wgt!="NA") %>% mean_ci(beliefs2, wt=wgt)
ps %>% filter(wgt!="NA") %>% mean_ci(beliefs3, wt=wgt)


belq_mean <- data.frame(belqs=c("beliefs1", "beliefs2", "beliefs3"),
                        hline=c(.627, .673, .422))


psbeliefs <- ps %>% filter(fields!="NA", wgt!="NA") %>% 
  gather(key="belqs", value="belans", beliefs1, beliefs2, beliefs3, na.rm=T) %>% 
  group_by(fields, belqs) %>% mean_ci(belans, wt=wgt)
  
psbeliefs <- left_join(psbeliefs, belq_mean, by="belqs")
  

psbeliefs %>% mutate(belqs=frcode(belqs=="beliefs1" ~ "Beliefs about the value\nof impartiality",
                                  belqs=="beliefs2" ~ "Beliefs about mobilizing\nthe public",
                                  belqs=="beliefs3" ~ "Beliefs about being a\npolitical role model")) %>%  
  ggplot(aes(x=fields, y=mean, fill=fields)) + 
  geom_col() +
  geom_hline(aes(yintercept=hline)) +
  facet_wrap(~belqs) +
  coord_flip() +
  scale_fill_paletteer_d("IslamicArt::damascus") +
  theme_ps() + 
  theme(legend.position = "") +
  labs(x="", y="Mean Score", caption="Note: The vertical line is the mean.")
ggsave(file=paste0(psdir, "Fig_A2.tiff"), height=8, width=6.5, dpi=300)


#Table A3 - Principal Components Analysis of Political Activities####
acts <- ps %>% dplyr::select(q14_c1, q14_c2, q14_c3, q14_c4, q14_c5, q14_c6, q14_c7, q14_c8, q14_c9, q14_c10, q14_c11, q14_c12, q14_c13, q14_c14, q14_c15)
principal(acts, nfactors=3, rotate = "varimax", cor="tet")

#actsnc is a 0-1 scale, while the actsn variable is a count 
ps <- ps %>% mutate(acts1c=(q14_c1+q14_c4+q14_c5+q14_c7+q14_c8+q14_c9+q14_c10)/7,
                    acts2c=(q14_c6+q14_c12+q14_c13+q14_c14+q14_c15)/5,
                    acts3c=(q14_c2+q14_c3+q14_c11)/3,
                    acts1=(q14_c1+q14_c4+q14_c5+q14_c7+q14_c8+q14_c9+q14_c10),
                    acts2=(q14_c6+q14_c12+q14_c13+q14_c14+q14_c15),
                    acts3=(q14_c2+q14_c3+q14_c11))


ps %>% filter(wgt!="NA") %>% mean_ci(acts1c, wt=wgt) #m=.345 partisans
ps %>% filter(wgt!="NA") %>% mean_ci(acts2c, wt=wgt) #m=.498 public scholars
ps %>% filter(wgt!="NA") %>% mean_ci(acts3c, wt=wgt) #m=.359 pedagogues


#Note 5 - correlations among activity types ####
ps %>% dplyr::select(acts1c, acts2c, acts3c) %>%
  datasummary_correlation(.)


#Figure 4 - How Types of Activities are Pursued by Academic Interest Areas####
psacts <- ps %>% gather(key="act_types", value="act_scores", acts1c, acts2c, acts3c, na.rm=T)

psacts %>% filter(fields!="NA", wgt!="NA") %>% 
  group_by(fields, act_types) %>% mean_ci(act_scores, ci=.84, wt=wgt) %>% 
  ggplot(aes(x=act_types, y=mean, fill=act_types)) + 
  geom_col(position=position_dodge(width=.9), color="black") +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1,
                position=position_dodge(width=.9)) +
  geom_text(aes(y=.07, label=paste0(round(mean*100,0),"%")), family="G", color="white",
            position=position_dodge(width=.9)) +
  coord_flip() +
  scale_x_discrete(labels=c("Partisan","Public", "Pedagogue")) +
  scale_y_continuous(labels=percent_format(accuracy = 1)) +
  scale_fill_manual(name="Activity Types",
                    labels=c("Partisan","Public Scholar", "Pedagogue"),
                    values=c("skyblue2", "darkorange2", "gray70"),
                    guide = guide_legend(reverse = TRUE)) +
  theme_ps() +
  labs(y="Amount of Activity", x="") + 
  facet_wrap(~fields) +
  theme(strip.background = element_rect(fill="gray80"),
        legend.position = c(.8, .15),
        panel.grid.major.x = element_line(),
        panel.grid.major.y = element_blank())

ggsave(file=paste0(psdir, "Fig_4.tiff"), height=6, width=6.5, dpi=300)

#Figure A3  –  How Political Engagement Types are Linked to Various Beliefs about Such Engagement####

psacts %>% gather(key="att", value="attans", q15_1r, q15_2r, q15_3r, q15_4r, q15_5r, q15_6r, 
                  q15_7r, q15_8r, q15_9r, q15_10r, q15_11r, na.rm=T) %>% 
  mutate(att=frcode(att=="q15_1r" ~ "My duty to get involved\n as an example",
                    att=="q15_2r" ~ "My duty to remain\n impartial",
                    att=="q15_3r" ~ "Public trusts academics\nLESS when they take\npolitical stands",
                    att=="q15_4r" ~ "Public trusts academics\nMORE when they take\npolitical stands",
                    att=="q15_5r" ~ "Educating the public is an\nessential part of my career",
                    att=="q15_6r" ~ "I should be encouraging\nmy students to be\n politically active",
                    att=="q15_7r" ~ "My students know how\npolitically engaged I am.",
                    att=="q15_8r" ~ "My students know my\npartisan affiliation",
                    att=="q15_9r" ~ "I stay away from certain\ntopics given the climate\nin my state",
                    att=="q15_10r" ~ "Activist academics\n have encouraged state legs to\nclamp down on higher ed",
                    att=="q15_11r" ~ "Conservatives are unfairly\ntargeting higher ed")) %>% 
  ggplot(aes(x=attans, y=act_scores, color=act_types, linetype=act_types)) + 
  geom_smooth() + 
  facet_wrap(~att, ncol=3) +
  scale_x_continuous(breaks=seq(0,4,1), labels=c("SD", "D", "N", "A", "SA")) +
  scale_color_discrete(name="Activity Types", labels=c("Partisans", "Public Scholar", "Teacher/Citizen")) +
  scale_linetype_discrete(name="Activity Types", labels=c("Partisans", "Public Scholar", "Teacher/Citizen")) +
  theme_ps() +
  scale_y_continuous(labels=percent_format(accuracy = 1)) +
  labs(x="Agreement with Each Belief", y="Amount of Each Activity Type")
ggsave(file=paste0(psdir, "Fig_A3.tiff"), height=12, width=9, dpi=300)


#Figure 5  – Estimated Effects on Activity Types (Negative Binomial Model)####

lma1 <- glm.nb(acts1 ~ rank+white+female+phd+reviews20sd+beliefs1+beliefs2+beliefs3+pid7+quant+theorist, data=ps, weight=wgt)
summ(lma1)
vif(lma1)

lma2 <- glm.nb(acts2 ~ rank+white+female+phd+reviews20sd+beliefs1+beliefs2+beliefs3+pid7+quant+theorist, data=ps, weight=wgt)
summ(lma2)
vif(lma2)

lma3 <- glm.nb(acts3 ~ rank+white+female+phd+reviews20sd+beliefs1+beliefs2+beliefs3+pid7+quant+theorist, data=ps, weight=wgt)
summ(lma3)
vif(lma3)

plot_summs(lma1, lma2, lma3, 
           model.names = c("Partisan", "Public\nScholar", "Pedagogue"),
           coefs=c("Associate"="rankAssociate",
                   "Full"="rankFull",
                   "Other"="rankOther",
                   "White"="white",
                   "Women"="femaleWomen",
                   "PhD Granting"="phdPhD Program",
                   "Reviewing Load"="reviews20sd",
                   "Beliefs: impartiality"="beliefs1",
                   "Beliefs: mobilizing"="beliefs2",
                   "Beliefs: role model"="beliefs3",
                   "Partisanship"="pid7",
                   "Quantitative scholar"="quant",
                   "Theorist"="theorist")) +
  theme_ps() +
  labs(y="", x="Estimated Effect on Political Activity")
ggsave(file=paste0(psdir, "Fig_5.tiff"), height=6.5, width=5, dpi=300)


#Table A4 - Estimates of the Three Activity Types  (negative binomial results)####
export_summs(lma1, lma2, lma3, 
             error_format = "({p.value})", error_pos = "below",
             to.file="docx", file.name = paste0(psdir, "Table_A4.docx"),
             model.names = c("Partisan", "Public\nScholar", "Pedagogue"),
             coefs=c("Associate"="rankAssociate",
                     "Full"="rankFull",
                     "Other"="rankOther",
                     "White"="white",
                     "Women"="femaleWomen",
                     "PhD Granting"="phdPhD Program",
                     "Reviewing Load"="reviews20sd",
                     "Beliefs: Impartiality"="beliefs1",
                     "Beliefs: mobilizing"="beliefs2",
                     "Beliefs: role model"="beliefs3",
                     "Partisanship"="pid7",
                     "Quantitative scholar"="quant",
                     "Theorist"="theorist")) 

#Checking for hypocrisy -- mention in Note 12####

ps <- ps %>% mutate(acts1cc=car::recode(acts1c, "0=0; .2:1=1"))

ps %>% ggplot(aes(x=beliefs1, y=acts1cc)) + 
  geom_smooth() +
  theme_ps() +
  scale_y_continuous(breaks=seq(0,1,.25), labels=percent_format(accuracy = 1)) +
  labs(y="At least one partisan act", 
       x="Beliefs about the value of impartiality") 



#Removes####
rm(impartial, mobilize, rolemodel)
rm(belq_mean, beliefs, psbeliefs)
rm(acts, psacts, ps, pss, theme_ps, pscts, psmeans)
rm(list=ls(pattern="lm"))
rm(psdir)
