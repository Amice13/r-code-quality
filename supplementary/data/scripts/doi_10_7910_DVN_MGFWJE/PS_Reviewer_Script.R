#Replication File for "Reviewing and the State of the Discipline"
#Forthcoming at PS: Political Science and Politics
#Authors: Paul A. Djupe (djupe@denison.edu) and Brooklyn Walker

#Installs and Libraries####
install.packages("tidyverse")
install.packages("rio")
install.packages("remotes")
remotes::install_github("ryanburge/socsci")
install.packages("patchwork")
install.packages("marginaleffects")
install.packages("showtext")

library(tidyverse)
library(rio)
library(socsci)
library(patchwork)
library(marginaleffects)
library(showtext)
font_add_google("EB Garamond", "G", regular.wt = 400)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)


theme_djupe <- function() {
  theme_minimal() %+replace%
    theme(text=element_text(family="G", size=12),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.position = "bottom",
          plot.title.position = "plot")
  
}

#Loading the data -- add your file location
pss <- import("<filelocation>/ps_reviewer.csv")


#Figure 1 - The Distribution of Reviewing in 2013 and 2024 by Political Scientists

pss %>% group_by(year) %>% ct(reviews20, show_na = F) %>% 
  ggplot(aes(x=reviews20, y=pct, color=as.factor(year), fill=as.factor(year))) +
  geom_col(alpha=.7, position="identity") +
  scale_color_manual(name="Year", values=c("black", "NA"), labels=c("2013", "2024")) +
  scale_fill_manual(name="Year", values=c("NA", "skyblue2"), labels=c("2013", "2024")) +
  labs(x="Number of Reviews Completed",y="") + 
  theme_djupe() +
  scale_y_continuous(labels=percent_format(accuracy = 1)) +
  scale_x_continuous(breaks=seq(0,20,5), 
                     labels=c("0", "5", "10", "15", "20+"))


#Figure 2  – Distribution of Reviewing by Rank and Program, 2013-2024####
pss %>% filter(rank!="NA", rank!="Other", phd!="NA") %>% 
  group_by(rank, phd, year) %>% 
  mean_ci(reviews20, ci=.84) %>% 
  ggplot(aes(x=year, color=phd, y=mean, ymin=lower, ymax=upper, 
             group=phd)) + 
  geom_point() + 
  geom_line() +
  geom_errorbar(width=.05) +
  facet_wrap(~rank) +
  theme_djupe() + 
  theme(strip.background = element_rect(fill="gray80")) +
  labs(x="", y="Mean Number of Reviews") +
  scale_color_manual(name="PhD Program?", labels=c("No", "Yes"),
                     values=c("skyblue3", "black")) +
  scale_x_continuous(breaks=c(2013, 2023.5), labels=c("2013", "2024"))


#Figure 3  – How the Review Uptake Rate Has Shifted by Year, Rank, and Review Requests####

lmacc <- lm(revpct ~ year*rank+phd+female+white+year*asks50, data=pss)
summary(lmacc)
ps_rate <- plot_slopes(lmacc, variables="year", condition=c("rank"), conf_level = .90) +
  geom_hline(yintercept=0, color="black") + 
  theme_minimal()+ 
  coord_flip() +
  labs(title="",
       y="Marginal Effect of 2024 on Acceptance Rate", x="") +
  theme_minimal() +
  theme(text=element_text(family="G"))

pp_rate <- plot_predictions(lmacc, condition=c("asks50", "year")) + 
  theme_djupe() + 
  labs(x="Number of Review Requests", y="Acceptance Rate")

pp_rate+ps_rate


#Figure 4  –  How Select Beliefs and Attitudes about Reviewing Have Shifted from 2013 to 2024 by Rank and PhD Program####
pss4 <- pss %>% filter(phd!="NA", rank!="NA") %>% 
  gather(key="revs", value="revsans", rev6, rev7, rev8, rev9, rev11, rev12, na.rm=T) %>% 
  mutate(revs=frcode(revs=="rev6" ~ "I am often asked to PR\nthat I am not qualified to judge",
                     revs=="rev7" ~ "Gatekeeping research is an\nimportant reason to PR",
                     revs=="rev8" ~ "Being asked to PR is a measure\nof professional stature",
                     revs=="rev9" ~ "Recognition by editors is\nan important reason to PR",
                     revs=="rev11" ~ "PR *is* important \n service that counts twd T/P",
                     revs=="rev12" ~ "PR *should* be considered\nservice that counts twd T/P")) %>% 
  group_by(revs, rank, phd, year) %>% mean_ci(revsans, ci=.76)

pss4 %>% filter(rank!="Other") %>% 
  ggplot(aes(x=rank, y=mean, color=phd, shape=as.factor(year), ymin=lower, ymax=upper)) +
  geom_errorbar(width=.1, position=position_dodge(width=.5)) +
  geom_point(position=position_dodge(width=.5), fill="white", size=1.75) +
  scale_shape_manual(name="", values=c(19, 21)) +
  scale_color_manual(name="", values=c("skyblue2", "black")) +
  facet_wrap(~revs) +
  theme_djupe() + 
  theme(strip.background = element_rect(fill="gray80")) +
  labs(x="", y="Mean Agreement")


#Figure 5  –  Support for Reviewing Norms by Rank and PhD Program, 2024####
pss %>% filter(rank!="NA", rank!="Other") %>% 
  gather(key="new", value="newans", q25_6r, q25_7r, q25_8r, na.rm=T) %>% 
  mutate(new=frcode(new=="q25_6r" ~ "We should review 3 papers\n for every one we submit",
                    new=="q25_7r" ~ "I do not review for journals\nI don't submit to",
                    new=="q25_8r" ~ "Given how much $ publishers make,\nI resent reviewing for free")) %>% 
  group_by(new, rank, phd) %>% mean_ci(newans, ci=.84) %>% 
  ggplot(aes(x=rank, color=phd, ymin=lower, ymax=upper, y=mean, group=phd)) + 
  geom_errorbar(width=.1, position=position_dodge(width=.6)) +
  geom_line(position=position_dodge(width=.6)) + 
  geom_point(position=position_dodge(width=.6)) +
  facet_wrap(~new) +
  labs(x="", y="Mean Agreement") + 
  theme_djupe() +
  theme(strip.background = element_rect(fill="gray80")) +
  scale_color_manual(name="", values=c("skyblue3", "black"))


#Removes####
rm(pss, pss4, pp_rate, ps_rate)
