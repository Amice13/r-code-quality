####################################################################################
##Replication files for all figures in
##Selecting for Masculinity: Women's Under-Representation in the Republican Party
##Christopher F. Karpowitz, J. Quin Monson, Jessica Robinson Preece, Alejandra Aldridge
##American Political Science Review
#####################################################################################

library(pacman)
p_load(varhandle, tidyverse, foreign, arm, stargazer, reshape2, data.table, 
       ggplot2, psych, ggmap, mapproj, mapdata, maptools, maps, sp, readstata13, 
       plyr, ggrepel, ztable, magrittr, scales, ggpubr, margins, marginaleffects, 
       here)


##Set working directory to location of replication folder
i_am("Selecting for Masculinity_All Figures_Replication.R")


###########################################################
##Load datasets
###########################################################

caucus_2016_double <-read.dta13("Figures/caucus_2016_db.dta", nonint.factors = TRUE)
caucus_2016_vote <-read.dta13("Figures/caucus_2016_vote_basic.dta", nonint.factors = TRUE)
caucus_2016_vote_ideo <-read.dta13("Figures/caucus_2016_vote_ideo.dta", nonint.factors = TRUE)
caucus_2016_vote_gender <-read.dta13("Figures/caucus_2016_vote_gender.dta", nonint.factors = TRUE)
caucus_2016_vote_gender_strongcon <-read.dta13("Figures/caucus_2016_vote_gender_strongcon.dta", nonint.factors = TRUE)

cces_combined_double <-read.dta13("Figures/cces_db.dta", nonint.factor = TRUE)
cces_combined_vote_byparty <-read.dta13("Figures/cces_vote_byparty.dta", nonint.factor = TRUE)
cces_combined_vote_byideo <- read.dta13("Figures/cces_vote_byideo.dta", nonint.factors = TRUE)
#cces_combined_vote_anyprimary <- read.dta13("Figures/cces_vote_anyprim.dta", nonint.factors = TRUE)
#cces_combined_vote_anyprim_byprim <- read.dta13("Figures/cces_vote_anyprim_byprim.dta", nonint.factors = TRUE)

caucus_2018_db <-read.dta13("Figures/caucus_2018_db.dta", nonint.factors = TRUE)
caucus_2018_db2 <-read.dta13("Figures/caucus_2018_db_2.dta", nonint.factors = TRUE)

caucus_2018_vote <-read.dta13("Figures/caucus_2018_vote.dta", nonint.factors = TRUE)
caucus_2018_vote_strongcon <-read.dta13("Figures/caucus_2018_vote_strongcon.dta", nonint.factors = TRUE)
caucus_2018_vote_othercon <-read.dta13("Figures/caucus_2018_vote_othercon.dta", nonint.factors = TRUE)
caucus_2018_vote_reshape <-read.dta13("Figures/caucus_2018_vote_reshape.dta", nonint.factors = TRUE)
caucus_2018_vote_strongcon_reshape <-read.dta13("Figures/caucus_2018_vote_strongcon_reshape.dta", nonint.factors = TRUE)

win_observe <- read.dta13("Figures/win_observe_marginfx.dta", nonint.factors = TRUE)



#################################################
## 2016-18 CCES Combined Experiment

##Figure 1
##Double Bind Figure -- Democrats
cces_combined_double$trait <- factor(cces_combined_double$trait, levels = c("Likeability", "Competence"))

cces_db_dem <- ggplot(data = subset(cces_combined_double, primary=="dem"), aes(x=treat, y=diff, ymin=diff-se*qnorm(.975), ymax=diff+se*qnorm(.975), fill=trait)) + 
  expand_limits(y=c(-0.3,0.3)) + 
  geom_bar(colour="black", width=.8, stat="identity", position=position_dodge()) +
  scale_fill_manual(values=c("#af8dc3", "#7fbf7b")) + 
  geom_errorbar(aes(), colour="black", width=.1, position=position_dodge(.8)) +
  #guides(fill=FALSE) +
  #xlab("\nExperimental Condition") + 
  scale_x_discrete(name="\nExperimental Condition")+
  ylab("Gender Difference in Trait Assessments \n(Higher Scores Favor Woman Candidate)\n") +
  #ggtitle("Candidate Likeability \nby Experimental Condition\n") + 
  theme_bw()+
  theme(axis.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 10)) +
  ggtitle("Democrats") +
  theme(plot.title = element_text(size = 14)) +
  theme(legend.title=element_blank())
cces_db_dem

##Double Bind Figure -- Republicans

cces_db_gop <- ggplot(data = subset(cces_combined_double, primary=="gop"), aes(x=treat, y=diff, ymin=diff-se*qnorm(.975), ymax=diff+se*qnorm(.975), fill=trait)) + 
  expand_limits(y=c(-0.3,0.3)) + 
  geom_bar(colour="black", width=.8, stat="identity", position=position_dodge()) +
  scale_fill_manual(values=c("#af8dc3", "#7fbf7b")) + 
  geom_errorbar(aes(), colour="black", width=.1, position=position_dodge(.8)) +
  #guides(fill=FALSE) +
  #xlab("\nExperimental Condition") + 
  scale_x_discrete(name="\nExperimental Condition")+
  ylab("Gender Difference in Trait Assessments \n(Higher Scores Favor Woman Candidate)\n") +
  #ggtitle("Candidate Likeability \nby Experimental Condition\n") + 
  theme_bw()+
  theme(axis.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 10)) +
  ggtitle("Republicans") +
  theme(plot.title = element_text(size = 14)) +
  theme(legend.title=element_blank())
cces_db_gop

ggarrange(cces_db_dem, cces_db_gop, labels = c("A", "B"),
          common.legend = TRUE, legend = "bottom",
          nrow = 2)

ggsave("Figures/fig1.pdf", width = 8.5, height = 11)

##Figure 2
##Vote for Julie, by Party
ggplot(data = cces_combined_vote_byparty, aes(x=treat, y=vote_julie, ymin=vote_julie-se*qnorm(.975), ymax=vote_julie+se*qnorm(.975), colour=primary, group=primary)) + 
  expand_limits(y=c(0,1)) + 
  geom_point(size=4, stat="identity", position=position_dodge(0.2)) +
  geom_errorbar(aes(), width=.1, position=position_dodge(.2)) +
  scale_colour_manual(values=c("#2166ac", "#b2182b")) + 
  geom_hline(aes(yintercept=0.5), linetype="dashed")+
  #guides(fill=FALSE) +
  #xlab("\nExperimental Condition") + 
  scale_x_discrete(name="\nExperimental Condition")+
  ylab("Proportion Voting for Woman Candidate") +
  #ggtitle("Probability of Vote for Female Candidate \nby Experimental Condition\n") + 
  theme_bw()+
  theme(axis.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())

ggsave("Figures/fig2.pdf", width = 8, height = 6)


##Figure 3
##Vote for Julie, by conservatism, GOP only

cces_vote_byideo_gop <- ggplot(data = subset(cces_combined_vote_byideo, primary=="Republican"), aes(x=treat, y=vote_julie, ymin=vote_julie-se*qnorm(.975), ymax=vote_julie+se*qnorm(.975), colour=ideo, group=ideo)) + 
  expand_limits(y=c(0,1)) + 
  geom_point(size=4, stat="identity", position=position_dodge(0.2)) +
  geom_errorbar(aes(), width=.1, position=position_dodge(.2)) +
  scale_colour_manual(values=c("#fdae61", "#d7191c")) + 
  geom_hline(aes(yintercept=0.5), linetype="dashed")+
  #guides(fill=FALSE) +
  #xlab("\nExperimental Condition") + 
  scale_x_discrete(name="\nExperimental Condition")+
  ylab("Proportion Voting for Woman Candidate") +
  #ggtitle("Probability of Vote for Female Candidate \nby Experimental Condition\n") + 
  theme_bw()+
  theme(axis.title = element_text(size = 14)) +
  ggtitle("Republicans") +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title=element_blank())+
  theme(legend.position="bottom")
cces_vote_byideo_gop


cces_vote_byideo_dem <- ggplot(data = subset(cces_combined_vote_byideo, primary=="Democrat"), aes(x=treat, y=vote_julie, ymin=vote_julie-se*qnorm(.975), ymax=vote_julie+se*qnorm(.975), colour=ideo, group=ideo)) + 
  expand_limits(y=c(0,1)) + 
  geom_point(size=4, stat="identity", position=position_dodge(0.2)) +
  geom_errorbar(aes(), width=.1, position=position_dodge(.2)) +
  scale_colour_manual(values=c("#67a9cf", "#2166ac")) + 
  geom_hline(aes(yintercept=0.5), linetype="dashed")+
  #guides(fill=FALSE) +
  #xlab("\nExperimental Condition") + 
  scale_x_discrete(name="\nExperimental Condition")+
  ylab("Proportion Voting for Woman Candidate") +
  #ggtitle("Probability of Vote for Female Candidate \nby Experimental Condition\n") + 
  theme_bw()+
  theme(axis.title = element_text(size = 14)) +
  ggtitle("Democrats") +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title=element_blank())+
  theme(legend.position="bottom")
cces_vote_byideo_dem


ggarrange(cces_vote_byideo_gop, cces_vote_byideo_dem, labels = c("A", "B"),
          nrow = 2)

ggsave("Figures/fig3.pdf", width = 8.5, height = 11)


#################################################
## 2016 Caucus Attenders Experiment

##Figure 4
##Double Bind -- predicted from regression models in Appendix
caucus_2016_double$trait <- factor(caucus_2016_double$trait, levels = c("Likeability", "Competence"))
ggplot(data = caucus_2016_double, aes(x=treat, y=diff, ymin=diff-se*qnorm(.975), ymax=diff+se*qnorm(.975), fill=trait)) + 
  expand_limits(y=c(-0.3,0.3)) + 
  geom_bar(colour="black", width=.8, stat="identity", position=position_dodge()) +
  scale_fill_manual(values=c("#af8dc3", "#7fbf7b")) + 
  geom_errorbar(aes(), colour="black", width=.1, position=position_dodge(.8)) +
  #guides(fill=FALSE) +
  #xlab("\nExperimental Condition") + 
  scale_x_discrete(name="\nExperimental Condition",
                   limits=c("Masculine", "Masculine+Mom", "Feminine", "Feminine+Mom", "Mom Only"), labels=c("Masculine", "Masculine Mom", "Feminine", "Feminine Mom", "Mom Only"))+
  ylab("Gender Difference in Trait Assessments \n(Higher Scores Favor Woman Candidate)\n") +
  #ggtitle("Candidate Likeability \nby Experimental Condition\n") + 
  theme_bw()+
  theme(axis.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title=element_blank())+
  theme(legend.position="bottom")

ggsave("Figures/fig4.pdf", width = 8, height = 6)

##Figure 5
##Vote for Julie -- predicted from regression models in Appendix
ggplot(data = caucus_2016_vote, aes(x=treat, y=vote_julie, ymin=vote_julie-se*qnorm(.975), ymax=vote_julie+se*qnorm(.975))) + 
  expand_limits(y=c(0.3,0.7)) + 
  geom_point(size=4, stat="identity", colour="#b2182b") +
  geom_errorbar(aes(), width=.1, colour="#b2182b") +
  geom_hline(aes(yintercept=0.5), linetype="dashed")+
  #geom_text(aes(label=format(vote_julie, digits=2)), fontface="bold", colour="white", size=2, position=position_dodge(0.3))+
  #guides(fill=FALSE) +
  #xlab("\nExperimental Condition") + 
  scale_x_discrete(name="\nExperimental Condition",
                   limits=c("Masculine", "Masculine+Mom", "Feminine", "Feminine+Mom", "Mom Only"), labels=c("Masculine", "Masculine Mom", "Feminine", "Feminine Mom", "Mom Only"))+
  ylab("Proportion Voting for Woman Candidate") +
  #ggtitle("Probability of Vote for Female Candidate \nby Experimental Condition\n") + 
  theme_bw()+
  theme(axis.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title=element_blank())+
  theme(legend.position="bottom")

ggsave("Figures/fig5.pdf", width = 8, height = 6)

##Figure 6
##Vote for Julie by Respondent Ideology -- predicted from regression models in Appendix
ggplot(data = subset(caucus_2016_vote_ideo), aes(x=treat, y=vote_julie, ymin=vote_julie-se*qnorm(.975), ymax=vote_julie+se*qnorm(.975), colour=ideo, group=ideo)) + 
  expand_limits(y=c(0.3,0.7)) + 
  geom_point(size=4, stat="identity", position=position_dodge(0.2)) +
  geom_errorbar(aes(), width=.1, position=position_dodge(.2)) +
  scale_colour_manual(values=c("#fdae61", "#d7191c")) + 
  geom_hline(aes(yintercept=0.5), linetype="dashed")+
  #guides(fill=FALSE) +
  #xlab("\nExperimental Condition") + 
  scale_x_discrete(name="\nExperimental Condition", labels=c("Masculine", "Masculine Mom", "Feminine", "Feminine Mom", "Mom Only"))+
  ylab("Proportion Voting for Woman Candidate") +
  #ggtitle("Probability of Vote for Female Candidate \nby Experimental Condition\n") + 
  theme_bw()+
  theme(axis.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title=element_blank())+
  theme(legend.position="bottom")

ggsave("Figures/fig6.pdf", width = 8, height = 6)


#################################################
## 2018 Caucus Attenders Experiment

##Figure 7
labels <- c("1"="Man Candidate with Masculine Profile", "2"="Man Candidate with Feminine Profile")
caucus_2018_db$trait <- factor(caucus_2018_db$trait, levels = c("Likeability", "Competence"))
labels_profile <- c("Masc"="Masculine", "Masc+"= "Masculine \nParent\n", "Fem"="Feminine", "Fem+"="Feminine \nParent\n")
profile_reorder <- c("Masc"="1", "Masc+"="2", "Fem"="3", "Fem+"="4")
caucus_2018_db$profile_reorder <- factor(caucus_2018_db$profile, levels=c("Masc", "Masc+", "Fem", "Fem+"))

ggplot(data = caucus_2018_db, aes(x=profile_reorder, y=diff, ymin=diff-se*qnorm(.975), ymax=diff+se*qnorm(.975), fill=trait)) + 
  expand_limits(y=c(-0.3,0.3)) + 
  geom_bar(colour="black", width=.8, stat="identity", position=position_dodge()) +
  scale_fill_manual(values=c("#af8dc3", "#7fbf7b")) + 
  geom_errorbar(aes(), colour="black", width=.1, position=position_dodge(.8)) +
  #guides(fill=FALSE) +
  #xlab("\nExperimental Condition") + 
  scale_x_discrete(name="\nWoman Candidate Profile", labels=labels_profile)+
  #scale_x_discrete(name="\nFemale Candidate Profile", labels=c("Masculine", "Masculine \nParent\n", "Feminine", "Feminine \nParent\n"))+
  ylab("Gender Difference in Trait Assessments \n(Higher Scores Favor Woman Candidate)\n") +
  #ggtitle("Candidate Likeability \nby Experimental Condition\n") + 
  facet_wrap(~male_facet, labeller=labeller(male_facet=labels))+
  theme_bw()+
  theme(axis.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())

ggsave("Figures/fig7.pdf", width = 8, height = 6)


##Figure A1
##Now produce a similar figure but the male candidate is described as a parent
labels2 <- c("1"="Man Candidate with Masculine Parent Profile", "2"="Man Candidate with Feminine Parent Profile")

caucus_2018_db2$trait <- factor(caucus_2018_db2$trait, levels = c("Likeability", "Competence"))
caucus_2018_db2$profile_reorder <- factor(caucus_2018_db2$profile, levels=c("Masc", "Masc+", "Fem", "Fem+"))

ggplot(data = caucus_2018_db2, aes(x=profile_reorder, y=diff, ymin=diff-se*qnorm(.975), ymax=diff+se*qnorm(.975), fill=trait)) + 
  expand_limits(y=c(-0.3,0.3)) + 
  geom_bar(colour="black", width=.8, stat="identity", position=position_dodge()) +
  scale_fill_manual(values=c("#af8dc3", "#7fbf7b")) + 
  geom_errorbar(aes(), colour="black", width=.1, position=position_dodge(.8)) +
  scale_x_discrete(name="\nWoman Candidate Profile", labels=labels_profile)+
  ylab("Gender Difference in Trait Assessments \n(Higher Scores Favor Woman Candidate)\n") +
  #ggtitle("Candidate Likeability \nby Experimental Condition\n") + 
  facet_wrap(~male_facet, labeller=labeller(male_facet=labels2))+
  theme_bw()+
  theme(axis.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())

ggsave("Figures/figA1.pdf", width = 8, height = 6)


##Heat map for 2018 vote results
##Figure 8
##All respondents
ggplot(caucus_2018_vote, aes(male_treat, fem_treat)) + # x and y axes => Var1 and Var2
  geom_tile(aes(fill = caucus_2018_vote$vote_julie)) + # background colours are mapped according to the value column
  geom_text(aes(fill = caucus_2018_vote$vote_julie, label = round(caucus_2018_vote$vote_julie, 2))) + # write the values
  geom_hline(data=caucus_2018_vote, aes(yintercept=2.5), linetype="dashed")+
  geom_vline(data=caucus_2018_vote, aes(xintercept=2.5), linetype="dashed")+
      scale_fill_gradient2(low = muted("midnightblue"), 
                       mid = "white", 
                       high = muted("darkred"), 
                       midpoint = 0.5) + # determine the colour
  theme(panel.grid.major.x=element_blank(), #no gridlines
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.y=element_blank(), 
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"), # background=white
        #axis.text.x = element_text(angle=90, hjust = 1,vjust=1,size = 12,face = "bold"),
        plot.title = element_text(size=16,face="bold"))+
        #axis.text.y = element_text(size = 12,face = "bold")) + 
  #ggtitle("All 2018 Caucus Activists") + 
  theme(legend.title=element_text(face="bold", size=12)) + 
  scale_x_discrete(name="Experimental Condition: Man") +
  scale_y_discrete(name="Experimental Condition: Woman") +
  theme(axis.title = element_text(size = 14)) +
  labs(fill="Prob. Vote \nfor Julie")

ggsave("Figures/fig8.pdf", width = 8, height = 6)


##Figure 9
##Strong conservatives
##Same as Figure 8
ggplot(caucus_2018_vote_strongcon, aes(male_treat, fem_treat)) + # x and y axes => Var1 and Var2
  geom_tile(aes(fill = caucus_2018_vote_strongcon$vote_julie)) + # background colours are mapped according to the value column
  geom_text(aes(fill = caucus_2018_vote_strongcon$vote_julie, label = round(caucus_2018_vote_strongcon$vote_julie, 2))) + # write the values
  geom_hline(data=caucus_2018_vote_strongcon, aes(yintercept=2.5), linetype="dashed")+
  geom_vline(data=caucus_2018_vote_strongcon, aes(xintercept=2.5), linetype="dashed")+
  #geom_vline(data=caucus_2018_vote_strongcon, aes(xintercept=1.5), linetype="dotted", colour="darkgrey")+
  #geom_vline(data=caucus_2018_vote_strongcon, aes(xintercept=3.5), linetype="dotted", colour="darkgrey")+
  #geom_hline(data=caucus_2018_vote_strongcon, aes(yintercept=1.5), linetype="dotted", colour="darkgrey")+
  #geom_hline(data=caucus_2018_vote_strongcon, aes(yintercept=3.5), linetype="dotted", colour="darkgrey")+
  scale_fill_gradient2(low = muted("midnightblue"), 
                       mid = "white", 
                       high = muted("darkred"), 
                       midpoint = 0.5) + # determine the colour
  theme(panel.grid.major.x=element_blank(), #no gridlines
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.y=element_blank(), 
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"), # background=white
        #axis.text.x = element_text(angle=90, hjust = 1,vjust=1,size = 12,face = "bold"),
        plot.title = element_text(size=16,face="bold"))+
  #axis.text.y = element_text(size = 12,face = "bold")) + 
  #ggtitle("Strong Conservatives") + 
  theme(legend.title=element_text(face="bold", size=12)) + 
  scale_x_discrete(name="Experimental Condition: Man") +
  scale_y_discrete(name="Experimental Condition: Woman") +
  theme(axis.title = element_text(size = 14)) +
  labs(fill="Prob. Vote \nfor Julie")

ggsave("Figures/fig9.pdf", width = 8, height = 6)


##Fig A2
###Caucus observer data
fig_win_observe <- ggplot(data=win_observe, aes(y=margin_eff, x=cand_sex, ymin=margin_eff-se*qnorm(.975), ymax=margin_eff+se*qnorm(.975)) ) +
  expand_limits(y=c(-.5, .5)) + 
  geom_point(size=4, stat="identity") +
  geom_errorbar(aes(), width=.05) +
  geom_hline(aes(yintercept=0), linetype="dashed", colour="red")+
  ylab("Marginal Effect of Female Self-Presentation") +
  xlab("Candidate Sex") +
  theme_bw()+
  theme(axis.title = element_text(size = 14), axis.text.x = element_text(size = 14))
fig_win_observe

ggsave("figA2.pdf")








