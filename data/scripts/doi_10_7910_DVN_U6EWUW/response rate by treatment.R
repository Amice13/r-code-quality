rm(list=ls())

library(foreign)
library(ggplot2)
library(readxl)
library(gridExtra)

setwd("/Users/johnholbein/Dropbox (Batten School @ UVA)/Work/Journalist Audit Study")

############################## Mean Differences ############################## 
priority<-read_excel("responsebytreatment.xlsx")

attach(priority) 

plot1<-ggplot(priority, aes(y=mean, reorder(ideo, num))) + 
  geom_bar( colour ="grey", stat="identity") +
  geom_errorbar(data=priority, aes(ymin=mean-1.96*se, ymax= mean+1.96*se), colour="black", width=0, size=4) +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x="Ideology of the Story", y="Proportion of Journalists Responding", title="") +
  theme(legend.position="none", legend.key=element_blank())   + theme(text = element_text(size=30)) +
  annotate("text", x=1, y = 0.15, label = "0.185", size=10, fontface="bold", colour="white", angle=0) +
  annotate("text", x=2, y = 0.15, label = "0.178", size=10, fontface="bold", colour="white", angle=0) +
  annotate("text", x=3, y = 0.15, label = "0.184", size=10, fontface="bold", colour="white", angle=0) +
  annotate("text", x=4, y = 0.15, label = "0.189", size=10, fontface="bold", colour="white", angle=0) 
  

plot1
ggsave(plot1, file="response_by_treatment_wp.pdf", width=8, height=6, scale=2)

############################## Coefplot ############################## 
priority<-read_excel("coefplot.xlsx")
permutation<-read.dta("/Users/johnholbein/Dropbox (Batten School @ UVA)/Work/Journalist Audit Study/treatment_permute.dta")

color.names<-c("#7fc97f", "#beaed4", "#fdc086")

attach(priority) 

plot1<-ggplot(priority, aes(y=mean, reorder(ideo, num)))+ 
  geom_hline(aes(yintercept=0), colour="grey8", linetype="dashed", size=2) +
  geom_point(data=priority, size=20, aes(colour = factor(col))) +
  geom_errorbar(data=priority, aes(ymin=mean-1.96*se, ymax= mean+1.96*se, colour = factor(col)), width=0, size=4) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey8")) +
  scale_colour_manual(values=color.names) +
  theme(legend.title = element_blank()) +
  theme(legend.position="none", legend.direction="horizontal") +
  labs(x="", y="Effect on Response (Base: Strong Progressive)", title="") +
  theme(text = element_text(size=34)) + 
  scale_y_continuous(limits=c(-0.1, 0.1)) + #+ geom_hline(aes(yintercept=-0.054), colour="red", linetype="dashed", size=2) +

  annotate("text", x=1, y = -0.09, label = "Liberal Bias", size=10, fontface="bold", colour="blue", angle=0,  alpha=0.55) +
  annotate("text", x=1, y = 0.09, label = "Conservative Bias", size=10, fontface="bold", colour="red", angle=0,  alpha=0.55) +
  
  annotate("text", x=2, y = -0.09, label = "Liberal Bias", size=10, fontface="bold", colour="blue", angle=0,  alpha=0.55) +
  annotate("text", x=2, y = 0.09, label = "Conservative Bias", size=10, fontface="bold", colour="red", angle=0,  alpha=0.55) +  
  
  annotate("text", x=3, y = -0.09, label = "Strong Liberal Bias", size=10, fontface="bold", colour="blue", angle=0) +
  annotate("text", x=3, y = 0.09, label = "Moderate Liberal Bias", size=10, fontface="bold", colour="blue", angle=0,  alpha=0.55) + 
  
    annotate("text", x=1.18, y = -0.005, label = "-0.002", size=10, fontface="bold", colour="black", angle=0) +
      annotate("text", x=1, y = -0.028, label = "-0.023", size=10, colour="black", angle=0) +
      annotate("text", x=1, y = 0.025, label = "0.020", size=10, colour="black", angle=0) +
  annotate("text", x=2.18, y = -0.0093665, label = "-0.009", size=10, fontface="bold", colour="black", angle=0) +
      annotate("text", x=2, y = -0.034, label = "-0.029", size=10, colour="black", angle=0) +
      annotate("text", x=2, y = 0.015, label = "0.010", size=10, colour="black", angle=0) +
  annotate("text", x=3.18, y = -0.0095067, label = "-0.009", size=10, fontface="bold", colour="black", angle=0) +
      annotate("text", x=3, y = -0.035, label = "-0.030", size=10, colour="black", angle=0) +
      annotate("text", x=3, y = 0.016, label = "0.011", size=10, colour="black", angle=0) 
plot1 

ggsave(plot1, file="coef.pdf", width=6, height=6, scale=2)


attach(permutation) 

  plot2<-ggplot(priority, x=b_treatment_2) +
    geom_density(data=permutation, aes(x=b_treatment_1), size=1.4, colour="#beaed4", fill="#beaed4",  alpha=0.15) +
    geom_density(data=permutation, aes(x=b_treatment_2), size=1.4, colour="#fdc086", fill="#fdc086", alpha=0.15) + 
    geom_density(data=permutation, aes(x=b_treatment_3), size=1.4, colour = "#7fc97f", fill="#7fc97f", alpha=0.15) +
    coord_flip() + theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x="", y="", title="") +
  theme(text = element_text(size=25))  + theme(legend.position="none") +
    scale_x_continuous(limits=c(-0.1, 0.1)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) + theme(panel.border = element_blank()) + theme(axis.line = element_line(colour = "white")) + geom_hline(yintercept=0, colour="white", size=3) 
 # geom_vline(aes(xintercept=-0.0093665), colour="#beaed4", linetype="dashed", size=2, alpha=0.3) + #purple
#    geom_vline(aes(xintercept=-0.0018364), colour="#7fc97f", linetype="dashed", size=1.5, alpha=0.3) + #green
#    geom_vline(aes(xintercept=-0.0095067), colour="#fdc086", linetype="dashed", size=1.5, alpha=0.3) #orange
  plot2
ggsave(plot2, file="permutation_dist.pdf", width=3, height=10, scale=2)

empty <- ggplot()+geom_point(aes(1,1), colour="white")+
  theme(axis.ticks=element_blank(), 
        panel.background=element_blank(), 
        axis.text.x=element_blank(), axis.text.y=element_blank(),           
        axis.title.x=element_blank(), axis.title.y=element_blank())


combo<-grid.arrange(empty, empty, plot1, plot2, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))
ggsave(combo, file="coef_and_permutation_dist.pdf", width=12, height=12, scale=2)

############################## Coefplot (no elect states held out) ############################## 
priority<-read_excel("/Users/johnholbein/Dropbox (Batten School @ UVA)/Work/Journalist Audit Study/coefplot_no_elect_states_held_out.xlsx")
permutation<-read.dta("/Users/johnholbein/Dropbox (Batten School @ UVA)/Work/Journalist Audit Study/treatment_permute_no_elect_states_held_out.dta")

color.names<-c("#7fc97f", "#beaed4", "#fdc086")

attach(priority) 

plot1<-ggplot(priority, aes(y=mean, reorder(ideo, num)))+ 
  geom_hline(aes(yintercept=0), colour="grey8", linetype="dashed", size=2) +
  geom_point(data=priority, size=20, aes(colour = factor(col))) +
  geom_errorbar(data=priority, aes(ymin=mean-1.96*se, ymax= mean+1.96*se, colour = factor(col)), width=0, size=4) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey8")) +
  scale_colour_manual(values=color.names) +
  theme(legend.title = element_blank()) +
  theme(legend.position="none", legend.direction="horizontal") +
  labs(x="", y="Effect on Response (Base: Strong Progressive)", title="") +
  theme(text = element_text(size=34)) + 
  scale_y_continuous(limits=c(-0.1, 0.1)) + #+ geom_hline(aes(yintercept=-0.054), colour="red", linetype="dashed", size=2) +
  
  annotate("text", x=1, y = -0.09, label = "Liberal Bias", size=10, fontface="bold", colour="blue", angle=0,  alpha=0.55) +
  annotate("text", x=1, y = 0.09, label = "Conservative Bias", size=10, fontface="bold", colour="red", angle=0,  alpha=0.55) +
  
  annotate("text", x=2, y = -0.09, label = "Liberal Bias", size=10, fontface="bold", colour="blue", angle=0,  alpha=0.55) +
  annotate("text", x=2, y = 0.09, label = "Conservative Bias", size=10, fontface="bold", colour="red", angle=0,  alpha=0.55) +  
  
  annotate("text", x=3, y = -0.09, label = "Strong Liberal Bias", size=10, fontface="bold", colour="blue", angle=0) +
  annotate("text", x=3, y = 0.09, label = "Moderate Liberal Bias", size=10, fontface="bold", colour="blue", angle=0,  alpha=0.55) + 
  
  annotate("text", x=1.18, y = 0.005, label = "0.0002", size=10, fontface="bold", colour="black", angle=0) +
  annotate("text", x=1, y = -0.027, label = "-0.022", size=10, colour="black", angle=0) +
  annotate("text", x=1, y = 0.028, label = "0.023", size=10, colour="black", angle=0) +
  
  annotate("text", x=2.18, y = -0.009, label = "-0.009", size=10, fontface="bold", colour="black", angle=0) +
  annotate("text", x=2, y = -0.034, label = "-0.029", size=10, colour="black", angle=0) +
  annotate("text", x=2, y = 0.016, label = "0.011", size=10, colour="black", angle=0) +
  
  annotate("text", x=3.18, y = -0.005, label = "-0.005", size=10, fontface="bold", colour="black", angle=0) +
  annotate("text", x=3, y = -0.03, label = "-0.025", size=10, colour="black", angle=0) +
  annotate("text", x=3, y = 0.02, label = "0.015", size=10, colour="black", angle=0) 
plot1 

ggsave(plot1, file="coef_no_elect_states_held_out.pdf", width=6, height=6, scale=2)


attach(permutation) 

plot2<-ggplot(priority, x=b_treatment_2) +
  geom_density(data=permutation, aes(x=b_treatment_1), size=1.4, colour="#beaed4", fill="#beaed4",  alpha=0.15) +
  geom_density(data=permutation, aes(x=b_treatment_2), size=1.4, colour="#fdc086", fill="#fdc086", alpha=0.15) + 
  geom_density(data=permutation, aes(x=b_treatment_3), size=1.4, colour = "#7fc97f", fill="#7fc97f", alpha=0.15) +
  coord_flip() + theme_bw() + theme(panel.grid.major = element_blank(), 
                                    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x="", y="", title="") +
  theme(text = element_text(size=25))  + theme(legend.position="none") +
  scale_x_continuous(limits=c(-0.1, 0.1)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + theme(panel.border = element_blank()) + theme(axis.line = element_line(colour = "white")) + geom_hline(yintercept=0, colour="white", size=3) 
# geom_vline(aes(xintercept=-0.0093665), colour="#beaed4", linetype="dashed", size=2, alpha=0.3) + #purple
#    geom_vline(aes(xintercept=-0.0018364), colour="#7fc97f", linetype="dashed", size=1.5, alpha=0.3) + #green
#    geom_vline(aes(xintercept=-0.0095067), colour="#fdc086", linetype="dashed", size=1.5, alpha=0.3) #orange
plot2
ggsave(plot2, file="permutation_dist_no_elect_states_held_out.pdf", width=3, height=10, scale=2)

empty <- ggplot()+geom_point(aes(1,1), colour="white")+
  theme(axis.ticks=element_blank(), 
        panel.background=element_blank(), 
        axis.text.x=element_blank(), axis.text.y=element_blank(),           
        axis.title.x=element_blank(), axis.title.y=element_blank())


combo<-grid.arrange(empty, empty, plot1, plot2, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))
ggsave(combo, file="coef_and_permutation_dist_no_elect_states_held_out.pdf", width=12, height=12, scale=2)


############################## Coefplot (by Trump VS) ############################## 
priority<-read_excel("coefplot_trump_perc.xlsx")

attach(priority) 

plot1<-ggplot(priority, aes(y=mean, reorder(ideo, num)))+ 
  geom_hline(aes(yintercept=0), colour="grey8", linetype="dashed", size=2) +
  geom_point(data=priority, colour ="grey8", size=20) +
  geom_errorbar(data=priority, aes(ymin=mean-1.96*se, ymax= mean+1.96*se), colour = "grey8", width=0, size=5) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey8")) +
  facet_wrap(~voteshare, scales = "free", nrow=1)   +
  theme(legend.title = element_blank()) +
  theme(legend.position="bottom", legend.direction="horizontal") +
  labs(x="", y="Effect on Response Rate", title="") +
  theme(text = element_text(size=45)) + 
  scale_y_continuous(limits=c(-0.1, 0.1)) 

plot1 

ggsave(plot1, file="coef_trump_perc.pdf", width=15, height=6, scale=2)

############################## Coefplot (by Newspaper Perceived Ideology ) ############################## 
priority<-read_excel("coefplot_newspaper_ideo.xlsx")

attach(priority) 

plot1<-ggplot(priority, aes(y=mean, reorder(ideo, num)))+ 
  geom_hline(aes(yintercept=0), colour="grey8", linetype="dashed", size=2) +
  geom_point(data=priority, colour ="grey8", size=20) +
  geom_errorbar(data=priority, aes(ymin=mean-1.96*se, ymax= mean+1.96*se), colour = "grey8", width=0, size=5) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey8")) +
  facet_wrap(~voteshare, nrow=1)   +
  theme(legend.title = element_blank()) +
  theme(legend.position="bottom", legend.direction="horizontal") +
  labs(x="", y="Effect on Response (Base: Strong Progressive)", title="") +
  theme(text = element_text(size=28)) + 
  scale_y_continuous(limits=c(-0.35, 0.35)) 

plot1 

ggsave(plot1, file="coefplot_newspaper_ideo.pdf", width=15, height=6, scale=2)

############################## Coefplot (by Journalist Ideology Twitter) ############################## 
priority<-read.csv("coefplot_journalist_ideo_twitter.csv")

attach(priority) 


plot1<-ggplot(priority, aes(y=mean, reorder(ideo, num)))+ 
  geom_hline(aes(yintercept=0), colour="grey8", linetype="dashed", size=2) +
  geom_point(data=priority, colour ="grey8", size=20) +
  geom_errorbar(data=priority, aes(ymin=mean-1.96*se, ymax= mean+1.96*se), colour = "grey8", width=0, size=5) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey8")) +
  facet_wrap(~voteshare, nrow=1)   +
  theme(legend.title = element_blank()) +
  theme(legend.position="bottom", legend.direction="horizontal") +
  labs(x="", y="Effect on Response Rate", title="") +
  theme(text = element_text(size=49.5))  + 
  scale_y_continuous(limits=c(-0.15, 0.15))

plot1 

ggsave(plot1, file="coefplot_journalist_ideo_twitter.pdf", width=20, height=6, scale=2)
