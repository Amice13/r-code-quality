#Title: Why Do Voters Prefer Local Candidates? Evidence from Danish Conjoint Survey Experiment
#Author: Niels Nyholt 

#File purpose: Conduct all statistical analysis. 
#NB: Set working directory to the folder where you have run "transformations.R" and saved the file "transformed_data.dta".
setwd() 
rm(list=ls())

#The following analyses were carried out using R version 4.3.1

require(haven) #version 2.5.3
require(cregg) #version 0.4.0
require(tidyverse) #version 2.0.0
require(cowplot) #version 1.1.1
require(estimatr) #version 1.0.0
require(texreg) #version 1.39.3

all_data <- read_dta("transformed_data.dta")
#Making some variables factors

all_data <- all_data %>%
  mutate(
    FeatDlocalism_grownlocal = factor(FeatDlocalism_grownlocal, labels = c("Raised elsewhere", "Raised locally")),
    FeatDlocalism_livelocal = factor(FeatDlocalism_livelocal, labels = c("Lives elsewhere", "Lives locally")),
    FeatBlocalism = factor(FeatBlocalism, labels = c("No behavioral information", "Only works on national issues", "Splits time between national and local issues", "Primarily works on local issues" )),
    FeatBlocalism_null = factor(FeatBlocalism_null, levels = c(1, 0), labels = c("Behavioral localism available", "No behavioral information")),
    FeatSlocalism_null = factor(FeatSlocalism_null, levels = c(1, 0), labels = c("No symbolic information", "Symbolic localism available")),
    FeatSlocalism = factor(FeatSlocalism, labels = c("No symbolic information", "Not seen regularly", "Seen during campaign", "Knows some names, has been active locally", "Knows names, active locally")),
    FeatGender = factor(FeatGender, labels = c("Man", "Woman")),
    FeatAge = factor(FeatAge, labels = c("27-33", "34-49", "50-63", "64-74")),
    FeatOccupation = factor(FeatOccupation, labels = c("Self-employed", "Farmer", "High school teacher", "Doctor", "Lawyer")),
    FeatPartisanship = factor(FeatPartisanship, labels = c("Socialist People's Party", "Social Democrats", "Radical Liberals", "Conservative People's Party", "The New Right", "Danish People's Party", "Liberals", "Unity List")),
    SameGender = factor(SameGender, labels = c("Similiar gender", "Not same gender")),
    SameAge = factor(SameAge, labels = c("Similiar age", "Not similiar age")),
    SameOccupation = factor(SameOccupation, labels = c("Same occupation", "Not same occutpation")),
    SamePartisanship = factor(SamePartisanship, labels = c("Same party", "Not same party")),
    SameBlock = factor(SameBlock, labels = c("Same block", "Not same block")),
    flocal3=factor(ifelse(F==is.na(local) & local< 1/4, 0,
                          ifelse(F==is.na(local) & local>=1/4 & local< 3/5, 1,
                                 ifelse(F==is.na(local) & local>=3/5, 2, NA))), 
                   levels = c(0,1, 2, NA), labels= c("low", "medium", "high"))
    
  )

#Data exclusion####
#Respondents excluded because of inattentiveness.
#Number of respondents that should be excluded according to preregistration:
length(unique(subset(all_data, Tot_time<240)$id))/length(unique(all_data$id)) 
#Cutoff in preregistration was 4 minutes it excludes 41 pct. of respondents. Tha is a lot.
#Instead I go with 3 minutes as a cutoff point.
length(unique(subset(all_data, Prime==0 & Tot_time<180 | Prime==1 & Tot_time<180+15)$id))/length(unique(all_data$id)) #23.7 pct. is much less extreme

main_data <- subset(all_data, Prime==0 & Tot_time>180   | Prime==1 & Tot_time>180+15) 

#respondents excluded at various points:
length(main_data$id)-length(subset(main_data, F==is.na(flocal) & F==is.na(FeatSup) & F==is.na(SameGender) & F==is.na(SameAge) & F==is.na(SameOccupation) & F==is.na(SamePartisanship) & F==is.na(SameBlock))$id)

#Indicies####
cor(main_data$FeatSup, main_data$Featfc, use="complete.obs", method = "pearson") #0.57 so good
#Testing reliability and validity of indecies of respondet's localism. 
#Validity: Expect a correlation of more than 0.3
cor(main_data$local1, main_data$local2, use="complete.obs", method = "pearson") #0.42 so good
cor(main_data$local1, main_data$local3, use="complete.obs", method = "pearson") #0.55 so good
cor(main_data$local2, main_data$local3, use="complete.obs", method = "pearson") #0.53 so good
#Reliability: Expect a Cronbachs Alpha of more than 0.7
psych::alpha(select(main_data, local1, local2, local3))$total$std.alpha
#0.75 so reliability is good.


#H1:####
simple <- formula(FeatSup ~ FeatDlocalism_grownlocal + FeatDlocalism_livelocal + FeatBlocalism + FeatSlocalism +
                    #Controls
                    FeatGender + FeatAge + FeatOccupation + FeatPartisanship)

controls <- formula(FeatSup ~ FeatDlocalism_grownlocal + FeatDlocalism_livelocal + FeatBlocalism + FeatSlocalism +
                      #Controls
                      FeatGender + FeatAge + FeatOccupation + FeatPartisanship +
                      SameGender + SameAge + SameOccupation + SamePartisanship + SameBlock)



h1_sup_amce <- cj(data = main_data, simple, id = ~ id , weights = ~ weight, estimate = "amce")
h1_sup_c_amce <- cj(data = main_data, controls, id = ~ id, weights = ~ weight, estimate = "amce")
h1_sup_mm <- cj(data = main_data, simple, id = ~ id , weights = ~ weight, estimate = "mm")
h1_sup_c_mm <- cj(data = main_data, controls, id = ~ id, weights = ~ weight, estimate = "mm")

h1_plot <- h1_sup_c_mm %>% 
  bind_rows(h1_sup_c_amce) %>% 
  mutate(
    fac=factor(ifelse(statistic=="mm", 1,
               ifelse(statistic=="amce", 2, NA)),
               levels = c(1,2), labels = c("MM","AMCE")),
    estimate = ifelse(is.na(std.error), NA, estimate),
    ref = ifelse(is.na(std.error) | is.na(estimate), "(ref)", NA),
    level = factor(level, levels = c(rev(levels(h1_sup_mm$level)))),
    feature = factor(feature, levels = c("FeatDlocalism_grownlocal",  "FeatDlocalism_livelocal", "FeatBlocalism", "FeatSlocalism", "FeatGender", "FeatAge", "FeatOccupation", "FeatPartisanship"),
                     labels = c("Descriptive \n localism", "Descriptive \n localism", "Behavioral\n localism", "Symbolic\n localism", "Gender", "Age", "Occupation", "Party")),
    statistic = factor(statistic, levels = c("mm", "amce"), 
                       labels = c("MM", "AMCE"))
  ) %>% 
  subset(F==is.na(level))

fig1 <- ggplot(h1_plot, aes(x=estimate, y= level))+
  facet_grid(rows = vars(feature),
             cols = vars(statistic),scales ="free" , space = "free")+
  geom_vline(data=filter(h1_plot, fac=="AMCE"), aes(xintercept=0), colour="darkgrey", linewidth = 1) + 
  geom_vline(data=filter(h1_plot, fac=="MM"), aes(xintercept=mean(main_data$FeatSup, na.rm = T)), colour="darkgrey", linewidth=1) +  
  geom_errorbarh(aes(xmax = upper, xmin = lower), height = 0, position = position_dodge(width = 0.5))+
  geom_text(data=filter(h1_plot, fac=="AMCE"), aes(x=0, y= level, label = ref ), size = 2.5)+
  geom_point()+
  scale_x_continuous(breaks = c(-0.10, 0.0,0.1, 0.3, 0.4))+
  labs(x = "", y = "")+
  theme(axis.text = element_text(color = "black"),
        axis.line = element_blank(),
        legend.title = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.direction	="vertical",
        panel.grid.major = element_line(linewidth = 0.1, linetype = 'solid', colour = "grey"), 
        panel.grid.minor = element_line(linewidth = 0.1, linetype = 'solid', colour = "grey"),
        panel.border = element_rect(fill=NA, colour = "black"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),
        strip.text.y = element_text(angle = 0, size = 8))

ggsave("Fig1.eps", 
       plot = fig1,
       scale = 1, width = 174 , height = 174, units = c("mm"),
       dpi = 600)

#H2:####
##Do effect of Descriptive localism diminish when voters are provided with information on behavioral localism?####
h2_sup_B_mm <- cj(data = main_data, 
                  FeatSup ~ FeatDlocalism_livelocal + FeatDlocalism_grownlocal,
                  by = ~ FeatBlocalism_null,
                  id = ~ id,
                  estimate = "mm",
                  weights = ~ weight)

h2_sup_B_mm_diff <- cj(data = main_data, 
                       FeatSup ~ FeatDlocalism_livelocal + FeatDlocalism_grownlocal,
                       by = ~ FeatBlocalism_null,
                       id = ~ id,
                       estimate = "mm_diff",
                       weights = ~ weight)

h2_sup_B_amce <- cj(data = main_data, 
                    FeatSup ~ FeatDlocalism_livelocal + FeatDlocalism_grownlocal,
                    by = ~ FeatBlocalism_null,
                    id = ~ id,
                    estimate = "amce",
                    weights = ~ weight)

h2_sup_B_amce_diff <- cj(data = main_data, 
                         FeatSup ~ FeatDlocalism_livelocal + FeatDlocalism_grownlocal,
                         by = ~ FeatBlocalism_null,
                         id = ~ id,
                         estimate = "amce_diff",
                         weights = ~ weight)


h2_sup_plot <- h2_sup_B_mm %>% 
  bind_rows(h2_sup_B_mm_diff, h2_sup_B_amce, h2_sup_B_amce_diff) %>% 
  mutate(
    fac=factor(statistic, levels = c("mm",  "amce", "mm_difference", "amce_difference"), labels = c("MM", "AMCE", "Difference in MM", "Difference in AMCEs")),
    facc = factor(ifelse(statistic %in% c("amce", "amce_difference"), "AMCE", "MM"), levels = c("MM", "AMCE")),
    facr = factor(ifelse(statistic %in% c("mm", "amce"), "estimate", "difference"), levels = c("estimate", "difference")),
    estimate = ifelse(is.na(std.error), NA, estimate),
    ref = ifelse(is.na(std.error) | is.na(estimate), "(ref)", NA),
    ) %>% 
  add_row(fac = "Difference in AMCEs", level="Raised and lives elsewhere", ref = "(ref)")  


h2mm <-
  ggplot(subset(h2_sup_plot, fac=="MM" ), aes(x=estimate, y= factor(level, levels  = c("Lives locally",
                                                                                       "Lives elsewhere",
                                                                                       "Raised locally",
                                                                                       "Raised elsewhere")), 
                                              xmax = upper, xmin = lower, group = FeatBlocalism_null, shape = FeatBlocalism_null, fill=FeatBlocalism_null))+
  geom_vline(aes(xintercept=mean(main_data$FeatSup, na.rm = T)), colour="darkgrey", linewidth=1) + 
  geom_linerange(position = position_dodge(width = 0.5))+
  geom_curve(aes(y = 4.25+0.1, x = 0.345, yend = 4.125+0.1, xend = subset(h2_sup_plot, fac=="MM" )$estimate[7]-0.005), curvature = -0.4 , color = "grey30", linewidth = 0.5, arrow = arrow(length = unit(0.03, "npc"))) +
  geom_curve(aes(y = 2.5, x = 0.335, yend = 2.85-0.1, xend = subset(h2_sup_plot, fac=="MM" )$estimate[4]), curvature = 0.4 , color = "grey30", linewidth = 0.5, arrow = arrow(length = unit(0.03, "npc"))) +
  geom_point(position = position_dodge(width =0.5)) + 
  scale_shape_manual(values=c(21, 21))+
  scale_fill_manual(values=c("white",  "black", "grey"))  +
  labs(y = "", x = "Likely to vote for candidate (0-1)")+
  geom_text(aes(y=4.26 , x=0.305, label = "Behavioral localism\ncue available"), size = 3, colour = "grey50", )+
  geom_text(aes(y=2.5 , x=0.3, label = "Cue not available" ), size = 3, colour = "grey50")+
  ggtitle("MM") +
  coord_cartesian(xlim = c(0.27, 0.43) ,clip = 'off')+
  theme(plot.title = element_text(size=12, hjust = .5),
        axis.line = element_blank(),
        axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 8),
        axis.text.y= element_text(color = "black"),
        axis.text.x= element_text(size = 8, color = "black"), 
        legend.position = "none",
        panel.grid.major = element_line(linewidth = 0.01, linetype = 'solid', colour = "grey"), 
        panel.grid.minor = element_line(linewidth = 0.01, linetype = 'solid', colour = "grey"),
        panel.border = element_rect(fill=NA, colour = "black"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank())

h2amce <-  
  ggplot(subset(h2_sup_plot, fac=="Difference in MM"),   aes(x=estimate, y= factor(level, levels  = c("Lives locally",
                                                                                                      "Lives elsewhere",
                                                                                                      "Raised locally",
                                                                                                      "Raised elsewhere")), 
                                                             xmax = upper, xmin = lower, group = FeatBlocalism_null, shape = FeatBlocalism_null, fill=FeatBlocalism_null))+
  geom_vline(aes(xintercept=0), colour="darkgrey", linewidth=1) + 
  geom_linerange(position = position_dodge(width = 0.5))+
  geom_point(position = position_dodge(width=0.5)) + 
  scale_shape_manual(values=c(21))+
  scale_fill_manual(values=c( "grey"))+
  geom_segment(aes(y = 1, yend = 1, x = 0.080, xend = 0.082),
               color = "grey30", lineend = "square") +
  geom_segment(aes(y = 2, yend = 2, x = 0.080, xend = 0.082),
               color = "grey30",lineend = "square") +
  geom_segment(aes(y = 3, yend = 3, x = 0.080, xend = 0.082),
               color = "grey30", lineend = "square") +
  geom_segment(aes(y = 4, yend = 4, x = 0.080, xend = 0.082),
               color = "grey30",lineend = "square") +
  geom_segment(aes(y = 2, yend = 1, x = 0.082, xend = 0.082),
               color = "grey30",lineend = "square") +
  geom_segment(aes(y = 3, yend = 4, x = 0.082, xend = 0.082),
               color = "grey30",) +
  geom_text(aes(x = 0.084, y = 1.5 , label = paste0("p=", round(subset(h2_sup_plot, fac=="Difference in AMCEs")$p[2], 2))),
            hjust = 0, size = 3, colour = "grey50")+
  geom_text(aes(x = 0.084, y = 3.5 , label = paste0("p=", round(subset(h2_sup_plot, fac=="Difference in AMCEs")$p[1], 2))),
            hjust = 0, size = 3, colour = "grey50")+
  coord_cartesian(xlim = c(-0.04, 0.125) ,clip = 'off')+
  labs(y = "", x = "Difference in score")+
  ggtitle("Difference in MMs") +
  theme(plot.title = element_text(size=12, hjust = .5),
        axis.line = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 8), 
        axis.text.y= element_blank(),
        axis.text.x= element_text(size = 8, color = "black"), 
        legend.position = "none",
        panel.grid.major = element_line(linewidth = 0.01, linetype = 'solid', colour = "grey"), 
        panel.grid.minor = element_line(linewidth = 0.01, linetype = 'solid', colour = "grey"),
        panel.border = element_rect(fill=NA, colour = "black"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank())

h2_bd_sup <- plot_grid(h2mm, h2amce, labels = c('', ''), rel_widths = c(1.1, 0.9), ncol = 2, nrow = 1 ,label_size = 12)

##Do effect of Descriptive localism diminish when voters are provided with information on symbolic localism?####
h2_sup_S_mm <- cj(data = main_data, 
                  FeatSup ~ FeatDlocalism_livelocal + FeatDlocalism_grownlocal,
                  by = ~ FeatSlocalism_null,
                  id = ~ id,
                  estimate = "mm",
                  weights = ~ weight)

h2_sup_S_mm_diff <- cj(data = main_data, 
                       FeatSup ~ FeatDlocalism_livelocal + FeatDlocalism_grownlocal,
                       by = ~ FeatSlocalism_null,
                       id = ~ id,
                       estimate = "mm_diff",
                       weights = ~ weight)

h2_sup_S_amce <- cj(data = main_data, 
                    FeatSup ~ FeatDlocalism_livelocal + FeatDlocalism_grownlocal,
                    by = ~ FeatSlocalism_null,
                    id = ~ id,
                    estimate = "amce",
                    weights = ~ weight)

h2_sup_S_amce_diff <- cj(data = main_data, 
                         FeatSup ~ FeatDlocalism_livelocal + FeatDlocalism_grownlocal,
                         by = ~ FeatSlocalism_null,
                         id = ~ id,
                         estimate = "amce_diff",
                         weights = ~ weight)



h2_sup_plot <- h2_sup_S_mm %>% 
  bind_rows(h2_sup_S_mm_diff, h2_sup_S_amce, h2_sup_S_amce_diff) %>% 
  mutate(
    fac=factor(statistic, levels = c("mm",  "amce", "mm_difference", "amce_difference"), labels = c("MM", "AMCE", "Difference in MM", "Difference in AMCEs")),
    facc = factor(ifelse(statistic %in% c("amce", "amce_difference"), "AMCE", "MM"), levels = c("MM", "AMCE")),
    facr = factor(ifelse(statistic %in% c("mm", "amce"), "estimate", "difference"), levels = c("estimate", "difference")),
    estimate = ifelse(is.na(std.error), NA, estimate),
    ref = ifelse(is.na(std.error) | is.na(estimate), "(ref)", NA),
  ) %>% 
  add_row(fac = "Difference in AMCEs", level="Raised and lives elsewhere", ref = "(ref)")

h2mm <-
  ggplot(subset(h2_sup_plot, fac=="MM" ), aes(x=estimate, y= factor(level, levels  = c("Lives locally",
                                                                                       "Lives elsewhere",
                                                                                       "Raised locally",
                                                                                       "Raised elsewhere")), 
                                              xmax = upper, xmin = lower, group = FeatSlocalism_null, shape = FeatSlocalism_null, fill=FeatSlocalism_null)) +
  geom_vline(aes(xintercept=mean(main_data$FeatSup, na.rm = T)), colour="darkgrey", size=1) + 
  geom_linerange(position = position_dodge(width = 0.5))+
  geom_curve(aes(y = 4.25+0.1, x = 0.345, yend = 4.125+0.1, xend = subset(h2_sup_plot, fac=="MM" )$estimate[7]-0.005), curvature = -0.4 , color = "grey30", linewidth = 0.5, arrow = arrow(length = unit(0.03, "npc"))) +
  geom_curve(aes(y = 2.5, x = 0.335, yend = 2.85-0.1, xend = subset(h2_sup_plot, fac=="MM" )$estimate[4]), curvature = 0.4 , color = "grey30", linewidth = 0.5, arrow = arrow(length = unit(0.03, "npc"))) +
  geom_point(position = position_dodge(width =0.5)) + 
  scale_shape_manual(values=c(21, 21))+
  scale_fill_manual(values=c("white",  "black", "grey"))  +
  labs(y = "", x = "Likely to vote for candidate (0-1)")+
  geom_text(aes(y=4.26 , x=0.305, label = "Symbolic localism\ncue available"), size = 3, colour = "grey50", )+
  geom_text(aes(y=2.5 , x=0.3, label = "Cue not available" ), size = 3, colour = "grey50")+
  ggtitle("MM") +
  coord_cartesian(xlim = c(0.27, 0.43) ,clip = 'off')+
  theme(plot.title = element_text(size=12, hjust = .5),
        axis.line = element_blank(),
        axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 8),
        axis.text.y= element_text(color = "black"),
        axis.text.x= element_text(size = 8, color = "black"), 
        legend.position = "none",
        panel.grid.major = element_line(size = 0.01, linetype = 'solid', colour = "grey"), 
        panel.grid.minor = element_line(size = 0.01, linetype = 'solid', colour = "grey"),
        panel.border = element_rect(fill=NA, colour = "black"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank())

h2amce <- 
  ggplot(subset(h2_sup_plot, fac=="Difference in MM"),   aes(x=estimate, y= factor(level, levels  = c("Lives locally",
                                                                                                      "Lives elsewhere",
                                                                                                      "Raised locally",
                                                                                                      "Raised elsewhere")), 
                                                             xmax = upper, xmin = lower, group = FeatSlocalism_null, shape = FeatSlocalism_null, fill=FeatSlocalism_null)) +
  geom_vline(aes(xintercept=0), colour="darkgrey", linewidth=1) + 
  geom_linerange(position = position_dodge(width = 0.5))+
  geom_point(position = position_dodge(width=0.5)) + 
  scale_shape_manual(values=c(21))+
  scale_fill_manual(values=c( "grey"))+
  labs(y = "", x = "Difference in score")+
  ggtitle("Difference in MMs") +
  geom_segment(aes(y = 1, yend = 1, x = 0.080, xend = 0.082),
               color = "grey30", lineend = "square") +
  geom_segment(aes(y = 2, yend = 2, x = 0.080, xend = 0.082),
               color = "grey30",lineend = "square") +
  geom_segment(aes(y = 3, yend = 3, x = 0.080, xend = 0.082),
               color = "grey30", lineend = "square") +
  geom_segment(aes(y = 4, yend = 4, x = 0.080, xend = 0.082),
               color = "grey30",lineend = "square") +
  geom_segment(aes(y = 2, yend = 1, x = 0.082, xend = 0.082),
               color = "grey30",lineend = "square") +
  geom_segment(aes(y = 3, yend = 4, x = 0.082, xend = 0.082),
               color = "grey30",) +
  
  geom_text(aes(x = 0.084, y = 1.5 , label = paste0("p=", round(subset(h2_sup_plot, fac=="Difference in AMCEs")$p[2], 2))),
            hjust = 0, size = 3, colour = "grey50")+
  geom_text(aes(x = 0.084, y = 3.5 , label = paste0("p=", round(subset(h2_sup_plot, fac=="Difference in AMCEs")$p[1], 2))),
            hjust = 0, size = 3, colour = "grey50")+
  coord_cartesian(xlim = c(-0.04, 0.125) ,clip = 'off')+
  theme(plot.title = element_text(size=12, hjust = .5),
        axis.line = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 8), 
        axis.text.y= element_blank(),
        axis.text.x= element_text(size = 8, color = "black"), 
        legend.position = "none",
        panel.grid.major = element_line(size = 0.01, linetype = 'solid', colour = "grey"), 
        panel.grid.minor = element_line(size = 0.01, linetype = 'solid', colour = "grey"),
        panel.border = element_rect(fill=NA, colour = "black"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank())

h2_sd_sup <- plot_grid(h2mm, h2amce, labels = c('', ''), rel_widths = c(1.1, 0.9), ncol = 2, nrow = 1 ,label_size = 12)

##Do effect of Behavioral localism diminish when voters are provided with information on symbolic localism?####
h2_sup_BS_mm <- cj(data = main_data, 
                   FeatSup ~ FeatBlocalism,
                   by = ~ FeatSlocalism_null ,
                   id = ~ id,
                   estimate = "mm",
                   weights = ~ weight
)

h2_sup_BS_mm_diff <- cj(data = main_data, 
                        FeatSup ~ FeatBlocalism,
                        by = ~ FeatSlocalism_null ,
                        id = ~ id,
                        estimate = "mm_diff",
                        weights = ~ weight)



h2_sup_BS_amce <- cj(data = main_data, 
                     FeatSup ~ FeatBlocalism,
                     by = ~ FeatSlocalism_null ,
                     id = ~ id,
                     estimate = "amce",
                     weights = ~ weight
)

h2_sup_BS_amce_diff <- cj(data = main_data, 
                          FeatSup ~ FeatBlocalism,
                          by = ~ FeatSlocalism_null ,
                          id = ~ id,
                          estimate = "amce_diff",
                          weights = ~ weight
)


h2_sup_plot <- h2_sup_BS_mm %>% 
  bind_rows(h2_sup_BS_mm_diff, h2_sup_BS_amce, h2_sup_BS_amce_diff) %>% 
  mutate(
    fac=factor(statistic, levels = c("mm",  "amce", "mm_difference", "amce_difference"), labels = c("MM", "AMCE", "Difference in MM", "Difference in AMCEs")),
    facc = factor(ifelse(statistic %in% c("amce", "amce_difference"), "AMCE", "MM"), levels = c("MM", "AMCE")),
    facr = factor(ifelse(statistic %in% c("mm", "amce"), "estimate", "difference"), levels = c("estimate", "difference")),
    estimate = ifelse(is.na(std.error), NA, estimate),
    ref = ifelse(is.na(std.error) | is.na(estimate), "(ref)", NA),
    )
h2mm <- 
  ggplot(subset(h2_sup_plot, fac=="MM" ), aes(x=estimate, y = factor(level, levels = c("Primarily works on local issues",
                                                                                       "Splits time between national and local issues",
                                                                                       "Only works on national issues",
                                                                                       "No behavioral information"),
                                                                     labels  = c("Works primarily \non local issues",
                                                                                 "Work split\nbetween national \nand local issues", 
                                                                                 "Only works on\nnational issues", 
                                                                                 "No behavioral\ninformation")),
                                              xmax = upper, xmin = lower, group = FeatSlocalism_null, shape = FeatSlocalism_null, fill=FeatSlocalism_null))+
  geom_vline(aes(xintercept=mean(main_data$FeatSup, na.rm = T)), colour="darkgrey", size=1) + 
  geom_linerange(position = position_dodge(width = 0.5))+
  geom_curve(aes(y = 4.25+0.1, x = 0.359, yend = 4.125+0.1, xend = subset(h2_sup_plot, fac=="MM" )$estimate[5]+0.005), curvature = 0.4 , color = "grey30", linewidth = 0.5, arrow = arrow(length = unit(0.03, "npc"))) +
  geom_curve(aes(y = 2.5, x = 0.359, yend = 2.85-0.1, xend = subset(h2_sup_plot, fac=="MM" )$estimate[2]), curvature = -0.4 , color = "grey30", linewidth = 0.5, arrow = arrow(length = unit(0.03, "npc"))) +
  geom_point(position = position_dodge(width =0.5)) + 
  scale_shape_manual(values=c(21, 21))+
  scale_fill_manual(values=c("white",  "black", "grey"))  +
  labs(y = "", x = "Likely to vote for candidate (0-1)")+
  geom_text(aes(y=4.26 , x=0.395, label = "Symbolic localism\ncue available"), size = 3, colour = "grey50", )+
  geom_text(aes(y=2.5 , x=0.395, label = "Cue not available" ), size = 3, colour = "grey50")+
  ggtitle("MM") +
  coord_cartesian(xlim = c(0.27, 0.43) ,clip = 'off')+
  theme(plot.title = element_text(size=12, hjust = .5),
        axis.line = element_blank(),
        axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 8),
        axis.text.y= element_text(color = "black"),
        axis.text.x= element_text(size = 8, color = "black"), 
        legend.position = "none",
        panel.grid.major = element_line(size = 0.01, linetype = 'solid', colour = "grey"), 
        panel.grid.minor = element_line(size = 0.01, linetype = 'solid', colour = "grey"),
        panel.border = element_rect(fill=NA, colour = "black"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank())


h2amce <-ggplot(subset(h2_sup_plot, fac=="Difference in MM"),  aes(x=estimate, y=  factor(level, levels = c("Primarily works on local issues", "Splits time between national and local issues", "Only works on national issues","No behavioral information"),
                                                                                          labels  = c("Works primarily\non local issues", "Work split\nbetween national\nand local issues", "Only works on\nnational issues", "No behavioral\ninformation")),
                                                                   xmax = upper, xmin = lower, group = FeatSlocalism_null, shape = FeatSlocalism_null, fill=FeatSlocalism_null))+
  geom_vline(aes(xintercept=0), colour="darkgrey", linewidth=1) + 
  geom_linerange(position = position_dodge(width = 0.5))+
  geom_point(position = position_dodge(width=0.5)) + 
  scale_shape_manual(values=c(21))+
  scale_fill_manual(values=c( "grey"))+
  labs(y = "", x = "Difference in score")+
  ggtitle("Difference in MMs") +
  geom_segment(aes(y = 1, yend = 4, x = 0.082, xend = 0.082),
               color = "grey30", lineend = "square") +
  geom_segment(aes(y = 1, yend = 1, x = 0.080, xend = 0.082),
               color = "grey30", lineend = "square") +
  geom_segment(aes(y = 4, yend = 4, x = 0.080, xend = 0.082),
               color = "grey30", lineend = "square") +
  geom_text(aes(x = 0.084, y = 1.5 , label = paste0("p=", round(subset(h2_sup_plot, fac=="Difference in AMCEs")$p[2], 2))),
            hjust = 0, size = 3, colour = "grey50")+
  
  geom_segment(aes(y = 2, yend = 4, x = 0.089, xend = 0.089),
               color = "grey30", lineend = "square") +
  geom_segment(aes(y = 2, yend = 2, x = 0.087, xend = 0.089),
               color = "grey30", lineend = "square") +
  geom_segment(aes(y = 4, yend = 4, x = 0.087, xend = 0.089),
               color = "grey30", lineend = "square") +
  geom_text(aes(x = 0.091, y = 2.5 , label = paste0("p=", round(subset(h2_sup_plot, fac=="Difference in AMCEs")$p[3], 2))),
            hjust = 0, size = 3, colour = "grey50")+
  
  geom_segment(aes(y = 3, yend = 4, x = 0.096, xend = 0.096),
               color = "grey30", lineend = "square") +
  geom_segment(aes(y = 3, yend = 3, x = 0.094, xend = 0.096),
               color = "grey30", lineend = "square") +
  geom_segment(aes(y = 4, yend = 4, x = 0.094, xend = 0.096),
               color = "grey30", lineend = "square") +
  geom_text(aes(x = 0.098, y = 3.5 , label = paste0("p=", round(subset(h2_sup_plot, fac=="Difference in AMCEs")$p[1], 2))),
            hjust = 0, size = 3, colour = "grey50")+
  coord_cartesian(xlim = c(-0.04, 0.125) ,clip = 'off')+
  theme(plot.title = element_text(size=12, hjust = .5),
        axis.line = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 8), 
        axis.text.y= element_blank(),
        axis.text.x= element_text(size = 8, color = "black"), 
        legend.position = "none",
        panel.grid.major = element_line(size = 0.01, linetype = 'solid', colour = "grey"), 
        panel.grid.minor = element_line(size = 0.01, linetype = 'solid', colour = "grey"),
        panel.border = element_rect(fill=NA, colour = "black"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank())


h2_sb_sup <- plot_grid(h2mm, h2amce, labels = c('', ''), rel_widths = c(1.1, 0.9) , ncol = 2, nrow = 1 ,label_size = 12)

plot_grid(h2_bd_sup, h2_sd_sup, h2_sb_sup, labels = c('A', 'B', "C"), ncol = 1, nrow = 3 ,label_size = 12)

ggsave("Fig2.eps", 
       plot = last_plot(),
       scale = 1, width = 174 , height = 190, units = c("mm"),
       dpi = 600)


#H3:####
summary(fixest::feols(FeatSup ~ FeatDlocalism_livelocal + FeatDlocalism_grownlocal + FeatBlocalism + flocal3 + FeatSlocalism + 
                        FeatGender + FeatAge + FeatOccupation + FeatPartisanship +
                        SameGender + SameAge + SameOccupation + SamePartisanship + SameBlock +
                        gender +factor(education)+factor(income),
                      cluster = ~ id,
                      weights = ~ weight,
                      data = main_data))

table(subset(main_data, contestnr=="conj1")$flocal3)/2
#when relying on foced choice there is no significant difference between high and low identifiers. The same cannot be said when I use the rating scores.This difference is quite puzzeling
h3_sup_mm<- cj(data = main_data, 
               FeatSup ~ FeatDlocalism_livelocal + FeatDlocalism_grownlocal + FeatBlocalism + FeatSlocalism, 
               by = ~ flocal3,
               id = ~ id,
               estimate = "mm",
               weights = ~ weight)

main_data$flocal3 <- factor(main_data$flocal3, levels = c( "medium","low", "high"))
h3_sup_mm_diff<- cj(data = main_data, 
                    FeatSup ~ FeatDlocalism_livelocal + FeatDlocalism_grownlocal + FeatBlocalism + FeatSlocalism , 
                    by = ~ flocal3,
                    id = ~ id,
                    estimate = "mm_diff",
                    weights = ~ weight)

h3_sup_amce<- cj(data = main_data, 
                 FeatSup ~ FeatDlocalism_livelocal + FeatDlocalism_grownlocal + FeatBlocalism + FeatSlocalism , 
                 by = ~ flocal3,
                 id = ~ id,
                 estimate = "amce",
                 weights = ~ weight)

h3_sup_amce_diff<- cj(data = main_data, 
                      FeatSup ~ FeatDlocalism_livelocal + FeatDlocalism_grownlocal + FeatBlocalism + FeatSlocalism, 
                      by = ~ flocal3,
                      id = ~ id,
                      estimate = "amce_diff",
                      weights = ~ weight)

h3_sup_plot <- h3_sup_mm %>% 
  bind_rows(h3_sup_mm_diff) %>% 
  mutate(
    fac=factor(statistic, levels = c("mm", "mm_difference", "amce", "amce_difference"), labels = c("MM", "Difference in MM", "AMCE", "Difference in AMCEs")),
    facc = factor(ifelse(statistic %in% c("amce", "amce_difference"), "AMCE", "MM"), levels = c("MM", "AMCE")),
    facr = factor(ifelse(statistic %in% c("mm", "amce"), "estimate", "difference"), levels = c("estimate", "difference")),
    estimate = ifelse(is.na(std.error), NA, estimate),
    level = factor(level, levels = c(rev(levels(h3_sup_mm$level)))),
    ref = ifelse(is.na(std.error) | is.na(estimate), "(ref)", NA),
    
    `Local identification:` = factor(ifelse(BY %in% c("low", "low - medium"), "low", 
                                            ifelse(BY %in% c("high", "high - medium"), "high", "medium")),
                                     levels = c("low", "medium", "high"), 
                                     labels = c("Weak", "Medium", "Strong")),
    
    lab = ifelse(BY == "Low local attachment" & level =="Raised and lives elsewhere","Low local \n attachment",
                 ifelse(BY == "High local attachment" & level =="Raised and lives locally","High local \n attachment",
                        NA) ),
    feature = factor(feature, levels = c("FeatDlocalism_grownlocal",  "FeatDlocalism_livelocal", "FeatBlocalism", "FeatSlocalism", "FeatGender", "FeatAge", "FeatOccupation", "FeatPartisanship"),
                     labels = c("Descriptive \n localism", "Descriptive \n localism", "Behavioral\n localism", "Symbolic\n localism", "Gender", "Age", "Occupation", "Party")),
    
  ) 
lowid <- data.frame(x=0.28, xend=subset(h3_sup_plot, fac=="MM" & feature == "Descriptive \n localism" & BY =="low")$estimate[1],
                    y=4.2, yend=3.95,
                    fac=factor(c("MM", "MM")),
                    feature=factor(c("Descriptive \n localism", "Descriptive \n localism")))

medid <- data.frame(x=0.28, xend=subset(h3_sup_plot, fac=="MM" & feature == "Descriptive \n localism" & BY =="medium")$estimate[2]-0.01,
                    y=3.2, yend=3.1,
                    fac=factor(c("MM", "MM")),
                    feature=factor(c("Descriptive \n localism", "Descriptive \n localism")))

highid <- data.frame(x=0.28, xend=subset(h3_sup_plot, fac=="MM" & feature == "Descriptive \n localism" & BY =="high")$estimate[4]-0.01,
                     y=1.3, yend=1.4,
                     fac=factor(c("MM", "MM")),
                     feature=factor(c("Descriptive \n localism", "Descriptive \n localism")))
dat_text <- data.frame(
  label = factor(c("Weak local\nidentification", "Medium local\nidentification", "Strong local\nidentification")),
  feature   = factor(c("Descriptive \n localism","Descriptive \n localism", "Descriptive \n localism")),
  fac = factor(c("MM", "MM", "MM")),
  x     = c(0.22, 0.22, 0.22),
  y     = c(4.2, 3.2, 1.3)
)


ggplot(h3_sup_plot, aes(x = estimate, y = level, shape = `Local identification:`, fill=`Local identification:`, xmax = upper, xmin = lower))+
  facet_grid(rows = vars(feature),
             cols = vars(fac),scales ="free" )+
  geom_vline(data=filter(h3_sup_plot, fac=="Difference in MM"), aes(xintercept=0), colour="darkgrey", linewidth=1) + 
  geom_vline(data=filter(h3_sup_plot, fac=="MM"), aes(xintercept= mean(main_data$FeatSup, na.rm = T)), colour="darkgrey", linewidth=1) + 
  geom_errorbarh(height = 0, position = position_dodge(width = 0.65))+
  geom_point(position = position_dodge(width = 0.65))+
  scale_shape_manual(values=c(21, 21, 21))+
  scale_fill_manual(values=c("white", "grey", "black"))+
  geom_curve(data = lowid, aes(y = y, x = x, xend = xend, yend = yend), inherit.aes = F, curvature = -0.4 , color = "grey30", linewidth = 0.5, arrow = arrow(length = unit(0.03, "npc")))+ 
  geom_curve(data = medid, aes(y = y, x = x, xend = xend, yend = yend), inherit.aes = F, curvature = -0.4 , color = "grey30", linewidth = 0.5, arrow = arrow(length = unit(0.03, "npc")))+ 
  geom_curve(data = highid, aes(y = y, x = x, xend = xend, yend = yend), inherit.aes = F, curvature = -0.3 , color = "grey30", linewidth = 0.5, arrow = arrow(length = unit(0.03, "npc"))) +
  geom_text(
    data  = dat_text,
    aes(x = x, y = y, label = label),
    size = 2.3, colour = "grey50", lineheight = .8, inherit.aes = F )+
  labs(x = "", y = "")+
  coord_cartesian(clip = 'off')+
  theme(axis.text = element_text(color = "black"),
        axis.line = element_blank(),
        axis.title = element_text(size = 8),
        legend.position="none", #"bottom",
        legend.title=element_text(size = 8),
        legend.background = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.key = element_blank(),
        panel.grid.major = element_line(linewidth = 0.1, linetype = 'solid', colour = "grey"), 
        panel.grid.minor = element_line(linewidth = 0.1, linetype = 'solid', colour = "grey"),
        panel.border = element_rect(fill=NA, colour = "black"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),
        strip.text.y = element_text(angle = 0, size = 8))


ggsave("Fig3.eps", plot = last_plot(),
       scale = 1, width = 174 , height = 126, units = c("mm"),
       dpi = 600)

#Appendix A:####
#Data exclusion
length(unique(subset(all_data, Prime==0 & Tot_time>4*60   | Prime==1 & Tot_time>4*60+15)$id))
length(unique(subset(all_data, Prime==0 & Tot_time>4*60   | Prime==1 & Tot_time>4*60+15)$id))/length(unique(all_data$id))

appa_data <- subset(all_data, Prime==0 & Tot_time>4*60   | Prime==1 & Tot_time>4*60+15) 

##H1:####
#Without controls

H1_fc <- lm_robust(Featfc ~   
                     #Candidate Features
                     #ref:Dlocalism_aa
                     FeatDlocalism_ll + FeatDlocalism_al + FeatDlocalism_la +
                     #ref:Blocalism
                     FeatBlocalism_low + FeatBlocalism_medium +FeatBlocalism_high +
                     #ref:Slocalism
                     FeatSlocalism_low + FeatSlocalism_lowmedium + FeatSlocalism_mediumhigh + FeatSlocalism_high +
                     #ref:Gender_Male
                     FeatGender_Female + 
                     #ref:Age_27_33
                     FeatAge_34_49 + FeatAge_50_63 + FeatAge_64_74 +
                     #ref:Occupation_lawyer
                     FeatOccupation_selfemployed + FeatOccupation_farmer + FeatOccupation_highschoolteacher + FeatOccupation_doctor+
                     #ref:Partisanship_A
                     FeatPartisanship_B + FeatPartisanship_C + FeatPartisanship_D + FeatPartisanship_F + FeatPartisanship_O + FeatPartisanship_V + FeatPartisanship_OE
                   , se_type = "CR2", data=appa_data, clusters = id)

H1_sup <- lm_robust(FeatSup ~   
                      #Candidate Features
                      #ref:Dlocalism_aa
                      FeatDlocalism_ll + FeatDlocalism_al + FeatDlocalism_la +
                      #ref:Blocalism
                      FeatBlocalism_low + FeatBlocalism_medium +FeatBlocalism_high +
                      #ref:Slocalism
                      FeatSlocalism_low + FeatSlocalism_lowmedium + FeatSlocalism_mediumhigh + FeatSlocalism_high +
                      #ref:Gender_Male
                      FeatGender_Female + 
                      #ref:Age_27_33
                      FeatAge_34_49 + FeatAge_50_63 + FeatAge_64_74 +
                      #ref:Occupation_lawyer
                      FeatOccupation_selfemployed + FeatOccupation_farmer + FeatOccupation_highschoolteacher + FeatOccupation_doctor+
                      #ref:Partisanship_A
                      FeatPartisanship_B + FeatPartisanship_C + FeatPartisanship_D + FeatPartisanship_F + FeatPartisanship_O + FeatPartisanship_V + FeatPartisanship_OE
                    , se_type = "CR2", data=appa_data, clusters = id)

#With controls
H1_fc_c <- lm_robust(Featfc ~
                       #Candidate Features
                       #ref:Dlocalism_aa
                       FeatDlocalism_ll + FeatDlocalism_al + FeatDlocalism_la +
                       #ref:Blocalism
                       FeatBlocalism_low + FeatBlocalism_medium +FeatBlocalism_high +
                       #ref:Slocalism
                       FeatSlocalism_low + FeatSlocalism_lowmedium + FeatSlocalism_mediumhigh + FeatSlocalism_high +
                       #ref:Gender_Male
                       FeatGender_Female + 
                       #ref:Age_27_33
                       FeatAge_34_49 + FeatAge_50_63 + FeatAge_64_74 +
                       #ref:Occupation_lawyer
                       FeatOccupation_selfemployed + FeatOccupation_farmer + FeatOccupation_highschoolteacher + FeatOccupation_doctor+
                       #ref:Partisanship_A
                       FeatPartisanship_B + FeatPartisanship_C + FeatPartisanship_D + FeatPartisanship_F + FeatPartisanship_O + FeatPartisanship_V + FeatPartisanship_OE+
                       SameAge +
                       SameGender +
                       SamePartisanship +
                       SameBlock+
                       SameOccupation
                     , se_type = "CR2", data=appa_data, clusters = id)


H1_sup_c <- lm_robust(FeatSup ~   
                        #Candidate Features
                        #ref:Dlocalism_aa
                        FeatDlocalism_ll + FeatDlocalism_al + FeatDlocalism_la +
                        #ref:Blocalism
                        FeatBlocalism_low + FeatBlocalism_medium +FeatBlocalism_high +
                        #ref:Slocalism
                        FeatSlocalism_low + FeatSlocalism_lowmedium + FeatSlocalism_mediumhigh + FeatSlocalism_high +
                        #ref:Gender_Male
                        FeatGender_Female + 
                        #ref:Age_27_33
                        FeatAge_34_49 + FeatAge_50_63 + FeatAge_64_74 +
                        #ref:Occupation_lawyer
                        FeatOccupation_selfemployed + FeatOccupation_farmer + FeatOccupation_highschoolteacher + FeatOccupation_doctor+
                        #ref:Partisanship_A
                        FeatPartisanship_B + FeatPartisanship_C + FeatPartisanship_D + FeatPartisanship_F + FeatPartisanship_O + FeatPartisanship_V + FeatPartisanship_OE+
                        SameAge +
                        SameGender +
                        SamePartisanship +
                        SameBlock+
                        SameOccupation
                      , se_type = "CR2", data=appa_data, clusters = id)

H1_fc <- tidy(H1_fc)
H1_fc$model <- "Forced choice"
H1_fc <- add_row(H1_fc, 
                 term=c("FeatDlocalism_aa","FeatBlocalism_null","FeatSlocalism_null", "FeatGender_Male","FeatAge_27_33","FeatOccupation_lawyer", "FeatPartisanship_A"),
                 estimate=rep(NA,7),
                 std.error=rep(NA,7),
                 statistic=rep(NA,7),
                 p.value=rep(NA,7),
                 conf.low=rep(NA,7),
                 conf.high=rep(NA,7),
                 df=rep(NA,7),
                 outcome=rep("FeatFc",7),
                 model=rep("Forced choice",7)
)

H1_sup <- tidy(H1_sup)
H1_sup$model <- "Rating Q"
H1_sup <- add_row(H1_sup, 
                  term=c("FeatDlocalism_aa","FeatBlocalism_null","FeatSlocalism_null", "FeatGender_Male","FeatAge_27_33","FeatOccupation_lawyer", "FeatPartisanship_A"),
                  estimate=rep(NA,7),
                  std.error=rep(NA,7),
                  statistic=rep(NA,7),
                  p.value=rep(NA,7),
                  conf.low=rep(NA,7),
                  conf.high=rep(NA,7),
                  df=rep(NA,7),
                  outcome=rep("FeatSup",7),
                  model=rep("Rating Q",7)
)


H1_fc_c <- tidy(H1_fc_c)
H1_fc_c$model <- "Forced choice \n +controls"
H1_fc_c <- add_row(H1_fc_c, 
                   term=c("FeatDlocalism_aa","FeatBlocalism_null","FeatSlocalism_null", "FeatGender_Male","FeatAge_27_33","FeatOccupation_lawyer","FeatPartisanship_A"),
                   estimate=rep(NA,7),
                   std.error=rep(NA,7),
                   statistic=rep(NA,7),
                   p.value=rep(NA,7),
                   conf.low=rep(NA,7),
                   conf.high=rep(NA,7),
                   df=rep(NA,7),
                   outcome=rep("FeatFc",7),
                   model=rep("Forced choice \n +controls",7)
)

H1_sup_c <- tidy(H1_sup_c)
H1_sup_c$model <- "Rating Q \n +controls"
H1_sup_c <- add_row(H1_sup_c, 
                    term=c("FeatDlocalism_aa","FeatBlocalism_null","FeatSlocalism_null", "FeatGender_Male","FeatAge_27_33","FeatOccupation_lawyer","FeatPartisanship_A"),
                    estimate=rep(NA,7),
                    std.error=rep(NA,7),
                    statistic=rep(NA,7),
                    p.value=rep(NA,7),
                    conf.low=rep(NA,7),
                    conf.high=rep(NA,7),
                    df=rep(NA,7),
                    outcome=rep("FeatSup",7),
                    model=rep("Rating Q \n +controls",7)
)

H1_plot <- H1_fc %>% 
  bind_rows(H1_sup, H1_fc_c, H1_sup_c) %>%
  subset(term!="(Intercept)") %>% 
  mutate(
    statistic = factor(ifelse(str_detect(term, "FeatDlocalism"), "Descriptive \n localism",
                              ifelse(str_detect(term, "FeatBlocalism"), "Behavioral \n localism",
                                     ifelse(str_detect(term, "FeatSlocalism"), "Symbolic \n localism",
                                            ifelse(str_detect(term, "FeatGender"), "Gender",
                                                   ifelse(str_detect(term, "FeatAge"), "Age",
                                                          ifelse(str_detect(term, "FeatOccupation"), "Occupation",
                                                                 ifelse(str_detect(term, "FeatPartisanship"), "Party",
                                                                        ifelse(str_detect(term, "Same"), "Correspondence",
                                                                               NA)))))))),
                       levels = c("Descriptive \n localism", "Behavioral \n localism", 
                                  "Symbolic \n localism", "Gender", "Age", "Occupation", "Party", "Correspondence")
    ),
    term=factor(term, levels= rev(c("FeatDlocalism_aa", "FeatDlocalism_la", "FeatDlocalism_al", "FeatDlocalism_ll",
                                    "FeatBlocalism_null", "FeatBlocalism_high", "FeatBlocalism_medium", "FeatBlocalism_low",
                                    "FeatSlocalism_null", "FeatSlocalism_high", "FeatSlocalism_mediumhigh", "FeatSlocalism_lowmedium", "FeatSlocalism_low", 
                                    "FeatGender_Male", "FeatGender_Female",
                                    "FeatAge_27_33", "FeatAge_34_49", "FeatAge_50_63", "FeatAge_64_74",
                                    "FeatOccupation_lawyer", "FeatOccupation_selfemployed","FeatOccupation_doctor", "FeatOccupation_farmer", "FeatOccupation_highschoolteacher",
                                    "FeatPartisanship_A", "FeatPartisanship_B","FeatPartisanship_C","FeatPartisanship_D", "FeatPartisanship_F","FeatPartisanship_O", "FeatPartisanship_V", "FeatPartisanship_OE", 
                                    "SameGenderNot same gender",
                                    "SameAgeNot similiar age",
                                    "SameOccupationNot same occutpation",
                                    "SameBlockNot same block",
                                    "SamePartisanshipNot same party")), 
                labels=rev(c(
                  "Raised and lives elsewhere",
                  "Raised locally, lives elsewhere",
                  "Raised elsewhere, lives locally",
                  "Raised and lives locally",
                  "No behavioral information",
                  "Primarily works on local issues",
                  "Splits time between national and local issues",
                  "Only works on national issues",
                  "No symbolic information",
                  "Knows names, active locally",
                  "Knows names, has been active locally",
                  "Seen during campaigns",
                  "Not seen regularly",
                  "Man",
                  "Woman",
                  "27-33",
                  "34-49",
                  "50-63",
                  "64-74",
                  "Lawyer",
                  "Self-employed",
                  "Farmer",
                  "High school teacher",
                  "Doctor",
                  "Social Democrats",
                  "Radical liberals",
                  "Conservative People's Party",
                  "The New Right",
                  "Socialist People's Party",
                  "Danish People's Party",
                  "Liberals",
                  "Unity list",
                  #OTHERS
                  "Same gender",
                  "Similiar age",
                  "Similiar occupation",
                  "Same political block",
                  "Same partisanship"))),
    model=factor(model, levels = c("Rating Q", "Rating Q \n +controls", "Forced choice", "Forced choice \n +controls"))
  )

ggplot(H1_plot, aes(x=estimate, y= term))+
  facet_grid(cols = vars(model),rows = vars(statistic), scales ="free" , space = "free")+
  geom_vline(aes(xintercept=0), colour="darkgrey", linewidth=1) + 
  geom_errorbarh(aes(xmax = conf.high, xmin = conf.low), height = 0, position = position_dodge(width = 0.5))+
  geom_text(data=filter(H1_plot, is.na(estimate)), aes(x=0, y= term, label = "ref" ), size = 2.5)+
  geom_point()+
  labs(x = "", y = "")+
  scale_x_continuous(breaks = c(-.2, -0.1, 0, 0.1 ), labels = c("-.2", "", "0", ".1"))+
  theme(axis.text = element_text(color = "black"),
        axis.line = element_blank(),
        panel.grid.major = element_line(size = 0.01, linetype = 'solid', colour = "grey"), 
        panel.border = element_rect(fill=NA, colour = "black"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),
        strip.text.y = element_text(angle = 0, size = 8),
        strip.text.x = element_text(size = 8),
        panel.spacing.x = unit(2, "mm"))

ggsave("Fig1A.eps", plot = last_plot(),
       scale = 1, width = 174 , height = 174, units = c("mm"),
       dpi = 600)

##H2:####
#Without controls
H2_fc_simp <- lm_robust(Featfc ~ 
                          #Candidate Features
                          #ref:Dlocalism_aa
                          FeatDlocalism_ll + FeatDlocalism_al + FeatDlocalism_la +
                          #ref:Blocalism_null
                          FeatBlocalism_plus+ #expect positive sign, but may be muted due to lower levels of behavioral localism.
                          FeatDlocalism_ll:FeatBlocalism_plus+ #expect negative sign
                          FeatDlocalism_al:FeatBlocalism_plus+ #expect negative sign
                          FeatDlocalism_la:FeatBlocalism_plus+ #expect negative sign
                          #ref:Blocalism_null
                          FeatSlocalism_plus+ #expect positive sign, but may be muted due to lower levels of behavioral localism.
                          FeatDlocalism_ll:FeatSlocalism_plus+ #expect negative sign
                          FeatDlocalism_al:FeatSlocalism_plus+ #expect negative sign
                          FeatDlocalism_la:FeatSlocalism_plus+ #expect negative sign
                          FeatBlocalism_plus:FeatSlocalism_plus + #expect negative sign
                          #ref:Gender_Male
                          FeatGender_Female + 
                          #ref:Age_27_33
                          FeatAge_34_49 + FeatAge_50_63 + FeatAge_64_74 +
                          #ref:Occupation_lawyer
                          FeatOccupation_selfemployed + FeatOccupation_farmer + FeatOccupation_highschoolteacher + FeatOccupation_doctor+
                          #ref:Partisanship_A
                          FeatPartisanship_B + FeatPartisanship_C + FeatPartisanship_D + FeatPartisanship_F + FeatPartisanship_O + FeatPartisanship_V + FeatPartisanship_OE
                        , se_type = "CR2", data=appa_data, clusters = id)


H2_fc_adv <- lm_robust(Featfc ~ 
                          #Candidate Features
                          #ref:Dlocalism_aa
                          FeatDlocalism_ll + FeatDlocalism_al + FeatDlocalism_la +
                          #ref:Blocalism_null
                          FeatBlocalism_negative + #expect neutral to negative sign
                          FeatBlocalism_positive + #expect neutral to negative sign
                          FeatDlocalism_ll:FeatBlocalism_negative+ #expect negative sign
                          FeatDlocalism_al:FeatBlocalism_negative+ #expect negative sign
                          FeatDlocalism_la:FeatBlocalism_negative+ #expect negative sign
                          FeatDlocalism_ll:FeatBlocalism_positive+ #expect negative sign
                          FeatDlocalism_al:FeatBlocalism_positive+ #expect negative sign
                          FeatDlocalism_la:FeatBlocalism_positive+ #expect negative sign
                          #ref:Slocalism_null
                          FeatSlocalism_negative + #expect neutral to negative sign
                          FeatSlocalism_positive + #expect positive sign
                          FeatDlocalism_ll:FeatSlocalism_negative+ #expect negative sign
                          FeatDlocalism_al:FeatSlocalism_negative+ #expect negative sign
                          FeatDlocalism_la:FeatSlocalism_negative+ #expect negative sign
                          FeatDlocalism_ll:FeatSlocalism_positive+ #expect negative sign
                          FeatDlocalism_al:FeatSlocalism_positive+ #expect negative sign
                          FeatDlocalism_la:FeatSlocalism_positive+ #expect negative sign
                          FeatBlocalism_negative:FeatSlocalism_negative+ #expect negative sign
                          FeatBlocalism_negative:FeatSlocalism_positive+ #expect negative sign
                          FeatBlocalism_positive:FeatSlocalism_negative+ #expect negative sign
                          FeatBlocalism_positive:FeatSlocalism_positive+ #expect negative sign
                          #ref:Gender_Male
                          FeatGender_Female + 
                          #ref:Age_27_33
                          FeatAge_34_49 + FeatAge_50_63 + FeatAge_64_74 +
                          #ref:Occupation_lawyer
                          FeatOccupation_selfemployed + FeatOccupation_farmer + FeatOccupation_highschoolteacher + FeatOccupation_doctor+
                          #ref:Partisanship_A
                          FeatPartisanship_B + FeatPartisanship_C + FeatPartisanship_D + FeatPartisanship_F + FeatPartisanship_O + FeatPartisanship_V + FeatPartisanship_OE
                          , se_type = "CR2", data=appa_data, clusters = id)


H2_sup_simp <- lm_robust(FeatSup ~ 
                           #Candidate Features
                           #ref:Dlocalism_aa
                           FeatDlocalism_ll + FeatDlocalism_al + FeatDlocalism_la +
                           #ref:Blocalism_null
                           FeatBlocalism_plus+ #expect positive sign, but may be muted due to lower levels of behavioral localism.
                           FeatDlocalism_ll:FeatBlocalism_plus+ #expect negative sign
                           FeatDlocalism_al:FeatBlocalism_plus+ #expect negative sign
                           FeatDlocalism_la:FeatBlocalism_plus+ #expect negative sign
                           #ref:Blocalism_null
                           FeatSlocalism_plus+ #expect positive sign, but may be muted due to lower levels of behavioral localism.
                           FeatDlocalism_ll:FeatSlocalism_plus+ #expect negative sign
                           FeatDlocalism_al:FeatSlocalism_plus+ #expect negative sign
                           FeatDlocalism_la:FeatSlocalism_plus+ #expect negative sign
                           FeatBlocalism_plus:FeatSlocalism_plus + #expect negative sign
                           #ref:Gender_Male
                           FeatGender_Female + 
                           #ref:Age_27_33
                           FeatAge_34_49 + FeatAge_50_63 + FeatAge_64_74 +
                           #ref:Occupation_lawyer
                           FeatOccupation_selfemployed + FeatOccupation_farmer + FeatOccupation_highschoolteacher + FeatOccupation_doctor+
                           #ref:Partisanship_A
                           FeatPartisanship_B + FeatPartisanship_C + FeatPartisanship_D + FeatPartisanship_F + FeatPartisanship_O + FeatPartisanship_V + FeatPartisanship_OE
                         , se_type = "CR2", data=appa_data, clusters = id)

H2_sup_adv <- lm_robust(FeatSup ~ 
                          #Candidate Features
                          #ref:Dlocalism_aa
                          FeatDlocalism_ll + FeatDlocalism_al + FeatDlocalism_la +
                          #ref:Blocalism_null
                          FeatBlocalism_negative + #expect neutral to negative sign
                          FeatBlocalism_positive + #expect neutral to negative sign
                          FeatDlocalism_ll:FeatBlocalism_negative+ #expect negative sign
                          FeatDlocalism_al:FeatBlocalism_negative+ #expect negative sign
                          FeatDlocalism_la:FeatBlocalism_negative+ #expect negative sign
                          FeatDlocalism_ll:FeatBlocalism_positive+ #expect negative sign
                          FeatDlocalism_al:FeatBlocalism_positive+ #expect negative sign
                          FeatDlocalism_la:FeatBlocalism_positive+ #expect negative sign
                          #ref:Slocalism_null
                          FeatSlocalism_negative + #expect neutral to negative sign
                          FeatSlocalism_positive + #expect positive sign
                          FeatDlocalism_ll:FeatSlocalism_negative+ #expect negative sign
                          FeatDlocalism_al:FeatSlocalism_negative+ #expect negative sign
                          FeatDlocalism_la:FeatSlocalism_negative+ #expect negative sign
                          FeatDlocalism_ll:FeatSlocalism_positive+ #expect negative sign
                          FeatDlocalism_al:FeatSlocalism_positive+ #expect negative sign
                          FeatDlocalism_la:FeatSlocalism_positive+ #expect negative sign
                          FeatBlocalism_negative:FeatSlocalism_negative+ #expect negative sign
                          FeatBlocalism_negative:FeatSlocalism_positive+ #expect negative sign
                          FeatBlocalism_positive:FeatSlocalism_negative+ #expect negative sign
                          FeatBlocalism_positive:FeatSlocalism_positive+ #expect negative sign
                          #ref:Gender_Male
                          FeatGender_Female + 
                          #ref:Age_27_33
                          FeatAge_34_49 + FeatAge_50_63 + FeatAge_64_74 +
                          #ref:Occupation_lawyer
                          FeatOccupation_selfemployed + FeatOccupation_farmer + FeatOccupation_highschoolteacher + FeatOccupation_doctor+
                          #ref:Partisanship_A
                          FeatPartisanship_B + FeatPartisanship_C + FeatPartisanship_D + FeatPartisanship_F + FeatPartisanship_O + FeatPartisanship_V + FeatPartisanship_OE
                        , se_type = "CR2", data=appa_data, clusters = id)

#With controls
H2_fc_simp_c <- lm_robust(Featfc ~ 
                            #Candidate Features
                            #ref:Dlocalism_aa
                            FeatDlocalism_ll + FeatDlocalism_al + FeatDlocalism_la +
                            #ref:Blocalism_null
                            FeatBlocalism_plus+ #expect positive sign, but may be muted due to lower levels of behavioral localism.
                            FeatDlocalism_ll:FeatBlocalism_plus+ #expect negative sign
                            FeatDlocalism_al:FeatBlocalism_plus+ #expect negative sign
                            FeatDlocalism_la:FeatBlocalism_plus+ #expect negative sign
                            #ref:Blocalism_null
                            FeatSlocalism_plus+ #expect positive sign, but may be muted due to lower levels of behavioral localism.
                            FeatDlocalism_ll:FeatSlocalism_plus+ #expect negative sign
                            FeatDlocalism_al:FeatSlocalism_plus+ #expect negative sign
                            FeatDlocalism_la:FeatSlocalism_plus+ #expect negative sign
                            FeatBlocalism_plus:FeatSlocalism_plus + #expect negative sign
                            #ref:Gender_Male
                            FeatGender_Female + 
                            #ref:Age_27_33
                            FeatAge_34_49 + FeatAge_50_63 + FeatAge_64_74 +
                            #ref:Occupation_lawyer
                            FeatOccupation_selfemployed + FeatOccupation_farmer + FeatOccupation_highschoolteacher + FeatOccupation_doctor+
                            #ref:Partisanship_A
                            FeatPartisanship_B + FeatPartisanship_C + FeatPartisanship_D + FeatPartisanship_F + FeatPartisanship_O + FeatPartisanship_V + FeatPartisanship_OE +
                            #Controls
                            SameAge +
                            SameGender +
                            SamePartisanship +
                            SameBlock+
                            SameOccupation
                          , se_type = "CR2", data=appa_data, clusters = id)

H2_fc_adv_c <- lm_robust(Featfc ~ 
                           #Candidate Features
                           #ref:Dlocalism_aa
                           FeatDlocalism_ll + FeatDlocalism_al + FeatDlocalism_la +
                           #ref:Blocalism_null
                           FeatBlocalism_negative + #expect neutral to negative sign
                           FeatBlocalism_positive + #expect neutral to negative sign
                           FeatDlocalism_ll:FeatBlocalism_negative+ #expect negative sign
                           FeatDlocalism_al:FeatBlocalism_negative+ #expect negative sign
                           FeatDlocalism_la:FeatBlocalism_negative+ #expect negative sign
                           FeatDlocalism_ll:FeatBlocalism_positive+ #expect negative sign
                           FeatDlocalism_al:FeatBlocalism_positive+ #expect negative sign
                           FeatDlocalism_la:FeatBlocalism_positive+ #expect negative sign
                           #ref:Slocalism_null
                           FeatSlocalism_negative + #expect neutral to negative sign
                           FeatSlocalism_positive + #expect positive sign
                           FeatDlocalism_ll:FeatSlocalism_negative+ #expect negative sign
                           FeatDlocalism_al:FeatSlocalism_negative+ #expect negative sign
                           FeatDlocalism_la:FeatSlocalism_negative+ #expect negative sign
                           FeatDlocalism_ll:FeatSlocalism_positive+ #expect negative sign
                           FeatDlocalism_al:FeatSlocalism_positive+ #expect negative sign
                           FeatDlocalism_la:FeatSlocalism_positive+ #expect negative sign
                           FeatBlocalism_negative:FeatSlocalism_negative+ #expect negative sign
                           FeatBlocalism_negative:FeatSlocalism_positive+ #expect negative sign
                           FeatBlocalism_positive:FeatSlocalism_negative+ #expect negative sign
                           FeatBlocalism_positive:FeatSlocalism_positive+ #expect negative sign
                           #ref:Gender_Male
                           FeatGender_Female + 
                           #ref:Age_27_33
                           FeatAge_34_49 + FeatAge_50_63 + FeatAge_64_74 +
                           #ref:Occupation_lawyer
                           FeatOccupation_selfemployed + FeatOccupation_farmer + FeatOccupation_highschoolteacher + FeatOccupation_doctor+
                           #ref:Partisanship_A
                           FeatPartisanship_B + FeatPartisanship_C + FeatPartisanship_D + FeatPartisanship_F + FeatPartisanship_O + FeatPartisanship_V + FeatPartisanship_OE + 
                           #Controls
                           SameAge +
                           SameGender +
                           SamePartisanship +
                           SameBlock+
                           SameOccupation
                         , se_type = "CR2", data=appa_data, clusters = id)

H2_sup_simp_c <- lm_robust(FeatSup ~ 
                             #Candidate Features
                             #ref:Dlocalism_aa
                             FeatDlocalism_ll + FeatDlocalism_al + FeatDlocalism_la +
                             #ref:Blocalism_null
                             FeatBlocalism_plus+ #expect positive sign, but may be muted due to lower levels of behavioral localism.
                             FeatDlocalism_ll:FeatBlocalism_plus+ #expect negative sign
                             FeatDlocalism_al:FeatBlocalism_plus+ #expect negative sign
                             FeatDlocalism_la:FeatBlocalism_plus+ #expect negative sign
                             #ref:Blocalism_null
                             FeatSlocalism_plus+ #expect positive sign, but may be muted due to lower levels of behavioral localism.
                             FeatDlocalism_ll:FeatSlocalism_plus+ #expect negative sign
                             FeatDlocalism_al:FeatSlocalism_plus+ #expect negative sign
                             FeatDlocalism_la:FeatSlocalism_plus+ #expect negative sign
                             FeatBlocalism_plus:FeatSlocalism_plus + #expect negative sign
                             #ref:Gender_Male
                             FeatGender_Female + 
                             #ref:Age_27_33
                             FeatAge_34_49 + FeatAge_50_63 + FeatAge_64_74 +
                             #ref:Occupation_lawyer
                             FeatOccupation_selfemployed + FeatOccupation_farmer + FeatOccupation_highschoolteacher + FeatOccupation_doctor+
                             #ref:Partisanship_A
                             FeatPartisanship_B + FeatPartisanship_C + FeatPartisanship_D + FeatPartisanship_F + FeatPartisanship_O + FeatPartisanship_V + FeatPartisanship_OE +
                             #Controls
                             SameAge +
                             SameGender +
                             SamePartisanship +
                             SameBlock+
                             SameOccupation
                           , se_type = "CR2", data=appa_data, clusters = id)

H2_sup_adv_c <- lm_robust(FeatSup ~ 
                            #Candidate Features
                            #ref:Dlocalism_aa
                            FeatDlocalism_ll + FeatDlocalism_al + FeatDlocalism_la +
                            #ref:Blocalism_null
                            FeatBlocalism_negative + #expect neutral to negative sign
                            FeatBlocalism_positive + #expect neutral to negative sign
                            FeatDlocalism_ll:FeatBlocalism_negative+ #expect negative sign
                            FeatDlocalism_al:FeatBlocalism_negative+ #expect negative sign
                            FeatDlocalism_la:FeatBlocalism_negative+ #expect negative sign
                            FeatDlocalism_ll:FeatBlocalism_positive+ #expect negative sign
                            FeatDlocalism_al:FeatBlocalism_positive+ #expect negative sign
                            FeatDlocalism_la:FeatBlocalism_positive+ #expect negative sign
                            #ref:Slocalism_null
                            FeatSlocalism_negative + #expect neutral to negative sign
                            FeatSlocalism_positive + #expect positive sign
                            FeatDlocalism_ll:FeatSlocalism_negative+ #expect negative sign
                            FeatDlocalism_al:FeatSlocalism_negative+ #expect negative sign
                            FeatDlocalism_la:FeatSlocalism_negative+ #expect negative sign
                            FeatDlocalism_ll:FeatSlocalism_positive+ #expect negative sign
                            FeatDlocalism_al:FeatSlocalism_positive+ #expect negative sign
                            FeatDlocalism_la:FeatSlocalism_positive+ #expect negative sign
                            FeatBlocalism_negative:FeatSlocalism_negative+ #expect negative sign
                            FeatBlocalism_negative:FeatSlocalism_positive+ #expect negative sign
                            FeatBlocalism_positive:FeatSlocalism_negative+ #expect negative sign
                            FeatBlocalism_positive:FeatSlocalism_positive+ #expect negative sign
                            #ref:Gender_Male
                            FeatGender_Female + 
                            #ref:Age_27_33
                            FeatAge_34_49 + FeatAge_50_63 + FeatAge_64_74 +
                            #ref:Occupation_lawyer
                            FeatOccupation_selfemployed + FeatOccupation_farmer + FeatOccupation_highschoolteacher + FeatOccupation_doctor+
                            #ref:Partisanship_A
                            FeatPartisanship_B + FeatPartisanship_C + FeatPartisanship_D + FeatPartisanship_F + FeatPartisanship_O + FeatPartisanship_V + FeatPartisanship_OE + 
                            #Controls
                            SameAge +
                            SameGender +
                            SamePartisanship +
                            SameBlock+
                            SameOccupation
                          , se_type = "CR2", data=appa_data, clusters = id)


tidy(H2_fc_simp_c)
tidy(H2_fc_adv_c)
tidy(H2_sup_simp_c)
tidy(H2_sup_adv_c)


texreg(list(H2_sup_simp, H2_sup_simp_c, H2_fc_simp, H2_fc_simp_c), include.ci = FALSE)
texreg(list(H2_sup_adv, H2_sup_adv_c, H2_fc_adv, H2_fc_adv_c), include.ci = FALSE)
##H3:####

H3_fc_simp_2st <- lm_robust(Featfc ~  local +
                              #Candidate Features
                              Featlocalism + #Expect positive sign
                              Featlocalism:local + #Expect positive sign 
                              #ref:Gender_Male
                              FeatGender_Female + 
                              #ref:Age_27_33
                              FeatAge_34_49 + FeatAge_50_63 + FeatAge_64_74 +
                              #ref:Occupation_lawyer
                              FeatOccupation_selfemployed + FeatOccupation_farmer + FeatOccupation_highschoolteacher + FeatOccupation_doctor+
                              #ref:Partisanship_A
                              FeatPartisanship_B + FeatPartisanship_C + FeatPartisanship_D + FeatPartisanship_F + FeatPartisanship_O + FeatPartisanship_V + FeatPartisanship_OE, 
                            se_type = "CR2", data=appa_data, clusters = id)

H3_sup_simp_2st <- lm_robust(FeatSup ~  local +
                               #Candidate Features
                               Featlocalism + #Expect positive sign
                               Featlocalism:local + #Expect positive sign 
                               #ref:Gender_Male
                               FeatGender_Female + 
                               #ref:Age_27_33
                               FeatAge_34_49 + FeatAge_50_63 + FeatAge_64_74 +
                               #ref:Occupation_lawyer
                               FeatOccupation_selfemployed + FeatOccupation_farmer + FeatOccupation_highschoolteacher + FeatOccupation_doctor+
                               #ref:Partisanship_A
                               FeatPartisanship_B + FeatPartisanship_C + FeatPartisanship_D + FeatPartisanship_F + FeatPartisanship_O + FeatPartisanship_V + FeatPartisanship_OE, 
                             se_type = "CR2", data=appa_data, clusters = id)

H3_fc_adv_2st <- lm_robust(Featfc ~ local +
                             #Candidate Features
                             #ref:Dlocalism_aa
                             FeatDlocalism_ll + #expect positive sign
                             FeatDlocalism_al + #expect positive sign
                             FeatDlocalism_la + #expect positive sign
                             #ref:Blocalism_
                             FeatBlocalism_low + #expect neutral to negative sign
                             FeatBlocalism_medium+ #expect positive sign
                             FeatBlocalism_high + #expect positive sign
                             #ref:Slocalism_
                             FeatSlocalism_low+ #expect neutral to negative sign
                             FeatSlocalism_lowmedium+ #expect neutral to negative sign
                             FeatSlocalism_mediumhigh+ #expect positive sign
                             FeatSlocalism_high+ #expect positive sign
                             #Interactions
                             FeatDlocalism_ll:local + #expect positive sign
                             FeatDlocalism_al:local + #expect positive sign
                             FeatDlocalism_la:local + #expect positive sign
                             FeatBlocalism_low:local + #expect positive sign
                             FeatBlocalism_medium:local+ #expect positive sign
                             FeatBlocalism_high:local + #expect positive sign
                             FeatSlocalism_low:local+ #expect positive sign
                             FeatSlocalism_lowmedium:local+ #expect positive sign
                             FeatSlocalism_mediumhigh:local+ #expect positive sign
                             FeatSlocalism_high:local+#expect positive sign
                             #ref:Gender_Male
                             FeatGender_Female + 
                             #ref:Age_27_33
                             FeatAge_34_49 + FeatAge_50_63 + FeatAge_64_74 +
                             #ref:Occupation_lawyer
                             FeatOccupation_selfemployed + FeatOccupation_farmer + FeatOccupation_highschoolteacher + FeatOccupation_doctor+
                             #ref:Partisanship_A
                             FeatPartisanship_B + FeatPartisanship_C + FeatPartisanship_D + FeatPartisanship_F + FeatPartisanship_O + FeatPartisanship_V + FeatPartisanship_OE, 
                             se_type = "CR2", data=appa_data, clusters = id)

H3_sup_adv_2st <- lm_robust(FeatSup ~ local +
                              #Candidate Features
                              #ref:Dlocalism_aa
                              FeatDlocalism_ll + #expect positive sign
                              FeatDlocalism_al + #expect positive sign
                              FeatDlocalism_la + #expect positive sign
                              #ref:Blocalism_
                              FeatBlocalism_low + #expect neutral to negative sign
                              FeatBlocalism_medium+ #expect positive sign
                              FeatBlocalism_high + #expect positive sign
                              #ref:Slocalism_
                              FeatSlocalism_low+ #expect neutral to negative sign
                              FeatSlocalism_lowmedium+ #expect neutral to negative sign
                              FeatSlocalism_mediumhigh+ #expect positive sign
                              FeatSlocalism_high+ #expect positive sign
                              #Interactions
                              FeatDlocalism_ll:local + #expect positive sign
                              FeatDlocalism_al:local + #expect positive sign
                              FeatDlocalism_la:local + #expect positive sign
                              FeatBlocalism_low:local + #expect positive sign
                              FeatBlocalism_medium:local+ #expect positive sign
                              FeatBlocalism_high:local + #expect positive sign
                              FeatSlocalism_low:local+ #expect positive sign
                              FeatSlocalism_lowmedium:local+ #expect positive sign
                              FeatSlocalism_mediumhigh:local+ #expect positive sign
                              FeatSlocalism_high:local+#expect positive sign
                              #ref:Gender_Male
                              FeatGender_Female + 
                              #ref:Age_27_33
                              FeatAge_34_49 + FeatAge_50_63 + FeatAge_64_74 +
                              #ref:Occupation_lawyer
                              FeatOccupation_selfemployed + FeatOccupation_farmer + FeatOccupation_highschoolteacher + FeatOccupation_doctor+
                              #ref:Partisanship_A
                              FeatPartisanship_B + FeatPartisanship_C + FeatPartisanship_D + FeatPartisanship_F + FeatPartisanship_O + FeatPartisanship_V + FeatPartisanship_OE, 
                            se_type = "CR2", data=appa_data, clusters = id)

H3_fc_simp_2st_c <- lm_robust(Featfc ~ local +
                                #Candidate Features
                                Featlocalism + #Expect positive sign
                                Featlocalism:local + #Expect positive sign 
                                #ref:Gender_Male
                                FeatGender_Female + 
                                #ref:Age_27_33
                                FeatAge_34_49 + FeatAge_50_63 + FeatAge_64_74 +
                                #ref:Occupation_lawyer
                                FeatOccupation_selfemployed + FeatOccupation_farmer + FeatOccupation_highschoolteacher + FeatOccupation_doctor+
                                #ref:Partisanship_A
                                FeatPartisanship_B + FeatPartisanship_C + FeatPartisanship_D + FeatPartisanship_F + FeatPartisanship_O + FeatPartisanship_V + FeatPartisanship_OE +
                                #Controls
                                SameAge + SameGender + SamePartisanship + SameBlock + SameOccupation,
                              se_type = "CR2", data=appa_data, clusters = id)


H3_sup_simp_2st_c <- lm_robust(FeatSup ~  local +
                                 #Candidate Features
                                 Featlocalism + #Expect positive sign
                                 Featlocalism:local + #Expect positive sign 
                                 #ref:Gender_Male
                                 FeatGender_Female + 
                                 #ref:Age_27_33
                                 FeatAge_34_49 + FeatAge_50_63 + FeatAge_64_74 +
                                 #ref:Occupation_lawyer
                                 FeatOccupation_selfemployed + FeatOccupation_farmer + FeatOccupation_highschoolteacher + FeatOccupation_doctor+
                                 #ref:Partisanship_A
                                 FeatPartisanship_B + FeatPartisanship_C + FeatPartisanship_D + FeatPartisanship_F + FeatPartisanship_O + FeatPartisanship_V + FeatPartisanship_OE + 
                                  #Controls
                                 SameAge + SameGender + SamePartisanship + SameBlock + SameOccupation,
                               se_type = "CR2", data=appa_data, clusters = id)

H3_fc_adv_2st_c <- lm_robust(Featfc ~ local +
                               #Candidate Features
                               #ref:Dlocalism_aa
                               FeatDlocalism_ll + #expect positive sign
                               FeatDlocalism_al + #expect positive sign
                               FeatDlocalism_la + #expect positive sign
                               #ref:Blocalism_
                               FeatBlocalism_low + #expect neutral to negative sign
                               FeatBlocalism_medium+ #expect positive sign
                               FeatBlocalism_high + #expect positive sign
                               #ref:Slocalism_
                               FeatSlocalism_low+ #expect neutral to negative sign
                               FeatSlocalism_lowmedium+ #expect neutral to negative sign
                               FeatSlocalism_mediumhigh+ #expect positive sign
                               FeatSlocalism_high+ #expect positive sign
                               #Interactions
                               FeatDlocalism_ll:local + #expect positive sign
                               FeatDlocalism_al:local + #expect positive sign
                               FeatDlocalism_la:local + #expect positive sign
                               FeatBlocalism_low:local + #expect positive sign
                               FeatBlocalism_medium:local+ #expect positive sign
                               FeatBlocalism_high:local + #expect positive sign
                               FeatSlocalism_low:local+ #expect positive sign
                               FeatSlocalism_lowmedium:local+ #expect positive sign
                               FeatSlocalism_mediumhigh:local+ #expect positive sign
                               FeatSlocalism_high:local+#expect positive sign
                               #ref:Gender_Male
                               FeatGender_Female + 
                               #ref:Age_27_33
                               FeatAge_34_49 + FeatAge_50_63 + FeatAge_64_74 +
                               #ref:Occupation_lawyer
                               FeatOccupation_selfemployed + FeatOccupation_farmer + FeatOccupation_highschoolteacher + FeatOccupation_doctor+
                               #ref:Partisanship_A
                               FeatPartisanship_B + FeatPartisanship_C + FeatPartisanship_D + FeatPartisanship_F + FeatPartisanship_O + FeatPartisanship_V + FeatPartisanship_OE + 
                               #Controls
                               SameAge + SameGender + SamePartisanship + SameBlock + SameOccupation,
                             se_type = "CR2", data=appa_data, clusters = id)

H3_sup_adv_2st_c <- lm_robust(FeatSup ~ local +
                                #Candidate Features
                                #ref:Dlocalism_aa
                                FeatDlocalism_ll + #expect positive sign
                                FeatDlocalism_al + #expect positive sign
                                FeatDlocalism_la + #expect positive sign
                                #ref:Blocalism_
                                FeatBlocalism_low + #expect neutral to negative sign
                                FeatBlocalism_medium+ #expect positive sign
                                FeatBlocalism_high + #expect positive sign
                                #ref:Slocalism_
                                FeatSlocalism_low+ #expect neutral to negative sign
                                FeatSlocalism_lowmedium+ #expect neutral to negative sign
                                FeatSlocalism_mediumhigh+ #expect positive sign
                                FeatSlocalism_high+ #expect positive sign
                                #Interactions
                                FeatDlocalism_ll:local + #expect positive sign
                                FeatDlocalism_al:local + #expect positive sign
                                FeatDlocalism_la:local + #expect positive sign
                                FeatBlocalism_low:local + #expect positive sign
                                FeatBlocalism_medium:local+ #expect positive sign
                                FeatBlocalism_high:local + #expect positive sign
                                FeatSlocalism_low:local+ #expect positive sign
                                FeatSlocalism_lowmedium:local+ #expect positive sign
                                FeatSlocalism_mediumhigh:local+ #expect positive sign
                                FeatSlocalism_high:local+#expect positive sign
                                #ref:Gender_Male
                                FeatGender_Female + 
                                #ref:Age_27_33
                                FeatAge_34_49 + FeatAge_50_63 + FeatAge_64_74 +
                                #ref:Occupation_lawyer
                                FeatOccupation_selfemployed + FeatOccupation_farmer + FeatOccupation_highschoolteacher + FeatOccupation_doctor+
                                #ref:Partisanship_A
                                FeatPartisanship_B + FeatPartisanship_C + FeatPartisanship_D + FeatPartisanship_F + FeatPartisanship_O + FeatPartisanship_V + FeatPartisanship_OE+ 
                                #Controls
                                SameAge + SameGender + SamePartisanship + SameBlock + SameOccupation,
                              se_type = "CR2", data=appa_data, clusters = id)
H3_fc_simp_2st
H3_sup_simp_2st
H3_fc_adv_2st
H3_sup_adv_2st
H3_fc_simp_2st_c
H3_sup_simp_2st_c
H3_fc_adv_2st_c
H3_sup_adv_2st_c

texreg(list(H3_sup_adv_2st,
            H3_sup_adv_2st_c,
            H3_fc_adv_2st,
            H3_fc_adv_2st_c), include.ci = FALSE
       
       
)


#Appendix B: Exclusion criterion####
appb_data <- subset(all_data, Prime==0 & Tot_time>4*60   | Prime==1 & Tot_time>4*60+15) 
simple <- formula(FeatSup ~ FeatDlocalism_grownlocal + FeatDlocalism_livelocal + FeatBlocalism + FeatSlocalism +
                    #Controls
                    FeatGender + FeatAge + FeatOccupation + FeatPartisanship)

controls <- formula(FeatSup ~ FeatDlocalism_grownlocal + FeatDlocalism_livelocal + FeatBlocalism + FeatSlocalism +
                      #Controls
                      FeatGender + FeatAge + FeatOccupation + FeatPartisanship +
                      SameGender + SameAge + SameOccupation + SamePartisanship + SameBlock)

##H1:####
h1_sup_amce <- amce(data = appb_data, 
                    simple,
                    id = ~ id, 
                    weights = ~ weight
)


h1_sup_c_amce <- amce(data = appb_data, 
                      controls,
                      id = ~ id, 
                      weights = ~ weight
)
h1_sup_mm <- mm(data = appb_data, 
                simple,
                id = ~ id, 
                weights = ~ weight
)

h1_sup_c_mm <- mm(data = appb_data, 
                  controls, 
                  id = ~ id, 
                  weights = ~weight)

h1_sup_c_mm %>% 
  union(h1_sup_c_amce) %>% 
  mutate(
    fac=factor(ifelse(statistic=="mm", 1,
                      ifelse(statistic=="amce", 2, NA)),
               levels = c(1,2), labels = c("MM","AMCE")),
    estimate = ifelse(is.na(std.error), NA, estimate),
    ref = ifelse(is.na(std.error) | is.na(estimate), "(ref)", NA),
    level = factor(level, levels = c(rev(levels(h1_sup_mm$level)))),
    feature = factor(feature, levels = c("FeatDlocalism_grownlocal",  "FeatDlocalism_livelocal", "FeatBlocalism", "FeatSlocalism", "FeatGender", "FeatAge", "FeatOccupation", "FeatPartisanship"),
                     labels = c("Descriptive \n localism", "Descriptive \n localism", "Behavioral\n localism", "Symbolic\n localism", "Gender", "Age", "Occupation", "Party")),
    statistic = factor(statistic, levels = c("mm", "amce"), 
                       labels = c("MM", "AMCE"))
  ) %>% 
  subset(F==is.na(level)) %>% 
  {.} -> h1_plot 

fig1<- ggplot(h1_plot, aes(x=estimate, y= level))+
  facet_grid(rows = vars(feature),
             cols = vars(statistic),scales ="free" , space = "free")+
  geom_vline(data=filter(h1_plot, fac=="AMCE"), aes(xintercept=0), colour="darkgrey", linewidth=1) + 
  geom_vline(data=filter(h1_plot, fac=="MM"), aes(xintercept=mean(appb_data$FeatSup, na.rm = T)), colour="darkgrey", linewidth=1) +  
  geom_errorbarh(aes(xmax = upper, xmin = lower), height = 0, position = position_dodge(width = 0.5))+
  geom_text(data=filter(h1_plot, fac=="AMCE"), aes(x=0, y= level, label = ref ), size = 2.5)+
  geom_point()+
  scale_x_continuous(breaks = c(-0.10, 0.0,0.1, 0.3, 0.4))+
  labs(x = "", y = "")+
  theme(axis.text = element_text(color = "black"),
        axis.line = element_blank(),
        legend.title = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.direction	="vertical",
        panel.grid.major = element_line(linewidth = 0.1, linetype = 'solid', colour = "grey"), 
        panel.grid.minor = element_line(linewidth = 0.1, linetype = 'solid', colour = "grey"),
        panel.border = element_rect(fill=NA, colour = "black"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),
        strip.text.y = element_text(angle = 0, size = 8))
ggsave("Fig1B.eps", 
       plot = fig1,
       scale = 1, width = 174 , height = 174, units = c("mm"),
       dpi = 600)

##H2:####
#Marginal means
#Baseline
###Do effect of Descriptive localism diminish when voters are provided with information on behavioral localism?####
h2_sup_B_mm <- cj(data = appb_data, 
                  FeatSup ~ FeatDlocalism_livelocal + FeatDlocalism_grownlocal,
                  by = ~ FeatBlocalism_null ,
                  id = ~ id,
                  estimate = "mm",
                  weights = ~ weight)

h2_sup_B_mm_diff <- cj(data = appb_data, 
                       FeatSup ~ FeatDlocalism_livelocal + FeatDlocalism_grownlocal,
                       by = ~ FeatBlocalism_null ,
                       id = ~ id,
                       estimate = "mm_diff",
                       weights = ~ weight)


h2_sup_B_amce <- cj(data = appb_data, 
                    FeatSup ~ FeatDlocalism_livelocal + FeatDlocalism_grownlocal,
                    by = ~ FeatBlocalism_null ,
                    id = ~ id,
                    estimate = "amce",
                    weights = ~ weight)

h2_sup_B_amce_diff <- cj(data = appb_data, 
                         FeatSup ~ FeatDlocalism_livelocal + FeatDlocalism_grownlocal,
                         by = ~ FeatBlocalism_null ,
                         id = ~ id,
                         estimate = "amce_diff",
                         weights = ~ weight)

h2_sup_B_mm %>% 
  bind_rows(h2_sup_B_mm_diff, h2_sup_B_amce, h2_sup_B_amce_diff) %>% 
  mutate(
    fac=factor(statistic, levels = c("mm",  "amce", "mm_difference", "amce_difference"), labels = c("MM", "AMCE", "Difference in MM", "Difference in AMCEs")),
    facc = factor(ifelse(statistic %in% c("amce", "amce_difference"), "AMCE", "MM"), levels = c("MM", "AMCE")),
    facr = factor(ifelse(statistic %in% c("mm", "amce"), "estimate", "difference"), levels = c("estimate", "difference")),
    estimate = ifelse(is.na(std.error), NA, estimate),
    ref = ifelse(is.na(std.error) | is.na(estimate), "(ref)", NA)
  ) %>% 
  add_row(fac = "Difference in AMCEs", level="Raised and lives elsewhere", ref = "(ref)") %>% 
  {.} -> h2_sup_plot 

h2mm <-
  ggplot(subset(h2_sup_plot, fac=="MM" ), aes(x=estimate, y= factor(level, levels  = c("Lives locally", "Lives elsewhere", "Raised locally", "Raised elsewhere")), 
                                              xmax = upper, xmin = lower, group = FeatBlocalism_null, shape = FeatBlocalism_null, fill=FeatBlocalism_null))+
  geom_vline(aes(xintercept=mean(appb_data$FeatSup, na.rm = T)), colour="darkgrey", linewidth=1) + 
  geom_linerange(position = position_dodge(width = 0.5))+
  geom_curve(aes(y = 4.25+0.1, x = 0.345, yend = 4.125+0.1, xend = subset(h2_sup_plot, fac=="MM" )$estimate[7]-0.005), curvature = -0.4 , color = "grey30", linewidth = 0.5, arrow = arrow(length = unit(0.03, "npc"))) +
  geom_curve(aes(y = 2.5, x = 0.335, yend = 2.85-0.1, xend = subset(h2_sup_plot, fac=="MM" )$estimate[4]), curvature = 0.4 , color = "grey30", linewidth = 0.5, arrow = arrow(length = unit(0.03, "npc"))) +
  geom_point(position = position_dodge(width =0.5)) + 
  scale_shape_manual(values=c(21, 21))+
  scale_fill_manual(values=c("white",  "black", "grey"))  +
  labs(y = "", x = "Likely to vote for candidate (0-1)")+
  geom_text(aes(y=4.26 , x=0.305, label = "Behavioral localism\ncue available"), size = 3, colour = "grey50", )+
  geom_text(aes(y=2.5 , x=0.3, label = "Cue not available" ), size = 3, colour = "grey50")+
  ggtitle("MM") +
  coord_cartesian(xlim = c(0.27, 0.43) ,clip = 'off')+
  theme(plot.title = element_text(size=12, hjust = .5),
        axis.line = element_blank(),
        axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 8),
        axis.text.y= element_text(color = "black"),
        axis.text.x= element_text(size = 8, color = "black"), 
        legend.position = "none",
        panel.grid.major = element_line(linewidth = 0.01, linetype = 'solid', colour = "grey"), 
        panel.grid.minor = element_line(linewidth = 0.01, linetype = 'solid', colour = "grey"),
        panel.border = element_rect(fill=NA, colour = "black"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank())

h2amce <-  
  ggplot(subset(h2_sup_plot, fac=="Difference in MM"),   aes(x=estimate, y= factor(level, levels  = c("Lives locally",
                                                                                                      "Lives elsewhere",
                                                                                                      "Raised locally",
                                                                                                      "Raised elsewhere")), 
                                                             xmax = upper, xmin = lower, group = FeatBlocalism_null, shape = FeatBlocalism_null, fill=FeatBlocalism_null))+
  geom_vline(aes(xintercept=0), colour="darkgrey", linewidth=1) + 
  geom_linerange(position = position_dodge(width = 0.5))+
  geom_point(position = position_dodge(width=0.5)) + 
  scale_shape_manual(values=c(21))+
  scale_fill_manual(values=c( "grey"))+
  geom_segment(aes(y = 1, yend = 1, x = 0.080, xend = 0.082),
               color = "grey30", lineend = "square") +
  geom_segment(aes(y = 2, yend = 2, x = 0.080, xend = 0.082),
               color = "grey30",lineend = "square") +
  geom_segment(aes(y = 3, yend = 3, x = 0.080, xend = 0.082),
               color = "grey30", lineend = "square") +
  geom_segment(aes(y = 4, yend = 4, x = 0.080, xend = 0.082),
               color = "grey30",lineend = "square") +
  geom_segment(aes(y = 2, yend = 1, x = 0.082, xend = 0.082),
               color = "grey30",lineend = "square") +
  geom_segment(aes(y = 3, yend = 4, x = 0.082, xend = 0.082),
               color = "grey30",) +
  
  geom_text(aes(x = 0.084, y = 1.5 , label = paste0("p=", round(subset(h2_sup_plot, fac=="Difference in AMCEs")$p[2], 2))),
            hjust = 0, size = 3, colour = "grey50")+
  geom_text(aes(x = 0.084, y = 3.5 , label = paste0("p=", round(subset(h2_sup_plot, fac=="Difference in AMCEs")$p[1], 2))),
            hjust = 0, size = 3, colour = "grey50")+
  coord_cartesian(xlim = c(-0.04, 0.125) ,clip = 'off')+
  labs(y = "", x = "Difference in score")+
  ggtitle("Difference in MMs") +
  theme(plot.title = element_text(size=12, hjust = .5),
        axis.line = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 8), 
        axis.text.y= element_blank(),
        axis.text.x= element_text(size = 8, color = "black"), 
        legend.position = "none",
        panel.grid.major = element_line(linewidth = 0.01, linetype = 'solid', colour = "grey"), 
        panel.grid.minor = element_line(linewidth = 0.01, linetype = 'solid', colour = "grey"),
        panel.border = element_rect(fill=NA, colour = "black"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank())

h2_bd_sup <- plot_grid(h2mm, h2amce, labels = c('', ''), rel_widths = c(1.1, 0.9), ncol = 2, nrow = 1 ,label_size = 12)

###Do effect of Descriptive localism diminish when voters are provided with information on symbolic localism?####
h2_sup_S_mm <- cj(data = appb_data, 
                  FeatSup ~ FeatDlocalism_livelocal + FeatDlocalism_grownlocal,
                  by = ~ FeatSlocalism_null ,
                  id = ~ id,
                  estimate = "mm",
                  weights = ~ weight
)

h2_sup_S_mm_diff <- cj(data = appb_data, 
                       FeatSup ~ FeatDlocalism_livelocal + FeatDlocalism_grownlocal,
                       by = ~ FeatSlocalism_null ,
                       id = ~ id,
                       estimate = "mm_diff",
                       weights = ~ weight)

h2_sup_S_amce <- cj(data = appb_data, 
                    FeatSup ~ FeatDlocalism_livelocal + FeatDlocalism_grownlocal,
                    by = ~ FeatSlocalism_null ,
                    id = ~ id,
                    estimate = "amce",
                    weights = ~ weight)

h2_sup_S_amce_diff <- cj(data = appb_data, 
                         FeatSup ~ FeatDlocalism_livelocal + FeatDlocalism_grownlocal ,
                         by = ~ FeatSlocalism_null ,
                         id = ~ id,
                         estimate = "amce_diff",
                         weights = ~ weight
)

h2_sup_S_mm %>% 
  bind_rows(h2_sup_S_mm_diff, h2_sup_S_amce, h2_sup_S_amce_diff) %>% 
  mutate(
    fac=factor(statistic, levels = c("mm",  "amce", "mm_difference", "amce_difference"), labels = c("MM", "AMCE", "Difference in MM", "Difference in AMCEs")),
    facc = factor(ifelse(statistic %in% c("amce", "amce_difference"), "AMCE", "MM"), levels = c("MM", "AMCE")),
    facr = factor(ifelse(statistic %in% c("mm", "amce"), "estimate", "difference"), levels = c("estimate", "difference")),
    estimate = ifelse(is.na(std.error), NA, estimate),
    ref = ifelse(is.na(std.error) | is.na(estimate), "(ref)", NA),
  ) %>% 
  add_row(fac = "Difference in AMCEs", level="Raised and lives elsewhere", ref = "(ref)") %>% 
  {.} -> h2_sup_plot 

h2mm <-
  ggplot(subset(h2_sup_plot, fac=="MM" ), aes(x=estimate, y= factor(level, levels  = c("Lives locally",
                                                                                       "Lives elsewhere",
                                                                                       "Raised locally",
                                                                                       "Raised elsewhere")), 
                                              xmax = upper, xmin = lower, group = FeatSlocalism_null, shape = FeatSlocalism_null, fill=FeatSlocalism_null)) +
  geom_vline(aes(xintercept=mean(appb_data$FeatSup, na.rm = T)), colour="darkgrey", linewidth=1) + 
  geom_linerange(position = position_dodge(width = 0.5))+
  geom_curve(aes(y = 4.25+0.1, x = 0.345, yend = 4.125+0.1, xend = subset(h2_sup_plot, fac=="MM" )$estimate[7]-0.005), curvature = -0.4 , color = "grey30", linewidth = 0.5, arrow = arrow(length = unit(0.03, "npc"))) +
  geom_curve(aes(y = 2.5, x = 0.335, yend = 2.85-0.1, xend = subset(h2_sup_plot, fac=="MM" )$estimate[4]), curvature = 0.4 , color = "grey30", linewidth = 0.5, arrow = arrow(length = unit(0.03, "npc"))) +
  geom_point(position = position_dodge(width =0.5)) + 
  scale_shape_manual(values=c(21, 21))+
  scale_fill_manual(values=c("white",  "black", "grey"))  +
  labs(y = "", x = "Likely to vote for candidate (0-1)")+
  geom_text(aes(y=4.26 , x=0.305, label = "Symbolic localism\ncue available"), size = 3, colour = "grey50", )+
  geom_text(aes(y=2.5 , x=0.3, label = "Cue not available" ), size = 3, colour = "grey50")+
  ggtitle("MM") +
  coord_cartesian(xlim = c(0.27, 0.43) ,clip = 'off')+
  theme(plot.title = element_text(size=12, hjust = .5),
        axis.line = element_blank(),
        axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 8),
        axis.text.y= element_text(color = "black"),
        axis.text.x= element_text(size = 8, color = "black"), 
        legend.position = "none",
        panel.grid.major = element_line(size = 0.01, linetype = 'solid', colour = "grey"), 
        panel.grid.minor = element_line(size = 0.01, linetype = 'solid', colour = "grey"),
        panel.border = element_rect(fill=NA, colour = "black"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank())

h2amce <- 
  ggplot(subset(h2_sup_plot, fac=="Difference in MM"),   aes(x=estimate, y= factor(level, levels  = c("Lives locally",
                                                                                                      "Lives elsewhere",
                                                                                                      "Raised locally",
                                                                                                      "Raised elsewhere")), 
                                                             xmax = upper, xmin = lower, group = FeatSlocalism_null, shape = FeatSlocalism_null, fill=FeatSlocalism_null)) +
  geom_vline(aes(xintercept=0), colour="darkgrey", linewidth=1) + 
  geom_linerange(position = position_dodge(width = 0.5))+
  geom_point(position = position_dodge(width=0.5)) + 
  scale_shape_manual(values=c(21))+
  scale_fill_manual(values=c( "grey"))+
  labs(y = "", x = "Difference in score")+
  ggtitle("Difference in MMs") +
  geom_segment(aes(y = 1, yend = 1, x = 0.080, xend = 0.082),
               color = "grey30", lineend = "square") +
  geom_segment(aes(y = 2, yend = 2, x = 0.080, xend = 0.082),
               color = "grey30",lineend = "square") +
  geom_segment(aes(y = 3, yend = 3, x = 0.080, xend = 0.082),
               color = "grey30", lineend = "square") +
  geom_segment(aes(y = 4, yend = 4, x = 0.080, xend = 0.082),
               color = "grey30",lineend = "square") +
  geom_segment(aes(y = 2, yend = 1, x = 0.082, xend = 0.082),
               color = "grey30",lineend = "square") +
  geom_segment(aes(y = 3, yend = 4, x = 0.082, xend = 0.082),
               color = "grey30",) +
  
  geom_text(aes(x = 0.084, y = 1.5 , label = paste0("p=", round(subset(h2_sup_plot, fac=="Difference in AMCEs")$p[2], 2))),
            hjust = 0, size = 3, colour = "grey50")+
  geom_text(aes(x = 0.084, y = 3.5 , label = paste0("p=", round(subset(h2_sup_plot, fac=="Difference in AMCEs")$p[1], 2))),
            hjust = 0, size = 3, colour = "grey50")+
  coord_cartesian(xlim = c(-0.04, 0.125) ,clip = 'off')+
  theme(plot.title = element_text(size=12, hjust = .5),
        axis.line = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 8), 
        axis.text.y= element_blank(),
        axis.text.x= element_text(size = 8, color = "black"), 
        legend.position = "none",
        panel.grid.major = element_line(size = 0.01, linetype = 'solid', colour = "grey"), 
        panel.grid.minor = element_line(size = 0.01, linetype = 'solid', colour = "grey"),
        panel.border = element_rect(fill=NA, colour = "black"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank())

h2_sd_sup <- plot_grid(h2mm, h2amce, labels = c('', ''), rel_widths = c(1.1, 0.9), ncol = 2, nrow = 1 ,label_size = 12)


###Do effect of Behavioral localism diminish when voters are provided with information on symbolic localism?####
h2_sup_BS_mm <- cj(data = appb_data, 
                   FeatSup ~ FeatBlocalism,
                   by = ~ FeatSlocalism_null ,
                   id = ~ id,
                   estimate = "mm",
                   weights = ~ weight
)

h2_sup_BS_mm_diff <- cj(data = appb_data, 
                        FeatSup ~ FeatBlocalism,
                        by = ~ FeatSlocalism_null ,
                        id = ~ id,
                        estimate = "mm_diff",
                        weights = ~ weight)



h2_sup_BS_amce <- cj(data = appb_data, 
                     FeatSup ~ FeatBlocalism,
                     by = ~ FeatSlocalism_null ,
                     id = ~ id,
                     estimate = "amce",
                     weights = ~ weight
)

h2_sup_BS_amce_diff <- cj(data = appb_data, 
                          FeatSup ~ FeatBlocalism,
                          by = ~ FeatSlocalism_null ,
                          id = ~ id,
                          estimate = "amce_diff",
                          weights = ~ weight
)


h2_sup_BS_mm %>% 
  bind_rows(h2_sup_BS_mm_diff, h2_sup_BS_amce, h2_sup_BS_amce_diff) %>% 
  mutate(
    fac=factor(statistic, levels = c("mm",  "amce", "mm_difference", "amce_difference"), labels = c("MM", "AMCE", "Difference in MM", "Difference in AMCEs")),
    facc = factor(ifelse(statistic %in% c("amce", "amce_difference"), "AMCE", "MM"), levels = c("MM", "AMCE")),
    facr = factor(ifelse(statistic %in% c("mm", "amce"), "estimate", "difference"), levels = c("estimate", "difference")),
    estimate = ifelse(is.na(std.error), NA, estimate),
    ref = ifelse(is.na(std.error) | is.na(estimate), "(ref)", NA),
  ) %>% 
  {.} -> h2_sup_plot 
h2mm <- 
  ggplot(subset(h2_sup_plot, fac=="MM" ), aes(x=estimate, y = factor(level, levels = c("Primarily works on local issues",
                                                                                       "Splits time between national and local issues",
                                                                                       "Only works on national issues",
                                                                                       "No behavioral information"),
                                                                     labels  = c("Works primarily \non local issues",
                                                                                 "Work split\nbetween national \nand local issues", 
                                                                                 "Only works on\nnational issues", 
                                                                                 "No behavioral\ninformation")),
                                              xmax = upper, xmin = lower, group = FeatSlocalism_null, shape = FeatSlocalism_null, fill=FeatSlocalism_null))+
  geom_vline(aes(xintercept=mean(appb_data$FeatSup, na.rm = T)), colour="darkgrey", size=1) + 
  geom_linerange(position = position_dodge(width = 0.5))+
  geom_curve(aes(y = 4.25+0.1, x = 0.359, yend = 4.125+0.1, xend = subset(h2_sup_plot, fac=="MM" )$estimate[5]+0.005), curvature = 0.4 , color = "grey30", linewidth = 0.5, arrow = arrow(length = unit(0.03, "npc"))) +
  geom_curve(aes(y = 2.5, x = 0.359, yend = 2.85-0.1, xend = subset(h2_sup_plot, fac=="MM" )$estimate[2]), curvature = -0.4 , color = "grey30", linewidth = 0.5, arrow = arrow(length = unit(0.03, "npc"))) +
  geom_point(position = position_dodge(width =0.5)) + 
  scale_shape_manual(values=c(21, 21))+
  scale_fill_manual(values=c("white",  "black", "grey"))  +
  labs(y = "", x = "Likely to vote for candidate (0-1)")+
  geom_text(aes(y=4.26 , x=0.395, label = "Symbolic localism\ncue available"), size = 3, colour = "grey50", )+
  geom_text(aes(y=2.5 , x=0.395, label = "Cue not available" ), size = 3, colour = "grey50")+
  ggtitle("MM") +
  coord_cartesian(xlim = c(0.27, 0.43) ,clip = 'off')+
  theme(plot.title = element_text(size=12, hjust = .5),
        axis.line = element_blank(),
        axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 8),
        axis.text.y= element_text(color = "black"),
        axis.text.x= element_text(size = 8, color = "black"), 
        legend.position = "none",
        panel.grid.major = element_line(size = 0.01, linetype = 'solid', colour = "grey"), 
        panel.grid.minor = element_line(size = 0.01, linetype = 'solid', colour = "grey"),
        panel.border = element_rect(fill=NA, colour = "black"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank())


h2amce <-ggplot(subset(h2_sup_plot, fac=="Difference in MM"),  aes(x=estimate, y=  factor(level, levels = c("Primarily works on local issues", "Splits time between national and local issues", "Only works on national issues","No behavioral information"),
                                                                                          labels  = c("Works primarily\non local issues", "Work split\nbetween national\nand local issues", "Only works on\nnational issues", "No behavioral\ninformation")),
                                                                   xmax = upper, xmin = lower, group = FeatSlocalism_null, shape = FeatSlocalism_null, fill=FeatSlocalism_null))+
  geom_vline(aes(xintercept=0), colour="darkgrey", linewidth=1) + 
  geom_linerange(position = position_dodge(width = 0.5))+
  geom_point(position = position_dodge(width=0.5)) + 
  scale_shape_manual(values=c(21))+
  scale_fill_manual(values=c( "grey"))+
  labs(y = "", x = "Difference in score")+
  ggtitle("Difference in MMs") +
  geom_segment(aes(y = 1, yend = 4, x = 0.082+0.019, xend = 0.082+0.019),
               color = "grey30", lineend = "square") +
  geom_segment(aes(y = 1, yend = 1, x = 0.080+0.019, xend = 0.082+0.019),
               color = "grey30", lineend = "square") +
  geom_segment(aes(y = 4, yend = 4, x = 0.080+0.019, xend = 0.082+0.019),
               color = "grey30", lineend = "square") +
  geom_text(aes(x = 0.084+0.019, y = 1.5 , label = paste0("p=", round(subset(h2_sup_plot, fac=="Difference in AMCEs")$p[2], 2))),
            hjust = 0, size = 3, colour = "grey50")+
  
  geom_segment(aes(y = 2, yend = 4, x = 0.089+0.019, xend = 0.089+0.019),
               color = "grey30", lineend = "square") +
  geom_segment(aes(y = 2, yend = 2, x = 0.087+0.019, xend = 0.089+0.019),
               color = "grey30", lineend = "square") +
  geom_segment(aes(y = 4, yend = 4, x = 0.087+0.019, xend = 0.089+0.019),
               color = "grey30", lineend = "square") +
  geom_text(aes(x = 0.091+0.019, y = 2.5 , label = paste0("p=", round(subset(h2_sup_plot, fac=="Difference in AMCEs")$p[3], 2))),
            hjust = 0, size = 3, colour = "grey50")+
  
  geom_segment(aes(y = 3, yend = 4, x = 0.096+0.019, xend = 0.096+0.019),
               color = "grey30", lineend = "square") +
  geom_segment(aes(y = 3, yend = 3, x = 0.094+0.019, xend = 0.096+0.019),
               color = "grey30", lineend = "square") +
  geom_segment(aes(y = 4, yend = 4, x = 0.094+0.019, xend = 0.096+0.019),
               color = "grey30", lineend = "square") +
  geom_text(aes(x = 0.098+0.019, y = 3.5 , label = paste0("p=", round(subset(h2_sup_plot, fac=="Difference in AMCEs")$p[1], 2))),
            hjust = 0, size = 3, colour = "grey50")+
  coord_cartesian(xlim = c(-0.04+0.016, 0.125+0.016) ,clip = 'off')+
  theme(plot.title = element_text(size=12, hjust = .5),
        axis.line = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 8), 
        axis.text.y= element_blank(),
        axis.text.x= element_text(size = 8, color = "black"), 
        legend.position = "none",
        panel.grid.major = element_line(size = 0.01, linetype = 'solid', colour = "grey"), 
        panel.grid.minor = element_line(size = 0.01, linetype = 'solid', colour = "grey"),
        panel.border = element_rect(fill=NA, colour = "black"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank())


h2_sb_sup <- plot_grid(h2mm, h2amce, labels = c('', ''), rel_widths = c(1.1, 0.9) , ncol = 2, nrow = 1 ,label_size = 12)


plot_grid(h2_bd_sup, h2_sd_sup, h2_sb_sup, labels = c('A', 'B', "C"), ncol = 1, nrow = 3 ,label_size = 12)

ggsave("Fig2B.eps", 
       plot = last_plot(),
       scale = 1, width = 174 , height = 190, units = c("mm"),
       dpi = 600)

##H3:####
appb_data <- appb_data %>% 
  mutate(
    flocal3=factor(ifelse(F==is.na(local) & local< 1/4, 0,
                          ifelse(F==is.na(local) & local>=1/4 & local< 3/5, 1,
                                 ifelse(F==is.na(local) & local>=3/5, 2, NA))), 
                   levels = c(0,1, 2, NA), labels= c("low", "medium", "high")))

summary(fixest::feols(FeatSup ~ FeatDlocalism_livelocal + FeatDlocalism_grownlocal + FeatBlocalism + flocal3+FeatSlocalism + 
                        FeatGender + FeatAge + FeatOccupation + FeatPartisanship +
                        SameGender + SameAge + SameOccupation + SamePartisanship + SameBlock +
                        gender +factor(education)+factor(income),
                      cluster = ~ id,
                      data = appa_data))

#Does effect vary with identification with local area?
h3_sup_mm<- cj(data = appa_data, 
               FeatSup ~ FeatDlocalism_livelocal + FeatDlocalism_grownlocal + FeatBlocalism + FeatSlocalism, 
               by = ~ flocal3,
               id = ~ id,
               estimate = "mm",
               weights = ~ weight)

appa_data$flocal3 <- factor(appa_data$flocal3, levels = c("medium", "low", "high"))
h3_sup_mm_diff<- cj(data = appa_data, 
                    FeatSup ~ FeatDlocalism_livelocal + FeatDlocalism_grownlocal + FeatBlocalism + FeatSlocalism , 
                    by = ~ flocal3,
                    id = ~ id,
                    estimate = "mm_diff",
                    weights = ~ weight)
h3_sup_amce<- cj(data = appa_data, 
                 FeatSup ~ FeatDlocalism_livelocal + FeatDlocalism_grownlocal + FeatBlocalism + FeatSlocalism , 
                 by = ~ flocal3,
                 id = ~ id,
                 estimate = "amce",
                 weights = ~ weight)

h3_sup_amce_diff<- cj(data = appa_data, 
                      Featfc ~ FeatDlocalism_livelocal + FeatDlocalism_grownlocal + FeatBlocalism + FeatSlocalism, 
                      by = ~ flocal3,
                      id = ~ id,
                      estimate = "amce_diff",
                      weights = ~ weight)
h3_sup_mm %>% 
  bind_rows(h3_sup_mm_diff) %>% 
  mutate(
    fac=factor(statistic, levels = c("mm", "mm_difference", "amce", "amce_difference"), labels = c("MM", "Difference in MM", "AMCE", "Difference in AMCEs")),
    facc = factor(ifelse(statistic %in% c("amce", "amce_difference"), "AMCE", "MM"), levels = c("MM", "AMCE")),
    facr = factor(ifelse(statistic %in% c("mm", "amce"), "estimate", "difference"), levels = c("estimate", "difference")),
    estimate = ifelse(is.na(std.error), NA, estimate),
    level = factor(level, levels = c(rev(levels(h3_sup_mm$level)))),
    ref = ifelse(is.na(std.error) | is.na(estimate), "(ref)", NA),
    
    `Local identification:` = factor(ifelse(BY %in% c("low", "low - medium"), "low", 
                                            ifelse(BY %in% c("high", "high - medium"), "high", "medium")),
                                     levels = c("low", "medium", "high"), 
                                     labels = c("Weak", "Medium", "Strong")),
    
    lab = ifelse(BY == "Low local attachment" & level =="Raised and lives elsewhere","Low local \n attachment",
                 ifelse(BY == "High local attachment" & level =="Raised and lives locally","High local \n attachment",
                        NA) ),
    feature = factor(feature, levels = c("FeatDlocalism_grownlocal",  "FeatDlocalism_livelocal", "FeatBlocalism", "FeatSlocalism", "FeatGender", "FeatAge", "FeatOccupation", "FeatPartisanship"),
                     labels = c("Descriptive \n localism", "Descriptive \n localism", "Behavioral\n localism", "Symbolic\n localism", "Gender", "Age", "Occupation", "Party")),
    
  ) %>% 
  {.} -> h3_sup_plot 

lowid <- data.frame(x=0.28, xend=subset(h3_sup_plot, fac=="MM" & feature == "Descriptive \n localism" & BY =="low")$estimate[1],
                    y=4.2, yend=3.95,
                    fac=factor(c("MM", "MM")),
                    feature=factor(c("Descriptive \n localism", "Descriptive \n localism")))

medid <- data.frame(x=0.28, xend=subset(h3_sup_plot, fac=="MM" & feature == "Descriptive \n localism" & BY =="medium")$estimate[2]-0.01,
                    y=3.2, yend=3.1,
                    fac=factor(c("MM", "MM")),
                    feature=factor(c("Descriptive \n localism", "Descriptive \n localism")))

highid <- data.frame(x=0.28, xend=subset(h3_sup_plot, fac=="MM" & feature == "Descriptive \n localism" & BY =="high")$estimate[4]-0.01,
                     y=1.3, yend=1.4,
                     fac=factor(c("MM", "MM")),
                     feature=factor(c("Descriptive \n localism", "Descriptive \n localism")))
dat_text <- data.frame(
  label = factor(c("Weak local\nidentification", "Medium local\nidentification", "Strong local\nidentification")),
  feature   = factor(c("Descriptive \n localism","Descriptive \n localism", "Descriptive \n localism")),
  fac = factor(c("MM", "MM", "MM")),
  x     = c(0.22, 0.22, 0.22),
  y     = c(4.2, 3.2, 1.3)
)


ggplot(h3_sup_plot, aes(x = estimate, y = level, shape = `Local identification:`, fill=`Local identification:`, xmax = upper, xmin = lower))+
  facet_grid(rows = vars(feature),
             cols = vars(fac),scales ="free" )+
  geom_vline(data=filter(h3_sup_plot, fac=="Difference in MM"), aes(xintercept=0), colour="darkgrey", linewidth=1) + 
  geom_vline(data=filter(h3_sup_plot, fac=="MM"), aes(xintercept= mean(appa_data$FeatSup, na.rm = T)), colour="darkgrey", linewidth=1) + 
  geom_errorbarh(height = 0, position = position_dodge(width = 0.65))+
  geom_point(position = position_dodge(width = 0.65))+
  scale_shape_manual(values=c(21, 21, 21))+
  scale_fill_manual(values=c("white", "grey", "black"))+
  geom_curve(data = lowid, aes(y = y, x = x, xend = xend, yend = yend), inherit.aes = F, curvature = -0.4 , color = "grey30", linewidth = 0.5, arrow = arrow(length = unit(0.03, "npc")))+ 
  geom_curve(data = medid, aes(y = y, x = x, xend = xend, yend = yend), inherit.aes = F, curvature = -0.4 , color = "grey30", linewidth = 0.5, arrow = arrow(length = unit(0.03, "npc")))+ 
  geom_curve(data = highid, aes(y = y, x = x, xend = xend, yend = yend), inherit.aes = F, curvature = -0.3 , color = "grey30", linewidth = 0.5, arrow = arrow(length = unit(0.03, "npc"))) +
  geom_text(
    data  = dat_text,
    aes(x = x, y = y, label = label),
    size = 2.3, colour = "grey50", lineheight = .8, inherit.aes = F )+
  labs(x = "", y = "")+
  coord_cartesian(clip = 'off')+
  theme(axis.text = element_text(color = "black"),
        axis.line = element_blank(),
        axis.title = element_text(size = 8),
        legend.position="none", #"bottom",
        legend.title=element_text(size = 8),
        legend.background = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.key = element_blank(),
        panel.grid.major = element_line(linewidth = 0.1, linetype = 'solid', colour = "grey"), 
        panel.grid.minor = element_line(linewidth = 0.1, linetype = 'solid', colour = "grey"),
        panel.border = element_rect(fill=NA, colour = "black"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),
        strip.text.y = element_text(angle = 0, size = 8))


ggsave("Fig3B.eps", plot = last_plot(),
       scale = 1, width = 174 , height = 126, units = c("mm"),
       dpi = 600)

#Appendix C: Sensitivity to choice of dependent variable####

simple <- formula(Featfc ~ FeatDlocalism_grownlocal + FeatDlocalism_livelocal + FeatBlocalism + FeatSlocalism +
                    #Controls
                    FeatGender + FeatAge + FeatOccupation + FeatPartisanship)

controls <- formula(Featfc ~ FeatDlocalism_grownlocal + FeatDlocalism_livelocal + FeatBlocalism + FeatSlocalism +
                      #Controls
                      FeatGender + FeatAge + FeatOccupation + FeatPartisanship +
                      SameGender + SameAge + SameOccupation + SamePartisanship + SameBlock)

##H1:####

h1_sup_amce <- amce(data = main_data, 
                    simple, 
                    id = ~ id, 
                    weights = ~ weight
)


h1_sup_c_amce <- amce(data = main_data, 
                      controls,
                      id = ~ id, 
                      weights = ~ weight
)
h1_sup_mm <- mm(data = main_data, 
                simple,
                id = ~ id, 
                weights = ~ weight
)

h1_sup_c_mm <- mm(data = main_data, 
                  controls,
                  id = ~ id, 
                  weights = ~weight)

h1_sup_c_mm %>% 
  bind_rows(h1_sup_c_amce) %>% 
  mutate(
    fac=factor(ifelse(statistic=="mm", 1,
                      ifelse(statistic=="amce", 2, NA)),
               levels = c(1,2), labels = c("MM","AMCE")),
    estimate = ifelse(is.na(std.error), NA, estimate),
    ref = ifelse(is.na(std.error) | is.na(estimate), "(ref)", NA),
    level = factor(level, levels = c(rev(levels(h1_sup_mm$level)))),
    feature = factor(feature, levels = c("FeatDlocalism_grownlocal",  "FeatDlocalism_livelocal", "FeatBlocalism", "FeatSlocalism", "FeatGender", "FeatAge", "FeatOccupation", "FeatPartisanship"),
                     labels = c("Descriptive \n localism", "Descriptive \n localism", "Behavioral\n localism", "Symbolic\n localism", "Gender", "Age", "Occupation", "Party")),
    statistic = factor(statistic, levels = c("mm", "amce"), 
                       labels = c("MM", "AMCE"))
  ) %>% 
  subset(F==is.na(level)) %>% 
  {.} -> h1_plot 

fig1<- ggplot(h1_plot, aes(x=estimate, y= level))+
  facet_grid(rows = vars(feature),
             cols = vars(statistic),scales ="free" , space = "free")+
  geom_vline(data=filter(h1_plot, fac=="AMCE"), aes(xintercept=0), colour="darkgrey", linewidth=1) + 
  geom_vline(data=filter(h1_plot, fac=="MM"), aes(xintercept=mean(main_data$Featfc, na.rm = T)), colour="darkgrey", linewidth=1) +  
  geom_errorbarh(aes(xmax = upper, xmin = lower), height = 0, position = position_dodge(width = 0.5))+
  geom_text(data=filter(h1_plot, fac=="AMCE"), aes(x=0, y= level, label = ref ), size = 2.5)+
  geom_point()+
  scale_x_continuous(breaks = c(-0.16,-0.08, 0.0,0.08, 0.16, 0.4, 0.5, 0.6))+
  labs(x = "", y = "")+
  theme(axis.text = element_text(color = "black"),
        axis.line = element_blank(),
        legend.title = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.direction	="vertical",
        panel.grid.major = element_line(linewidth = 0.1, linetype = 'solid', colour = "grey"), 
        panel.grid.minor = element_line(linewidth = 0.1, linetype = 'solid', colour = "grey"),
        panel.border = element_rect(fill=NA, colour = "black"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),
        strip.text.y = element_text(angle = 0, size = 8))
ggsave("Fig1C.eps", 
       plot = fig1,
       scale = 1, width = 174 , height = 174, units = c("mm"),
       dpi = 600)

##H2:####
#Marginal means
#Baseline
###Do effect of Descriptive localism diminish when voters are provided with information on behavioral localism?####

h2_sup_B_mm <- cj(data = main_data, 
                  Featfc ~ FeatDlocalism_livelocal + FeatDlocalism_grownlocal,
                  by = ~ FeatBlocalism_null ,
                  id = ~ id,
                  estimate = "mm",
                  weights = ~ weight)

h2_sup_B_mm_diff <- cj(data = main_data, 
                       Featfc ~ FeatDlocalism_livelocal + FeatDlocalism_grownlocal,
                       by = ~ FeatBlocalism_null ,
                       id = ~ id,
                       estimate = "mm_diff",
                       weights = ~ weight)


h2_sup_B_amce <- cj(data = main_data, 
                    Featfc ~ FeatDlocalism_livelocal + FeatDlocalism_grownlocal,
                    by = ~ FeatBlocalism_null ,
                    id = ~ id,
                    estimate = "amce",
                    weights = ~ weight)

h2_sup_B_amce_diff <- cj(data = main_data, 
                         Featfc ~ FeatDlocalism_livelocal + FeatDlocalism_grownlocal,
                         by = ~ FeatBlocalism_null ,
                         id = ~ id,
                         estimate = "amce_diff",
                         weights = ~ weight)


h2_sup_B_mm %>% 
  bind_rows(h2_sup_B_mm_diff, h2_sup_B_amce, h2_sup_B_amce_diff) %>% 
  mutate(
    fac=factor(statistic, levels = c("mm",  "amce", "mm_difference", "amce_difference"), labels = c("MM", "AMCE", "Difference in MM", "Difference in AMCEs")),
    facc = factor(ifelse(statistic %in% c("amce", "amce_difference"), "AMCE", "MM"), levels = c("MM", "AMCE")),
    facr = factor(ifelse(statistic %in% c("mm", "amce"), "estimate", "difference"), levels = c("estimate", "difference")),
    estimate = ifelse(is.na(std.error), NA, estimate),
    ref = ifelse(is.na(std.error) | is.na(estimate), "(ref)", NA)
  ) %>% 
  add_row(fac = "Difference in AMCEs", level="Raised and lives elsewhere", ref = "(ref)") %>% 
  {.} -> h2_sup_plot 
h2mm <-
  ggplot(subset(h2_sup_plot, fac=="MM" ), aes(x=estimate, y= factor(level, levels  = c("Lives locally",
                                                                                       "Lives elsewhere",
                                                                                       "Raised locally",
                                                                                       "Raised elsewhere")), 
                                              xmax = upper, xmin = lower, group = FeatBlocalism_null, shape = FeatBlocalism_null, fill=FeatBlocalism_null))+
  geom_vline(aes(xintercept=mean(main_data$Featfc, na.rm = T)), colour="darkgrey", size=1) + 
  geom_linerange(position = position_dodge(width = 0.5))+
  geom_curve(aes(y = 3.8, x = 0.58, yend = 3.125+0.1, xend = subset(h2_sup_plot, fac=="MM" )$estimate[8]+0.005), curvature = -0.1 , color = "grey30", linewidth = 0.5, arrow = arrow(length = unit(0.03, "npc"))) +
  geom_curve(aes(y = 2.5, x = 0.495, yend = 2.85-0.1, xend = subset(h2_sup_plot, fac=="MM" )$estimate[4]), curvature = -0.4 , color = "grey30", linewidth = 0.5, arrow = arrow(length = unit(0.03, "npc"))) +
  geom_point(position = position_dodge(width =0.5)) + 
  scale_shape_manual(values=c(21, 21))+
  scale_fill_manual(values=c("white",  "black", "grey"))  +
  labs(y = "", x = "Likely to pick candidate (0-1)")+
  geom_text(aes(y=4.26 , x=0.58, label = "Behavioral localism\ncue available"), size = 3, colour = "grey50", )+
  geom_text(aes(y=2.5 , x=0.567, label = "Cue not available" ), size = 3, colour = "grey50")+
  ggtitle("MM") +
  coord_cartesian(xlim = c(0.35, 0.65) ,clip = 'off')+
  theme(plot.title = element_text(size=12, hjust = .5),
        axis.line = element_blank(),
        axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 8),
        axis.text.y= element_text(color = "black"),
        axis.text.x= element_text(size = 8, color = "black"), 
        legend.position = "none",
        panel.grid.major = element_line(linewidth = 0.01, linetype = 'solid', colour = "grey"), 
        panel.grid.minor = element_line(linewidth = 0.01, linetype = 'solid', colour = "grey"),
        panel.border = element_rect(fill=NA, colour = "black"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank())

h2amce <-  
  ggplot(subset(h2_sup_plot, fac=="Difference in MM"),   aes(x=estimate, y= factor(level, levels  = c("Lives locally",
                                                                                                      "Lives elsewhere",
                                                                                                      "Raised locally",
                                                                                                      "Raised elsewhere")), 
                                                             xmax = upper, xmin = lower, group = FeatBlocalism_null, shape = FeatBlocalism_null, fill=FeatBlocalism_null))+
  geom_vline(aes(xintercept=0), colour="darkgrey", linewidth=1) + 
  geom_linerange(position = position_dodge(width = 0.5))+
  geom_point(position = position_dodge(width=0.5)) + 
  scale_shape_manual(values=c(21))+
  scale_fill_manual(values=c( "grey"))+
  geom_segment(aes(y = 1, yend = 1, x = 0.140, xend = 0.142),
               color = "grey30", lineend = "square") +
  geom_segment(aes(y = 2, yend = 2, x = 0.140, xend = 0.142),
               color = "grey30",lineend = "square") +
  geom_segment(aes(y = 3, yend = 3, x = 0.140, xend = 0.142),
               color = "grey30", lineend = "square") +
  geom_segment(aes(y = 4, yend = 4, x = 0.140, xend = 0.142),
               color = "grey30",lineend = "square") +
  geom_segment(aes(y = 2, yend = 1, x = 0.142, xend = 0.142),
               color = "grey30",lineend = "square") +
  geom_segment(aes(y = 3, yend = 4, x = 0.142, xend = 0.142),
               color = "grey30",) +
  
  geom_text(aes(x = 0.144, y = 1.5 , label = paste0("p=", round(subset(h2_sup_plot, fac=="Difference in AMCEs")$p[2], 2))),
            hjust = 0, size = 3, colour = "grey50")+
  geom_text(aes(x = 0.144, y = 3.5 , label = paste0("p=", round(subset(h2_sup_plot, fac=="Difference in AMCEs")$p[1], 2))),
            hjust = 0, size = 3, colour = "grey50")+
  coord_cartesian(xlim = c(-0.065, 0.25) ,clip = 'off')+
  labs(y = "", x = "Difference in score")+
  ggtitle("Difference in MMs") +
  theme(plot.title = element_text(size=12, hjust = .5),
        axis.line = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 8), 
        axis.text.y= element_blank(),
        axis.text.x= element_text(size = 8, color = "black"), 
        legend.position = "none",
        panel.grid.major = element_line(linewidth = 0.01, linetype = 'solid', colour = "grey"), 
        panel.grid.minor = element_line(linewidth = 0.01, linetype = 'solid', colour = "grey"),
        panel.border = element_rect(fill=NA, colour = "black"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank())

h2_bd_sup <- plot_grid(h2mm, h2amce, labels = c('', ''), rel_widths = c(1.1, 0.9), ncol = 2, nrow = 1 ,label_size = 12)

###Do effect of Descriptive localism diminish when voters are provided with information on symbolic localism?####
h2_sup_S_mm <- cj(data = main_data, 
                  Featfc ~ FeatDlocalism_livelocal + FeatDlocalism_grownlocal,
                  by = ~ FeatSlocalism_null ,
                  id = ~ id,
                  estimate = "mm",
                  weights = ~ weight
)

h2_sup_S_mm_diff <- cj(data = main_data, 
                       Featfc ~ FeatDlocalism_livelocal + FeatDlocalism_grownlocal,
                       by = ~ FeatSlocalism_null ,
                       id = ~ id,
                       estimate = "mm_diff",
                       weights = ~ weight)

h2_sup_S_amce <- cj(data = main_data, 
                    Featfc ~ FeatDlocalism_livelocal + FeatDlocalism_grownlocal,
                    by = ~ FeatSlocalism_null ,
                    id = ~ id,
                    estimate = "amce",
                    weights = ~ weight)

h2_sup_S_amce_diff <- cj(data = main_data, 
                         Featfc ~ FeatDlocalism_livelocal + FeatDlocalism_grownlocal ,
                         by = ~ FeatSlocalism_null ,
                         id = ~ id,
                         estimate = "amce_diff",
                         weights = ~ weight
)

h2_sup_S_mm %>% 
  bind_rows(h2_sup_S_mm_diff, h2_sup_S_amce, h2_sup_S_amce_diff) %>% 
  mutate(
    fac=factor(statistic, levels = c("mm",  "amce", "mm_difference", "amce_difference"), labels = c("MM", "AMCE", "Difference in MM", "Difference in AMCEs")),
    facc = factor(ifelse(statistic %in% c("amce", "amce_difference"), "AMCE", "MM"), levels = c("MM", "AMCE")),
    facr = factor(ifelse(statistic %in% c("mm", "amce"), "estimate", "difference"), levels = c("estimate", "difference")),
    estimate = ifelse(is.na(std.error), NA, estimate),
    ref = ifelse(is.na(std.error) | is.na(estimate), "(ref)", NA),
  ) %>% 
  add_row(fac = "Difference in AMCEs", level="Raised and lives elsewhere", ref = "(ref)") %>% 
  {.} -> h2_sup_plot 

h2mm <-
  ggplot(subset(h2_sup_plot, fac=="MM" ), aes(x=estimate, y= factor(level, levels  = c("Lives locally",
                                                                                       "Lives elsewhere",
                                                                                       "Raised locally",
                                                                                       "Raised elsewhere")), 
                                              xmax = upper, xmin = lower, group = FeatSlocalism_null, shape = FeatSlocalism_null, fill=FeatSlocalism_null)) +
  geom_vline(aes(xintercept=mean(main_data$Featfc, na.rm = T)), colour="darkgrey", size=1) + 
  geom_linerange(position = position_dodge(width = 0.5))+
  geom_curve(aes(y = 4.25+0.1, x = 0.51, yend = 4.125+0.1, xend = subset(h2_sup_plot, fac=="MM" )$estimate[7]), curvature = 0.4 , color = "grey30", linewidth = 0.5, arrow = arrow(length = unit(0.03, "npc"))) +
  geom_curve(aes(y = 2.5, x = 0.495, yend = 2.85-0.1, xend = subset(h2_sup_plot, fac=="MM" )$estimate[4]), curvature = -0.4 , color = "grey30", linewidth = 0.5, arrow = arrow(length = unit(0.03, "npc"))) +
  geom_point(position = position_dodge(width =0.5)) + 
  scale_shape_manual(values=c(21, 21))+
  scale_fill_manual(values=c("white",  "black", "grey"))  +
  labs(y = "", x = "Likely to pick candidate (0-1)")+
  geom_text(aes(y=4.26 , x=0.58, label = "Symbolic localism\ncue available"), size = 3, colour = "grey50", )+
  geom_text(aes(y=2.5 , x=0.567, label = "Cue not available" ), size = 3, colour = "grey50")+
  ggtitle("MM") +
  coord_cartesian(xlim = c(0.35, 0.65) ,clip = 'off')+
  theme(plot.title = element_text(size=12, hjust = .5),
        axis.line = element_blank(),
        axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 8),
        axis.text.y= element_text(color = "black"),
        axis.text.x= element_text(size = 8, color = "black"), 
        legend.position = "none",
        panel.grid.major = element_line(size = 0.01, linetype = 'solid', colour = "grey"), 
        panel.grid.minor = element_line(size = 0.01, linetype = 'solid', colour = "grey"),
        panel.border = element_rect(fill=NA, colour = "black"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank())

h2amce <- 
  ggplot(subset(h2_sup_plot, fac=="Difference in MM"),   aes(x=estimate, y= factor(level, levels  = c("Lives locally",
                                                                                                      "Lives elsewhere",
                                                                                                      "Raised locally",
                                                                                                      "Raised elsewhere")), 
                                                             xmax = upper, xmin = lower, group = FeatSlocalism_null, shape = FeatSlocalism_null, fill=FeatSlocalism_null)) +
  geom_vline(aes(xintercept=0), colour="darkgrey", linewidth=1) + 
  geom_linerange(position = position_dodge(width = 0.5))+
  geom_point(position = position_dodge(width=0.5)) + 
  scale_shape_manual(values=c(21))+
  scale_fill_manual(values=c( "grey"))+
  labs(y = "", x = "Difference in score")+
  ggtitle("Difference in MMs") +
  geom_segment(aes(y = 1, yend = 1, x = 0.140, xend = 0.142),
               color = "grey30", lineend = "square") +
  geom_segment(aes(y = 2, yend = 2, x = 0.140, xend = 0.142),
               color = "grey30",lineend = "square") +
  geom_segment(aes(y = 3, yend = 3, x = 0.140, xend = 0.142),
               color = "grey30", lineend = "square") +
  geom_segment(aes(y = 4, yend = 4, x = 0.140, xend = 0.142),
               color = "grey30",lineend = "square") +
  geom_segment(aes(y = 2, yend = 1, x = 0.142, xend = 0.142),
               color = "grey30",lineend = "square") +
  geom_segment(aes(y = 3, yend = 4, x = 0.142, xend = 0.142),
               color = "grey30",) +
  
  
  geom_text(aes(x = 0.144, y = 1.5 , label = paste0("p=", round(subset(h2_sup_plot, fac=="Difference in AMCEs")$p[2], 2))),
            hjust = 0, size = 3, colour = "grey50")+
  geom_text(aes(x = 0.144, y = 3.5 , label = paste0("p=", round(subset(h2_sup_plot, fac=="Difference in AMCEs")$p[1], 2))),
            hjust = 0, size = 3, colour = "grey50")+
  coord_cartesian(xlim = c(-0.065, 0.25) ,clip = 'off')+
  theme(plot.title = element_text(size=12, hjust = .5),
        axis.line = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 8), 
        axis.text.y= element_blank(),
        axis.text.x= element_text(size = 8, color = "black"), 
        legend.position = "none",
        panel.grid.major = element_line(size = 0.01, linetype = 'solid', colour = "grey"), 
        panel.grid.minor = element_line(size = 0.01, linetype = 'solid', colour = "grey"),
        panel.border = element_rect(fill=NA, colour = "black"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank())

h2_sd_sup <- plot_grid(h2mm, h2amce, labels = c('', ''), rel_widths = c(1.1, 0.9), ncol = 2, nrow = 1 ,label_size = 12)

###Do effect of Behavioral localism diminish when voters are provided with information on symbolic localism?####
h2_sup_BS_mm <- cj(data = main_data, 
                   Featfc ~ FeatBlocalism,
                   by = ~ FeatSlocalism_null ,
                   id = ~ id,
                   estimate = "mm",
                   weights = ~ weight
)

h2_sup_BS_mm_diff <- cj(data = main_data, 
                        Featfc ~ FeatBlocalism,
                        by = ~ FeatSlocalism_null ,
                        id = ~ id,
                        estimate = "mm_diff",
                        weights = ~ weight)



h2_sup_BS_amce <- cj(data = main_data, 
                     Featfc ~ FeatBlocalism,
                     by = ~ FeatSlocalism_null ,
                     id = ~ id,
                     estimate = "amce",
                     weights = ~ weight
)

h2_sup_BS_amce_diff <- cj(data = main_data, 
                          Featfc ~ FeatBlocalism,
                          by = ~ FeatSlocalism_null ,
                          id = ~ id,
                          estimate = "amce_diff",
                          weights = ~ weight
)


h2_sup_BS_mm %>% 
  bind_rows(h2_sup_BS_mm_diff, h2_sup_BS_amce, h2_sup_BS_amce_diff) %>% 
  mutate(
    fac=factor(statistic, levels = c("mm",  "amce", "mm_difference", "amce_difference"), labels = c("MM", "AMCE", "Difference in MM", "Difference in AMCEs")),
    facc = factor(ifelse(statistic %in% c("amce", "amce_difference"), "AMCE", "MM"), levels = c("MM", "AMCE")),
    facr = factor(ifelse(statistic %in% c("mm", "amce"), "estimate", "difference"), levels = c("estimate", "difference")),
    estimate = ifelse(is.na(std.error), NA, estimate),
    ref = ifelse(is.na(std.error) | is.na(estimate), "(ref)", NA),
  ) %>% 
  {.} -> h2_sup_plot 
h2mm <- 
  ggplot(subset(h2_sup_plot, fac=="MM" ), aes(x=estimate, y = factor(level, levels = c("Primarily works on local issues",
                                                                                       "Splits time between national and local issues",
                                                                                       "Only works on national issues",
                                                                                       "No behavioral information"),
                                                                     labels  = c("Works primarily \non local issues",
                                                                                 "Work split\nbetween national \nand local issues", 
                                                                                 "Only works on\nnational issues", 
                                                                                 "No behavioral\ninformation")),
                                              xmax = upper, xmin = lower, group = FeatSlocalism_null, shape = FeatSlocalism_null, fill=FeatSlocalism_null))+
  geom_vline(aes(xintercept=mean(main_data$Featfc, na.rm = T)), colour="darkgrey", size=1) + 
  geom_linerange(position = position_dodge(width = 0.5))+
  geom_curve(aes(y = 4.25+0.1, x = 0.505, yend = 4.125+0.1, xend = subset(h2_sup_plot, fac=="MM" )$estimate[5]+0.005), curvature = 0.4 , color = "grey30", linewidth = 0.5, arrow = arrow(length = unit(0.03, "npc"))) +
  geom_curve(aes(y = 2.5, x = 0.505, yend = 2.85-0.1, xend = subset(h2_sup_plot, fac=="MM" )$estimate[2]), curvature = -0.4 , color = "grey30", linewidth = 0.5, arrow = arrow(length = unit(0.03, "npc"))) +
  geom_point(position = position_dodge(width =0.5)) + 
  scale_shape_manual(values=c(21, 21))+
  scale_fill_manual(values=c("white",  "black", "grey"))  +
  labs(y = "", x = "Likely to pick candidate (0-1)")+
  geom_text(aes(y=4.26 , x=0.567, label = "Symbolic localism\ncue available"), size = 3, colour = "grey50", )+
  geom_text(aes(y=2.5 , x=0.567, label = "Cue not available" ), size = 3, colour = "grey50")+
  ggtitle("MM") +
  coord_cartesian(xlim = c(0.35, 0.65) ,clip = 'off')+
  theme(plot.title = element_text(size=12, hjust = .5),
        axis.line = element_blank(),
        axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 8),
        axis.text.y= element_text(color = "black"),
        axis.text.x= element_text(size = 8, color = "black"), 
        legend.position = "none",
        panel.grid.major = element_line(size = 0.01, linetype = 'solid', colour = "grey"), 
        panel.grid.minor = element_line(size = 0.01, linetype = 'solid', colour = "grey"),
        panel.border = element_rect(fill=NA, colour = "black"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank())


h2amce <-ggplot(subset(h2_sup_plot, fac=="Difference in MM"),  aes(x=estimate, y=  factor(level, levels = c("Primarily works on local issues", "Splits time between national and local issues", "Only works on national issues","No behavioral information"),
                                                                                          labels  = c("Works primarily\non local issues", "Work split\nbetween national\nand local issues", "Only works on\nnational issues", "No behavioral\ninformation")),
                                                                   xmax = upper, xmin = lower, group = FeatSlocalism_null, shape = FeatSlocalism_null, fill=FeatSlocalism_null))+
  geom_vline(aes(xintercept=0), colour="darkgrey", linewidth=1) + 
  geom_linerange(position = position_dodge(width = 0.5))+
  geom_point(position = position_dodge(width=0.5)) + 
  scale_shape_manual(values=c(21))+
  scale_fill_manual(values=c( "grey"))+
  labs(y = "", x = "Difference in score")+
  ggtitle("Difference in MMs") +
  
  geom_segment(aes(y = 1, yend = 4, x = 0.192, xend = 0.192),
               color = "grey30", lineend = "square") +
  geom_segment(aes(y = 1, yend = 1, x = 0.190, xend = 0.192),
               color = "grey30", lineend = "square") +
  geom_segment(aes(y = 4, yend = 4, x = 0.190, xend = 0.192),
               color = "grey30", lineend = "square") +
  geom_text(aes(x = 0.194, y = 1.5 , label = paste0("p=", round(subset(h2_sup_plot, fac=="Difference in AMCEs")$p[2], 2))),
            hjust = 0, size = 3, colour = "grey50")+
  
  geom_segment(aes(y = 2, yend = 4, x = 0.199, xend = 0.199),
               color = "grey30", lineend = "square") +
  geom_segment(aes(y = 2, yend = 2, x = 0.197, xend = 0.199),
               color = "grey30", lineend = "square") +
  geom_segment(aes(y = 4, yend = 4, x = 0.197, xend = 0.199),
               color = "grey30", lineend = "square") +
  geom_text(aes(x = 0.201, y = 2.5 , label = paste0("p=", round(subset(h2_sup_plot, fac=="Difference in AMCEs")$p[3], 2))),
            hjust = 0, size = 3, colour = "grey50")+
  
  geom_segment(aes(y = 3, yend = 4, x = 0.206, xend = 0.206),
               color = "grey30", lineend = "square") +
  geom_segment(aes(y = 3, yend = 3, x = 0.204, xend = 0.206),
               color = "grey30", lineend = "square") +
  geom_segment(aes(y = 4, yend = 4, x = 0.204, xend = 0.206),
               color = "grey30", lineend = "square") +
  geom_text(aes(x = 0.208, y = 3.5 , label = paste0("p=", round(subset(h2_sup_plot, fac=="Difference in AMCEs")$p[1], 2))),
            hjust = 0, size = 3, colour = "grey50")+
  coord_cartesian(xlim = c(-0.065, 0.25) ,clip = 'off')+
  theme(plot.title = element_text(size=12, hjust = .5),
        axis.line = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 8), 
        axis.text.y= element_blank(),
        axis.text.x= element_text(size = 8, color = "black"), 
        legend.position = "none",
        panel.grid.major = element_line(size = 0.01, linetype = 'solid', colour = "grey"), 
        panel.grid.minor = element_line(size = 0.01, linetype = 'solid', colour = "grey"),
        panel.border = element_rect(fill=NA, colour = "black"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank())


h2_sb_sup <- plot_grid(h2mm, h2amce, labels = c('', ''), rel_widths = c(1.1, 0.9) , ncol = 2, nrow = 1 ,label_size = 12)

plot_grid(h2_bd_sup, h2_sd_sup, h2_sb_sup, labels = c('A', 'B', "C"), ncol = 1, nrow = 3 ,label_size = 12)

ggsave("Fig2C.eps", 
       plot = last_plot(),
       scale = 1, width = 174 , height = 190, units = c("mm"),
       dpi = 600)

##H3:####
main_data %>% 
  mutate(
    flocal3=factor(ifelse(F==is.na(local) & local< 1/4, 0,
                          ifelse(F==is.na(local) & local>=1/4 & local< 3/5, 1,
                                 ifelse(F==is.na(local) & local>=3/5, 2, NA))), 
                   levels = c(0,1, 2, NA), labels= c("low", "medium", "high"))) %>% 
  {.}-> main_data



summary(fixest::feols(Featfc ~ FeatDlocalism_livelocal + FeatDlocalism_grownlocal + FeatBlocalism + flocal3+FeatSlocalism + 
                        FeatGender + FeatAge + FeatOccupation + FeatPartisanship +
                        SameGender + SameAge + SameOccupation + SamePartisanship + SameBlock +
                        gender +factor(education)+factor(income),
                      cluster = ~ id,
                      data = main_data))


h3_sup_mm<- cj(data = main_data, 
               Featfc ~ FeatDlocalism_livelocal + FeatDlocalism_grownlocal + FeatBlocalism + FeatSlocalism, 
               by = ~ flocal3,
               id = ~ id,
               estimate = "mm",
               weights = ~ weight)

main_data$flocal3 <- factor(main_data$flocal3, levels = c("medium", "low", "high"))
h3_sup_mm_diff<- cj(data = main_data, 
                    Featfc ~ FeatDlocalism_livelocal + FeatDlocalism_grownlocal + FeatBlocalism + FeatSlocalism , 
                    by = ~ flocal3,
                    id = ~ id,
                    estimate = "mm_diff",
                    weights = ~ weight)
h3_sup_amce<- cj(data = main_data, 
                 Featfc ~ FeatDlocalism_livelocal + FeatDlocalism_grownlocal + FeatBlocalism + FeatSlocalism , 
                 by = ~ flocal3,
                 id = ~ id,
                 estimate = "amce",
                 weights = ~ weight)

h3_sup_amce_diff<- cj(data = main_data, 
                      Featfc ~ FeatDlocalism_livelocal + FeatDlocalism_grownlocal + FeatBlocalism + FeatSlocalism, 
                      by = ~ flocal3,
                      id = ~ id,
                      estimate = "amce_diff",
                      weights = ~ weight)
h3_sup_mm %>% 
  bind_rows(h3_sup_mm_diff) %>% 
  mutate(
    fac=factor(statistic, levels = c("mm", "mm_difference", "amce", "amce_difference"), labels = c("MM", "Difference in MM", "AMCE", "Difference in AMCEs")),
    facc = factor(ifelse(statistic %in% c("amce", "amce_difference"), "AMCE", "MM"), levels = c("MM", "AMCE")),
    facr = factor(ifelse(statistic %in% c("mm", "amce"), "estimate", "difference"), levels = c("estimate", "difference")),
    estimate = ifelse(is.na(std.error), NA, estimate),
    level = factor(level, levels = c(rev(levels(h3_sup_mm$level)))),
    ref = ifelse(is.na(std.error) | is.na(estimate), "(ref)", NA),
    
    `Local identification:` = factor(ifelse(BY %in% c("low", "low - medium"), "low", 
                                            ifelse(BY %in% c("high", "high - medium"), "high", "medium")),
                                     levels = c("low", "medium", "high"), 
                                     labels = c("Weak", "Medium", "Strong")),
    
    lab = ifelse(BY == "Low local attachment" & level =="Raised and lives elsewhere","Low local \n attachment",
                 ifelse(BY == "High local attachment" & level =="Raised and lives locally","High local \n attachment",
                        NA) ),
    feature = factor(feature, levels = c("FeatDlocalism_grownlocal",  "FeatDlocalism_livelocal", "FeatBlocalism", "FeatSlocalism", "FeatGender", "FeatAge", "FeatOccupation", "FeatPartisanship"),
                     labels = c("Descriptive \n localism", "Descriptive \n localism", "Behavioral\n localism", "Symbolic\n localism", "Gender", "Age", "Occupation", "Party")),
    
  ) %>% 
  {.} -> h3_sup_plot 

lowid <- data.frame(x=0.38, xend=subset(h3_sup_plot, fac=="MM" & feature == "Descriptive \n localism" & BY =="low")$estimate[1]-0.01,
                    y=3.8, yend=3.65,
                    fac=factor(c("MM", "MM")),
                    feature=factor(c("Descriptive \n localism", "Descriptive \n localism")))

medid <- data.frame(x=0.44, xend=subset(h3_sup_plot, fac=="MM" & feature == "Descriptive \n localism" & BY =="medium")$estimate[2]-0.01,
                    y=3.2, yend=3.1,
                    fac=factor(c("MM", "MM")),
                    feature=factor(c("Descriptive \n localism", "Descriptive \n localism")))

highid <- data.frame(x=0.44, xend=subset(h3_sup_plot, fac=="MM" & feature == "Descriptive \n localism" & BY =="high")$estimate[4]-0.01,
                     y=1.3, yend=1.4,
                     fac=factor(c("MM", "MM")),
                     feature=factor(c("Descriptive \n localism", "Descriptive \n localism")))
dat_text <- data.frame(
  label = factor(c("Weak local\nidentification", "Medium local\nidentification", "Strong local\nidentification")),
  feature   = factor(c("Descriptive \n localism","Descriptive \n localism", "Descriptive \n localism")),
  fac = factor(c("MM", "MM", "MM")),
  x     = c(0.36, 0.36, 0.36),
  y     = c(4.2, 3.2, 1.3)
)


ggplot(h3_sup_plot, aes(x = estimate, y = level, shape = `Local identification:`, fill=`Local identification:`, xmax = upper, xmin = lower))+
  facet_grid(rows = vars(feature),
             cols = vars(fac),scales ="free" )+
  geom_vline(data=filter(h3_sup_plot, fac=="Difference in MM"), aes(xintercept=0), colour="darkgrey", linewidth=1) + 
  geom_vline(data=filter(h3_sup_plot, fac=="MM"), aes(xintercept= mean(main_data$Featfc, na.rm = T)), colour="darkgrey", linewidth=1) + 
  geom_errorbarh(height = 0, position = position_dodge(width = 0.65))+
  geom_point(position = position_dodge(width = 0.65))+
  scale_shape_manual(values=c(21, 21, 21))+
  scale_fill_manual(values=c("white", "grey", "black"))+
  geom_curve(data = lowid, aes(y = y, x = x, xend = xend, yend = yend), inherit.aes = F, curvature = +0.4 , color = "grey30", linewidth = 0.5, arrow = arrow(length = unit(0.03, "npc")))+ 
  geom_curve(data = medid, aes(y = y, x = x, xend = xend, yend = yend), inherit.aes = F, curvature = -0.4 , color = "grey30", linewidth = 0.5, arrow = arrow(length = unit(0.03, "npc")))+ 
  geom_curve(data = highid, aes(y = y, x = x, xend = xend, yend = yend), inherit.aes = F, curvature = -0.3 , color = "grey30", linewidth = 0.5, arrow = arrow(length = unit(0.03, "npc"))) +
  geom_text(
    data  = dat_text,
    aes(x = x, y = y, label = label),
    size = 2.3, colour = "grey50", lineheight = .8, inherit.aes = F )+
  labs(x = "", y = "")+
  coord_cartesian(clip = 'off')+
  theme(axis.text = element_text(color = "black"),
        axis.line = element_blank(),
        axis.title = element_text(size = 8),
        legend.position="none", #"bottom",
        legend.title=element_text(size = 8),
        legend.background = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.key = element_blank(),
        panel.grid.major = element_line(linewidth = 0.1, linetype = 'solid', colour = "grey"), 
        panel.grid.minor = element_line(linewidth = 0.1, linetype = 'solid', colour = "grey"),
        panel.border = element_rect(fill=NA, colour = "black"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),
        strip.text.y = element_text(angle = 0, size = 8))


ggsave("Fig3C.eps", plot = last_plot(),
       scale = 1, width = 174 , height = 126, units = c("mm"),
       dpi = 600)

#Appendix D: Stability and no-carryover effects and no-profile order effects####
#Test whether there is a difference in candidate evaluations depending on the round. Here an example of the test, with regard to H1 forced choice. Similiar tests will be conducted for other models:
main_data$contnr <- factor(main_data$contestnr)

h1_sup_c_amce_task1 <- amce(data = subset(main_data, contestnr == "conj1"), 
                            FeatSup ~ FeatDlocalism_grownlocal + FeatDlocalism_livelocal + FeatBlocalism + FeatSlocalism +
                              #Controls
                              FeatGender + FeatAge + FeatOccupation + FeatPartisanship +
                              SameGender + SameAge + SameOccupation + SamePartisanship + SameBlock,
                            id = ~ id, 
                            weights = ~ weight
)
h1_sup_c_amce_task2 <- amce(data = subset(main_data, contestnr == "conj2"), 
                            FeatSup ~ FeatDlocalism_grownlocal + FeatDlocalism_livelocal + FeatBlocalism + FeatSlocalism +
                              #Controls
                              FeatGender + FeatAge + FeatOccupation + FeatPartisanship +
                              SameGender + SameAge + SameOccupation + SamePartisanship + SameBlock,
                            id = ~ id, 
                            weights = ~ weight
)
h1_sup_c_amce_task3 <- amce(data = subset(main_data, contestnr == "conj3"), 
                            FeatSup ~ FeatDlocalism_grownlocal + FeatDlocalism_livelocal + FeatBlocalism + FeatSlocalism +
                              #Controls
                              FeatGender + FeatAge + FeatOccupation + FeatPartisanship +
                              SameGender + SameAge + SameOccupation + SamePartisanship + SameBlock,
                            id = ~ id, 
                            weights = ~ weight
)
h1_sup_c_amce_task4 <- amce(data = subset(main_data, contestnr == "conj4"), 
                            FeatSup ~ FeatDlocalism_grownlocal + FeatDlocalism_livelocal + FeatBlocalism + FeatSlocalism +
                              #Controls
                              FeatGender + FeatAge + FeatOccupation + FeatPartisanship +
                              SameGender + SameAge + SameOccupation + SamePartisanship + SameBlock,
                            id = ~ id, 
                            weights = ~ weight
)
h1_sup_c_amce_task5 <- amce(data = subset(main_data, contestnr == "conj5"), 
                            FeatSup ~ FeatDlocalism_grownlocal + FeatDlocalism_livelocal + FeatBlocalism + FeatSlocalism +
                              #Controls
                              FeatGender + FeatAge + FeatOccupation + FeatPartisanship +
                              SameGender + SameAge + SameOccupation + SamePartisanship + SameBlock,
                            id = ~ id, 
                            weights = ~ weight
)

h1_sup_c_amce_task1$tasknr <- "task1"
h1_sup_c_amce_task2$tasknr <- "task2"
h1_sup_c_amce_task3$tasknr <- "task3"
h1_sup_c_amce_task4$tasknr <- "task4"
h1_sup_c_amce_task5$tasknr <- "task5"


h1_sup_c_amce_task1 %>%
  bind_rows(h1_sup_c_amce_task2, h1_sup_c_amce_task3, h1_sup_c_amce_task4, h1_sup_c_amce_task5) %>% 
  mutate(feature = factor(feature, levels = c("FeatDlocalism_grownlocal", "FeatDlocalism_livelocal", "FeatBlocalism", "FeatSlocalism", "FeatGender", "FeatAge", "FeatOccupation", "FeatPartisanship",
                                              "SameGender", "SameAge", "SameOccupation", "SamePartisanship", "SameBlock"),
                          labels = c("Descriptive \n localism", "Descriptive \n localism", "Behavioral\n localism", "Symbolic\n localism", "Gender", "Age", "Occupation", "Party", "Correspondance", "Correspondance", "Correspondance", "Correspondance","Correspondance")),
         estimate = ifelse(is.na(std.error), NA, estimate),
         ref = ifelse(is.na(std.error) | is.na(estimate), "(ref)", NA),
         tasknr=factor(tasknr, levels = c("task1", "task2", "task3", "task4", "task5"), labels = c("Task: 1", "Task: 2", "Task: 3", "Task: 4", "Task: 5"))
  ) %>% 
  {.} -> h1_sup_c_amce_task 

ggplot(subset(h1_sup_c_amce_task), aes(x=estimate, y= level))+
  facet_grid(rows = vars(feature), cols = vars(tasknr),scales ="free" , space = "free")+
  geom_vline(aes(xintercept=0), colour="darkgrey", size=1) +
  geom_point()+
  geom_errorbarh(aes(xmax = upper, xmin = lower), height = 0)+
  geom_text(data=filter(h1_sup_c_amce_task), aes(x=0, y= level, label = ref ), size = 2.5)+
  scale_x_continuous(breaks = c(-0.20, 0.0, 0.20))+
  labs(x = "", y = "")+
  theme(axis.text = element_text(color = "black"),
        axis.line = element_blank(),
        legend.title = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.direction	="vertical",
        panel.grid.major = element_line(size = 0.01, linetype = 'solid', colour = "grey"), 
        panel.grid.minor = element_line(size = 0.01, linetype = 'solid', colour = "grey"),
        panel.border = element_rect(fill=NA, colour = "black"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),
        strip.text.y = element_text(angle = 0, size = 8))

ggsave("Fig1D.eps", plot = last_plot(),
       scale = 1, width = 10., height = 6, units = c("in"),
       dpi = 600)

##Test of No Profile-Order Effects####
#Test whether there is a difference in candidate evaluations depending on the whether the candidates is the seen first or last. Here an example of the test, with regard to H1 forced choice. Similiar tests will be conducted for other models:

h1_sup_c_amce_profilea <- amce(data = subset(main_data, profile == "a"), 
                               FeatSup ~ FeatDlocalism_grownlocal + FeatDlocalism_livelocal + FeatBlocalism + FeatSlocalism +
                                 #Controls
                                 FeatGender + FeatAge + FeatOccupation + FeatPartisanship +
                                 SameGender + SameAge + SameOccupation + SamePartisanship + SameBlock,
                               id = ~ id, 
                               weights = ~ weight
)

h1_sup_c_amce_profileb <- amce(data = subset(main_data, profile == "b"), 
                               FeatSup ~ FeatDlocalism_grownlocal + FeatDlocalism_livelocal + FeatBlocalism + FeatSlocalism +
                                 #Controls
                                 FeatGender + FeatAge + FeatOccupation + FeatPartisanship +
                                 SameGender + SameAge + SameOccupation + SamePartisanship + SameBlock,
                               id = ~ id, 
                               weights = ~ weight
)
h1_sup_c_amce_profilea$profile <- "Profile: A"
h1_sup_c_amce_profileb$profile <- "Profile: B"

h1_sup_c_amce_profilea %>%
  bind_rows(h1_sup_c_amce_profileb) %>% 
  mutate(feature = factor(feature, levels = c("FeatDlocalism_grownlocal" , "FeatDlocalism_livelocal", "FeatBlocalism", "FeatSlocalism", "FeatGender", "FeatAge", "FeatOccupation", "FeatPartisanship",
                                              "SameGender", "SameAge", "SameOccupation", "SamePartisanship", "SameBlock"),
                          labels = c("Descriptive \n localism", "Descriptive \n localism", "Behavioral\n localism", "Symbolic\n localism", "Gender", "Age", "Occupation", "Party", "Correspondance", "Correspondance", "Correspondance", "Correspondance","Correspondance")),
         estimate = ifelse(is.na(std.error), NA, estimate),
         ref = ifelse(is.na(std.error) | is.na(estimate), "(ref)", NA)) %>% 
  {.} -> h1_sup_c_amce_profile 


ggplot(subset(h1_sup_c_amce_profile), aes(x=estimate, y= level))+
  facet_grid(rows = vars(feature), cols = vars(profile),scales ="free" , space = "free")+
  geom_vline(aes(xintercept=0), colour="darkgrey", size=1) +
  geom_point()+
  geom_errorbarh(aes(xmax = upper, xmin = lower), height = 0)+
  geom_text(data=filter(h1_sup_c_amce_profile), aes(x=0, y= level, label = ref ), size = 2.5)+
  scale_x_continuous(breaks = c(-0.3, -0.20, -0.1, 0.0, 0.10, 0.2, 0.3))+
  labs(x = "", y = "")+
  theme(axis.text = element_text(color = "black"),
        axis.line = element_blank(),
        legend.title = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        legend.direction	="vertical",
        panel.grid.major = element_line(size = 0.01, linetype = 'solid', colour = "grey"), 
        panel.grid.minor = element_line(size = 0.01, linetype = 'solid', colour = "grey"),
        panel.border = element_rect(fill=NA, colour = "black"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_blank(),
        strip.text.y = element_text(angle = 0, size = 8))

ggsave("Fig2D.eps", plot = last_plot(),
       scale = 1, width = 10., height = 6, units = c("in"),
       dpi = 600)

#Appendix E: Priming of local attachment####
#I) Manipulation check
#visualization MC#
ms <- unique(select(main_data,id, local1, local2, local3, local, Prime))
ms %>% 
  group_by(Prime) %>%
  summarise(
    m_local = mean(local, na.rm = T),
    m_local1 = mean(local1, na.rm = T),
    m_local2 = mean(local2, na.rm = T),
    m_local3 = mean(local3, na.rm = T),
    lb_local = t.test(local)$conf.int[1],
    ub_local = t.test(local)$conf.int[2],
    
    lb_local1 = t.test(local1)$conf.int[1],
    ub_local1 = t.test(local1)$conf.int[2],
    
    lb_local2 = t.test(local2)$conf.int[1],
    ub_local2 = t.test(local2)$conf.int[2],
    
    
    lb_local3 = t.test(local3)$conf.int[1],
    ub_local3 = t.test(local3)$conf.int[2]
  ) %>% 
  gather("vars", "a",2:13) %>% 
  separate(vars, c("b", "value"), sep = "_") %>% 
  spread(b,a) %>% 
  mutate(Prime=ifelse(Prime==1, "Prime", "No prime"),
         value=ifelse(value=="local", "Indeks",
                      ifelse(value=="local1", "Typical of area",
                             ifelse(value=="local2", "Says \"we\"", "Feel attached")))
         
  ) %>% 
  {.} -> ms

ggplot(data = ms, aes(x=Prime))+
  geom_col(aes(y=m), fill=c("#ababab","#575757","#ababab","#575757","#ababab","#575757","#ababab","#575757"), colour = "#ffffff")+
  geom_errorbar(aes(ymin=lb, ymax=ub), width=0.4)+
  facet_grid(col = vars(factor(value, levels = c("Indeks", "Feel attached", "Typical of area", "Says \"we\""))))+
  ylim(0, 1)+
  theme_minimal(base_size = 11)+  
  labs(x = "", y = "")+
  theme(axis.text = element_text(color = "black"),
        axis.line = element_blank(),
        legend.position = "none",
        panel.border = element_rect(fill=NA),
        panel.background = element_blank(),
        strip.background = element_rect(colour = "black", fill = "white"),
        axis.title=element_text(size=12),
        axis.text.x = element_text(angle = -45, hjust = 0))

ggsave("Fig1E.eps", plot = last_plot(),
       scale = 1, width = 6., height = 3, units = c("in"),
       dpi = 600)


#Testing strength of instrument
#F-statistic should exceed 10
main_data$localH <- ifelse(main_data$local<mean(main_data$local, na.rm = T),0,
                       ifelse(main_data$local>mean(main_data$local, na.rm = T), 1,NA))

mc  <- lm_robust(local  ~ Prime, data = subset(main_data, contestnr == "conj1" & profile == "a"), weights = weight)
mc1 <- lm_robust(local1 ~ Prime, data = subset(main_data, contestnr == "conj1" & profile == "a"), weights = weight)
mc2 <- lm_robust(local2 ~ Prime, data = subset(main_data, contestnr == "conj1" & profile == "a"), weights = weight)
mc3 <- lm_robust(local3 ~ Prime, data = subset(main_data, contestnr == "conj1" & profile == "a"), weights = weight)
mc4 <- lm_robust(localH ~ Prime, data = subset(main_data, contestnr == "conj1" & profile == "a"), weights = weight)

summary(mc)$fstatistic
summary(mc1)$fstatistic
summary(mc2)$fstatistic
summary(mc3)$fstatistic
summary(mc4)$fstatistic
