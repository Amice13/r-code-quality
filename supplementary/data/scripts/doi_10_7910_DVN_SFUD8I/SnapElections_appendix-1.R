##Replication requires wrangling from file "SnapElections_REPfile.r"##



##Figure A1##
trust<- read_csv("meantrust.csv")
trust <-trust%>% 
  mutate(uk= as.factor(uk))
cbp2 <- c("#205C8A", "#FE6DB6")
FA1<- ggplot(trust,
                mapping = aes(x = direct_country, y = trustgov, color =uk)) +
  geom_jitter(show.legend = FALSE, size=4, alpha=.6, height=.01, width=.03) +
  scale_colour_manual(values=cbp2)+
  xlim (0, 1) +
  ylim (0, 1) +
  annotate(
    geom="text", x = .75, y = .22, size = 4, color = "#FE6DB6", fontface=2,
    label = "UK")+
  annotate(
    geom = "curve", x = .75, y = .20, xend = .46, yend = .43, 
    curvature = -.3, arrow = arrow(length = unit(2, "mm")), colour="#FE6DB6")+
  geom_vline(xintercept = .42, linetype="dashed", color="red2", size=1, alpha=.5) +
  geom_hline(yintercept = .45, linetype="dashed", color="red2", size=1, alpha=.5) +
  labs(x = 'Think country going in right direction', y = 'Trust the government',
       caption="Reference lines indicate mean y and x values among all countries") +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 10, face="bold"),
    plot.subtitle = element_text(hjust = 1, color = "blue", size = 10),
    axis.title = element_text(size=10))+
  theme_minimal()
ggsave("FigireA1.png", dpi=320)

##Figure A2##
modelEF<-lm (counts ~ treat*rile + sex + agecat + employ + urban + nutsID, data=dataframe)
summ(modelEF, robust=TRUE)
dataframe$predictEF<-predict(modelEF, dataframe)
treat <- subset(dataframe, treatnum== 2)
control <- subset(dataframe, treatnum== 1)

gg_EF <-
  modelEF %>%
  margins(at = list(rile = seq(1, 10, by = 1))) %>%
  summary %>%
  as.data.frame() %>%
  filter(factor == "treat1")

goodEF <-
  ggplot(dataframe, aes(rile, counts, shape = treat, group = treat, color=treat)) +
  scale_shape_manual(values = c(20, 17))+
  scale_color_manual(values = c("#205C8A", "#FE6DB6")) +
  scale_fill_manual(values = c("#205C8A", "#FE6DB6")) +
  theme_minimal()+
  stat_smooth(method = "lm_robust", fullrange = TRUE, se=FALSE) +
  geom_jitter(data=treat, aes(x=rile, y=predictEF, fill=treat, size=agecat), 
              alpha=.2, height=.04, width=1.2, shape=20,
              pch=21, color="#FE6DB6")+
  geom_jitter(data=control, aes(x=rile, y=predictEF, fill=treat,  size=agecat), 
              alpha=.2, height=.04, width=1.2, shape=17,
              pch=21, color="#205C8A")+
  theme(legend.position = "none")+
  xlim(0, 10)+
  ylab("Pr(Trusts Government)")+
  xlab("")+
  theme(axis.text.x =element_blank())+
  labs(subtitle="ITT effect moderated by ideological position")+
  annotate(
    geom="text", x = 4.5, y =.85, size = 3, color = "#FE6DB6", fontface=2,
    label = "Slope for \ntreated respondents")+
  annotate(
    geom = "curve", x = 5.2, y = .85, xend = 8, yend = .68, 
    curvature = -.2, arrow = arrow(length = unit(2, "mm")), colour="#FE6DB6")+
  annotate(
    geom="text", x = 7.5, y =.2, size = 3, color = "#205C8A", fontface=2,
    label = "Slope for \ncontrol respondents")+
  annotate(
    geom = "curve", x = 8, y = .2, xend = 9, yend = .45, 
    curvature = .2, arrow = arrow(length = unit(2, "mm")), colour="#205C8A")+
  theme(plot.subtitle = element_text(face="bold", color="#205C8A"))

ame1<- ggplot(gg_EF, aes(rile, AME)) +
  geom_point(colour="#FE6DB6") +
  coord_cartesian(xlim = c(1, 10), ylim = c(-.4, .6)) +
  geom_errorbar(aes(ymax = lower, ymin = upper), width = 0, colour="#FE6DB6") +
  geom_hline(yintercept = 0, linetype = "dashed", colour="#205C8A") +
  xlab("Left-right placement") +
  ylab("") +
  labs(title="ii) 'Voice counts in the UK'")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0, size = 14, face="bold"))

modelDa<- lm (direct_country ~ treat + sex + agecat + employ + urban + nutsID, data=rightonly)
modelDb<- lm (direct_country ~ treat + sex + agecat + employ + urban + nutsID, data=leftonly)
modelD<-lm (direct_country ~ treat*rile + sex + agecat + employ + urban + nutsID, data=dataframe)

gg_D <-
  modelD %>%
  margins(at = list(rile = seq(1, 10, by = 1))) %>%
  summary %>%
  as.data.frame() %>%
  filter(factor == "treat1")

ame2<- ggplot(gg_D, aes(rile, AME)) +
  geom_point(colour="#FE6DB6") +
  coord_cartesian(xlim = c(1, 10), ylim = c(-.4, .6)) +
  geom_errorbar(aes(ymax = lower, ymin = upper), width = 0, colour="#FE6DB6") +
  geom_hline(yintercept = 0, linetype = "dashed", colour="#205C8A") +
  xlab("Left-right placement") +
  ylab("") +
  labs(title="i) 'Country moving in right direction'")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0, size = 14, face="bold"))

p1<- plot_summs(modelDa, modelDb, scale = TRUE, plot.distributions = TRUE,
                coefs = c("Treatment effect" = "treat"),
                model.names = c("Right-wing voters", "Left-wing voters"), 
                ci_level = .9, colors = "Blues")+
  labs(subtitle="i) Country going in the right direction")+
  theme(legend.position = "bottom")+
  theme(axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        legend.title=element_blank(),
        legend.text=element_text(face="bold"),
        panel.border = element_rect(fill=NA),
        plot.subtitle = element_text(hjust = 0, size = 10, face="bold"))

modelEFa<- lm (counts ~ treat + sex + agecat + employ + urban + nutsID, data=rightonly)
modelEFb<- lm (counts ~ treat + sex + agecat + employ + urban + nutsID, data=leftonly)

p2 <- plot_summs(modelEFa, modelEFb, scale = TRUE, plot.distributions = TRUE,
                 coefs = c("Treatment effect" = "treat"),
                 model.names = c("Right-wing voters", "Left-wing voters"), 
                 ci_level = .9, colors = "Blues")+
  labs(subtitle="ii) My voice counts in the UK")+
  theme(legend.position = "bottom")+
  theme(axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        legend.title=element_blank(),
        legend.text=element_text(face="bold"),
        panel.border = element_rect(fill=NA),
        plot.subtitle = element_text(hjust = 0, size = 10, face="bold"))

(ame2+ame1)/(p1+p2)
ggsave("FigureA2.png", dpi=320)



##Figure A3##
notreatday<- subset(dataframe, run!=0)
model1<- lm (trustgov ~ treat + rile + sex + agecat + employ + urban + nutsID, data=dataframe)
model1x<- lm (trustgov ~ treat + rile + sex + agecat + employ + urban + nutsID, data=notreatday)
summ(model1)

plot_summs(model1,model1x,
           scale = TRUE, plot.distributions = TRUE, colors = "Blues",
           model.names = c("Main model", "Model exlcuding day 0"), 
           coefs = c("ATE" = "treat"), ci_level = .9)+
  labs(title = "Testing influence of day 0 respondents")+
  theme(axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        plot.subtitle = element_text(hjust = 0, size = 10, face="bold"),
        legend.position = c(0.32, 0.88),
        legend.title = element_blank(),
        panel.border = element_rect(fill=NA))

ggsave("FigureA3.png", dpi=320)

##Figure A4##
rds <- rdsensitivity(dataframe$trustgov, 
                     dataframe$run, 
                     cutoff= 0, 
                     wlist = seq(1, 6, by=1),
                     reps = 10000)

##Figure A5##
library(ritest)
ritest1<- ritest(model1, 'treat', reps = 2000)
Rplot1 <- ggplot(data.frame(betas = ritest1$betas), aes(betas)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.5, fill="#FE6DB6") +
  geom_vline(xintercept =  model1$coefficients[2], colour="blue2") +
  theme_minimal() +
  labs(y = "Density", x = "Coefficient", title = "Full sample")


ritest2<- ritest(model2a, 'treat', reps = 2000)
Rplot2 <- ggplot(data.frame(betas = ritest2$betas), aes(betas)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.5, fill="#FE6DB6") +
  geom_vline(xintercept =  model2a$coefficients[2], colour="blue2") +
  theme_minimal() +
  labs(y = "Density", x = "Coefficient", title = "Right-wing only")


ritest3<- ritest(model2b, 'treat', reps = 2000)
Rplot3 <- ggplot(data.frame(betas = ritest2$betas), aes(betas)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.5, fill="#FE6DB6") +
  geom_vline(xintercept =  model2b$coefficients[2], colour="blue2") +
  theme_minimal() +
  labs(y = "Density", x = "Coefficient", title = "Left-wing only")+
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

Rplot1/(Rplot2+Rplot3)+ 
  plot_annotation(
    title = "Randomisation inference using 2000 permutations") &
  theme(plot.title = element_text(face=2))

ggsave("FigureA5.png", dpi=320)


##Figure A6##
modelI<- lm (trustgov ~ treat*interest + rile + sex + agecat + employ + urban + nutsID, data=dataframe)
summ(modelI)


ameI <-
  modelI %>%
  margins(at = list(interest = c("0", "1"))) %>%
  summary %>%
  as.data.frame() %>%
  filter(factor == "treat1")

ameIplot<- ggplot(ameI, aes(interest, AME)) +
  geom_point(colour="#FE6DB6") +
  geom_errorbar(aes(ymax = lower, ymin = upper), width = 0, colour="#FE6DB6") +
  geom_hline(yintercept = 0, linetype = "dashed", colour="#205C8A") +
  xlab("") +
  ylab("") +
  scale_x_discrete(labels=c("0" = "Not politically interested", "1" = "Politically interested"))+
  labs(title="Treatment effects (un)conditioned by level of political interest")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0, size = 14, face="bold"))

ggsave("FigureA6.png", dpi=320)




##Multiverse analysis##

df <-dataframe%>% 
  mutate(Sex= as.factor(sex),
         LeftRight= as.numeric(rile),
         Treatment= as.numeric(treatnum),
         Age= as.factor(agecat),
         Employment= as.factor(employ),
         Urbanicity= as.factor(urban),
         RegionalFE= as.factor(nutsID),
         Trust_Gov =as.numeric(trustgov),
         Trust_Media =as.numeric(trustmedia),
         VoiceCounts_UK =as.numeric(counts),
         VoiceCounts_EU =as.numeric(countsEU),
         Trust_LegalSys =as.numeric(trustlegal),
         LifeSatisfaction =as.numeric(lifesat),
         DirectionGoing_UK =as.numeric(direct_country),
         DirectionGoing_EU =as.numeric(direct_EU))


setup_specs(y = c("Trust_Gov", "VoiceCounts_UK", "VoiceCounts_EU", "DirectionGoing_UK", "DirectionGoing_EU", "LifeSatisfaction", "Trust_LegalSys", "Trust_Media"),              
            x = c("Treatment"),         
            model = c("lm", "glm"),           
            controls = c("LeftRight", "Sex", "Age", "Employment", "Urbanicity", "RegionalFE"))

results <- run_specs(df = df, 
                     y = c("Trust_Gov", "VoiceCounts_UK", "VoiceCounts_EU", "DirectionGoing_UK", "DirectionGoing_EU", "LifeSatisfaction", "Trust_LegalSys", "Trust_Media"),              
                     x = c("Treatment"), 
                     model = c("lm", "glm"),    
                     controls = c("LeftRight", "Sex", "Age", "Employment", "Urbanicity", "RegionalFE"),
                     conf.level = .90)

results2 <- run_specs(df = df, 
                      y = c("Trust_Gov"),              
                      x = c("Treatment"), 
                      model = c("lm", "glm"),    
                      controls = c("LeftRight", "Sex", "Age", "Employment", "Urbanicity", "RegionalFE"),
                      conf.level = .90)

results3 <- run_specs(df = df, 
                      y = c("VoiceCounts_UK", "VoiceCounts_EU", "DirectionGoing_UK", "DirectionGoing_EU", "LifeSatisfaction", "Trust_LegalSys", "Trust_Media"),              
                      x = c("Treatment"), 
                      model = c("lm", "glm"),    
                      controls = c("LeftRight", "Sex", "Age", "Employment", "Urbanicity", "RegionalFE"),
                      conf.level = .90)

summarise_specs(results)

plot_decisiontree(results, 
                  legend = TRUE)

p1<- plot_curve(results) +
  geom_hline(yintercept = median(results2$estimate),
             linetype = "dashed", color="steelblue4")+
  geom_hline(yintercept = median(results3$estimate),
             linetype = "dashed", color="red2")+
  theme_minimal()+
  labs(y="", x="")+
  annotate(
    geom="text", x = 50, y = .09, size = 4, color = "steelblue4", fontface=2,
    label = "Median ITT for Trust in Gov.")+
  annotate(
    geom="text", x = 100, y = -.035, size = 4, color = "red2", fontface=2,
    label = "Median ITT for placebos")

p2 <- plot_choices(results) +
  labs(x = "") +
  theme_minimal()+  
  theme(strip.text.x = element_blank())

p3 <- plot_samplesizes(results) +
  theme_minimal()


plot1 <- ggarrange(p1, p2, ncol=1, nrow=2,align = "h") 

plot2<- annotate_figure(plot1,
                        top = text_grob("Multiverse analysis specification curve\n", 
                                        size=14, hjust=.9, face=2))
ggsave("FigureA7.png",width =21, height = 29, units = "cm", dpi=320)


