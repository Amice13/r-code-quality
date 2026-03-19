load("observational_data.RData")

library(xtable); library(ggplot2); library(tidyverse)

###########
# Table 2 # 
###########
# Overall
overall.plain <- survey %>%
  summarize(weighted.mean(QPHILO3_1 %in% c("Very important", "Somewhat important"), w = Propwts)) #plain meaning to modern reader
overall.intent <- survey %>%
  summarize(weighted.mean(QPHILO3_2 %in% c("Very important", "Somewhat important"), w = Propwts)) #intent of framers
overall.adopted <- survey %>%
  summarize(weighted.mean(QPHILO3_3 %in% c("Very important", "Somewhat important"), w = Propwts)) #what most thought when adopted
overall.precedent <- survey %>%
  summarize(weighted.mean(QPHILO3_4 %in% c("Very important", "Somewhat important"), w = Propwts)) #stare decisis (previous SC decisions)
overall.strong <- survey %>%
  summarize(weighted.mean(QPHILO3_5 %in% c("Very important", "Somewhat important"), w = Propwts)) #strong reason, e.g. national security
overall.political <- survey %>%
  summarize(weighted.mean(QPHILO3_6 %in% c("Very important", "Somewhat important"), w = Propwts)) #political activity
overall.state <- survey %>%
  summarize(weighted.mean(QPHILO3_7 %in% c("Very important", "Somewhat important"), w = Propwts)) #state or federal
overall.consequences <- survey %>%
  summarize(weighted.mean(QPHILO3_8 %in% c("Very important", "Somewhat important"), w = Propwts)) #consequences
overall.countries <- survey %>% 
  summarize(weighted.mean(QPHILO3_9 %in% c("Very important", "Somewhat important"), w = Propwts)) #other countries
overall.public <- survey %>%
  summarize(weighted.mean(QPHILO3_10 %in% c("Very important", "Somewhat important"), w = Propwts)) #public opinion

# By ideology
ideology.plain <- survey %>%
  group_by(ideo3) %>%
  summarize(weighted.mean(QPHILO3_1 %in% c("Very important", "Somewhat important"), w = Propwts)) #plain meaning to modern reader
ideology.intent <- survey %>%
  group_by(ideo3) %>%
  summarize(weighted.mean(QPHILO3_2 %in% c("Very important", "Somewhat important"), w = Propwts)) #intent of framers
ideology.adopted <- survey %>%
  group_by(ideo3) %>%
  summarize(weighted.mean(QPHILO3_3 %in% c("Very important", "Somewhat important"), w = Propwts)) #what most thought when adopted
ideology.precedent <- survey %>%
  group_by(ideo3) %>%
  summarize(weighted.mean(QPHILO3_4 %in% c("Very important", "Somewhat important"), w = Propwts)) #stare decisis (previous SC decisions)
ideology.strong <- survey %>%
  group_by(ideo3) %>%
  summarize(weighted.mean(QPHILO3_5 %in% c("Very important", "Somewhat important"), w = Propwts)) #strong reason, e.g. national security
ideology.political <- survey %>%
  group_by(ideo3) %>%
  summarize(weighted.mean(QPHILO3_6 %in% c("Very important", "Somewhat important"), w = Propwts)) #political activity
ideology.state <- survey %>%
  group_by(ideo3) %>%
  summarize(weighted.mean(QPHILO3_7 %in% c("Very important", "Somewhat important"), w = Propwts)) #state or federal
ideology.consequences <- survey %>%
  group_by(ideo3) %>%
  summarize(weighted.mean(QPHILO3_8 %in% c("Very important", "Somewhat important"), w = Propwts)) #consequences
ideology.countries <- survey %>% 
  group_by(ideo3) %>%
  summarize(weighted.mean(QPHILO3_9 %in% c("Very important", "Somewhat important"), w = Propwts)) #other countries
ideology.public <- survey %>%
  group_by(ideo3) %>%
  summarize(weighted.mean(QPHILO3_10 %in% c("Very important", "Somewhat important"), w = Propwts)) #public opinion

t2.plain.meaning <- c(overall.plain$`weighted.mean(...)`, ideology.plain$`weighted.mean(...)`[c(2,3,1)])
t2.original.intent <- c(overall.intent$`weighted.mean(...)`, ideology.intent$`weighted.mean(...)`[c(2,3,1)])
t2.opinion.when.adopted <- c(overall.adopted$`weighted.mean(...)`, ideology.adopted$`weighted.mean(...)`[c(2,3,1)])
t2.precedent <- c(overall.precedent$`weighted.mean(...)`, ideology.precedent$`weighted.mean(...)`[c(2,3,1)])
t2.average.traditional <- apply(rbind(t2.plain.meaning, t2.original.intent, t2.opinion.when.adopted, t2.precedent), 2, mean)

t2.plain.meaning <- paste(as.character(round(t2.plain.meaning*100,0)),"%",sep="")
t2.original.intent <- paste(as.character(round(t2.original.intent*100,0)),"%",sep="")
t2.opinion.when.adopted <- paste(as.character(round(t2.opinion.when.adopted*100,0)),"%",sep="")
t2.precedent <- paste(as.character(round(t2.precedent*100,0)),"%",sep="")
t2.average.traditional <- paste(as.character(round(t2.average.traditional*100,0)),"%",sep="") 

t2.consequences <- c(overall.consequences$`weighted.mean(...)`, ideology.consequences$`weighted.mean(...)`[c(2,3,1)])
t2.other.countries <- c(overall.countries$`weighted.mean(...)`, ideology.countries$`weighted.mean(...)`[c(2,3,1)])
t2.public.opinion <- c(overall.public$`weighted.mean(...)`, ideology.public$`weighted.mean(...)`[c(2,3,1)])
t2.average.non.traditional <- apply(rbind(t2.consequences, t2.other.countries, t2.public.opinion), 2, mean)

t2.consequences <- paste(as.character(round(t2.consequences*100,0)),"%",sep="")
t2.other.countries <- paste(as.character(round(t2.other.countries*100,0)),"%",sep="")
t2.public.opinion <- paste(as.character(round(t2.public.opinion*100,0)),"%",sep="")
t2.average.non.traditional <- paste(as.character(round(t2.average.non.traditional*100,0)),"%",sep="") 

t2.strong.reason <- c(overall.strong$`weighted.mean(...)`, ideology.strong$`weighted.mean(...)`[c(2,3,1)])
t2.political.activity <- c(overall.political$`weighted.mean(...)`, ideology.political$`weighted.mean(...)`[c(2,3,1)])
t2.state.federal <- c(overall.state$`weighted.mean(...)`, ideology.state$`weighted.mean(...)`[c(2,3,1)])

t2.strong.reason <- paste(as.character(round(t2.strong.reason*100,0)),"%",sep="")
t2.political.activity <- paste(as.character(round(t2.political.activity*100,0)),"%",sep="")
t2.state.federal <- paste(as.character(round(t2.state.federal*100,0)),"%",sep="")


t2.dataframe <- data.frame(rbind(t2.average.traditional, t2.plain.meaning, t2.original.intent, t2.opinion.when.adopted, t2.precedent,
                                 t2.average.non.traditional, t2.consequences, t2.other.countries, t2.public.opinion,
                                 t2.strong.reason, t2.political.activity, t2.state.federal),
                                    row.names = c("Traditional principles","Plain meaning","Original intent","Opinion when adopted","Precedent",
                                                  "Non-traditional principles","Consequences","Other countries","Public opinion",
                                                  "Strong reason","Political activity","State or federal"))
colnames(t2.dataframe) <- c("All","Liberal","Moderate","Conservative")

xtable(t2.dataframe)

############
# Figure 1 # 
############
# Creates a new "long" dataset with an observation for each of the 10 principles questions for each respondent
survey_long <- survey %>% gather(question,principles,principles1:principles10)

survey_long$question[survey_long$question=="principles1"] <- "Plain Meaning (T)"
survey_long$question[survey_long$question=="principles2"] <- "Original Intent (T)"
survey_long$question[survey_long$question=="principles3"] <- "Opinion when Adopted (T)"
survey_long$question[survey_long$question=="principles4"] <- "Precedent (T)"
survey_long$question[survey_long$question=="principles5"] <- "Strong Reason (Other)"
survey_long$question[survey_long$question=="principles6"] <- "Political Activity (Other)"
survey_long$question[survey_long$question=="principles7"] <- "State or Federal (Other)"
survey_long$question[survey_long$question=="principles8"] <- "Consequences (NT)"
survey_long$question[survey_long$question=="principles9"] <- "Other Countries (NT)"
survey_long$question[survey_long$question=="principles10"] <- "Public Opinion (NT)"

# Create index of first four principles (traditional) and last three principles (non-traditional)
trad <- survey %>%
  mutate(Support = (principles1 + principles2 + principles3 + principles4) / 4,
         Group = "Traditional Principles")

non_trad <- survey %>%
  mutate(Support = (principles8 + principles9 + principles10) / 3,
         Group = "Non-Traditional Principles")

# Creating the figure -- plots the combined traditional and non-traditional results first, then the individual principles
pdf("figure-1.pdf", width=9, height=8)
survey_long %>%
  rename(Support = principles) %>% # Renames principles evaluation variable to Support
  bind_rows(rename(trad, question = Group)) %>%
  bind_rows(rename(non_trad, question = Group)) %>%
  mutate(Support = (Support - 1) / 3, # Makes support 0-1
         ideo3 = factor(ideo3, levels = c("Conservative", "Moderate", "Liberal")),
         question = factor(question, levels = c("Traditional Principles",
                                                "Non-Traditional Principles",
                                                "Plain Meaning (T)",
                                                "Original Intent (T)",
                                                "Opinion when Adopted (T)",
                                                "Precedent (T)",
                                                "Consequences (NT)",
                                                "Other Countries (NT)",
                                                "Public Opinion (NT)",
                                                "Strong Reason (Other)",
                                                "Political Activity (Other)",
                                                "State or Federal (Other)"))) %>% 
  filter(ideo3 != "Moderate") %>% # Omits moderate
  ggplot(aes(y=Support,x=knowledge,color=ideo3,linetype = ideo3,weight=Propwts)) + # Defining what will be plotted: support as a function of knowledge and ideology, defining weights to be used
  geom_smooth(method="lm") + # Plotting linear fits, inherits the aesthetic mappings from aes above
  ylab("Average Support for Principle") +
  xlab("Court Knowledge") +
  facet_wrap(~question) + # Different panels for each principle or grouping of principles
  guides(color=guide_legend(title="Ideology"),
         linetype = guide_legend(title = "Ideology")) + theme_bw() +
  scale_color_manual(values=c("Red", "Blue")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
dev.off()
