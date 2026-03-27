########################################
#### ONLINE APPENDIX               #
########################################


#This code replicates the figures and the tables included in the online appendix 

#INCLUDE YOUR PATH HERE
rm(list=ls())


library("rio")
library("cjoint")
library("ggthemes")
library("plyr")
library("dplyr")
library("stargazer")
library("cregg")
library("openxlsx")
library("ggplot2")

#Open the dataset
conj <- rio::import("conjoint-data-stacked.rds")

#Reorder names to easy plotting
names(conj)[names(conj)=="wealth"] <- "awealth"
names(conj)[names(conj)=="wealthiest"] <- "bwealthiest"
names(conj)[names(conj)=="poorest"] <- "cpoorest"
names(conj)[names(conj)=="mobility"] <- "dmobility"
names(conj)[names(conj)=="origin"] <- "eorigin"

#labels
names_at <- c("The country's wealth...", 
              "The wealthiest...", 
              "The poorest...", 
              "Social mobility",
              "People's wealth would still come from...")

#format graph
text.size=11
text.color = "black"
theme_bw1 <- function(base_size = text.size, base_family = "") {
  theme_classic(base_size = base_size, base_family = base_family) %+replace%
    theme(axis.text.x = element_text(size = base_size*.9, colour = text.color, 
                                     hjust = .5 , vjust=1),
          axis.text.y = element_text(size = base_size, 
                                     colour = text.color, hjust = 0 , vjust=.5 ), 
          axis.ticks = element_blank(),
          axis.title.y =  element_text(size = base_size,angle=90,vjust=.01,hjust=.1),
          plot.title = element_text(face = "bold"),legend.position = "none")}


# FIGURE A2 -------------------------------------------------------

#Survey completion time (in minutes)
time_df <- read_xlsx("ses_data.xlsx", sheet="data")

time_df$time <- time_df$time_taken/60

time_df_short <- subset(time_df, time<50)
time_df_short <- subset(time_df_short, time>5)

ggplot(time_df_short, aes(time)) +
  geom_histogram(binwidth=0.5, aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Percent", x = "")  +
  theme_pander() 

# TABLE A1 -------------------------------------------------------

#Descriptive statistics - main sociodemographic indicators
prop.table(table(conj$sex))
summary(conj$age)
prop.table(table(conj$education))
prop.table(table(conj$ethnicity))
prop.table(table(conj$PID))
prop.table(table(conj$income2))


# FIGURE A3 -------------------------------------------------------

#Respondents' perceived fairness towards different redistributive policies


ggplot(conj, aes(fair)) +
  geom_histogram(binwidth=0.5, aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Percent", x = "")  +
  scale_x_discrete(limit = c(1:11) ,
                   labels = c("Very Unfair", "1", "2",
                              "3", "4", "5", "6", "7", "8", "9", "Very Fair")) +
  theme_pander() 


# FIGURE A4 -------------------------------------------------------

#Frequencies of conjoint features
plot(cj_freqs(conj, ~ awealth + eorigin  + bwealthiest + cpoorest +
                dmobility, id = ~ ID,
              feature_labels = list(awealth = "The country's wealth...",
                                    eorigin = "People's wealth would still come from...",
                                    bwealthiest = "The wealthiest...",
                                    cpoorest = "The poorest...", 
                                    dmobility = "Social mobility")), legend_pos = "none")

# TABLE A2 -------------------------------------------------------

#Perceived fairness of policies (with and without controls)
detach("package:cregg", unload=TRUE)
m_fair <- amce(fair ~ awealth + eorigin  + bwealthiest + cpoorest +
                 dmobility, 
               data = conj,
               respondent.id = "ID")

m_fair_controls <- amce(fair ~ awealth + eorigin  + bwealthiest + cpoorest +
                          dmobility + age_cat + education + sex + ethnicity, 
                        data = conj,
                        respondent.id = "ID")
summary(m_fair)
summary(m_fair_controls)

#stargazer(m_fair, m_fair_controls,
#          column.labels=c("default"), align=TRUE,
#          keep.stat = c("n","rsq"),
#          no.space=TRUE, 
#          title="Perceived fairness of policies (with and without controls)")

# FIGURE A5 -------------------------------------------------------

#The effect of different policies on the fairness of redistribution across ideological positions (alternative operationalisation)
conj$Ideology <-conj$ideol_cat 

library("cregg")
mm_byideol <- cj(conj, fair ~ awealth + eorigin  + bwealthiest + cpoorest +
                   dmobility, id = ~ID, estimate = "mm", by = ~Ideology,
                 feature_labels = list(awealth = "The country's wealth...",
                                       eorigin = "People's wealth would still come from...",
                                       bwealthiest = "The wealthiest...",
                                       cpoorest = "The poorest...", 
                                       dmobility = "Social mobility"))


plot(mm_byideol, group = "Ideology") +
  geom_vline(xintercept = mean(conj$fair), linetype="dashed", size=0.3, colour="grey") +
  scale_color_manual(values=c("red", "blue", "orange")) 


# FIGURE A6 -------------------------------------------------------

#The effect of different policies on the fairness of redistribution across household income groups

conj$h_income3 <- NA
conj$h_income3[conj$h_income2=="Less than £10,000"] <- 1
conj$h_income3[conj$h_income2=="£10,000 - £19,999"] <- 1
conj$h_income3[conj$h_income2=="£100,000 - £149,999"] <- 0
conj$h_income3[conj$h_income2=="More than £150,000"] <- 0


conj$h_income3 <- as.factor(conj$h_income3)
levels(conj$h_income3)[levels(conj$h_income3)==0] <- "High"
levels(conj$h_income3)[levels(conj$h_income3)==1] <- "Low"

conj_h_income <- conj[!(is.na(conj$h_income3)), ]

conj_h_income$H.Income <- conj_h_income$h_income3

mm_byhincome <- cj(conj_h_income, fair ~ awealth + eorigin  + bwealthiest + cpoorest +
                     dmobility, id = ~ID, estimate = "mm", by = ~H.Income,
                   feature_labels = list(awealth = "The country's wealth...",
                                         eorigin = "People's wealth would still come from...",
                                         bwealthiest = "The wealthiest...",
                                         cpoorest = "The poorest...", 
                                         dmobility = "Social mobility"))

plot(mm_byhincome, group = "H.Income") +
  geom_vline(xintercept = mean(conj$fair), linetype="dashed", size=0.3, colour="grey") 


# FIGURE A7 -------------------------------------------------------

#The effect of different policies on the fairness of redistribution (weighted)
detach("package:cregg", unload=TRUE)

conj$weights <- 0
conj$weights[conj$ideol_cat=="Left"] <- 0.278
conj$weights[conj$ideol_cat=="Centre"] <- 0.3449
conj$weights[conj$ideol_cat=="Right"] <- 0.3770

library("cjoint")
m_fair_weighted <- amce(fair ~ awealth + eorigin  + bwealthiest + cpoorest +
                          dmobility, 
                        data = conj,
                        weights = "weights",
                        respondent.id = "ID")

x <- plot(m_fair_weighted, 
          plot.theme =  theme_bw1()  ,  
          attribute.names = names_at)

# FIGURE A8 -------------------------------------------------------

#Balance testing using respondents' ideology
library("cregg")
#Balance testing
mm_testing <- mm(conj, self_ideol_1 ~ awealth + eorigin  + bwealthiest + cpoorest +
                   dmobility, id = ~ID, 
                 feature_labels = list(awealth = "The country's wealth...",
                                       eorigin = "People's wealth would still come from...",
                                       bwealthiest = "The wealthiest...",
                                       cpoorest = "The poorest...", 
                                       dmobility = "Social mobility")) 
plot(mm_testing) +
  geom_vline(xintercept = mean(conj$self_ideol_1), linetype="dashed", size=0.3, colour="grey") +
  theme(legend.position = "none")

# FIGURE A9 -------------------------------------------------------

#Carryover diagnostic
f1 <- fair ~ awealth + eorigin  + bwealthiest + cpoorest +
  dmobility

carry_df <- cj(conj, f1, id = ~ID, by = ~pair, estimate = "mm", group = "pair",    
               feature_labels = list(awealth = "The country's wealth...",
                                     eorigin = "People's wealth would still come from...",
                                     bwealthiest = "The wealthiest...",
                                     cpoorest = "The poorest...", 
                                     dmobility = "Social mobility"))

carry_df$Round <- carry_df$pair

plot(carry_df, group = "Round") 

# FIGURE A10 -------------------------------------------------------

#Differences in Conditional Marginal Means, by Party Identification (Democrats and Republicans)
df_pid <- subset(conj, PID =="Democrat" | PID=="Republican")
df_pid[] <- lapply(df_pid, function(x) if(is.factor(x)) factor(x) else x)


mm_diff_plot <- mm_diffs(df_pid, fair ~ awealth + eorigin  + bwealthiest + cpoorest +
                           dmobility, ~ PID, id = ~ ID)
plot(mm_diff_plot)


# FIGURE A11 -------------------------------------------------------

#Differences in Conditional Marginal Means, by income group
diff_mms_income <- cj(conj, fair ~ awealth + eorigin  + bwealthiest + cpoorest +
                        dmobility, id = ~ID, estimate = "mm_diff", by = ~Income_group,
                      feature_labels = list(awealth = "The country's wealth...",
                                            eorigin = "People's wealth would still come from...",
                                            bwealthiest = "The wealthiest...",
                                            cpoorest = "The poorest...", 
                                            dmobility = "Social mobility"))

plot(diff_mms_income) + ggplot2::theme(legend.position="none")

# FIGURE A12 -------------------------------------------------------

#AMCE across alternative reference categories
awealthdf <- amce_by_reference(conj, fair ~ awealth + eorigin  + bwealthiest + cpoorest +
                                 dmobility  ,variable =  ~ awealth, id = ~ ID)
awealthdf <- awealthdf[which(awealthdf$feature=="awealth"),]
awealthdf$feature <- c("The country's wealth...")
awealth_plot <- plot(awealthdf, group = "BY")

eorigindf <- amce_by_reference(conj, fair ~ awealth + eorigin  + bwealthiest + cpoorest +
                                 dmobility  ,variable =  ~ eorigin, id = ~ ID)
eorigindf <- eorigindf[which(eorigindf$feature=="eorigin"),]
eorigindf$feature <- c("People's wealth would still come from...")
eorigin_plot <- plot(eorigindf, group = "BY")

bwealthiestdf <- amce_by_reference(conj, fair ~ awealth + eorigin  + bwealthiest + cpoorest +
                                     dmobility  ,variable =  ~ bwealthiest, id = ~ ID)
bwealthiestdf <- bwealthiestdf[which(bwealthiestdf$feature=="bwealthiest"),]
bwealthiestdf$feature <- c("The wealthiest...")
bwealthiest_plot <- plot(bwealthiestdf, group = "BY")

cpoorestdf <- amce_by_reference(conj, fair ~ awealth + eorigin  + bwealthiest + cpoorest +
                                  dmobility  ,variable =  ~ cpoorest, id = ~ ID)
cpoorestdf <- cpoorestdf[which(cpoorestdf$feature=="cpoorest"),]
cpoorestdf$feature <- c("The poorest...")
cpoorest_plot <- plot(cpoorestdf, group = "BY")

dmobilitydf <- amce_by_reference(conj, fair ~ awealth + eorigin  + bwealthiest + cpoorest +
                                   dmobility  ,variable =  ~ dmobility, id = ~ ID)
dmobilitydf <- dmobilitydf[which(dmobilitydf$feature=="dmobility"),]
dmobilitydf$feature <- c("Social mobility")
dmobilitydf_plot <- plot(dmobilitydf, group = "BY")


library("cowplot")
plot_grid(awealth_plot,bwealthiest_plot,cpoorest_plot,eorigin_plot,dmobilitydf_plot, ncol=1)







