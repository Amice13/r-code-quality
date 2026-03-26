################################################ Appendix Tables and Figures ###########################################################

####################### Table (1): Summary Statistics (Full Sample)
subdata <- subset(data, select=c("age", "edu", "female", "white", "Rep", "media_score", "article_accuracy", "poltrust_score", 
                                 "account_score", "google", "social_media", "gov_tanks", "nongov_tanks", "liberal_media", 
                                 "consv_media", "alt_media", "academic", "fbi", "cia", "rep_lead", "dem_lead", 
                                 "courts", "sup_courts", "investigation", "performance", "vote", "no_impeach"))


stargazer(as.data.frame(subdata), type="latex", title="Descriptive Statistics of the Full Sample", summary=TRUE, 
          median=T, iqr = FALSE, 
          covariate.labels = c("Age", "Education", "Female", "White", "Republican", 
                               "Trust in Information Providers", "Article Accuracy", "Trust in Pol. Institutions", 
                               "Political Support", "Trust in Search Engines", "Trust in Social Media", "Trust in Gov. Think Tanks", 
                               "Trust in Non-gov. Think Tanks", "Trust in Liberal Media", "Trust in Converv. Media", 
                               "Trust in Alternative Media", "Trust in Academic Inst.", "Trust in FBI", "Trust in CIA", 
                               "Trust in Republican Leaders", "Trust in Democratic Leaders", "Trust in Courts", 
                               "Trust in Supreme Court", "Investigation", "Performance", "Vote", "Impeachment"), 
          omit.summary.stat = c("p25", "p75"), digits=1)


######################### Table (2): Summary Statistics (Achievement Condition)
data_achievement <- subset(data, data$scandal==0)

subdata <- subset(data_achievement, select=c("age", "edu", "female", "white", "Rep", "media_score", "article_accuracy", "poltrust_score", 
                                             "account_score", "google", "social_media", "gov_tanks", "nongov_tanks", "liberal_media", 
                                             "consv_media", "alt_media", "academic", "fbi", "cia", "rep_lead", "dem_lead", 
                                             "courts", "sup_courts", "investigation", "performance", "vote", "no_impeach"))


stargazer(as.data.frame(subdata), type="latex", title="Descriptive Statistics of the Achievement Condition", summary=TRUE, 
          median=T, iqr = FALSE, 
          covariate.labels = c("Age", "Education", "Female", "White", "Republican", 
                               "Trust in Information Providers", "Article Accuracy", "Trust in Pol. Institutions", 
                               "Political Support", "Trust in Search Engines", "Trust in Social Media", "Trust in Gov. Think Tanks", 
                               "Trust in Non-gov. Think Tanks", "Trust in Liberal Media", "Trust in Converv. Media", 
                               "Trust in Alternative Media", "Trust in Academic Inst.", "Trust in FBI", "Trust in CIA", 
                               "Trust in Republican Leaders", "Trust in Democratic Leaders", "Trust in Courts", 
                               "Trust in Supreme Court", "Investigation", "Performance", "Vote", "Impeachment"), 
          omit.summary.stat = c("p25", "p75"), digits=1)


########################### Table (3): Summary Statistics (Scandal Condition)

data_scandal <- subset(data, data$scandal==1)
subdata <- subset(data_scandal, select=c("age", "edu", "female", "white", "Rep", "media_score", "article_accuracy", "poltrust_score", 
                                         "account_score", "google", "social_media", "gov_tanks", "nongov_tanks", "liberal_media", 
                                         "consv_media", "alt_media", "academic", "fbi", "cia", "rep_lead", "dem_lead", 
                                         "courts", "sup_courts", "investigation", "performance", "vote", "no_impeach"))


stargazer(as.data.frame(subdata), type="latex", title="Descriptive Statistics of the Scandal Condition", summary=TRUE, 
          median=T, iqr = FALSE, 
          covariate.labels = c("Age", "Education", "Female", "White", "Republican", 
                               "Trust in Information Providers", "Article Accuracy", "Trust in Pol. Institutions", 
                               "Political Support", "Trust in Search Engines", "Trust in Social Media", "Trust in Gov. Think Tanks", 
                               "Trust in Non-gov. Think Tanks", "Trust in Liberal Media", "Trust in Converv. Media", 
                               "Trust in Alternative Media", "Trust in Academic Inst.", "Trust in FBI", "Trust in CIA", 
                               "Trust in Republican Leaders", "Trust in Democratic Leaders", "Trust in Courts", 
                               "Trust in Supreme Court", "Investigation", "Performance", "Vote", "Impeachment"), 
          omit.summary.stat = c("p25", "p75"), digits=1)



########################### Table (5): OLS Regression Estimates of the Effects of Conspiracy and Scandal on Political Support
support <- lm(account_score ~ conspiracy+scandal, data = data)
support1 <- lm(account_score ~ conspiracy+scandal + scandal*conspiracy, data = data)

stargazer(support, support1, align=TRUE,  no.space=TRUE, 
          covariate.labels = c("Conspiracy", "Scandal", "Scandal x Conspiracy", "Control"), 
          keep.stat="n", dep.var.caption="Outcome", digits=3, column.sep.width = "-35pt", header=F)


########################## Figure (1): Treatment Effects on Trust in Information Providers. 
#Full Sample Models
m3 <- lm(google ~ conspiracy + scandal, data = data)
m4 <- lm(social_media ~ conspiracy + scandal, data = data)
m5 <- lm(liberal_media ~ conspiracy + scandal, data = data)
m6 <- lm(consv_media ~ conspiracy + scandal, data = data)
m7 <- lm(gov_tanks ~ conspiracy + scandal, data = data)
m8 <- lm(nongov_tanks ~ conspiracy + scandal, data = data)
m9 <- lm(alt_media ~ conspiracy + scandal, data = data)
m10 <- lm(academic ~ conspiracy + scandal, data = data)

#Scandal Condition Models
m3a <- lm(google ~ conspiracy , data = data_scandal)
m4a <- lm(social_media ~ conspiracy, data = data_scandal)
m5a <- lm(liberal_media ~ conspiracy, data = data_scandal)
m6a <- lm(consv_media ~ conspiracy , data = data_scandal)
m7a <- lm(gov_tanks ~ conspiracy , data = data_scandal)
m8a <- lm(nongov_tanks ~ conspiracy , data = data_scandal)
m9a <- lm(alt_media ~ conspiracy , data = data_scandal)
m10a <- lm(academic ~ conspiracy , data = data_scandal)

#Achievement Condition Models
m3b <- lm(google ~ conspiracy , data = data_achievement)
m4b <- lm(social_media ~ conspiracy , data = data_achievement)
m5b <- lm(liberal_media ~ conspiracy, data = data_achievement)
m6b <- lm(consv_media ~ conspiracy, data = data_achievement)
m7b <- lm(gov_tanks ~ conspiracy, data = data_achievement)
m8b <- lm(nongov_tanks ~ conspiracy, data = data_achievement)
m9b <- lm(alt_media ~ conspiracy, data = data_achievement)
m10b <- lm(academic ~ conspiracy, data = data_achievement)

#Exract coefficients and SEs to plot
est_m3<- summary(m3)$coefficients[2,1]
est_m4<- summary(m4)$coefficients[2,1]
est_m5<- summary(m5)$coefficients[2,1]
est_m6<- summary(m6)$coefficients[2,1]
est_m7<- summary(m7)$coefficients[2,1]
est_m8<- summary(m8)$coefficients[2,1]
est_m9<- summary(m9)$coefficients[2,1]
est_m10<- summary(m10)$coefficients[2,1]


se_m3 <- summary(m3)$coefficients[2,2]
se_m4 <- summary(m4)$coefficients[2,2]
se_m5 <- summary(m5)$coefficients[2,2]
se_m6 <- summary(m6)$coefficients[2,2]
se_m7 <- summary(m7)$coefficients[2,2]
se_m8 <- summary(m8)$coefficients[2,2]
se_m9 <- summary(m9)$coefficients[2,2]
se_m10 <- summary(m10)$coefficients[2,2]


est_m3a<- summary(m3a)$coefficients[2,1]
est_m4a<- summary(m4a)$coefficients[2,1]
est_m5a<- summary(m5a)$coefficients[2,1]
est_m6a<- summary(m6a)$coefficients[2,1]
est_m7a<- summary(m7a)$coefficients[2,1]
est_m8a<- summary(m8a)$coefficients[2,1]
est_m9a<- summary(m9a)$coefficients[2,1]
est_m10a<- summary(m10a)$coefficients[2,1]


se_m3a <- summary(m3a)$coefficients[2,2]
se_m4a <- summary(m4a)$coefficients[2,2]
se_m5a <- summary(m5a)$coefficients[2,2]
se_m6a <- summary(m6a)$coefficients[2,2]
se_m7a <- summary(m7a)$coefficients[2,2]
se_m8a <- summary(m8a)$coefficients[2,2]
se_m9a <- summary(m9a)$coefficients[2,2]
se_m10a <- summary(m10a)$coefficients[2,2]

est_m3b<- summary(m3b)$coefficients[2,1]
est_m4b<- summary(m4b)$coefficients[2,1]
est_m5b<- summary(m5b)$coefficients[2,1]
est_m6b<- summary(m6b)$coefficients[2,1]
est_m7b<- summary(m7b)$coefficients[2,1]
est_m8b<- summary(m8b)$coefficients[2,1]
est_m9b<- summary(m9b)$coefficients[2,1]
est_m10b<- summary(m10b)$coefficients[2,1]


se_m3b <- summary(m3b)$coefficients[2,2]
se_m4b <- summary(m4b)$coefficients[2,2]
se_m5b <- summary(m5b)$coefficients[2,2]
se_m6b <- summary(m6b)$coefficients[2,2]
se_m7b <- summary(m7b)$coefficients[2,2]
se_m8b <- summary(m8b)$coefficients[2,2]
se_m9b <- summary(m9b)$coefficients[2,2]
se_m10b <- summary(m10b)$coefficients[2,2]



estimates <- c(est_m3,est_m4,est_m5,est_m6,est_m7,est_m8,est_m9, est_m10, 
               est_m3a,est_m4a,est_m5a,est_m6a,est_m7a,est_m8a,est_m9a, est_m10a,
               est_m3b,est_m4b,est_m5b,est_m6b,est_m7b,est_m8b,est_m9b, est_m10b)

se <- c(se_m3,se_m4,se_m5,se_m6,se_m7,se_m8,se_m9,se_m10,
        se_m3a,se_m4a,se_m5a,se_m6a,se_m7a,se_m8a,se_m9a,se_m10a,
        se_m3b,se_m4b,se_m5b,se_m6b,se_m7b,se_m8b,se_m9b,se_m10b)


condition <- rep(c("Full", "Scandal", "Achievement"), each = 8)
condition <- as.factor(condition)
levels(condition) <- c("1", "2", "3")
condition <- factor(condition, labels=c("Full", "Scandal", "Achievement"))
dvs <- rep(c("Search Engines", 
             "Social Media", "Liberal Media", "Conservative Media", "Gov. Think Tanks", 
             "Non-Gov. Think Tanks", 
             "Alternative Media", "Academic Inst."), 3)

figure1 <-  data.frame(estimates,se,Condition = condition,dvs) %>%
  ggplot(aes(y = estimates, x = dvs, color = Condition, shape = Condition)) +
  geom_point(position = position_dodge(width = .25), cex = 2) +
  geom_errorbar(aes(ymin = estimates - 1.96 *se, ymax = estimates + 1.96 * se), 
                width = 0, position = position_dodge(width = .25)) +  
  geom_errorbar(aes(ymin = estimates - 1.64 *se, ymax = estimates + 1.64 * se), 
                width = 0, position = position_dodge(width = .25), lwd = 1.1) +
  coord_flip()+
  xlab("") +
  ylab("") +
  geom_hline(yintercept = 0, col = "red") + 
  scale_color_manual(values = c("black", "brown", "blue")) +
  scale_y_continuous(limits=c(-20,20))+
  theme_minimal() + theme(axis.text=element_text(size=16), 
                          #       axis.title.x = element_text(size=22, face="bold"), 
                          #  plot.title=element_text( hjust=0.6, vjust=1.5, face='bold',size=22),
                          #   axis.title.y = element_text(size=22, face="bold"),
                          legend.title = element_text(size = 14, face="bold"),
                          legend.text = element_text(size = 14), legend.position = "bottom", 
                          panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor.x = element_blank(),
                          panel.grid.minor.y = element_line(colour = "grey15"), axis.line = element_line(colour = "black"))  


################################## Table (6): Treatment Effects on Trust in Different Information Providers (No Interaction Effects)

google <- lm(google ~ conspiracy+scandal, data = data)
social_media <- lm(social_media ~ conspiracy+scandal, data = data)
gov_tanks <- lm(gov_tanks ~ conspiracy+scandal, data = data)
nongov_tanks <- lm(nongov_tanks ~ conspiracy+scandal, data = data)
liberal_media <- lm(liberal_media ~ conspiracy+scandal, data = data)
consv_media <- lm(consv_media ~ conspiracy+scandal, data = data)
alt_media <- lm(alt_media ~ conspiracy+scandal, data = data)
academic <- lm(academic ~ conspiracy+scandal, data = data)


stargazer(google, social_media, gov_tanks, nongov_tanks, liberal_media, consv_media, alt_media, academic, align=TRUE,  no.space=TRUE, 
          covariate.labels = c("Conspiracy", "Scandal", "Control"), 
          keep.stat="n", dep.var.labels = c("\\multirow{2}{3 cm}{ \\centering Search\\ Engines}", 
                                            "\\multirow{2}{3 cm}{\\centering Social\\ Media}",
                                            "\\multirow{2}{3 cm}{ \\centering Gov.\\ Tanks}",
                                            "\\multirow{2}{3 cm}{ \\centering Non-gov.\\ Tanks}", 
                                            "\\multirow{2}{3 cm}{\\centering Liberal\\ Media}",
                                            "\\multirow{2}{3 cm}{ \\centering Conserv.\\ Media}", 
                                            "\\multirow{2}{3 cm}{ \\centering Alt.\\ Media}",
                                            "\\multirow{2}{3 cm}{ \\centering Academic\\ Instit.}"), 
          dep.var.caption="Outcome", digits=2, column.sep.width = "-35pt", header=F)


############################### Table (7): Treatment Effects on Trust in Different Information Providers (with Interaction Effects)

google <- lm(google ~ conspiracy+scandal+conspiracy*scandal, data = data)
social_media <- lm(social_media ~ conspiracy+scandal+conspiracy*scandal, data = data)
gov_tanks <- lm(gov_tanks ~ conspiracy+scandal+conspiracy*scandal, data = data)
nongov_tanks <- lm(nongov_tanks ~ conspiracy+scandal+conspiracy*scandal, data = data)
liberal_media <- lm(liberal_media ~ conspiracy+scandal+conspiracy*scandal, data = data)
consv_media <- lm(consv_media ~ conspiracy+scandal+conspiracy*scandal, data = data)
alt_media <- lm(alt_media ~ conspiracy+scandal+conspiracy*scandal, data = data)
academic <- lm(academic ~ conspiracy+scandal+conspiracy*scandal, data = data)


stargazer(google, social_media, gov_tanks, nongov_tanks, liberal_media, consv_media, alt_media, academic, align=TRUE,  no.space=TRUE, 
          covariate.labels = c("Conspiracy", "Scandal","Conspiracy x Scandal", "Control"), 
          keep.stat="n", dep.var.labels = c("\\multirow{2}{3 cm}{ \\centering Search\\ Engines}", 
                                            "\\multirow{2}{3 cm}{\\centering Social\\ Media}",
                                            "\\multirow{2}{3 cm}{ \\centering Gov.\\ Tanks}",
                                            "\\multirow{2}{3 cm}{ \\centering Non-gov.\\ Tanks}", 
                                            "\\multirow{2}{3 cm}{\\centering Liberal\\ Media}",
                                            "\\multirow{2}{3 cm}{ \\centering Conserv.\\ Media}", 
                                            "\\multirow{2}{3 cm}{ \\centering Alt.\\ Media}",
                                            "\\multirow{2}{3 cm}{ \\centering Academic\\ Instit.}"), 
          dep.var.caption="Outcome", digits=2, column.sep.width = "-35pt", header=F)


############################## Figure (2): Treatment Effects on Trust in Different Political Institutions

#Full sample models
m3 <- lm(fbi ~ conspiracy + scandal, data = data)
m4 <- lm(cia ~ conspiracy + scandal, data = data)
m5 <- lm(rep_lead ~ conspiracy + scandal, data = data)
m6 <- lm(dem_lead ~ conspiracy + scandal, data = data)
m7 <- lm(courts ~ conspiracy + scandal, data = data)
m8 <- lm(sup_courts ~ conspiracy + scandal, data = data)

#Scandal condition models
m3a <- lm(fbi ~ conspiracy + scandal, data = data_scandal)
m4a <- lm(cia ~ conspiracy + scandal, data = data_scandal)
m5a <- lm(rep_lead ~ conspiracy + scandal, data = data_scandal)
m6a <- lm(dem_lead ~ conspiracy + scandal, data = data_scandal)
m7a <- lm(courts ~ conspiracy + scandal, data = data_scandal)
m8a <- lm(sup_courts ~ conspiracy + scandal, data = data_scandal)

#Achievement condition models
m3b <- lm(fbi ~ conspiracy + scandal, data = data_achievement)
m4b <- lm(cia ~ conspiracy + scandal, data = data_achievement)
m5b <- lm(rep_lead ~ conspiracy + scandal, data = data_achievement)
m6b <- lm(dem_lead ~ conspiracy + scandal, data = data_achievement)
m7b <- lm(courts ~ conspiracy + scandal, data = data_achievement)
m8b <- lm(sup_courts ~ conspiracy + scandal, data = data_achievement)

#Extract coefficients and SEs
est_m3<- summary(m3)$coefficients[2,1]
est_m4<- summary(m4)$coefficients[2,1]
est_m5<- summary(m5)$coefficients[2,1]
est_m6<- summary(m6)$coefficients[2,1]
est_m7<- summary(m7)$coefficients[2,1]
est_m8<- summary(m8)$coefficients[2,1]

se_m3 <- summary(m3)$coefficients[2,2]
se_m4 <- summary(m4)$coefficients[2,2]
se_m5 <- summary(m5)$coefficients[2,2]
se_m6 <- summary(m6)$coefficients[2,2]
se_m7 <- summary(m7)$coefficients[2,2]
se_m8 <- summary(m8)$coefficients[2,2]

est_m3a<- summary(m3a)$coefficients[2,1]
est_m4a<- summary(m4a)$coefficients[2,1]
est_m5a<- summary(m5a)$coefficients[2,1]
est_m6a<- summary(m6a)$coefficients[2,1]
est_m7a<- summary(m7a)$coefficients[2,1]
est_m8a<- summary(m8a)$coefficients[2,1]

se_m3a <- summary(m3a)$coefficients[2,2]
se_m4a <- summary(m4a)$coefficients[2,2]
se_m5a <- summary(m5a)$coefficients[2,2]
se_m6a <- summary(m6a)$coefficients[2,2]
se_m7a <- summary(m7a)$coefficients[2,2]
se_m8a <- summary(m8a)$coefficients[2,2]

est_m3b<- summary(m3b)$coefficients[2,1]
est_m4b<- summary(m4b)$coefficients[2,1]
est_m5b<- summary(m5b)$coefficients[2,1]
est_m6b<- summary(m6b)$coefficients[2,1]
est_m7b<- summary(m7b)$coefficients[2,1]
est_m8b<- summary(m8b)$coefficients[2,1]

se_m3b <- summary(m3b)$coefficients[2,2]
se_m4b <- summary(m4b)$coefficients[2,2]
se_m5b <- summary(m5b)$coefficients[2,2]
se_m6b <- summary(m6b)$coefficients[2,2]
se_m7b <- summary(m7b)$coefficients[2,2]
se_m8b <- summary(m8b)$coefficients[2,2]

estimates <- c(est_m3,est_m4,est_m5,est_m6,est_m7,est_m8, 
               est_m3a,est_m4a,est_m5a,est_m6a,est_m7a,est_m8a,
               est_m3b,est_m4b,est_m5b,est_m6b,est_m7b,est_m8b)

se <- c(se_m3,se_m4,se_m5,se_m6,se_m7,se_m8,
        se_m3a,se_m4a,se_m5a,se_m6a,se_m7a,se_m8a,
        se_m3b,se_m4b,se_m5b,se_m6b,se_m7b,se_m8b)


condition <- rep(c("Full", "Scandal", "Achievement"), each = 6)
condition <- as.factor(condition)
levels(condition) <- c("1", "2", "3")
condition <- factor(condition, labels=c("Full", "Scandal", "Achievement"))
dvs <- rep(c("FBI", 
             "CIA", "Republican Leaders", "Democratic Leaders", "District Courts", 
             "Supreme Court"), 3)

figure2 <-  data.frame(estimates,se,Condition = condition,dvs) %>%
  ggplot(aes(y = estimates, x = dvs, color = Condition, shape = Condition)) +
  geom_point(position = position_dodge(width = .25), cex = 2) +
  geom_errorbar(aes(ymin = estimates - 1.96 *se, ymax = estimates + 1.96 * se), 
                width = 0, position = position_dodge(width = .25)) +  
  geom_errorbar(aes(ymin = estimates - 1.64 *se, ymax = estimates + 1.64 * se), 
                width = 0, position = position_dodge(width = .25), lwd = 1.1) +
  coord_flip()+
  xlab("") +
  ylab("") +
  geom_hline(yintercept = 0, col = "red") + 
  scale_color_manual(values = c("black", "brown", "blue")) +
  scale_y_continuous(limits=c(-20,20))+
  theme_minimal() + theme(axis.text=element_text(size=16), 
                          #       axis.title.x = element_text(size=22, face="bold"), 
                          #  plot.title=element_text( hjust=0.6, vjust=1.5, face='bold',size=22),
                          #   axis.title.y = element_text(size=22, face="bold"),
                          legend.title = element_text(size = 14, face="bold"),
                          legend.text = element_text(size = 14), legend.position = "bottom", 
                          panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor.x = element_blank(),
                          panel.grid.minor.y = element_line(colour = "grey15"), axis.line = element_line(colour = "black")) 


##################################### Table (8): Treatment Effects on Trust in Different Political Institutions (No Interaction Effects)

fbi <- lm(fbi ~ conspiracy+scandal, data = data)
cia <- lm(cia ~ conspiracy+scandal, data = data)
rep_lead <- lm(rep_lead ~ conspiracy+scandal, data = data)
dem_lead <- lm(dem_lead ~ conspiracy+scandal, data = data)
courts <- lm(courts ~ conspiracy+scandal, data = data)
sup_courts <- lm(sup_courts ~ conspiracy+scandal, data = data)



stargazer(fbi, cia, rep_lead, dem_lead, courts, sup_courts, align=TRUE,  no.space=TRUE, 
          covariate.labels = c("Conspiracy", "Scandal", "Control"), 
          keep.stat="n", dep.var.labels = c("\\multirow{2}{3 cm}{ \\centering FBI}", 
                                            "\\multirow{2}{3 cm}{\\centering CIA}",
                                            "\\multirow{2}{3 cm}{ \\centering Rep. \\ Leaders}",
                                            "\\multirow{2}{3 cm}{ \\centering Dem. \\ Leaders}", 
                                            "\\multirow{2}{3 cm}{\\centering District \\ Courts}",
                                            "\\multirow{2}{3 cm}{ \\centering Supreme \\ Courts}"), 
          dep.var.caption="Outcome", digits=2, column.sep.width = "-35pt", header=F)



################################## Table (9): Treatment Effects on Trust in Different Political Institutions (with Interaction Effects)

fbi <- lm(fbi ~ conspiracy+scandal+conspiracy*scandal, data = data)
cia <- lm(cia ~ conspiracy+scandal+conspiracy*scandal, data = data)
rep_lead <- lm(rep_lead ~ conspiracy+scandal+conspiracy*scandal, data = data)
dem_lead <- lm(dem_lead ~ conspiracy+scandal+conspiracy*scandal, data = data)
courts <- lm(courts ~ conspiracy+scandal+conspiracy*scandal, data = data)
sup_courts <- lm(sup_courts ~ conspiracy+scandal+conspiracy*scandal, data = data)

stargazer(fbi, cia, rep_lead, dem_lead, courts, sup_courts, align=TRUE,  no.space=TRUE, 
          covariate.labels = c("Conspiracy", "Scandal",  "Conspiracy x Scandal", "Control"), 
          keep.stat="n", dep.var.labels = c("\\multirow{2}{3 cm}{ \\centering FBI}", 
                                            "\\multirow{2}{3 cm}{\\centering CIA}",
                                            "\\multirow{2}{3 cm}{ \\centering Rep. \\ Leaders}",
                                            "\\multirow{2}{3 cm}{ \\centering Dem. \\ Leaders}", 
                                            "\\multirow{2}{3 cm}{\\centering District \\ Courts}",
                                            "\\multirow{2}{3 cm}{ \\centering Supreme \\ Courts}"), 
          dep.var.caption="Outcome", digits=2, column.sep.width = "-35pt", header=F)


################################# Figure (3): Treatment Effects on Different Items Composing the Political Support Score

#Full sample models
m3 <- lm(vote ~ conspiracy + scandal, data = data)
m4 <- lm(no_impeach ~ conspiracy + scandal, data = data)
m5 <- lm(investigation ~ conspiracy + scandal, data = data)
m6 <- lm(performance ~ conspiracy + scandal, data = data)

#Scandal condition models
m3a <- lm(vote ~ conspiracy + scandal, data = data_scandal)
m4a <- lm(no_impeach ~ conspiracy + scandal, data = data_scandal)
m5a <- lm(investigation ~ conspiracy + scandal, data = data_scandal)
m6a <- lm(performance ~ conspiracy + scandal, data = data_scandal)

#Achievement condition models
m3b <- lm(vote ~ conspiracy + scandal, data = data_achievement)
m4b <- lm(no_impeach ~ conspiracy + scandal, data = data_achievement)
m5b <- lm(investigation ~ conspiracy + scandal, data = data_achievement)
m6b <- lm(performance ~ conspiracy + scandal, data = data_achievement)

#Extracting coefficients and SEs
est_m3<- summary(m3)$coefficients[2,1]
est_m4<- summary(m4)$coefficients[2,1]
est_m5<- summary(m5)$coefficients[2,1]
est_m6<- summary(m6)$coefficients[2,1]

se_m3 <- summary(m3)$coefficients[2,2]
se_m4 <- summary(m4)$coefficients[2,2]
se_m5 <- summary(m5)$coefficients[2,2]
se_m6 <- summary(m6)$coefficients[2,2]

est_m3a<- summary(m3a)$coefficients[2,1]
est_m4a<- summary(m4a)$coefficients[2,1]
est_m5a<- summary(m5a)$coefficients[2,1]
est_m6a<- summary(m6a)$coefficients[2,1]

se_m3a <- summary(m3a)$coefficients[2,2]
se_m4a <- summary(m4a)$coefficients[2,2]
se_m5a <- summary(m5a)$coefficients[2,2]
se_m6a <- summary(m6a)$coefficients[2,2]

est_m3b<- summary(m3b)$coefficients[2,1]
est_m4b<- summary(m4b)$coefficients[2,1]
est_m5b<- summary(m5b)$coefficients[2,1]
est_m6b<- summary(m6b)$coefficients[2,1]

se_m3b <- summary(m3b)$coefficients[2,2]
se_m4b <- summary(m4b)$coefficients[2,2]
se_m5b <- summary(m5b)$coefficients[2,2]
se_m6b <- summary(m6b)$coefficients[2,2]



estimates <- c(est_m3,est_m4,est_m5,est_m6,
               est_m3a,est_m4a,est_m5a,est_m6a,
               est_m3b,est_m4b,est_m5b,est_m6b)

se <- c(se_m3,se_m4,se_m5,se_m6,
        se_m3a,se_m4a,se_m5a,se_m6a,
        se_m3b,se_m4b,se_m5b,se_m6b)


condition <- rep(c("Full", "Scandal", "Achievement"), each = 4)
condition <- as.factor(condition)
levels(condition) <- c("1", "2", "3")
condition <- factor(condition, labels=c("Full", "Scandal", "Achievement"))
dvs <- rep(c("Vote", "Impeachment", "Investigation", "Performance"), 3)

figure3 <-  data.frame(estimates,se,Condition = condition,dvs) %>%
  ggplot(aes(y = estimates, x = dvs, color = Condition, shape = Condition)) +
  geom_point(position = position_dodge(width = .25), cex = 2) +
  geom_errorbar(aes(ymin = estimates - 1.96 *se, ymax = estimates + 1.96 * se), 
                width = 0, position = position_dodge(width = .25)) +  
  geom_errorbar(aes(ymin = estimates - 1.64 *se, ymax = estimates + 1.64 * se), 
                width = 0, position = position_dodge(width = .25), lwd = 1.1) +
  coord_flip()+
  xlab("") +
  ylab("") +
  geom_hline(yintercept = 0, col = "red") + 
  scale_color_manual(values = c("black", "brown", "blue")) +
  scale_y_continuous(limits=c(-20,20))+
  theme_minimal() + theme(axis.text=element_text(size=16), 
                          #       axis.title.x = element_text(size=22, face="bold"), 
                          #  plot.title=element_text( hjust=0.6, vjust=1.5, face='bold',size=22),
                          #   axis.title.y = element_text(size=22, face="bold"),
                          legend.title = element_text(size = 14, face="bold"),
                          legend.text = element_text(size = 14), legend.position = "bottom", 
                          panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor.x = element_blank(),
                          panel.grid.minor.y = element_line(colour = "grey15"), axis.line = element_line(colour = "black"))  



################################### Table (10): Treatment Effects on the Different Items Composing the Political Support Score

investigation <- lm(investigation ~ conspiracy+scandal, data = data)
performance <- lm(performance ~ conspiracy+scandal, data = data)
vote <- lm(vote ~ conspiracy+scandal, data = data)
no_impeach <- lm(no_impeach ~ conspiracy+scandal, data = data)

investigation1 <- lm(investigation ~ conspiracy+scandal+conspiracy*scandal, data = data)
performance1 <- lm(performance ~ conspiracy+scandal+conspiracy*scandal, data = data)
vote1 <- lm(vote ~ conspiracy+scandal+conspiracy*scandal, data = data)
no_impeach1 <- lm(no_impeach ~ conspiracy+scandal+conspiracy*scandal, data = data)


stargazer(investigation, performance, vote, no_impeach, investigation1, performance1, vote1, no_impeach1, align=TRUE,  no.space=TRUE, 
          covariate.labels = c("Conspiracy", "Scandal", "Scandal x Conspiracy", "Control"), 
          keep.stat="n", dep.var.labels = c("\\multirow{2}{3 cm}{ \\centering Investigation}", 
                                            "\\multirow{2}{3 cm}{\\centering Perfomance}",
                                            "\\multirow{2}{3 cm}{ \\centering Vote}",
                                            "\\multirow{2}{3 cm}{ \\centering Impeachment}", 
                                            "\\multirow{2}{3 cm}{ \\centering Investigation}", 
                                            "\\multirow{2}{3 cm}{\\centering Perfomance}",
                                            "\\multirow{2}{3 cm}{ \\centering Vote}",
                                            "\\multirow{2}{3 cm}{ \\centering Impeachment}"), 
          dep.var.caption="Outcome", digits=2, column.sep.width = "-35pt", header=F)


###################################### Figure (4): Treatment Effects by Partisanship

#Full Sample
m1 <- lm(media_score ~ conspiracy*Rep+scandal, data = data)
m2 <- lm(article_accuracy ~ conspiracy*Rep+scandal, data = data)
m3 <- lm(poltrust_score ~ conspiracy*Rep+scandal, data = data)
m4 <- lm(account_score ~ conspiracy*Rep+scandal, data = data)

est_m1a<- summary(m1)$coefficients[2,1]
est_m1b<- summary(m1)$coefficients[4,1]
est_m2a<- summary(m2)$coefficients[2,1]
est_m2b<- summary(m2)$coefficients[4,1]
est_m3a<- summary(m3)$coefficients[2,1]
est_m3b<- summary(m3)$coefficients[4,1]
est_m4a<- summary(m4)$coefficients[2,1]
est_m4b<- summary(m4)$coefficients[4,1]

se_m1a<- summary(m1)$coefficients[2,2]
se_m1b<- summary(m1)$coefficients[4,2]
se_m2a<- summary(m2)$coefficients[2,2]
se_m2b<- summary(m2)$coefficients[4,2]
se_m3a<- summary(m3)$coefficients[2,2]
se_m3b<- summary(m3)$coefficients[4,2]
se_m4a<- summary(m4)$coefficients[2,2]
se_m4b<- summary(m4)$coefficients[4,2]

estimates <- c(est_m1a,est_m1b,est_m2a,est_m2b,est_m3a,est_m3b,est_m4a,est_m4b)
se <- c(se_m1a,se_m1b,se_m2a,se_m2b,se_m3a,se_m3b,se_m4a,se_m4b)
variable <- c("Conspiracy", "Conspiracy x Rep.", 
              "Conspiracy", "Conspiracy x Rep.",
              "Conspiracy", "Conspiracy x Rep.",
              "Conspiracy", "Conspiracy x Rep.")

dvs <- c( "Trust in Info. Providers", "Trust in Info. Providers", 
          "Article Accuracy", "Article Accuracy",
          "Trust in Political Inst.", "Trust in Political Inst.",
          "Political Support", "Political Support")

dvs <- factor(dvs, levels=unique(dvs))

figure4_full <-  data.frame(estimates,se, Condition = variable, dvs) %>%
  ggplot(aes(y = estimates, x = dvs, color = Condition, shape = Condition)) +
  geom_point(position = position_dodge(width = .25), cex = 2) +
  geom_errorbar(aes(ymin = estimates - 1.96 *se, ymax = estimates + 1.96 * se), 
                width = 0, position = position_dodge(width = .25)) +  
  geom_errorbar(aes(ymin = estimates - 1.64 *se, ymax = estimates + 1.64 * se), 
                width = 0, position = position_dodge(width = .25), lwd = 1.1) +
  coord_flip()+
  xlab("") +
  ylab("") +
  geom_hline(yintercept = 0, col = "red") + 
  scale_color_manual(values = c("grey", "black")) +
  scale_y_continuous(limits=c(-30,30))+
  theme_minimal() + theme(axis.text=element_text(size=16), 
                          #       axis.title.x = element_text(size=22, face="bold"), 
                          #  plot.title=element_text( hjust=0.6, vjust=1.5, face='bold',size=22),
                          #   axis.title.y = element_text(size=22, face="bold"),
                          legend.title = element_blank(),
                          legend.text = element_text(size = 14), legend.position = "bottom", 
                          panel.border = element_blank(), panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                          plot.title = element_text(color="black", size=16, face="bold", hjust=(0.5), vjust=1))+ggtitle("Full Sample")


#Achievement Condition

m1 <- lm(media_score ~ conspiracy*Rep, data = data_achievement)
m2 <- lm(article_accuracy ~ conspiracy*Rep, data = data_achievement)
m3 <- lm(poltrust_score ~ conspiracy*Rep, data = data_achievement)
m4 <- lm(account_score ~ conspiracy*Rep, data = data_achievement)

est_m1a<- summary(m1)$coefficients[2,1]
est_m1b<- summary(m1)$coefficients[4,1]
est_m2a<- summary(m2)$coefficients[2,1]
est_m2b<- summary(m2)$coefficients[4,1]
est_m3a<- summary(m3)$coefficients[2,1]
est_m3b<- summary(m3)$coefficients[4,1]
est_m4a<- summary(m4)$coefficients[2,1]
est_m4b<- summary(m4)$coefficients[4,1]

se_m1a<- summary(m1)$coefficients[2,2]
se_m1b<- summary(m1)$coefficients[4,2]
se_m2a<- summary(m2)$coefficients[2,2]
se_m2b<- summary(m2)$coefficients[4,2]
se_m3a<- summary(m3)$coefficients[2,2]
se_m3b<- summary(m3)$coefficients[4,2]
se_m4a<- summary(m4)$coefficients[2,2]
se_m4b<- summary(m4)$coefficients[4,2]

estimates <- c(est_m1a,est_m1b,est_m2a,est_m2b,est_m3a,est_m3b,est_m4a,est_m4b)
se <- c(se_m1a,se_m1b,se_m2a,se_m2b,se_m3a,se_m3b,se_m4a,se_m4b)
condition <- c("Conspiracy", "Conspiracy x Rep.", 
               "Conspiracy", "Conspiracy x Rep.",
               "Conspiracy", "Conspiracy x Rep.",
               "Conspiracy", "Conspiracy x Rep.")

dvs <- c("Trust in Info. Providers", "Trust in Info. Providers", 
         "Article Accuracy", "Article Accuracy",
         "Trust in Political Inst.", "Trust in Political Inst.",
         "Political Support", "Political Support")

dvs <- factor(dvs, levels=unique(dvs))

figure4_ach <-  data.frame(estimates,se, Condition = variable, dvs) %>%
  ggplot(aes(y = estimates, x = dvs, color = Condition, shape = Condition)) +
  geom_point(position = position_dodge(width = .25), cex = 2) +
  geom_errorbar(aes(ymin = estimates - 1.96 *se, ymax = estimates + 1.96 * se), 
                width = 0, position = position_dodge(width = .25)) +  
  geom_errorbar(aes(ymin = estimates - 1.64 *se, ymax = estimates + 1.64 * se), 
                width = 0, position = position_dodge(width = .25), lwd = 1.1) +
  coord_flip()+
  xlab("") +
  ylab("") +
  geom_hline(yintercept = 0, col = "red") + 
  scale_color_manual(values = c("grey", "black")) +
  scale_y_continuous(limits=c(-30,30))+
  theme_minimal() + theme(axis.text=element_text(size=16), 
                          #       axis.title.x = element_text(size=22, face="bold"), 
                          #  plot.title=element_text( hjust=0.6, vjust=1.5, face='bold',size=22),
                          #   axis.title.y = element_text(size=22, face="bold"),
                          legend.title = element_blank(),
                          legend.text = element_text(size = 14), legend.position = "bottom", 
                          panel.border = element_blank(), panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                          plot.title = element_text(color="black", size=16, face="bold", hjust=(0.5), vjust=1))+ggtitle("Achievement Condition")


#Scandal Condition

m1 <- lm(media_score ~ conspiracy*Rep, data = data_scandal)
m2 <- lm(article_accuracy ~ conspiracy*Rep, data = data_scandal)
m3 <- lm(poltrust_score ~ conspiracy*Rep, data = data_scandal)
m4 <- lm(account_score ~ conspiracy*Rep, data = data_scandal)

est_m1a<- summary(m1)$coefficients[2,1]
est_m1b<- summary(m1)$coefficients[4,1]
est_m2a<- summary(m2)$coefficients[2,1]
est_m2b<- summary(m2)$coefficients[4,1]
est_m3a<- summary(m3)$coefficients[2,1]
est_m3b<- summary(m3)$coefficients[4,1]
est_m4a<- summary(m4)$coefficients[2,1]
est_m4b<- summary(m4)$coefficients[4,1]

se_m1a<- summary(m1)$coefficients[2,2]
se_m1b<- summary(m1)$coefficients[4,2]
se_m2a<- summary(m2)$coefficients[2,2]
se_m2b<- summary(m2)$coefficients[4,2]
se_m3a<- summary(m3)$coefficients[2,2]
se_m3b<- summary(m3)$coefficients[4,2]
se_m4a<- summary(m4)$coefficients[2,2]
se_m4b<- summary(m4)$coefficients[4,2]

estimates <- c(est_m1a,est_m1b,est_m2a,est_m2b,est_m3a,est_m3b,est_m4a,est_m4b)
se <- c(se_m1a,se_m1b,se_m2a,se_m2b,se_m3a,se_m3b,se_m4a,se_m4b)
condition <- c("Conspiracy", "Conspiracy x Rep.", 
               "Conspiracy", "Conspiracy x Rep.",
               "Conspiracy", "Conspiracy x Rep.",
               "Conspiracy", "Conspiracy x Rep.")

dvs <- c("Trust in Info. Providers", "Trust in Info. Providers", 
         "Article Accuracy", "Article Accuracy",
         "Trust in Political Inst.", "Trust in Political Inst.",
         "Political Support", "Political Support")

dvs <- factor(dvs, levels=unique(dvs))

figure4_scandal <-  data.frame(estimates,se, Condition = variable, dvs) %>%
  ggplot(aes(y = estimates, x = dvs, color = Condition, shape = Condition)) +
  geom_point(position = position_dodge(width = .25), cex = 2) +
  geom_errorbar(aes(ymin = estimates - 1.96 *se, ymax = estimates + 1.96 * se), 
                width = 0, position = position_dodge(width = .25)) +  
  geom_errorbar(aes(ymin = estimates - 1.64 *se, ymax = estimates + 1.64 * se), 
                width = 0, position = position_dodge(width = .25), lwd = 1.1) +
  coord_flip()+
  xlab("") +
  ylab("") +
  geom_hline(yintercept = 0, col = "red") + 
  scale_color_manual(values = c("grey", "black")) +
  scale_y_continuous(limits=c(-30,30))+
  theme_minimal() + theme(axis.text=element_text(size=16), 
                          #       axis.title.x = element_text(size=22, face="bold"), 
                          #  plot.title=element_text( hjust=0.6, vjust=1.5, face='bold',size=22),
                          #   axis.title.y = element_text(size=22, face="bold"),
                          legend.title = element_blank(),
                          legend.text = element_text(size = 14), legend.position = "bottom", 
                          panel.border = element_blank(), panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                          plot.title = element_text(color="black", size=16, face="bold", hjust=(0.5), vjust=1))+ggtitle("Scandal Condition")


############################## Table (11): OLS Analysis of Treatment Effects by Partisanship

media <- lm(media_score ~ conspiracy+scandal+conspiracy*Rep, data = data)
article <- lm(article_accuracy ~ conspiracy+scandal+conspiracy*Rep, data = data)
poltrust <- lm(poltrust_score ~ conspiracy+scandal+conspiracy*Rep, data = data)
support <- lm(account_score ~ conspiracy+scandal+conspiracy*Rep, data = data)


media1 <- lm(media_score ~ conspiracy+scandal + scandal*conspiracy*Rep, data = data)
article1 <- lm(article_accuracy ~ conspiracy+scandal + scandal*conspiracy*Rep, data = data)
poltrust1 <- lm(poltrust_score ~ conspiracy+scandal + scandal*conspiracy*Rep, data = data)
support1 <- lm(account_score ~ conspiracy+scandal + scandal*conspiracy*Rep, data = data)


stargazer(media, article, poltrust, support, media1, article1, poltrust1, support1, align=TRUE,  no.space=TRUE, 
          covariate.labels = c("Conspiracy", "Scandal", "Rep", "Scandal x Conspiracy", "Scandal x Rep", "Conspiracy x Rep", "Conspiracy x Scandal x Rep", "Control"), 
          keep.stat="n", dep.var.labels = c("\\multirow{2}{3 cm}{ \\centering Trust\\ Info. Providers}", 
                                            "\\multirow{2}{3 cm}{\\centering Article\\ Accuracy}",
                                            "\\multirow{2}{3 cm}{ \\centering Trust\\ Instit.}",
                                            "\\multirow{2}{3 cm}{ \\centering Political\\ Support}",
                                            "\\multirow{2}{3 cm}{ \\centering Trust\\ Info. Providers}", 
                                            "\\multirow{2}{3 cm}{\\centering Article\\ Accuracy}",
                                            "\\multirow{2}{3 cm}{ \\centering Trust\\ Instit.}",
                                            "\\multirow{2}{3 cm}{ \\centering Political\\ Support}"), 
          dep.var.caption="Outcome", digits=2, column.sep.width = "-35pt", header=F)


################################### Figure (5): Treatment Effects by Race

#Full Sample

m1 <- lm(media_score ~ conspiracy*white, data = data)
m2 <- lm(article_accuracy ~ conspiracy*white, data = data)
m3 <- lm(poltrust_score ~ conspiracy*white, data = data)
m4 <- lm(account_score ~ conspiracy*white, data = data)

est_m1a<- summary(m1)$coefficients[2,1]
est_m1b<- summary(m1)$coefficients[4,1]
est_m2a<- summary(m2)$coefficients[2,1]
est_m2b<- summary(m2)$coefficients[4,1]
est_m3a<- summary(m3)$coefficients[2,1]
est_m3b<- summary(m3)$coefficients[4,1]
est_m4a<- summary(m4)$coefficients[2,1]
est_m4b<- summary(m4)$coefficients[4,1]

se_m1a<- summary(m1)$coefficients[2,2]
se_m1b<- summary(m1)$coefficients[4,2]
se_m2a<- summary(m2)$coefficients[2,2]
se_m2b<- summary(m2)$coefficients[4,2]
se_m3a<- summary(m3)$coefficients[2,2]
se_m3b<- summary(m3)$coefficients[4,2]
se_m4a<- summary(m4)$coefficients[2,2]
se_m4b<- summary(m4)$coefficients[4,2]

estimates <- c(est_m1a,est_m1b,est_m2a,est_m2b,est_m3a,est_m3b,est_m4a,est_m4b)
se <- c(se_m1a,se_m1b,se_m2a,se_m2b,se_m3a,se_m3b,se_m4a,se_m4b)
variable <- c("Conspiracy", "Conspiracy x White", 
              "Conspiracy", "Conspiracy x White",
              "Conspiracy", "Conspiracy x White",
              "Conspiracy", "Conspiracy x White")

dvs <- c( "Trust in Info. Providers", "Trust in Info. Providers", 
          "Article Accuracy", "Article Accuracy",
          "Trust in Political Inst.", "Trust in Political Inst.",
          "Political Support", "Political Support")

dvs <- factor(dvs, levels=unique(dvs))

figure5_full <-  data.frame(estimates,se, Condition = variable, dvs) %>%
  ggplot(aes(y = estimates, x = dvs, color = Condition, shape = Condition)) +
  geom_point(position = position_dodge(width = .25), cex = 2) +
  geom_errorbar(aes(ymin = estimates - 1.96 *se, ymax = estimates + 1.96 * se), 
                width = 0, position = position_dodge(width = .25)) +  
  geom_errorbar(aes(ymin = estimates - 1.64 *se, ymax = estimates + 1.64 * se), 
                width = 0, position = position_dodge(width = .25), lwd = 1.1) +
  coord_flip()+
  xlab("") +
  ylab("") +
  geom_hline(yintercept = 0, col = "red") + 
  scale_color_manual(values = c("grey", "black")) +
  scale_y_continuous(limits=c(-30,30))+
  theme_minimal() + theme(axis.text=element_text(size=16), 
                          #       axis.title.x = element_text(size=22, face="bold"), 
                          #  plot.title=element_text( hjust=0.6, vjust=1.5, face='bold',size=22),
                          #   axis.title.y = element_text(size=22, face="bold"),
                          legend.title = element_blank(),
                          legend.text = element_text(size = 14), legend.position = "bottom", 
                          panel.border = element_blank(), panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                          plot.title = element_text(color="black", size=16, face="bold", hjust=(0.5), vjust=1))+ggtitle("Full Sample")


#Achievement Condition

m1 <- lm(media_score ~ conspiracy*white, data = data_achievement)
m2 <- lm(article_accuracy ~ conspiracy*white, data = data_achievement)
m3 <- lm(poltrust_score ~ conspiracy*white, data = data_achievement)
m4 <- lm(account_score ~ conspiracy*white, data = data_achievement)

est_m1a<- summary(m1)$coefficients[2,1]
est_m1b<- summary(m1)$coefficients[4,1]
est_m2a<- summary(m2)$coefficients[2,1]
est_m2b<- summary(m2)$coefficients[4,1]
est_m3a<- summary(m3)$coefficients[2,1]
est_m3b<- summary(m3)$coefficients[4,1]
est_m4a<- summary(m4)$coefficients[2,1]
est_m4b<- summary(m4)$coefficients[4,1]

se_m1a<- summary(m1)$coefficients[2,2]
se_m1b<- summary(m1)$coefficients[4,2]
se_m2a<- summary(m2)$coefficients[2,2]
se_m2b<- summary(m2)$coefficients[4,2]
se_m3a<- summary(m3)$coefficients[2,2]
se_m3b<- summary(m3)$coefficients[4,2]
se_m4a<- summary(m4)$coefficients[2,2]
se_m4b<- summary(m4)$coefficients[4,2]

estimates <- c(est_m1a,est_m1b,est_m2a,est_m2b,est_m3a,est_m3b,est_m4a,est_m4b)
se <- c(se_m1a,se_m1b,se_m2a,se_m2b,se_m3a,se_m3b,se_m4a,se_m4b)
condition <- c("Conspiracy", "Conspiracy x White", 
               "Conspiracy", "Conspiracy x White",
               "Conspiracy", "Conspiracy x White",
               "Conspiracy", "Conspiracy x White")

dvs <- c("Trust in Info. Providers", "Trust in Info. Providers", 
         "Article Accuracy", "Article Accuracy",
         "Trust in Political Inst.", "Trust in Political Inst.",
         "Political Support", "Political Support")

dvs <- factor(dvs, levels=unique(dvs))

figure5_ach <-  data.frame(estimates,se, Condition = variable, dvs) %>%
  ggplot(aes(y = estimates, x = dvs, color = Condition, shape = Condition)) +
  geom_point(position = position_dodge(width = .25), cex = 2) +
  geom_errorbar(aes(ymin = estimates - 1.96 *se, ymax = estimates + 1.96 * se), 
                width = 0, position = position_dodge(width = .25)) +  
  geom_errorbar(aes(ymin = estimates - 1.64 *se, ymax = estimates + 1.64 * se), 
                width = 0, position = position_dodge(width = .25), lwd = 1.1) +
  coord_flip()+
  xlab("") +
  ylab("") +
  geom_hline(yintercept = 0, col = "red") + 
  scale_color_manual(values = c("grey", "black")) +
  scale_y_continuous(limits=c(-30,30))+
  theme_minimal() + theme(axis.text=element_text(size=16), 
                          #       axis.title.x = element_text(size=22, face="bold"), 
                          #  plot.title=element_text( hjust=0.6, vjust=1.5, face='bold',size=22),
                          #   axis.title.y = element_text(size=22, face="bold"),
                          legend.title = element_blank(),
                          legend.text = element_text(size = 14), legend.position = "bottom", 
                          panel.border = element_blank(), panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                          plot.title = element_text(color="black", size=16, face="bold", hjust=(0.5), vjust=1))+ggtitle("Achievement Condition")


#Scandal Condition

m1 <- lm(media_score ~ conspiracy*white, data = data_scandal)
m2 <- lm(article_accuracy ~ conspiracy*white, data = data_scandal)
m3 <- lm(poltrust_score ~ conspiracy*white, data = data_scandal)
m4 <- lm(account_score ~ conspiracy*white, data = data_scandal)

est_m1a<- summary(m1)$coefficients[2,1]
est_m1b<- summary(m1)$coefficients[4,1]
est_m2a<- summary(m2)$coefficients[2,1]
est_m2b<- summary(m2)$coefficients[4,1]
est_m3a<- summary(m3)$coefficients[2,1]
est_m3b<- summary(m3)$coefficients[4,1]
est_m4a<- summary(m4)$coefficients[2,1]
est_m4b<- summary(m4)$coefficients[4,1]

se_m1a<- summary(m1)$coefficients[2,2]
se_m1b<- summary(m1)$coefficients[4,2]
se_m2a<- summary(m2)$coefficients[2,2]
se_m2b<- summary(m2)$coefficients[4,2]
se_m3a<- summary(m3)$coefficients[2,2]
se_m3b<- summary(m3)$coefficients[4,2]
se_m4a<- summary(m4)$coefficients[2,2]
se_m4b<- summary(m4)$coefficients[4,2]

estimates <- c(est_m1a,est_m1b,est_m2a,est_m2b,est_m3a,est_m3b,est_m4a,est_m4b)
se <- c(se_m1a,se_m1b,se_m2a,se_m2b,se_m3a,se_m3b,se_m4a,se_m4b)
condition <- c("Conspiracy", "Conspiracy x White", 
               "Conspiracy", "Conspiracy x White",
               "Conspiracy", "Conspiracy x White",
               "Conspiracy", "Conspiracy x White")

dvs <- c("Trust in Info. Providers", "Trust in Info. Providers", 
         "Article Accuracy", "Article Accuracy",
         "Trust in Political Inst.", "Trust in Political Inst.",
         "Political Support", "Political Support")

dvs <- factor(dvs, levels=unique(dvs))

figure5_scandal <-  data.frame(estimates,se, Condition = variable, dvs) %>%
  ggplot(aes(y = estimates, x = dvs, color = Condition, shape = Condition)) +
  geom_point(position = position_dodge(width = .25), cex = 2) +
  geom_errorbar(aes(ymin = estimates - 1.96 *se, ymax = estimates + 1.96 * se), 
                width = 0, position = position_dodge(width = .25)) +  
  geom_errorbar(aes(ymin = estimates - 1.64 *se, ymax = estimates + 1.64 * se), 
                width = 0, position = position_dodge(width = .25), lwd = 1.1) +
  coord_flip()+
  xlab("") +
  ylab("") +
  geom_hline(yintercept = 0, col = "red") + 
  scale_color_manual(values = c("grey", "black")) +
  scale_y_continuous(limits=c(-30,30))+
  theme_minimal() + theme(axis.text=element_text(size=16), 
                          #       axis.title.x = element_text(size=22, face="bold"), 
                          #  plot.title=element_text( hjust=0.6, vjust=1.5, face='bold',size=22),
                          #   axis.title.y = element_text(size=22, face="bold"),
                          legend.title = element_blank(),
                          legend.text = element_text(size = 14), legend.position = "bottom", 
                          panel.border = element_blank(), panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                          plot.title = element_text(color="black", size=16, face="bold", hjust=(0.5), vjust=1))+ggtitle("Scandal Condition")


################################### Table (12): OLS Analysis of Treatment Effects by Race

media <- lm(media_score ~ conspiracy+scandal+conspiracy*white, data = data)
article <- lm(article_accuracy ~ conspiracy+scandal+conspiracy*white, data = data)
poltrust <- lm(poltrust_score ~ conspiracy+scandal+conspiracy*white, data = data)
support <- lm(account_score ~ conspiracy+scandal+conspiracy*white, data = data)


media1 <- lm(media_score ~ conspiracy+scandal + scandal*conspiracy*white, data = data)
article1 <- lm(article_accuracy ~ conspiracy+scandal + scandal*conspiracy*white, data = data)
poltrust1 <- lm(poltrust_score ~ conspiracy+scandal + scandal*conspiracy*white, data = data)
support1 <- lm(account_score ~ conspiracy+scandal + scandal*conspiracy*white, data = data)


stargazer(media, article, poltrust, support, media1, article1, poltrust1, support1, align=TRUE,  no.space=TRUE, 
          covariate.labels = c("Conspiracy", "Scandal", "White", "Scandal x Conspiracy", "Scandal x White", "Conspiracy x White", "Conspiracy x Scandal x White", "Control"), 
          keep.stat="n", dep.var.labels = c("\\multirow{2}{3 cm}{ \\centering Trust\\ Info. Providers}", 
                                            "\\multirow{2}{3 cm}{\\centering Article\\ Accuracy}",
                                            "\\multirow{2}{3 cm}{ \\centering Trust\\ Instit.}",
                                            "\\multirow{2}{3 cm}{ \\centering Political\\ Support}",
                                            "\\multirow{2}{3 cm}{ \\centering Trust\\ Info. Providers}", 
                                            "\\multirow{2}{3 cm}{\\centering Article\\ Accuracy}",
                                            "\\multirow{2}{3 cm}{ \\centering Trust\\ Instit.}",
                                            "\\multirow{2}{3 cm}{ \\centering Political\\ Support}"), 
          dep.var.caption="Outcome", digits=2, column.sep.width = "-35pt", header=F)


##################################### Figure (6): Treatment Effects by Religiosity

#Full Sample

m1 <- lm(media_score ~ conspiracy*religiosity, data = data)
m2 <- lm(article_accuracy ~ conspiracy*religiosity, data = data)
m3 <- lm(poltrust_score ~ conspiracy*religiosity, data = data)
m4 <- lm(account_score ~ conspiracy*religiosity, data = data)

est_m1a<- summary(m1)$coefficients[2,1]
est_m1b<- summary(m1)$coefficients[4,1]
est_m2a<- summary(m2)$coefficients[2,1]
est_m2b<- summary(m2)$coefficients[4,1]
est_m3a<- summary(m3)$coefficients[2,1]
est_m3b<- summary(m3)$coefficients[4,1]
est_m4a<- summary(m4)$coefficients[2,1]
est_m4b<- summary(m4)$coefficients[4,1]

se_m1a<- summary(m1)$coefficients[2,2]
se_m1b<- summary(m1)$coefficients[4,2]
se_m2a<- summary(m2)$coefficients[2,2]
se_m2b<- summary(m2)$coefficients[4,2]
se_m3a<- summary(m3)$coefficients[2,2]
se_m3b<- summary(m3)$coefficients[4,2]
se_m4a<- summary(m4)$coefficients[2,2]
se_m4b<- summary(m4)$coefficients[4,2]

estimates <- c(est_m1a,est_m1b,est_m2a,est_m2b,est_m3a,est_m3b,est_m4a,est_m4b)
se <- c(se_m1a,se_m1b,se_m2a,se_m2b,se_m3a,se_m3b,se_m4a,se_m4b)
variable <- c("Conspiracy", "Conspiracy x Religiosity", 
              "Conspiracy", "Conspiracy x Religiosity",
              "Conspiracy", "Conspiracy x Religiosity",
              "Conspiracy", "Conspiracy x Religiosity")

dvs <- c( "Trust in Info. Providers", "Trust in Info. Providers", 
          "Article Accuracy", "Article Accuracy",
          "Trust in Political Inst.", "Trust in Political Inst.",
          "Political Support", "Political Support")

dvs <- factor(dvs, levels=unique(dvs))

figure6_full <-  data.frame(estimates,se, Condition = variable, dvs) %>%
  ggplot(aes(y = estimates, x = dvs, color = Condition, shape = Condition)) +
  geom_point(position = position_dodge(width = .25), cex = 2) +
  geom_errorbar(aes(ymin = estimates - 1.96 *se, ymax = estimates + 1.96 * se), 
                width = 0, position = position_dodge(width = .25)) +  
  geom_errorbar(aes(ymin = estimates - 1.64 *se, ymax = estimates + 1.64 * se), 
                width = 0, position = position_dodge(width = .25), lwd = 1.1) +
  coord_flip()+
  xlab("") +
  ylab("") +
  geom_hline(yintercept = 0, col = "red") + 
  scale_color_manual(values = c("grey", "black")) +
  scale_y_continuous(limits=c(-30,30))+
  theme_minimal() + theme(axis.text=element_text(size=16), 
                          #       axis.title.x = element_text(size=22, face="bold"), 
                          #  plot.title=element_text( hjust=0.6, vjust=1.5, face='bold',size=22),
                          #   axis.title.y = element_text(size=22, face="bold"),
                          legend.title = element_blank(),
                          legend.text = element_text(size = 14), legend.position = "bottom", 
                          panel.border = element_blank(), panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                          plot.title = element_text(color="black", size=16, face="bold", hjust=(0.5), vjust=1))+ggtitle("Full Sample")


#Achievement Condition

m1 <- lm(media_score ~ conspiracy*religiosity, data = data_achievement)
m2 <- lm(article_accuracy ~ conspiracy*religiosity, data = data_achievement)
m3 <- lm(poltrust_score ~ conspiracy*religiosity, data = data_achievement)
m4 <- lm(account_score ~ conspiracy*religiosity, data = data_achievement)

est_m1a<- summary(m1)$coefficients[2,1]
est_m1b<- summary(m1)$coefficients[4,1]
est_m2a<- summary(m2)$coefficients[2,1]
est_m2b<- summary(m2)$coefficients[4,1]
est_m3a<- summary(m3)$coefficients[2,1]
est_m3b<- summary(m3)$coefficients[4,1]
est_m4a<- summary(m4)$coefficients[2,1]
est_m4b<- summary(m4)$coefficients[4,1]

se_m1a<- summary(m1)$coefficients[2,2]
se_m1b<- summary(m1)$coefficients[4,2]
se_m2a<- summary(m2)$coefficients[2,2]
se_m2b<- summary(m2)$coefficients[4,2]
se_m3a<- summary(m3)$coefficients[2,2]
se_m3b<- summary(m3)$coefficients[4,2]
se_m4a<- summary(m4)$coefficients[2,2]
se_m4b<- summary(m4)$coefficients[4,2]

estimates <- c(est_m1a,est_m1b,est_m2a,est_m2b,est_m3a,est_m3b,est_m4a,est_m4b)
se <- c(se_m1a,se_m1b,se_m2a,se_m2b,se_m3a,se_m3b,se_m4a,se_m4b)
condition <- c("Conspiracy", "Conspiracy x Religiosity", 
               "Conspiracy", "Conspiracy x Religiosity",
               "Conspiracy", "Conspiracy x Religiosity",
               "Conspiracy", "Conspiracy x Religiosity")

dvs <- c("Trust in Info. Providers", "Trust in Info. Providers", 
         "Article Accuracy", "Article Accuracy",
         "Trust in Political Inst.", "Trust in Political Inst.",
         "Political Support", "Political Support")

dvs <- factor(dvs, levels=unique(dvs))

figure6_ach <-  data.frame(estimates,se, Condition = variable, dvs) %>%
  ggplot(aes(y = estimates, x = dvs, color = Condition, shape = Condition)) +
  geom_point(position = position_dodge(width = .25), cex = 2) +
  geom_errorbar(aes(ymin = estimates - 1.96 *se, ymax = estimates + 1.96 * se), 
                width = 0, position = position_dodge(width = .25)) +  
  geom_errorbar(aes(ymin = estimates - 1.64 *se, ymax = estimates + 1.64 * se), 
                width = 0, position = position_dodge(width = .25), lwd = 1.1) +
  coord_flip()+
  xlab("") +
  ylab("") +
  geom_hline(yintercept = 0, col = "red") + 
  scale_color_manual(values = c("grey", "black")) +
  scale_y_continuous(limits=c(-30,30))+
  theme_minimal() + theme(axis.text=element_text(size=16), 
                          #       axis.title.x = element_text(size=22, face="bold"), 
                          #  plot.title=element_text( hjust=0.6, vjust=1.5, face='bold',size=22),
                          #   axis.title.y = element_text(size=22, face="bold"),
                          legend.title = element_blank(),
                          legend.text = element_text(size = 14), legend.position = "bottom", 
                          panel.border = element_blank(), panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                          plot.title = element_text(color="black", size=16, face="bold", hjust=(0.5), vjust=1))+ggtitle("Achievement Condition")


#Scandal Condition

m1 <- lm(media_score ~ conspiracy*religiosity, data = data_scandal)
m2 <- lm(article_accuracy ~ conspiracy*religiosity, data = data_scandal)
m3 <- lm(poltrust_score ~ conspiracy*religiosity, data = data_scandal)
m4 <- lm(account_score ~ conspiracy*religiosity, data = data_scandal)

est_m1a<- summary(m1)$coefficients[2,1]
est_m1b<- summary(m1)$coefficients[4,1]
est_m2a<- summary(m2)$coefficients[2,1]
est_m2b<- summary(m2)$coefficients[4,1]
est_m3a<- summary(m3)$coefficients[2,1]
est_m3b<- summary(m3)$coefficients[4,1]
est_m4a<- summary(m4)$coefficients[2,1]
est_m4b<- summary(m4)$coefficients[4,1]

se_m1a<- summary(m1)$coefficients[2,2]
se_m1b<- summary(m1)$coefficients[4,2]
se_m2a<- summary(m2)$coefficients[2,2]
se_m2b<- summary(m2)$coefficients[4,2]
se_m3a<- summary(m3)$coefficients[2,2]
se_m3b<- summary(m3)$coefficients[4,2]
se_m4a<- summary(m4)$coefficients[2,2]
se_m4b<- summary(m4)$coefficients[4,2]

estimates <- c(est_m1a,est_m1b,est_m2a,est_m2b,est_m3a,est_m3b,est_m4a,est_m4b)
se <- c(se_m1a,se_m1b,se_m2a,se_m2b,se_m3a,se_m3b,se_m4a,se_m4b)
condition <- c("Conspiracy", "Conspiracy x Religiosity", 
               "Conspiracy", "Conspiracy x Religiosity",
               "Conspiracy", "Conspiracy x Religiosity",
               "Conspiracy", "Conspiracy x Religiosity")

dvs <- c("Trust in Info. Providers", "Trust in Info. Providers", 
         "Article Accuracy", "Article Accuracy",
         "Trust in Political Inst.", "Trust in Political Inst.",
         "Political Support", "Political Support")

dvs <- factor(dvs, levels=unique(dvs))

figure6_scandal <-  data.frame(estimates,se, Condition = variable, dvs) %>%
  ggplot(aes(y = estimates, x = dvs, color = Condition, shape = Condition)) +
  geom_point(position = position_dodge(width = .25), cex = 2) +
  geom_errorbar(aes(ymin = estimates - 1.96 *se, ymax = estimates + 1.96 * se), 
                width = 0, position = position_dodge(width = .25)) +  
  geom_errorbar(aes(ymin = estimates - 1.64 *se, ymax = estimates + 1.64 * se), 
                width = 0, position = position_dodge(width = .25), lwd = 1.1) +
  coord_flip()+
  xlab("") +
  ylab("") +
  geom_hline(yintercept = 0, col = "red") + 
  scale_color_manual(values = c("grey", "black")) +
  scale_y_continuous(limits=c(-30,30))+
  theme_minimal() + theme(axis.text=element_text(size=16), 
                          #       axis.title.x = element_text(size=22, face="bold"), 
                          #  plot.title=element_text( hjust=0.6, vjust=1.5, face='bold',size=22),
                          #   axis.title.y = element_text(size=22, face="bold"),
                          legend.title = element_blank(),
                          legend.text = element_text(size = 14), legend.position = "bottom", 
                          panel.border = element_blank(), panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                          plot.title = element_text(color="black", size=16, face="bold", hjust=(0.5), vjust=1))+ggtitle("Scandal Condition")



################################### Table (13): OLS Analysis of Treatment Effects by Religiosity

media <- lm(media_score ~ conspiracy+scandal+conspiracy*religiosity, data = data)
article <- lm(article_accuracy ~ conspiracy+scandal+conspiracy*religiosity, data = data)
poltrust <- lm(poltrust_score ~ conspiracy+scandal+conspiracy*religiosity, data = data)
support <- lm(account_score ~ conspiracy+scandal+conspiracy*religiosity, data = data)


media1 <- lm(media_score ~ conspiracy+scandal + scandal*conspiracy*religiosity, data = data)
article1 <- lm(article_accuracy ~ conspiracy+scandal + scandal*conspiracy*religiosity, data = data)
poltrust1 <- lm(poltrust_score ~ conspiracy+scandal + scandal*conspiracy*religiosity, data = data)
support1 <- lm(account_score ~ conspiracy+scandal + scandal*conspiracy*religiosity, data = data)


stargazer(media, article, poltrust, support, media1, article1, poltrust1, support1, align=TRUE,  no.space=TRUE, 
          covariate.labels = c("Conspiracy", "Scandal", "Religiosity", "Scandal x Conspiracy", "Scandal x Religiosity", "Conspiracy x Religiosity", "Consp. x Scandal x Religiosity", "Control"), 
          keep.stat="n", dep.var.labels = c("\\multirow{2}{3 cm}{ \\centering Trust\\ Info. Providers}", 
                                            "\\multirow{2}{3 cm}{\\centering Article\\ Accuracy}",
                                            "\\multirow{2}{3 cm}{ \\centering Trust\\ Instit.}",
                                            "\\multirow{2}{3 cm}{ \\centering Political\\ Support}",
                                            "\\multirow{2}{3 cm}{ \\centering Trust\\ Info. Providers}", 
                                            "\\multirow{2}{3 cm}{\\centering Article\\ Accuracy}",
                                            "\\multirow{2}{3 cm}{ \\centering Trust\\ Instit.}",
                                            "\\multirow{2}{3 cm}{ \\centering Political\\ Support}"), 
          dep.var.caption="Outcome", digits=2, column.sep.width = "-35pt", header=F)


##################################### Figure (7): Treatment Effects by Education (Political knowledge)

m1 <- lm(media_score ~ conspiracy*edu+scandal, data = data)
m2 <- lm(article_accuracy ~ conspiracy*edu+scandal, data = data)
m3 <- lm(poltrust_score ~ conspiracy*edu+scandal, data = data)
m4 <- lm(account_score ~ conspiracy*edu+scandal, data = data)


est_m1a<- summary(m1)$coefficients[2,1]
est_m1b<- summary(m1)$coefficients[4,1]
est_m2a<- summary(m2)$coefficients[2,1]
est_m2b<- summary(m2)$coefficients[4,1]
est_m3a<- summary(m3)$coefficients[2,1]
est_m3b<- summary(m3)$coefficients[4,1]
est_m4a<- summary(m4)$coefficients[2,1]
est_m4b<- summary(m4)$coefficients[4,1]

se_m1a<- summary(m1)$coefficients[2,2]
se_m1b<- summary(m1)$coefficients[4,2]
se_m2a<- summary(m2)$coefficients[2,2]
se_m2b<- summary(m2)$coefficients[4,2]
se_m3a<- summary(m3)$coefficients[2,2]
se_m3b<- summary(m3)$coefficients[4,2]
se_m4a<- summary(m4)$coefficients[2,2]
se_m4b<- summary(m4)$coefficients[4,2]

estimates <- c(est_m1a,est_m1b,est_m2a,est_m2b,est_m3a,est_m3b,est_m4a,est_m4b)
se <- c(se_m1a,se_m1b,se_m2a,se_m2b,se_m3a,se_m3b,se_m4a,se_m4b)
variable <- c("Conspiracy", "Conspiracy x Educ.", 
              "Conspiracy", "Conspiracy x Educ.",
              "Conspiracy", "Conspiracy x Educ.",
              "Conspiracy", "Conspiracy x Educ.")

dvs <- c( "Trust in Info. Providers", "Trust in Info. Providers", 
          "Article Accuracy", "Article Accuracy",
          "Trust in Political Inst.", "Trust in Political Inst.",
          "Political Support", "Political Support")

dvs <- factor(dvs, levels=unique(dvs))

figure7_full <-  data.frame(estimates,se, Condition = variable, dvs) %>%
  ggplot(aes(y = estimates, x = dvs, color = Condition, shape = Condition)) +
  geom_point(position = position_dodge(width = .25), cex = 2) +
  geom_errorbar(aes(ymin = estimates - 1.96 *se, ymax = estimates + 1.96 * se), 
                width = 0, position = position_dodge(width = .25)) +  
  geom_errorbar(aes(ymin = estimates - 1.64 *se, ymax = estimates + 1.64 * se), 
                width = 0, position = position_dodge(width = .25), lwd = 1.1) +
  coord_flip()+
  xlab("") +
  ylab("") +
  geom_hline(yintercept = 0, col = "red") + 
  scale_color_manual(values = c("grey", "black")) +
  scale_y_continuous(limits=c(-30,30))+
  theme_minimal() + theme(axis.text=element_text(size=16), 
                          #       axis.title.x = element_text(size=22, face="bold"), 
                          #  plot.title=element_text( hjust=0.6, vjust=1.5, face='bold',size=22),
                          #   axis.title.y = element_text(size=22, face="bold"),
                          legend.title = element_blank(),
                          legend.text = element_text(size = 14), legend.position = "bottom", 
                          panel.border = element_blank(), panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                          plot.title = element_text(color="black", size=16, face="bold", hjust=(0.5), vjust=1))+ggtitle("Full Sample")


#Achievement Condition

m1 <- lm(media_score ~ conspiracy*edu, data = data_achievement)
m2 <- lm(article_accuracy ~ conspiracy*edu, data = data_achievement)
m3 <- lm(poltrust_score ~ conspiracy*edu, data = data_achievement)
m4 <- lm(account_score ~ conspiracy*edu, data = data_achievement)

est_m1a<- summary(m1)$coefficients[2,1]
est_m1b<- summary(m1)$coefficients[4,1]
est_m2a<- summary(m2)$coefficients[2,1]
est_m2b<- summary(m2)$coefficients[4,1]
est_m3a<- summary(m3)$coefficients[2,1]
est_m3b<- summary(m3)$coefficients[4,1]
est_m4a<- summary(m4)$coefficients[2,1]
est_m4b<- summary(m4)$coefficients[4,1]

se_m1a<- summary(m1)$coefficients[2,2]
se_m1b<- summary(m1)$coefficients[4,2]
se_m2a<- summary(m2)$coefficients[2,2]
se_m2b<- summary(m2)$coefficients[4,2]
se_m3a<- summary(m3)$coefficients[2,2]
se_m3b<- summary(m3)$coefficients[4,2]
se_m4a<- summary(m4)$coefficients[2,2]
se_m4b<- summary(m4)$coefficients[4,2]

estimates <- c(est_m1a,est_m1b,est_m2a,est_m2b,est_m3a,est_m3b,est_m4a,est_m4b)
se <- c(se_m1a,se_m1b,se_m2a,se_m2b,se_m3a,se_m3b,se_m4a,se_m4b)
condition <- c("Conspiracy", "Conspiracy x Educ.", 
               "Conspiracy", "Conspiracy x Educ.",
               "Conspiracy", "Conspiracy x Educ.",
               "Conspiracy", "Conspiracy x Educ.")

dvs <- c("Trust in Info. Providers", "Trust in Info. Providers", 
         "Article Accuracy", "Article Accuracy",
         "Trust in Political Inst.", "Trust in Political Inst.",
         "Political Support", "Political Support")

dvs <- factor(dvs, levels=unique(dvs))

figure7_ach <-  data.frame(estimates,se, Condition = variable, dvs) %>%
  ggplot(aes(y = estimates, x = dvs, color = Condition, shape = Condition)) +
  geom_point(position = position_dodge(width = .25), cex = 2) +
  geom_errorbar(aes(ymin = estimates - 1.96 *se, ymax = estimates + 1.96 * se), 
                width = 0, position = position_dodge(width = .25)) +  
  geom_errorbar(aes(ymin = estimates - 1.64 *se, ymax = estimates + 1.64 * se), 
                width = 0, position = position_dodge(width = .25), lwd = 1.1) +
  coord_flip()+
  xlab("") +
  ylab("") +
  geom_hline(yintercept = 0, col = "red") + 
  scale_color_manual(values = c("grey", "black")) +
  scale_y_continuous(limits=c(-30,30))+
  theme_minimal() + theme(axis.text=element_text(size=16), 
                          #       axis.title.x = element_text(size=22, face="bold"), 
                          #  plot.title=element_text( hjust=0.6, vjust=1.5, face='bold',size=22),
                          #   axis.title.y = element_text(size=22, face="bold"),
                          legend.title = element_blank(),
                          legend.text = element_text(size = 14), legend.position = "bottom", 
                          panel.border = element_blank(), panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                          plot.title = element_text(color="black", size=16, face="bold", hjust=(0.5), vjust=1))+ggtitle("Achievement Condition")


#Scandal Condition

m1 <- lm(media_score ~ conspiracy*edu, data = data_scandal)
m2 <- lm(article_accuracy ~ conspiracy*edu, data = data_scandal)
m3 <- lm(poltrust_score ~ conspiracy*edu, data = data_scandal)
m4 <- lm(account_score ~ conspiracy*edu, data = data_scandal)

est_m1a<- summary(m1)$coefficients[2,1]
est_m1b<- summary(m1)$coefficients[4,1]
est_m2a<- summary(m2)$coefficients[2,1]
est_m2b<- summary(m2)$coefficients[4,1]
est_m3a<- summary(m3)$coefficients[2,1]
est_m3b<- summary(m3)$coefficients[4,1]
est_m4a<- summary(m4)$coefficients[2,1]
est_m4b<- summary(m4)$coefficients[4,1]

se_m1a<- summary(m1)$coefficients[2,2]
se_m1b<- summary(m1)$coefficients[4,2]
se_m2a<- summary(m2)$coefficients[2,2]
se_m2b<- summary(m2)$coefficients[4,2]
se_m3a<- summary(m3)$coefficients[2,2]
se_m3b<- summary(m3)$coefficients[4,2]
se_m4a<- summary(m4)$coefficients[2,2]
se_m4b<- summary(m4)$coefficients[4,2]

estimates <- c(est_m1a,est_m1b,est_m2a,est_m2b,est_m3a,est_m3b,est_m4a,est_m4b)
se <- c(se_m1a,se_m1b,se_m2a,se_m2b,se_m3a,se_m3b,se_m4a,se_m4b)
condition <- c("Conspiracy", "Conspiracy x Educ.", 
               "Conspiracy", "Conspiracy x Educ.",
               "Conspiracy", "Conspiracy x Educ.",
               "Conspiracy", "Conspiracy x Educ.")

dvs <- c("Trust in Info. Providers", "Trust in Info. Providers", 
         "Article Accuracy", "Article Accuracy",
         "Trust in Political Inst.", "Trust in Political Inst.",
         "Political Support", "Political Support")

dvs <- factor(dvs, levels=unique(dvs))

figure7_scandal <-  data.frame(estimates,se, Condition = variable, dvs) %>%
  ggplot(aes(y = estimates, x = dvs, color = Condition, shape = Condition)) +
  geom_point(position = position_dodge(width = .25), cex = 2) +
  geom_errorbar(aes(ymin = estimates - 1.96 *se, ymax = estimates + 1.96 * se), 
                width = 0, position = position_dodge(width = .25)) +  
  geom_errorbar(aes(ymin = estimates - 1.64 *se, ymax = estimates + 1.64 * se), 
                width = 0, position = position_dodge(width = .25), lwd = 1.1) +
  coord_flip()+
  xlab("") +
  ylab("") +
  geom_hline(yintercept = 0, col = "red") + 
  scale_color_manual(values = c("grey", "black")) +
  scale_y_continuous(limits=c(-30,30))+
  theme_minimal() + theme(axis.text=element_text(size=16), 
                          #       axis.title.x = element_text(size=22, face="bold"), 
                          #  plot.title=element_text( hjust=0.6, vjust=1.5, face='bold',size=22),
                          #   axis.title.y = element_text(size=22, face="bold"),
                          legend.title = element_blank(),
                          legend.text = element_text(size = 14), legend.position = "bottom", 
                          panel.border = element_blank(), panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                          plot.title = element_text(color="black", size=16, face="bold", hjust=(0.5), vjust=1))+ggtitle("Scandal Condition")


###################################### Table (14): OLS Analysis of Treatment Effects by Political Knowledge (Educational Attainment)

media <- lm(media_score ~ conspiracy+scandal+conspiracy*edu, data = data)
article <- lm(article_accuracy ~ conspiracy+scandal+conspiracy*edu, data = data)
poltrust <- lm(poltrust_score ~ conspiracy+scandal+conspiracy*edu, data = data)
support <- lm(account_score ~ conspiracy+scandal+conspiracy*edu, data = data)


media1 <- lm(media_score ~ conspiracy+scandal + scandal*conspiracy*edu, data = data)
article1 <- lm(article_accuracy ~ conspiracy+scandal + scandal*conspiracy*edu, data = data)
poltrust1 <- lm(poltrust_score ~ conspiracy+scandal + scandal*conspiracy*edu, data = data)
support1 <- lm(account_score ~ conspiracy+scandal + scandal*conspiracy*edu, data = data)


stargazer(media, article, poltrust, support, media1, article1, poltrust1, support1, align=TRUE,  no.space=TRUE, 
          covariate.labels = c("Conspiracy", "Scandal", "Educ.", "Scandal x Conspiracy", "Scandal x Educ.", "Conspiracy x Educ.", "Consp. x Scandal x Educ.", "Control"), 
          keep.stat="n", dep.var.labels = c("\\multirow{2}{3 cm}{ \\centering Trust\\ Info. Providers}", 
                                            "\\multirow{2}{3 cm}{\\centering Article\\ Accuracy}",
                                            "\\multirow{2}{3 cm}{ \\centering Trust\\ Instit.}",
                                            "\\multirow{2}{3 cm}{ \\centering Political\\ Support}",
                                            "\\multirow{2}{3 cm}{ \\centering Trust\\ Info. Providers}", 
                                            "\\multirow{2}{3 cm}{\\centering Article\\ Accuracy}",
                                            "\\multirow{2}{3 cm}{ \\centering Trust\\ Instit.}",
                                            "\\multirow{2}{3 cm}{ \\centering Political\\ Support}"), 
          dep.var.caption="Outcome", digits=2, column.sep.width = "-35pt", header=F)



################################### Figure (8): Treatment Effects by Strength of Partisanship 

#Full Sample

m1 <- lm(media_score ~ conspiracy*strongpid, data = data)
m2 <- lm(article_accuracy ~ conspiracy*strongpid, data = data)
m3 <- lm(poltrust_score ~ conspiracy*strongpid, data = data)
m4 <- lm(account_score ~ conspiracy*strongpid, data = data)

est_m1a<- summary(m1)$coefficients[2,1]
est_m1b<- summary(m1)$coefficients[4,1]
est_m2a<- summary(m2)$coefficients[2,1]
est_m2b<- summary(m2)$coefficients[4,1]
est_m3a<- summary(m3)$coefficients[2,1]
est_m3b<- summary(m3)$coefficients[4,1]
est_m4a<- summary(m4)$coefficients[2,1]
est_m4b<- summary(m4)$coefficients[4,1]

se_m1a<- summary(m1)$coefficients[2,2]
se_m1b<- summary(m1)$coefficients[4,2]
se_m2a<- summary(m2)$coefficients[2,2]
se_m2b<- summary(m2)$coefficients[4,2]
se_m3a<- summary(m3)$coefficients[2,2]
se_m3b<- summary(m3)$coefficients[4,2]
se_m4a<- summary(m4)$coefficients[2,2]
se_m4b<- summary(m4)$coefficients[4,2]

estimates <- c(est_m1a,est_m1b,est_m2a,est_m2b,est_m3a,est_m3b,est_m4a,est_m4b)
se <- c(se_m1a,se_m1b,se_m2a,se_m2b,se_m3a,se_m3b,se_m4a,se_m4b)
variable <- c("Conspiracy", "Conspiracy x Partisan", 
              "Conspiracy", "Conspiracy x Partisan",
              "Conspiracy", "Conspiracy x Partisan",
              "Conspiracy", "Conspiracy x Partisan")

dvs <- c( "Trust in Info. Providers", "Trust in Info. Providers", 
          "Article Accuracy", "Article Accuracy",
          "Trust in Political Inst.", "Trust in Political Inst.",
          "Political Support", "Political Support")

dvs <- factor(dvs, levels=unique(dvs))

figure8_full <-  data.frame(estimates,se, Condition = variable, dvs) %>%
  ggplot(aes(y = estimates, x = dvs, color = Condition, shape = Condition)) +
  geom_point(position = position_dodge(width = .25), cex = 2) +
  geom_errorbar(aes(ymin = estimates - 1.96 *se, ymax = estimates + 1.96 * se), 
                width = 0, position = position_dodge(width = .25)) +  
  geom_errorbar(aes(ymin = estimates - 1.64 *se, ymax = estimates + 1.64 * se), 
                width = 0, position = position_dodge(width = .25), lwd = 1.1) +
  coord_flip()+
  xlab("") +
  ylab("") +
  geom_hline(yintercept = 0, col = "red") + 
  scale_color_manual(values = c("grey", "black")) +
  scale_y_continuous(limits=c(-30,30))+
  theme_minimal() + theme(axis.text=element_text(size=16), 
                          #       axis.title.x = element_text(size=22, face="bold"), 
                          #  plot.title=element_text( hjust=0.6, vjust=1.5, face='bold',size=22),
                          #   axis.title.y = element_text(size=22, face="bold"),
                          legend.title = element_blank(),
                          legend.text = element_text(size = 14), legend.position = "bottom", 
                          panel.border = element_blank(), panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                          plot.title = element_text(color="black", size=16, face="bold", hjust=(0.5), vjust=1))+ggtitle("Full Sample")


#Achievement condition

m1 <- lm(media_score ~ conspiracy*strongpid, data = data_achievement)
m2 <- lm(article_accuracy ~ conspiracy*strongpid, data = data_achievement)
m3 <- lm(poltrust_score ~ conspiracy*strongpid, data = data_achievement)
m4 <- lm(account_score ~ conspiracy*strongpid, data = data_achievement)

est_m1a<- summary(m1)$coefficients[2,1]
est_m1b<- summary(m1)$coefficients[4,1]
est_m2a<- summary(m2)$coefficients[2,1]
est_m2b<- summary(m2)$coefficients[4,1]
est_m3a<- summary(m3)$coefficients[2,1]
est_m3b<- summary(m3)$coefficients[4,1]
est_m4a<- summary(m4)$coefficients[2,1]
est_m4b<- summary(m4)$coefficients[4,1]

se_m1a<- summary(m1)$coefficients[2,2]
se_m1b<- summary(m1)$coefficients[4,2]
se_m2a<- summary(m2)$coefficients[2,2]
se_m2b<- summary(m2)$coefficients[4,2]
se_m3a<- summary(m3)$coefficients[2,2]
se_m3b<- summary(m3)$coefficients[4,2]
se_m4a<- summary(m4)$coefficients[2,2]
se_m4b<- summary(m4)$coefficients[4,2]

estimates <- c(est_m1a,est_m1b,est_m2a,est_m2b,est_m3a,est_m3b,est_m4a,est_m4b)
se <- c(se_m1a,se_m1b,se_m2a,se_m2b,se_m3a,se_m3b,se_m4a,se_m4b)
condition <- c("Conspiracy", "Conspiracy x Partisan", 
               "Conspiracy", "Conspiracy x Partisan",
               "Conspiracy", "Conspiracy x Partisan",
               "Conspiracy", "Conspiracy x Partisan")

dvs <- c("Trust in Info. Providers", "Trust in Info. Providers", 
         "Article Accuracy", "Article Accuracy",
         "Trust in Political Inst.", "Trust in Political Inst.",
         "Political Support", "Political Support")

dvs <- factor(dvs, levels=unique(dvs))

figure8_ach <-  data.frame(estimates,se, Condition = variable, dvs) %>%
  ggplot(aes(y = estimates, x = dvs, color = Condition, shape = Condition)) +
  geom_point(position = position_dodge(width = .25), cex = 2) +
  geom_errorbar(aes(ymin = estimates - 1.96 *se, ymax = estimates + 1.96 * se), 
                width = 0, position = position_dodge(width = .25)) +  
  geom_errorbar(aes(ymin = estimates - 1.64 *se, ymax = estimates + 1.64 * se), 
                width = 0, position = position_dodge(width = .25), lwd = 1.1) +
  coord_flip()+
  xlab("") +
  ylab("") +
  geom_hline(yintercept = 0, col = "red") + 
  scale_color_manual(values = c("grey", "black")) +
  scale_y_continuous(limits=c(-30,30))+
  theme_minimal() + theme(axis.text=element_text(size=16), 
                          #       axis.title.x = element_text(size=22, face="bold"), 
                          #  plot.title=element_text( hjust=0.6, vjust=1.5, face='bold',size=22),
                          #   axis.title.y = element_text(size=22, face="bold"),
                          legend.title = element_blank(),
                          legend.text = element_text(size = 14), legend.position = "bottom", 
                          panel.border = element_blank(), panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                          plot.title = element_text(color="black", size=16, face="bold", hjust=(0.5), vjust=1))+ggtitle("Achievement Condition")


#Scandal Condition

m1 <- lm(media_score ~ conspiracy*strongpid, data = data_scandal)
m2 <- lm(article_accuracy ~ conspiracy*strongpid, data = data_scandal)
m3 <- lm(poltrust_score ~ conspiracy*strongpid, data = data_scandal)
m4 <- lm(account_score ~ conspiracy*strongpid, data = data_scandal)

est_m1a<- summary(m1)$coefficients[2,1]
est_m1b<- summary(m1)$coefficients[4,1]
est_m2a<- summary(m2)$coefficients[2,1]
est_m2b<- summary(m2)$coefficients[4,1]
est_m3a<- summary(m3)$coefficients[2,1]
est_m3b<- summary(m3)$coefficients[4,1]
est_m4a<- summary(m4)$coefficients[2,1]
est_m4b<- summary(m4)$coefficients[4,1]

se_m1a<- summary(m1)$coefficients[2,2]
se_m1b<- summary(m1)$coefficients[4,2]
se_m2a<- summary(m2)$coefficients[2,2]
se_m2b<- summary(m2)$coefficients[4,2]
se_m3a<- summary(m3)$coefficients[2,2]
se_m3b<- summary(m3)$coefficients[4,2]
se_m4a<- summary(m4)$coefficients[2,2]
se_m4b<- summary(m4)$coefficients[4,2]

estimates <- c(est_m1a,est_m1b,est_m2a,est_m2b,est_m3a,est_m3b,est_m4a,est_m4b)
se <- c(se_m1a,se_m1b,se_m2a,se_m2b,se_m3a,se_m3b,se_m4a,se_m4b)
condition <- c("Conspiracy", "Conspiracy x Partisan", 
               "Conspiracy", "Conspiracy x Partisan",
               "Conspiracy", "Conspiracy x Partisan",
               "Conspiracy", "Conspiracy x Partisan")

dvs <- c("Trust in Info. Providers", "Trust in Info. Providers", 
         "Article Accuracy", "Article Accuracy",
         "Trust in Political Inst.", "Trust in Political Inst.",
         "Political Support", "Political Support")

dvs <- factor(dvs, levels=unique(dvs))

figure8_scandal <-  data.frame(estimates,se, Condition = variable, dvs) %>%
  ggplot(aes(y = estimates, x = dvs, color = Condition, shape = Condition)) +
  geom_point(position = position_dodge(width = .25), cex = 2) +
  geom_errorbar(aes(ymin = estimates - 1.96 *se, ymax = estimates + 1.96 * se), 
                width = 0, position = position_dodge(width = .25)) +  
  geom_errorbar(aes(ymin = estimates - 1.64 *se, ymax = estimates + 1.64 * se), 
                width = 0, position = position_dodge(width = .25), lwd = 1.1) +
  coord_flip()+
  xlab("") +
  ylab("") +
  geom_hline(yintercept = 0, col = "red") + 
  scale_color_manual(values = c("grey", "black")) +
  scale_y_continuous(limits=c(-30,30))+
  theme_minimal() + theme(axis.text=element_text(size=16), 
                          #       axis.title.x = element_text(size=22, face="bold"), 
                          #  plot.title=element_text( hjust=0.6, vjust=1.5, face='bold',size=22),
                          #   axis.title.y = element_text(size=22, face="bold"),
                          legend.title = element_blank(),
                          legend.text = element_text(size = 14), legend.position = "bottom", 
                          panel.border = element_blank(), panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                          plot.title = element_text(color="black", size=16, face="bold", hjust=(0.5), vjust=1))+ggtitle("Scandal Condition")


###################################### Table (15): OLS Analysis of Treatment Effects by the Strength of Partisanship

media <- lm(media_score ~ conspiracy+scandal+conspiracy*strongpid, data = data)
article <- lm(article_accuracy ~ conspiracy+scandal+conspiracy*strongpid, data = data)
poltrust <- lm(poltrust_score ~ conspiracy+scandal+conspiracy*strongpid, data = data)
support <- lm(account_score ~ conspiracy+scandal+conspiracy*strongpid, data = data)


media1 <- lm(media_score ~ conspiracy+scandal + scandal*conspiracy*strongpid, data = data)
article1 <- lm(article_accuracy ~ conspiracy+scandal + scandal*conspiracy*strongpid, data = data)
poltrust1 <- lm(poltrust_score ~ conspiracy+scandal + scandal*conspiracy*strongpid, data = data)
support1 <- lm(account_score ~ conspiracy+scandal + scandal*conspiracy*strongpid, data = data)


stargazer(media, article, poltrust, support, media1, article1, poltrust1, support1, align=TRUE,  no.space=TRUE, 
          covariate.labels = c("Conspiracy", "Scandal", "Partisan", "Scandal x Conspiracy", "Scandal x Partisan", "Conspiracy x Partisan", "Consp. x Scandal x Partisan", "Control"), 
          keep.stat="n", dep.var.labels = c("\\multirow{2}{3 cm}{ \\centering Trust\\ Info. Providers}", 
                                            "\\multirow{2}{3 cm}{\\centering Article\\ Accuracy}",
                                            "\\multirow{2}{3 cm}{ \\centering Trust\\ Instit.}",
                                            "\\multirow{2}{3 cm}{ \\centering Political\\ Support}",
                                            "\\multirow{2}{3 cm}{ \\centering Trust\\ Info. Providers}", 
                                            "\\multirow{2}{3 cm}{\\centering Article\\ Accuracy}",
                                            "\\multirow{2}{3 cm}{ \\centering Trust\\ Instit.}",
                                            "\\multirow{2}{3 cm}{ \\centering Political\\ Support}"), 
          dep.var.caption="Outcome", digits=2, column.sep.width = "-35pt", header=F)



############################# Table (16): Treatment Effect on Believing in Conspiracy Theories
consp <- lm(consp_score ~ conspiracy+scandal, data = data)
consp1 <- lm(consp_score ~ conspiracy+scandal + scandal*conspiracy, data = data)

stargazer(consp, consp1, align=TRUE,  no.space=TRUE, 
          covariate.labels = c("Conspiracy", "Scandal", "Scandal x Conspiracy", "Control"), 
          keep.stat="n", dep.var.caption="Outcome", digits=2, column.sep.width = "-35pt", header=F)


################################## Note: Construction of Indices ####################################
#The indices were constructed using the following functions
#####################################################################################################

#Political support (accountability) index
items <- data[, c("investigation", "performance", "vote", "no_impeach")]
scaleKey <- c(1,1,1,1)
results <- scoreItems(keys = scaleKey, items = items, 
                      totals = FALSE, missing = FALSE, min = 0, 
                      max = 100)
data$account_score <- results$scores


#Trust in media index
items <- data[, c("google", "social_media", "liberal_media", "consv_media", "gov_tanks", "nongov_tanks", "alt_media","academic")]
scaleKey <- c(1,1,1,1, 1, 1, 1,1)
results <- scoreItems(keys = scaleKey, items = items, 
                      totals = FALSE, missing = FALSE, min = 0, 
                      max = 100)
data$media_score <- results$scores

#Trust in political institutions index
items <- data[, c("fbi", "cia", "rep_lead", "dem_lead", "courts", "sup_courts")]
scaleKey <- c(1,1,1,1, 1, 1)
results <- scoreItems(keys = scaleKey, items = items, 
                      totals = FALSE, missing = FALSE, min = 0, 
                      max = 100)
data$poltrust_score <- results$scores


#Beliefs in conspiracy theories index
items <- data[, c("consp_obama", "consp_jew", "consp_911", "consp_pharma", "consp_moon", "consp_flat")]
scaleKey <- c(1,1,1,1, 1, 1)
results <- scoreItems(keys = scaleKey, items = items, 
                      totals = FALSE, missing = FALSE, min = 0, 
                      max = 100)
data$consp_score <- results$scores

