##############
#Setup
########

#set working directory
##change working directory to location of pre-loaded data
setwd("Your_Working_Directory_Here")

#load cjoint package, which is used for conjoint analysis
##uncomment line below to install package (only need to install once)
#install.packages("cjoint")
library(cjoint)

#read in main dataset
load("conjoint.RData")

#read in conjoint design
load("cjoint_design.RData")

#combine originalist philosophies into "Originalism"
dat$Philosophy2 <- as.character(dat$Philosophy)
dat$Philosophy2[dat$Philosophy2=="Original intent" | dat$Philosophy2=="Original meaning"] <- "Originalism"
dat$Philosophy2 <- factor(dat$Philosophy2)

#read in conjoint design separating original intent and original meaning
load("cjoint_design2.RData")

#######################################
#Analysis
######################

#indicate baselines for AMCEs
baselines <- list()
baselines$Ideology <- "Moderate"
baselines$Position <- "Attorney in private practice"
baselines$Religion<- "Not Religious"
baselines$Philosophy2 <- "Living document"
baselines$Sex <- "Male"
baselines$Race <- "White"
baselines$Rating <- "Not Qualified"

#estimate AMCEs (See Figure 1 and Figure 2)
summary(model_gen <- amce(Preferred~Age+Rating+Held_Office+Position+Legal_Education+
                            pid3*(Sex+Race+Religion+Philosophy2+Party+Ideology),
                          data=dat,design=design_adj,na.ignore=TRUE,
                          respondent.id="id",cluster=TRUE,
                          weights="weight",baselines=baselines,
                          respondent.varying="pid3"))

#reestimate AMCEs using alternative baselines (See Figure 1 and Figure 2)
baselines$Ideology <- "Conservative"
baselines$Philosophy2 <- "Precedent"
baselines$Rating <- "Qualified"
summary(model_gen <- amce(Preferred~Age+Rating+Held_Office+Position+Legal_Education+
                            pid3*(Sex+Race+Religion+Philosophy2+Party+Ideology),
                          data=dat,design=design_adj,na.ignore=TRUE,
                          respondent.id="id",cluster=TRUE,
                          weights="weight",baselines=baselines,
                          respondent.varying="pid3"))

#canned plot (see Section D of Appendix)
plot(model_gen,plot.display="unconditional")

#canned plot (see Section E of Appendix)
facet.levels1 <- list()
facet.levels1[["pid3"]] <- c("Democrat","Independent","Republican")
plot(model_gen,plot.display="interaction",
     facet.names="pid3",facet.levels=facet.levels1)

#correlation between partisanship and ideology (see Section I of Appendix)
dat$ideo5[dat$ideo5==6] <- NA
dat$pid7[dat$pid7==8] <- NA
cor(as.numeric(dat$ideo5),as.numeric(dat$pid7),use="pairwise.complete.obs")
gray <- adjustcolor("black",alpha.f=.25)
plot(jitter(as.numeric(dat$pid7)),jitter(as.numeric(dat$ideo5)),col=gray,pch=19,
     xlab="Partisanship (Strong Democrat to Strong Republican)",
     ylab="Ideology (Very Liberal to Very Conservative)")
text(5.5,1,labels = "Correlation = .69",font=4)

#categorize 5-point ideology scale into liberals, moderates, and conservatives
dat$ideo <- as.numeric(as.character(dat$ideo5))
dat$ideo[dat$ideo<3] <- "Liberal"
dat$ideo[dat$ideo==3] <- "Moderate"
dat$ideo[dat$ideo==4 | dat$ideo==5] <- "Conservative"
dat$ideo <- factor(dat$ideo)

#substitute partisanship for ideology (see Section I of Appendix)
baselines <- list()
baselines$Ideology <- "Moderate"
baselines$Position <- "Attorney in private practice"
baselines$Religion<- "Not Religious"
baselines$Philosophy2 <- "Living document"
baselines$Sex <- "Male"
baselines$Race <- "White"
baselines$Rating <- "Not Qualified"
summary(model_gen <- amce(Preferred~Age+Rating+Held_Office+Position+Legal_Education+
                            ideo*(Sex+Race+Religion+Philosophy2+Party+Ideology),
                          data=dat,design=design_adj,na.ignore=TRUE,
                          respondent.id="id",cluster=TRUE,
                          weights="weight",baselines=baselines,
                          respondent.varying="ideo"))
#note that we substituted ideo for pid3 in the model

#reestimate using alternative baselines (See Section I of Appendix)
baselines$Ideology <- "Conservative"
baselines$Philosophy2 <- "Precedent"
baselines$Rating <- "Qualified"
summary(model_gen <- amce(Preferred~Age+Rating+Held_Office+Position+Legal_Education+
                            ideo*(Sex+Race+Religion+Philosophy2+Party+Ideology),
                          data=dat,design=design_adj,na.ignore=TRUE,
                          respondent.id="id",cluster=TRUE,
                          weights="weight",baselines=baselines,
                          respondent.varying="ideo"))

#comparing original intent and original meaning (see Section C of Appendix)
##note: using Philosophy (instead of Philosophy2 variable)
##note: using design_adj_orig (instead of design_adj)
baselines$Philosophy <- "Living document"
summary(model_gen <- amce(Preferred~Age+Rating+Held_Office+Position+Legal_Education+
                            pid3*(Sex+Race+Religion+Philosophy+Party+Ideology),
                          data=dat,design=design_adj_orig,na.ignore=TRUE,
                          respondent.id="id",cluster=TRUE,
                          weights="weight",baselines=baselines,
                          respondent.varying="pid3"))

#Regression Analysis (see Figures 3 and 4 in main text, and Section H of Appendix)
##subset data to key judicial philosophies and main three parties; ignores other data
dat <- dat[dat$pid3!="Other" & dat$pid3!="Not Sure" & 
             (dat$Philosophy2=="Originalism" | dat$Philosophy2=="Living document" | dat$Philosophy2=="Precedent"),]
dat$Ideology <- relevel(dat$Ideology,ref="Moderate")
dat$pid3 <- relevel(dat$pid3,ref="Democrat")
summary(glm(Preferred~Rating+Ideology+pid3*Party*Philosophy2,
            data=dat,family=binomial))

#####################################
#Ratings analysis (instead of forced choice)
load("conjoint2.RData")

#indicate baselines for AMCEs
baselines <- list()
baselines$Ideology <- "Moderate"
baselines$Position <- "Attorney in private practice"
baselines$Religion<- "Not Religious"
baselines$Philosophy <- "Living document"
baselines$Sex <- "Male"
baselines$Race <- "White"

#estimate AMCEs
summary(model_gen <- amce(Score~Age+Rating+Held_Office+Position+Legal_Education+
                            pid3*(Sex+Race+Religion+Philosophy+Party+Ideology),
                          data=dat2,design=design_adj_orig,na.ignore=TRUE,
                          respondent.id="id",cluster=TRUE,
                          weights="weight",baselines=baselines,
                          respondent.varying="pid3"))
#note that we are using design_adj_orig (separates out originalist philosophies)

#canned plot (see section F of Appendix)
plot(model_gen,plot.display="unconditional",text.size=8)

#canned plot (see section G of Appendix)
facet.levels1 <- list()
facet.levels1[["pid3"]] <- c("Democrat","Independent","Republican")
plot(model_gen,plot.display="interaction",
     facet.names="pid3",facet.levels=facet.levels1)

