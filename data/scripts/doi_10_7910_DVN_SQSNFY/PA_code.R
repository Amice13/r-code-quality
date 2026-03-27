## Replication code for:
## The Effect of Human vs. Automated Interaction on Willingness to Participate in Government Programs: The Role of Representation
## Public Administration

## Note: Required packages to be installed prior to running code below: survey, ggplot2, ggpubr, emmeans

library(survey)
library(ggplot2)
library(ggpubr)
library(emmeans)

PAdata <- read.csv("insert path")

###level and labels
PAdata$envirSUPPORT <- factor(PAdata$envirSUPPORT,
                              levels = c("0", "1"),
                              labels = c("Oppose", "Support"))

PAdata$Group <- factor(PAdata$Group,
                       levels = c("3", "2", "1"),
                       labels = c("Automated\nSystem", "Male\nEmployee", "Female\nEmployee"))

PAdata$female <- factor(PAdata$female,
                        levels = c("0", "1"),
                        labels = c("Male Resp", "Female Resp"))

survey <- svydesign(ids = ~1, weights= ~teamweight, data = PAdata)

######Female respondents by group

svyby(~female, ~Group,design=survey, svyciprop, vartype="ci")
svychisq(~Group+female, survey, statistic="Chisq")

prop.table(table(PAdata$female, PAdata$Group), margin = 2)
chisq.test(PAdata$female, PAdata$Group)

###Democratic respondents by group

svyby(~Dem, ~Group,design=survey, svyciprop, vartype="ci")
svychisq(~Group+Dem, survey, statistic="Chisq")

prop.table(table(PAdata$Dem, PAdata$Group), margin = 2)
chisq.test(PAdata$Dem, PAdata$Group)

###Republican respondents by group

svyby(~Rep, ~Group,design=survey, svyciprop, vartype="ci")
svychisq(~Group+Rep, survey, statistic="Chisq")

prop.table(table(PAdata$Rep, PAdata$Group), margin = 2)
chisq.test(PAdata$Rep, PAdata$Group)

###Respondent Age by group

svyby(~age, ~Group,design=survey, svymean, vartype="ci")
summary(svyglm(age~Group, survey))

aggregate(PAdata$age, list(PAdata$Group), FUN=mean)
summary(glm(age~Group, data=PAdata))

###Black respondents by group

svyby(~BlackResp, ~Group,design=survey, svyciprop, vartype="ci")
svychisq(~Group+BlackResp, survey, statistic="Chisq")

prop.table(table(PAdata$BlackResp, PAdata$Group), margin = 2)
chisq.test(PAdata$BlackResp, PAdata$Group)

###Hispanic respondents by group

svyby(~HispResp, ~Group,design=survey, svyciprop, vartype="ci")
svychisq(~Group+HispResp, survey, statistic="Chisq")

prop.table(table(PAdata$HispResp, PAdata$Group), margin = 2)
chisq.test(PAdata$HispResp, PAdata$Group)

###Respondents with College Degree by group

svyby(~college, ~Group,design=survey, svyciprop, vartype="ci")
svychisq(~Group+college, survey, statistic="Chisq")

prop.table(table(PAdata$college, PAdata$Group), margin = 2)
chisq.test(PAdata$college, PAdata$Group)

###Homeowner respondents by group

svyby(~homeowner, ~Group,design=survey, svyciprop, vartype="ci")
svychisq(~Group+homeowner, survey, statistic="Chisq")

prop.table(table(PAdata$homeowner, PAdata$Group), margin = 2)
chisq.test(PAdata$homeowner, PAdata$Group)

##Models: Gender 
###Participation (0-100)
model1 <- glm(partic100 ~ Group*female, data=PAdata)
summary(model1)
a2 <- emmeans(model1, ~ Group*female)

G1 = c(1, 0, 0, 0, 0, 0)
G2 = c(0, 1, 0, 0, 0, 0)
G3 = c(0, 0, 1, 0, 0, 0)
G4 = c(0, 0, 0, 1, 0, 0)
G5 = c(0, 0, 0, 0, 1, 0)
G6 = c(0, 0, 0, 0, 0, 1)

fig2apr <- contrast(a2, method = list(G1 - G1, G2 - G1, G3 - G1, G4 - G4, G5 - G4, G6 - G4), infer = TRUE)
fig2a <- as.data.frame(fig2apr) 
fig2a$gender <- c("Male Resp", "Male Resp", "Male Resp", "Female Resp", "Female Resp", "Female Resp")
fig2a$group <- c("Automated \nSystem", "Male \nEmployee", "Female \nEmployee", "Automated \nSystem", "Male \nEmployee", "Female \nEmployee")

model2 <- svyglm(partic100 ~ Group*female, design=survey)
summary(model2)
b2 <- emmeans(model2, ~ Group*female)

G1 = c(1, 0, 0, 0, 0, 0)
G2 = c(0, 1, 0, 0, 0, 0)
G3 = c(0, 0, 1, 0, 0, 0)
G4 = c(0, 0, 0, 1, 0, 0)
G5 = c(0, 0, 0, 0, 1, 0)
G6 = c(0, 0, 0, 0, 0, 1)

fig2bpr <- contrast(b2, method = list(G1 - G1, G2 - G1, G3 - G1, G4 - G4, G5 - G4, G6 - G4), infer = TRUE)
fig2b <- as.data.frame(fig2bpr) 
fig2b$gender <- c("Male Resp", "Male Resp", "Male Resp", "Female Resp", "Female Resp", "Female Resp")
fig2b$group <- c("Automated \nSystem", "Male \nEmployee", "Female \nEmployee", "Automated \nSystem", "Male \nEmployee", "Female \nEmployee")

out <- rbind(fig2a, fig2b)
out$notweighted <- c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
out$notweighted <- factor(out$notweighted,
                          levels = c("0", "1"),
                          labels = c("Weighted", "Not Weighted"))
out$group <- factor(out$group,
                    levels = c("Automated \nSystem", "Male \nEmployee", "Female \nEmployee"),
                    labels = c("Automated \nSystem", "Male \nEmployee", "Female \nEmployee"))

fig2ab <- ggplot(data = out, aes(x = estimate, y = group, color=group, shape = notweighted)) +
  geom_vline(xintercept = 0) +
  geom_point(position=position_dodge2(.25), size=2.5) + xlab("Difference in Willingness to Participate") + ylab("") +
  geom_linerange(aes(xmin = lower.CL , xmax = upper.CL), position=position_dodge2(.25)) +
  facet_wrap(~gender) +
  theme_bw() + ggtitle ("Willingness to Participate (0-100)") + theme(legend.position="bottom", 
                                                                      legend.title = element_blank(),
                                                                      plot.title = element_text(size=11),
                                                                      axis.title=element_text(size=8)) +
  guides(color="none") +
  scale_color_manual(values=c("turquoise4", "tomato3", "dodgerblue4")) + labs(color="")

print(fig2ab)        

###Participation (0-5)

model3 <- glm(partic5 ~ Group*female, data=PAdata)
summary(model3)
c2 <- emmeans(model3, ~ Group*female)

G1 = c(1, 0, 0, 0, 0, 0)
G2 = c(0, 1, 0, 0, 0, 0)
G3 = c(0, 0, 1, 0, 0, 0)
G4 = c(0, 0, 0, 1, 0, 0)
G5 = c(0, 0, 0, 0, 1, 0)
G6 = c(0, 0, 0, 0, 0, 1)

fig2cpr <- contrast(c2, method = list(G1 - G1, G2 - G1, G3 - G1, G4 - G4, G5 - G4, G6 - G4), infer = TRUE)
fig2c <- as.data.frame(fig2cpr) 
fig2c$gender <- c("Male Resp", "Male Resp", "Male Resp", "Female Resp", "Female Resp", "Female Resp")
fig2c$group <- c("Automated \nSystem", "Male \nEmployee", "Female \nEmployee", "Automated \nSystem", "Male \nEmployee", "Female \nEmployee")

model4 <- svyglm(partic5 ~ Group*female, design=survey)
summary(model4)
d2 <- emmeans(model4, ~ Group*female)

G1 = c(1, 0, 0, 0, 0, 0)
G2 = c(0, 1, 0, 0, 0, 0)
G3 = c(0, 0, 1, 0, 0, 0)
G4 = c(0, 0, 0, 1, 0, 0)
G5 = c(0, 0, 0, 0, 1, 0)
G6 = c(0, 0, 0, 0, 0, 1)

fig2dpr <- contrast(d2, method = list(G1 - G1, G2 - G1, G3 - G1, G4 - G4, G5 - G4, G6 - G4), infer = TRUE)
fig2d <- as.data.frame(fig2dpr) 
fig2d$gender <- c("Male Resp", "Male Resp", "Male Resp", "Female Resp", "Female Resp", "Female Resp")
fig2d$group <- c("Automated \nSystem", "Male \nEmployee", "Female \nEmployee", "Automated \nSystem", "Male \nEmployee", "Female \nEmployee")

out <- rbind(fig2c, fig2d)
out$notweighted <- c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
out$notweighted <- factor(out$notweighted,
                          levels = c("0", "1"),
                          labels = c("Weighted", "Not Weighted"))
out$group <- factor(out$group,
                    levels = c("Automated \nSystem", "Male \nEmployee", "Female \nEmployee"),
                    labels = c("Automated \nSystem", "Male \nEmployee", "Female \nEmployee"))

fig2cd <- ggplot(data = out, aes(x = estimate, y = group, color=group, shape = notweighted)) +
  geom_vline(xintercept = 0) +
  geom_point(position=position_dodge2(.25), size=2.5) + xlab("Difference in Willingness to Participate") + ylab("") +
  geom_linerange(aes(xmin = lower.CL , xmax = upper.CL), position=position_dodge2(.25)) +
  facet_wrap(~gender) +
  theme_bw() + ggtitle ("Willingness to Participate (0-5)") + theme(legend.position="bottom", 
                                                                    legend.title = element_blank(),
                                                                    plot.title = element_text(size=11),
                                                                    axis.title=element_text(size=8)) +
  guides(color="none") +
  scale_color_manual(values=c("turquoise4", "tomato3", "dodgerblue4")) + labs(color="")

print(fig2cd)        

######Participation (0, 1)

model5 <- glm(partic1 ~ Group*female, data=PAdata)
summary(model5)
e2 <- emmeans(model5, ~ Group*female)

G1 = c(1, 0, 0, 0, 0, 0)
G2 = c(0, 1, 0, 0, 0, 0)
G3 = c(0, 0, 1, 0, 0, 0)
G4 = c(0, 0, 0, 1, 0, 0)
G5 = c(0, 0, 0, 0, 1, 0)
G6 = c(0, 0, 0, 0, 0, 1)

fig2epr <- contrast(e2, method = list(G1 - G1, G2 - G1, G3 - G1, G4 - G4, G5 - G4, G6 - G4), infer = TRUE)
fig2e <- as.data.frame(fig2epr) 
fig2e$gender <- c("Male Resp", "Male Resp", "Male Resp", "Female Resp", "Female Resp", "Female Resp")
fig2e$group <- c("Automated \nSystem", "Male \nEmployee", "Female \nEmployee", "Automated \nSystem", "Male \nEmployee", "Female \nEmployee")

model6 <- svyglm(partic1 ~ Group*female, design=survey)
summary(model6)
f2 <- emmeans(model6, ~ Group*female)

G1 = c(1, 0, 0, 0, 0, 0)
G2 = c(0, 1, 0, 0, 0, 0)
G3 = c(0, 0, 1, 0, 0, 0)
G4 = c(0, 0, 0, 1, 0, 0)
G5 = c(0, 0, 0, 0, 1, 0)
G6 = c(0, 0, 0, 0, 0, 1)

fig2fpr <- contrast(f2, method = list(G1 - G1, G2 - G1, G3 - G1, G4 - G4, G5 - G4, G6 - G4), infer = TRUE)
fig2f <- as.data.frame(fig2fpr) 
fig2f$gender <- c("Male Resp", "Male Resp", "Male Resp", "Female Resp", "Female Resp", "Female Resp")
fig2f$group <- c("Automated \nSystem", "Male \nEmployee", "Female \nEmployee", "Automated \nSystem", "Male \nEmployee", "Female \nEmployee")

out <- rbind(fig2e, fig2f)
out$notweighted <- c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
out$notweighted <- factor(out$notweighted,
                          levels = c("0", "1"),
                          labels = c("Weighted", "Not Weighted"))
out$group <- factor(out$group,
                    levels = c("Automated \nSystem", "Male \nEmployee", "Female \nEmployee"),
                    labels = c("Automated \nSystem", "Male \nEmployee", "Female \nEmployee"))

fig2ef <- ggplot(data = out, aes(x = estimate, y = group, color=group, shape = notweighted)) +
  geom_vline(xintercept = 0) +
  geom_point(position=position_dodge2(.25), size=2.5) + xlab("Difference in Willingness to Participate") + ylab("") +
  geom_linerange(aes(xmin = lower.CL , xmax = upper.CL), position=position_dodge2(.25)) +
  facet_wrap(~gender) +
  theme_bw() + ggtitle ("Willingness to Participate (0, 1)") + theme(legend.position="bottom", 
                                                                     legend.title = element_blank(),
                                                                     plot.title = element_text(size=11),
                                                                     axis.title=element_text(size=8)) +
  guides(color="none") +
  scale_color_manual(values=c("turquoise4", "tomato3", "dodgerblue4")) + labs(color="")

print(fig2ef)        

fig2 <- ggarrange(fig2ab, fig2cd, fig2ef, ncol=3, nrow=1, common.legend = TRUE, legend="bottom")
annotate_figure(fig2,  
                bottom = text_grob("95% Confidence Intervals", color = "black",
                                   hjust = 1, x = 1, size = 10))

##Models: environmental policy support
###Participation (0-100)

model7 <- glm(partic100 ~ Group*envirSUPPORT, data=PAdata)
summary(model7)
a3 <- emmeans(model7, ~ Group*envirSUPPORT)

G1 = c(1, 0, 0, 0, 0, 0)
G2 = c(0, 1, 0, 0, 0, 0)
G3 = c(0, 0, 1, 0, 0, 0)
G4 = c(0, 0, 0, 1, 0, 0)
G5 = c(0, 0, 0, 0, 1, 0)
G6 = c(0, 0, 0, 0, 0, 1)

fig3apr <- contrast(a3, method = list(G1 - G1, G2 - G1, G3 - G1, G4 - G4, G5 - G4, G6 - G4), infer = TRUE)
fig3a <- as.data.frame(fig3apr) 
fig3a$support <- c("Oppose", "Oppose", "Oppose", "Support", "Support", "Support")
fig3a$group <- c("Automated \nSystem", "Male \nEmployee", "Female \nEmployee", "Automated \nSystem", "Male \nEmployee", "Female \nEmployee")

model8 <- svyglm(partic100 ~ Group*envirSUPPORT, design=survey)
summary(model8)
b3 <- emmeans(model8, ~ Group*envirSUPPORT)

G1 = c(1, 0, 0, 0, 0, 0)
G2 = c(0, 1, 0, 0, 0, 0)
G3 = c(0, 0, 1, 0, 0, 0)
G4 = c(0, 0, 0, 1, 0, 0)
G5 = c(0, 0, 0, 0, 1, 0)
G6 = c(0, 0, 0, 0, 0, 1)

fig3bpr <- contrast(b3, method = list(G1 - G1, G2 - G1, G3 - G1, G4 - G4, G5 - G4, G6 - G4), infer = TRUE)
fig3b <- as.data.frame(fig3bpr) 
fig3b$support <- c("Oppose", "Oppose", "Oppose", "Support", "Support", "Support")
fig3b$group <- c("Automated \nSystem", "Male \nEmployee", "Female \nEmployee", "Automated \nSystem", "Male \nEmployee", "Female \nEmployee")

out <- rbind(fig3a, fig3b)
out$notweighted <- c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
out$notweighted <- factor(out$notweighted,
                          levels = c("0", "1"),
                          labels = c("Weighted", "Not Weighted"))
out$group <- factor(out$group,
                    levels = c("Automated \nSystem", "Male \nEmployee", "Female \nEmployee"),
                    labels = c("Automated \nSystem", "Male \nEmployee", "Female \nEmployee"))

fig3ab <- ggplot(data = out, aes(x = estimate, y = group, color=group, shape = notweighted)) +
  geom_vline(xintercept = 0) +
  geom_point(position=position_dodge2(.25), size=2.5) + xlab("Difference in Willingness to Participate") + ylab("") +
  geom_linerange(aes(xmin = lower.CL , xmax = upper.CL), position=position_dodge2(.25)) +
  facet_wrap(~support) +
  theme_bw() + ggtitle ("Willingness to Participate (0-100)") + theme(legend.position="bottom", 
                                                                      legend.title = element_blank(),
                                                                      plot.title = element_text(size=11),
                                                                      axis.title=element_text(size=8)) +
  guides(color="none") +
  scale_color_manual(values=c("turquoise4", "tomato3", "dodgerblue4")) + labs(color="")

print(fig3ab)        

###Participation (0-5)

model9 <- glm(partic5 ~ Group*envirSUPPORT, data=PAdata)
summary(model9)
c3 <- emmeans(model9, ~ Group*envirSUPPORT)

G1 = c(1, 0, 0, 0, 0, 0)
G2 = c(0, 1, 0, 0, 0, 0)
G3 = c(0, 0, 1, 0, 0, 0)
G4 = c(0, 0, 0, 1, 0, 0)
G5 = c(0, 0, 0, 0, 1, 0)
G6 = c(0, 0, 0, 0, 0, 1)

fig3cpr <- contrast(c3, method = list(G1 - G1, G2 - G1, G3 - G1, G4 - G4, G5 - G4, G6 - G4), infer = TRUE)
fig3c <- as.data.frame(fig3cpr) 
fig3c$support <- c("Oppose", "Oppose", "Oppose", "Support", "Support", "Support")
fig3c$group <- c("Automated \nSystem", "Male \nEmployee", "Female \nEmployee", "Automated \nSystem", "Male \nEmployee", "Female \nEmployee")

model10 <- svyglm(partic5 ~ Group*envirSUPPORT, design=survey)
summary(model10)
d3 <- emmeans(model10, ~ Group*envirSUPPORT)

G1 = c(1, 0, 0, 0, 0, 0)
G2 = c(0, 1, 0, 0, 0, 0)
G3 = c(0, 0, 1, 0, 0, 0)
G4 = c(0, 0, 0, 1, 0, 0)
G5 = c(0, 0, 0, 0, 1, 0)
G6 = c(0, 0, 0, 0, 0, 1)

fig3dpr <- contrast(d3, method = list(G1 - G1, G2 - G1, G3 - G1, G4 - G4, G5 - G4, G6 - G4), infer = TRUE)
fig3d <- as.data.frame(fig3dpr) 
fig3d$support <- c("Oppose", "Oppose", "Oppose", "Support", "Support", "Support")
fig3d$group <- c("Automated \nSystem", "Male \nEmployee", "Female \nEmployee", "Automated \nSystem", "Male \nEmployee", "Female \nEmployee")

out <- rbind(fig3c, fig3d)
out$notweighted <- c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
out$notweighted <- factor(out$notweighted,
                          levels = c("0", "1"),
                          labels = c("Weighted", "Not Weighted"))
out$group <- factor(out$group,
                    levels = c("Automated \nSystem", "Male \nEmployee", "Female \nEmployee"),
                    labels = c("Automated \nSystem", "Male \nEmployee", "Female \nEmployee"))

fig3cd <- ggplot(data = out, aes(x = estimate, y = group, color=group, shape = notweighted)) +
  geom_vline(xintercept = 0) +
  geom_point(position=position_dodge2(.25), size=2.5) + xlab("Difference in Willingness to Participate") + ylab("") +
  geom_linerange(aes(xmin = lower.CL , xmax = upper.CL), position=position_dodge2(.25)) +
  facet_wrap(~support) +
  theme_bw() + ggtitle ("Willingness to Participate (0-5)") + theme(legend.position="bottom", 
                                                                    legend.title = element_blank(),
                                                                    plot.title = element_text(size=11),
                                                                    axis.title=element_text(size=8)) +
  guides(color="none") +
  scale_color_manual(values=c("turquoise4", "tomato3", "dodgerblue4")) + labs(color="")

print(fig3cd)        

######Participation (0, 1)

model11 <- glm(partic1 ~ Group*envirSUPPORT, data=PAdata)
summary(model11)
e3 <- emmeans(model11, ~ Group*envirSUPPORT)

G1 = c(1, 0, 0, 0, 0, 0)
G2 = c(0, 1, 0, 0, 0, 0)
G3 = c(0, 0, 1, 0, 0, 0)
G4 = c(0, 0, 0, 1, 0, 0)
G5 = c(0, 0, 0, 0, 1, 0)
G6 = c(0, 0, 0, 0, 0, 1)

fig3epr <- contrast(e3, method = list(G1 - G1, G2 - G1, G3 - G1, G4 - G4, G5 - G4, G6 - G4), infer = TRUE)
fig3e <- as.data.frame(fig3epr) 
fig3e$support <- c("Oppose", "Oppose", "Oppose", "Support", "Support", "Support")
fig3e$group <- c("Automated \nSystem", "Male \nEmployee", "Female \nEmployee", "Automated \nSystem", "Male \nEmployee", "Female \nEmployee")

model12 <- svyglm(partic1 ~ Group*envirSUPPORT, design=survey)
summary(model12)
f3 <- emmeans(model12, ~ Group*envirSUPPORT)

G1 = c(1, 0, 0, 0, 0, 0)
G2 = c(0, 1, 0, 0, 0, 0)
G3 = c(0, 0, 1, 0, 0, 0)
G4 = c(0, 0, 0, 1, 0, 0)
G5 = c(0, 0, 0, 0, 1, 0)
G6 = c(0, 0, 0, 0, 0, 1)

fig3fpr <- contrast(f3, method = list(G1 - G1, G2 - G1, G3 - G1, G4 - G4, G5 - G4, G6 - G4), infer = TRUE)
fig3f <- as.data.frame(fig3fpr) 
fig3f$support <- c("Oppose", "Oppose", "Oppose", "Support", "Support", "Support")
fig3f$group <- c("Automated \nSystem", "Male \nEmployee", "Female \nEmployee", "Automated \nSystem", "Male \nEmployee", "Female \nEmployee")

out <- rbind(fig3e, fig3f)
out$notweighted <- c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
out$notweighted <- factor(out$notweighted,
                          levels = c("0", "1"),
                          labels = c("Weighted", "Not Weighted"))
out$group <- factor(out$group,
                    levels = c("Automated \nSystem", "Male \nEmployee", "Female \nEmployee"),
                    labels = c("Automated \nSystem", "Male \nEmployee", "Female \nEmployee"))

fig3ef <- ggplot(data = out, aes(x = estimate, y = group, color=group, shape = notweighted)) +
  geom_vline(xintercept = 0) +
  geom_point(position=position_dodge2(.25), size=2.5) + xlab("Difference in Willingness to Participate") + ylab("") +
  geom_linerange(aes(xmin = lower.CL , xmax = upper.CL), position=position_dodge2(.25)) +
  facet_wrap(~support) +
  theme_bw() + ggtitle ("Willingness to Participate (0, 1)") + theme(legend.position="bottom", 
                                                                     legend.title = element_blank(),
                                                                     plot.title = element_text(size=11),
                                                                     axis.title=element_text(size=8)) +
  guides(color="none") +
  scale_color_manual(values=c("turquoise4", "tomato3", "dodgerblue4")) + labs(color="")

print(fig3ef)        

fig3 <- ggarrange(fig3ab, fig3cd, fig3ef, ncol=3, nrow=1, common.legend = TRUE, legend="bottom")
annotate_figure(fig3,  
                bottom = text_grob("95% Confidence Intervals", color = "black",
                                   hjust = 1, x = 1, size = 10))
