# Replication code for: 
# "Do Political Elites Have Accurate Perceptions of Social Conditions?"
# By Adam Thal

# Load required packages

library("ggplot2")
library("Rmisc")

# Set working directory

setwd("~/Dropbox/Political Elites Paper/Writing/Elites BJPS/BJPS Publication Files/Replication Files")

# Load data

data <- read.csv("thalelitesbjps.csv")

############
# Figure 1 #
############

# Measure gap between perception and reality

data$state.gap.insecure <- data$perception.insecure - data$actual.insecure
data$state.gap.medcare <- data$perception.medcare - data$actual.medcare
data$state.gap.eduloan <- data$perception.eduloan - data$actual.eduloan

# Measure mean distance for each issue by party

insecure.plot <- na.omit(summarySE(data, measurevar="state.gap.insecure", groupvars=c("pid"), na.rm = T))
colnames(insecure.plot)[3] <- "estimate"
insecure.plot$variable <- rep("state.gap.insecure", 2)

medcare.plot <- na.omit(summarySE(data, measurevar="state.gap.medcare", groupvars=c("pid"), na.rm = T))
colnames(medcare.plot)[3] <- "estimate"
medcare.plot$variable <- rep("state.gap.medcare", 2)

eduloan.plot <- na.omit(summarySE(data, measurevar="state.gap.eduloan", groupvars=c("pid"), na.rm = T))
colnames(eduloan.plot)[3] <- "estimate"
eduloan.plot$variable <- rep("state.gap.eduloan", 2)

# Combine estimates and plot

plot <- rbind(insecure.plot, medcare.plot, eduloan.plot)

plot$pid <- factor(plot$pid,
                   levels = c("Democrat", "Republican"),
                   labels = c("Democratic\nPoliticians",
                              "Republican\nPoliticians"))

plot$variable <- factor(plot$variable,levels = c("state.gap.insecure",
                                                 "state.gap.medcare",
                                                 "state.gap.eduloan"), 
                                      labels = c("Financial Insecurity",
                                                  "Unaffordable Healthcare",
                                                  "College Debt"))

plot$estimate <- plot$estimate/100
plot$ci <- plot$ci/100

ggplot(plot, aes(x=pid, y=estimate, shape=pid, colour=pid)) +
  facet_grid(.~variable) +
  geom_errorbar(aes(ymin=estimate-ci, ymax=estimate+ci), position=position_dodge(width=0.9), 
                width=.1, size=1, colour="black") +
  geom_point(stat="identity",size=9) +
  scale_shape_manual(values=c(17, 16))+
  scale_colour_manual(values=c("grey73", "grey53")) +
  scale_y_continuous(limits = c(-.10,.25),
                     breaks = round(seq(-.10, .25, by = .05),2)) +
  geom_hline(yintercept = 0, linetype="dashed") +
  ylab("Average Distance Between\nPoliticians' Perceptions and Reality\n") +
  xlab("") +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.title.y = element_text(size=26),
        strip.text.x = element_text(size=28), 
        legend.position = "none")

############
# Figure 2 #
############

# Create categorical measure of distance

data$state.gap.insecure.under10 <- ifelse(data$state.gap.insecure < -10, 1, 0)
data$state.gap.medcare.under10 <- ifelse(data$state.gap.medcare < -10, 1, 0)
data$state.gap.eduloan.under10 <- ifelse(data$state.gap.eduloan < -10, 1, 0)

data$state.gap.insecure.within10 <- ifelse(data$state.gap.insecure >= -10 &
																	         data$state.gap.insecure <= 10, 1, 0)
data$state.gap.medcare.within10 <- ifelse(data$state.gap.medcare >= -10 &
																	        data$state.gap.medcare <= 10, 1, 0)
data$state.gap.eduloan.within10 <- ifelse(data$state.gap.eduloan >= -10 &
																	        data$state.gap.eduloan <= 10, 1, 0)

data$state.gap.insecure.over10 <- ifelse(data$state.gap.insecure > 10, 1, 0)
data$state.gap.medcare.over10 <- ifelse(data$state.gap.medcare > 10, 1, 0)
data$state.gap.eduloan.over10 <- ifelse(data$state.gap.eduloan > 10, 1, 0)

# Measure distribution across categories by party

finsec.under <- na.omit(summarySE(data, measurevar="state.gap.insecure.under10", groupvars=c("pid"), na.rm = T))
colnames(finsec.under)[3] <- "estimate"
finsec.under$variable <- rep("Financial Insecurity", 2)
finsec.under$type <- rep("Underestimate by more than 10 points", 2)

medcare.under <- na.omit(summarySE(data, measurevar="state.gap.medcare.under10", groupvars=c("pid"), na.rm = T))
colnames(medcare.under)[3] <- "estimate"
medcare.under$variable <- rep("Unaffordable Healthcare", 2)
medcare.under$type <- rep("Underestimate by more than 10 points", 2)

eduloan.under <- na.omit(summarySE(data, measurevar="state.gap.eduloan.under10", groupvars=c("pid"), na.rm = T))
colnames(eduloan.under)[3] <- "estimate"
eduloan.under$variable <- rep("College Debt", 2)
eduloan.under$type <- rep("Underestimate by more than 10 points", 2)

finsec.within <- na.omit(summarySE(data, measurevar="state.gap.insecure.within10", groupvars=c("pid"), na.rm = T))
colnames(finsec.within)[3] <- "estimate"
finsec.within$variable <- rep("Financial Insecurity", 2)
finsec.within$type <- rep("Within 10 points", 2)

medcare.within <- na.omit(summarySE(data, measurevar="state.gap.medcare.within10", groupvars=c("pid"), na.rm = T))
colnames(medcare.within)[3] <- "estimate"
medcare.within$variable <- rep("Unaffordable Healthcare", 2)
medcare.within$type <- rep("Within 10 points", 2)

eduloan.within <- na.omit(summarySE(data, measurevar="state.gap.eduloan.within10", groupvars=c("pid"), na.rm = T))
colnames(eduloan.within)[3] <- "estimate"
eduloan.within$variable <- rep("College Debt", 2)
eduloan.within$type <- rep("Within 10 points", 2)

finsec.over <- na.omit(summarySE(data, measurevar="state.gap.insecure.over10", groupvars=c("pid"), na.rm = T))
colnames(finsec.over)[3] <- "estimate"
finsec.over$variable <- rep("Financial Insecurity", 2)
finsec.over$type <- rep("Overestimate by more than 10 points", 2)

medcare.over <- na.omit(summarySE(data, measurevar="state.gap.medcare.over10", groupvars=c("pid"), na.rm = T))
colnames(medcare.over)[3] <- "estimate"
medcare.over$variable <- rep("Unaffordable Healthcare", 2)
medcare.over$type <- rep("Overestimate by more than 10 points", 2)

eduloan.over <- na.omit(summarySE(data, measurevar="state.gap.eduloan.over10", groupvars=c("pid"), na.rm = T))
colnames(eduloan.over)[3] <- "estimate"
eduloan.over$variable <- rep("College Debt", 2)
eduloan.over$type <- rep("Overestimate by more than 10 points", 2)

# Combine estimates and plot

plot <- rbind(finsec.under, medcare.under, eduloan.under,
							finsec.within, medcare.within, eduloan.within,
							finsec.over, medcare.over, eduloan.over)

plot$pid <- factor(plot$pid,
                   levels = c("Democrat", "Republican"),
                   labels = c("Democratic\nPoliticians",
                              "Republican\nPoliticians"))

plot$variable <- factor(plot$variable, levels = c("Financial Insecurity",
                                  							  "Unaffordable Healthcare",
                                  								"College Debt"))

plot$type <- factor(plot$type, levels = c("Underestimate by more than 10 points",
                                  			  "Within 10 points",
                                  			  "Overestimate by more than 10 points"),
										labels = c("Underestimate\nby more than\n10 points",
                               "Within\n10 points",
                               "Overestimate\nby more than\n10 points"))

plot$label <- paste(round(plot$estimate, 2)*100,"%",sep="")

ggplot(plot, aes(x=type, y=estimate)) +
  facet_grid(variable~pid) +
  geom_bar(stat="identity", aes_string(fill="pid"), colour = "black") +
  geom_errorbar(aes(ymin=estimate-ci, ymax=estimate+ci), position=position_dodge(width=0.9), 
                width=.2, size=1, colour="black") +
	geom_text(aes(label = label, y = estimate + 0.15, size = 10)) +
  scale_fill_manual(values=c("grey73", "grey53")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
  									 limits = c(0,1),
                     breaks = round(seq(0, 1, by = 0.2) ,1)) +
  ylab("Proportion") +
  xlab("") +
  theme_bw() +
  theme(text = element_text(size=20),
        axis.text.y = element_text(size=15),
        strip.text.y = element_text(size=14),
        legend.position = "none")

############
# Figure 3 #
############

# Create dataset that does not include respondents from Wyoming

data2 <- data[data$state != "Wyoming",]

# Create tables with treatment effect estimates for each financial insecurity outcome

spend.insecure.table <- as.data.frame(c("Democratic Politicians", "Republican Politicians"))
colnames(spend.insecure.table) <- "pid"
spend.insecure.table$estimate <- c(NA,NA)
spend.insecure.table$lci <- c(NA,NA)
spend.insecure.table$uci <- c(NA,NA)
spend.insecure.table$policy <- c("spend.insecure","spend.insecure")

spend.insecure.table[spend.insecure.table$pid == "Democratic Politicians",]$estimate <- coef(lm(spend.insecure ~ correct.info, data = data2[data2$dem == 1,]))[2]
spend.insecure.table[spend.insecure.table$pid == "Democratic Politicians",]$lci <- confint(lm(spend.insecure ~ correct.info, data = data2[data2$dem == 1,]))[2,1]
spend.insecure.table[spend.insecure.table$pid == "Democratic Politicians",]$uci <- confint(lm(spend.insecure ~ correct.info, data = data2[data2$dem == 1,]))[2,2]

spend.insecure.table[spend.insecure.table$pid == "Republican Politicians",]$estimate <- coef(lm(spend.insecure ~ correct.info, data = data2[data2$gop == 1,]))[2]
spend.insecure.table[spend.insecure.table$pid == "Republican Politicians",]$lci <- confint(lm(spend.insecure ~ correct.info, data = data2[data2$gop == 1,]))[2,1]
spend.insecure.table[spend.insecure.table$pid == "Republican Politicians",]$uci <- confint(lm(spend.insecure ~ correct.info, data = data2[data2$gop == 1,]))[2,2]

favor.minwage.table <- as.data.frame(c("Democratic Politicians", "Republican Politicians"))
colnames(favor.minwage.table) <- "pid"
favor.minwage.table$estimate <- c(NA,NA)
favor.minwage.table$lci <- c(NA,NA)
favor.minwage.table$uci <- c(NA,NA)
favor.minwage.table$policy <- c("favor.minwage","favor.minwage")

favor.minwage.table[favor.minwage.table$pid == "Democratic Politicians",]$estimate <- coef(lm(favor.minwage ~ correct.info, data = data2[data2$dem == 1,]))[2]
favor.minwage.table[favor.minwage.table$pid == "Democratic Politicians",]$lci <- confint(lm(favor.minwage ~ correct.info, data = data2[data2$dem == 1,]))[2,1]
favor.minwage.table[favor.minwage.table$pid == "Democratic Politicians",]$uci <- confint(lm(favor.minwage ~ correct.info, data = data2[data2$dem == 1,]))[2,2]

favor.minwage.table[favor.minwage.table$pid == "Republican Politicians",]$estimate <- coef(lm(favor.minwage ~ correct.info, data = data2[data2$gop == 1,]))[2]
favor.minwage.table[favor.minwage.table$pid == "Republican Politicians",]$lci <- confint(lm(favor.minwage ~ correct.info, data = data2[data2$gop == 1,]))[2,1]
favor.minwage.table[favor.minwage.table$pid == "Republican Politicians",]$uci <- confint(lm(favor.minwage ~ correct.info, data = data2[data2$gop == 1,]))[2,2]

favor.asset.table <- as.data.frame(c("Democratic Politicians", "Republican Politicians"))
colnames(favor.asset.table) <- "pid"
favor.asset.table$estimate <- c(NA,NA)
favor.asset.table$lci <- c(NA,NA)
favor.asset.table$uci <- c(NA,NA)
favor.asset.table$policy <- c("favor.asset","favor.asset")

favor.asset.table[favor.asset.table$pid == "Democratic Politicians",]$estimate <- coef(lm(favor.asset ~ correct.info, data = data2[data2$dem == 1,]))[2]
favor.asset.table[favor.asset.table$pid == "Democratic Politicians",]$lci <- confint(lm(favor.asset ~ correct.info, data = data2[data2$dem == 1,]))[2,1]
favor.asset.table[favor.asset.table$pid == "Democratic Politicians",]$uci <- confint(lm(favor.asset ~ correct.info, data = data2[data2$dem == 1,]))[2,2]

favor.asset.table[favor.asset.table$pid == "Republican Politicians",]$estimate <- coef(lm(favor.asset ~ correct.info, data = data2[data2$gop == 1,]))[2]
favor.asset.table[favor.asset.table$pid == "Republican Politicians",]$lci <- confint(lm(favor.asset ~ correct.info, data = data2[data2$gop == 1,]))[2,1]
favor.asset.table[favor.asset.table$pid == "Republican Politicians",]$uci <- confint(lm(favor.asset ~ correct.info, data = data2[data2$gop == 1,]))[2,2]

eval.assetlimit.table <- as.data.frame(c("Democratic Politicians", "Republican Politicians"))
colnames(eval.assetlimit.table) <- "pid"
eval.assetlimit.table$estimate <- c(NA,NA)
eval.assetlimit.table$lci <- c(NA,NA)
eval.assetlimit.table$uci <- c(NA,NA)
eval.assetlimit.table$policy <- c("eval.assetlimit","eval.assetlimit")

eval.assetlimit.table[eval.assetlimit.table$pid == "Democratic Politicians",]$estimate <- coef(lm(eval.assetlimit ~ correct.info, data = data2[data2$dem == 1,]))[2]
eval.assetlimit.table[eval.assetlimit.table$pid == "Democratic Politicians",]$lci <- confint(lm(eval.assetlimit ~ correct.info, data = data2[data2$dem == 1,]))[2,1]
eval.assetlimit.table[eval.assetlimit.table$pid == "Democratic Politicians",]$uci <- confint(lm(eval.assetlimit ~ correct.info, data = data2[data2$dem == 1,]))[2,2]

eval.assetlimit.table[eval.assetlimit.table$pid == "Republican Politicians",]$estimate <- coef(lm(eval.assetlimit ~ correct.info, data = data2[data2$gop == 1,]))[2]
eval.assetlimit.table[eval.assetlimit.table$pid == "Republican Politicians",]$lci <- confint(lm(eval.assetlimit ~ correct.info, data = data2[data2$gop == 1,]))[2,1]
eval.assetlimit.table[eval.assetlimit.table$pid == "Republican Politicians",]$uci <- confint(lm(eval.assetlimit ~ correct.info, data = data2[data2$gop == 1,]))[2,2]

resp.insecure.table <- as.data.frame(c("Democratic Politicians", "Republican Politicians"))
colnames(resp.insecure.table) <- "pid"
resp.insecure.table$estimate <- c(NA,NA)
resp.insecure.table$lci <- c(NA,NA)
resp.insecure.table$uci <- c(NA,NA)
resp.insecure.table$policy <- c("resp.insecure","resp.insecure")

resp.insecure.table[resp.insecure.table$pid == "Democratic Politicians",]$estimate <- coef(lm(resp.insecure ~ correct.info, data = data2[data2$dem == 1,]))[2]
resp.insecure.table[resp.insecure.table$pid == "Democratic Politicians",]$lci <- confint(lm(resp.insecure ~ correct.info, data = data2[data2$dem == 1,]))[2,1]
resp.insecure.table[resp.insecure.table$pid == "Democratic Politicians",]$uci <- confint(lm(resp.insecure ~ correct.info, data = data2[data2$dem == 1,]))[2,2]

resp.insecure.table[resp.insecure.table$pid == "Republican Politicians",]$estimate <- coef(lm(resp.insecure ~ correct.info, data = data2[data2$gop == 1,]))[2]
resp.insecure.table[resp.insecure.table$pid == "Republican Politicians",]$lci <- confint(lm(resp.insecure ~ correct.info, data = data2[data2$gop == 1,]))[2,1]
resp.insecure.table[resp.insecure.table$pid == "Republican Politicians",]$uci <- confint(lm(resp.insecure ~ correct.info, data = data2[data2$gop == 1,]))[2,2]

# Combine estimates and plot

plot <- rbind(spend.insecure.table,
              favor.minwage.table,
              favor.asset.table,
              eval.assetlimit.table,
              resp.insecure.table)

plot$policy <- factor(plot$policy, levels = rev(c("spend.insecure",
                                                  "favor.minwage",
                                                  "favor.asset",
                                                  "eval.assetlimit",
                                                  "resp.insecure")),
                      labels = rev(c("Support for increasing\nspending on cash assistance",
                                     "Support for increasing\nthe minimum wage",
                                     "Support for eliminating\nthe welfare asset limit",
                                     "Agreement that welfare\nasset limit is too low",
                                     "Agreement that financial security\nis government responsibility")))

plot$PID <- factor(plot$pid, levels = c("Republican Politicians",
                                            "Democratic Politicians"))

ggplot(plot, aes(x=policy, y=estimate, shape=PID,colour=PID)) +
  geom_hline(yintercept=0,linetype="dashed") +
  geom_errorbar(aes(ymin=lci, ymax=uci), position=position_dodge(width=0.3), 
                width=.2, size=.75,colour="black") +
  geom_point(size=5.5, position=position_dodge(width=0.3)) +
  ylim(-.2, .2) +
  coord_flip() +    
  scale_colour_manual(values=c("grey53", "grey73"),
                      guide = guide_legend(reverse = TRUE)) +
  scale_shape_manual(values=c(16, 17),
                     guide = guide_legend(reverse = TRUE)) +
  ylab("Treatment Effect Estimates With\n95% Confidence Intervals") +
  xlab("") +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size=20),
        axis.text.y=element_text(size=20),
        axis.text.x=element_text(size=20),
        axis.title.x=element_text(size=20))

############
# Figure 4 #
############

# Create tables with treatment effect estimates for each unaffordable healthcare outcome

spend.medicaid.table <- as.data.frame(c("Democratic Politicians", "Republican Politicians"))
colnames(spend.medicaid.table) <- "pid"
spend.medicaid.table$estimate <- c(NA,NA)
spend.medicaid.table$lci <- c(NA,NA)
spend.medicaid.table$uci <- c(NA,NA)
spend.medicaid.table$policy <- c("spend.medicaid","spend.medicaid")

spend.medicaid.table[spend.medicaid.table$pid == "Democratic Politicians",]$estimate <- coef(lm(spend.medicaid ~ correct.info, data = data2[data2$dem == 1,]))[2]
spend.medicaid.table[spend.medicaid.table$pid == "Democratic Politicians",]$lci <- confint(lm(spend.medicaid ~ correct.info, data = data2[data2$dem == 1,]))[2,1]
spend.medicaid.table[spend.medicaid.table$pid == "Democratic Politicians",]$uci <- confint(lm(spend.medicaid ~ correct.info, data = data2[data2$dem == 1,]))[2,2]

spend.medicaid.table[spend.medicaid.table$pid == "Republican Politicians",]$estimate <- coef(lm(spend.medicaid ~ correct.info, data = data2[data2$gop == 1,]))[2]
spend.medicaid.table[spend.medicaid.table$pid == "Republican Politicians",]$lci <- confint(lm(spend.medicaid ~ correct.info, data = data2[data2$gop == 1,]))[2,1]
spend.medicaid.table[spend.medicaid.table$pid == "Republican Politicians",]$uci <- confint(lm(spend.medicaid ~ correct.info, data = data2[data2$gop == 1,]))[2,2]

favor.limitcharge.table <- as.data.frame(c("Democratic Politicians", "Republican Politicians"))
colnames(favor.limitcharge.table) <- "pid"
favor.limitcharge.table$estimate <- c(NA,NA)
favor.limitcharge.table$lci <- c(NA,NA)
favor.limitcharge.table$uci <- c(NA,NA)
favor.limitcharge.table$policy <- c("favor.limitcharge","favor.limitcharge")

favor.limitcharge.table[favor.limitcharge.table$pid == "Democratic Politicians",]$estimate <- coef(lm(favor.limitcharge ~ correct.info, data = data2[data2$dem == 1,]))[2]
favor.limitcharge.table[favor.limitcharge.table$pid == "Democratic Politicians",]$lci <- confint(lm(favor.limitcharge ~ correct.info, data = data2[data2$dem == 1,]))[2,1]
favor.limitcharge.table[favor.limitcharge.table$pid == "Democratic Politicians",]$uci <- confint(lm(favor.limitcharge ~ correct.info, data = data2[data2$dem == 1,]))[2,2]

favor.limitcharge.table[favor.limitcharge.table$pid == "Republican Politicians",]$estimate <- coef(lm(favor.limitcharge ~ correct.info, data = data2[data2$gop == 1,]))[2]
favor.limitcharge.table[favor.limitcharge.table$pid == "Republican Politicians",]$lci <- confint(lm(favor.limitcharge ~ correct.info, data = data2[data2$gop == 1,]))[2,1]
favor.limitcharge.table[favor.limitcharge.table$pid == "Republican Politicians",]$uci <- confint(lm(favor.limitcharge ~ correct.info, data = data2[data2$gop == 1,]))[2,2]

favor.payplan.table <- as.data.frame(c("Democratic Politicians", "Republican Politicians"))
colnames(favor.payplan.table) <- "pid"
favor.payplan.table$estimate <- c(NA,NA)
favor.payplan.table$lci <- c(NA,NA)
favor.payplan.table$uci <- c(NA,NA)
favor.payplan.table$policy <- c("favor.payplan","favor.payplan")

favor.payplan.table[favor.payplan.table$pid == "Democratic Politicians",]$estimate <- coef(lm(favor.payplan ~ correct.info, data = data2[data2$dem == 1,]))[2]
favor.payplan.table[favor.payplan.table$pid == "Democratic Politicians",]$lci <- confint(lm(favor.payplan ~ correct.info, data = data2[data2$dem == 1,]))[2,1]
favor.payplan.table[favor.payplan.table$pid == "Democratic Politicians",]$uci <- confint(lm(favor.payplan ~ correct.info, data = data2[data2$dem == 1,]))[2,2]

favor.payplan.table[favor.payplan.table$pid == "Republican Politicians",]$estimate <- coef(lm(favor.payplan ~ correct.info, data = data2[data2$gop == 1,]))[2]
favor.payplan.table[favor.payplan.table$pid == "Republican Politicians",]$lci <- confint(lm(favor.payplan ~ correct.info, data = data2[data2$gop == 1,]))[2,1]
favor.payplan.table[favor.payplan.table$pid == "Republican Politicians",]$uci <- confint(lm(favor.payplan ~ correct.info, data = data2[data2$gop == 1,]))[2,2]

eval.premium.table <- as.data.frame(c("Democratic Politicians", "Republican Politicians"))
colnames(eval.premium.table) <- "pid"
eval.premium.table$estimate <- c(NA,NA)
eval.premium.table$lci <- c(NA,NA)
eval.premium.table$uci <- c(NA,NA)
eval.premium.table$policy <- c("eval.premium","eval.premium")

eval.premium.table[eval.premium.table$pid == "Democratic Politicians",]$estimate <- coef(lm(eval.premium ~ correct.info, data = data2[data2$dem == 1,]))[2]
eval.premium.table[eval.premium.table$pid == "Democratic Politicians",]$lci <- confint(lm(eval.premium ~ correct.info, data = data2[data2$dem == 1,]))[2,1]
eval.premium.table[eval.premium.table$pid == "Democratic Politicians",]$uci <- confint(lm(eval.premium ~ correct.info, data = data2[data2$dem == 1,]))[2,2]

eval.premium.table[eval.premium.table$pid == "Republican Politicians",]$estimate <- coef(lm(eval.premium ~ correct.info, data = data2[data2$gop == 1,]))[2]
eval.premium.table[eval.premium.table$pid == "Republican Politicians",]$lci <- confint(lm(eval.premium ~ correct.info, data = data2[data2$gop == 1,]))[2,1]
eval.premium.table[eval.premium.table$pid == "Republican Politicians",]$uci <- confint(lm(eval.premium ~ correct.info, data = data2[data2$gop == 1,]))[2,2]

resp.medcare.table <- as.data.frame(c("Democratic Politicians", "Republican Politicians"))
colnames(resp.medcare.table) <- "pid"
resp.medcare.table$estimate <- c(NA,NA)
resp.medcare.table$lci <- c(NA,NA)
resp.medcare.table$uci <- c(NA,NA)
resp.medcare.table$policy <- c("resp.medcare","resp.medcare")

resp.medcare.table[resp.medcare.table$pid == "Democratic Politicians",]$estimate <- coef(lm(resp.medcare ~ correct.info, data = data2[data2$dem == 1,]))[2]
resp.medcare.table[resp.medcare.table$pid == "Democratic Politicians",]$lci <- confint(lm(resp.medcare ~ correct.info, data = data2[data2$dem == 1,]))[2,1]
resp.medcare.table[resp.medcare.table$pid == "Democratic Politicians",]$uci <- confint(lm(resp.medcare ~ correct.info, data = data2[data2$dem == 1,]))[2,2]

resp.medcare.table[resp.medcare.table$pid == "Republican Politicians",]$estimate <- coef(lm(resp.medcare ~ correct.info, data = data2[data2$gop == 1,]))[2]
resp.medcare.table[resp.medcare.table$pid == "Republican Politicians",]$lci <- confint(lm(resp.medcare ~ correct.info, data = data2[data2$gop == 1,]))[2,1]
resp.medcare.table[resp.medcare.table$pid == "Republican Politicians",]$uci <- confint(lm(resp.medcare ~ correct.info, data = data2[data2$gop == 1,]))[2,2]

# Combine estimates and plot

plot <- rbind(spend.medicaid.table,
              favor.limitcharge.table,
              favor.payplan.table,
              eval.premium.table,
              resp.medcare.table)

plot$policy <- factor(plot$policy, levels = rev(c("spend.medicaid",
                                                  "favor.limitcharge",
                                                  "favor.payplan",
                                                  "eval.premium",
                                                  "resp.medcare")),
                      labels = rev(c("Support for increasing\nspending on Medicaid",
                                     "Support for limiting\nhospital charges",
                                     "Support for requiring\nhospital pay plans",
                                     "Agreement that ACA\npremium is too high",
                                     "Agreement that healthcare\nis government responsibility")))

plot$PID <- factor(plot$pid, levels = c("Republican Politicians",
                                        "Democratic Politicians"))

ggplot(plot, aes(x=policy, y=estimate, shape=PID,colour=PID)) +
  geom_hline(yintercept=0,linetype="dashed") +
  geom_errorbar(aes(ymin=lci, ymax=uci), position=position_dodge(width=0.3), 
                width=.2, size=.75,colour="black") +
  geom_point(size=5.5, position=position_dodge(width=0.3)) +
  ylim(-.2, .2) +
  coord_flip() +    
  scale_colour_manual(values=c("grey53", "grey73"),
                      guide = guide_legend(reverse = TRUE)) +
  scale_shape_manual(values=c(16, 17),
                     guide = guide_legend(reverse = TRUE)) +
  ylab("Treatment Effect Estimates With\n95% Confidence Intervals") +
  xlab("") +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size=20),
        axis.text.y=element_text(size=20),
        axis.text.x=element_text(size=20),
        axis.title.x=element_text(size=20))

############
# Figure 5 #
############

# Create tables with treatment effect estimates for each college debt outcome

spend.finaid.table <- as.data.frame(c("Democratic Politicians", "Republican Politicians"))
colnames(spend.finaid.table) <- "pid"
spend.finaid.table$estimate <- c(NA,NA)
spend.finaid.table$lci <- c(NA,NA)
spend.finaid.table$uci <- c(NA,NA)
spend.finaid.table$policy <- c("spend.finaid","spend.finaid")

spend.finaid.table[spend.finaid.table$pid == "Democratic Politicians",]$estimate <- coef(lm(spend.finaid ~ correct.info, data = data[data$dem == 1,]))[2]
spend.finaid.table[spend.finaid.table$pid == "Democratic Politicians",]$lci <- confint(lm(spend.finaid ~ correct.info, data = data[data$dem == 1,]))[2,1]
spend.finaid.table[spend.finaid.table$pid == "Democratic Politicians",]$uci <- confint(lm(spend.finaid ~ correct.info, data = data[data$dem == 1,]))[2,2]

spend.finaid.table[spend.finaid.table$pid == "Republican Politicians",]$estimate <- coef(lm(spend.finaid ~ correct.info, data = data[data$gop == 1,]))[2]
spend.finaid.table[spend.finaid.table$pid == "Republican Politicians",]$lci <- confint(lm(spend.finaid ~ correct.info, data = data[data$gop == 1,]))[2,1]
spend.finaid.table[spend.finaid.table$pid == "Republican Politicians",]$uci <- confint(lm(spend.finaid ~ correct.info, data = data[data$gop == 1,]))[2,2]

favor.cutspend.table <- as.data.frame(c("Democratic Politicians", "Republican Politicians"))
colnames(favor.cutspend.table) <- "pid"
favor.cutspend.table$estimate <- c(NA,NA)
favor.cutspend.table$lci <- c(NA,NA)
favor.cutspend.table$uci <- c(NA,NA)
favor.cutspend.table$policy <- c("favor.cutspend","favor.cutspend")

favor.cutspend.table[favor.cutspend.table$pid == "Democratic Politicians",]$estimate <- coef(lm(favor.cutspend ~ correct.info, data = data[data$dem == 1,]))[2]
favor.cutspend.table[favor.cutspend.table$pid == "Democratic Politicians",]$lci <- confint(lm(favor.cutspend ~ correct.info, data = data[data$dem == 1,]))[2,1]
favor.cutspend.table[favor.cutspend.table$pid == "Democratic Politicians",]$uci <- confint(lm(favor.cutspend ~ correct.info, data = data[data$dem == 1,]))[2,2]

favor.cutspend.table[favor.cutspend.table$pid == "Republican Politicians",]$estimate <- coef(lm(favor.cutspend ~ correct.info, data = data[data$gop == 1,]))[2]
favor.cutspend.table[favor.cutspend.table$pid == "Republican Politicians",]$lci <- confint(lm(favor.cutspend ~ correct.info, data = data[data$gop == 1,]))[2,1]
favor.cutspend.table[favor.cutspend.table$pid == "Republican Politicians",]$uci <- confint(lm(favor.cutspend ~ correct.info, data = data[data$gop == 1,]))[2,2]

favor.limittuition.table <- as.data.frame(c("Democratic Politicians", "Republican Politicians"))
colnames(favor.limittuition.table) <- "pid"
favor.limittuition.table$estimate <- c(NA,NA)
favor.limittuition.table$lci <- c(NA,NA)
favor.limittuition.table$uci <- c(NA,NA)
favor.limittuition.table$policy <- c("favor.limittuition","favor.limittuition")

favor.limittuition.table[favor.limittuition.table$pid == "Democratic Politicians",]$estimate <- coef(lm(favor.limittuition ~ correct.info, data = data[data$dem == 1,]))[2]
favor.limittuition.table[favor.limittuition.table$pid == "Democratic Politicians",]$lci <- confint(lm(favor.limittuition ~ correct.info, data = data[data$dem == 1,]))[2,1]
favor.limittuition.table[favor.limittuition.table$pid == "Democratic Politicians",]$uci <- confint(lm(favor.limittuition ~ correct.info, data = data[data$dem == 1,]))[2,2]

favor.limittuition.table[favor.limittuition.table$pid == "Republican Politicians",]$estimate <- coef(lm(favor.limittuition ~ correct.info, data = data[data$gop == 1,]))[2]
favor.limittuition.table[favor.limittuition.table$pid == "Republican Politicians",]$lci <- confint(lm(favor.limittuition ~ correct.info, data = data[data$gop == 1,]))[2,1]
favor.limittuition.table[favor.limittuition.table$pid == "Republican Politicians",]$uci <- confint(lm(favor.limittuition ~ correct.info, data = data[data$gop == 1,]))[2,2]

eval.tuition.table <- as.data.frame(c("Democratic Politicians", "Republican Politicians"))
colnames(eval.tuition.table) <- "pid"
eval.tuition.table$estimate <- c(NA,NA)
eval.tuition.table$lci <- c(NA,NA)
eval.tuition.table$uci <- c(NA,NA)
eval.tuition.table$policy <- c("eval.tuition","eval.tuition")

eval.tuition.table[eval.tuition.table$pid == "Democratic Politicians",]$estimate <- coef(lm(eval.tuition ~ correct.info, data = data[data$dem == 1,]))[2]
eval.tuition.table[eval.tuition.table$pid == "Democratic Politicians",]$lci <- confint(lm(eval.tuition ~ correct.info, data = data[data$dem == 1,]))[2,1]
eval.tuition.table[eval.tuition.table$pid == "Democratic Politicians",]$uci <- confint(lm(eval.tuition ~ correct.info, data = data[data$dem == 1,]))[2,2]

eval.tuition.table[eval.tuition.table$pid == "Republican Politicians",]$estimate <- coef(lm(eval.tuition ~ correct.info, data = data[data$gop == 1,]))[2]
eval.tuition.table[eval.tuition.table$pid == "Republican Politicians",]$lci <- confint(lm(eval.tuition ~ correct.info, data = data[data$gop == 1,]))[2,1]
eval.tuition.table[eval.tuition.table$pid == "Republican Politicians",]$uci <- confint(lm(eval.tuition ~ correct.info, data = data[data$gop == 1,]))[2,2]

resp.eduafford.table <- as.data.frame(c("Democratic Politicians", "Republican Politicians"))
colnames(resp.eduafford.table) <- "pid"
resp.eduafford.table$estimate <- c(NA,NA)
resp.eduafford.table$lci <- c(NA,NA)
resp.eduafford.table$uci <- c(NA,NA)
resp.eduafford.table$policy <- c("resp.eduafford","resp.eduafford")

resp.eduafford.table[resp.eduafford.table$pid == "Democratic Politicians",]$estimate <- coef(lm(resp.eduafford ~ correct.info, data = data[data$dem == 1,]))[2]
resp.eduafford.table[resp.eduafford.table$pid == "Democratic Politicians",]$lci <- confint(lm(resp.eduafford ~ correct.info, data = data[data$dem == 1,]))[2,1]
resp.eduafford.table[resp.eduafford.table$pid == "Democratic Politicians",]$uci <- confint(lm(resp.eduafford ~ correct.info, data = data[data$dem == 1,]))[2,2]

resp.eduafford.table[resp.eduafford.table$pid == "Republican Politicians",]$estimate <- coef(lm(resp.eduafford ~ correct.info, data = data[data$gop == 1,]))[2]
resp.eduafford.table[resp.eduafford.table$pid == "Republican Politicians",]$lci <- confint(lm(resp.eduafford ~ correct.info, data = data[data$gop == 1,]))[2,1]
resp.eduafford.table[resp.eduafford.table$pid == "Republican Politicians",]$uci <- confint(lm(resp.eduafford ~ correct.info, data = data[data$gop == 1,]))[2,2]

# Combine estimates and plot

plot <- rbind(spend.finaid.table,
              favor.cutspend.table,
              favor.limittuition.table,
              eval.tuition.table,
              resp.eduafford.table)

plot$policy <- factor(plot$policy, levels = rev(c("spend.finaid",
                                                  "favor.cutspend",
                                                  "favor.limittuition",
                                                  "eval.tuition",
                                                  "resp.eduafford")),
                      labels = rev(c("Support for increasing\nspending on financial aid",
                                     "Oppose spending cuts\nto public universities",
                                     "Support for limiting\ntuition",
                                     "Agreement that \ntuition is too high",
                                     "Agreement that college affordability\nis government responsibility")))

plot$PID <- factor(plot$pid, levels = c("Republican Politicians",
                                        "Democratic Politicians"))

ggplot(plot, aes(x=policy, y=estimate, shape=PID,colour=PID)) +
  geom_hline(yintercept=0,linetype="dashed") +
  geom_errorbar(aes(ymin=lci, ymax=uci), position=position_dodge(width=0.3), 
                width=.2, size=.75,colour="black") +
  geom_point(size=5.5, position=position_dodge(width=0.3)) +
  ylim(-.2, .2) +
  coord_flip() +    
  scale_colour_manual(values=c("grey53", "grey73"),
                      guide = guide_legend(reverse = TRUE)) +
  scale_shape_manual(values=c(16, 17),
                     guide = guide_legend(reverse = TRUE)) +
  ylab("Treatment Effect Estimates With\n95% Confidence Intervals") +
  xlab("") +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size=20),
        axis.text.y=element_text(size=20),
        axis.text.x=element_text(size=20),
        axis.title.x=element_text(size=20))
