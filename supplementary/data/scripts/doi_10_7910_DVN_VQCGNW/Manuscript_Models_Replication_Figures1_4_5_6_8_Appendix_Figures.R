library(margins)
library(ggplot2)
library(effects)
library(lmtest)
library(MASS)
library(sandwich)
library(multiwayvcov)
library(pscl)
library(plyr)
library(cowplot)
library(grid)
library(gridExtra)

setwd("/Final Data Replication Files")

set.seed(123)

# This R script contains the code to replicate the analysis and model figures in the Political Behavior Manuscript: "Congressional Approval & Responsible Party Government: The Role of Partisanship & Ideology in Citizen Assessments of the Contemporary U.S. Congress."

load("Replication_CCES_ANES_Files.Rdata")

options(scipen=999)

###### Figure 1: Plotting Partisan Congresional Approval over Time" ###### 

x <- nes
x$cong_approval_dich <- as.numeric(as.character(x$cong_approval_dich))
anes_approval <- ddply(x,.(year,pid3),summarize,approval = weighted.mean(cong_approval_dich,w=weight, na.rm=T))

x <- cces
x$cong_approval_binary <- ifelse(x$cong_approval_binary == "Approve",1,ifelse(x$cong_approval_binary == "Disapprove",0,NA))
cces_approval <- ddply(x,.(year,pid3),summarize,approval = weighted.mean(cong_approval_binary,w=weight, na.rm=T))

anes_approval$data <- "ANES"
cces_approval$data <- "CCES"

x <- rbind(anes_approval,cces_approval)
x <- subset(x,!is.na(x$pid3))
x$year <- as.numeric(x$year)
x$approval <- as.numeric(x$approval) * 100

x1 <-  ddply(x,.(year,pid3),summarize,approval = mean(approval, na.rm=T))
x1$dem_house <- ifelse(x1$year %in% seq(1980,1994,1),1,ifelse(x1$year %in% seq(2007,2010,1),1,0))
x1$dem_senate <- ifelse(x1$year %in% seq(1987,1994,1),1, ifelse(x1$year == c(1980,2002), 1, ifelse(x1$year %in% seq(2007,2014,1),1,0)))
x1$congress_type <- ifelse(x1$dem_house == 1 & x1$dem_senate == 1, "Democratic Congress",ifelse(x1$dem_house == 0 & x1$dem_senate == 0, "Republican Congress", "Split Congress"))

x <- nes
x$cong_approval_dich <- as.numeric(as.character(x$cong_approval_dich))
anes_approval <- ddply(x,.(year),summarize,approval = weighted.mean(cong_approval_dich,w=weight, na.rm=T))

x <- cces
x$cong_approval_binary <- ifelse(x$cong_approval_binary == "Approve",1,ifelse(x$cong_approval_binary == "Disapprove",0,NA))
cces_approval <- ddply(x,.(year),summarize,approval = weighted.mean(cong_approval_binary,w=weight, na.rm=T))

anes_approval$data <- "ANES"
cces_approval$data <- "CCES"
x <- rbind(anes_approval,cces_approval)
x$year <- as.numeric(x$year)
x$approval <- as.numeric(x$approval) * 100
x <-  ddply(x,.(year),summarize,approval = mean(approval, na.rm=T))
x$pid3 <- "Global Mean"
x$dem_house <- ifelse(x$year %in% seq(1980,1994,1),1,ifelse(x$year %in% seq(2007,2010,1),1,0))
x$dem_senate <- ifelse(x$year %in% seq(1987,1994,1),1, ifelse(x$year == c(1980,2002), 1, ifelse(x$year %in% seq(2007,2014,1),1,0)))
x$congress_type <- ifelse(x$dem_house == 1 & x$dem_senate == 1, "Democratic Congress",ifelse(x$dem_house == 0 & x$dem_senate == 0, "Republican Congress", "Split Congress"))

x1 <- rbind(x1,x)
x2 <- subset(x1,x1$pid3 != "Independent")
x2$pid3 <- factor(x2$pid3,levels=c("Republican","Global Mean","Democrat"),labels=c("Republicans","Global Mean","Democrats"))
plot <- ggplot(x2,aes(x=year,y=approval,color=pid3,linetype=pid3)) + geom_line(size=1) + scale_y_continuous(breaks=seq(0,75,5),"Weighted Percentage of Congressional Approval",labels=c("0%","5%","10%","15%","20%","25%","30%","35%","40%","45%","50%","55%","60%","65%","70%","75%")) + scale_x_continuous("",breaks=seq(1980,2016,2)) + theme_minimal() + theme(legend.position="bottom", legend.direction = "horizontal", legend.box.just = "bottom") + scale_color_manual("",values=c("red","purple","blue")) + scale_linetype_manual("",values=c("dashed", "solid","dotted")) + annotate(geom = "text", x = 1981.5, y = 10, label = "Split Congress", color = "purple",angle=90) + geom_vline(xintercept=c(1981,1986,1994,2001,2002,2006,2010,2014),linetype = "dashed") + annotate(geom = "text", x = 1986.5, y = 10, label = "Dem Congress", color = "blue",angle=90) + annotate(geom = "text", x = 1994.5, y = 10, label = "GOP Congress", color = "red",angle=90) + annotate(geom = "text", x = 2001.5, y = 10, label = "Split Congress", color = "purple",angle=90) + annotate(geom = "text", x = 2002.5, y = 10, label = "GOP Congress", color = "red",angle=90) + annotate(geom = "text", x = 2006.5, y = 10, label = "Dem Congress", color = "blue",angle=90) + annotate(geom = "text", x = 2010.5, y = 60, label = "Split Congress", color = "purple",angle=90) + annotate(geom = "text", x = 2014.5, y = 60, label = "GOP Congress", color = "red",angle=90) + labs(caption=  "Data: Cooperative Congressional Election Study & American National Election Study.")
#ggsave(plot,file="approval_by_pid3.png", width = (12/1.5), height = 6, units = "in") # Figure 1

rm(anes_approval,cces_approval,plot,x,x1,x2)

###### Figure 4A/B & Figure 5A/B: CCES Pooled Models of Congressional Approval ###### 

# Proximity: Marginal Effects (ALDMCK)

summary(cces_model_aldmck <- glm(cong_approval_binary ~ pid3*congress_type + dem_proximity_rule_aldmck_cong_pty_placement*congress_type + comprehenesive_knowledge_scale_cut + pres_approval_num + retro_econ_eval_num + cong_delegation_approval + aldmck_proximity_betwen_mc_citizen, data=cces, weights=weight, family = binomial(link = "logit")))
coeftest(cces_model_aldmck, vcov = cluster.vcov(cces_model_aldmck, cluster=cces$year_district_id))

cces_model_margins_aldmck <- summary(margins(cces_model_aldmck,variables=c("pid3","dem_proximity_rule_aldmck_cong_pty_placement"), at = list(congress_type=c("Republican Congress","Split Congress","Democratic Congress")), type="response", change="sd", vcov=cluster.vcov(cces_model_aldmck, cluster=cces$year_district_id)))
cces_model_margins_aldmck$congress_type <- factor(cces_model_margins_aldmck$congress_type,levels=c("Republican Congress", "Split Congress", "Democratic Congress"))
cces_model_margins_aldmck$ame_label <- round(cces_model_margins_aldmck$AME,2)

cces_model_margins_aldmck$ame_label <- ifelse(cces_model_margins_aldmck$p < 0.01,paste(cces_model_margins_aldmck$ame_label,"***",sep=""),ifelse(cces_model_margins_aldmck$p > 0.01 & cces_model_margins_aldmck$p < 0.05,paste(cces_model_margins_aldmck$ame_label,"**",sep=""),ifelse(cces_model_margins_aldmck$p > 0.05 & cces_model_margins_aldmck$p < 0.10,paste(cces_model_margins_aldmck$ame_label,"*",sep=""),cces_model_margins_aldmck$ame_label)))

# Probabilities: Marginal Effects (ALDMCK)

summary(cces_model_aldmck <- glm(cong_approval_binary ~ dem_proximity_rule_aldmck_cong_pty_placement*congress_type + pid3*congress_type + comprehenesive_knowledge_scale_cut + pres_approval_num + retro_econ_eval_num + cong_delegation_approval + aldmck_proximity_betwen_mc_citizen, data=cces, weights=weight, family = binomial(link = "logit")))

plot(cces_model_probs_aldmck <- effect("dem_proximity_rule_aldmck_cong_pty_placement*congress_type", se=TRUE, mod = cces_model_aldmck, confidence.level = 0.95,xlevels=list(dem_proximity_rule_aldmck_cong_pty_placement=unique(cces$dem_proximity_rule_aldmck_cong_pty_placement),vcov = cluster.vcov(cces_model_aldmck, cluster=cces$year_district_id))))
cces_model_probs_aldmck <- data.frame(cces_model_probs_aldmck)
cces_model_probs_aldmck$congress_type <- factor(cces_model_probs_aldmck$congress_type,levels=c("Democratic Congress", "Split Congress", "Republican Congress"))

rm(cces_model_aldmck)

# Proximity: Marginal Effects (Joint Scaling)

summary(cces_model_joint_scaling <- glm(cong_approval_binary ~ pid3*congress_type + dem_proximity_rule_joint_scaling*congress_type + comprehenesive_knowledge_scale_cut + pres_approval_num + retro_econ_eval_num + cong_delegation_approval + jointscaling_proximity_between_mc_citizen, data=cces, weights=weight, family = binomial(link = "logit")))
coeftest(cces_model_joint_scaling, vcov = cluster.vcov(cces_model_joint_scaling, cluster=cces$year_district_id))

cces_model_margins_joint_scaling <- summary(margins(cces_model_joint_scaling, variables=c("dem_proximity_rule_joint_scaling"), at = list(congress_type=c("Republican Congress","Split Congress","Democratic Congress")), type="response", change="sd", vcov=cluster.vcov(cces_model_joint_scaling, cluster=cces$year_district_id)))
cces_model_margins_joint_scaling$congress_type <- factor(cces_model_margins_joint_scaling$congress_type,levels=c("Democratic Congress", "Split Congress", "Republican Congress"))
cces_model_margins_joint_scaling$ame_label <- round(cces_model_margins_joint_scaling$AME,2)

cces_model_margins_joint_scaling$ame_label <- ifelse(cces_model_margins_joint_scaling$p < 0.01,paste(cces_model_margins_joint_scaling$ame_label,"***",sep=""),ifelse(cces_model_margins_joint_scaling$p > 0.01 & cces_model_margins_joint_scaling$p < 0.05,paste(cces_model_margins_joint_scaling$ame_label,"**",sep=""),ifelse(cces_model_margins_joint_scaling$p > 0.05 & cces_model_margins_joint_scaling$p < 0.10,paste(cces_model_margins_joint_scaling$ame_label,"*",sep=""),cces_model_margins_joint_scaling$ame_label)))

# Probabilities: Marginal Effects (Joint Scaling)

summary(cces_model_joint_scaling <- glm(cong_approval_binary ~ dem_proximity_rule_joint_scaling*congress_type +  pid3*congress_type + comprehenesive_knowledge_scale_cut + pres_approval_num + retro_econ_eval_num + cong_delegation_approval + jointscaling_proximity_between_mc_citizen, data=cces, weights=weight, family = binomial(link = "logit")))

plot(cces_model_probs_joint_scaling <- effect("dem_proximity_rule_joint_scaling*congress_type", se=TRUE, mod = cces_model_joint_scaling, confidence.level = 0.95,xlevels=list(dem_proximity_rule_joint_scaling=seq(-2.56,2.56,0.001),vcov=cluster.vcov(cces_model_joint_scaling, cluster=cces$year_district_id))))

cces_model_probs_joint_scaling <- data.frame(cces_model_probs_joint_scaling)
cces_model_probs_joint_scaling$congress_type <- factor(cces_model_probs_joint_scaling$congress_type,levels=c("Democratic Congress", "Split Congress", "Republican Congress"))

colnames(cces_model_probs_aldmck)[1] <- "proximity"
colnames(cces_model_probs_joint_scaling)[1] <- "proximity"

cces_model_probs_aldmck$model <- "Perceptual-Based Measure Model"
cces_model_probs_joint_scaling$model <- "Roll-Call Based Measure Model"

probs <- rbind(cces_model_probs_aldmck,cces_model_probs_joint_scaling)

x <- subset(probs,probs$model %in% "Perceptual-Based Measure Model")
x1 <- subset(cces,select=c(year,district,cong_approval_binary,dem_proximity_rule_aldmck_cong_pty_placement,congress_type,pid3,congress_type,comprehenesive_knowledge_scale_cut,pres_approval_num,retro_econ_eval_num,cong_delegation_approval,aldmck_proximity_betwen_mc_citizen,weight))
x1 <- na.omit(x1)

plot1 <- ggplot(x,aes(x=proximity,y=fit,ymin=lower,ymax=upper,color=congress_type,fill=congress_type)) + geom_line() + geom_ribbon(alpha=0.2) + theme_minimal() + scale_x_continuous("",breaks=c(min(x$proximity,na.rm=T),-0.706,-0.238,0.23,0.698,max(x$proximity,na.rm=T)),labels=c("Closer \nto \nCong \nGOP","","","","","Closer \nto \nCong \nDems")) + scale_y_continuous("Predicted Probability of Congressional Approval",breaks=seq(0,1,0.05),limits=c(0.05,0.30)) + facet_wrap(~model) + scale_color_manual("Congress",values=c("blue","purple","red")) + scale_fill_manual("Congress",values=c("blue","purple","red")) + theme(legend.position = "bottom")  + theme(panel.spacing = unit(2, "lines")) #+ geom_rug(x1,mapping=aes(x=dem_proximity_rule_aldmck_cong_pty_placement),inherit.aes = F,sides="b",color="gray")

legend <- get_legend(plot1)

plot1 <- plot1 + theme(legend.position = "none") 

x <- subset(probs,probs$model %in% "Roll-Call Based Measure Model")
x1 <- subset(cces,select=c(year,district,cong_approval_binary,dem_proximity_rule_joint_scaling,congress_type,pid3,congress_type,comprehenesive_knowledge_scale_cut,pres_approval_num,retro_econ_eval_num,cong_delegation_approval,jointscaling_proximity_between_mc_citizen,weight))
x1 <- na.omit(x1)

plot2 <- ggplot(x,aes(x=proximity,y=fit,ymin=lower,ymax=upper,color=congress_type,fill=congress_type)) + geom_line() + geom_ribbon(alpha=0.2) + theme_minimal() + scale_x_continuous("",breaks=c(min(x$proximity,na.rm=T),-1.54,-0.512,0.512,1.54,max(x$proximity,na.rm=T)),labels=c("Closer \nto \nCong \nGOP","","","","","Closer \nto \nCong \nDems")) + scale_y_continuous("",breaks=seq(0,1,0.05),limits=c(0.05,0.45)) + facet_wrap(~model) + scale_color_manual("Congress",values=c("blue","purple","red")) + scale_fill_manual("Congress",values=c("blue","purple","red")) + theme(legend.position = "none") + theme(panel.spacing = unit(2, "lines")) #+ geom_rug(x1,mapping=aes(x=dem_proximity_rule_joint_scaling),inherit.aes = F,sides="b",color="gray")

blankPlot <- ggplot(x,aes(x=proximity,y=fit,ymin=lower,ymax=upper,color=congress_type,fill=congress_type)) + scale_x_continuous("Ideological Proximity Between Republican & Democratic Congressional Parties") + theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.y=element_blank(),legend.position="none",panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),plot.background=element_blank())

#ggsave("predicted_probabilities_pooled_proximity.png",grid.arrange(plot1, plot2,blankPlot,legend,ncol=2, nrow = 3, layout_matrix = rbind(c(1,2), c(3,3),c(4,4)), widths = c(2.7, 2.7), heights = c(2.5, 0.2,0.2)),width = 8, height =5.43, units = "in")

# Marginal Effects: Proximity 

cces_model_margins_aldmck$model <- "Perceptual-Based Measure Model"
cces_model_margins_joint_scaling$model <- "Roll-Call Based Measure Model"

x <- rbind(cces_model_margins_aldmck,cces_model_margins_joint_scaling)
x <- subset(x,x$factor %in% c("dem_proximity_rule_aldmck_cong_pty_placement","dem_proximity_rule_joint_scaling"))

plot <- ggplot(x,aes(x=congress_type, y=AME, ymin=lower, ymax=upper,label=ame_label)) + geom_pointrange(color=c("red","purple","blue","red","purple","blue")) + geom_point(shape=21, fill="white",color=c("red","purple","blue","red","purple","blue")) + theme_minimal() + scale_y_continuous("Marginal Effect of 1 SD Change in Proximity on P(Approval)",breaks=round(seq(-0.20,0.20,0.05),2)) + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + scale_x_discrete("") + geom_text(size=3.5,hjust = 0, nudge_x = 0.175,color=c("red","purple","blue","red","purple","blue")) + theme(axis.text.y = element_text(colour = c("red","purple","blue"))) + labs(caption="* p < 0.1; ** p < 0.05; *** p < 0.01") + coord_flip() + facet_wrap(~model,nrow=2)
#ggsave(file="marginal_effects_pooled_proximity.png", plot, width = 8, height = 5.43, units = "in")

# Partisanship: Marginal Effects (ALDMCK)

summary(cces_model_aldmck <- glm(cong_approval_binary ~ pid3*congress_type + dem_proximity_rule_aldmck_cong_pty_placement*congress_type + comprehenesive_knowledge_scale_cut + pres_approval_num + retro_econ_eval_num + cong_delegation_approval + aldmck_proximity_betwen_mc_citizen, data=cces, weights=weight, family = binomial(link = "logit")))
coeftest(cces_model_aldmck, vcov = cluster.vcov(cces_model_aldmck, cluster=cces$year_district_id))

cces_model_margins_aldmck <- summary(margins(cces_model_aldmck,variables=c("pid3"), at = list(congress_type=c("Republican Congress","Split Congress","Democratic Congress")), type="response", change="minmax", vcov=cluster.vcov(cces_model_aldmck, cluster=cces$year_district_id)))
cces_model_margins_aldmck$congress_type <- factor(cces_model_margins_aldmck$congress_type,levels=c("Republican Congress", "Split Congress", "Democratic Congress"))
cces_model_margins_aldmck$ame_label <- round(cces_model_margins_aldmck$AME,2)

cces_model_margins_aldmck$ame_label <- ifelse(cces_model_margins_aldmck$p < 0.01,paste(cces_model_margins_aldmck$ame_label,"***",sep=""),ifelse(cces_model_margins_aldmck$p > 0.01 & cces_model_margins_aldmck$p < 0.05,paste(cces_model_margins_aldmck$ame_label,"**",sep=""),ifelse(cces_model_margins_aldmck$p > 0.05 & cces_model_margins_aldmck$p < 0.10,paste(cces_model_margins_aldmck$ame_label,"*",sep=""),cces_model_margins_aldmck$ame_label)))

# Probabilities: Marginal Effects (ALDMCK)

summary(cces_model_aldmck <- glm(cong_approval_binary ~  pid3*congress_type + dem_proximity_rule_aldmck_cong_pty_placement*congress_type + comprehenesive_knowledge_scale_cut + pres_approval_num + retro_econ_eval_num + cong_delegation_approval + aldmck_proximity_betwen_mc_citizen, data=cces, weights=weight, family = binomial(link = "logit")))

plot(cces_model_probs_aldmck <- effect("pid3*congress_type", se=TRUE, mod = cces_model_aldmck, confidence.level = 0.95,xlevels=list(pid3=c("Republican","Independent","Democrat"),vcov = cluster.vcov(cces_model_aldmck, cluster=cces$year_district_id))))
cces_model_probs_aldmck <- data.frame(cces_model_probs_aldmck)
cces_model_probs_aldmck$congress_type <- factor(cces_model_probs_aldmck$congress_type,levels=c("Democratic Congress", "Split Congress", "Republican Congress"))

rm(cces_model_aldmck)

# Proximity: Marginal Effects (Joint Scaling)

summary(cces_model_joint_scaling <- glm(cong_approval_binary ~ pid3*congress_type + dem_proximity_rule_joint_scaling*congress_type + comprehenesive_knowledge_scale_cut + pres_approval_num + retro_econ_eval_num + cong_delegation_approval + jointscaling_proximity_between_mc_citizen, data=cces, weights=weight, family = binomial(link = "logit")))
coeftest(cces_model_joint_scaling, vcov = cluster.vcov(cces_model_joint_scaling, cluster=cces$year_district_id))

cces_model_margins_joint_scaling <- summary(margins(cces_model_joint_scaling, variables=c("pid3"), at = list(congress_type=c("Republican Congress","Split Congress","Democratic Congress")), type="response", change="minmax", vcov=cluster.vcov(cces_model_joint_scaling, cluster=cces$year_district_id)))
cces_model_margins_joint_scaling$congress_type <- factor(cces_model_margins_joint_scaling$congress_type,levels=c("Democratic Congress", "Split Congress", "Republican Congress"))
cces_model_margins_joint_scaling$ame_label <- round(cces_model_margins_joint_scaling$AME,2)

cces_model_margins_joint_scaling$ame_label <- ifelse(cces_model_margins_joint_scaling$p < 0.01,paste(cces_model_margins_joint_scaling$ame_label,"***",sep=""),ifelse(cces_model_margins_joint_scaling$p > 0.01 & cces_model_margins_joint_scaling$p < 0.05,paste(cces_model_margins_joint_scaling$ame_label,"**",sep=""),ifelse(cces_model_margins_joint_scaling$p > 0.05 & cces_model_margins_joint_scaling$p < 0.10,paste(cces_model_margins_joint_scaling$ame_label,"*",sep=""),cces_model_margins_joint_scaling$ame_label)))

# Probabilities: Marginal Effects (Joint Scaling)

summary(cces_model_joint_scaling <- glm(cong_approval_binary ~ pid3*congress_type + dem_proximity_rule_joint_scaling*congress_type +  comprehenesive_knowledge_scale_cut + pres_approval_num + retro_econ_eval_num + cong_delegation_approval + jointscaling_proximity_between_mc_citizen, data=cces, weights=weight, family = binomial(link = "logit")))

plot(cces_model_probs_joint_scaling <- effect("pid3*congress_type", se=TRUE, mod = cces_model_joint_scaling, confidence.level = 0.95,xlevels=list(pid3=c("Republican","Independent","Democrat"),cluster.vcov(cces_model_joint_scaling, cluster=cces$year_district_id))))

cces_model_probs_joint_scaling <- data.frame(cces_model_probs_joint_scaling)
cces_model_probs_joint_scaling$congress_type <- factor(cces_model_probs_joint_scaling$congress_type,levels=c("Democratic Congress", "Split Congress", "Republican Congress"))

colnames(cces_model_probs_aldmck)[1] <- "pid3"
colnames(cces_model_probs_joint_scaling)[1] <- "pid3"

cces_model_probs_aldmck$model <- "Perceptual-Based Measure Model"
cces_model_probs_joint_scaling$model <- "Roll-Call Based Measure Model"

probs <- rbind(cces_model_probs_aldmck,cces_model_probs_joint_scaling)

plot <- ggplot(probs,aes(x=pid3, y=fit, ymin=lower, ymax=upper, shape=congress_type, label=round(probs$fit,2),group=congress_type,color=congress_type)) + theme(legend.position="bottom") + facet_wrap(~model) + geom_errorbar(width=0.2,size=1,position= position_dodge(width=0.45))  + geom_point(size=4, fill="white",position= position_dodge(width=0.45)) + scale_y_continuous("Predicted Probability of Congressional Approval",breaks=seq(0,1,0.02)) + theme_minimal() + scale_shape_manual("",values=rep(22:21,9)) + scale_color_manual("",values=c("blue","purple","red"),guide=F) + theme(legend.position="bottom") + scale_x_discrete("",labels=c("Democratic \nPartisan","Independent \nPartisan","Republican \nPartisan"))

# Marginal Effects: Partisanship

cces_model_margins_aldmck$model <- "Perceptual-Based Measure Model"
cces_model_margins_joint_scaling$model <- "Roll-Call Based Measure Model"

x <- rbind(cces_model_margins_aldmck,cces_model_margins_joint_scaling)
x <- subset(x,x$factor %in% "pid3Democrat")

plot <- ggplot(x,aes(x=congress_type, y=AME, ymin=lower, ymax=upper,label=ame_label)) + geom_pointrange(color=c("red","purple","blue","red","purple","blue")) + geom_point(shape=21, fill="white",color=c("red","purple","blue","red","purple","blue")) + theme_minimal() + scale_y_continuous("Marginal Effect of Democratic Partisanship on P(Approval)",breaks=round(seq(-0.20,0.20,0.05),2)) + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + scale_x_discrete("") + geom_text(size=3.5,hjust = 0, nudge_x = 0.175,color=c("red","purple","blue","red","purple","blue")) + theme(axis.text.y = element_text(colour = c("red","purple","blue"))) + labs(caption="* p < 0.1; ** p < 0.05; *** p < 0.01") + coord_flip() + facet_wrap(~model,nrow=2)
#ggsave(file="marginal_effects_pooled_pid3.png", plot, width = 8, height = 5.43, units = "in")

rm(cces_model_aldmck,cces_model_aldmck_cong_pty_placement,cces_model_joint_scaling,cces_model_margins_aldmck,cces_model_margins_aldmck_cong_pty,cces_model_margins_joint_scaling,g,plot,footnote,plot,plot1,plot2,probx,x,x1,probs,cces_model_joint_scaling,blankPlot,cces_model_margins_aldmck,cces_model_margins_joint_scaling,cces_model_probs_aldmck,cces_model_probs_joint_scaling,legend)

###### Figure 6A/B: CCES Panel Models of Congressional Approval ###### 

summary(panel_model_10 <- glm(cong_approval_binary_10 ~ dem_proximity_rule_joint_scaling + approval_mcs_senators_scale_10 + knowledge_scale_10_cut + obama_approval_binary_10 + pid3_10 + retro_econ_eval_10 + jointscaling_proximity_between_mc_citizen, data=cces_panel, weights=weight, family = binomial(link = "logit")))
coeftest(panel_model_10,vcov=vcovBS(panel_model_10,R=1000))
panel_model_10_margins <- summary(margins(panel_model_10, variables=c("dem_proximity_rule_joint_scaling","pid3_10"), type="response", change="sd",vcov=vcovBS(panel_model_10,R=1000)))

summary(panel_model_12 <- glm(cong_approval_binary_12 ~ dem_proximity_rule_joint_scaling + cong_approval_binary_10 + approval_mcs_senators_scale_12 + knowledge_scale_12_cut + obama_approval_binary_12 + pid3_10 + retro_econ_eval_12 + jointscaling_proximity_between_mc_citizen, data=cces_panel, weights=weight, family = binomial(link = "logit")))
coeftest(panel_model_12,vcov=vcovBS(panel_model_12,R=1000))
panel_model_12_margins <- summary(margins(panel_model_12, variables=c("dem_proximity_rule_joint_scaling","pid3_10"), type="response", change="sd",vcov=vcovBS(panel_model_12,R=1000)))

summary(panel_model_14 <- glm(cong_approval_binary_14 ~ dem_proximity_rule_joint_scaling + cong_approval_binary_12 + approval_mcs_senators_scale_14 + knowledge_scale_14_cut + obama_approval_binary_14 + pid3_10 + retro_econ_eval_14 + jointscaling_proximity_between_mc_citizen, data=cces_panel, weights=weight, family = binomial(link = "logit")))
coeftest(panel_model_14,vcov=vcovBS(panel_model_14,R=1000))
panel_model_14_margins <- summary(margins(panel_model_14, variables=c("dem_proximity_rule_joint_scaling","pid3_10"), type="response", change="sd",vcov=vcovBS(panel_model_14,R=1000)))

panel_model_10_margins$year <- "2010"
panel_model_12_margins$year <- "2012"
panel_model_14_margins$year <- "2014"

panel_margins <- rbind(panel_model_10_margins,panel_model_12_margins,panel_model_14_margins)
panel_margins$ame_label <- round(panel_margins$AME,2)

panel_margins$ame_label <- ifelse(panel_margins$p < 0.01,paste(panel_margins$ame_label,"***",sep=""),ifelse(panel_margins$p > 0.01 & panel_margins$p < 0.05,paste(panel_margins$ame_label,"**",sep=""),ifelse(panel_margins$p > 0.05 & panel_margins$p < 0.10,paste(panel_margins$ame_label,"*",sep=""),panel_margins$ame_label)))

plot <- ggplot(subset(panel_margins,panel_margins$factor == "dem_proximity_rule_joint_scaling"),aes(x=year, y=AME, ymin=lower, ymax=upper,label=ame_label)) + geom_errorbar(width=0.2,size=1,color=c("blue","purple","purple")) + geom_point(size=4, shape=21, fill="white",color=c("blue","purple","purple")) + theme_minimal() + scale_y_continuous("Marginal Effect of 1 SD Change in 2010 Proximity on P(Approval)",breaks=round(seq(-0.20,0.20,0.05),2)) + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + scale_x_discrete("Panel Survey Wave",labels=c("2010 \nDemocratic Congress","2012 \nSplit Congress","2014 \nSplit Congress")) + geom_text(size=3.5,hjust =0,nudge_x = 0.175,color=c("blue","purple","purple")) + theme(axis.text.x = element_text(colour = c("blue","purple","purple"))) + labs(caption="* p < 0.1; ** p < 0.05; *** p < 0.01")
#ggsave(plot,file="cces_panel_proximity.png", width = 6.82, height = 5.5, units = "in") # Figure 6A

summary(panel_model_10 <- glm(cong_approval_binary_10 ~ dem_proximity_rule_joint_scaling + approval_mcs_senators_scale_10 + knowledge_scale_10_cut + obama_approval_binary_10 + pid3_10 + retro_econ_eval_10 + jointscaling_proximity_between_mc_citizen, data=cces_panel, weights=weight, family = binomial(link = "logit")))
coeftest(panel_model_10,vcov=vcovBS(panel_model_10,R=1000))

panel_model_10_margins <- summary(margins(panel_model_10, variables=c("dem_proximity_rule_joint_scaling","pid3_10"), type="response", change="minmax",vcov=vcovBS(panel_model_10,R=1000)))

summary(panel_model_12 <- glm(cong_approval_binary_12 ~ dem_proximity_rule_joint_scaling + cong_approval_binary_10 + approval_mcs_senators_scale_12 + knowledge_scale_12_cut + obama_approval_binary_12 + pid3_10 + retro_econ_eval_12 + jointscaling_proximity_between_mc_citizen, data=cces_panel, weights=weight, family = binomial(link = "logit")))
coeftest(panel_model_12,vcov=vcovBS(panel_model_12,R=1000))

panel_model_12_margins <- summary(margins(panel_model_12, variables=c("dem_proximity_rule_joint_scaling","pid3_10"), type="response", change="minmax",vcov=vcovBS(panel_model_12,R=1000)))

summary(panel_model_14 <- glm(cong_approval_binary_14 ~ dem_proximity_rule_joint_scaling + cong_approval_binary_12 + approval_mcs_senators_scale_14 + knowledge_scale_14_cut + obama_approval_binary_14 + pid3_10 + retro_econ_eval_14 + jointscaling_proximity_between_mc_citizen, data=cces_panel, weights=weight, family = binomial(link = "logit")))
coeftest(panel_model_14,vcov=vcovBS(panel_model_14,R=1000))

panel_model_14_margins <- summary(margins(panel_model_14, variables=c("dem_proximity_rule_joint_scaling","pid3_10"), type="response", change="minmax",vcov=vcovBS(panel_model_14,R=1000)))

panel_model_10_margins$year <- "2010"
panel_model_12_margins$year <- "2012"
panel_model_14_margins$year <- "2014"

panel_margins <- rbind(panel_model_10_margins,panel_model_12_margins,panel_model_14_margins)
panel_margins$ame_label <- round(panel_margins$AME,2)

panel_margins$ame_label <- ifelse(panel_margins$p < 0.01,paste(panel_margins$ame_label,"***",sep=""),ifelse(panel_margins$p > 0.01 & panel_margins$p < 0.05,paste(panel_margins$ame_label,"**",sep=""),ifelse(panel_margins$p > 0.05 & panel_margins$p < 0.10,paste(panel_margins$ame_label,"*",sep=""),panel_margins$ame_label)))

plot <- ggplot(subset(panel_margins,panel_margins$factor == "pid3_10D"),aes(x=year, y=AME, ymin=lower, ymax=upper,label=ame_label)) + geom_errorbar(width=0.2,size=1,color=c("blue","purple","purple")) + geom_point(size=4, shape=21, fill="white",color=c("blue","purple","purple")) + theme_minimal() + scale_y_continuous("Marginal Effect of Democratic Partisanship in 2010 on P(Approval)",breaks=round(seq(-0.20,0.20,0.05),2)) + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + scale_x_discrete("Panel Survey Wave",labels=c("2010 \nDemocratic Congress","2012 \nSplit Congress","2014 \nSplit Congress")) + geom_text(size=3.5,hjust =0,nudge_x = 0.175,color=c("blue","purple","purple")) + theme(axis.text.x = element_text(colour = c("blue","purple","purple"))) + labs(caption="* p < 0.1; ** p < 0.05; *** p < 0.01")
#ggsave(plot,file="cces_panel_pid.png", width = 6.82, height = 5.5, units = "in") # Figure 6B

rm(g,panel_margins,panel_model_10,panel_model_10_margins,panel_model_12,panel_model_12_margins,panel_model_14,panel_model_14_margins,plot,footnote)

###### Isolating Ideological Effects: Partisan-Models in Figures 8A/B ###### 

panel_margins_by_party <- list()
for(i in c("I")){
  x <- subset(cces_panel,cces_panel$pid3_10 == i)
  
  summary(panel_model_10 <- glm(cong_approval_binary_10 ~ dem_proximity_rule_joint_scaling + approval_mcs_senators_scale_10 + knowledge_scale_10_cut + retro_econ_eval_10 + jointscaling_proximity_between_mc_citizen, data=x, weights=weight, family = binomial(link = "logit")))
  panel_model_10_margins <- summary(margins(panel_model_10, variables=c("dem_proximity_rule_joint_scaling"), type="response", change="sd",vcov=vcovBS(panel_model_10,R=1000)))
  
  summary(panel_model_12 <- glm(cong_approval_binary_12 ~ dem_proximity_rule_joint_scaling + cong_approval_binary_10 + approval_mcs_senators_scale_12 + knowledge_scale_12_cut + retro_econ_eval_12 + jointscaling_proximity_between_mc_citizen, data=x, weights=weight, family = binomial(link = "logit")))
  panel_model_12_margins <- summary(margins(panel_model_12, variables=c("dem_proximity_rule_joint_scaling"), type="response", change="sd",vcov=vcovBS(panel_model_12,R=1000))) #obama_approval_binary_12
  
  summary(panel_model_14 <- glm(cong_approval_binary_14 ~ dem_proximity_rule_joint_scaling + cong_approval_binary_12 + approval_mcs_senators_scale_14 + knowledge_scale_14_cut + retro_econ_eval_14 + jointscaling_proximity_between_mc_citizen, data=x, weights=weight, family = binomial(link = "logit")))
  panel_model_14_margins <- summary(margins(panel_model_14, variables=c("dem_proximity_rule_joint_scaling"), type="response", change="sd",vcov=vcovBS(panel_model_14,R=1000))) #obama_approval_binary_14
  
  # Proximity Original
  
  panel_model_10_margins$year <- "2010"
  panel_model_12_margins$year <- "2012"
  panel_model_14_margins$year <- "2014"
  
  panel_margins <- rbind(panel_model_10_margins,panel_model_12_margins,panel_model_14_margins)
  panel_margins <- data.frame(panel_margins)
  panel_margins$ame_label <- round(panel_margins$AME,2)
  panel_margins$party <- i
  
  panel_margins_by_party[[i]] <- panel_margins
}

for(i in c("R","D")){
  x <- subset(cces_panel,cces_panel$pid3_10 == i)
  
  summary(panel_model_10 <- glm(cong_approval_binary_10 ~ dem_proximity_rule_joint_scaling + approval_mcs_senators_scale_10 + knowledge_scale_10_cut + retro_econ_eval_10 + jointscaling_proximity_between_mc_citizen, data=x, weights=weight, family = binomial(link = "logit")))
  panel_model_10_margins <- summary(margins(panel_model_10, variables=c("dem_proximity_rule_joint_scaling"), type="response", change="sd",vcov=vcovBS(panel_model_10,R=1000)))
  
  summary(panel_model_12 <- glm(cong_approval_binary_12 ~ dem_proximity_rule_joint_scaling + cong_approval_binary_10 + approval_mcs_senators_scale_12 + knowledge_scale_12_cut + retro_econ_eval_12 + jointscaling_proximity_between_mc_citizen, data=x, weights=weight, family = binomial(link = "logit")))
  panel_model_12_margins <- summary(margins(panel_model_12, variables=c("dem_proximity_rule_joint_scaling"), type="response", change="sd",vcov=vcovBS(panel_model_12,R=1000)))
  
  summary(panel_model_14 <- glm(cong_approval_binary_14 ~ dem_proximity_rule_joint_scaling + cong_approval_binary_12 + approval_mcs_senators_scale_14 + knowledge_scale_14_cut + retro_econ_eval_14 + jointscaling_proximity_between_mc_citizen, data=x, weights=weight, family = binomial(link = "logit")))
  panel_model_14_margins <- summary(margins(panel_model_14, variables=c("dem_proximity_rule_joint_scaling"), type="response", change="sd",vcov=vcovBS(panel_model_14,R=1000))) 
  
  # Proximity Original
  
  panel_model_10_margins$year <- "2010"
  panel_model_12_margins$year <- "2012"
  panel_model_14_margins$year <- "2014"
  
  panel_margins <- rbind(panel_model_10_margins,panel_model_12_margins,panel_model_14_margins)
  panel_margins <- data.frame(panel_margins)
  panel_margins$ame_label <- round(panel_margins$AME,2)
  panel_margins$party <- i
  
  panel_margins_by_party[[i]] <- panel_margins
}
panel_margins_by_party <- ldply(panel_margins_by_party,data.frame)

panel_margins_by_party$party_label <- ifelse(panel_margins_by_party$party == "D","Democratic Partisanship Model",ifelse(panel_margins_by_party$party == "R","Republican Partisanship Model",ifelse(panel_margins_by_party$party == "I","Independent Partisanship Model",NA)))
panel_margins_by_party$party_label <- factor(panel_margins_by_party$party_label,levels=c("Democratic Partisanship Model","Independent Partisanship Model","Republican Partisanship Model"))

panel_margins_by_party$ame_label <- ifelse(panel_margins_by_party$p < 0.01,paste(panel_margins_by_party$ame_label,"***",sep=""),ifelse(panel_margins_by_party$p > 0.01 & panel_margins_by_party$p < 0.05,paste(panel_margins_by_party$ame_label,"**",sep=""),ifelse(panel_margins_by_party$p > 0.05 & panel_margins_by_party$p < 0.10,paste(panel_margins_by_party$ame_label,"*",sep=""),panel_margins_by_party$ame_label)))

plot <- ggplot(panel_margins_by_party,aes(x=year, y=AME, ymin=lower, ymax=upper,label=ame_label)) + geom_errorbar(width=0.2,size=1,color=c("purple","purple","purple","red","red","red","blue","blue","blue")) + geom_point(size=4, shape=21, fill="white",color=c("purple","purple","purple","red","red","red","blue","blue","blue")) + theme_minimal() + scale_y_continuous("Marginal Effect of 1 SD Change in 2010 Proximity on P(Approval)",breaks=round(seq(-0.20,0.20,0.05),2)) + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + scale_x_discrete("Panel Survey Wave",labels=c("2010 \nDemocratic \nCongress","2012 \nSplit \nCongress","2014 \nSplit \nCongress")) + facet_wrap(~party_label) + geom_text(size=3.05,hjust =0,nudge_x = 0.15,color=c("purple","purple","purple","red","red","red","blue","blue","blue")) + labs(caption="* p < 0.1; ** p < 0.05; *** p < 0.01")
#ggsave(plot,file="cces_panel_proximity_by_party.png", width = 9.5, height = 5.5, units = "in") # Figure 8B

cces_model_margins_aldmck_by_party <- list()
for(i in c("Republican","Democrat")){
  x <- subset(cces,cces$pid3 == i)
  
  summary(cces_model_aldmck <- glm(cong_approval_binary ~ dem_proximity_rule_aldmck_cong_pty_placement*congress_type + comprehenesive_knowledge_scale_cut + pres_approval_num + retro_econ_eval_num + cong_delegation_approval + aldmck_proximity_betwen_mc_citizen , data=x, weights=weight, family = binomial(link = "logit")))
  cces_model_margins_aldmck <- summary(margins(cces_model_aldmck,variables=c("dem_proximity_rule_aldmck_cong_pty_placement"), at = list(congress_type=c("Republican Congress","Split Congress","Democratic Congress")), type="response", change="sd", vcov=cluster.vcov(cces_model_aldmck, cluster=x$year_district_id)))
  cces_model_margins_aldmck <- data.frame(cces_model_margins_aldmck)
  cces_model_margins_aldmck$congress_type <- factor(cces_model_margins_aldmck$congress_type,levels=c("Democratic Congress", "Split Congress", "Republican Congress"))
  cces_model_margins_aldmck$ame_label <- round(cces_model_margins_aldmck$AME,2)
  cces_model_margins_aldmck$party <- i
  cces_model_margins_aldmck_by_party[[i]] <- cces_model_margins_aldmck
}
for(i in c("Independent")){
  x <- subset(cces,cces$pid3 == i)
  
  summary(cces_model_aldmck <- glm(cong_approval_binary ~ dem_proximity_rule_aldmck_cong_pty_placement*congress_type + comprehenesive_knowledge_scale_cut + pres_approval_num + retro_econ_eval_num + cong_delegation_approval + aldmck_proximity_betwen_mc_citizen, data=x, weights=weight, family = binomial(link = "logit")))
  cces_model_margins_aldmck <- summary(margins(cces_model_aldmck,variables=c("dem_proximity_rule_aldmck_cong_pty_placement"), at = list(congress_type=c("Republican Congress","Split Congress","Democratic Congress")), type="response", change="sd", vcov=cluster.vcov(cces_model_aldmck, cluster=x$year_district_id)))
  cces_model_margins_aldmck <- data.frame(cces_model_margins_aldmck)
  cces_model_margins_aldmck$congress_type <- factor(cces_model_margins_aldmck$congress_type,levels=c("Democratic Congress", "Split Congress", "Republican Congress"))
  cces_model_margins_aldmck$ame_label <- round(cces_model_margins_aldmck$AME,2)
  cces_model_margins_aldmck$party <- i
  cces_model_margins_aldmck_by_party[[i]] <- cces_model_margins_aldmck
}
cces_model_margins_aldmck_by_party <- ldply(cces_model_margins_aldmck_by_party,data.frame)

cces_model_margins_joint_scaling_by_party <- list()
for(i in c("Republican","Democrat")){
  x <- subset(cces,cces$pid3 == i)
  
  summary(cces_model_joint_scaling <- glm(cong_approval_binary ~ dem_proximity_rule_joint_scaling*congress_type + comprehenesive_knowledge_scale_cut + pres_approval_num + retro_econ_eval_num + cong_delegation_approval + jointscaling_proximity_between_mc_citizen , data=x, weights=weight, family = binomial(link = "logit")))
  coeftest(cces_model_joint_scaling, vcov = cluster.vcov(cces_model_joint_scaling, cluster=x$year_district_id))
  
  cces_model_joint_scaling <- summary(margins(cces_model_joint_scaling,variables=c("dem_proximity_rule_joint_scaling"), at = list(congress_type=c("Republican Congress","Split Congress","Democratic Congress")), type="response", change="sd", vcov=cluster.vcov(cces_model_joint_scaling, cluster=x$year_district_id)))
  cces_model_joint_scaling <- data.frame(cces_model_joint_scaling)
  cces_model_joint_scaling$congress_type <- factor(cces_model_joint_scaling$congress_type,levels=c("Democratic Congress", "Split Congress", "Republican Congress"))
  cces_model_joint_scaling$ame_label <- round(cces_model_joint_scaling$AME,2)
  cces_model_joint_scaling$party <- i
  cces_model_margins_joint_scaling_by_party[[i]] <- cces_model_joint_scaling
}
for(i in c("Independent")){
  x <- subset(cces,cces$pid3 == i)
  
  summary(cces_model_joint_scaling <- glm(cong_approval_binary ~ dem_proximity_rule_joint_scaling*congress_type + comprehenesive_knowledge_scale_cut + pres_approval_num + retro_econ_eval_num + cong_delegation_approval + jointscaling_proximity_between_mc_citizen, data=x, weights=weight, family = binomial(link = "logit")))
  cces_model_joint_scaling <- summary(margins(cces_model_joint_scaling,variables=c("dem_proximity_rule_joint_scaling"), at = list(congress_type=c("Republican Congress","Split Congress","Democratic Congress")), type="response", change="sd", vcov=cluster.vcov(cces_model_joint_scaling, cluster=x$year_district_id)))
  cces_model_joint_scaling <- data.frame(cces_model_joint_scaling)
  cces_model_joint_scaling$congress_type <- factor(cces_model_joint_scaling$congress_type,levels=c("Democratic Congress", "Split Congress", "Republican Congress"))
  cces_model_joint_scaling$ame_label <- round(cces_model_joint_scaling$AME,2)
  cces_model_joint_scaling$party <- i
  cces_model_margins_joint_scaling_by_party[[i]] <- cces_model_joint_scaling
}
cces_model_margins_joint_scaling_by_party <- ldply(cces_model_margins_joint_scaling_by_party,data.frame)

cces_model_margins_joint_scaling_by_party$party_model <- ifelse(cces_model_margins_joint_scaling_by_party$party == "Democrat","Democratic Partisanship Model",ifelse(cces_model_margins_joint_scaling_by_party$party == "Independent","Independent Partisanship Model",ifelse(cces_model_margins_joint_scaling_by_party$party == "Republican","Republican Partisanship Model",NA)))

cces_model_margins_joint_scaling_by_party$congress_type2 <- ifelse(cces_model_margins_joint_scaling_by_party$congress_type == "Democratic Congress","Democratic \nCongress",ifelse(cces_model_margins_joint_scaling_by_party$congress_type == "Republican Congress","Republican \nCongress",ifelse(cces_model_margins_joint_scaling_by_party$congress_type == "Split Congress","Split \nCongress",NA)))

cces_model_margins_joint_scaling_by_party$congress_type2 <- factor(cces_model_margins_joint_scaling_by_party$congress_type2,levels=c("Democratic \nCongress","Split \nCongress","Republican \nCongress"))

cces_model_margins_aldmck_by_party$party_model <- ifelse(cces_model_margins_aldmck_by_party$party == "Democrat","Democratic Partisanship Model",ifelse(cces_model_margins_aldmck_by_party$party == "Independent","Independent Partisanship Model",ifelse(cces_model_margins_aldmck_by_party$party == "Republican","Republican Partisanship Model",NA)))

cces_model_margins_aldmck_by_party$congress_type2 <- ifelse(cces_model_margins_aldmck_by_party$congress_type == "Democratic Congress","Democratic \nCongress",ifelse(cces_model_margins_aldmck_by_party$congress_type == "Republican Congress","Republican \nCongress",ifelse(cces_model_margins_aldmck_by_party$congress_type == "Split Congress","Split \nCongress",NA)))

cces_model_margins_aldmck_by_party$congress_type2 <- factor(cces_model_margins_aldmck_by_party$congress_type2,levels=c("Democratic \nCongress","Split \nCongress","Republican \nCongress"))

cces_model_margins_aldmck_by_party$model <- "Perceptual Based Model"
cces_model_margins_joint_scaling_by_party$model <-"Roll-Call Based Model"

x <- rbind(cces_model_margins_aldmck_by_party,cces_model_margins_joint_scaling_by_party)
x$location <- ifelse(x$model == "Perceptual Based Model", 2.45, ifelse(x$model == "Roll-Call Based Model", 1-2.75,NA))
x$model <- factor(x$model) 

plot <- ggplot(x,aes(x=congress_type2, y=AME, ymin=lower, ymax=upper, shape=model, label=ame_label,group=model,color=party_model)) + theme(legend.position="bottom") + facet_wrap(~party_model) + geom_errorbar(width=0.2,size=1,position= position_dodge(width=0.45))  + geom_point(size=4, fill="white",position= position_dodge(width=0.45)) + scale_y_continuous("Marginal Effect of 1 SD Change in Proximity on P(Approval)",breaks=round(seq(-0.35,0.25,0.05),2)) + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + scale_x_discrete("") + facet_wrap(~party_model)  + theme_minimal() + scale_shape_manual("",values=rep(22:21,9)) + scale_color_manual("",values=c("blue","purple","red"),guide=F) + theme(legend.position="bottom") #+ geom_text(size=3.5,hjust =x$location)
#ggsave(plot,file="CCES_rollcall_am_models_unified.png", width = 9.5, height = 6, units = "in") # Figure 8A

rm(cces_model_aldmck,cces_model_joint_scaling,cces_model_margins_aldmck,cces_model_margins_aldmck_by_party,cces_model_margins_joint_scaling_by_party,g,panel_margins,panel_margins_by_party,panel_model_10,panel_model_10_margins,panel_model_12,panel_model_12_margins,panel_model_14,panel_model_14_margins,plot,x,footnote,i)

###### Appendix Figures: Yearly CCES Models & ANES Robustness Checks Figures ###### 

cces_model_margins_joint_scaling_yearly <- list()
for(i in seq(2008,2016,1)){
  y <- subset(cces,cces$year %in% i)
summary(cces_model_joint_scaling_yearly <- glm(cong_approval_binary ~ pid3 + dem_proximity_rule_joint_scaling + comprehenesive_knowledge_scale_cut + pres_approval_num + retro_econ_eval_num + cong_delegation_approval + jointscaling_proximity_between_mc_citizen, data=y, weights=weight, family = binomial(link = "logit")))

coeftest(cces_model_joint_scaling_yearly, vcov = cluster.vcov(cces_model_joint_scaling_yearly, cluster=y$year_district_id))

margins <- summary(margins(cces_model_joint_scaling_yearly, variables=c("dem_proximity_rule_joint_scaling"),type="response", change="sd", vcov=cluster.vcov(cces_model_joint_scaling_yearly, cluster=y$year_district_id)))

margins2 <- summary(margins(cces_model_joint_scaling_yearly, variables=c("pid3"),type="response", change="minmax", vcov=cluster.vcov(cces_model_joint_scaling_yearly, cluster=y$year_district_id)))

margins <- rbind(margins,margins2)
margins$years <- i
cces_model_margins_joint_scaling_yearly[[i]] <- margins
}
cces_model_margins_joint_scaling_yearly <- ldply(cces_model_margins_joint_scaling_yearly,data.frame)

cces_model_margins_joint_scaling_yearly$party_control <- ifelse(cces_model_margins_joint_scaling_yearly$years %in% c(2008,2009,2010), "Democratic \nCongress",ifelse(cces_model_margins_joint_scaling_yearly$years %in% c(2011,2012,2013,2014), "Split \nCongress", ifelse(cces_model_margins_joint_scaling_yearly$years %in% c(2015,2016), "Republican \nCongress",NA)))
cces_model_margins_joint_scaling_yearly$ame_label <- round(cces_model_margins_joint_scaling_yearly$AME,2)
cces_model_margins_joint_scaling_yearly$party_control <- factor(cces_model_margins_joint_scaling_yearly$party_control,levels=c("Democratic \nCongress","Split \nCongress","Republican \nCongress"))

cces_model_margins_aldmck_cong_pty_yearly <- list()
for(i in seq(2008,2016,1)){
  y <- subset(cces,cces$year %in% i)
  summary(cces_model_aldmck_cong_pty_placement_yearly <- glm(cong_approval_binary ~ pid3 + dem_proximity_rule_aldmck_cong_pty_placement + comprehenesive_knowledge_scale_cut + pres_approval_num + retro_econ_eval_num + cong_delegation_approval + aldmck_proximity_betwen_mc_citizen, data=y, weights=weight, family = binomial(link = "logit")))
  
  coeftest(cces_model_aldmck_cong_pty_placement_yearly, vcov = cluster.vcov(cces_model_aldmck_cong_pty_placement_yearly, cluster=y$year_district_id))
  
  margins <- summary(margins(cces_model_aldmck_cong_pty_placement_yearly, variables=c("dem_proximity_rule_aldmck_cong_pty_placement"),type="response", change="sd", vcov=cluster.vcov(cces_model_aldmck_cong_pty_placement_yearly, cluster=y$year_district_id)))
  
  margins2 <- summary(margins(cces_model_aldmck_cong_pty_placement_yearly, variables=c("pid3"),type="response", change="minmax", vcov=cluster.vcov(cces_model_aldmck_cong_pty_placement_yearly, cluster=y$year_district_id)))
  
  margins <- rbind(margins,margins2)
  margins$years <- i
  cces_model_margins_aldmck_cong_pty_yearly[[i]] <- margins
}
cces_model_margins_aldmck_cong_pty_yearly <- ldply(cces_model_margins_aldmck_cong_pty_yearly,data.frame)

cces_model_margins_aldmck_cong_pty_yearly$party_control <- ifelse(cces_model_margins_aldmck_cong_pty_yearly$years %in% c(2008,2009,2010), "Democratic \nCongress",ifelse(cces_model_margins_aldmck_cong_pty_yearly$years %in% c(2011,2012,2013,2014), "Split \nCongress", ifelse(cces_model_margins_aldmck_cong_pty_yearly$years %in% c(2015,2016), "Republican \nCongress",NA)))
cces_model_margins_aldmck_cong_pty_yearly$ame_label <- round(cces_model_margins_aldmck_cong_pty_yearly$AME,2)
cces_model_margins_aldmck_cong_pty_yearly$party_control <- factor(cces_model_margins_aldmck_cong_pty_yearly$party_control,levels=c("Democratic \nCongress","Split \nCongress","Republican \nCongress"))

x <- cces_model_margins_aldmck_cong_pty_yearly
x <- subset(x,x$factor == "dem_proximity_rule_aldmck_cong_pty_placement")
y <- cces_model_margins_joint_scaling_yearly
y <- subset(y,y$factor == "dem_proximity_rule_joint_scaling")

x$scale_type <- "Perceptual-Based Ideological Scaling"
y$scale_type <- "Roll Call-Based Ideological Scaling"

x <- rbind(x,y)
rm(y)

plot <- ggplot(x,aes(x=factor(years), y=AME, ymin=lower, ymax=upper, group=party_control, shape=party_control, label=ame_label)) + geom_errorbar(width=0.2,size=1,colour="black") + geom_point(size=4, fill="white") + theme_minimal() + scale_y_continuous("Marginal Effect of 1 SD Change in Proximity on P(Approval)",breaks=c(-0.35,-.30,-0.25,-0.20,-0.15,-0.10,-0.05,0,0.05,0.10,0.15,0.20,0.25,0.30)) + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + scale_x_discrete("") + scale_shape_manual("",values=c(21,24,23)) + theme(legend.position="bottom") + geom_text(size=2.5,hjust = 0, nudge_x = 0.15) + facet_wrap(~scale_type,ncol=2) #+scale_colour_manual("",values=c("blue","red","purple")) #colour=party_control, in aes
grid.newpage()
footnote <- "Marginal effects derived from yearly model estimates. DV: Congressional approval."
g <- arrangeGrob(plot, bottom = textGrob(footnote, x = 0.025, hjust = 0, vjust= 0, y=0.75, gp = gpar(fontface = "italic", fontsize = 9, col = "black")))
grid.draw(g)
#ggsave(g,file="cces_proximity_marginals_by_year.png", width = 10, height = 6, units = "in") # Appendix Figure 1A

x <- cces_model_margins_aldmck_cong_pty_yearly
x <- subset(x,x$factor == "pid3Democrat")
y <- cces_model_margins_joint_scaling_yearly
y <- subset(y,y$factor == "pid3Democrat")

x$scale_type <- "Perceptual-Based Ideological Scaling"
y$scale_type <- "Roll Call-Based Ideological Scaling"

x <- rbind(x,y)
rm(y)

plot <- ggplot(x,aes(x=factor(years), y=AME, ymin=lower, ymax=upper, group=party_control, shape=party_control, label=ame_label)) + geom_errorbar(width=0.2,size=1,colour="black") + geom_point(size=4, fill="white") + theme_minimal() + scale_y_continuous("Marginal Effect of 1 SD Change in Proximity on P(Approval)",breaks=c(-0.35,-.30,-0.25,-0.20,-0.15,-0.10,-0.05,0,0.05,0.10,0.15,0.20,0.25,0.30)) + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + scale_x_discrete("") + scale_shape_manual("",values=c(21,24,23)) + theme(legend.position="bottom") + geom_text(size=2.5,hjust = 0, nudge_x = 0.15) + facet_wrap(~scale_type,ncol=2) #+scale_colour_manual("",values=c("blue","red","purple")) #colour=party_control, in aes
grid.newpage()
footnote <- "Marginal effects derived from yearly model estimates. DV: Congressional approval."
g <- arrangeGrob(plot, bottom = textGrob(footnote, x = 0.025, hjust = 0, vjust= 0, y=0.75, gp = gpar(fontface = "italic", fontsize = 9, col = "black")))
grid.draw(g)
#ggsave(g,file="cces_pid_marginals_by_year_both_models.png", width = 10, height = 6, units = "in") # Appendix Figure 1B

x <- subset(nes,select=c(respondent_id,cong_approval_dich,pid3,full_knowledge_scale,dem_pty_proximity_bam,ideo_ideal_pts_50,dem_pres_approve_clean,retro_econ_eval_clean_scale,weight,year,self_libcon_placement_numeric))
x <- na.omit(x)

plot <- ggplot(x, aes(x=self_libcon_placement_numeric, y=ideo_ideal_pts_50, group=self_libcon_placement_numeric)) + geom_boxplot(colour = "black",outlier.shape = NA) + scale_y_continuous("Liberal-Conservative Bayesian Aldrich-McKelvey Perceptual-Based Ideal Point Estimates",limits=c(-3,3),breaks=seq(-3,3,1)) + scale_x_continuous("Liberal-Conservative Raw Ideological Self-Placement",breaks=seq(1,7,1)) + scale_fill_discrete(guide=F) + stat_summary(fun.y = mean, geom="point",colour="black", size=2.00, shape= 17) + scale_shape_discrete("") + theme_minimal()
grid.newpage()
footnote <- "Triangles denote mean value of Aldrich-McKelvey ideal point estimate by raw ideological self-placement."
g <- arrangeGrob(plot, bottom = textGrob(footnote, x = 0.015, y = 0.5, hjust = 0, vjust= 0, gp = gpar(fontface = "italic", fontsize = 9, col = "black"))) 
grid.draw(g)
#ggsave(g,file="anes_citizen_boxplot_aldmck_raw_placements_pooled.png", width = 7.52, height = 6.82, units = "in") # Appendix Figure 2

summary(nes_model <- glm(cong_approval_dich ~ pid3*congress_type + dem_pty_proximity_bam*congress_type + dem_pres_approve_clean + full_knowledge_scale + inc_approval_clean_factor_dich + retro_econ_eval_clean_scale, data=nes, weights=weight, family = binomial(link = "logit")))
coeftest(nes_model, vcov = cluster.vcov(nes_model, cluster=nes$year))

nes_model_margins <- summary(margins(nes_model, variables=c("pid3","dem_pty_proximity_bam"), at = list(congress_type=c("Republican Congress","Split Congress","Democratic Congress")), type="response", change="sd",vcov = cluster.vcov(nes_model, cluster=nes$year)))
nes_model_margins$congress_type <- factor(nes_model_margins$congress_type,levels=c("Democratic Congress", "Split Congress", "Republican Congress"))
nes_model_margins$ame_label <- round(nes_model_margins$AME,2)

plot <- ggplot(subset(nes_model_margins,nes_model_margins$factor == "dem_pty_proximity_bam"),aes(x=congress_type, y=AME, ymin=lower, ymax=upper,label=ame_label)) + geom_errorbar(width=0.2,size=1) + geom_point(size=4, shape=21, fill="white") + theme_minimal() + scale_y_continuous("Marginal Effect of 1 SD Change in Proximity on P(Approval)",breaks=round(seq(-0.20,0.20,0.05),2)) + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + scale_x_discrete("") + geom_text(size=3.5,hjust = 0, nudge_x = 0.15)
grid.newpage()
footnote <- "Marginal effects derived from interactive model estimates. DV: Congressional approval."
g <- arrangeGrob(plot, bottom = textGrob(footnote, x = 0.025, hjust = 0, vjust= 0, y=0.75, gp = gpar(fontface = "italic", fontsize = 9, col = "black")))
grid.draw(g)
#ggsave(g,file="anes_congress_type_aldmck_party_pos_proximitys.png", width = 6.82, height = 6.82, units = "in") # Appendix Figure 3A

summary(nes_model <- glm(cong_approval_dich ~ pid3*congress_type + dem_pty_proximity_bam*congress_type + dem_pres_approve_clean + full_knowledge_scale + inc_approval_clean_factor_dich + retro_econ_eval_clean_scale, data=nes, weights=weight, family = binomial(link = "logit")))
coeftest(nes_model, vcov = cluster.vcov(nes_model, cluster=nes$year))

nes_model_margins <- summary(margins(nes_model, variables=c("pid3"), at = list(congress_type=c("Republican Congress","Split Congress","Democratic Congress")), type="response", change="minmax", vcov = cluster.vcov(nes_model, cluster=nes$year)))
nes_model_margins$congress_type <- factor(nes_model_margins$congress_type,levels=c("Democratic Congress", "Split Congress", "Republican Congress"))
nes_model_margins$ame_label <- round(nes_model_margins$AME,2)

plot <- ggplot(subset(nes_model_margins,nes_model_margins$factor == "pid3Democrat"),aes(x=congress_type, y=AME, ymin=lower, ymax=upper, group=congress_type, shape=congress_type, label=ame_label)) + geom_errorbar(width=0.2,size=1,colour="black") + geom_point(size=4, shape=21, fill="white") + theme_minimal() + scale_y_continuous("Marginal Effect of Democratic Partisanship on P(Approval)",breaks=round(seq(-0.30,0.25,0.05),2)) + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + scale_x_discrete("") + scale_shape_manual("",values=c(21,24,23)) + theme(legend.position="bottom") + geom_text(size=3.5,hjust = 0, nudge_x = 0.175) #+scale_colour_manual("",values=c("blue","red","purple")) #colour=party_control, in aes
grid.newpage()
footnote <- "Marginal effects derived from interactive model estimates. DV: Congressional approval."
g <- arrangeGrob(plot, bottom = textGrob(footnote, x = 0.025, hjust = 0, vjust= 0, y=0.75, gp = gpar(fontface = "italic", fontsize = 9, col = "black")))
grid.draw(g)
#ggsave(g,file="anes_congress_type_pid.png", width = 6.82, height = 6.82, units = "in") # # Appendix Figure 3B

nes_margins <- list()
for(i in c("Republican","Independent","Democrat")){
  x <- subset(nes,nes$pid3 == i)
  summary(nes_model <- glm(cong_approval_dich ~ dem_pty_proximity_bam*congress_type + dem_pres_approve_clean + full_knowledge_scale + retro_econ_eval_clean_scale, data=x, weights=weight, family = binomial(link = "logit")))
  nes_model_margins_yearly <- summary(margins(nes_model, variables=c("dem_pty_proximity_bam"), at = list(congress_type=c("Republican Congress","Split Congress","Democratic Congress")), type="response", change="sd", vcov = cluster.vcov(nes_model, cluster=x$year)))
  nes_model_margins_yearly <- data.frame(nes_model_margins_yearly)
  nes_model_margins_yearly$partisanship <- i
  nes_margins[[i]] <- nes_model_margins_yearly
}

nes_margins <- ldply(nes_margins,data.frame)
nes_margins$ame_label <- round(nes_margins$AME,2)
nes_margins$partisanship_model <- ifelse(nes_margins$partisanship == "Democrat","Democratic Partisanship Model",ifelse(nes_margins$partisanship == "Independent","Independent Partisanship Model",ifelse(nes_margins$partisanship == "Republican","Republican Partisanship Model",NA)))
nes_margins$congress_type <- factor(nes_margins$congress_type,levels=c("Democratic Congress","Split Congress","Republican Congress"))

plot <- ggplot(nes_margins,aes(x=congress_type, y=AME, ymin=lower, ymax=upper,label=ame_label)) + geom_errorbar(width=0.2,size=1,color=c("red","red","red","purple","purple","purple","blue","blue","blue")) + geom_point(size=4, shape=21, fill="white",color=c("red","red","red","purple","purple","purple","blue","blue","blue")) + theme_minimal() + scale_y_continuous("Marginal Effect of 1 SD Change in Proximity on P(Approval)",breaks=round(seq(-0.20,0.20,0.05),2)) + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + scale_x_discrete("",labels=c("Democratic \nCongress","Split \nCongress","Republican \nCongress")) + facet_wrap(~partisanship_model) + geom_text(size=3.5,hjust =0,nudge_x = 0.15,color=c("red","red","red","purple","purple","purple","blue","blue","blue"))
grid.newpage()
footnote <- "Confidence intervals calculated with standard errors clustered by survey year. DV: Congressional approval."
g <- arrangeGrob(plot, bottom = textGrob(footnote, x = 0.025, hjust = 0, vjust= 0, y=0.25, gp = gpar(fontface = "italic", fontsize = 9, col = "black")))
grid.draw(g)
#ggsave(g,file="nes_proximity_by_congress_type_party.png", width = 7.5, height = 5.5, units = "in") # Appendix Figure 4