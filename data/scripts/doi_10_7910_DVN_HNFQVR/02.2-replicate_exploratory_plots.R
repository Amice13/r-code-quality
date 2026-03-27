# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Replicate confirmatory plots for paper.
# 
# Author: Soubhik Barari
# 
# Environment:
# - must use R 3.6
# 
# Input:
# - PAI-Misinfo-Feb2021_explore.RData
#
# Output:
# - figures_confirm/*
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#####------------------------------------------------------#
##### Pre-amble ####
#####------------------------------------------------------#

rm(list=ls())

library(broom)
library(stargazer)
library(ggplot2)
library(tidyverse)

load("PAI-Misinfo-Feb2021_explore.RData")

agree_levels <- c("Strongly disagree","Somewhat disagree","Neither agree nor disagree","Somewhat agree","Strongly agree")
approve_levels <- c("Strongly disapprove","Somewhat disapprove","Neither approve nor disapprove","Somewhat approve","Strongly approve")
freq2_levels <- c("Not sure", "Never", "Less often than every few weeks", "Every few weeks at most", "A few times a week", "About once a day", "Several times a day")
confid_levels <- c("No answer","No confidence","A fair amount of confidence","Not too much confidence","Complete confidence")

my_theme <- theme_linedraw() + 
	theme(panel.grid.major.x = element_blank(), 
		  panel.grid.major.y = element_blank())

#####------------------------------------------------------#
##### Intra-index correlation ####
#####------------------------------------------------------#

support_idx.vars <- c("remove_approve","downrank_approve","falsepos_approve","labels_approve_type",
					  "labels_eval_1","labels_eval_2","labels_eval_3","labels_eval_4")
cor(dat[support_idx.vars], use="pairwise.complete.obs") %>%
	as.data.frame() %>%
	rownames_to_column("var1") %>%
	gather(key="var2", value="corr", -var1) %>%
	mutate(corr=round(corr, 2)) %>%
	mutate(var1=gsub("_"," ", var1), var2=gsub("_"," ", var2)) %>%
	ggplot(aes(x=var1, y=var2, fill=corr)) +
		geom_tile(color="black") + 
	geom_text(aes(label=corr)) + 
	xlab("") + ylab("") + 
	scale_fill_gradient2(name="correlation", low="darkred", mid="white", high="darkgreen",midpoint=0.5) + 
	scale_y_discrete(labels=c("Down-ranking approval","False positive approval","Label coverage approval",
							  "Seen good labels","Seen bad labels","Informative for you","Informative for others","Removal approval")) + 
	scale_x_discrete(labels=c("Down-ranking approval","False positive approval","Label coverage approval",
							  "Seen good labels","Seen bad labels","Informative for you","Informative for others","Removal approval")) + 
	theme(axis.text.x = element_text(angle=10, hjust=1)) + theme(legend.position="none")
ggsave("figures_explore/corrmat_support_idx.png", width=6, height=2)
system("open figures_explore/corrmat_support_idx.png")

inst_trust.vars <- c("confid_inst_academics","confid_inst_electoffic","confid_inst_factcheckers",
					 "confid_inst_localnews","confid_inst_msm","confid_inst_supremecourt","confid_inst_socmedcompanies")
cor(dat[inst_trust.vars], use="pairwise.complete.obs") %>%
	as.data.frame() %>%
	rownames_to_column("var1") %>%
	gather(key="var2", value="corr", -var1) %>%
	mutate(corr=round(corr, 2)) %>%
	mutate(var1=gsub("_"," ", var1), var2=gsub("_"," ", var2)) %>%
	ggplot(aes(x=var1, y=var2, fill=corr)) +
		geom_tile(color="black") + 
	geom_text(aes(label=corr)) + 
	xlab("Trust in...") + ylab("Trust in...") + 
	scale_y_discrete(labels=c("academics","elected officials","fact-checkers","local news","MSM","SM companies","SCOTUS")) + 
	scale_x_discrete(labels=c("academics","elected officials","fact-checkers","local news","MSM","SM companies","SCOTUS")) + 
	scale_fill_gradient2(name="correlation", low="darkred", mid="white", high="darkgreen",midpoint=0.5) + 
	theme(axis.text.x = element_text(angle=10, hjust=1)) + theme(legend.position="none")
ggsave("figures_explore/corrmat_inst_trust.png", width=6, height=2)
system("open figures_explore/corrmat_inst_trust.png")

decide.vars <- c("shoulddecide_algos","shoulddecide_electoffic","shoulddecide_socmedemployees",
				 "shoulddecide_factcheckers","shoulddecide_experts","shoulddecide_users_youknow",
				 "shoulddecide_users_diverse","shoulddecide_users_moderate")
cor(dat[decide.vars], use="pairwise.complete.obs") %>%
	as.data.frame() %>%
	rownames_to_column("var1") %>%
	gather(key="var2", value="corr", -var1) %>%
	mutate(corr=round(corr, 2)) %>%
	mutate(var1=gsub("_"," ", var1), var2=gsub("_"," ", var2)) %>%
	ggplot(aes(x=var1, y=var2, fill=corr)) +
		geom_tile(color="black") + 
	geom_text(aes(label=corr)) + 
	xlab("Should decide...") + ylab("Should decide...") + 
	scale_y_discrete(labels=c("algorithms","elected officials","experts","fact-checkers","SM companies","users (diverse)","users (moderate)","users you know")) + 
	scale_x_discrete(labels=c("algorithms","elected officials","experts","fact-checkers","SM companies","users (diverse)","users (moderate)","users you know")) + 
	scale_fill_gradient2(name="correlation", low="darkred", mid="white", high="darkgreen",midpoint=0.5) + 
	theme(axis.text.x = element_text(angle=45, hjust=1)) + theme(legend.position="none")
ggsave("figures_explore/corrmat_shoulddecide.png", width=5, height=2.5)
system("open figures_explore/corrmat_shoulddecide.png")


#####------------------------------------------------------#
##### Other correlations ####
#####------------------------------------------------------#

## PID and institutional trust
cor(dat$inst_trust, ifelse(dat$demog_PID == "Republican", 1, 0), 
	use="pairwise.complete.obs") ##-0.14
cor(dat$inst_trust, ifelse(dat$demog_PID == "Democrat", 1, 0), 
	use="pairwise.complete.obs") ##0.29

## experience
exp.vars <- c("moder_bad","moder_good","moder_others","moder_you")
cor(dat[exp.vars], use="pairwise.complete.obs") %>%
	as.data.frame() %>%
	rownames_to_column("var1") %>%
	gather(key="var2", value="corr", -var1) %>%
	mutate(corr=round(corr, 2)) %>%
	mutate(var1=gsub("_"," ", var1), var2=gsub("_"," ", var2)) %>%
	ggplot(aes(x=var1, y=var2, fill=corr)) +
		geom_tile(color="black") + 
	geom_text(aes(label=corr)) + 
	xlab("") + ylab("") + 
	scale_fill_gradient2(name="correlation") + 
	theme(axis.text.x = element_text(angle=45, hjust=1))
ggsave("figures_explore/corrmat_exp.png", width=5, height=3)
system("open figures_explore/corrmat_exp.png")

#####------------------------------------------------------#
##### Interesting baselines ####
#####------------------------------------------------------#

####** support by PID ####
dat %>% 
	select(demog_PID_2pt, demog_PID_6pt, support_idx) %>%
	filter(!is.na(support_idx)) %>%
	group_by(demog_PID_2pt) %>% mutate(tot=n()) %>%
	group_by(demog_PID_2pt, demog_PID_6pt) %>%
	summarise(m=mean(support_idx,na.rm=T), se=sd(support_idx,na.rm=T)/n()) %>%
	ggplot(aes(x=demog_PID_6pt, y=m)) + 
	ylab("Support index") + xlab("") + 
	geom_bar(aes(fill=m), stat="identity", color="black") + 
	geom_linerange(aes(x=demog_PID_6pt, ymin=m-2*se, ymax=m+2*se), color="black", size=2) + 
	ggtitle("Support for interventions") + 
	coord_flip() + 
	my_theme + 
	theme(legend.position="none") 
ggsave("figures_explore/support_idx.png", width=8, height=3)
system("open figures_explore/support_idx.png")

####** follow media you disagree? ####
prop.table(table(dat$follow_disagree)) ##27% follow outlets they disagree with
dat %>% 
	select(demog_PID_2pt, demog_PID_6pt, follow_disagree) %>%
	filter(!is.na(follow_disagree)) %>%
	group_by(demog_PID_2pt, demog_PID_6pt) %>% mutate(tot=n()) %>%
	group_by(demog_PID_2pt, demog_PID_6pt, follow_disagree) %>%
	summarise(pct=n()/tot) %>% distinct() %>%
	mutate(follow_disagree= gsub("to ", "to\n", follow_disagree)) %>%
	mutate(follow_disagree = factor(follow_disagree, 
									levels=rev(c("Yes", "No", "Not sure/No response")))) %>%
	# mutate(labels_eval_human= gsub(" errors", "\nerrors\n", labels_eval_human)) %>%
	ggplot(aes(x=demog_PID_6pt, y=pct, fill=follow_disagree)) + 
	scale_y_continuous(labels = scales::percent_format(accuracy=1)) +
	ylab("% of self-reported partisans") + xlab("") + 
	geom_bar(aes(fill=follow_disagree), stat="identity", position=position_stack(vjust=1)) + 
	geom_text(aes(x=demog_PID_6pt, y=pct, label=paste0(round(pct*100,0),"%")), size=3.6, colour='white', fontface='bold', position = position_stack(vjust=0.5)) +
	ggtitle("Do you follow social media accounts whose views you disagree with?") + 
	scale_fill_manual(values=c("grey", "red", "darkgreen"), name="") + 
	coord_flip() + 
	my_theme +
	theme(legend.position="bottom") 
ggsave("figures_explore/disagree.png", width=8, height=3)
system("open figures_explore/disagree.png")


####** how much content do you think is moderated? ####
dat %>% filter(!is.na(check_amt)) %>%
	group_by(check_amt) %>% 
	summarise(n=n()) %>%
	mutate(pct=n/sum(n)) %>%
	mutate(check_amt = factor(check_amt, 
							  levels=c("Not sure",
							  		   "No content is checked",
							  		   "Most content is NOT checked",
							  		   "About half of content is checked",
							  		   "Most content is checked",
							  		   "All content is checked"))) %>%
	ggplot(aes(x=check_amt, y=pct)) + 
	coord_flip() +
	geom_text(aes(x=check_amt, y=pct+0.01, label=paste0(round(pct*100,1),"%"))) +
	scale_y_continuous(labels = scales::percent_format(accuracy=1)) +
	ggtitle("How much content on your most-used social media platform is checked\nfor accuracy?") + 
	xlab("") + ylab("% of respondents") +
	geom_bar(stat="identity", color="black") + 
	my_theme
ggsave("figures_explore/check_amt.png", width=8, height=2.5)
system("open figures_explore/check_amt.png")


bind_rows(dat %>% filter(!is.na(check_amt)) %>%
			group_by(check_amt) %>% 
			summarise(n=n()) %>%
			mutate(pct=n/sum(n), demog_PID_2pt="All"),
		dat %>% filter(!is.na(check_amt)) %>%
			group_by(demog_PID_2pt) %>% 
			mutate(tot=n()) %>%
			group_by(demog_PID_2pt, check_amt) %>%
			summarise(pct=n()/tot) %>% distinct()) %>%
	mutate(check_amt = factor(check_amt, 
							  levels=c("Not sure",
							  		   "No content is checked",
							  		   "Most content is NOT checked",
							  		   "About half of content is checked",
							  		   "Most content is checked",
							  		   "All content is checked"))) %>%
	mutate(demog_PID_2pt = factor(demog_PID_2pt, levels=c("All","Democrat","Republican","Neither"))) %>%
	ggplot(aes(x=check_amt, y=pct)) + 
	coord_flip() +
	scale_y_continuous(labels = scales::percent_format(accuracy=1), limits=c(0,0.5), breaks=c(0,0.25,0.5)) +
	facet_grid(~ demog_PID_2pt) +
	ggtitle("How much content on your most-used social media platform is checked for accuracy?") + 
	xlab("") + ylab("% of respondents") +
	geom_bar(aes(fill=demog_PID_2pt), stat="identity", color="black") + 
	geom_text(aes(x=check_amt, y=pct+0.08, label=paste0(round(pct*100,1),"%")), size=2) +
	scale_fill_manual(values=c("white","blue","red","grey40")) + 
	my_theme + theme(legend.position="none", axis.text.x =element_text(size=5), axis.text.y=element_text(size=7), plot.title = element_text(size=10))
ggsave("figures_explore/check_amt2.png", width=7, height=1.75)
system("open figures_explore/check_amt2.png")


####** important that public informed? ####
dat %>% filter(!is.na(informed_should)) %>%
	group_by(informed_should) %>% 
	summarise(n=n()) %>%
	mutate(pct=n/sum(n)) %>%
	mutate(informed_should = factor(informed_should, 
									levels=agree_levels)) %>%
	ggplot(aes(x=informed_should, y=pct)) + 
	coord_flip() +
	scale_y_continuous(labels = scales::percent_format(accuracy=1)) +
	ggtitle("Is it important that the public be informed about current events?") + 
	xlab("") + ylab("% of respondents") +
	geom_bar(stat="identity", color="black") + 
	theme_bw()
ggsave("figures_explore/informed_should.png", width=8, height=3)
system("open figures_explore/informed_should.png")


####** how often encounter labels? ####
dat %>% select(labels_freq_FB,
			   labels_freq_IG,
			   labels_freq_TW,
			   labels_freq_TK,
			   labels_freq_YT,
			   demog_PID_2pt) %>%
	gather(key="platform", value="freq", -demog_PID_2pt) %>%
	filter(!is.na(freq), !grepl("Other", freq)) %>%
	group_by(platform) %>% mutate(tot=n()) %>% ungroup() %>%
	group_by(demog_PID_2pt, platform, freq) %>% summarise(pct=n()/tot, .groups="keep") %>% distinct() %>%
	mutate(platform=gsub("labels_freq_","", platform)) %>%
	mutate(platform=gsub("FB","Facebook", platform)) %>% 
	mutate(platform=gsub("TW","Twitter", platform)) %>% 
	mutate(platform=gsub("YT","YouTube", platform)) %>% 
	mutate(platform=gsub("IG","Instagram", platform)) %>% 
	mutate(platform=gsub("TK","TikTok", platform)) %>% 
	mutate(freq=factor(freq, levels=freq2_levels)) %>%
	ggplot(aes(y=freq, x=pct, fill=demog_PID_2pt)) +
	geom_bar(stat="identity", color="black") +
	scale_fill_manual(values=c("blue","red","grey"), name="PID:") + 
	ylab("") + xlab("% of self-reported users") +
	ggtitle("Since the election how often did you encounter credibility labels on...") + 
	scale_x_continuous(labels = scales::percent_format(accuracy=1)) +
	facet_grid(~ platform) +
	my_theme + theme(legend.position="bottom")
ggsave("figures_explore/encounter.png", width=8, height=4)
system("open figures_explore/encounter.png")

dat %>% select(labels_freq_FB,
			   labels_freq_IG,
			   labels_freq_TW,
			   labels_freq_TK,
			   labels_freq_YT,
			   demog_PID_6pt) %>%
	gather(key="platform", value="freq", -demog_PID_6pt) %>%
	filter(!is.na(freq), !grepl("Other", freq)) %>%
	group_by(platform) %>% mutate(tot=n()) %>% ungroup() %>%
	group_by(demog_PID_6pt, platform, freq) %>% summarise(pct=n()/tot, .groups="keep") %>% distinct() %>%
	mutate(platform=gsub("labels_freq_","", platform)) %>%
	mutate(platform=gsub("FB","Facebook", platform)) %>% 
	mutate(platform=gsub("TW","Twitter", platform)) %>% 
	mutate(platform=gsub("YT","YouTube", platform)) %>% 
	mutate(platform=gsub("IG","Instagram", platform)) %>% 
	mutate(platform=gsub("TK","TikTok", platform)) %>% 
	mutate(freq=factor(freq, levels=freq2_levels)) %>%
	ggplot(aes(y=freq, x=pct, fill=demog_PID_6pt)) +
	geom_bar(stat="identity") +
	scale_fill_manual(values=c("blue","lightblue","grey","red","darkred","black"), name="") + 
	ylab("") + xlab("% of self-reported users") +
	ggtitle("Since the election how often did you encounter credibility labels on...") + 
	scale_x_continuous(labels = scales::percent_format(accuracy=1)) +
	facet_grid(~ platform) +
	my_theme + theme(legend.position="bottom", axis.text.x = element_text(size=5))
ggsave("figures_explore/encounter2.png", width=8.5, height=2.5)
system("open figures_explore/encounter2.png")


p <- bind_rows(
	dat %>% select(labels_freq_FB,
				   labels_freq_IG,
				   labels_freq_TW,
				   labels_freq_TK,
				   labels_freq_YT) %>%
		gather(key="platform", value="freq") %>%
		# filter(!is.na(freq), !grepl("Other", freq)) %>%
		group_by(platform) %>% mutate(tot=n()) %>% ungroup() %>%
		group_by(platform, freq) %>% summarise(pct=n()/tot, .groups="keep") %>% distinct() %>%
		mutate(demog_PID_2pt = "All")
	,
	dat %>% select(labels_freq_FB,
				   labels_freq_IG,
				   labels_freq_TW,
				   labels_freq_TK,
				   labels_freq_YT,
				   demog_PID_2pt,
				   demog_PID_6pt) %>%
		gather(key="platform", value="freq", -demog_PID_6pt, -demog_PID_2pt) %>%
		group_by(platform, demog_PID_2pt) %>% mutate(tot=n()) %>% ungroup() %>%
		group_by(demog_PID_2pt, platform, freq) %>% summarise(pct=n()/tot, .groups="keep") %>% distinct()
) %>% mutate(demog_PID_2pt=factor(demog_PID_2pt, levels=c("All","Democrat","Republican","Neither"))) %>%
	filter(!is.na(freq), !grepl("Other", freq)) %>%
	mutate(platform=gsub("labels_freq_","", platform)) %>%
	mutate(platform=gsub("FB","Facebook", platform)) %>% 
	mutate(platform=gsub("TW","Twitter", platform)) %>% 
	mutate(platform=gsub("YT","YouTube", platform)) %>% 
	mutate(platform=gsub("IG","Instagram", platform)) %>% 
	mutate(platform=gsub("TK","TikTok", platform)) %>% 
	mutate(freq=factor(freq, levels=freq2_levels)) %>%
	ggplot(aes(y=freq, x=pct, fill=demog_PID_2pt)) +
	geom_bar(stat="identity", color="black") +
	geom_text(aes(x=pct+0.04, y=freq, label=paste0(round(pct*100,1),"%"), group=demog_PID_2pt), size=2) +
	scale_fill_manual(values=c("white","blue","red","grey"), name="") + 
	ylab("") + xlab("% of all self-reported users in group and on platform") +
	ggtitle("Since the election how often did you encounter credibility labels on...") + 
	scale_x_continuous(labels = scales::percent_format(accuracy=1), limits=c(0,0.25)) +
	facet_grid(demog_PID_2pt ~ platform) +
	my_theme + theme(legend.position="none", axis.text.x = element_text(size=5))
ggsave("figures_explore/encounter3.png", width=8, height=4)
system("open figures_explore/encounter3.png")

labels_PID6pt_df <- bind_rows(tidy(mod.fb1) %>% mutate(platform = "Facebook", model = "univariate"),
		  tidy(mod.fb2) %>% mutate(platform = "Facebook", model = "adjusted"),
		  tidy(mod.ig1) %>% mutate(platform = "Instagram", model = "univariate"),
		  tidy(mod.ig2) %>% mutate(platform = "Instagram", model = "adjusted"),
		  tidy(mod.yt1) %>% mutate(platform = "YouTube", model = "univariate"),
		  tidy(mod.yt2) %>% mutate(platform = "YouTube", model = "adjusted"),
		  tidy(mod.tk1) %>% mutate(platform = "TikTok", model = "univariate"),
		  tidy(mod.tk2) %>% mutate(platform = "TikTok", model = "adjusted"),
		  tidy(mod.tw1) %>% mutate(platform = "Twitter", model = "univariate"),
		  tidy(mod.tw2) %>% mutate(platform = "Twitter", model = "adjusted")) %>%
	filter(grepl("PID", term)) %>%
	mutate(term = gsub("demog_PID_6pt","", term)) %>%
	mutate(sig=ifelse(estimate-1.96*std.error < 0 & estimate+1.96*std.error > 0, 
	                  "gray", "black")) %>%
	mutate(term = factor(term, levels = c(
		"Weak Democrat", "True Independent", "Weak Republican", "Strong Republican", "Everyone else"
	)))

labels_PID6pt_df <- labels_PID6pt_df[order(labels_PID6pt_df$p.value),]
alpha <- 0.05
k <- nrow(labels_PID6pt_df); r <- 1:k
alpha.adj <- (r*alpha)/k
labels_PID6pt_df$z.crit.adj <- qnorm(1 - (alpha.adj)/2)
labels_PID6pt_df$sig.BH <- ifelse(labels_PID6pt_df$p.value < alpha.adj, "black", "gray")

p2 <- ggplot(labels_PID6pt_df, aes(x=term)) +
	## null hypothesis
	geom_hline(yintercept=0, lty=2, alpha=0.5) +
	## BHq CI
	geom_linerange(aes(y=estimate, 
					   ymin=estimate-z.crit.adj*std.error, 
					   ymax=estimate+z.crit.adj*std.error, 
					   shape=model, colour=sig.BH), 
				   position=position_dodge(width=.75), 
				   size=0.5, stroke=.5) +
	## 95% CI
	geom_pointrange(aes(y=estimate,
						ymin=estimate-1.96*std.error,
						ymax=estimate+1.96*std.error,
						shape=model, colour=sig.BH),
					position=position_dodge(width=.75),
					size=0.75, stroke=.5) +
	## 90% CI
	geom_linerange(aes(y=estimate,
					   ymin=estimate-1.65*std.error,
					   ymax=estimate+1.65*std.error,
					   shape=model, colour=sig.BH),
				   position=position_dodge(width=.75),
				   size=0.8, stroke=.5) +
	scale_colour_identity() + 
	scale_shape_manual(values=c(16,25,15)) + 
	facet_grid(~ platform) + ## different outcomes
	ylab(TeX("Marginal $(\\pm \\sigma)$ level of encounter (relative to Strong Democrat)")) + 
	xlab("") +
	coord_flip() + 
	my_theme + 
	theme(axis.text.x = element_text(size=8),
		  strip.text = element_text(size = 10),
		  # axis.title.x = element_text(size=10),
		  plot.margin = margin(5, 0, 7, -5),
		  legend.text = element_text(size = 8),
		  legend.title = element_text(size = 10),
		  legend.spacing.y = unit(1.0, 'cm'),
		  legend.position="bottom") + 
	guides(shape=guide_legend(title="model:", nrow=1))

p_p2 <- cowplot::plot_grid(p + theme(axis.text.y=element_text(size=8)), 
						   p2 + theme(plot.margin=unit(x=c(-3,0,0,0),units="mm"), strip.background = element_blank()), 
						   align="v", axis="lr", nrow = 2, rel_heights=c(2.4,1.2))
cowplot::save_plot(p_p2, file ="figures_explore/encounter3_1.png", base_width=8.5, base_height=5.5)
system("open figures_explore/encounter3_1.png")

dat %>% select(labels_freq_FB,
			   labels_freq_IG,
			   labels_freq_TW,
			   labels_freq_TK,
			   labels_freq_YT,
			   follow_disagree) %>%
	gather(key="platform", value="freq", -follow_disagree) %>%
	filter(!is.na(freq), !grepl("Other", freq)) %>%
	group_by(follow_disagree, platform) %>% mutate(tot=n()) %>% ungroup() %>%
	group_by(follow_disagree, platform, freq) %>% summarise(pct=n()/first(tot), .groups="keep") %>% distinct() %>%
	mutate(platform=gsub("labels_freq_","", platform)) %>%
	mutate(platform=gsub("FB","Facebook", platform)) %>% 
	mutate(platform=gsub("TW","Twitter", platform)) %>% 
	mutate(platform=gsub("YT","YouTube", platform)) %>% 
	mutate(platform=gsub("IG","Instagram", platform)) %>% 
	mutate(platform=gsub("TK","TikTok", platform)) %>% 
	mutate(freq=factor(freq, levels=freq2_levels)) %>%
	mutate(freq=recode(freq, 'Not sure/Other'='Not sure/\nOther',
					         'Never'='Never',
					         'Less often than every few weeks'='< Every\nfew weeks',
					         'Every few weeks at most'='~Every\nfew weeks',
					         'About once a day'='~1\na day',
					         'A few times a week'='Few\na week',
					         'Several times a day'='Several\na day')) %>%
	mutate(follow_disagree = recode(follow_disagree, 
									'Not sure/No response'='Not sure/\nNo response',
									'Yes'='Yes',
									'No'='No')) %>%
	mutate(follow_disagree = factor(follow_disagree, levels = rev(c('Yes', 'No', 'Not sure/\nNo response')))) %>%
	ggplot(aes(y=follow_disagree, x=pct, fill=freq)) +
	geom_bar(stat="identity", position = position_stack()) +
	geom_text(aes(label=ifelse(freq == 'Several\na day', paste0(round(pct*100,0),"%"),""), 
				  x=(pct/2)+0.04,
				  group=follow_disagree), size=3.5, color="white", fontface = "bold") +
	ylab("Follow non-congenial media?") + xlab("% of self-reported users") +
	ggtitle("Since the election how often did you encounter credibility labels on...") + 
	scale_fill_brewer(palette= "YlOrRd", name="", guide = guide_legend(reverse = TRUE, nrow=1)) +
	scale_x_continuous(labels = scales::percent_format(accuracy=1), limits=c(0,1)) +
	facet_grid(. ~ platform) +
	my_theme + 
	theme(legend.position="bottom",
		  strip.text = element_text(size=12),
		  axis.text.y = element_text(size=10),
		  axis.text.x = element_text(size=5),
		  axis.title.y = element_text(size=10))
		  
ggsave("figures_explore/encounter4.png", width=8.5, height=2.5)
system("open figures_explore/encounter4.png")

####** delete? ####
dat %>%
	select(delete, demog_PID_2pt) %>%
	separate_rows(delete, sep="\\t\\t\\t\\t\\t\\t\\t\\t,") %>%
	filter(!is.na(delete)) %>% 
	filter(delete != "None") %>%
	group_by(demog_PID_2pt, delete) %>%
	summarise(n=n()) %>%
	mutate(pct=n/nrow(dat)) %>%
	arrange(pct) %>%
	mutate(delete = as_factor(delete)) %>%
	ggplot(aes(y=delete, x=pct, fill=demog_PID_2pt)) +
	geom_bar(stat="identity", color="black") +
	scale_fill_manual(values=c("blue","red","grey"), name="PID:") + 
	ggtitle("Have you temporarily or permanently deleted any of the following platforms\nbecause you disagreed with their content moderation choices?") + 
	scale_x_continuous(labels = scales::percent_format(accuracy=1)) +
	ylab("") + xlab("% of respondents") + my_theme + theme(legend.position="bottom")
ggsave("figures_explore/delete.png", width=8, height=4)
system("open figures_explore/delete.png")

dat %>%
	select(delete, demog_PID_6pt) %>%
	separate_rows(delete, sep="\\t\\t\\t\\t\\t\\t\\t\\t,") %>%
	filter(!is.na(delete)) %>% 
	filter(delete != "None") %>%
	group_by(demog_PID_6pt, delete) %>%
	summarise(n=n()) %>%
	mutate(pct=n/nrow(dat)) %>%
	arrange(pct) %>%
	mutate(delete = as_factor(delete)) %>%
	ggplot(aes(y=delete, x=pct, fill=demog_PID_6pt)) +
	geom_bar(stat="identity") +
	scale_fill_manual(values=c("blue","lightblue","grey","red","darkred","black"), name="PID:") + 
	ggtitle("Have you temporarily or permanently deleted any of the following platforms\nbecause you disagreed with their content moderation choices?") + 
	scale_x_continuous(labels = scales::percent_format(accuracy=1)) +
	ylab("") + xlab("% of respondents") + my_theme + theme(legend.position="bottom")
ggsave("figures_explore/delete2.png", width=8, height=3)
system("open figures_explore/delete2.png")

####** confidence in institutions? ####
confid <- dat %>% select(confid_inst_electoffic,
			   confid_inst_factcheckers,
			   confid_inst_msm,
			   confid_inst_localnews,
			   confid_inst_academics,
			   confid_inst_socmedcompanies,
			   confid_inst_pplfollow,
			   confid_inst_supremecourt,
			   demog_PID_2pt) %>%
	gather(key="inst", value="freq", -demog_PID_2pt) %>%
	filter(!is.na(freq), !grepl("Other", freq)) %>%
	group_by(inst) %>% mutate(tot=n()) %>% ungroup() %>%
	group_by(demog_PID_2pt, inst, freq) %>% summarise(pct=n()/tot, .groups="keep") %>% distinct() %>%
	mutate(inst=gsub("confid_inst_","", inst)) %>%
	mutate(inst=gsub("electoffic","Elected Officials", inst)) %>% 
	mutate(inst=gsub("factcheckers","Fact Checkers", inst)) %>% 
	mutate(inst=gsub("msm","Mainstream Media", inst)) %>% 
	mutate(inst=gsub("localnews","Local News", inst)) %>% 
	mutate(inst=gsub("academics","Academics", inst)) %>% 
	mutate(inst=gsub("socmedcompanies","Social Media\nCompanies", inst)) %>% 
	mutate(inst=gsub("pplfollow","People You Follow", inst)) %>% 
	mutate(inst=gsub("supremecourt","The Supreme\nCourt", inst)) %>% 
	mutate(freq=factor(freq)) %>%
	ggplot(aes(y=freq, x=pct, fill=demog_PID_2pt))

confid +
	geom_bar(stat="identity", color="black", position="dodge", width=0.8) +
	ylab("") + xlab("% of self-reported users") +
	ggtitle("Confidence in groups to make decisions in public interest") + 
	scale_x_continuous(labels = scales::percent_format(accuracy=1)) +
	scale_fill_manual(values=c("blue","red","grey"), name="PID") + 
	facet_wrap(. ~ inst) +
	scale_y_discrete(labels=confid_levels) + 
	# coord_flip() + 
	theme_bw()
ggsave("figures_explore/confid.png", width=8, height=6)
system("open figures_explore/confid.png")

confid +
	geom_bar(stat="identity", color="black", width=0.8) +
	ylab("") + xlab("% of self-reported users") +
	ggtitle("Confidence in groups to make decisions in public interest") + 
	scale_x_continuous(labels = scales::percent_format(accuracy=1)) +
	scale_fill_manual(values=c("blue","red","grey"), name="PID") + 
	facet_wrap(. ~ inst) +
	scale_y_discrete(labels=confid_levels) + 
	# coord_flip() + 
	theme_bw()
ggsave("figures_explore/confid2.png", width=8, height=6)
system("open figures_explore/confid2.png")

dat %>% select(confid_inst_electoffic,
			   confid_inst_factcheckers,
			   confid_inst_msm,
			   confid_inst_localnews,
			   confid_inst_academics,
			   confid_inst_socmedcompanies,
			   confid_inst_pplfollow,
			   confid_inst_supremecourt,
			   demog_PID_2pt) %>%
	gather(key="inst", value="freq", -demog_PID_2pt) %>%
	filter(!is.na(freq), !grepl("Other", freq)) %>%
	group_by(demog_PID_2pt, inst) %>% summarise(m=mean(freq), sd=sd(freq), n=n(), .groups="keep") %>% distinct() %>%
	mutate(inst=gsub("confid_inst_","", inst)) %>%
	mutate(inst=gsub("electoffic","Elected Officials", inst)) %>% 
	mutate(inst=gsub("factcheckers","Fact Checkers", inst)) %>% 
	mutate(inst=gsub("msm","Mainstream Media", inst)) %>% 
	mutate(inst=gsub("localnews","Local News", inst)) %>% 
	mutate(inst=gsub("academics","Academics", inst)) %>% 
	mutate(inst=gsub("socmedcompanies","Social Media\nCompanies", inst)) %>% 
	mutate(inst=gsub("pplfollow","People You Follow", inst)) %>% 
	mutate(inst=gsub("supremecourt","The Supreme\nCourt", inst)) %>% 
	ggplot(aes(x=demog_PID_2pt, y=m, ymin=m-1.96*sd/sqrt(n), ymax=m+1.96*sd/sqrt(n), fill=demog_PID_2pt)) +
	geom_bar(stat="identity", color="black") +
	geom_linerange(color="black", lwd=1) + 
	xlab("") + ylab("confidence level (1-5)") +
	ggtitle("Confidence in groups to make decisions in public interest") + 
	scale_fill_manual(values=c("blue","red","grey")) + 
	facet_wrap(. ~ inst) +
	coord_flip() +
	my_theme + 
	theme(legend.position = "none")
ggsave("figures_explore/confid3.png", width=8, height=4)
system("open figures_explore/confid3.png")
 
dat %>% select(confid_inst_electoffic,
			   confid_inst_factcheckers,
			   confid_inst_msm,
			   confid_inst_localnews,
			   confid_inst_academics,
			   confid_inst_socmedcompanies,
			   confid_inst_pplfollow,
			   confid_inst_supremecourt,
			   demog_PID_2pt) %>%
	gather(key="inst", value="freq", -demog_PID_2pt) %>%
	filter(!is.na(freq), !grepl("Other", freq)) %>%
	group_by(demog_PID_2pt, inst) %>% summarise(m=mean(freq), sd=sd(freq), n=n(), .groups="keep") %>% distinct() %>%
	mutate(inst=gsub("confid_inst_","", inst)) %>%
	mutate(inst=gsub("electoffic","Elected Officials", inst)) %>% 
	mutate(inst=gsub("factcheckers","Fact Checkers", inst)) %>% 
	mutate(inst=gsub("msm","Mainstream Media", inst)) %>% 
	mutate(inst=gsub("localnews","Local News", inst)) %>% 
	mutate(inst=gsub("academics","Academics", inst)) %>% 
	mutate(inst=gsub("socmedcompanies","Social Media Companies", inst)) %>% 
	mutate(inst=gsub("pplfollow","People You Follow", inst)) %>% 
	mutate(inst=gsub("supremecourt","The Supreme Court", inst)) %>% 
	ggplot(aes(x=inst, y=m, ymin=m-1.96*sd/sqrt(n), ymax=m+1.96*sd/sqrt(n), fill=demog_PID_2pt)) +
	geom_segment(aes(colour=demog_PID_2pt, x=inst, xend=inst, y=2, yend=m), stat="identity", size=4.25) + 
	geom_linerange(color="black", lwd=1) + 
	xlab("") + ylab("Confidence level (1-5)") + ylim(2, 4) +
	ggtitle("Confidence in groups to make decisions in public interest") + 
	scale_fill_manual(values=c("blue","red","grey")) + 
	facet_wrap(. ~ demog_PID_2pt) +
	coord_flip() +
	my_theme +
	theme(legend.position = "none", strip.text = element_text(size=12))
ggsave("figures_explore/confid4.png", width=8, height=2.25)
system("open figures_explore/confid4.png")
 
####** labels approval ####
dat %>% 
	select(labels_approve, remove_approve, downrank_approve) %>%
	gather(key="outcome", value="support") %>%
	filter(!is.na(support)) %>%
	group_by(outcome, support) %>% 
	summarise(n=n()) %>%
	mutate(pct=n/nrow(dat)) %>%
	arrange(pct) %>%
	mutate(support=factor(support)) %>%
	mutate(outcome=gsub("downrank_approve", "Content\ndown-ranking", outcome)) %>%
	mutate(outcome=gsub("labels_approve", "Content\nlabels", outcome)) %>%
	mutate(outcome=gsub("remove_approve", "Content\nremoval", outcome)) %>%
	ggplot(aes(y=support, x=pct)) + 
	scale_y_discrete(labels=approve_levels) +
	scale_x_continuous(labels = scales::percent_format(accuracy=1)) +
	xlab("% of users") + ylab("") + 
	geom_bar(stat="identity", color="black") + 
	ggtitle("Support for specific interventions") + 
	facet_wrap(~ outcome) + 
	theme_bw()
ggsave("figures_explore/interv_approval.png", width=8, height=2.5)
system("open figures_explore/interv_approval.png")

dat %>% 
	select(labels_approve, remove_approve, downrank_approve, demog_PID_2pt, demog_PID_6pt) %>%
	gather(key="outcome", value="support", -demog_PID_2pt, -demog_PID_6pt) %>%
	filter(!is.na(support)) %>%
	group_by(outcome) %>% mutate(tot=n()) %>% ungroup() %>%
	group_by(outcome, demog_PID_2pt, demog_PID_6pt, support) %>% summarise(pct=n()/tot, .groups="keep") %>% distinct() %>%
	arrange(pct) %>%
	mutate(support=factor(support)) %>%
	mutate(outcome=gsub("downrank_approve", "Downranking", outcome)) %>%
	mutate(outcome=gsub("labels_approve", "Labels", outcome)) %>%
	mutate(outcome=gsub("remove_approve", "Removal", outcome)) %>%
	ggplot(aes(y=support, x=pct, fill=demog_PID_6pt)) + 
	scale_y_discrete(labels=approve_levels) +
	scale_x_continuous(labels = scales::percent_format(accuracy=1)) +
	scale_fill_manual(values=c("blue","lightblue","grey","red","darkred","black"), name="") + 
	xlab("% of users") + ylab("") + 
	geom_bar(stat="identity", color="black") + 
	ggtitle("Support for specific interventions") + 
	facet_grid(demog_PID_2pt ~ outcome) + 
	my_theme + 
	theme(axis.text.x = element_text(size=4),
		  legend.position = "top", 
		  legend.key.size = unit(0.5, 'cm'), #change legend key size
          legend.key.height = unit(0.5, 'cm'), #change legend key height
          legend.key.width = unit(0.5, 'cm'), #change legend key width
          legend.title = element_text(size=6), #change legend title font size
          legend.text = element_text(size=5))
ggsave("figures_explore/interv_approval2.png", width=5, height=4)
system("open figures_explore/interv_approval2.png")

p <- bind_rows(
	dat %>% 
		select(labels_approve, remove_approve, downrank_approve, demog_PID_2pt) %>%
		gather(key="outcome", value="support", -demog_PID_2pt) %>%
		filter(!is.na(support)) %>%
		group_by(outcome) %>% mutate(tot=n()) %>% ungroup() %>%
		group_by(outcome, demog_PID_2pt, support) %>% summarise(pct=n()/tot, .groups="keep") %>% distinct() %>%
		arrange(pct) 
	,
	dat %>% 
		select(labels_approve, remove_approve, downrank_approve) %>%
		gather(key="outcome", value="support") %>%
		filter(!is.na(support)) %>%
		group_by(outcome) %>% mutate(tot=n()) %>% ungroup() %>%
		group_by(outcome, support) %>% summarise(pct=n()/tot, .groups="keep") %>% distinct() %>%
		arrange(pct) %>% mutate(demog_PID_2pt = "All")
) %>%  mutate(demog_PID_2pt=factor(demog_PID_2pt, levels=c("All","Democrat","Republican","Neither"))) %>%
	mutate(support=factor(support)) %>%
	mutate(outcome=gsub("downrank_approve", "Downranking", outcome)) %>%
	mutate(outcome=gsub("labels_approve", "Labels", outcome)) %>%
	mutate(outcome=gsub("remove_approve", "Removal", outcome)) %>%
	ggplot(aes(y=support, x=pct, fill=demog_PID_2pt)) + 
	scale_y_discrete(labels=approve_levels) +
	scale_x_continuous(labels = scales::percent_format(accuracy=1)) +
	scale_fill_manual(values=c("white","blue","red","grey"), name="") + 
	xlab("% of users") + ylab("") + 
	geom_bar(stat="identity", color="black") + 
	geom_text(aes(x=pct+0.03, y=support, label=paste0(round(pct*100,1),"%"), group=demog_PID_2pt), size=2) +
	ggtitle("Support for specific interventions") + 
	facet_grid(demog_PID_2pt ~ outcome) + 
	my_theme + 
	theme(axis.text.x = element_text(size=6),
		  strip.text.y = element_text(size = 6),
		  legend.position = "none", 
		  legend.key.size = unit(0.5, 'cm'), #change legend key size
          legend.key.height = unit(0.5, 'cm'), #change legend key height
          legend.key.width = unit(0.5, 'cm'), #change legend key width
          legend.title = element_text(size=6), #change legend title font size
          legend.text = element_text(size=5))

specific_PID6pt_df <- bind_rows(tidy(mod.lab1) %>% mutate(outcome = "Labels", model = "univariate"),
		  tidy(mod.lab2) %>% mutate(outcome = "Labels", model = "adjusted"),
		  tidy(mod.rem1) %>% mutate(outcome = "Removal", model = "univariate"),
		  tidy(mod.rem2) %>% mutate(outcome = "Removal", model = "adjusted"),
		  tidy(mod.dr1) %>% mutate(outcome = "Downranking", model = "univariate"),
		  tidy(mod.dr2) %>% mutate(outcome = "Downranking", model = "adjusted")) %>%
	filter(grepl("PID", term)) %>%
	mutate(term = gsub("demog_PID_6pt","", term)) %>%
	mutate(sig=ifelse(estimate-1.96*std.error < 0 & estimate+1.96*std.error > 0, 
	                  "gray", "black")) %>%
	mutate(term = factor(term, levels = c(
		"Weak Democrat", "True Independent", "Weak Republican", "Strong Republican", "Everyone else"
	)))

specific_PID6pt_df <- specific_PID6pt_df[order(specific_PID6pt_df$p.value),]
alpha <- 0.05
k <- nrow(specific_PID6pt_df); r <- 1:k
alpha.adj <- (r*alpha)/k
specific_PID6pt_df$z.crit.adj <- qnorm(1 - (alpha.adj)/2)
specific_PID6pt_df$sig.BH <- ifelse(specific_PID6pt_df$p.value < alpha.adj, "black", "gray")

p2 <- ggplot(specific_PID6pt_df, aes(x=term)) +
	## null hypothesis
	geom_hline(yintercept=0, lty=2, alpha=0.5) +
	## BHq CI
	geom_linerange(aes(y=estimate, 
					   ymin=estimate-z.crit.adj*std.error, 
					   ymax=estimate+z.crit.adj*std.error, 
					   shape=model, colour=sig.BH), 
				   position=position_dodge(width=.55), 
				   size=0.5, stroke=.5) +
	## 95% CI
	geom_pointrange(aes(y=estimate,
						ymin=estimate-1.96*std.error,
						ymax=estimate+1.96*std.error,
						shape=model, colour=sig.BH),
					position=position_dodge(width=.55),
					size=0.75, stroke=.5) +
	## 90% CI
	geom_linerange(aes(y=estimate,
					   ymin=estimate-1.65*std.error,
					   ymax=estimate+1.65*std.error,
					   shape=model, colour=sig.BH),
				   position=position_dodge(width=.55),
				   size=0.8, stroke=.5) +
	scale_colour_identity() + 
	scale_shape_manual(values=c(16,25,15)) + 
	facet_grid(~ outcome) + ## different outcomes
	ylab(TeX("Marginal $(\\pm \\sigma)$ level of support (relative to Strong Democrat)")) + 
	xlab("") +
	coord_flip() + 
	my_theme + 
	theme(axis.text.x = element_text(size=8),
		  # axis.title.x = element_text(size=10),
		  plot.margin = margin(5, 0, 7, -5),
		  legend.text = element_text(size = 8),
		  legend.title = element_text(size = 10),
		  legend.spacing.y = unit(1.0, 'cm'),
		  legend.position="bottom") + 
	guides(shape=guide_legend(title="model:", nrow=1))

p_p2 <- cowplot::plot_grid(p + theme(axis.text.y=element_text(size=8)), 
						   p2 + theme(plot.margin=unit(x=c(-3,0,0,0),units="mm"), strip.background = element_blank()), 
						   align="v", axis="lr", nrow = 2, rel_heights=c(2.2,1.4))
cowplot::save_plot(p_p2, file ="figures_explore/interv_approval2_1.png", base_width=8.5, base_height=5.5)
system("open figures_explore/interv_approval2_1.png")

####** labels eval ####
dat %>% 
	select(demog_PID_2pt, `Correct/appropriate`=labels_eval_1, `Incorrect/inappropriate`=labels_eval_2, `Informative for you`=labels_eval_3, `Informative for others`=labels_eval_4) %>%
	gather(key="labels_eval", value="value", -demog_PID_2pt) %>%
	mutate(value=value/6) %>%
	group_by(demog_PID_2pt) %>% mutate(tot=n()) %>% ungroup() %>%
	group_by(demog_PID_2pt, labels_eval) %>%
	summarise(m=mean(value, na.rm=T), se=sd(value, na.rm=T)/sqrt(tot)) %>% distinct() %>%
	# mutate(labels_eval_human= gsub(" errors", "\nerrors\n", labels_eval_human)) %>%
	ggplot(aes(x=labels_eval, y=m)) + 
	ylab("Frequency [0-1]") + xlab("") + 
	geom_bar(aes(fill=demog_PID_2pt), stat="identity", color="black") + 
	ggtitle("Perceptions of labels") + 
	scale_fill_manual(values=c("blue", "red", "grey")) + 
	facet_wrap(demog_PID_2pt ~ .) + 
	coord_flip() + 
	my_theme + 
	theme(legend.position="none") 
ggsave("figures_explore/labels_eval.png", width=8, height=2.5)
system("open figures_explore/labels_eval.png")

dat %>% 
	select(demog_PID_2pt, `Correct/appropriate`=labels_eval_1, `Incorrect/inappropriate`=labels_eval_2, `Informative for you`=labels_eval_3, `Informative for others`=labels_eval_4) %>%
	gather(key="labels_eval", value="value", -demog_PID_2pt) %>%
	group_by(demog_PID_2pt) %>% mutate(tot=n()) %>% ungroup() %>%
	group_by(demog_PID_2pt, labels_eval) %>%
	summarise(m=mean(value, na.rm=T), se=sd(value, na.rm=T)/sqrt(tot)) %>% distinct() %>%
	# mutate(labels_eval_human= gsub(" errors", "\nerrors\n", labels_eval_human)) %>%
	ggplot(aes(x=labels_eval, y=m)) + 
	ylab("Frequency [0-1]") + xlab("") + 
	geom_bar(aes(fill=demog_PID_2pt), stat="identity", color="black", position=position_dodge(width=1)) + 
	ggtitle("Perceptions of labels") + 
	scale_fill_manual(values=c("blue", "red", "grey"), name="PID:") + 
	coord_flip() + 
	theme_bw()
ggsave("figures_explore/labels_eval.png", width=8, height=2.5)
system("open figures_explore/labels_eval.png")

dat %>% 
	select(demog_PID_2pt, labels_eval_human) %>%
	separate_rows(labels_eval_human, sep=",") %>%
	filter(!is.na(labels_eval_human)) %>%
	group_by(demog_PID_2pt) %>% mutate(tot=n()) %>%
	group_by(demog_PID_2pt, labels_eval_human) %>%
	summarise(pct=n()/tot) %>% distinct() %>%
	mutate(labels_eval_human= gsub("to ", "to\n", labels_eval_human)) %>%
	# mutate(labels_eval_human= gsub(" errors", "\nerrors\n", labels_eval_human)) %>%
	ggplot(aes(x=labels_eval_human, y=pct)) + 
	scale_y_continuous(labels = scales::percent_format(accuracy=1)) +
	ylab("% of self-reported partisans who reported encountered inappropriate labels") + xlab("") + 
	geom_bar(aes(fill=demog_PID_2pt), stat="identity", color="black") + 
	ggtitle("Types of perceived errors in content labels") + 
	scale_fill_manual(values=c("blue", "red", "grey")) + 
	facet_wrap(demog_PID_2pt ~ .) + 
	coord_flip() + 
	my_theme +  
	theme(legend.position="none") 
ggsave("figures_explore/label_error_type.png", width=8, height=2.5)
system("open figures_explore/label_error_type.png")

dat %>% 
	select(demog_PID_2pt, demog_PID_6pt, labels_eval_human) %>%
	separate_rows(labels_eval_human, sep=",") %>%
	filter(!is.na(labels_eval_human)) %>%
	group_by(demog_PID_2pt) %>% mutate(tot=n()) %>%
	group_by(demog_PID_2pt, demog_PID_6pt, labels_eval_human) %>%
	summarise(pct=n()/tot) %>% distinct() %>%
	mutate(labels_eval_human= gsub("to ", "to\n", labels_eval_human)) %>%
	# mutate(labels_eval_human= gsub(" errors", "\nerrors\n", labels_eval_human)) %>%
	ggplot(aes(x=labels_eval_human, y=pct)) + 
	scale_y_continuous(labels = scales::percent_format(accuracy=1)) +
	ylab("% of self-reported partisans who reported encountered inappropriate labels") + xlab("") + 
	geom_bar(aes(fill=demog_PID_6pt), stat="identity", color="black") + 
	ggtitle("Types of perceived errors in content labels") + 
	scale_fill_manual(values=c("blue","lightblue","grey","red","darkred","black"), name="PID:") + 
	facet_wrap(demog_PID_2pt ~ .) + 
	coord_flip() + 
	my_theme + 
	theme(legend.position="none") 
ggsave("figures_explore/label_error_type2.png", width=8, height=2.5)
system("open figures_explore/label_error_type2.png")

dat %>% 
	select(demog_PID_6pt, labels_eval_human) %>%
	separate_rows(labels_eval_human, sep=",") %>%
	filter(!is.na(labels_eval_human)) %>%
	group_by(demog_PID_6pt) %>% mutate(tot=n()) %>%
	group_by(demog_PID_6pt, labels_eval_human) %>%
	summarise(pct=n()/tot) %>% distinct() %>%
	mutate(labels_eval_human= gsub("Due to ", "", labels_eval_human)) %>% as.data.frame() %>%
	mutate(labels_eval_human = as_factor(labels_eval_human)) %>%
	# mutate(labels_eval_human= gsub(" errors", "\nerrors\n", labels_eval_human)) %>%
	ggplot(aes(x=labels_eval_human, y=pct)) + 
	scale_y_continuous(labels = scales::percent_format(accuracy=1), limits=c(0,1)) +
	ylab("% of self-reported users in group who reported encountered inappropriate labels") + xlab("Perceived errors") + 
	geom_bar(aes(fill=demog_PID_6pt), stat="identity", color="black") +
	geom_text(aes(y=pct+0.08, x=labels_eval_human, label=paste0(round(pct*100,1),"%"), group=demog_PID_6pt), size=2) +
	ggtitle("Types of perceived errors in content labels") + 
	scale_fill_manual(values=c("blue","lightblue","grey","red","darkred","black")) + 
	facet_wrap(~ demog_PID_6pt) + 
	coord_flip() + 
	my_theme +  
	theme(legend.position="none", axis.text.x = element_text(size=5), strip.text.x = element_text(size=12)) 
ggsave("figures_explore/label_error_type3.png", width=8, height=2)
system("open figures_explore/label_error_type3.png")

####** should decide ####
decide <- dat %>% select(shoulddecide_algos,
			   shoulddecide_electoffic,
			   shoulddecide_socmedemployees,
			   shoulddecide_factcheckers,
			   shoulddecide_experts,
			   shoulddecide_users_youknow,
			   shoulddecide_users_diverse,
			   shoulddecide_users_moderate,
			   demog_PID_2pt) %>%
	gather(key="inst", value="freq", -demog_PID_2pt) %>%
	filter(!is.na(freq), !grepl("Other", freq)) %>%
	group_by(inst) %>% mutate(tot=n()) %>% ungroup() %>%
	group_by(demog_PID_2pt, inst, freq) %>% summarise(pct=n()/tot, .groups="keep") %>% distinct() %>%
	mutate(inst=gsub("shoulddecide_","", inst)) %>%
	mutate(inst=gsub("algos","Algorithms", inst)) %>% 
	mutate(inst=gsub("electoffic","Elected Officials", inst)) %>% 
	mutate(inst=gsub("socmedemployees","Social Media\nEmployees", inst)) %>% 
	mutate(inst=gsub("factcheckers","Independent\nFact-Checkers", inst)) %>% 
	mutate(inst=gsub("experts","Credentialed\nExperts", inst)) %>% 
	mutate(inst=gsub("users_youknow","Users\nYou Know", inst)) %>% 
	mutate(inst=gsub("users_diverse","Volunteer Users\n(Diverse)", inst)) %>% 
	mutate(inst=gsub("users_moderate","Volunteer Users\n(Moderate)", inst)) %>% 
	mutate(freq=factor(freq)) %>%
	ggplot(aes(y=freq, x=pct, fill=demog_PID_2pt))

decide +
	geom_bar(stat="identity", color="black", position="dodge", width=0.8) +
	ylab("") + xlab("% of self-reported users") +
	ggtitle("Approval as intervention decision-makers") + 
	scale_x_continuous(labels = scales::percent_format(accuracy=1)) +
	scale_fill_manual(values=c("blue","red","grey"), name="PID") + 
	facet_wrap(. ~ inst) +
	scale_y_discrete(labels=approve_levels) + 
	# coord_flip() + 
	theme_bw()
ggsave("figures_explore/decide.png", width=8, height=6)
system("open figures_explore/decide.png")

decide +
	geom_bar(stat="identity", color="black", width=0.8) +
	ylab("") + xlab("% of self-reported users") +
	ggtitle("Confidence in groups to make decisions in public interest") + 
	scale_x_continuous(labels = scales::percent_format(accuracy=1)) +
	scale_fill_manual(values=c("blue","red","grey"), name="PID") + 
	facet_wrap(. ~ inst) +
	scale_y_discrete(labels=approve_levels) + 
	# coord_flip() + 
	theme_bw()
ggsave("figures_explore/decide2.png", width=8, height=6)
system("open figures_explore/decide2.png")

dat %>% select(shoulddecide_algos,
			   shoulddecide_electoffic,
			   shoulddecide_socmedemployees,
			   shoulddecide_factcheckers,
			   shoulddecide_experts,
			   shoulddecide_users_youknow,
			   shoulddecide_users_diverse,
			   shoulddecide_users_moderate,
			   demog_PID_2pt) %>%
	gather(key="inst", value="freq", -demog_PID_2pt) %>%
	filter(!is.na(freq), !grepl("Other", freq)) %>%
	group_by(demog_PID_2pt, inst) %>% summarise(m=mean(freq), sd=sd(freq), n=n(), .groups="keep") %>% distinct() %>%
	mutate(inst=gsub("shoulddecide_","", inst)) %>%
	mutate(inst=gsub("algos","Algorithms", inst)) %>% 
	mutate(inst=gsub("electoffic","Elected Officials", inst)) %>% 
	mutate(inst=gsub("socmedemployees","Social Media\nEmployees", inst)) %>% 
	mutate(inst=gsub("factcheckers","Independent\nFact-Checkers", inst)) %>% 
	mutate(inst=gsub("experts","Credentialed\nExperts", inst)) %>% 
	mutate(inst=gsub("users_youknow","Users\nYou Know", inst)) %>% 
	mutate(inst=gsub("users_diverse","Volunteer Users\n(Diverse)", inst)) %>% 
	mutate(inst=gsub("users_moderate","Volunteer Users\n(Moderate)", inst)) %>%
	ggplot(aes(x=demog_PID_2pt, y=m, ymin=m-1.96*sd/sqrt(n), ymax=m+1.96*sd/sqrt(n))) +
	geom_bar(aes(fill=demog_PID_2pt), color="black", stat="identity") + 
	geom_linerange(color="black", lwd=1) + 
	scale_fill_manual(values=c("blue", "red", "gray")) + 
	xlab("") + ylab("approval level (1-5)") +
	facet_wrap(~ inst) +
	coord_flip() + 
	my_theme + 
	theme(legend.position = "none")
ggsave("figures_explore/decide3.png", width=8, height=5)
system("open figures_explore/decide3.png")

bind_rows(
	dat %>% select(shoulddecide_algos,
				   shoulddecide_electoffic,
				   shoulddecide_socmedemployees,
				   shoulddecide_factcheckers,
				   shoulddecide_experts,
				   shoulddecide_users_youknow,
				   shoulddecide_users_diverse,
				   shoulddecide_users_moderate,
				   demog_PID_2pt) %>%
		gather(key="inst", value="freq", -demog_PID_2pt) %>%
		filter(!is.na(freq), !grepl("Other", freq)) %>%
		group_by(demog_PID_2pt, inst) %>% summarise(m=mean(freq), sd=sd(freq), n=n(), .groups="keep") %>% distinct()
	,
	dat %>% select(shoulddecide_algos,
				   shoulddecide_electoffic,
				   shoulddecide_socmedemployees,
				   shoulddecide_factcheckers,
				   shoulddecide_experts,
				   shoulddecide_users_youknow,
				   shoulddecide_users_diverse,
				   shoulddecide_users_moderate) %>%
		gather(key="inst", value="freq") %>%
		filter(!is.na(freq), !grepl("Other", freq)) %>%
		group_by(inst) %>% summarise(m=mean(freq), sd=sd(freq), n=n(), .groups="keep") %>% distinct() %>%
		mutate(demog_PID_2pt = "All")
) %>% mutate(demog_PID_2pt = factor(demog_PID_2pt, levels=c("All","Democrat","Republican","Neither"))) %>%
	mutate(inst=gsub("shoulddecide_","", inst)) %>%
	mutate(inst=gsub("algos","Algorithms", inst)) %>% 
	mutate(inst=gsub("electoffic","Elected Officials", inst)) %>% 
	mutate(inst=gsub("socmedemployees","Social Media Employees", inst)) %>% 
	mutate(inst=gsub("factcheckers","Independent Fact-Checkers", inst)) %>% 
	mutate(inst=gsub("experts","Credentialed Experts", inst)) %>% 
	mutate(inst=gsub("users_youknow","Users You Know", inst)) %>% 
	mutate(inst=gsub("users_diverse","Volunteer Users (Diverse)", inst)) %>% 
	mutate(inst=gsub("users_moderate","Volunteer Users (Moderate)", inst)) %>%
	ggplot(aes(x=inst, y=m, ymin=m-1.96*sd/sqrt(n), ymax=m+1.96*sd/sqrt(n))) +
	geom_bar(aes(fill=demog_PID_2pt), color="black", stat="identity") + 
	geom_linerange(color="black", lwd=1) + 
	scale_fill_manual(values=c("white","blue", "red", "gray")) + 
	xlab("") + ylab("Approval level (1-5)") +
	facet_grid(~ demog_PID_2pt) +
	coord_flip() + 
	my_theme +
	theme(legend.position = "none")
ggsave("figures_explore/decide4.png", width=8, height=2)
system("open figures_explore/decide4.png")

####** confidence in companies? ####
dat %>% select(confid_facebook,
			   confid_twitter,
			   confid_tiktok,
			   confid_instagram,
			   confid_reddit,
			   confid_google,
			   confid_youtube,
			   confid_whatsapp,
			   demog_PID_2pt) %>%
	gather(key="inst", value="confid", -demog_PID_2pt) %>%
	filter(!is.na(confid)) %>%
	group_by(demog_PID_2pt, inst) %>% summarise(m=mean(confid), sd=sd(confid), n=n(), .groups="keep") %>% distinct() %>%
	mutate(inst=gsub("confid_","", inst)) %>%
	mutate(inst=stringr::str_to_title(inst)) %>%
	ggplot(aes(x=demog_PID_2pt, y=m, ymin=m-1.96*sd/sqrt(n), ymax=m+1.96*sd/sqrt(n))) +
	geom_bar(aes(fill=demog_PID_2pt), color="black", stat="identity") + 
	geom_linerange(color="black", lwd=1) + 
	scale_fill_manual(values=c("blue", "red", "gray")) + 
	xlab("") + ylab("approval level (1-5)") +
	facet_wrap(~ inst) +
	ggtitle("Confidence in social media companies to make decisions in public interest") + 
	coord_flip() + 
	my_theme + 
	theme(legend.position = "none")
ggsave("figures_explore/socmed_confid3.png", width=8, height=3.5)
system("open figures_explore/socmed_confid3.png")

dat %>% select(confid_facebook,
			   confid_twitter,
			   confid_tiktok,
			   confid_instagram,
			   confid_reddit,
			   confid_google,
			   confid_youtube,
			   confid_whatsapp,
			   demog_PID_2pt) %>%
	gather(key="inst", value="confid", -demog_PID_2pt) %>%
	filter(!is.na(confid)) %>%
	group_by(demog_PID_2pt, inst) %>% summarise(m=mean(confid), sd=sd(confid), n=n(), .groups="keep") %>% distinct() %>%
	mutate(inst=gsub("confid_","", inst)) %>%
	mutate(inst=stringr::str_to_title(inst)) %>%
	ggplot(aes(x=inst, y=m, ymin=m-1.96*sd/sqrt(n), ymax=m+1.96*sd/sqrt(n))) +
	geom_segment(aes(colour=demog_PID_2pt, x=inst, xend=inst, y=1.5, yend=m), stat="identity", size=4.25) + 
	geom_linerange(color="black", lwd=1) + 
	scale_colour_manual(values=c("blue", "red", "gray")) + 
	xlab("") + ylab("Approval level (1-5)") + ylim(c(1.5, 3.5)) +
	facet_wrap(~ demog_PID_2pt) +
	ggtitle("Confidence in social media companies to make decisions in public interest") + 
	coord_flip() + 
	my_theme + 
	theme(legend.position = "none", strip.text = element_text(size=12))
ggsave("figures_explore/socmed_confid4.png", width=8, height=2.5)
system("open figures_explore/socmed_confid4.png")


#####------------------------------------------------------#
##### Support -- clustering analysis ####
#####------------------------------------------------------#

TOP_N <- 10
outcomes <- list(support_idx="Support for Interventions",
				 labels_approve="Support for Labels",
				 downrank_approve="Support for Downranking",
				 remove_approve="Support for Removal")

clus_vars <- c("demog_PID_6pt",
			   "demog_agegroup",
			   "demog_gender",
			   "demog_educ",
			   "demog_income",
			   "demog_diglit",
			   "demog_race_white",
			   "demog_race_black",
			   "demog_race_asian",
			   "usage_facebook",
			   "usage_tiktok",
			   "usage_twitter",
			   "usage_whatsapp",
			   "usage_reddit",
			   "usage_instagram",
			   "usage_youtube",
			   "usage_n",
			   "usage_avg",
			   "confid_inst_academics",
			   "confid_inst_electoffic",
			   "confid_inst_factcheckers",
			   "confid_inst_localnews",
			   "confid_inst_msm",
			   "confid_inst_pplfollow",
			   "confid_inst_supremecourt",
			   "confid_inst_socmedcompanies",
			   "moder_you",
			   "moder_others",
			   "moder_good",
			   "moder_bad",
			   "follow_disagree",
			   "inst_trust",
			   "follow_news",
			   "news_slants_avg")

confid <- 1 ## re-run the below for both 0 and 1
for (outcome_var in names(outcomes)) {

	##MAKE DATA
	outcome_label <- outcomes[[outcome_var]]
	dat_fmt <- dat[c(clus_vars,outcome_var)] %>%
		mutate(## PID
			   `Strong Republican`=ifelse(demog_PID_6pt == "Strong Republican", 1, 0),
			   `Weak Republican`=ifelse(demog_PID_6pt == "Weak Republican", 1, 0),
			   `True Independent`=ifelse(demog_PID_6pt == "True Independent", 1, 0),
			   `Weak Democrat`=ifelse(demog_PID_6pt == "Weak Democrat", 1, 0),
			   ## Age group
			   `25-34 y.o.`=ifelse(demog_agegroup == "25-34", 1, 0),
			   `35-44 y.o.`=ifelse(demog_agegroup == "35-44", 1, 0),
			   `45-64 y.o.`=ifelse(demog_agegroup == "45-64", 1, 0),
			   `65+ y.o.`=ifelse(demog_agegroup == "65+", 1, 0),
			   ## Digital literacy
			   `Digital literacy`=demog_diglit,
			   ## Gender
			   `Female`=ifelse(demog_gender == "Female", 1, 0),
			   ## Race 
			   `Black` = as.numeric(demog_race_black),
			   `Asian` = as.numeric(demog_race_asian),
			   ## Education
			   `College credentialed`=ifelse(demog_educ == "College", 1, 0),
			   `High school credentialed`=ifelse(demog_educ == "High school", 1, 0),
			   `Postgraduate credentialed`=ifelse(demog_educ == "Postgraduate", 1, 0),
			   ## Income
			   `$100,000 to $149,999`=ifelse(demog_income == "$100,000 to $149,999", 1, 0),
			   `$150,000 or more`=ifelse(demog_income == "$150,000 or more", 1, 0),
			   `$25,000 to $49,999`=ifelse(demog_income == "$25,000 to $49,999", 1, 0),
			   `$50,000 to $74,999`=ifelse(demog_income == "$50,000 to $74,999", 1, 0),		   
			   `$75,000 to $99,999`=ifelse(demog_income == "$75,000 to $99,999", 1, 0),
			   `Less than $25,000`=ifelse(demog_income == "Less than $25,000", 1, 0),
			   ## Confid
			   `Trust in academics`=confid_inst_academics,
			   `Trust in elected officials`=confid_inst_electoffic,
			   `Trust in fact-checkers`=confid_inst_factcheckers,
			   `Trust in local news`=confid_inst_localnews,
			   `Trust in MSM`=confid_inst_msm,
			   `Trust in users I follow`=confid_inst_pplfollow,
			   `Trust in SCOTUS`=confid_inst_supremecourt,
			   `Trust in SM companies`=confid_inst_socmedcompanies,
			   `Institutional trust`=inst_trust,
			   ## Others
			   `Follow uncongenial news` = ifelse(follow_disagree == "Yes", 1, 0),
			   `Follow news (freq)` = follow_news,
			   # `Cons. media slant`= news_slants_avg,
			   # `YouTube usage` = usage_youtube,
			   # `TikTik usage` = usage_tiktok,
			   # `Facebook usage` = usage_facebook,
			   # `Instagram usage` = usage_instagram,
			   # `Reddit usage` = usage_reddit,
			   # `WhatsApp usage` = usage_whatsapp,
			   `Social media accts` = usage_n,
			   `Social media usage` = usage_avg,
			   ## Experience
			   `Seen good intervention`=moder_good,
			   `Seen bad intervention`=moder_bad,
			   `Used intervention`=moder_others,
			   `Received intervention`=moder_you
			   )
	if (confid == 1) {
		dat_fmt <- dat_fmt[!grepl("Institutional", colnames(dat_fmt))]
	} else {
		dat_fmt <- dat_fmt[!grepl("Trust in", colnames(dat_fmt))]
	}
	dat_fmt <- dat_fmt[!(colnames(dat_fmt) %in% clus_vars)]
	dat_fmt <- na.omit(dat_fmt)
	
	dat_outcome <- dat_fmt[outcome_var]
	dat_scaled <- scale(dat_fmt[colnames(dat_fmt) != outcome_var], center = T, scale = T)
	
	##Lasso
	library(caret)
    library(glmnet) ##https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html

    ## regression with L-1 (abs value / "diamond"-shaped) regularization
    ## - pro: encourages sparsity of non-zero coefficients
    ## - con: more useful for variable selection than optimizing MSE
    ## - con: when k > n, only n covariates selected
    ## - con: for correlated covariates, picks one and discards others
    lasso_fit <- cv.glmnet(x=dat_scaled, y=unlist(dat_outcome))
    
    lasso_coefs <- coef(lasso_fit)
    lasso_df <- data.frame(
    	coef=lasso_coefs[,1],
    	name=rownames(lasso_coefs),
    	title=outcome_label
    )
    # lasso_df <- lasso_df %>% arrange(abs(coef)) %>% filter(!grepl("Intercept", name)) %>% as.data.frame()
	lasso_df <- lasso_df %>% arrange(abs(coef)) %>% filter(!grepl("Intercept", name), coef != 0.00) %>% as.data.frame()

    lasso_df$name <- factor(lasso_df$name, levels = lasso_df$name)
    pp <- ggplot(lasso_df, aes(x=name, y=coef, )) + 
    	geom_bar(aes(fill=coef), stat="identity") + 
    	coord_flip() + 
    	ylab("Coefficent") + xlab("") + 
    	scale_fill_gradient2(low="darkred", mid="white", high="darkgreen", midpoint=0) + 
    	facet_wrap(.~ title) + 
    	my_theme + theme(legend.position = "none")
	ggsave(sprintf("figures_explore/lasso-%s_%d.png",outcome_var,confid), height=4, width=3.5)
	system(sprintf("open figures_explore/lasso-%s_%d.png",outcome_var,confid))    

	##PCA
	pca_out <- prcomp(dat_scaled)
	summary(pca_out) ## summarise principal components
	PC1 <- pca_out$x[,1]
	PC2 <- pca_out$x[,2]
	
	### viz PCA
	df <- data.frame(PC1, PC2, outcome=unlist(dat_outcome))
	ggplot(df, aes(x=PC1, y=PC2, color=outcome)) + 
		geom_point(aes(color=outcome), size=3) + 
		scale_color_gradient2(low="darkred", mid="white", high="darkgreen", midpoint=mean(dat[[outcome_var]],na.rm=T), name=paste0(outcome_label,":")) + 
		my_theme + 
		theme(legend.position="bottom", legend.direction = "horizontal")
	ggsave(sprintf("figures_explore/pca0-%s_%d.png",outcome_var,confid), height=5, width=9)
	system(sprintf("open figures_explore/pca0-%s_%d.png",outcome_var,confid))
	
	### viz PCA with loadings
	pca_loads <- data.frame(var=rownames(pca_out$rotation), pca_out$rotation)
	pc_scale <- 12
	pca_loads$abs <- sqrt(pca_loads$PC1^2 + pca_loads$PC2^2)
	pca_loads <- pca_loads[order(-pca_loads$abs)[1:TOP_N],]
	
	ggplot(df, aes(x=PC1, y=PC2, color=outcome)) + 
		geom_segment(data=pca_loads, 
					 aes(x=0, y=0, xend=(PC1*pc_scale), yend=(PC2*pc_scale)),
					 arrow=arrow(length = unit(1/2, "picas")), 
					 color = "black") +
		geom_point(aes(color=outcome), size=3) + 
		annotate("label", 
				 x=(pca_loads$PC1*pc_scale), 
				 y=(pca_loads$PC2*pc_scale),
				 label=pca_loads$var, alpha=0.8) +
		scale_color_gradient2(low="darkred", mid="white", high="darkgreen", midpoint=mean(dat[[outcome_var]],na.rm=T), name=paste0(outcome_label,":")) + 
		my_theme + 
		xlab("Principal Component 1") + 
		ylab("Principal Component 2") + 
		theme(legend.position="bottom", legend.direction = "horizontal", axis.title=element_text(size=14), strip.text = element_text(size=12)) +
		guides(color=guide_colorbar(title.position = "top", title.hjust=0.5))
	ggsave(sprintf("figures_explore/pca-%s_%d.png",outcome_var,confid), height=5, width=9)
	system(sprintf("open figures_explore/pca-%s_%d.png",outcome_var,confid))
	
	
	### summarise PCA components 
	summary(pca_out)
	
	pca_out$rotation %>%
		as.data.frame() %>%
		rownames_to_column("var") %>%
		gather(key="PC", value="load", -var) %>%
		filter(PC %in% c("PC1")) %>%
		arrange(-load) %>%
		mutate(var=forcats::as_factor(var)) %>%
		ggplot(aes(x=var, y=load)) + 
		geom_bar(stat="identity", color="black") +
		xlab("") + ylab("PC 1 Loading") + 
		# facet_wrap(~PC) +
		coord_flip() +
		theme_bw()
	ggsave(sprintf("figures_explore/pcaload-%s_%d.png",outcome_var,confid), height=5, width=3.5)
	system(sprintf("open figures_explore/pcaload-%s_%d.png",outcome_var,confid))
}


#####------------------------------------------------------#
##### RQ1 (Partisanship) -- disaggregate into interventions ####
#####------------------------------------------------------#

## 6pt -- type of interventions
h1.1.viz <- bind_rows(
	h1.1.df.lab6pt %>% mutate(outcome="Content labels\napproval", model="univariate"),
	h1.1.df.lab6pt.adj %>% mutate(outcome="Content labels\napproval", model="adjusted"),
	h1.1.df.rem6pt %>% mutate(outcome="Content removal\napproval", model="univariate"),
	h1.1.df.rem6pt.adj %>% mutate(outcome="Content removal\napproval", model="adjusted"),
	h1.1.df.rank6pt %>% mutate(outcome="Content down-ranking\napproval", model="univariate"),
	h1.1.df.rank6pt.adj %>% mutate(outcome="Content down-ranking\napproval", model="adjusted")
) %>%
	filter(grepl("PID", term)) %>%
	mutate(term = gsub("demog_PID_6pt","", term)) %>%
	mutate(sig=ifelse(estimate-1.96*std.error < 0 & estimate+1.96*std.error > 0, 
	                  "gray", "black")) %>%
	mutate(term = factor(term, levels = c(
		"Weak Democrat", "True Independent", "Weak Republican", "Strong Republican", "Everyone else"
	)))

h1.1.viz <- h1.1.viz[order(h1.1.viz$p.value),]
alpha <- 0.05
k <- nrow(h1.1.viz); r <- 1:k
alpha.adj <- (r*alpha)/k
h1.1.viz$z.crit.adj <- qnorm(1 - (alpha.adj)/2)
h1.1.viz$sig.BH <- ifelse(h1.1.viz$p.value < alpha.adj, "black", "gray")

p <- ggplot(h1.1.viz, aes(x=term)) +
	## null hypothesis
	geom_hline(yintercept=0, lty=2, alpha=0.5) +
	## BHq CI
	geom_linerange(aes(y=estimate, 
					   ymin=estimate-z.crit.adj*std.error, 
					   ymax=estimate+z.crit.adj*std.error, 
					   shape=model, colour=sig.BH), 
				   position=position_dodge(width=.25), 
				   size=0.5, stroke=.5) +
	## 95% CI
	geom_pointrange(aes(y=estimate,
						ymin=estimate-1.96*std.error,
						ymax=estimate+1.96*std.error,
						shape=model, colour=sig.BH),
					position=position_dodge(width=.25),
					size=1, stroke=.5) +
	## 90% CI
	geom_linerange(aes(y=estimate,
					   ymin=estimate-1.65*std.error,
					   ymax=estimate+1.65*std.error,
					   shape=model, colour=sig.BH),
				   position=position_dodge(width=.25),
				   size=1.5, stroke=.5) +
	scale_colour_identity() + 
	scale_shape_manual(values=c(16,25,15)) + 
	facet_grid(~ outcome) + ## different outcomes
	ylab(TeX("Marginal Support $(\\pm \\sigma)$ (Relative to Strong Democrat)")) + 
	xlab("") +
	coord_flip() + 
	my_theme +
	theme(axis.text.x = element_text(size=8),
		  strip.text = element_text(size = 10),
		  # axis.title.x = element_text(size=10),
		  plot.margin = margin(5, 0, 7, -5),
		  legend.text = element_text(size = 8),
		  legend.title = element_text(size = 10),
		  legend.spacing.y = unit(1.0, 'cm'),
		  legend.position="bottom") + 
	guides(shape=guide_legend(title="model:", nrow=1))
ggsave("figures_explore/RQ1_disagg.png", width=8, height=4)
system("open figures_explore/RQ1_disagg.png")

## 6pt -- appropriateness of interventions
h1.1.viz2 <- bind_rows(
	h1.1.df.correct6pt %>% mutate(outcome="Correct/appropriate", model="univariate"),
	h1.1.df.correct6pt.adj %>% mutate(outcome="Correct/appropriate", model="adjusted"),
	h1.1.df.incorrect6pt %>% mutate(outcome="Incorrect/inappropriate", model="univariate"),
	h1.1.df.incorrect6pt.adj %>% mutate(outcome="Incorrect/inappropriate", model="adjusted"),
	h1.1.df.informyou6pt %>% mutate(outcome="Informative for you", model="univariate"),
	h1.1.df.informyou6pt.adj %>% mutate(outcome="Informative for you", model="adjusted"),
	h1.1.df.informothers6pt %>% mutate(outcome="Informative for others", model="univariate"),
	h1.1.df.informothers6pt.adj %>% mutate(outcome="Informative for others", model="adjusted")
) %>%
	filter(grepl("PID", term)) %>%
	mutate(term = gsub("demog_PID_6pt","", term)) %>%
	mutate(sig=ifelse(estimate-1.96*std.error < 0 & estimate+1.96*std.error > 0, 
	                  "gray", "black")) %>%
	mutate(term = factor(term, levels = c(
		"Weak Democrat", "True Independent", "Weak Republican", "Strong Republican", "Everyone else"
	)))

h1.1.viz2 <- h1.1.viz2[order(h1.1.viz2$p.value),]
alpha <- 0.05
k <- nrow(h1.1.viz2); r <- 1:k
alpha.adj <- (r*alpha)/k
h1.1.viz2$z.crit.adj <- qnorm(1 - (alpha.adj)/2)
h1.1.viz2$sig.BH <- ifelse(h1.1.viz2$p.value < alpha.adj, "black", "gray")

p <- ggplot(h1.1.viz2, aes(x=term)) +
	## null hypothesis
	geom_hline(yintercept=0, lty=2, alpha=0.5) +
	## BHq CI
	geom_linerange(aes(y=estimate, 
					   ymin=estimate-z.crit.adj*std.error, 
					   ymax=estimate+z.crit.adj*std.error, 
					   shape=model, colour=sig.BH), 
				   position=position_dodge(width=.35), 
				   size=0.5, stroke=.5) +
	## 95% CI
	geom_pointrange(aes(y=estimate,
						ymin=estimate-1.96*std.error,
						ymax=estimate+1.96*std.error,
						shape=model, colour=sig.BH),
					position=position_dodge(width=.35),
					size=1, stroke=.5) +
	## 90% CI
	geom_linerange(aes(y=estimate,
					   ymin=estimate-1.65*std.error,
					   ymax=estimate+1.65*std.error,
					   shape=model, colour=sig.BH),
				   position=position_dodge(width=.35),
				   size=1.5, stroke=.5) +
	scale_colour_identity() + 
	scale_shape_manual(values=c(16,25,15)) + 
	facet_grid(~ outcome) + ## different outcomes
	ylab(TeX("Marginal Agreement $(\\pm \\sigma)$ (Relative to Strong Democrat)")) + 
	xlab("") +
	coord_flip() + 
	my_theme +
	theme(axis.text.x = element_text(size=8),
		  strip.text = element_text(size = 10),
		  # axis.title.x = element_text(size=10),
		  plot.margin = margin(5, 0, 7, -5),
		  legend.text = element_text(size = 8),
		  legend.title = element_text(size = 10),
		  legend.spacing.y = unit(1.0, 'cm'),
		  legend.position="bottom") + 
	guides(shape=guide_legend(title="model:", nrow=1))
ggsave("figures_explore/RQ1_disagg2.png", width=8, height=4)
system("open figures_explore/RQ1_disagg2.png")


## 6pt -- other things
h1.1.viz3 <- bind_rows(
	h1.1.df.falsepos6pt %>% mutate(outcome="False\npositives", model="univariate"),
	h1.1.df.type6pt %>% mutate(outcome="Greater coverage of\nvisual labels", model="univariate"),
	h1.1.df.falsepos6pt.adj %>% mutate(outcome="False\npositives", model="adjusted"),
	h1.1.df.type6pt.adj %>% mutate(outcome="Greater coverage of\nvisual labels", model="adjusted")
) %>%
	filter(grepl("PID", term)) %>%
	mutate(term = gsub("demog_PID_6pt","", term)) %>%
	mutate(sig=ifelse(estimate-1.96*std.error < 0 & estimate+1.96*std.error > 0, 
	                  "gray", "black")) %>%
	mutate(term = factor(term, levels = c(
		"Weak Democrat", "True Independent", "Weak Republican", "Strong Republican", "Everyone else"
	)))

h1.1.viz3 <- h1.1.viz3[order(h1.1.viz3$p.value),]
alpha <- 0.05
k <- nrow(h1.1.viz3); r <- 1:k
alpha.adj <- (r*alpha)/k
h1.1.viz3$z.crit.adj <- qnorm(1 - (alpha.adj)/2)
h1.1.viz3$sig.BH <- ifelse(h1.1.viz3$p.value < alpha.adj, "black", "gray")

p <- ggplot(h1.1.viz3, aes(x=term)) +
	## null hypothesis
	geom_hline(yintercept=0, lty=2, alpha=0.5) +
	## BHq CI
	geom_linerange(aes(y=estimate, 
					   ymin=estimate-z.crit.adj*std.error, 
					   ymax=estimate+z.crit.adj*std.error, 
					   shape=model, colour=sig.BH), 
				   position=position_dodge(width=.35), 
				   size=0.5, stroke=.5) +
	## 95% CI
	geom_pointrange(aes(y=estimate,
						ymin=estimate-1.96*std.error,
						ymax=estimate+1.96*std.error,
						shape=model, colour=sig.BH),
					position=position_dodge(width=.35),
					size=1, stroke=.5) +
	## 90% CI
	geom_linerange(aes(y=estimate,
					   ymin=estimate-1.65*std.error,
					   ymax=estimate+1.65*std.error,
					   shape=model, colour=sig.BH),
				   position=position_dodge(width=.35),
				   size=1.5, stroke=.5) +
	scale_colour_identity() + 
	scale_shape_manual(values=c(16,25,15)) + 
	facet_grid(~ outcome) + ## different outcomes
	ylab(TeX("Marginal Agreement $(\\pm \\sigma)$ (Relative to Strong Democrat)")) + 
	xlab("") +
	coord_flip() + 
	my_theme +
	theme(axis.text.x = element_text(size=8),
		  strip.text = element_text(size = 10),
		  # axis.title.x = element_text(size=10),
		  plot.margin = margin(5, 0, 7, -5),
		  legend.text = element_text(size = 8),
		  legend.title = element_text(size = 10),
		  legend.spacing.y = unit(1.0, 'cm'),
		  legend.position="bottom") + 
	guides(shape=guide_legend(title="model:", nrow=1))
ggsave("figures_explore/RQ1_disagg2.png", width=8, height=3.5)
system("open figures_explore/RQ1_disagg2.png")

#####------------------------------------------------------#
##### RQ2 (Institutional Trust) -- disaggregate institutions ####
#####------------------------------------------------------#

h2.1.viz <- bind_rows(
# h2.1.df.idxinst.adj %>% mutate(outcome="Support for\ninterventions", model="adjusted 1"),
h2.1.df.idxinst.adj2 %>% mutate(outcome="Support for\ninterventions", model="adjusted"),
# h2.1.df.usersinst.adj %>% mutate(outcome="Support for\nuser-decided\ninterventions", model="adjusted 1"),
h2.1.df.usersinst.adj2 %>% mutate(outcome="Support for\nuser-decided\ninterventions", model="adjusted"),
# h2.1.df.algosinst.adj %>% mutate(outcome="Support for\nalgorithm-decided\ninterventions", model="adjusted 1"),
h2.1.df.algosinst.adj2 %>% mutate(outcome="Support for\nalgorithm-decided\ninterventions", model="adjusted"),

h2.1.df.idxinst1 %>% mutate(outcome="Support for\ninterventions", model="univariate"),
h2.1.df.idxinst2 %>% mutate(outcome="Support for\ninterventions", model="univariate"),
h2.1.df.idxinst3 %>% mutate(outcome="Support for\ninterventions", model="univariate"),
h2.1.df.idxinst4 %>% mutate(outcome="Support for\ninterventions", model="univariate"),
h2.1.df.idxinst5 %>% mutate(outcome="Support for\ninterventions", model="univariate"),
h2.1.df.idxinst6 %>% mutate(outcome="Support for\ninterventions", model="univariate"),
h2.1.df.idxinst7 %>% mutate(outcome="Support for\ninterventions", model="univariate"),
h2.1.df.idxinst8 %>% mutate(outcome="Support for\ninterventions", model="univariate"),

h2.1.df.usersinst1 %>% mutate(outcome="Support for\nuser-decided\ninterventions", model="univariate"),
h2.1.df.usersinst2 %>% mutate(outcome="Support for\nuser-decided\ninterventions", model="univariate"),
h2.1.df.usersinst3 %>% mutate(outcome="Support for\nuser-decided\ninterventions", model="univariate"),
h2.1.df.usersinst4 %>% mutate(outcome="Support for\nuser-decided\ninterventions", model="univariate"),
h2.1.df.usersinst5 %>% mutate(outcome="Support for\nuser-decided\ninterventions", model="univariate"),
h2.1.df.usersinst6 %>% mutate(outcome="Support for\nuser-decided\ninterventions", model="univariate"),
h2.1.df.usersinst7 %>% mutate(outcome="Support for\nuser-decided\ninterventions", model="univariate"),
h2.1.df.usersinst8 %>% mutate(outcome="Support for\nuser-decided\ninterventions", model="univariate"),

h2.1.df.algosinst1 %>% mutate(outcome="Support for\nalgorithm-decided\ninterventions", model="univariate"),
h2.1.df.algosinst2 %>% mutate(outcome="Support for\nalgorithm-decided\ninterventions", model="univariate"),
h2.1.df.algosinst3 %>% mutate(outcome="Support for\nalgorithm-decided\ninterventions", model="univariate"),
h2.1.df.algosinst4 %>% mutate(outcome="Support for\nalgorithm-decided\ninterventions", model="univariate"),
h2.1.df.algosinst5 %>% mutate(outcome="Support for\nalgorithm-decided\ninterventions", model="univariate"),
h2.1.df.algosinst6 %>% mutate(outcome="Support for\nalgorithm-decided\ninterventions", model="univariate"),
h2.1.df.algosinst7 %>% mutate(outcome="Support for\nalgorithm-decided\ninterventions", model="univariate"),
h2.1.df.algosinst8 %>% mutate(outcome="Support for\nalgorithm-decided\ninterventions", model="univariate")
)

h2.1.viz <- h2.1.viz %>%
	filter(grepl("confid_inst", term)) %>%
	mutate(sig=ifelse(estimate-1.96*std.error < 0 & estimate+1.96*std.error > 0, 
	                  "gray", "black")) %>%
	mutate(term=gsub("confid_inst_","", term)) %>%
	mutate(term=gsub("electoffic","Elected Officials", term)) %>% 
	mutate(term=gsub("factcheckers","Fact Checkers", term)) %>% 
	mutate(term=gsub("msm","Mainstream Media", term)) %>% 
	mutate(term=gsub("localnews","Local News", term)) %>% 
	mutate(term=gsub("academics","Academics", term)) %>% 
	mutate(term=gsub("socmedcompanies","Social Media Companies", term)) %>% 
	mutate(term=gsub("pplfollow","People You Follow", term)) %>% 
	mutate(term=gsub("supremecourt","The Supreme Court", term)) %>%
	mutate(outcome=factor(outcome, levels=c(
		"Support for\ninterventions",
		"Support for\nuser-decided\ninterventions",
		"Support for\nalgorithm-decided\ninterventions"
	)))

h2.1.viz <- h2.1.viz[order(h2.1.viz$p.value),]
alpha <- 0.05
k <- nrow(h2.1.viz); r <- 1:k
alpha.adj <- (r*alpha)/k
h2.1.viz$z.crit.adj <- qnorm(1 - (alpha.adj)/2)
h2.1.viz$sig.BH <- ifelse(h2.1.viz$p.value < alpha.adj, "black", "gray")

p <- ggplot(h2.1.viz, aes(x=term)) +
	## null hypothesis
	geom_hline(yintercept=0, lty=2, alpha=0.5) +
	## BHq CI
	geom_linerange(aes(y=estimate, 
					   ymin=estimate-z.crit.adj*std.error, 
					   ymax=estimate+z.crit.adj*std.error, 
					   shape=model, colour=sig.BH), 
				   position=position_dodge(width=.35), 
				   size=0.5, stroke=.5) +
	## 95% CI
	geom_pointrange(aes(y=estimate,
						ymin=estimate-1.96*std.error,
						ymax=estimate+1.96*std.error,
						shape=model, colour=sig.BH),
					position=position_dodge(width=.35),
					size=1, stroke=.5) +
	## 90% CI
	geom_linerange(aes(y=estimate,
					   ymin=estimate-1.65*std.error,
					   ymax=estimate+1.65*std.error,
					   shape=model, colour=sig.BH),
				   position=position_dodge(width=.35),
				   size=1.5, stroke=.5) +
	scale_colour_identity() + 
	scale_shape_manual(values=c(16,25,15)) + 
	geom_vline(xintercept=1.5, alpha=0.1) +
	geom_vline(xintercept=2.5, alpha=0.1) +
	geom_vline(xintercept=3.5, alpha=0.1) +
	geom_vline(xintercept=4.5, alpha=0.1) +
	geom_vline(xintercept=5.5, alpha=0.1) +
	geom_vline(xintercept=6.5, alpha=0.1) +
	geom_vline(xintercept=7.5, alpha=0.1) +
	facet_grid(~ outcome) + ## different outcomes
	ylab(TeX("Marginal $(\\pm \\sigma)$ support predicted from marginal confidence")) + 
	xlab("Confidence in...") +
	coord_flip() + 
	my_theme +
	theme(axis.text.x = element_text(size=8),
		  strip.text = element_text(size = 10),
		  # axis.title.x = element_text(size=10),
		  # plot.margin = margin(5, 0, 7, -5),
		  legend.text = element_text(size = 8),
		  legend.title = element_text(size = 10),
		  legend.spacing.y = unit(1.0, 'cm'),
		  legend.position="bottom") + 
	guides(shape=guide_legend(title="model:", nrow=1))
ggsave("figures_explore/RQ2_disagg.png", width=8, height=4)
system("open figures_explore/RQ2_disagg.png")


