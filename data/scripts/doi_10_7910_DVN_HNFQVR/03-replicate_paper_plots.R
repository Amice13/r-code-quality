# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Replicate plots displayed in main paper (mix of confirmatory
# and exploratory results found using pre-registered procedures).
# 
# Author: Soubhik Barari
# 
# Environment:
# - must use R 3.6
# 
# Input:
# - PAI-Misinfo-Feb2021_confirm.RData
# - PAI-Misinfo-Feb2021_explore.RData
#
# Output:
# - figures_paper/*
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

library(tidyverse)
library(ggplot2)
library(latex2exp)

#####------------------------------------------------------#
##### Pre-amble ####
#####------------------------------------------------------#

rm(list=ls())

library(broom)
library(stargazer)
library(ggplot2)
library(tidyverse)

load("PAI-Misinfo-Feb2021_confirm.RData")
load("PAI-Misinfo-Feb2021_explore.RData")

agree_levels <- c("Strongly disagree","Somewhat disagree","Neither agree nor disagree","Somewhat agree","Strongly agree")
approve_levels <- c("Strongly disapprove","Somewhat disapprove","Neither approve nor disapprove","Somewhat approve","Strongly approve")
freq2_levels <- c("Not sure/Other", "Never", "Less often than every few weeks", "Every few weeks at most", "A few times a week", "About once a day", "Several times a day")
confid_levels <- c("No answer","No confidence","A fair amount of confidence","Not too much confidence","Complete confidence")

my_theme <- theme_linedraw() + 
	theme(panel.grid.major.x = element_blank(), 
		  panel.grid.major.y = element_blank())

#####------------------------------------------------------#
##### Figure 1 ####
#####------------------------------------------------------#

p <- bind_rows(
	dat %>% select(labels_freq_FB,
				   labels_freq_IG,
				   labels_freq_TW,
				   labels_freq_TK,
				   labels_freq_YT) %>%
		gather(key="platform", value="freq") %>%
		filter(!is.na(freq)) %>%
		mutate(freq = ifelse(grepl("(Other|Not sure)", freq), "Not sure/Other", freq)) %>%
		group_by(platform) %>% mutate(tot=n()) %>% ungroup() %>%
		group_by(platform, freq) %>% summarise(pct=n()/first(tot), .groups="keep") %>% distinct() %>%
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
		filter(!is.na(freq)) %>%
		mutate(freq = ifelse(grepl("(Other|Not sure)", freq), "Not sure/Other", freq)) %>%
		group_by(platform, demog_PID_2pt) %>% mutate(tot=n()) %>% ungroup() %>%
		group_by(demog_PID_2pt, platform, freq) %>% summarise(pct=n()/first(tot), .groups="keep") %>% distinct()
) %>% mutate(demog_PID_2pt=factor(demog_PID_2pt, levels=rev(c("All","Democrat","Republican","Neither")))) %>%
	mutate(freq = ifelse(grepl("(Other|Not sure)", freq), "Not sure/Other", freq)) %>%
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
	# filter(platform == "Facebook") %>% ##debug ##<BOOKMARK -- looks like `factor()` is the problem ...>
	ggplot(aes(y=demog_PID_2pt, x=pct, fill=freq)) +
	geom_bar(stat="identity", position = position_stack()) +
	# geom_col(position="fill") +
	geom_text(aes(label=ifelse(freq == 'Several\na day', paste0(round(pct*100,0),"%"),""), 
				  x=(pct/2)+0.04,
				  group=demog_PID_2pt), size=3.5, color="white", fontface = "bold") +
	# scale_fill_manual(values=c("black","blue","red","grey"), name="") +
	scale_fill_brewer(palette= "YlOrRd", name="", guide = guide_legend(reverse = TRUE, nrow=1)) +
	ylab("") + xlab("% of self-reported users in group and on platform who responded") +
	ggtitle("Since the election how often did you encounter credibility labels on...") + 
	scale_x_continuous(labels = scales::percent_format(accuracy=1), limits=c(0,1)) +
	facet_wrap(. ~ platform, nrow=1) +
	my_theme + 
	theme(legend.position="bottom", 
		  strip.text = element_text(size=12),
		  axis.text.y = element_text(size=10),
		  axis.text.x = element_text(size=5))

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
	mutate(term = factor(term, levels = rev(c(
		"Weak Democrat", "True Independent", "Weak Republican", "Strong Republican", "Everyone else"
	))))

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
		  strip.text = element_text(size = 12),
		  # axis.title.x = element_text(size=10),
		  axis.text.y = element_text(size=10),
		  plot.margin = margin(5, 0, 7, -5),
		  legend.text = element_text(size = 8),
		  legend.title = element_text(size = 10),
		  legend.spacing.y = unit(1.0, 'cm'),
		  legend.position="bottom") + 
	guides(shape=guide_legend(title="model:", nrow=1))

p_p2 <- cowplot::plot_grid(p + theme(axis.text.y=element_text(size=8)), 
						   p2 + theme(plot.margin=unit(x=c(-3,0,0,0),units="mm"), strip.background = element_blank()), 
						   align="v", axis="lr", nrow = 2, rel_heights=c(1.6,1.2))
cowplot::save_plot(p_p2, file ="figures_paper/fig1.png", base_width=8.5, base_height=5.5)
system("open figures_paper/fig1.png")

# #####------------------------------------------------------#
# ##### Figure 1 (old) ####
# #####------------------------------------------------------#
# 
# p <- bind_rows(
# 	dat %>% select(labels_freq_FB,
# 				   labels_freq_IG,
# 				   labels_freq_TW,
# 				   labels_freq_TK,
# 				   labels_freq_YT) %>%
# 		gather(key="platform", value="freq") %>%
# 		filter(!is.na(freq)) %>%
# 		mutate(freq = ifelse(grepl("(Other|Not sure)", freq), "Not sure/Other", freq)) %>%
# 		group_by(platform) %>% mutate(tot=n()) %>% ungroup() %>%
# 		group_by(platform, freq) %>% summarise(pct=n()/tot, .groups="keep") %>% distinct() %>%
# 		mutate(demog_PID_2pt = "All")
# 	,
# 	dat %>% select(labels_freq_FB,
# 				   labels_freq_IG,
# 				   labels_freq_TW,
# 				   labels_freq_TK,
# 				   labels_freq_YT,
# 				   demog_PID_2pt,
# 				   demog_PID_6pt) %>%
# 		gather(key="platform", value="freq", -demog_PID_6pt, -demog_PID_2pt) %>%
# 		filter(!is.na(freq)) %>%
# 		mutate(freq = ifelse(grepl("(Other|Not sure)", freq), "Not sure/Other", freq)) %>%
# 		group_by(platform, demog_PID_2pt) %>% mutate(tot=n()) %>% ungroup() %>%
# 		group_by(demog_PID_2pt, platform, freq) %>% summarise(pct=n()/tot, .groups="keep") %>% distinct()
# ) %>% mutate(demog_PID_2pt=factor(demog_PID_2pt, levels=c("All","Democrat","Republican","Neither"))) %>%
# 	mutate(freq = ifelse(grepl("(Other|Not sure)", freq), "Not sure/Other", freq)) %>%
# 	mutate(platform=gsub("labels_freq_","", platform)) %>%
# 	mutate(platform=gsub("FB","Facebook", platform)) %>% 
# 	mutate(platform=gsub("TW","Twitter", platform)) %>% 
# 	mutate(platform=gsub("YT","YouTube", platform)) %>% 
# 	mutate(platform=gsub("IG","Instagram", platform)) %>% 
# 	mutate(platform=gsub("TK","TikTok", platform)) %>% 
# 	mutate(freq=factor(freq, levels=freq2_levels)) %>%
# 	ggplot(aes(y=freq, x=pct, fill=demog_PID_2pt)) +
# 	geom_bar(stat="identity") +
# 	geom_text(aes(x=pct+0.1, y=freq, label=paste0(round(pct*100,1),"%"), group=demog_PID_2pt), size=2.5) +
# 	scale_fill_manual(values=c("black","blue","red","grey"), name="") + 
# 	ylab("") + xlab("% of self-reported users in group and on platform who responded") +
# 	ggtitle("Since the election how often did you encounter credibility labels on...") + 
# 	scale_x_continuous(labels = scales::percent_format(accuracy=1), limits=c(0,0.6)) +
# 	facet_grid(demog_PID_2pt ~ platform) +
# 	my_theme + theme(legend.position="none", axis.text.x = element_text(size=5))
# 
# labels_PID6pt_df <- bind_rows(tidy(mod.fb1) %>% mutate(platform = "Facebook", model = "univariate"),
# 		  tidy(mod.fb2) %>% mutate(platform = "Facebook", model = "adjusted"),
# 		  tidy(mod.ig1) %>% mutate(platform = "Instagram", model = "univariate"),
# 		  tidy(mod.ig2) %>% mutate(platform = "Instagram", model = "adjusted"),
# 		  tidy(mod.yt1) %>% mutate(platform = "YouTube", model = "univariate"),
# 		  tidy(mod.yt2) %>% mutate(platform = "YouTube", model = "adjusted"),
# 		  tidy(mod.tk1) %>% mutate(platform = "TikTok", model = "univariate"),
# 		  tidy(mod.tk2) %>% mutate(platform = "TikTok", model = "adjusted"),
# 		  tidy(mod.tw1) %>% mutate(platform = "Twitter", model = "univariate"),
# 		  tidy(mod.tw2) %>% mutate(platform = "Twitter", model = "adjusted")) %>%
# 	filter(grepl("PID", term)) %>%
# 	mutate(term = gsub("demog_PID_6pt","", term)) %>%
# 	mutate(sig=ifelse(estimate-1.96*std.error < 0 & estimate+1.96*std.error > 0, 
# 	                  "gray", "black")) %>%
# 	mutate(term = factor(term, levels = c(
# 		"Weak Democrat", "True Independent", "Weak Republican", "Strong Republican", "Everyone else"
# 	)))
# 
# labels_PID6pt_df <- labels_PID6pt_df[order(labels_PID6pt_df$p.value),]
# alpha <- 0.05
# k <- nrow(labels_PID6pt_df); r <- 1:k
# alpha.adj <- (r*alpha)/k
# labels_PID6pt_df$z.crit.adj <- qnorm(1 - (alpha.adj)/2)
# labels_PID6pt_df$sig.BH <- ifelse(labels_PID6pt_df$p.value < alpha.adj, "black", "gray")
# 
# p2 <- ggplot(labels_PID6pt_df, aes(x=term)) +
# 	## null hypothesis
# 	geom_hline(yintercept=0, lty=2, alpha=0.5) +
# 	## BHq CI
# 	geom_linerange(aes(y=estimate, 
# 					   ymin=estimate-z.crit.adj*std.error, 
# 					   ymax=estimate+z.crit.adj*std.error, 
# 					   shape=model, colour=sig.BH), 
# 				   position=position_dodge(width=.75), 
# 				   size=0.5, stroke=.5) +
# 	## 95% CI
# 	geom_pointrange(aes(y=estimate,
# 						ymin=estimate-1.96*std.error,
# 						ymax=estimate+1.96*std.error,
# 						shape=model, colour=sig.BH),
# 					position=position_dodge(width=.75),
# 					size=0.75, stroke=.5) +
# 	## 90% CI
# 	geom_linerange(aes(y=estimate,
# 					   ymin=estimate-1.65*std.error,
# 					   ymax=estimate+1.65*std.error,
# 					   shape=model, colour=sig.BH),
# 				   position=position_dodge(width=.75),
# 				   size=0.8, stroke=.5) +
# 	scale_colour_identity() + 
# 	scale_shape_manual(values=c(16,25,15)) + 
# 	facet_grid(~ platform) + ## different outcomes
# 	ylab(TeX("Marginal $(\\pm \\sigma)$ level of encounter (relative to Strong Democrat)")) + 
# 	xlab("") +
# 	coord_flip() + 
# 	my_theme + 
# 	theme(axis.text.x = element_text(size=8),
# 		  strip.text = element_text(size = 10),
# 		  # axis.title.x = element_text(size=10),
# 		  plot.margin = margin(5, 0, 7, -5),
# 		  legend.text = element_text(size = 8),
# 		  legend.title = element_text(size = 10),
# 		  legend.spacing.y = unit(1.0, 'cm'),
# 		  legend.position="bottom") + 
# 	guides(shape=guide_legend(title="model:", nrow=1))
# 
# p_p2 <- cowplot::plot_grid(p + theme(axis.text.y=element_text(size=8)), 
# 						   p2 + theme(plot.margin=unit(x=c(-3,0,0,0),units="mm"), strip.background = element_blank()), 
# 						   align="v", axis="lr", nrow = 2, rel_heights=c(2.4,1.2))
# cowplot::save_plot(p_p2, file ="figures_paper/fig1.png", base_width=8.5, base_height=5.5)
# system("open figures_paper/fig1.png")

#####------------------------------------------------------#
##### Figure 2 ####
#####------------------------------------------------------#

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
	mutate(check_amt=recode(check_amt, "Not sure"='Not\nsure',
							  		   "No content is checked"='None',
							  		   "Most content is NOT checked"='Most not\nchecked',
							  		   "About half of content is checked"='~Half\nand half',
							  		   "Most content is checked"='Most\nchecked',
							  		   "All content is checked"='All\nchecked')) %>%	
	mutate(demog_PID_2pt = factor(demog_PID_2pt, levels=rev(c("All","Democrat","Republican","Neither")))) %>%
	ggplot(aes(x=demog_PID_2pt, y=pct, fill=check_amt)) + 
	coord_flip() +
	scale_y_continuous(labels = scales::percent_format(accuracy=1), limits=c(0,1), breaks=c(0,0.25,0.5,0.75,1)) +
	# facet_grid(~ demog_PID_2pt) +
	ggtitle("How much content on your most-used social media platform is checked for accuracy?") + 
	xlab("") + ylab("% of self-reported users in group") +
	geom_bar(stat="identity", position = position_stack()) +
	geom_text(aes(x=demog_PID_2pt, y=pct, label=ifelse(!(check_amt %in% c("None","Not\nsure")), paste0(round(pct*100,0),"%"), "")), size=3.6, colour='white', fontface='bold', position = position_stack(vjust=0.5)) +
	scale_fill_brewer(palette= "YlOrRd", name="", guide = guide_legend(reverse = TRUE, nrow=1, label.theme = element_text(size=7))) +
	my_theme + 
	theme(legend.position="bottom", 
		  axis.text.x =element_text(size=8), 
		  axis.text.y=element_text(size=10), 
		  plot.title = element_text(size=11))
ggsave("figures_paper/fig2.png", width=7, height=2.75)
system("open figures_paper/fig2.png")

#####------------------------------------------------------#
##### Figure 2 (old) ####
#####------------------------------------------------------#

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
	xlab("") + ylab("% of self-reported users in group") +
	geom_bar(aes(fill=demog_PID_2pt), stat="identity") + 
	geom_text(aes(x=check_amt, y=pct+0.08, label=paste0(round(pct*100,1),"%")), size=3) +
	scale_fill_manual(values=c("black","blue","red","gray")) + 
	my_theme + theme(legend.position="none", axis.text.x =element_text(size=5), axis.text.y=element_text(size=7), plot.title = element_text(size=10))
ggsave("figures_paper/fig2.png", width=7, height=1.75)
system("open figures_paper/fig2.png")

#####------------------------------------------------------#
##### Figure 3 ####
#####------------------------------------------------------#

h1.1.viz.idx6pt <- bind_rows(
	h1.1.df.idx6pt %>% mutate(model="univariate"), 
	h1.1.df.idx6pt.adj %>% mutate(model="adjusted")
	) %>%
	filter(grepl("PID", term)) %>%
	mutate(term = gsub("demog_PID_6pt","", term)) %>%
	mutate(sig=ifelse(estimate-1.96*std.error < 0 & estimate+1.96*std.error > 0, 
	                  "gray", "black")) %>%
	mutate(term = factor(term, levels = c(
		"Weak Democrat", "True Independent", "Weak Republican", "Strong Republican", "Everyone else"
	)))

h1.1.viz.idx6pt <- h1.1.viz.idx6pt[order(h1.1.viz.idx6pt$p.value),]
alpha <- 0.05
k <- nrow(h1.1.viz.idx6pt); r <- 1:k
alpha.adj <- (r*alpha)/k
h1.1.viz.idx6pt$sig.BH <- ifelse(h1.1.viz.idx6pt$p.value < alpha.adj, "black", "gray")
h1.1.viz.idx6pt$z.crit.adj <- qnorm(1 - (alpha.adj)/2)

p <- ggplot(h1.1.viz.idx6pt, aes(x=term)) +
	## null hypothesis
	geom_hline(yintercept=0, lty=2, alpha=0.5) +
	## BHq CI
	geom_linerange(aes(y=estimate, 
					   ymin=estimate-z.crit.adj*std.error, 
					   ymax=estimate+z.crit.adj*std.error, 
					   shape=model, colour=sig.BH), 
				   position=position_dodge(width=.65), 
				   size=0.5, stroke=.5) +
	## 95% CI
	geom_pointrange(aes(y=estimate,
						ymin=estimate-1.96*std.error,
						ymax=estimate+1.96*std.error,
						shape=model, colour=sig.BH),
					position=position_dodge(width=.65),
					size=0.75, stroke=.5) +
	## 90% CI
	geom_linerange(aes(y=estimate,
					   ymin=estimate-1.65*std.error,
					   ymax=estimate+1.65*std.error,
					   shape=model, colour=sig.BH),
				   position=position_dodge(width=.65),
				   size=1, stroke=.5) +
	scale_colour_identity() + 
	scale_shape_manual(values=c(16,25,15)) + 
	# facet_grid(~ group) + ## different outcomes
	ylab(TeX("Marginal Support $(\\pm \\sigma)$ for Interventions (Relative to Strong Democrat)")) + 
	xlab("") +
	coord_flip() + 
	theme_bw() +
	theme(axis.text.x = element_text(size=8),
		  strip.text = element_text(size = 15),
		  # axis.title.x = element_text(size=10),
		  plot.margin = margin(5, 0, 7, -5),
		  legend.text = element_text(size = 8),
		  legend.title = element_text(size = 10),
		  legend.spacing.y = unit(1.0, 'cm'),
		  legend.position="bottom") + 
	guides(shape=guide_legend(title="model:", nrow=1))

p2 <- dat %>%
	group_by(demog_PID_6pt) %>%
	summarise(s_mean=mean(support_idx0,na.rm=T),
			  s_se=sd(support_idx0,na.rm=T)/sqrt(n())) %>%
	ggplot(aes(x=demog_PID_6pt, y=s_mean, ymin=s_mean-1.96*s_se, ymax=s_mean+1.96*s_se)) +
	geom_bar(stat="identity") + 
	geom_linerange() + 
	xlab("") + ylab("Support for interventions [0-1]") + 
	coord_flip() + my_theme

pp <- cowplot::plot_grid(p2, 
				   p + ylab(TeX("Marginal $(\\pm \\sigma)$ support")), 
				   ncol=2, align="hv")
cowplot::save_plot(pp, file ="figures_paper/fig3.png", base_width=7, base_height=2.25)
system("open figures_paper/fig3.png")


#####------------------------------------------------------#
##### Figure 4 ####
#####------------------------------------------------------#

p <- bind_rows(
	dat %>% 
		select(labels_approve, remove_approve, downrank_approve, demog_PID_2pt) %>%
		gather(key="outcome", value="support", -demog_PID_2pt) %>%
		filter(!is.na(support)) %>%
		group_by(outcome, demog_PID_2pt) %>% mutate(tot=n()) %>% ungroup() %>%
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
) %>%  mutate(demog_PID_2pt=factor(demog_PID_2pt, levels=rev(c("All","Democrat","Republican","Neither")))) %>%
	mutate(support=factor(support)) %>%
	mutate(outcome=gsub("downrank_approve", "Downranking", outcome)) %>%
	mutate(outcome=gsub("labels_approve", "Labels", outcome)) %>%
	mutate(outcome=gsub("remove_approve", "Removal", outcome)) %>%
	mutate(support = recode(support, '1'='Strongly\ndisapprove',
							         '2'='Somewhat\ndisapprove',
							         '3'='Neutral',
							         '4'='Somewhat\napprove',
							         '5'='Strongly\napprove')) %>%
	ggplot(aes(y=demog_PID_2pt, x=pct, fill=support)) + 
	scale_x_continuous(labels = scales::percent_format(accuracy=1), limits=c(0, 1)) +
	scale_fill_brewer(name='', palette = "Spectral", guide = guide_legend(reverse = TRUE, nrow=1)) +
	scale_colour_manual(name='', values=rev(c('lightblue','lightgreen','white','orange','#FFD9D9')), guide = FALSE) +
	xlab("% of self-reported users in group") + ylab("") + 
	geom_bar(stat="identity", position = position_stack()) +
	geom_text(aes(x=pct, y=demog_PID_2pt, colour=support, label=ifelse(support %in% c('Strongly\napprove', 'Strongly\ndisapprove'),paste0(round(pct*100,0),"%"),'')), size=3.6, fontface='bold', position = position_stack(vjust=0.5)) +
	ggtitle("Support for specific interventions") + 
	facet_grid(. ~ outcome) +
	my_theme + 
	theme(axis.text.x = element_text(size=7),
		  strip.text = element_text(size=12),
		  legend.position = "bottom", 
		  legend.key.size = unit(0.5, 'cm'), #change legend key size
          legend.key.height = unit(0.5, 'cm'), #change legend key height
          # legend.key.width = unit(0.5, 'cm'), #change legend key width
          legend.title = element_text(size=6), #change legend title font size
          legend.text = element_text(size=8))

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
	theme(axis.text.x = element_text(size=9),
		  axis.title.x = element_text(size=10),
		  plot.margin = margin(5, 0, 7, -5),
		  legend.text = element_text(size = 8),
		  legend.title = element_text(size = 8),
		  legend.spacing.y = unit(1.0, 'cm'),
		  legend.position="bottom") + 
	guides(shape=guide_legend(title="model:", nrow=1))

p_p2 <- cowplot::plot_grid(p + theme(axis.text.y=element_text(size=8)), 
						   p2 + theme(plot.margin=unit(x=c(-3,0,0,0),units="mm"), strip.background = element_blank()), 
						   align="v", axis="lr", nrow = 2, rel_heights=c(1.2,1.2))
cowplot::save_plot(p_p2, file ="figures_paper/fig4.png", base_width=8.5, base_height=5.5)
system("open figures_paper/fig4.png")

#####------------------------------------------------------#
##### Figure 5 ####
#####------------------------------------------------------#

dat %>% 
	select(demog_PID_6pt, labels_eval_human) %>%
	separate_rows(labels_eval_human, sep=",") %>%
	filter(!is.na(labels_eval_human)) %>%
	filter(!grepl("Other", labels_eval_human)) %>%
	group_by(demog_PID_6pt) %>% mutate(tot=n()) %>%
	group_by(demog_PID_6pt, labels_eval_human) %>%
	summarise(pct=n()/tot) %>% distinct() %>%
	# filter(!grepl("Other", labels_eval_human)) %>%
	mutate(labels_eval_human= gsub("Due to ", "", labels_eval_human)) %>% as.data.frame() %>%
	mutate(labels_eval_human = as_factor(labels_eval_human)) %>%
	group_by(demog_PID_6pt) %>% arrange(labels_eval_human) %>%
	# mutate(labels_eval_human= gsub(" errors", "\nerrors\n", labels_eval_human)) %>%
	ggplot(aes(x=demog_PID_6pt, y=pct, fill=labels_eval_human)) + 
	scale_y_continuous(labels = scales::percent_format(accuracy=1), limits=c(0,1)) +
	ylab("% of self-reported users in group who reported encountered inappropriate labels") + xlab("Perceived errors") + 
	geom_bar(aes(fill=labels_eval_human), stat="identity") +
	geom_text(aes(y=pct, x=demog_PID_6pt, label=paste0(round(pct*100,1),"%")), size=3.6, position = position_stack(vjust = 0.5), color="white", fontface="bold") +
	ggtitle("Types of perceived errors in content labels") + 
	# scale_fill_manual(values=c("blue","lightblue","grey","red","darkred","black")) + 
	scale_fill_brewer(palette="Dark2", name="Percieved source of error\nin content label:", 
					  labels=rev(c('Unintentional\nhuman errors','Machine/\ncomputer errors','Biased\nhuman judgment')), guide = guide_legend(reverse = T)) + 
	# facet_wrap(~ demog_PID_6pt) + 
	coord_flip() + 
	my_theme +  
	theme(legend.position="bottom", axis.text.x = element_text(size=7), strip.text.x = element_text(size=12))
ggsave("figures_paper/fig5.png", width=8, height=3)
system("open figures_paper/fig5.png")

#####------------------------------------------------------#
##### Figure 6 ####
#####------------------------------------------------------#

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
	arrange(desc(inst)) %>%
	mutate(inst = as_factor(inst)) %>%
	ggplot(aes(x=inst, y=m, ymin=m-1.96*sd/sqrt(n), ymax=m+1.96*sd/sqrt(n))) +
	geom_segment(aes(colour=demog_PID_2pt, x=inst, xend=inst, y=2, yend=m), stat="identity", size=4.25) + 
	geom_linerange(color="black", lwd=1) + 
	scale_colour_manual(values=c("lightgray", "blue", "red", "darkgray")) + 
	scale_x_discrete(limits=rev) +
	scale_y_continuous(limits=c(2,4), labels=c("2","2.5","3","3.5","4")) +
	xlab("") + ylab("Approval level (1-5)") +
	facet_grid(~ demog_PID_2pt) +
	coord_flip() + 
	my_theme +
	theme(legend.position = "none", strip.text = element_text(size=12))
ggsave("figures_paper/fig6.png", width=8, height=2)
system("open figures_paper/fig6.png")

#####------------------------------------------------------#
##### Figure 7 ####
#####------------------------------------------------------#

h2.1.viz <- bind_rows(
h2.1.df.idxinst %>% mutate(outcome="Support for\ninterventions", model="univariate", lab=" "),
h2.1.df.idxinst.adj %>% mutate(outcome="Support for\ninterventions", model="adjusted", lab=""),
h2.1.df.idxinst.adj.D %>% mutate(outcome="Support for\ninterventions", model="adjusted", lab="D only"),
h2.1.df.idxinst.adj.R %>% mutate(outcome="Support for\ninterventions", model="adjusted", lab="R only"),

h2.2.df.usersinst %>% mutate(outcome="Support for\nuser-decided\ninterventions", model="univariate", lab=" "),
h2.2.df.usersinst.adj %>% mutate(outcome="Support for\nuser-decided\ninterventions", model="adjusted", lab=""),
h2.2.df.usersinst.adj.D %>% mutate(outcome="Support for\nuser-decided\ninterventions", model="adjusted", lab="D only"),
h2.2.df.usersinst.adj.R %>% mutate(outcome="Support for\nuser-decided\ninterventions", model="adjusted", lab="R only"),

h2.3.df.algosinst %>% mutate(outcome="Support for\nalgorithm-decided\ninterventions", model="univariate", lab=" "),
h2.3.df.algosinst.adj %>% mutate(outcome="Support for\nalgorithm-decided\ninterventions", model="adjusted", lab=""),
h2.3.df.algosinst.adj.D %>% mutate(outcome="Support for\nalgorithm-decided\ninterventions", model="adjusted", lab="D only"),
h2.3.df.algosinst.adj.R %>% mutate(outcome="Support for\nalgorithm-decided\ninterventions", model="adjusted", lab="R only")
)

h2.1.viz <- h2.1.viz[order(h2.1.viz$p.value),]
alpha <- 0.05
k <- nrow(h2.1.viz); r <- 1:k
alpha.adj <- (r*alpha)/k
h2.1.viz$z.crit.adj <- qnorm(1 - (alpha.adj)/2)
h2.1.viz$sig.BH <- ifelse(h2.1.viz$p.value < alpha.adj, "black", "gray")
h2.1.viz$sig.BH <- ifelse(h2.1.viz$sig.BH == "black" & h2.1.viz$lab == "D only", "blue", h2.1.viz$sig.BH)
h2.1.viz$sig.BH <- ifelse(h2.1.viz$sig.BH == "black" & h2.1.viz$lab == "R only", "red", h2.1.viz$sig.BH)

h2.1.viz %>% 
	filter(grepl("inst_trust", term)) %>%
	mutate(term = "") %>%
	mutate(outcome = factor(outcome, levels=c("Support for\ninterventions",
											  "Support for\nuser-decided\ninterventions",
											  "Support for\nalgorithm-decided\ninterventions"))) %>%
	ggplot(aes(x=term, group=lab)) +
	## null hypothesis
	geom_hline(yintercept=0, lty=2, alpha=0.5) +
	## BHq CI
	geom_linerange(aes(y=estimate, 
					   ymin=estimate-z.crit.adj*std.error, 
					   ymax=estimate+z.crit.adj*std.error, 
					   shape=model, colour=sig.BH), 
				   position=position_dodge(width=.45), 
				   size=0.5, stroke=.5) +
	## 95% CI
	geom_pointrange(aes(y=estimate,
						ymin=estimate-1.96*std.error,
						ymax=estimate+1.96*std.error,
						shape=model, colour=sig.BH),
					position=position_dodge(width=.45),
					size=1, stroke=.5) +
	## 90% CI
	geom_linerange(aes(y=estimate,
					   ymin=estimate-1.65*std.error,
					   ymax=estimate+1.65*std.error,
					   shape=model, colour=sig.BH),
				   position=position_dodge(width=.45),
				   size=1.5, stroke=.5) +
	scale_colour_identity() + 
	scale_shape_manual(values=c(16,25,15)) + 
	facet_grid(~ outcome) + ## different outcomes
	ylab(TeX("Marginal effect of trust in institutions on $(\\pm \\sigma)$ support")) + 
	geom_text(aes(x=term, 
				  y=estimate-z.crit.adj*std.error,
				  color=sig.BH,
				  label=lab), 
			  position=position_dodge(width=.45), hjust=1.1, size=2) + 
	xlab("") +
	coord_flip() + 
	my_theme + 
	theme(axis.text.x = element_text(size=8),
		  strip.text = element_text(size = 12),
		  axis.ticks.y = element_blank(),
		  # axis.title.x = element_text(size=10),
		  # plot.margin = margin(5, 0, 7, -5),
		  legend.text = element_text(size = 8),
		  legend.title = element_text(size = 10),
		  legend.spacing.y = unit(1.0, 'cm'),
		  legend.position="bottom") + 
	guides(shape=guide_legend(title="model:", nrow=1))
ggsave("figures_paper/fig7.png", width=7, height=3)
system("open figures_paper/fig7.png")

#####------------------------------------------------------#
##### Figure 8 ####
#####------------------------------------------------------#

h4.1.viz <- bind_rows(
	h4.1.df.moder_good %>% mutate(model="univariate", lab=" "),
	h4.1.df.moder_good.adj %>% mutate(model="adjusted", lab=""),
	h4.1.df.moder_good.adj.D %>% mutate(model="adjusted", lab="D only"),
	h4.1.df.moder_good.adj.R %>% mutate(model="adjusted", lab="R only"),
	
	h4.1.df.moder_bad %>% mutate(model="univariate", lab=" "),
	# h4.1.df.moder_bad.adj %>% mutate(model="adjusted"),
	
	h4.2.df.moder_you %>% mutate(model="univariate", lab=" "),
	# h4.2.df.moder_you.adj %>% mutate(model="adjusted"),
	h4.3.df.moder_others %>% mutate(model="univariate", lab=" "),
	# h4.3.df.moder_others.adj %>% mutate(model="adjusted"),
	
	h4.4.df.usage_n %>% mutate(model="univariate", lab=" "),
	h4.4.df.usage_n.adj %>% mutate(model="adjusted", lab=""),
	h4.4.df.usage_n.adj.D %>% mutate(model="adjusted", lab="D only"),
	h4.4.df.usage_n.adj.R %>% mutate(model="adjusted", lab="R only"),
	
	h4.5.df.usage_avg %>% mutate(model="univariate"),
	h4.5.df.usage_avg.adj %>% mutate(model="adjusted", lab=""),
	h4.5.df.usage_avg.adj.D %>% mutate(model="adjusted", lab="D only"),
	h4.5.df.usage_avg.adj.R %>% mutate(model="adjusted", lab="R only"),
) %>% 
	filter(grepl("moder|usage", term)) %>%
	mutate(term=gsub("moder_bad", "Seen 'inappropriate'\ncontent intervention", term)) %>%
	mutate(term=gsub("moder_good", "Seen 'appropriate'\ncontent intervention", term)) %>%
	mutate(term=gsub("moder_you", "Received\ncontent intervention", term)) %>%
	mutate(term=gsub("moder_others", "Used\ncontent intervention", term)) %>%
	mutate(term=gsub("usage_n", "Platform usage:\nnumber", term)) %>%
	mutate(term=gsub("usage_avg", "Platform usage:\naverage frequency", term)) %>%
	mutate(term=factor(term,
					   levels = c(
							"Seen 'inappropriate'\ncontent intervention",
							"Seen 'appropriate'\ncontent intervention",
							"Received\ncontent intervention",
							"Used\ncontent intervention",
							"Platform usage:\nnumber",
							"Platform usage:\naverage frequency"
					   ))) %>%
	mutate(sig=ifelse(estimate-1.96*std.error < 0 & estimate+1.96*std.error > 0, 
	                  "gray", "black"))

h4.1.viz <- h4.1.viz[order(h4.1.viz$p.value),]
alpha <- 0.05
k <- nrow(h4.1.viz); r <- 1:k
alpha.adj <- (r*alpha)/k
h4.1.viz$z.crit.adj <- qnorm(1 - (alpha.adj)/2)
h4.1.viz$sig.BH <- ifelse(h4.1.viz$p.value < alpha.adj, "black", "gray")
h4.1.viz$sig.BH <- ifelse(h4.1.viz$sig.BH == "black" & h4.1.viz$lab == "D only", "blue", h4.1.viz$sig.BH)
h4.1.viz$sig.BH <- ifelse(h4.1.viz$sig.BH == "black" & h4.1.viz$lab == "R only", "red", h4.1.viz$sig.BH)

p <- ggplot(h4.1.viz, aes(x=term, group=lab)) +
	## null hypothesis
	geom_hline(yintercept=0, lty=2, alpha=0.5) +
	## BHq CI
	geom_linerange(aes(y=estimate, 
					   ymin=estimate-z.crit.adj*std.error, 
					   ymax=estimate+z.crit.adj*std.error, 
					   shape=model, colour=sig.BH), 
				   position=position_dodge(width=.45), 
				   size=0.5, stroke=.5) +
	## 95% CI
	geom_pointrange(aes(y=estimate,
						ymin=estimate-1.96*std.error,
						ymax=estimate+1.96*std.error,
						shape=model, colour=sig.BH),
					position=position_dodge(width=.45),
					size=1, stroke=.5) +
	## 90% CI
	geom_linerange(aes(y=estimate,
					   ymin=estimate-1.65*std.error,
					   ymax=estimate+1.65*std.error,
					   shape=model, colour=sig.BH),
				   position=position_dodge(width=.45),
				   size=1.25, stroke=.5) +
	scale_colour_identity() + 
	scale_shape_manual(values=c(16,25,15)) + 
	geom_vline(xintercept = 2.5, lty = 3) +
	geom_vline(xintercept = 4.5, lty = 3) + 
	# facet_grid(~ outcome) + ## different outcomes
	ylab(TeX("Marginal $(\\pm \\sigma)$ support for interventions")) + 
	geom_text(aes(x=term, 
				  y=estimate-z.crit.adj*std.error,
				  color=sig.BH,
				  label=lab), 
			  position=position_dodge(width=.45), hjust=1.1, size=2) + 
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
ggsave("figures_paper/fig8.png", width=7, height=4.5)
system("open figures_paper/fig8.png")