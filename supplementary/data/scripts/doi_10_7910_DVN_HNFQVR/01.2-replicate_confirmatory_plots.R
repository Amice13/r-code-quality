# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Replicate confirmatory plots for paper.
# 
# Author: Soubhik Barari
# 
# Environment:
# - must use R 3.6
# 
# Input:
# - PAI-Misinfo-Feb2021_confirm.RData
#
# Output:
# - figures_confirm/*
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

rm(list=ls())

require(ggplot2)
require(broom)
require(latex2exp)
require(tidyverse)

load("PAI-Misinfo-Feb2021_confirm.RData")

agree_levels <- c("Strongly disagree","Somewhat disagree","Neither agree nor disagree","Somewhat agree","Strongly agree")
approve_levels <- c("Strongly disapprove","Somewhat disapprove","Neither approve nor disapprove","Somewhat approve","Strongly approve")

my_theme <- theme_linedraw() + 
	theme(panel.grid.major.x = element_blank(), 
		  panel.grid.major.y = element_blank())


#####------------------------------------------------------#
##### RQ1 (Partisanship) ####
#####------------------------------------------------------#

## H1.1: Stronger Republican self-identification predicts decreased
##       support for veracity labels and removal interventions.

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

ggsave(p, "figures_confirm/RQ1_pid6pt_support.png", width=7, height=3)
system("open figures_confirm/RQ1_pid6pt_support.png")

pp <- cowplot::plot_grid(p2, 
				   p + ylab(TeX("Marginal $(\\pm \\sigma)$ support")), 
				   ncol=2, align="hv")
cowplot::save_plot(pp, file ="figures_confirm/RQ1_pid6pt_support2.png", base_width=7, base_height=2.25)
system("open figures_confirm/RQ1_pid6pt_support2.png")


h1.1.viz.idx2pt <- bind_rows(
	h1.1.df.idx2pt %>% mutate(model="univariate"), 
	h1.1.df.idx2pt.adj %>% mutate(model="adjusted")
	) %>%
	filter(grepl("PID", term)) %>%
	mutate(term = gsub("demog_PID_2pt","", term)) %>%
	mutate(sig=ifelse(estimate-1.96*std.error < 0 & estimate+1.96*std.error > 0, 
	                  "gray", "black")) %>%
	mutate(term = factor(term, levels = c(
		"Neither", "Republican"
	)))

h1.1.viz.idx2pt <- h1.1.viz.idx2pt[order(h1.1.viz.idx2pt$p.value),]
alpha <- 0.05
k <- nrow(h1.1.viz.idx2pt); r <- 1:k
alpha.adj <- (r*alpha)/k
h1.1.viz.idx2pt$sig.BH <- ifelse(h1.1.viz.idx2pt$p.value < alpha.adj, "black", "gray")
h1.1.viz.idx2pt$z.crit.adj <- qnorm(1 - (alpha.adj)/2)

p <- ggplot(h1.1.viz.idx2pt, aes(x=term)) +
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
	# facet_grid(~ group) + ## different outcomes
	ylab(TeX("Marginal Support $(\\pm \\sigma)$ for Interventions (Relative to Democrat)")) + 
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
ggsave("figures_confirm/RQ1_pid2pt_support.png", width=7, height=3)
system("open figures_confirm/RQ1_pid2pt_support.png")

# H1.2: Stronger Republican self-identification predicts belief 
#       that interventions are biased by ideological lean of content.

h1.2.viz.bias2pt <- bind_rows(
	h1.2.df.bias2pt1 %>% mutate(model="univariate", outcome="Social media interventions\nideologically targeted\n"),
	h1.2.df.bias2pt1.adj %>% mutate(model="adjusted", outcome="Social media interventions\nideologically targeted\n"),
	h1.2.df.bias2pt2 %>% mutate(model="univariate", outcome="Social media interventions\nideologically targeted\nagainst conservatives"),
	h1.2.df.bias2pt2.adj %>% mutate(model="adjusted", outcome="Social media interventions\nideologically targeted\nagainst conservatives"),
	h1.2.df.bias2pt3 %>% mutate(model="univariate", outcome="Social media interventions\nideologically targeted\nagainst liberals"),
	h1.2.df.bias2pt3.adj %>% mutate(model="adjusted", outcome="Social media interventions\nideologically targeted\nagainst liberals")
)

h1.2.viz.bias2pt <- h1.2.viz.bias2pt %>%
	filter(grepl("PID", term)) %>%
	mutate(term = gsub("demog_PID_2pt","", term)) %>%
	mutate(sig=ifelse(estimate-1.96*std.error < 0 & estimate+1.96*std.error > 0, 
	                  "gray", "black")) %>%
	mutate(term = factor(term, levels = c(
		"Neither", "Republican"
	)))

h1.2.viz.bias2pt <- h1.2.viz.bias2pt[order(h1.2.viz.bias2pt$p.value),]
alpha <- 0.05
k <- nrow(h1.2.viz.bias2pt); r <- 1:k
alpha.adj <- (r*alpha)/k
h1.2.viz.bias2pt$z.crit.adj <- qnorm(1 - (alpha.adj)/2)
h1.2.viz.bias2pt$sig.BH <- ifelse(h1.2.viz.bias2pt$p.value < alpha.adj, "black", "gray")

p <- ggplot(h1.2.viz.bias2pt, aes(x=term)) +
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
	ylab(TeX("Marginal Agreement $(\\pm \\sigma)$ (Relative to Democrat)")) + 
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
ggsave("figures_confirm/RQ1_pid2pt_bias.png", width=7, height=3)
system("open figures_confirm/RQ1_pid2pt_bias.png")

h1.2.viz.bias6pt <- bind_rows(
	# h1.2.df.bias6pt1 %>% mutate(model="univariate", outcome="Social media interventions\nideologically targeted\n"),
	# h1.2.df.bias6pt1.adj %>% mutate(model="adjusted", outcome="Social media interventions\nideologically targeted\n"),
	h1.2.df.bias6pt2 %>% mutate(model="univariate", outcome="Social media interventions\nideologically targeted\nagainst conservatives"),
	h1.2.df.bias6pt2.adj %>% mutate(model="adjusted", outcome="Social media interventions\nideologically targeted\nagainst conservatives"),
	h1.2.df.bias6pt3 %>% mutate(model="univariate", outcome="Social media interventions\nideologically targeted\nagainst liberals"),
	h1.2.df.bias6pt3.adj %>% mutate(model="adjusted", outcome="Social media interventions\nideologically targeted\nagainst liberals")
)
h1.2.viz.bias6pt <- h1.2.viz.bias6pt %>%
	filter(grepl("PID", term)) %>%
	mutate(term = gsub("demog_PID_6pt","", term)) %>%
	mutate(sig=ifelse(estimate-1.96*std.error < 0 & estimate+1.96*std.error > 0, 
	                  "gray", "black")) %>%
	mutate(term = factor(term, levels = c(
		"Weak Democrat", "True Independent", "Weak Republican", "Strong Republican", "Everyone else"
	)))

h1.2.viz.bias6pt <- h1.2.viz.bias6pt[order(h1.2.viz.bias6pt$p.value),]
alpha <- 0.05
k <- nrow(h1.2.viz.bias6pt); r <- 1:k
alpha.adj <- (r*alpha)/k
h1.2.viz.bias6pt$z.crit.adj <- qnorm(1 - (alpha.adj)/2)
h1.2.viz.bias6pt$sig.BH <- ifelse(h1.2.viz.bias6pt$p.value < alpha.adj, "black", "gray")

p <- ggplot(h1.2.viz.bias6pt, aes(x=term)) +
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
	facet_grid(~ outcome) + ## different outcomes
	ylab(TeX("Marginal $(\\pm \\sigma)$ agreement (relative to Strong Democrat)")) + 
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
ggsave("figures_confirm/RQ1_pid6pt_bias.png", width=7, height=2.5)
system("open figures_confirm/RQ1_pid6pt_bias.png")

#####------------------------------------------------------#
##### RQ2 (Institutional Trust) ####
#####------------------------------------------------------#

## H2.1:	Trust in American institutions (e.g. government, social media companies, academia) 
##          predicts increased support for interventions.

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
	ylab(TeX("Marginal $(\\pm \\sigma)$ support")) + 
	geom_text(aes(x=term, 
				  y=estimate-z.crit.adj*std.error,
				  color=sig.BH,
				  label=lab), 
			  position=position_dodge(width=.45), hjust=1.1, size=2) + 
	xlab("Trust in institutions") +
	coord_flip() + 
	my_theme + 
	theme(axis.text.x = element_text(size=8),
		  strip.text = element_text(size = 10),
		  axis.ticks.y = element_blank(),
		  # axis.title.x = element_text(size=10),
		  # plot.margin = margin(5, 0, 7, -5),
		  legend.text = element_text(size = 8),
		  legend.title = element_text(size = 10),
		  legend.spacing.y = unit(1.0, 'cm'),
		  legend.position="bottom") + 
	guides(shape=guide_legend(title="model:", nrow=1))
ggsave("figures_confirm/RQ2_insttrust.png", width=7, height=3)
system("open figures_confirm/RQ2_insttrust.png")


#####------------------------------------------------------#
##### RQ3 (Media Preference) ####
#####------------------------------------------------------#

h3.1.viz.idxcons <- bind_rows(
	h3.1.df.idxcons %>% mutate(model="univariate"), 
	h3.1.df.idxcons.adj %>% mutate(model="adjusted"),
	h3.1.df.idxcons2 %>% mutate(model="univariate"), 
	h3.1.df.idxcons.adj2 %>% mutate(model="adjusted")
	) %>%
	filter(grepl("slant", term)) %>%
	mutate(term = ifelse(term == "I(news_slants_avg > 0)TRUE", "Conservative news\npreference\n(0/1)", term)) %>%
	mutate(term = gsub("news_slants_avg", "Conservative\nnews preference\n(magnitude)", term)) %>%
	mutate(sig=ifelse(estimate-1.96*std.error < 0 & estimate+1.96*std.error > 0, 
	                  "gray", "black"))

h3.1.viz.idxcons <- h3.1.viz.idxcons[order(h3.1.viz.idxcons$p.value),]
alpha <- 0.05
k <- nrow(h3.1.viz.idxcons); r <- 1:k
alpha.adj <- (r*alpha)/k
h3.1.viz.idxcons$z.crit.adj <- qnorm(1 - (alpha.adj)/2)
h3.1.viz.idxcons$sig.BH <- ifelse(h3.1.viz.idxcons$p.value < alpha.adj, "black", "gray")

p <- ggplot(h3.1.viz.idxcons, aes(x=term)) +
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
	# facet_grid(~ group) + ## different outcomes
	ylab(TeX("Marginal support $(\\pm \\sigma)$ for interventions")) + 
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
ggsave("figures_confirm/RQ3_pref_support.png", width=7, height=3)
system("open figures_confirm/RQ3_pref_support.png")

h3.2.viz <- bind_rows(
	h3.2.df.idxnews %>% mutate(model="univariate"),
	h3.2.df.idxnews.adj %>% mutate(model="adjusted"),
	h3.2.df.idxnews2 %>% mutate(model="univariate"),
	h3.2.df.idxnews2.adj %>% mutate(model="adjusted")
) %>% filter(grepl("follow_news|news_freq", term)) %>%
	mutate(term = gsub("follow_news", "Frequency of\nfollowing news", term)) %>%
	mutate(term = gsub("news_freq", "Average frequency\nof news-seeking\nacross news modes", term)) %>%
	mutate(sig=ifelse(estimate-1.96*std.error < 0 & estimate+1.96*std.error > 0, 
	                  "gray", "black"))

h3.2.viz <- h3.2.viz[order(h3.2.viz$p.value),]
alpha <- 0.05
k <- nrow(h3.2.viz); r <- 1:k
alpha.adj <- (r*alpha)/k
h3.2.viz$z.crit.adj <- qnorm(1 - (alpha.adj)/2)
h3.2.viz$sig.BH <- ifelse(h3.2.viz$p.value < alpha.adj, "black", "gray")

p <- ggplot(h3.2.viz, aes(x=term)) +
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
	# facet_grid(~ group) + ## different outcomes
	ylab(TeX("Marginal ambivalence $(\\pm \\sigma)$ about interventions")) + 
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
ggsave("figures_confirm/RQ3_ambiv.png", width=7, height=3)
system("open figures_confirm/RQ3_ambiv.png")

#####------------------------------------------------------#
##### RQ4 (Experience) ####
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
ggsave("figures_confirm/RQ4_exp_support.png", width=7, height=4.5)
system("open figures_confirm/RQ4_exp_support.png")


