
#############
# Code to reproduce figures and estimates reported in 
# "Great Expectations: The Democratic Advantage in Trade Attitudes" 
# by Frederick R. Chen, Jon C.W. Pevehouse, and Ryan M. Powers 
# forthcoming in World Politics
#############


# session information for reported results
#R version 4.1.2 (2021-11-01)
#Platform: aarch64-apple-darwin20 (64-bit)
#Running under: macOS Monterey 12.5.1
#
#Matrix products: default
#LAPACK: /Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/lib/libRlapack.dylib
#
#locale:
#  [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
#
#attached base packages:
#  [1] stats     graphics  grDevices utils     datasets  methods   base     
#
#other attached packages:
#  [1] sandwich_3.0-1     stargazer_5.2.2    reporttools_1.1.3  xtable_1.8-4       wesanderson_0.3.6  CGPfunctions_0.6.3 slopegraph_0.1.14  devtools_2.4.3    
# [9] usethis_2.1.5      estimatr_0.30.6    emmeans_1.7.2      ggrepel_0.9.1      foreign_0.8-81     readstata13_0.10.0 forcats_0.5.1      stringr_1.4.0     
# [17] dplyr_1.0.10       purrr_0.3.4        readr_2.1.2        tidyr_1.2.0        tibble_3.1.8       tidyverse_1.3.1    ggplot2_3.3.6     
#


rm(list=ls())
# Load libraries
library(ggplot2)
library(tidyverse)
library(readstata13)
library(dplyr)
library(foreign)
library(ggrepel)
library(emmeans)
library(estimatr)
library(devtools)
#install_github("leeper/slopegraph")
library(slopegraph)
library(CGPfunctions)

###
# Figure 1
###

plot_theme = theme(text = element_text(size=10),  
                   axis.text = element_text(color="black"),
                   axis.line = element_line(colour = "black"),
                   strip.text.y = element_text(angle = 0), axis.title.y = element_text(angle=0, hjust=.5, vjust=.5),
                   axis.title = element_text(lineheight=1.1), 
                   legend.key = element_rect(fill = "transparent", colour = "white")
) 

pew_formatted <- read.csv("data/pew_formatted.csv")
pew_formatted$ally <- recode(as.character(pew_formatted$ally), "1" = "Alliance/Defense Pact", "0" = "No Alliance/Defense Pact")


figure1 <- ggplot(pew_formatted, aes(x=FHt, y=trade_support)) + 
  geom_smooth(se = FALSE, method = "gam", formula = y ~ s(x), color="#DDDDDD", size=1) + 
  geom_point(aes(color=as.factor(ally), size=NY.GDP.PCAP.KD)) +
  scale_size_continuous(name="GDP per capita\n(constant 2010 USD)") +
  scale_color_manual(values=c("#000000", "#DA291C"), name="Alliance status") +
  geom_label_repel(aes(label = plot_name),
                   box.padding   = 0.3, 
                   point.padding = 0.2,
                   size=4,
                   min.segment.length = .15,
                   segment.color = 'black') +  theme_classic() + 
  plot_theme + 
  labs(x = "Freedom House Democracy Score", y="% increased trade\nwith X would be\ngood for U.S.") + 
  theme(text = element_text(size=12)) 
figure1
ggsave("figure1.png", figure1, width=9, height=5.5)



# plot theme
plot_theme = theme(text = element_text(size=10),  
                   panel.grid.major = element_line(colour = "#EEEEEE", size=.66),
                   panel.grid.major.x = element_blank(),
                   axis.text = element_text(color="black"),
                   axis.line = element_line(colour = "black"),
                   strip.text.y = element_text(angle = 0), axis.title.y = element_text(angle=0, hjust=.5, vjust=.5),
                   axis.title = element_text(lineheight=1.1), 
                   strip.background=element_rect(colour=NA, fill="#EEEEEE")
) 


####
# FIGURE 2
####


###
# elites
###

elites_df <- read.dta13("data/study1_elites.dta")

elites_bin_dv <-lm(bin_support_trade ~ democracy+high_income+small_econ+small_mil+ally, data=elites_df)
elites_binary_results <- as.data.frame(coef(summary(elites_bin_dv)))
elites_binary_results <- cbind(elites_binary_results, confint(elites_bin_dv))
elites_binary_results <- elites_binary_results[-1,]
elites_binary_results$DV <- "Binary"
elites_binary_results$sample <- "Trade/Development\nPolicy Elites"
elites_binary_results$Name <-  c("Democracy", "High wages", "Small economy", "Small military", "Ally")



##
# public (lucid)
##
lucid_df <- read.dta13("data/study1_lucid.dta")
#limiting to completes
lucid_bin_dv<-lm(bin_support ~ democracy+sim_culture+high_income+small_econ+ally, data=na.omit(lucid_df[c("support_trade_cont","bin_support", "democracy", "sim_culture", "high_income", "small_econ", "ally", "hhi", "Female", "age", "partyID", "bin_avg_inst", "bin_avg_sec", "bin_avg_econ", "education")])
)
lucid_binary_results <- as.data.frame(coef(summary(lucid_bin_dv)))
lucid_binary_results <- cbind(lucid_binary_results, confint(lucid_bin_dv))
lucid_binary_results <- lucid_binary_results[-1,]
lucid_binary_results$DV <- "Binary"
lucid_binary_results$Name <- c("Democracy", "Similar culture", "High wages", "Small economy", "Ally")
lucid_binary_results$Name <- factor(lucid_binary_results$Name, levels= c("Similar culture", "Small economy", "High wages", "Ally", "Democracy"))
lucid_binary_results$sample <- "General Population\n(Lucid)"

## 
# public (mTurk)
##
mturk_df <- read.dta13("data/study1_mturk.dta")
#limiting to completes
mturk_bin_dv<-lm(bin_support ~ democracy+sim_culture+high_income+small_econ+ally, data=na.omit(mturk_df[c("support_trade_cont","bin_support", "democracy", "sim_culture", "high_income", "small_econ", "ally", "income", "Female", "age", "partyID_recode", "bin_avg_inst", "bin_avg_sec", "bin_avg_econ")])
)
mturk_binary_results <- as.data.frame(coef(summary(mturk_bin_dv)))
mturk_binary_results <- cbind(mturk_binary_results, confint(mturk_bin_dv))
mturk_binary_results <- mturk_binary_results[-1,]
mturk_binary_results$DV <- "Binary"
mturk_binary_results$Name <- c("Democracy", "Similar culture", "High wages", "Small economy", "Ally")
mturk_binary_results$Name <- factor(mturk_binary_results$Name, levels= c("Similar culture", "Small economy", "High wages", "Ally", "Democracy"))
mturk_binary_results$sample <- "General Population\n(mTurk)"

#combine results 
results <- rbind(elites_binary_results, lucid_binary_results, mturk_binary_results)
results$sample <- factor(results$sample, levels= c("Trade/Development\nPolicy Elites", "General Population\n(Lucid)" ,"General Population\n(mTurk)"))
results <- results %>% mutate(Name = factor(Name, levels = c("Democracy", "Ally", "Similar culture", "Small economy", "High wages", "Small military")))

figure2 <- ggplot(results[results$Name!="Small military",], aes(y=Estimate, x=sample)) + 
			geom_hline(yintercept = 0, color="red") +
			geom_point(position = position_dodge(width = 0.8)) + 
			geom_errorbar(aes(ymin=`2.5 %`, ymax=`97.5 %`, width=0), size=.5, position = position_dodge(width = 0.8)) +
			geom_text(aes(label=round(Estimate, digits=2)), size=2.75, position = position_dodge(width = 0.8), vjust=-1.25)+
			coord_flip() +
			facet_grid(Name~., scales="free") +
			theme_classic() + plot_theme + labs(y = "ATE (and 95% CI) of each treatment on support for trade", x="Sample") + theme(legend.position="bottom")
figure2
ggsave("figure2.png", figure2, width=6, height=7, dpi=300)




###
# Figure 3
###

#
# Elites
#

elites_df <- read.dta13("data/study1_elites.dta")
#mech dvs
#econ
econ_dvs <- c("bin_increase_us_jobs", "bin_improve_econ", "bin_decrease_prices", "bin_avg_econ", "increase_us_jobs", "improve_us_economy", "decrease_prices", "avg_econ")
econ_names <- c("Increase US manufacturing jobs", "Improve US economy", "Decrease Prices", "Avg. Economic Concerns", "Increase US manufacturing jobs (ord.)", "Improve US economy (ord.)", "Decrease Prices (ord.)", "Avg. Economic Concerns (ord.)" )
#security
sec_dvs <- c("bin_improve_us_natsec", "bin_decrease_conflict", "bin_increase_military_size", "bin_avg_sec", "improve_us_natsec", "decrease_conflict", "increase_military_size", "avg_sec")
sec_names <- c("Increase national security", "Decrease military conflict", "Increase U.S. military size", "Avg. security concerns","Increase national security (ord.)", "Decrease military conflict (ord.)", "Increase U.S. military size (ord.)", "Avg. security concerns (ord.)")
#inst
inst_dvs <- c("bin_increase_consumer_saf","increase_consumer_saf")
inst_names <- c("Increase consumer safety", "Increase consumer safety (ord.)")


#make a long list of the DVs
all_dvs <- c(econ_dvs, sec_dvs, inst_dvs)
all_names <- c(econ_names, sec_names, inst_names)


#estimate our models...
models <- lapply(all_dvs, function(x) {
    lm(substitute(i ~ democracy+high_income+small_econ+ally+small_mil, list(i = as.name(x))), data = elites_df)
})

#get effect size and CI
coeff <- lapply(models, function(x) { summary(x)$coefficients["democracy","Estimate"] })
lower <- lapply(models, function(x) { confint(x)["democracy","2.5 %"] })
upper <- lapply(models, function(x) { confint(x)["democracy","97.5 %"] })

#group labels 
groups <- c(rep("Economic\nconcerns",length(econ_dvs)),  rep("Security\nconcerns", length(sec_dvs)),  rep("Institutional\nconcerns",length(inst_dvs)))
elite_results <- data.frame(all_names,unlist(all_dvs), unlist(coeff), unlist(lower), unlist(upper), unlist(groups), stringsAsFactors=FALSE)
elite_results$dv_type <- "Ordinal"
elite_results[grep("bin_", elite_results$unlist.all_dvs.),]$dv_type <- "Binary"
elite_results$sample <- "Policy practitioners"
names(elite_results) <- c("label","DV", "ATE", "lower", "upper", "mech_group", "dv_type", "sample")



#
# Appendix: Figure 1
#
figureA1 <- ggplot(elite_results[elite_results$dv_type=="Binary",], aes(x=as.factor(label), y=ATE*100)) + 
  geom_hline(yintercept = 0, color="red") +
  geom_rect(xmax=1.5, xmin=.5, ymin=0-Inf, ymax=Inf, alpha=.05) +
  geom_point(position = position_dodge(width = 0.8)) + coord_flip() +
  facet_grid(mech_group ~., scales="free", space="free")+
  geom_errorbar(aes(ymin=lower*100, ymax=upper*100, width=0), size=.5, position = position_dodge(width = 0.8)) +
  labs(y = "ATE of democracy on support for trade", x="Mechanism") +
  theme_classic() + plot_theme + theme(legend.position="bottom") 

figureA1

ggsave("Appendix/figureA1.png", figureA1, width=7, height=8)



#
# General population (Lucid)
#

df <- read.dta13("data/study1_lucid.dta")

#econ concerns
#reverse code several vars
df$increase_prices <- 6-df$increase_prices
df$increase_diplo_tension <- 6-df$increase_diplo_tension
df$increase_conflict <- 6 - df$increase_conflict

#mech dvs
#econ
econ_dvs <- c("bin_increase_us_jobs", "bin_improve_personal_econ", "bin_improve_econ", "bin_decrease_prices", "bin_avg_econ", "increase_us_jobs", "improve_personal_econ", "improve_us_economy", "increase_prices", "avg_econ")
econ_names <- c("Increase US manufacturing jobs", "Improve personal economic situation", "Improve US economy", "Decrease Prices", "Avg. Economic Concerns", "Increase US manufacturing jobs (ord.)", "Improve personal economic situation (ord.)", "Improve US economy (ord.)", "Decrease Prices (ord.)", "Avg. Economic Concerns (ord.)" )
#security
sec_dvs <- c("bin_improve_us_natsec", "bin_decrease_diplo_tension", "bin_decrease_conflict", "bin_avg_sec", "improve_us_natsec", "increase_diplo_tension", "increase_conflict", "avg_sec")
sec_names <- c("Increase national security", "Decrease diplomatic tension", "Decrease military conflict", "Avg. security concerns","Increase national security (ord.)", "Decrease diplomatic tension (ord.)", "Decrease military conflict (ord.)", "Avg. security concerns (ord.)")

#inst
inst_dvs <- c("bin_compete_fairly", "bin_honor_commitments", "bin_increase_consumer_saf", "bin_avg_inst",  "compete_fairly", "honor_commitments", "increase_consumer_saf", "avg_commit")
inst_names <- c("Compete fairly", "Honor commitments", "Increase consumer safety", "Avg. institutional concerns", "Compete fairly  (ord.)", "Honor commitments  (ord.)", "Increase consumer safety (ord.)", "Avg. institutional concerns  (ord.)")

#make a long list of the DVs
all_dvs <- c(econ_dvs, sec_dvs, inst_dvs)
all_names <- c(econ_names, sec_names, inst_names)

#estimate our models...
models <- lapply(all_dvs, function(x) {
    lm(substitute(i ~ democracy+sim_culture+high_income+small_econ+ally+sim_culture, list(i = as.name(x))), data = na.omit(df[c("bin_support", "democracy", "sim_culture", "high_income", "small_econ", "ally", "hhi", "Female", "age", "education", "partyID", econ_dvs, sec_dvs, inst_dvs)]))
})

#get effect size and CI
coeff <- lapply(models, function(x) { summary(x)$coefficients["democracy","Estimate"] })
lower <- lapply(models, function(x) { confint(x)["democracy","2.5 %"] })
upper <- lapply(models, function(x) { confint(x)["democracy","97.5 %"] })
#group labels 
groups <- c(rep("Economic\nconcerns",length(econ_dvs)),  rep("Security\nconcerns", length(sec_dvs)),  rep("Institutional\nconcerns",length(inst_dvs)))
lucid_results <- data.frame(all_names,unlist(all_dvs), unlist(coeff), unlist(lower), unlist(upper), unlist(groups), stringsAsFactors=FALSE)
lucid_results$dv_type <- "Ordinal"
lucid_results[grep("bin_", lucid_results$unlist.all_dvs.),]$dv_type <- "Binary"
lucid_results$sample <- "Lucid"
names(lucid_results) <- c("label","DV", "ATE", "lower", "upper", "mech_group", "dv_type", "sample")

# ally results for Appendix Figure 2
#get effect size and CI
ally_coeff <- lapply(models, function(x) { summary(x)$coefficients["ally","Estimate"] })
ally_lower <- lapply(models, function(x) { confint(x)["ally","2.5 %"] })
ally_upper <- lapply(models, function(x) { confint(x)["ally","97.5 %"] })
#group labels 
ally_groups <- c(rep("Economic\nconcerns",length(econ_dvs)),  rep("Security\nconcerns", length(sec_dvs)),  rep("Institutional\nconcerns",length(inst_dvs)))
ally_lucid_results <- data.frame(all_names,unlist(all_dvs), unlist(ally_coeff), unlist(ally_lower), unlist(ally_upper), unlist(ally_groups), stringsAsFactors=FALSE)
ally_lucid_results$dv_type <- "Ordinal"
ally_lucid_results[grep("bin_", ally_lucid_results$unlist.all_dvs.),]$dv_type <- "Binary"
ally_lucid_results$sample <- "Lucid"
names(ally_lucid_results) <- c("label","DV", "ATE", "lower", "upper", "mech_group", "dv_type", "sample")

#
# General population (mTurk)
#

mturk_df <- read.dta13("data/study1_mturk.dta")

#econ
econ_dvs <- c("bin_improve_personal_econ", "bin_improve_econ", "bin_decrease_prices", "bin_avg_econ", "improve_personal_econ", "improve_us_economy", "decrease_prices", "avg_econ")
econ_names <- c("Improve personal economic situation", "Improve US economy", "Decrease Prices", "Avg. Economic Concerns", "Improve personal economic situation (ord.)", "Improve US economy (ord.)", "Decrease Prices (ord.)", "Avg. Economic Concerns (ord.)" )
#security
sec_dvs <- c("bin_improve_us_natsec", "bin_decrease_conflict", "bin_avg_sec" ,"improve_us_natsec", "decrease_conf", "avg_sec")
sec_names <- c("Increase national security", "Decrease military conflict", "Avg. security concerns", "Increase national security (ord.)", "Decrease military conflict (ord.)", "Avg. security concerns (ord.)")
#inst
inst_dvs <- c("bin_compete_fairly", "bin_honor_commitments", "bin_treat_fairly",  "bin_avg_inst", "compete_fairly", "honor_commitments", "treat_fairly", "avg_inst")
inst_names <- c("Compete fairly", "Honor commitments", "Fair legal treatment", "Avg. institutional concerns", "Compete fairly  (ord.)", "Honor commitments  (ord.)", "Fair legal treatment (ord.)", "Avg. institutional concerns  (ord.)")

#make a long list of the DVs
all_dvs <- c(econ_dvs, sec_dvs, inst_dvs)
all_names <- c(econ_names, sec_names, inst_names)


#estimate our models...
models <- lapply(all_dvs, function(x) {
    lm(substitute(i ~ democracy+sim_culture+high_income+small_econ+ally, list(i = as.name(x))),data = na.omit(mturk_df[c("bin_support", "democracy", "sim_culture", "high_income", "small_econ", "ally", "income", "Female", "age", "edu", "partyID_recode", econ_dvs, sec_dvs, inst_dvs)]))
})


coeff <- lapply(models, function(x) { summary(x)$coefficients["democracy","Estimate"] })
lower <- lapply(models, function(x) { confint(x)["democracy","2.5 %"] })
upper <- lapply(models, function(x) { confint(x)["democracy","97.5 %"] })
#group labels 
groups <- c(rep("Economic\nconcerns",length(econ_dvs)),  rep("Security\nconcerns", length(sec_dvs)),  rep("Institutional\nconcerns",length(inst_dvs)))
mturk_results <- data.frame(all_names,unlist(all_dvs), unlist(coeff), unlist(lower), unlist(upper), unlist(groups), stringsAsFactors=FALSE)
mturk_results$dv_type <- "Ordinal"
mturk_results[grep("bin_", mturk_results$unlist.all_dvs.),]$dv_type <- "Binary"
mturk_results$sample <- "mTurk"
names(mturk_results) <- c("label","DV", "ATE", "lower", "upper", "mech_group", "dv_type", "sample")

combined_results <- rbind(mturk_results, lucid_results, elite_results)

combined_results$mech_group <- factor(combined_results$mech_group, levels= c("Institutional\nconcerns", "Security\nconcerns", "Economic\nconcerns"))


#ally reuslts for Appendix Figure 2
ally_coeff <- lapply(models, function(x) { summary(x)$coefficients["ally","Estimate"] })
ally_lower <- lapply(models, function(x) { confint(x)["ally","2.5 %"] })
ally_upper <- lapply(models, function(x) { confint(x)["ally","97.5 %"] })
#group labels 
ally_groups <- c(rep("Economic\nconcerns",length(econ_dvs)),  rep("Security\nconcerns", length(sec_dvs)),  rep("Institutional\nconcerns",length(inst_dvs)))
ally_mturk_results <- data.frame(all_names,unlist(all_dvs), unlist(ally_coeff), unlist(ally_lower), unlist(ally_upper), unlist(ally_groups), stringsAsFactors=FALSE)
ally_mturk_results$dv_type <- "Ordinal"
ally_mturk_results[grep("bin_", ally_mturk_results$unlist.all_dvs.),]$dv_type <- "Binary"
ally_mturk_results$sample <- "mTurk"
names(ally_mturk_results) <- c("label","DV", "ATE", "lower", "upper", "mech_group", "dv_type", "sample")

#
# Figure 3
#
figure3 <- ggplot(combined_results[combined_results$dv_type=="Binary" & (combined_results$sample=="Lucid" | combined_results$sample=="mTurk"),], aes(x=as.factor(label), y=ATE*100, shape=sample)) + 
      geom_hline(yintercept = 0, color="red") +
      geom_text(aes(label=round(ATE*100, digits=1)), size=2.5, position = position_dodge(width = 0.8), vjust=-1)+
      geom_rect(xmax=1.5, xmin=.5, ymin=0-Inf, ymax=Inf, alpha=.05) +
      geom_point(position = position_dodge(width = 0.8)) + coord_flip() +
      facet_grid(mech_group ~., scales="free", space="free")+
      geom_errorbar(aes(ymin=lower*100, ymax=upper*100, width=0), size=.5, position = position_dodge(width = 0.8)) +
      labs(y = "ATE of democracy on support for trade", x="Mechanism") +
      theme_classic() + plot_theme + theme(legend.position="bottom") +
  ylim(-12, 35)
figure3
ggsave("figure3.png", figure3, width=7, height=8, dpi=300)


## 
# Appendix: Figure 2
## 

###ally results
ally_combined_results <- rbind(ally_mturk_results, ally_lucid_results)


#all mechanisms, mturk, lucid  
figureA2 <- ggplot(ally_combined_results[ally_combined_results$dv_type=="Binary" & (ally_combined_results$sample=="Lucid" | ally_combined_results$sample=="mTurk"),], aes(x=as.factor(label), y=ATE*100, shape=sample)) + 
  geom_text(aes(label=round(ATE*100, digits=2)), size=2.5, position = position_dodge(width = 0.8), vjust=-1)+
  geom_hline(yintercept = 0, color="red") +
  geom_rect(xmax=1.5, xmin=.5, ymin=0-Inf, ymax=Inf, alpha=.05) +
  geom_point(position = position_dodge(width = 0.8)) + coord_flip() +
  facet_grid(mech_group ~., scales="free", space="free")+
  geom_errorbar(aes(ymin=lower*100, ymax=upper*100, width=0), size=.5, position = position_dodge(width = 0.8)) +
  labs(y = "ATE of alliance on support for trade", x="Mechanism") +
  theme_classic() + plot_theme + theme(legend.position="bottom") +
  ylim(-10, 35)
figureA2
ggsave("Appendix/figureA2.png", figureA2, width=7, height=8, dpi=300)







###
## Figure 4 
###

#BK approach using code from Tomz and Weeks 
#lucid 1 mediation
lucid_med_df <- na.omit(lucid_df[c("bin_support", "democracy", "sim_culture", "high_income", "small_econ", "ally", "hhi", "Female", "age", "partyID", "bin_avg_inst", "bin_avg_sec", "bin_avg_econ", "education")])

#mturk 1 mediation
mturk_med_df <- na.omit(mturk_df[c("bin_support", "democracy", "sim_culture", "high_income", "small_econ", "ally", "income", "Female", "age", "partyID_recode", "edu", "bin_avg_inst", "bin_avg_sec", "bin_avg_econ")])

#write dataset for mediation analysis
write.dta(mturk_med_df, "mturk_med_df.dta")
write.dta(lucid_med_df, "lucid_med_df.dta")

med_results<-read.dta("mediation_results.dta")
med_results$name <- factor(med_results$name, levels = c("Other concerns","Security concerns", "Economic concerns",  "Institutional concerns"))


figure4 <-  ggplot(med_results, aes(x=name, y=est)) + 
  geom_hline(yintercept = 0, color="red")+
  geom_point(position = position_dodge(width = 0.8)) + coord_flip() +
  facet_grid(sample ~., scales="free", space="free")+
  geom_text(aes(label=round(est, digits=0)), size=2.75, position = position_dodge(width = 0.8), vjust=-1.25)+
  geom_errorbar(aes(ymin=lo, ymax=hi, width=0), size=.5, position = position_dodge(width = 0.8)) +
  labs(y = "Proportion mediated", x="Mechanisms") +
  theme_classic() + plot_theme + theme(legend.position="bottom") 
figure4
ggsave("figure4.png", figure4, dpi=300, width=6, height=3)



###
# Study 2 (Lucid)
###

lucid2 <- read.dta13("data/study2_lucid.dta")


### 
# Figure 5
###
#predicted outcomes from study 2. 

bin_dv<-lm(bin_support ~ democracy*treat_fair*honor_int+as.numeric(hhi)+Female+age+as.numeric(partyID)+education, data=lucid2)

emmeans_lucid2 <- as_tibble(emmeans(bin_dv, ~treat_fair*honor_int, by= "democracy")) %>% 
  mutate(behavior = case_when(                    treat_fair == 1 & honor_int == 1 ~ "Good Past Behavior\n(Fair Legal Treatment\nand Honor Trade Commit.)", 
                                                  treat_fair == 0 & honor_int == 1 ~ "Mixed",
                                                  treat_fair == 1 & honor_int == 0 ~ "Mixed",
                                                  treat_fair == 0 & honor_int == 0 ~ "Bad Past Behavior\n(~Fair Legal Treatment\nand ~Honor Trade Commit.)"))%>%
  mutate(behavior = factor(behavior, levels = c("Mixed", "Bad Past Behavior\n(~Fair Legal Treatment\nand ~Honor Trade Commit.)", "Good Past Behavior\n(Fair Legal Treatment\nand Honor Trade Commit.)"))) %>% 
  filter(behavior != "Mixed") %>% 
  mutate(dem_label = case_when(democracy == 0 ~ "Not a democracy", 
                               democracy == 1 ~ "Democracy")) %>%
  mutate(dem_label = factor(dem_label, levels = c( "Not a democracy", "Democracy")))


figure5 <- ggplot(emmeans_lucid2 , 
               aes(y=emmean*100, x=behavior, shape=as.factor(dem_label))) +
  
  geom_point(position = position_dodge(width = 0.5)) + 
  geom_text(aes(label=round(emmean*100, digits=1)), size=2.5, position = position_dodge(width = 0.8))+
  geom_errorbar(aes(ymin=lower.CL*100, ymax=upper.CL*100, width=0), size=.5,  position = position_dodge(width = 0.5)) +
  theme_classic() +
  plot_theme + labs(x = "Treatments", y="% support\nfor trade") +
  scale_shape_discrete(name="Regime type") +
  theme(legend.position = c(.4, 0.85), legend.title = element_text(size=8, face="bold"), legend.text = element_text(size=8), legend.direction = "horizontal", legend.box.background = element_rect(colour = "black", size=.7))  
figure5

ggsave("figure5.png", figure5, width=6, height=4, dpi=300)


#
# Appendix: Figure 3
#

bin_dv<-lm(bin_support ~ democracy*treat_fair*honor_int+as.numeric(hhi)+Female+age+as.numeric(partyID)+education, data=lucid2)
emmeans_lucid2 <- as_tibble(emmeans(bin_dv, ~treat_fair*honor_int, by= "democracy")) %>% 
  mutate(behavior = case_when(                    treat_fair == 1 & honor_int == 1 ~ "Good Past Behavior\n(Fair Legal Treatment\nand Honor Trade Commit.)", 
                                                  treat_fair == 0 & honor_int == 1 ~ "Mixed Past Behavior\n(~Fair Legal Treatment\nand Honor Trade Commit.)",
                                                  treat_fair == 1 & honor_int == 0 ~ "Mixed Past Behavior\n(Fair Legal Treatment\nand ~Honor Trade Commit.)",
                                                  treat_fair == 0 & honor_int == 0 ~ "Bad Past Behavior\n(~Fair Legal Treatment\nand ~Honor Trade Commit.)"))%>%
  mutate(behavior = factor(behavior, levels = c("Bad Past Behavior\n(~Fair Legal Treatment\nand ~Honor Trade Commit.)", "Mixed Past Behavior\n(Fair Legal Treatment\nand ~Honor Trade Commit.)",  "Mixed Past Behavior\n(~Fair Legal Treatment\nand Honor Trade Commit.)", "Good Past Behavior\n(Fair Legal Treatment\nand Honor Trade Commit.)"))) %>% 
  mutate(dem_label = case_when(democracy == 0 ~ "Not a democracy", 
                               democracy == 1 ~ "Democracy")) %>%
  mutate(dem_label = factor(dem_label, levels = c( "Not a democracy", "Democracy")))

figureA3 <- ggplot(emmeans_lucid2 , 
                  aes(y=emmean*100, x=behavior, shape=as.factor(dem_label))) +
  
  geom_point(position = position_dodge(width = 0.5)) + 
  geom_text(aes(label=round(emmean*100, digits=1)), size=2.5, position = position_dodge(width = 0.9))+
  geom_errorbar(aes(ymin=lower.CL*100, ymax=upper.CL*100, width=0), size=.5,  position = position_dodge(width = 0.5)) +
  theme_classic() +
  plot_theme + labs(x = "Treatments", y="% support\nfor trade") +
  scale_shape_discrete(name="Regime type") +
  theme(legend.position = c(.4, 0.85), legend.title = element_text(size=8, face="bold"), legend.text = element_text(size=8), legend.direction = "horizontal", legend.box.background = element_rect(colour = "black", size=.7))  
figureA3

ggsave("Appendix/figureA3.png", figureA3, width=8, height=6, dpi=300)




###
# Figure 6
###


lucid2 <- lucid2 %>% mutate(behavior = case_when(treat_fair == 1 & honor_int == 1 ~ "Good", 
                                                 treat_fair == 0 & honor_int == 1 ~ "Mixed",
                                                 treat_fair == 1 & honor_int == 0 ~ "Mixed",
                                                 treat_fair == 0 & honor_int == 0 ~ "Bad")) %>%
                     mutate(behavior = factor(behavior, levels = c("Good", "Mixed", "Bad"))) 

lucid2_contrasts <- lm_robust(bin_support ~ democracy*behavior+as.numeric(hhi)+Female+age+as.numeric(partyID)+education, data=lucid2 )
lucid_em <- emmeans(lucid2_contrasts, ~ behavior, by ="democracy") 

relative_to_good <- as_tibble(contrast(lucid_em, method="pairwise", by="democracy", infer =TRUE)) %>% 
  separate(contrast, c("contrast1", "contrast2")) %>% 
  filter(contrast1 == "Good" & contrast2 == "Bad")  %>% 
  mutate(regime = case_when(democracy == 1 ~ "Democracy", 
                            democracy == 0 ~ "Not a democracy"))



#p-value on democracy*bad behavior
dem_eff <- round(summary(lucid2_contrasts)$coefficients[,1][["democracy:behaviorBad"]]*100,1)
#low on democracy*bad behavior
dem_eff_low <- round(lucid2_contrasts$conf.low[["democracy:behaviorBad"]]*100,1)
#hiw on democracy*bad behavior
dem_eff_high <- round(lucid2_contrasts$conf.high[["democracy:behaviorBad"]]*100,1)

#p-value on democracy*bad behavior
dem_eff_p <- round(summary(lucid2_contrasts)$coefficients[,4][["democracy:behaviorBad"]],2)


figure6 <- ggplot(data = relative_to_good, aes(x=as.factor(regime), y=estimate*-100)) + 
  geom_errorbar(aes(ymin=lower.CL*-100, ymax=upper.CL*-100, width=0), size=.5, position = position_dodge(width = 0.8)) +
  geom_segment(aes(x=1, xend=2, y=-38.2, yend=-23.9), linetype=2, color="#AAAAAA") +
  geom_hline(yintercept = 0, color="red") +
  geom_point(position = position_dodge(width = 0.8)) + coord_flip() +
  geom_text(aes(label=round(estimate*-100, digits=1)), size=2.5, position = position_dodge(width = 0.8), vjust=-1)+
  labs(y = "ATE of bad behavior on support for trade", x="Regime type") +
  theme_classic() + plot_theme + theme(legend.position="bottom") +
  geom_text(aes(x =1.5, y = -25), label = paste0("    Effect of democracy: ", dem_eff , "\n(95% CI: ", dem_eff_low, ",", dem_eff_high, "; p-value = ", dem_eff_p, "." )
, size=2.5, hjust = 0, lineheight = .9) 
figure6

ggsave("figure6.png", figure6, width=7, height=3, dpi=300)


###
# Figure 7
###

bin_dv<-lm(bin_support ~ democracy+sim_culture+high_income+small_econ+ally+sim_culture, data=lucid_df)
country_specific_pred <- data.frame(country=c("China", "India", "Canada", "EU", "Germany","Brazil","Mexico","Russia", "South Korea","Japan"), 
                                    democracy=c(0,1,1,1,1,1,1,0,1,1), 
                                    sim_culture=c(0,0,1,.5,.5,0,0,0,0,0), 
                                    high_income=c(.5,0,1,1,1,.5,.5,.5,.5,1), 
                                    small_econ=c(0,0,0,0,0,0,0,0,0,0), 
                                    ally=c(0,0,1,1,1,1,0,0,1,1))
pred <- predict(bin_dv, newdata=country_specific_pred, interval="confidence") 
country_specific_pred <- cbind(country_specific_pred,pred)
pew_support <- read.csv("data/pew_formatted.csv")
pew <- pew_support[c("country.x","year", "trade_support")]
pew <- pew %>% 
  spread("year", "trade_support") %>% 
  left_join(country_specific_pred[c("country","fit")], by=c("country.x"="country")) %>%
  select(-`2010`)
pew$fit <- round(pew$fit, digits=0)
pew <- na.omit(pew) %>% rename("Predicted Support" = fit, "Pew 2014" = "2014", "Country" = country.x)
predicted_support_plot <- gather(pew[!is.na(pew$`Pew 2014`),], "Predicted Support", "Pew 2014",  key="observation", value="trade_support") 
figure7<-newggslopegraph(predicted_support_plot, observation, trade_support, Country, Title=NULL, SubTitle=NULL, Caption=NULL, LineColor="black", LineThickness=.8, XTextSize=9)
figure7
ggsave("figure7.png", figure7, width=5, height=6, dpi=300)


###
# End
##






