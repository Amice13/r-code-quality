# descriptive plots for social media paper
rm(list=ls())

library(dplyr)
library(ggplot2)
library(ggthemes)

theme_set(theme_bw() + theme(axis.text = element_text(size = 12)))


dat = readRDS("cleaned_survey.rds")
dat = dat %>% filter(as.numeric(Progress) > 49)




# Use of social media -----------------------------------------------------

usage = c("1-3 times", "4-9 times", "10-15 times", "More than 15 times")
sm_use = data.frame(type = c("Facebook", "Twitter", "Instagram", "Nextdoor", "Other"),
                    pct = NA_real_)
usedf = subset(dat, !is.na(off_sm_use_fb) | !is.na(off_sm_use_twitter) |
                 !is.na(off_sm_use_ig) | !is.na(off_sm_use_nextdoor) |
                 !is.na(off_sm_use_other))

# facebook
sm_use$pct[sm_use$type == "Facebook"] = mean(usedf$off_sm_use_fb %in% usage)

# twitter
sm_use$pct[sm_use$type == "Twitter"] = mean(usedf$off_sm_use_twit %in% usage)

# instagram
sm_use$pct[sm_use$type == "Instagram"] = mean(usedf$off_sm_use_ig %in% usage)

# nextdoor
sm_use$pct[sm_use$type == "Nextdoor"] = mean(usedf$off_sm_use_nextdoor %in% usage)

# other
sm_use$pct[sm_use$type == "Other"] = mean(usedf$off_sm_use_other %in% usage)

# re-arrange
sm_use = sm_use[order(sm_use$pct),]
sm_use$type = factor(sm_use$type, levels = sm_use$type)


# plot it
ggplot(sm_use) + 
  aes(x = type, y = pct * 100) + 
  geom_bar(stat = "identity") + 
  coord_flip(ylim = c(0,70)) + 
  labs(x = NULL, y = "Percent of officials who use SM in their official capacity") + 
  scale_y_continuous(breaks = seq(0, 100, 10))
ggsave("Figs_and_tabs/usage_of_sm.pdf", height = 4, width=6)


# any form of sm 
any_sm =     dat$off_sm_use_fb %in% usage | 
             dat$off_sm_use_twit %in% usage | 
             dat$off_sm_use_ig %in% usage | 
             dat$off_sm_use_nextdoor %in% usage | 
             dat$off_sm_use_other %in% usage
mean(any_sm) * 100



# Modes of hearing from constituents - prevalence and usefulness ----------

# Percent of respondents hearing from constituents via these means at least
# once in the past month and Percent of respondents saying this mode of 
# communication tends to be Very useful in forming own views.
usage = c("1-3 times", "4-9 times", "10-15 times", "More than 15 times")
useful = "Very useful"

comm_from_sum = data.frame(mode = c("Public meeting comments",
                                    "Op-eds",
                                    "Public social media comments",
                                    "Online petitions",
                                    "In-person petitions",
                                    "Private meetings",
                                    "Emails",
                                    "Letters",
                                    "Phone calls", 
                                    "Private messages on social media"))
comm_from_sum$hilight = 0
comm_from_sum$hilight[grepl("social media", comm_from_sum$mode)] = 1
comm_from_sum$public = ifelse(
  comm_from_sum$mode %in%
    c(
      "Public meeting comments",
      "Op-eds",
      "Public social media comments",
      "Online petitions",
      "In-person petitions"
    ), 1, 0
)


commfromdf = subset(dat,
                    !is.na(comm_from_freq_pubmeetingcomments) |  !is.na(comm_from_freq_pubsmcomments) |
                    !is.na(comm_from_freq_pubopeds) |  !is.na(comm_from_freq_pubonlinepetition) |
                    !is.na(comm_from_freq_pubinpersonpetition) |!is.na(comm_from_freq_privmeetings) |
                    !is.na(comm_from_freq_privemails) |  !is.na(comm_from_freq_privletters) |
                    !is.na(comm_from_freq_privphonecalls) |  !is.na(comm_from_freq_privsmmessages) )
commfromdf = commfromdf %>% 
  mutate_at(vars(starts_with("comm_from_use")), 
            .funs = function(x){
             case_when( x == "Not at all useful" ~ 0,
              x == "Not very useful" ~ 1/3,
              x == "Somewhat useful" ~ 2/3,
              x == "Very useful" ~ 1)
            })

# table with crosstab
tmp = commfromdf %>% 
  select(starts_with("comm_from_use")) %>% 
  lapply(function(x) as.vector(prop.table(table(x))) ) 
tmp = data.frame(do.call(rbind, tmp))
tmp$modality = rownames(tmp)
tmp$mode = (
  case_when(
    tmp$modality == "comm_from_use_pubonlinepetition" ~ "Online petitions",
    tmp$modality == "comm_from_use_pubopeds" ~ "Op-eds",
    tmp$modality == "comm_from_use_pubinpersonpetition" ~ "In-person petitions",
    tmp$modality == "comm_from_use_pubsmcomments" ~ "Public social media comments",
    tmp$modality == "comm_from_use_privsmmessages" ~ "Private messages on social media",
    tmp$modality == "comm_from_use_privletters" ~ "Letters",
    tmp$modality == "comm_from_use_pubmeetingcomments" ~ "Public meeting comments",
    tmp$modality == "comm_from_use_privemails" ~ "Emails",
    tmp$modality == "comm_from_use_privphonecalls" ~ "Phone calls",
    tmp$modality == "comm_from_use_privmeetings" ~ "Private meetings"
  ))  
out = tmp[, c("mode", "X1", "X2", "X3", "X4")]
rownames(out) = NULL
out$X1 = paste0(round(100*out$X1, 1), "%")
out$X2 = paste0(round(100*out$X2, 1), "%")
out$X3 = paste0(round(100*out$X3, 1), "%")
out$X4 = paste0(round(100*out$X4, 1), "%")

names(out) = c("Modality", "Not at all useful", "Not very useful", "Somewhat useful", "Very useful")
write.csv(out, "Figs_and_tabs/usefulness_tabs.csv", row.names = FALSE)

  
  


# public meeting comments
comm_from_sum$usage_pct[comm_from_sum$mode == "Public meeting comments"] = mean(commfromdf$comm_from_freq_pubmeetingcomments %in% usage, na.rm=TRUE)
comm_from_sum$very_useful_pct[comm_from_sum$mode == "Public meeting comments"] = mean(commfromdf$comm_from_use_pubmeetingcomments == 1, na.rm=TRUE)
comm_from_sum$useful_mean[comm_from_sum$mode == "Public meeting comments"] = mean(commfromdf$comm_from_use_pubmeetingcomments, na.rm=TRUE)

# op-eds
comm_from_sum$usage_pct[comm_from_sum$mode == "Op-eds"] = mean(commfromdf$comm_from_freq_pubopeds %in% usage, na.rm=TRUE)
comm_from_sum$very_useful_pct[comm_from_sum$mode == "Op-eds"] = mean(commfromdf$comm_from_use_pubopeds == 1, na.rm=TRUE)
comm_from_sum$useful_mean[comm_from_sum$mode == "Op-eds"] = mean(commfromdf$comm_from_use_pubopeds, na.rm=TRUE)

# public SM comments
comm_from_sum$usage_pct[comm_from_sum$mode == "Public social media comments"] = mean(commfromdf$comm_from_freq_pubsmcomments %in% usage, na.rm=TRUE)
comm_from_sum$very_useful_pct[comm_from_sum$mode == "Public social media comments"] = mean(commfromdf$comm_from_use_pubsmcomments == 1, na.rm=TRUE)
comm_from_sum$useful_mean[comm_from_sum$mode == "Public social media comments"] = mean(commfromdf$comm_from_use_pubsmcomments, na.rm=TRUE)

# online petitions
comm_from_sum$usage_pct[comm_from_sum$mode == "Online petitions"] = mean(commfromdf$comm_from_freq_pubonlinepetition %in% usage, na.rm=TRUE)
comm_from_sum$very_useful_pct[comm_from_sum$mode == "Online petitions"] = mean(commfromdf$comm_from_use_pubonlinepetition == 1, na.rm=TRUE)
comm_from_sum$useful_mean[comm_from_sum$mode == "Online petitions"] = mean(commfromdf$comm_from_use_pubonlinepetition, na.rm=TRUE)

# in-person petitions
comm_from_sum$usage_pct[comm_from_sum$mode == "In-person petitions"] = mean(commfromdf$comm_from_freq_pubinpersonpetition %in% usage, na.rm=TRUE)
comm_from_sum$very_useful_pct[comm_from_sum$mode == "In-person petitions"] = mean(commfromdf$comm_from_use_pubinpersonpetition == 1, na.rm=TRUE)
comm_from_sum$useful_mean[comm_from_sum$mode == "In-person petitions"] = mean(commfromdf$comm_from_use_pubinpersonpetition, na.rm=TRUE)

# private meetings
comm_from_sum$usage_pct[comm_from_sum$mode == "Private meetings"] = mean(commfromdf$comm_from_freq_privmeetings %in% usage, na.rm=TRUE)
comm_from_sum$very_useful_pct[comm_from_sum$mode == "Private meetings"] = mean(commfromdf$comm_from_use_privmeetings == 1, na.rm=TRUE)
comm_from_sum$useful_mean[comm_from_sum$mode == "Private meetings"] = mean(commfromdf$comm_from_use_privmeetings, na.rm=TRUE)

# emails
comm_from_sum$usage_pct[comm_from_sum$mode == "Emails"] = mean(commfromdf$comm_from_freq_privemails %in% usage, na.rm=TRUE)
comm_from_sum$very_useful_pct[comm_from_sum$mode == "Emails"] = mean(commfromdf$comm_from_use_privemails == 1, na.rm=TRUE)
comm_from_sum$useful_mean[comm_from_sum$mode == "Emails"] = mean(commfromdf$comm_from_use_privemails, na.rm=TRUE)

# letters
comm_from_sum$usage_pct[comm_from_sum$mode == "Letters"] = mean(commfromdf$comm_from_freq_privletters %in% usage, na.rm=TRUE)
comm_from_sum$very_useful_pct[comm_from_sum$mode == "Letters"] = mean(commfromdf$comm_from_use_privletters == 1, na.rm=TRUE)
comm_from_sum$useful_mean[comm_from_sum$mode == "Letters"] = mean(commfromdf$comm_from_use_privletters, na.rm=TRUE)

# phone calls
comm_from_sum$usage_pct[comm_from_sum$mode == "Phone calls"] = mean(commfromdf$comm_from_freq_privphonecalls %in% usage, na.rm=TRUE)
comm_from_sum$very_useful_pct[comm_from_sum$mode == "Phone calls"] = mean(commfromdf$comm_from_use_privphonecalls == 1, na.rm=TRUE)
comm_from_sum$useful_mean[comm_from_sum$mode == "Phone calls"] = mean(commfromdf$comm_from_use_privphonecalls, na.rm=TRUE)

# private messages on SM
comm_from_sum$usage_pct[comm_from_sum$mode == "Private messages on social media"] = mean(commfromdf$comm_from_freq_privsmmessages %in% usage, na.rm=TRUE)
comm_from_sum$very_useful_pct[comm_from_sum$mode == "Private messages on social media"] = mean(commfromdf$comm_from_use_privsmmessages == 1, na.rm=TRUE)
comm_from_sum$useful_mean[comm_from_sum$mode == "Private messages on social media"] = mean(commfromdf$comm_from_use_privsmmessages, na.rm=TRUE)




## PLOT PREVALENCE OF EACH MODE OF COMMUNICATION ##
# re-order
comm_from_sum = comm_from_sum[order(comm_from_sum$usage_pct), ]
comm_from_sum$mode = factor(comm_from_sum$mode, comm_from_sum$mode)

# plot it
ggplot(comm_from_sum) +
  aes(x = mode, y = usage_pct * 100, fill = as.factor(1-hilight)) + 
  geom_bar(stat = "identity") + 
  coord_flip(ylim = c(0, 100)) +
  scale_fill_grey() + 
  scale_y_continuous(breaks = seq(0, 101, 25)) + 
  # scale_fill_manual(values = c(col.pal[3], col.pal[1])) +
  guides(fill = FALSE) + 
  labs(x = NULL, y = "Percent of respondents hearing\nfrom constituents via each mode") 
ggsave("Figs_and_tabs/modes_hearing_from_const.pdf", width=6, height = 4)



ggplot(subset(comm_from_sum, public == 1)) +
  aes(x = mode, y = usage_pct * 100, fill = as.factor(1-hilight)) + 
  geom_bar(stat = "identity") + 
  coord_flip(ylim = c(0, 100)) +
  scale_fill_grey() + 
  scale_y_continuous(breaks = seq(0, 101, 25)) + 
  # scale_fill_manual(values = c(col.pal[3], col.pal[1])) +
  guides(fill = FALSE) + 
  labs(x = NULL, y = "Percent of respondents hearing\nfrom constituents via each mode") 
ggsave("Figs_and_tabs/modes_hearing_from_const_public.pdf", width=6, height = 4)



ggplot(subset(comm_from_sum, public == 0)) +
  aes(x = mode, y = usage_pct * 100, fill = as.factor(1-hilight)) + 
  geom_bar(stat = "identity") + 
  coord_flip(ylim = c(0, 100)) +
  scale_fill_grey() + 
  scale_y_continuous(breaks = seq(0, 101, 25)) + 
  # scale_fill_manual(values = c(col.pal[3], col.pal[1])) +
  guides(fill = FALSE) + 
  labs(x = NULL, y = "Percent of respondents hearing\nfrom constituents via each mode") 
ggsave("Figs_and_tabs/modes_hearing_from_const_private.pdf", width=6, height = 4)











### PERCENT SAYING VERY USEFUL ###
comm_from_sum = comm_from_sum[order(comm_from_sum$very_useful_pct), ]
comm_from_sum$mode = factor(comm_from_sum$mode, comm_from_sum$mode)

# plot it
ggplot(comm_from_sum) +
  aes(x = mode, y = very_useful_pct * 100, fill = as.factor(1-hilight)) + 
  geom_bar(stat = "identity") + 
  coord_flip(ylim = c(0, 100)) +
  scale_fill_grey() + 
  scale_y_continuous(breaks = seq(0, 101, 25)) + 
  # scale_fill_manual(values = c(col.pal[3], col.pal[1])) +
  guides(fill = FALSE) + 
  labs(x = NULL, y = "Percent of respondents rating each\nmode as 'Very Useful' in forming views") 
ggsave("Figs_and_tabs/modes_hearing_from_const_usefulness.pdf", width=6, height = 4)



ggplot(subset(comm_from_sum, public == 1)) +
  aes(x = mode, y = very_useful_pct * 100, fill = as.factor(1-hilight)) + 
  geom_bar(stat = "identity") + 
  coord_flip(ylim = c(0, 100)) +
  scale_fill_grey() + 
  scale_y_continuous(breaks = seq(0, 101, 25)) + 
  # scale_fill_manual(values = c(col.pal[3], col.pal[1])) +
  guides(fill = FALSE) + 
  labs(x = NULL, y = "Percent of respondents rating each\nmode as 'Very Useful' in forming views") 
ggsave("Figs_and_tabs/modes_hearing_from_const_usefulness_public.pdf", width=6, height = 4)



ggplot(subset(comm_from_sum, public == 0)) +
  aes(x = mode, y = very_useful_pct * 100, fill = as.factor(1-hilight)) + 
  geom_bar(stat = "identity") + 
  coord_flip(ylim = c(0, 100)) +
  scale_fill_grey() + 
  scale_y_continuous(breaks = seq(0, 101, 25)) + 
  # scale_fill_manual(values = c(col.pal[3], col.pal[1])) +
  guides(fill = FALSE) + 
  labs(x = NULL, y = "Percent of respondents rating each\nmode as 'Very Useful' in forming views") 
ggsave("Figs_and_tabs/modes_hearing_from_const_usefulness_private.pdf", width=6, height = 4)













### MEAN USEFULNESS, 0-1 scale ###
comm_from_sum = comm_from_sum[order(comm_from_sum$useful_mean),]
comm_from_sum$mode = factor(comm_from_sum$mode, comm_from_sum$mode)

# plot it
ggplot(comm_from_sum) +
  aes(x = mode, y = useful_mean, fill = as.factor(1-hilight)) + 
  geom_bar(stat = "identity") + 
  coord_flip(ylim = c(0, 1)) +
  scale_fill_grey() + 
  scale_y_continuous(breaks = seq(0, 1, .25)) + 
  # scale_fill_manual(values = c(col.pal[3], col.pal[1])) +
  guides(fill = FALSE) + 
  labs(x = NULL, y = "Mean usefulness rating (0-1)") 
ggsave("Figs_and_tabs/modes_hearing_from_const_mean_useful.pdf", width=6, height = 4)



ggplot(subset(comm_from_sum, public == 1)) +
  aes(x = mode, y = useful_mean, fill = as.factor(1-hilight)) + 
  geom_bar(stat = "identity") + 
  coord_flip(ylim = c(0, 1)) +
  scale_fill_grey() + 
  scale_y_continuous(breaks = seq(0, 1, .25)) + 
  # scale_fill_manual(values = c(col.pal[3], col.pal[1])) +
  guides(fill = FALSE) + 
  labs(x = NULL, y = "Mean usefulness rating (0-1)") 
ggsave("Figs_and_tabs/modes_hearing_from_const_mean_useful_public.pdf", width=6, height = 4)



ggplot(subset(comm_from_sum, public == 0)) +
  aes(x = mode, y = useful_mean, fill = as.factor(1-hilight)) + 
  geom_bar(stat = "identity") + 
  coord_flip(ylim = c(0, 1)) +
  scale_fill_grey() + 
  scale_y_continuous(breaks = seq(0, 1, .25)) + 
  # scale_fill_manual(values = c(col.pal[3], col.pal[1])) +
  guides(fill = FALSE) + 
  labs(x = NULL, y = "Mean usefulness rating (0-1)") 
ggsave("Figs_and_tabs/modes_hearing_from_const_mean_useful_private.pdf", width=6, height = 4)





# Effective Modes ---------------------------------------------------------


# Imagine you have not already arrived at a firm decision on an issue. How 
# useful would each of the following communication strategies be for 
# influencing your decision?

useful = c("Very useful")

moddf = subset(dat, 
               !is.na(mod_inpersonmeeting) | 
                 !is.na(mod_emails) | 
                 !is.na(mod_letters) | 
                 !is.na(mod_pubmeetings) | 
                 !is.na(mod_phonecalls) | 
                 !is.na(mod_smdms) | 
                 !is.na(mod_smpubcomments) | 
                 !is.na(mod_opeds) | 
                 !is.na(mod_petitions) | 
                 !is.na(mod_polls))
moddf = moddf %>% 
  mutate_at(vars(starts_with("mod_")),
            function(x){
              case_when(
                x == "Not at all useful" ~ 0,
                x == "Somewhat useful" ~ .5,
                x == "Very useful" ~ 1
              )
            })

mod_sum = data.frame(mode = c("Private meetings",
                              "Emails from constituents",
                              "Letters sent in the mail",
                              "Public meetings",
                              "Phone calls",
                              "Direct messages on social media",
                              "Public comments on social media",
                              "Newspaper opinion articles",
                              "Petitions", 
                              "Public opinion polls"),
                     pct = NA_real_)
mod_sum$hilight = 0
mod_sum$hilight[grep("social media", mod_sum$mode)] = 1



mod_sum$useful_mean[1]  = mean(moddf$mod_inpersonmeeting ,  na.rm=TRUE)
mod_sum$useful_mean[2]  = mean(moddf$mod_emails ,  na.rm=TRUE)
mod_sum$useful_mean[3]  = mean(moddf$mod_letters ,  na.rm=TRUE)
mod_sum$useful_mean[4]  = mean(moddf$mod_pubmeetings ,  na.rm=TRUE)
mod_sum$useful_mean[5]  = mean(moddf$mod_phonecalls ,  na.rm=TRUE)
mod_sum$useful_mean[6]  = mean(moddf$mod_smdms ,  na.rm=TRUE)
mod_sum$useful_mean[7]  = mean(moddf$mod_smpubcomment ,  na.rm=TRUE)
mod_sum$useful_mean[8]  = mean(moddf$mod_opeds ,  na.rm=TRUE)
mod_sum$useful_mean[9]  = mean(moddf$mod_petitions ,  na.rm=TRUE)
mod_sum$useful_mean[10] = mean(moddf$mod_polls ,  na.rm=TRUE)


# re-order
mod_sum = mod_sum[order(mod_sum$useful_mean), ]
mod_sum$mode = factor(mod_sum$mode, mod_sum$mode)

# get rid of polls, petitions, and newspaper articles
# mod_sum_gg = subset(mod_sum, !mode %in% c("Petitions", "Public opinion polls", "Newspaper opinion articles"))

# plot it
modes = ggplot(mod_sum) + 
  aes(x = mode, y = useful_mean, fill = as.factor(1-hilight)) + 
  geom_bar( stat = "identity") + 
  coord_flip(ylim = c(0, 1)) +
  scale_fill_grey() +
  guides(fill = FALSE) + 
  labs(x = NULL, y = "Mean usefulness (0-1)") + 
  scale_y_continuous(breaks = seq(0, 1, .25))
ggsave(modes, filename = "Figs_and_tabs/effective_modes.pdf", height = 4, width = 6)





## table with response distributions
moddf_tab = moddf %>% 
  select(starts_with("mod_")) %>% 
  lapply(function(x) as.vector(prop.table(table(x))) ) 
moddf_tab = data.frame(do.call(rbind, moddf_tab))
moddf_tab$modality = rownames(moddf_tab)
moddf_tab$mode = case_when(
  moddf_tab$modality == "mod_inpersonmeeting" ~  "Private meetings",
  moddf_tab$modality == "mod_emails" ~  "Emails from constituents",
  moddf_tab$modality == "mod_letters" ~  "Letters sent in the mail",
  moddf_tab$modality == "mod_pubmeetings" ~  "Public meetings",
  moddf_tab$modality == "mod_phonecalls" ~  "Phone calls",
  moddf_tab$modality == "mod_smdms" ~  "Direct messages on social media",
  moddf_tab$modality == "mod_smpubcomments" ~  "Public comments on social media",
  moddf_tab$modality == "mod_opeds" ~  "Newspaper opinion articles",
  moddf_tab$modality == "mod_petitions" ~  "Petitions", 
  moddf_tab$modality == "mod_polls" ~  "Public opinion polls"
)
out = moddf_tab[, c("mode", "X1", "X2", "X3")]
rownames(out) = NULL
out$X1 = paste0(round(100*out$X1, 1), "%")
out$X2 = paste0(round(100*out$X2, 1), "%")
out$X3 = paste0(round(100*out$X3, 1), "%")
names(out) = c("Modality", "Not at all useful", "Somewhat useful", "Very useful")
write.csv(out, "Figs_and_tabs/influence_tabs.csv", row.names = FALSE)



# perceptions of social media users ---------------------------------------

agree = "Agree"
tone_sum = expand.grid(mode = c("On social media", "In person"),
                       tone = c("Be well-informed", "Be rude or disrespectful", "Tell a personal story", "Cite studies and statistics"))
tone_sum$pct = NA_real_

tonedf = subset(dat,
                !is.na(tone_sm_wellinformed) | !is.na(tone_inperson_wellinformed) | 
                  !is.na(tone_sm_rude) | !is.na(tone_inperson_rude) |         
                  !is.na(tone_sm_personalstory) | !is.na(tone_inperson_personalstory) |
                  !is.na(tone_sm_citestats) | !is.na(tone_inperson_citestats))

tone_sum$pct[tone_sum$mode == "On social media" & tone_sum$tone == "Be well-informed"] = mean(tonedf$tone_sm_wellinformed == agree, na.rm=TRUE)
tone_sum$pct[tone_sum$mode == "On social media" & tone_sum$tone == "Be rude or disrespectful"] = mean(tonedf$tone_sm_rude == agree, na.rm=TRUE)
tone_sum$pct[tone_sum$mode == "On social media" & tone_sum$tone == "Tell a personal story"] = mean(tonedf$tone_sm_personalstory == agree, na.rm=TRUE)
tone_sum$pct[tone_sum$mode == "On social media" & tone_sum$tone == "Cite studies and statistics"] = mean(tonedf$tone_sm_citestats == agree, na.rm=TRUE)

tone_sum$pct[tone_sum$mode == "In person" & tone_sum$tone == "Be well-informed"] = mean(tonedf$tone_inperson_wellinformed == agree, na.rm=TRUE)
tone_sum$pct[tone_sum$mode == "In person" & tone_sum$tone == "Be rude or disrespectful"] = mean(tonedf$tone_inperson_rude == agree, na.rm=TRUE)
tone_sum$pct[tone_sum$mode == "In person" & tone_sum$tone == "Tell a personal story"] = mean(tonedf$tone_inperson_personalstory == agree, na.rm=TRUE)
tone_sum$pct[tone_sum$mode == "In person" & tone_sum$tone == "Cite studies and statistics"] = mean(tonedf$tone_inperson_citestats == agree, na.rm=TRUE)

tone_sum$tone = factor(tone_sum$tone, rev(c("Tell a personal story", "Cite studies and statistics","Be well-informed", "Be rude or disrespectful")))
tone_sum$mode = factor(tone_sum$mode, rev(c("On social media", "In person")))

ggplot(tone_sum) + 
  aes(x = tone, y = pct*100, fill = mode) + 
  geom_bar(stat="identity", position = position_dodge()) + 
  scale_fill_grey() + 
  guides(fill = guide_legend(title = NULL, rev = TRUE)) +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 100, 10)) + 
  theme(legend.position = "top") +
  labs(x = NULL, y = "Percent agreeing with descriptors of constituents")
ggsave("figs/perceptions_of_const_by_mode.pdf", width=6, height=4)




# barriers to SM use ------------------------------------------------------

barriers_from_sum = data.frame(type = c("Too many comments/posts\nto keep up with",
                                        "Not enough people use social media",
                                        "Can't tell if commenters\nlive in area",
                                        "Comments are too short to\ngive useful info",
                                        "Comments tend to be rude",
                                        "Not sure how to use social media\nto hear from constituents",
                                        "Don't have time to use social media"),
                               pct = NA_real_)

barriersdf = subset(dat, 
                    !is.na(barriers_hearfrom_toomany) | 
                      !is.na(barriers_hearfrom_notenough)  |
                      !is.na(barriers_hearfrom_canttellid) | 
                      !is.na(barriers_hearfrom_tooshort) | 
                      !is.na(barriers_hearfrom_rude) | 
                      !is.na(barriers_hearfrom_notsurehow) |
                      !is.na(barriers_hearfrom_notime) )



barriers_from_sum$pct[1] = mean(barriersdf$barriers_hearfrom_toomany == agree, na.rm = TRUE)
barriers_from_sum$pct[2] = mean(barriersdf$barriers_hearfrom_notenough == agree, na.rm = TRUE)
barriers_from_sum$pct[3] = mean(barriersdf$barriers_hearfrom_canttellid == agree, na.rm = TRUE)
barriers_from_sum$pct[4] = mean(barriersdf$barriers_hearfrom_tooshort == agree, na.rm = TRUE)
barriers_from_sum$pct[5] = mean(barriersdf$barriers_hearfrom_rude == agree, na.rm = TRUE)
barriers_from_sum$pct[6] = mean(barriersdf$barriers_hearfrom_notsurehow == agree, na.rm = TRUE)
barriers_from_sum$pct[7] = mean(barriersdf$barriers_hearfrom_notime == agree, na.rm = TRUE)

# re-order
barriers_from_sum = barriers_from_sum[order(barriers_from_sum$pct),]
barriers_from_sum$type = factor(barriers_from_sum$type,barriers_from_sum$type)


ggplot(barriers_from_sum) + 
  aes(x = type, y = pct*100) + 
  geom_bar(stat="identity") + 
  coord_flip(ylim = c(0, 45)) +
  scale_y_continuous(breaks = seq(0, 100, 10)) + 
  scale_fill_grey() + 
  theme(legend.position = "top") +
  labs(x = NULL, y = "Percent agreeing with statement")
ggsave("figs/barriers_hearfrom.pdf", width=6, height=4)


### CODE FOR FIGURE 4

# Extract relevant columns for the plot and filter out NA values
activity_df <- dat %>%
  select(const_pol_sm, const_pol_email, const_pol_inperson) %>%
  gather(key = "communication_mode", value = "political_activity") %>%
  filter(!is.na(political_activity)) %>%
  group_by(communication_mode, political_activity) %>%
  summarize(count = n()) %>%
  mutate(percent = count / sum(count) * 100)

# Relevel factors to ensure proper ordering in the plot
activity_df$political_activity <- factor(activity_df$political_activity, 
                                         levels = c("Less active", "About average", "More active"))

# Rename communication modes for better readability
activity_df$communication_mode <- factor(activity_df$communication_mode,
                                         levels = c("const_pol_sm", "const_pol_email", "const_pol_inperson"),
                                         labels = c("Social media", "Email", "In-person"))

# Plot the data
ggplot(activity_df, aes(x = political_activity, y = percent, fill = communication_mode)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_grey(start = 0.3, end = 0.7) +
  labs(x = NULL, y = "Percent of respondents") +
  theme(legend.position = "top", 
        legend.title = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1))


