meps_votes = read_xlsx("MEPS_votes.xlsx")

meps_votes$count = 1

meps_votes_pgr = meps_votes %>% 
  group_by(pol_group) %>%
  summarise(final_vote = sum(final_vote, na.rm = T),
            amendment_vote = sum(amendment_vote, na.rm = T),
            sep18_vote_delete = sum(sep18_vote_delete, na.rm = T),
            sep18_vote_imco = sum(sep18_vote_imco, na.rm = T),
            sep18_vote_report = sum(sep18_vote_report, na.rm = T),
            sep18_vote_voss = sum(sep18_vote_voss, na.rm = T),
            count = sum(count, na.rm = T))

meps_votes_cnt = meps_votes %>% 
  group_by(country) %>%
  summarise(final_vote = sum(final_vote, na.rm = T),
            amendment_vote = sum(amendment_vote, na.rm = T),
            sep18_vote_delete = sum(sep18_vote_delete, na.rm = T),
            sep18_vote_imco = sum(sep18_vote_imco, na.rm = T),
            sep18_vote_report = sum(sep18_vote_report, na.rm = T),
            sep18_vote_voss = sum(sep18_vote_voss, na.rm = T),
            count = sum(count, na.rm = T))

meps_votes_cntpgr = meps_votes %>% 
  group_by(country, pol_group) %>%
  summarise(final_vote = sum(final_vote, na.rm = T),
            amendment_vote = sum(amendment_vote, na.rm = T),
            sep18_vote_delete = sum(sep18_vote_delete, na.rm = T),
            sep18_vote_imco = sum(sep18_vote_imco, na.rm = T),
            sep18_vote_report = sum(sep18_vote_report, na.rm = T),
            sep18_vote_voss = sum(sep18_vote_voss, na.rm = T),
            count = sum(count, na.rm = T))

meps_votes_pgr$final_perc = (meps_votes_pgr$final_vote/meps_votes_pgr$count)*100
meps_votes_cnt$final_perc = (meps_votes_cnt$final_vote/meps_votes_cnt$count)*100
meps_votes_cntpgr$final_perc = (meps_votes_cntpgr$final_vote/meps_votes_cntpgr$count)*100

meps_votes_pgr$country = "All"

meps_votes = rbind(meps_votes_pgr, meps_votes_cntpgr)

meps_votes_cntpgr = subset(meps_votes_cntpgr, country == "Austria" | country == "Belgium" | country == "France" | 
                          country == "Germany" | country == "Ireland" | country == "Italy" | country == "Netherlands" | 
                          country == "Poland" | country == "Portugal" | country == "Spain" |
                          country == "United Kingdom")

group_plot = function(polgroup, data_cnt, data_all){
  
  data_all = subset(data_all, data_all$pol_group == polgroup)
  data_cnt = subset(data_cnt, data_cnt$pol_group == polgroup)
  
  plot = ggplot(data_cnt, aes(x=final_perc, y=reorder(country, final_perc))) +
    geom_bar(stat = "identity", fill = "grey") +
    geom_vline(xintercept = data_all$final_perc, size = 3) +
    ggtitle(polgroup) +
    ylab(NULL) +
    xlab("Percentage of Votes in Favor of EU Copyright Directive") +
    theme_classic()
  
  return(plot)
  
}

table(meps_votes_cntpgr$pol_group)

plot_epp = group_plot(polgroup = "EPP", data_cnt = meps_votes_cntpgr, data_all = meps_votes_pgr)
plot_alde = group_plot(polgroup = "ALDE", data_cnt = meps_votes_cntpgr, data_all = meps_votes_pgr)
plot_ecr = group_plot(polgroup = "ECR", data_cnt = meps_votes_cntpgr, data_all = meps_votes_pgr)
plot_efdd = group_plot(polgroup = "EFDD", data_cnt = meps_votes_cntpgr, data_all = meps_votes_pgr)
plot_enf = group_plot(polgroup = "ENF", data_cnt = meps_votes_cntpgr, data_all = meps_votes_pgr)
plot_greens = group_plot(polgroup = "Greens/EFA", data_cnt = meps_votes_cntpgr, data_all = meps_votes_pgr)
plot_guengl = group_plot(polgroup = "GUE/NGL", data_cnt = meps_votes_cntpgr, data_all = meps_votes_pgr)
plot_na = group_plot(polgroup = "Non-attached", data_cnt = meps_votes_cntpgr, data_all = meps_votes_pgr)
plot_sd = group_plot(polgroup = "S&D", data_cnt = meps_votes_cntpgr, data_all = meps_votes_pgr)

plot_meps_vote = plot_grid(plot_epp, plot_sd, plot_alde, plot_enf, plot_greens, plot_guengl, plot_ecr, plot_efdd, plot_na,
                           ncol = 3)

ggsave("EUCD/MEPs_vote_final.png", plot_meps_vote, dpi = 640, width = 16, height = 10)
ggsave("EUCD/MEPs_vote_final.pdf", plot_meps_vote, dpi = 640, width = 16, height = 10)

plot_cnt_vote = ggplot(meps_votes_cnt, aes(x = final_perc, y = reorder(country, final_perc))) +
  geom_bar(stat = "identity", fill = "grey") +
  ggtitle(NULL) +
  ylab(NULL) +
  xlab("Percentage of Votes in Favor of EU Copyright Directive") +
  theme_classic()

ggsave("EUCD/MEPs_vote_final_country.png", plot_cnt_vote, dpi = 640, width = 10, height = 7)
ggsave("EUCD/MEPs_vote_final_country.pdf", plot_cnt_vote, dpi = 640, width = 10, height = 7)
