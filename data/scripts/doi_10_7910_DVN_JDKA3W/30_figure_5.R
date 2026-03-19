#### Prepare Figure 5: Effecvt Sizes of Basline Models for CW, Fatal MIDs, and Terr. Claims


###### CW
## re-estimate model 3 from table 1  
m.cw <- mdlr.conflict(dv="I(100*cw_onset_combined)",
                    x= c("lost_ga"),
                    cntr=c("bal_terr","tek_link","terr_maj_ag_23",
                           "log(area_sqkm_cntr)","log(area_sqkm_ag_corr)", "log(area_sqkm_corr)",
                           "terr_frac_ag_alt","terr_frac_cntr_alt",
                           "log(1+dist_capital_km)","war_hist_cw_yrs"),
                    fe= c("year","id_cap_rule_corr","time_since_cw_combined","time_in_current_border"),
                    model="ols",
                    dat=segments.df)
summary(m.cw,cluster=c("id_cap_rule_corr","ag_id"))


## Extract coefficient, standard error, and confidence interval limits for lost_ga
m.cw.sum <- summary(m.cw,cluster=c("id_cap_rule_corr","ag_id"))
plot.data.cw <- deltaMethod(m.cw,"lost_ga",vcov.=m.cw.sum$cov.scaled)
# get outcome mean in analysis sample to standardize coef
mu <- 100*mean(segments.df$cw_onset_combined[m.cw$obs_selection$obsRemoved])
plot.data.cw.rel <- plot.data.cw/mu


###### Fatal MIDs
## re-estimate model 3 from table 2  
m.mid <- mdlr.conflict(dv="I(100*mid_onset_initiator_dyad_fatal)",
                    x= c("lost_capital","tek_capital_a1_b0", "lost_unity_only_any", "tek_capital_a1_b1"),
                    cntr=c("bal_terr",
                           "warhist_mid", 
                           "log(area_sqkm_cntr_b)",
                           "log(area_sqkm_ag_alt_a)",
                           "log(area_sqkm_ag_alt_b)",
                           "terr_frac_cntr_alt_a",
                           "terr_frac_cntr_alt_b",
                           "terr_share_ag_alt_a",
                           "terr_share_ag_in_b",
                           "neighbors_ab",
                           "log(1+dist_km_ab)"
                    ),
                    fe= c("id_cap_rule_corr_a","id_cap_rule_corr_b","year","time_since_mid_initiator_dyad_fatal",
                          "time_in_current_border_a","time_in_current_border_b"),
                    model="ols",
                    dat=dyad.df[dyad.df$neighbors_ab%in%c(1,0) & dyad.df$year<2015,])
summary(m.mid,cluster=c("dyad_id_cow_directed","id_cap_rule_corr_a","id_cap_rule_corr_b"))


## Extract coefficient, standard error, and confidence interval limits for lost_capital
m.mid.sum <- summary(m.mid,cluster=c("dyad_id_cow_directed","id_cap_rule_corr_a","id_cap_rule_corr_b"))
plot.data.mid <- deltaMethod(m.mid,"lost_capital",vcov.=m.mid.sum$cov.scaled)
# get outcome mean in analysis sample to standardize coef
mu <- 100*mean(dyad.df$mid_onset_initiator_dyad_fatal[dyad.df$neighbors_ab%in%c(1,0) & dyad.df$year<2015])
plot.data.mid.rel <- plot.data.mid/mu





###### Terriitorial Claims
## re-estimate model 3 from table 3  
m.tc <- mdlr.conflict(dv="I(100*terr_claim_identity_a_onset)",
                    x= c("lost_capital","tek_capital_a1_b0", "lost_unity_only_any", "tek_capital_a1_b1"),
                    cntr=c("bal_terr",
                           "warhist_terr_claim", 
                           "log(area_sqkm_cntr_b)",
                           "log(area_sqkm_ag_alt_a)",
                           "log(area_sqkm_ag_alt_b)",
                           "terr_frac_cntr_alt_a",
                           "terr_frac_cntr_alt_b",
                           "terr_share_ag_alt_a",
                           "terr_share_ag_in_b",
                           "neighbors_ab",
                           "log(1+dist_km_ab)"),
                    fe= c("id_cap_rule_corr_a","id_cap_rule_corr_b","year","time_since_terr_claim_identity_a",
                          "time_in_current_border_a","time_in_current_border_b"),
                    model="ols",
                    dat=dyad.df[dyad.df$neighbors_ab%in%c(1,0) & dyad.df$year<2002,])
summary(m.tc,cluster=c("dyad_id_cow_directed","id_cap_rule_corr_a","id_cap_rule_corr_b"))

## Extract coefficient, standard error, and confidence interval limits for lost_capital
m.tc.sum <- summary(m.tc,cluster=c("dyad_id_cow_directed","id_cap_rule_corr_a","id_cap_rule_corr_b"))
plot.data.tc <- deltaMethod(m.tc,"lost_capital",vcov.=m.tc.sum$cov.scaled)
# get outcome mean in analysis sample to standardize coef
mu <- 100*mean(dyad.df$terr_claim_identity_a_onset[dyad.df$neighbors_ab%in%c(1,0) & dyad.df$year<2002])
plot.data.tc.rel <- plot.data.tc/mu


########## ########## ########## ########## ########## ########## ########## 
########## Prepare plot

## bind together coefs and confidence intervals
plot.data.rel <- as.data.frame(rbind(plot.data.cw.rel,plot.data.mid.rel,plot.data.tc.rel))
plot.data.rel$order <- c(0.3,0.2,0.1)
names(plot.data.rel) <- c("coef","se","lb","ub","order")

## define breaks and labels
brks <- c(0.3,0.2,0.1)
labs <- c("Ethnic Civil War", "Fatal MID", "Territorial Claims")

## make plot
p <- ggplot(plot.data.rel)
p <- p + geom_point(size=2.75,aes(x=coef,y=order), shape=19) + 
  geom_errorbarh(aes(y=order, xmin=lb, xmax=ub), linewidth=1, height=0.0) +
  geom_vline(xintercept=0,linetype="dotted", linewidth=0.6) +
  labs(title="Basline Results: Effect Sizes", 
       subtitle="Golden Age Loss and Conflict",
       y="Outcome",
       x="Estimate / Outcome Mean & 95% CI",
       caption="Error bars denote 95% confidence intervals.") +
  scale_y_continuous(breaks=brks,minor_breaks = NULL, 
                     labels=labs,
                     limits = c(0.05,0.35)) +
  theme_minimal(base_size=14) +
  theme(legend.position = "bottom")
p
ggsave(here(tab.path,"figure_5.pdf"),width=7,height=7)
