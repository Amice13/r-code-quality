## Table A2: MIDs, structural legacies

m1 <- mdlr.conflict(dv="I(100*mid_onset_initiator_dyad_fatal)",
                    x= c("lost_capital","tek_capital_a1_b0", "lost_unity_only_any", "tek_capital_a1_b1","capital_mean"),
                    cntr=c("bal_terr",
                           "log(area_sqkm_cntr_b)",
                           "neighbors_ab",
                           "log(1+dist_km_ab)"
                    ),
                    fe= c("year","time_since_mid_initiator_dyad_fatal"),
                    model="ols",
                    dat=dyad.df[dyad.df$neighbors_ab%in%c(0,1) & dyad.df$year<2015,])
summary(m1,cluster=c("dyad_id_cow_directed","id_cap_rule_corr_a","id_cap_rule_corr_b"))


m2 <- mdlr.conflict(dv="I(100*mid_onset_initiator_dyad_fatal)",
                    x= c("lost_capital","tek_capital_a1_b0", "lost_unity_only_any", "tek_capital_a1_b1","capital_mean"),
                    cntr=c("bal_terr",
                           "log(area_sqkm_cntr_b)",
                           "neighbors_ab",
                           "log(1+dist_km_ab)"
                    ),
                    fe= c("id_cap_rule_corr_a","id_cap_rule_corr_b","year","time_since_mid_initiator_dyad_fatal"),
                    model="ols",
                    dat=dyad.df[dyad.df$neighbors_ab%in%c(1,0) & dyad.df$year<2015,])
summary(m2,cluster=c("dyad_id_cow_directed","id_cap_rule_corr_a","id_cap_rule_corr_b"))


m3 <- mdlr.conflict(dv="I(100*mid_onset_initiator_dyad_fatal)",
                    x= c("lost_capital","tek_capital_a1_b0", "lost_unity_only_any", "tek_capital_a1_b1","capital_mean"),
                    cntr=c("bal_terr",
                           "log(area_sqkm_cntr_b)",
                           "log(area_sqkm_ag_alt_a)",
                           "log(area_sqkm_ag_alt_b)",
                           "terr_frac_cntr_alt_a",
                           "terr_frac_cntr_alt_b",
                           "terr_share_ag_alt_a",
                           "terr_share_ag_in_b",
                           "warhist_mid", 
                           "neighbors_ab",
                           "log(1+dist_km_ab)"
                    ),
                    fe= c("id_cap_rule_corr_a","id_cap_rule_corr_b","year","time_since_mid_initiator_dyad_fatal",
                          "time_in_current_border_a","time_in_current_border_b"),
                    model="ols",
                    dat=dyad.df[dyad.df$neighbors_ab%in%c(1,0) & dyad.df$year<2015,])
summary(m3,cluster=c("dyad_id_cow_directed","id_cap_rule_corr_a","id_cap_rule_corr_b"))


m4 <- mdlr.conflict(dv="I(100*mid_onset_initiator_dyad_fatal)",
                    x= c("lost_capital_16","lost_capital_pre16","tek_capital_a1_b0", "lost_unity_only_any_16","lost_unity_only_any_pre16", "tek_capital_a1_b1","capital_mean"),
                    cntr=c("bal_terr",
                           "log(area_sqkm_cntr_b)",
                           "log(area_sqkm_ag_alt_a)",
                           "log(area_sqkm_ag_alt_b)",
                           "terr_frac_cntr_alt_a",
                           "terr_frac_cntr_alt_b",
                           "terr_share_ag_alt_a",
                           "terr_share_ag_in_b",
                           "warhist_mid", 
                           "neighbors_ab",
                           "log(1+dist_km_ab)"
                    ),
                    fe= c("id_cap_rule_corr_a","id_cap_rule_corr_b","year",
                          "time_since_mid_initiator_dyad_fatal","time_in_current_border_a","time_in_current_border_b"),
                    model="ols",
                    dat=dyad.df[dyad.df$neighbors_ab%in%c(0,1) & dyad.df$year<2015,])
summary(m4,cluster=c("dyad_id_cow_directed","id_cap_rule_corr_a","id_cap_rule_corr_b"))


var.labs <- c("lost_capital" = "Lost home rule & lost unity",
              "lost_unity_only_any" = "Lost unity only",
              "lost_capital_16" = "Lost home rule & lost unity (post-1816)",
              "lost_capital_pre16" = "Lost home rule & lost unity (pre-1816)",
              "lost_unity_only_any_16" = "Lost unity only (post-1816)",
              "lost_unity_only_any_pre16" = "Lost unity only (pre-1816)",
              "capital_mean" = "Mean united home rule (1100 to $t-1$)",
              "id_cap_rule_corr_a" = "State A",
              "id_cap_rule_corr_b" = "State B",
              "dyad_id_cow_directed" = "Directed Dyad ID",
              "year" = "Year", "time_since_mid_initiator_dyad_fatal" = "Peace year",
              "time_in_current_border_a"="Border duration A",
              "time_in_current_border_b"="Border duration B",
              "I(100*mid_onset_initiator_dyad_fatal)"="Fatal MID onset $\\times 100$"
)


m.list <- summary(.l(list(m1,m2,m3,m4)), 
                  cluster=c("dyad_id_cow_directed","id_cap_rule_corr_a","id_cap_rule_corr_b"))

etable(m.list,tex=T, keep = c("lost","Lost","united"),
       order = c("lost","Lost","united"),
       dict = var.labs,
       title="Fatal MID Initiation: Structural Legacies",
       label="tab:mid_structural",
       style.tex = style.tex(main="base"), 
       file=file.path(tab.path,"table_A2.tex"),replace=T,
       extralines = list("-^Extended controls"=rep(c("","Yes"),each=2),
                         "-^Baseline controls"=rep("Yes",4)),
       signif.code = c("***"=0.001,"**"=0.01,"*"=0.05,"+"=0.1),
       fitstat = c("n"),
       notes = c("\\parbox[t]{\\width=\\textwidth}{\\textbf{Notes:} OLS estimates of fatal MID iniation. 
       The unit of analysis is the directed country dyad year. 
       Baseline controls: relative territorial size of state A vs. state B; logged absolute size of country B; 
       Share of aggregate group governing in state A located in state B;
       Share of aggregate group governing in state A located in own country;
       Dummies for peace and calendar years.  
       Extended controls: logged aggregate group size of governing segments in A and B;
       ethnic fractionalization of countries A and B;
       conflict history (number of past years with ongoing MIDs involving A and B);
       time since last border change involving country A or B (FE). 
       Standard errors clustered on dyad, state A, and state B in parentheses.
       Significance codes: ***: 0.001, **: 0.01, *: 0.05, +: 0.1}"))
