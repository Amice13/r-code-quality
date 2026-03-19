# Table A12: MIDs: Population-based controls
m1 <- mdlr.conflict(dv="I(100*mid_onset_initiator_dyad_fatal)",
                    x= c("lost_capital","tek_capital_a1_b0", "lost_unity_only_any", "tek_capital_a1_b1"),
                    cntr=c("bal_pop",
                           "log(popc_cntr_b)",
                           "neighbors_ab",
                           "log(1+dist_km_ab)"),
                    fe= c("year","time_since_mid_initiator_dyad_fatal"),
                    model="ols",
                    dat=dyad.df[dyad.df$neighbors_ab%in%c(1,0) & dyad.df$year<2002,])
summary(m1,cluster=c("dyad_id_cow_directed","id_cap_rule_corr_a","id_cap_rule_corr_b"))


m2 <- mdlr.conflict(dv="I(100*mid_onset_initiator_dyad_fatal)",
                    x= c("lost_capital","tek_capital_a1_b0", "lost_unity_only_any", "tek_capital_a1_b1"),
                    cntr=c("bal_pop",
                           "warhist_mid", 
                           "log(popc_cntr_b)",
                           "log(popc_ag_alt_a)",
                           "log(popc_ag_alt_b)",
                           "pop_frac_cntr_alt_a",
                           "pop_frac_cntr_alt_b",
                           "pop_share_ag_alt_a",
                           "pop_share_ag_in_b",
                           "neighbors_ab",
                           "log(1+dist_km_ab)"),
                    fe= c("id_cap_rule_corr_a","id_cap_rule_corr_b","year","time_since_mid_initiator_dyad_fatal",
                          "time_in_current_border_a","time_in_current_border_b"),
                    model="ols",
                    dat=dyad.df[dyad.df$neighbors_ab%in%c(1,0) & dyad.df$year<2002,])
summary(m2,cluster=c("dyad_id_cow_directed","id_cap_rule_corr_a","id_cap_rule_corr_b"))





m3 <- mdlr.conflict(dv="I(100*terr_claim_identity_a_onset)",
                    x= c("lost_capital","tek_capital_a1_b0", "lost_unity_only_any", "tek_capital_a1_b1"),
                    cntr=c("bal_pop",
                           "log(popc_cntr_b)",
                           "neighbors_ab",
                           "log(1+dist_km_ab)"),
                    fe= c("year","time_since_terr_claim_identity_a"),
                    model="ols",
                    dat=dyad.df[dyad.df$neighbors_ab%in%c(1,0) & dyad.df$year<2002,])
summary(m3,cluster=c("dyad_id_cow_directed","id_cap_rule_corr_a","id_cap_rule_corr_b"))


m4 <- mdlr.conflict(dv="I(100*terr_claim_identity_a_onset)",
                    x= c("lost_capital","tek_capital_a1_b0", "lost_unity_only_any", "tek_capital_a1_b1"),
                    cntr=c("bal_pop",
                           "warhist_mid", 
                           "log(popc_cntr_b)",
                           "log(popc_ag_alt_a)",
                           "log(popc_ag_alt_b)",
                           "pop_frac_cntr_alt_a",
                           "pop_frac_cntr_alt_b",
                           "pop_share_ag_alt_a",
                           "pop_share_ag_in_b",
                           "neighbors_ab",
                           "log(1+dist_km_ab)"),
                    fe= c("id_cap_rule_corr_a","id_cap_rule_corr_b","year","time_since_terr_claim_identity_a",
                          "time_in_current_border_a","time_in_current_border_b"),
                    model="ols",
                    dat=dyad.df[dyad.df$neighbors_ab%in%c(1,0) & dyad.df$year<2002,])
summary(m4,cluster=c("dyad_id_cow_directed","id_cap_rule_corr_a","id_cap_rule_corr_b"))




var.labs <- c("lost_capital" = "Lost home rule & lost unity",
              "lost_unity_only_any" = "Lost unity only",
              "id_cap_rule_corr_a" = "State A FE",
              "id_cap_rule_corr_b" = "State B FE",
              "dyad_id_cow_directed" = "Directed Dyad ID",
              "year" = "Year", "time_since_mid_initiator_dyad_fatal" = "Peace year FE (MID)",
              "time_since_terr_claim_identity_a" = "Peace year FE (TC)",
              "time_in_current_border_a"="Border duration A FE",
              "time_in_current_border_b"="Border duration B FE",
              "I(100*mid_onset_initiator_dyad_fatal)"="MID $\\times 100$",
              "I(100*terr_claim_identity_a_onset)"="TC $\\times 100$"
)


m.list <- summary(.l(list(m1,m2,m3,m4)), 
                  cluster=c("dyad_id_cow_directed","id_cap_rule_corr_a","id_cap_rule_corr_b"))

etable(m.list,tex=T, keep = c("lost","Lost","Regional","National"),
       dict = var.labs,
       title="Interstate Conflict: Population-based Controls",
       label="tab:mid_tc_pop",
       style.tex = style.tex(main="base"), 
       file=file.path(tab.path,"table_A12.tex"),replace=T,
       extralines = list("-^Extended controls"=rep(c("","Yes"),2),
                         "-^Baseline controls"=rep("Yes",4)),
       signif.code = c("***"=0.001,"**"=0.01,"*"=0.05,"+"=0.1),
       fitstat = c("n"),
       notes = c("\\parbox[t]{\\width=\\textwidth}{\\textbf{Notes:} OLS estimates of fatal MID iniation (Columns 1 and 2) 
       and ethnic territorial claim onset (Columns 3 and 4). 
       The unit of analysis is the directed country dyad year. 
       Baseline controls: relative population size of state A vs. state B; logged absolute population of country B; 
       indicators for whether governing group in A has governing or powerless kin segment in B;   
       dummies for peace and calendar years.  
       Extended controls: logged aggregate group population of governing segments in A and B;
       ethnic fractionalization of countries A and B;
       pop. share of aggregate group governing in state A located in state B;
       pop. share of aggregate group governing in state A located in own country;
       conflict history (number of past years with ongoing MIDs involving A and B);
       time since last border change involving country A or B (FE). 
       Standard errors clustered on dyad, state A, and state B in parentheses.
       Significance codes: ***: 0.001, **: 0.01, *: 0.05, +: 0.1}"))
