# Table A22: Ethnic Terr. Claims Logit Models

m1 <- mdlr.conflict(dv="I(terr_claim_identity_a_onset)",
                    x= c("lost_capital","tek_capital_a1_b0", "lost_unity_only", "tek_capital_a1_b1"),
                    cntr=c("bal_terr",
                           "log(area_sqkm_cntr_b)",
                           "neighbors_ab",
                           "log(1+dist_km_ab)",
                           "year", "I(year^2)", "I(year^3)",
                           "time_since_terr_claim_identity_a", "I(time_since_terr_claim_identity_a^2)", "I(time_since_terr_claim_identity_a^3)"),
                    fe= c("0"),
                    model="logit",
                    dat=dyad.df[dyad.df$neighbors_ab%in%c(0,1) & dyad.df$year<2002,])
summary(m1,cluster=c("dyad_id_cow_directed","id_cap_rule_corr_a","id_cap_rule_corr_b"))


m2 <- mdlr.conflict(dv="I(terr_claim_identity_a_onset)",
                    x= c("lost_capital","tek_capital_a1_b0", "lost_unity_only", "tek_capital_a1_b1"),
                    cntr=c("bal_terr",
                           "log(area_sqkm_cntr_b)",
                           "neighbors_ab",
                           "log(1+dist_km_ab)",
                           "year", "I(year^2)", "I(year^3)",
                           "time_since_terr_claim_identity_a", "I(time_since_terr_claim_identity_a^2)", "I(time_since_terr_claim_identity_a^3)"),
                    fe= c("id_cap_rule_corr_a","id_cap_rule_corr_b"),
                    model="logit",
                    dat=dyad.df[dyad.df$neighbors_ab%in%c(0,1) & dyad.df$year<2002,])
summary(m2,cluster=c("dyad_id_cow_directed","id_cap_rule_corr_a","id_cap_rule_corr_b"))


m3 <- mdlr.conflict(dv="I(terr_claim_identity_a_onset)",
                    x= c("lost_capital","tek_capital_a1_b0", "lost_unity_only", "tek_capital_a1_b1"),
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
                           "log(1+dist_km_ab)",
                           "year", "I(year^2)", "I(year^3)",
                           "time_since_terr_claim_identity_a", "I(time_since_terr_claim_identity_a^2)", "I(time_since_terr_claim_identity_a^3)",
                           "time_in_current_border_a","I(time_in_current_border_a^2)","I(time_in_current_border_a^3)",
                           "time_in_current_border_b","I(time_in_current_border_b^2)","I(time_in_current_border_b^3)"),
                    fe= c("id_cap_rule_corr_a","id_cap_rule_corr_b"),
                    model="logit",
                    dat=dyad.df[dyad.df$neighbors_ab%in%c(1,0) & dyad.df$year<2002,])
summary(m3,cluster=c("dyad_id_cow_directed","id_cap_rule_corr_a","id_cap_rule_corr_b"))



m4 <- mdlr.conflict(dv="I(terr_claim_identity_a_onset)",
                    x= c("lost_capital_16","lost_capital_pre16","tek_capital_a1_b0","lost_unity_only_16", "lost_unity_only_pre16"),
                    cntr=c("bal_terr",
                           "log(area_sqkm_cntr_b)",
                           "log(area_sqkm_ag_alt_a)", 
                           "log(area_sqkm_ag_alt_b)", 
                           "terr_share_ag_alt_a",
                           "terr_share_ag_in_b",
                           "tek_capital_a1_b0",
                           "tek_capital_a1_b1",
                           "neighbors_ab",
                           "log(1+dist_km_ab)",
                           "year", "I(year^2)", "I(year^3)",
                           "time_since_terr_claim_identity_a", "I(time_since_terr_claim_identity_a^2)", "I(time_since_terr_claim_identity_a^3)",
                           "time_in_current_border_a","I(time_in_current_border_a^2)","I(time_in_current_border_a^3)",
                           "time_in_current_border_b","I(time_in_current_border_b^2)","I(time_in_current_border_b^3)"),
                    fe= c("id_cap_rule_corr_a","id_cap_rule_corr_b"),
                    model="logit",
                    dat=dyad.df[dyad.df$neighbors_ab%in%c(0,1) & dyad.df$year<2002,])
summary(m4,cluster=c("dyad_id_cow_directed","id_cap_rule_corr_a","id_cap_rule_corr_b"))

var.labs <- c("lost_capital" = "Lost home rule & lost unity",
              "lost_unity_only" = "Lost unity only",
              "lost_capital_16" = "Lost home rule & lost unity (post-1816)",
              "lost_capital_pre16" = "Lost home rule & lost unity (pre-1816)",
              "lost_unity_only_16" = "Lost unity only (post-1816)",
              "lost_unity_only_pre16" = "Lost unity only (pre-1816)",
              "id_cap_rule_corr_a" = "State A",
              "id_cap_rule_corr_b" = "State B",
              "dyad_id_cow_directed" = "Directed Dyad ID",
              "year" = "Year", "time_since_terr_claim_identity_a" = "Peace year",
              "time_in_current_border_a"="Border duration A",
              "time_in_current_border_b"="Border duration B",
              "I(terr_claim_identity_a_onset)"="Ethnic Terr. Claim Onset")


m.list <- summary(.l(list(m1,m2,m3,m4)), 
                  cluster=c("dyad_id_cow_directed","id_cap_rule_corr_a","id_cap_rule_corr_b"))

etable(m.list,tex=T, keep = c("lost","Lost"),
       order = c("lost","Lost"),
       dict = var.labs,
       title="Ethnic Territorial Claims: Logit Models",
       label="tab:tc_logit",
       style.tex = style.tex(main="base"), 
       file=file.path(tab.path,"table_A22.tex"),replace=T,
       extralines = list("-^Extended controls"=rep(c("","Yes"),each=2),
                         "-^Baseline controls"=rep("Yes",4)),
       signif.code = c("***"=0.001,"**"=0.01,"*"=0.05,"+"=0.1),
       fitstat = c("n"),
       notes = c("\\parbox[t]{\\width=\\textwidth}{\\textbf{Notes:} Logit estimates of territorial claim iniation (identity-based claims). 
       The unit of analysis is the directed country dyad year. 
       Baseline controls: relative territorial size of state A vs. state B; logged absolute size of country B; 
       indicators for whether governing group in A has governing or powerless kin segment in B;   
       cubic polynomials for peace and calendar years.  
       Extended controls: logged aggregate group size of governing segments in A and B;
       ethnic fractionalization of countries A and B;
       Share of aggregate group governing in state A located in state B;
       Share of aggregate group governing in state A located in own country;
       conflict history (number of past years with ongoing territorial claims involving A and B);
       time since last border change involving country A or B (cubic polynomials). 
       Standard errors clustered on dyad, state A, and state B in parentheses.
       Significance codes: ***: 0.001, **: 0.01, *: 0.05, +: 0.1}"))
