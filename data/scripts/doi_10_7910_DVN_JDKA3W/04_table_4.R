#### Table 4: Golden Ages & Interstate Conflict: Timing


###### Fatal MIDs: golden age times systemic instability
m1 <- mdlr.conflict(dv="I(100*mid_onset_initiator_dyad_fatal)",
                    x= c("lost_capital*dw_pca_high","tek_capital_a1_b0", "lost_unity_only_any*dw_pca_high", "tek_capital_a1_b1"),
                    cntr=c("bal_terr",
                           "log(area_sqkm_cntr_b)",
                           "log(area_sqkm_ag_alt_a)", 
                           "log(area_sqkm_ag_alt_b)", 
                           "terr_share_ag_alt_a",
                           "terr_share_ag_in_b",
                           "terr_frac_cntr_alt_a",
                           "terr_frac_cntr_alt_b",
                           "neighbors_ab",
                           "log(1+dist_km_ab)",
                           "warhist_mid"),
                    fe= c("id_cap_rule_corr_a","id_cap_rule_corr_b","year","time_since_mid_initiator_dyad_fatal",
                          "time_in_current_border_a","time_in_current_border_b"),
                    model="ols",
                    dat=dyad.df[dyad.df$neighbors_ab%in%c(1,0) & dyad.df$year<2002 & dyad.df$major_power_a==0 & dyad.df$major_power_b==0,])
summary(m1,cluster=c("dyad_id_cow_directed","id_cap_rule_corr_a","id_cap_rule_corr_b"))

# compute relevant marginal effects and p values to manually add to table
sum.1 <- summary(m1,cluster=c("dyad_id_cow_directed","id_cap_rule_corr_a","id_cap_rule_corr_b"))
m1.mfx.1 <- deltaMethod(m1,"lost_capital + `lost_capital:dw_pca_high`",vcov. = sum.1$cov.scaled,level=0.95)
m1.mfx.1
m1.mfx.1.p <- 2*(1-pnorm(m1.mfx.1$Estimate/m1.mfx.1$SE))
m1.mfx.1.p


m1.mfx.2 <- deltaMethod(m1,"lost_unity_only_any + `dw_pca_high:lost_unity_only_any`",vcov. = sum.1$cov.scaled,level=0.95)
m1.mfx.2
m1.mfx.2.p <- 2*(1-pnorm(m1.mfx.2$Estimate/m1.mfx.2$SE))
m1.mfx.2.p


###### MIDs: golden age times national ideology
m2 <- mdlr.conflict(dv="I(100*mid_onset_initiator_dyad_fatal)",
                    x= c("lost_capital*national_ideol_gov_a_vdem","tek_capital_a1_b0", "lost_unity_only_any*national_ideol_gov_a_vdem", "tek_capital_a1_b1"),
                    cntr=c("bal_terr",
                           "log(area_sqkm_cntr_b)",
                           "log(area_sqkm_ag_alt_a)", 
                           "log(area_sqkm_ag_alt_b)", 
                           "terr_share_ag_alt_a",
                           "terr_share_ag_in_b",
                           "terr_frac_cntr_alt_a",
                           "terr_frac_cntr_alt_b",
                           "neighbors_ab",
                           "log(1+dist_km_ab)",
                           "warhist_mid"),
                    fe= c("id_cap_rule_corr_a","id_cap_rule_corr_b","year","time_since_mid_initiator_dyad_fatal",
                          "time_in_current_border_a","time_in_current_border_b"),
                    model="ols",
                    dat=dyad.df[dyad.df$neighbors_ab%in%c(1,0) & dyad.df$year<2015,])
summary(m2,cluster=c("dyad_id_cow_directed","id_cap_rule_corr_a","id_cap_rule_corr_b"))

sum.2 <- summary(m2,cluster=c("dyad_id_cow_directed","id_cap_rule_corr_a","id_cap_rule_corr_b"))
m2.mfx.1 <- deltaMethod(m2,"lost_capital + `lost_capital:national_ideol_gov_a_vdem`",vcov. = sum.2$cov.scaled,level=0.95)
m2.mfx.1
m2.mfx.1.p <- 2*(1-pnorm(m2.mfx.1$Estimate/m2.mfx.1$SE))
m2.mfx.1.p


m2.mfx.2 <- deltaMethod(m2,"lost_unity_only_any + `national_ideol_gov_a_vdem:lost_unity_only_any`",vcov. = sum.2$cov.scaled,level=0.95)
m2.mfx.2
m2.mfx.2.p <- 2*(1-pnorm(m2.mfx.2$Estimate/m2.mfx.2$SE))
m2.mfx.2.p


###### terr claims: golden age times systemic instability
m3 <- mdlr.conflict(dv="I(100*terr_claim_identity_a_onset)",
                    x= c("lost_capital*dw_pca_high","tek_capital_a1_b0", "lost_unity_only_any*dw_pca_high", "tek_capital_a1_b1"),
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
                    dat=dyad.df[dyad.df$neighbors_ab%in%c(1,0) & dyad.df$year<2002 & dyad.df$major_power_a==0 & dyad.df$major_power_b==0,])
summary(m3,cluster=c("dyad_id_cow_directed","id_cap_rule_corr_a","id_cap_rule_corr_b"))

# compute relevant marginal effects and p values to manually add to table
sum.3 <- summary(m3,cluster=c("dyad_id_cow_directed","id_cap_rule_corr_a","id_cap_rule_corr_b"))
m3.mfx.1 <- deltaMethod(m3,"lost_capital + `lost_capital:dw_pca_high`",vcov. = sum.3$cov.scaled,level=0.95)
m3.mfx.1
m3.mfx.1.p <- 2*(1-pnorm(m3.mfx.1$Estimate/m3.mfx.1$SE))
m3.mfx.1.p


m3.mfx.2 <- deltaMethod(m3,"lost_unity_only_any + `dw_pca_high:lost_unity_only_any`",vcov. = sum.3$cov.scaled,level=0.95)
m3.mfx.2
m3.mfx.2.p <- 2*(1-pnorm(m3.mfx.2$Estimate/m3.mfx.2$SE))
m3.mfx.2.p






###### terr claims: golden age times national ideology
m4 <- mdlr.conflict(dv="I(100*terr_claim_identity_a_onset)",
                    x= c("lost_capital*national_ideol_gov_a_vdem","tek_capital_a1_b0", "lost_unity_only_any*national_ideol_gov_a_vdem", "tek_capital_a1_b1"),
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
summary(m4,cluster=c("dyad_id_cow_directed","id_cap_rule_corr_a","id_cap_rule_corr_b"))

sum.4 <- summary(m4,cluster=c("dyad_id_cow_directed","id_cap_rule_corr_a","id_cap_rule_corr_b"))
m4.mfx.1 <- deltaMethod(m4,"lost_capital + `lost_capital:national_ideol_gov_a_vdem`",vcov. = sum.4$cov.scaled,level=0.95)
m4.mfx.1
m4.mfx.1.p <- 2*(1-pnorm(m4.mfx.1$Estimate/m4.mfx.1$SE))
m4.mfx.1.p


m4.mfx.2 <- deltaMethod(m4,"lost_unity_only_any + `national_ideol_gov_a_vdem:lost_unity_only_any`",vcov. = sum.4$cov.scaled,level=0.95)
m4.mfx.2
m4.mfx.2.p <- 2*(1-pnorm(m4.mfx.2$Estimate/m4.mfx.2$SE))
m4.mfx.2.p



var.labs <- c("lost_capital" = "Lost home rule & lost unity (LHR & LU)",
              "lost_unity_only_any" = "Lost unity only (LUO)",
              "national_ideol_gov_a_vdem" = "Nationalist government",
              "dw_pca_high" = "Regional instability",
              #"`lost_capital:dw_pca_high`" = "LHR & LU $\\times$ Reg. instab.",
              #"`lost_capital:national_ideol_gov_a_vdem`" = "LHR & LU $\\times$ Nat. gov.",
              #"`dw_pca_high:lost_unity_only_any`" = "LUO $\\times$ Reg. instab.",
              #"`national_ideol_gov_a_vdem:lost_unity_only_any`" = "LUO $\\times$ Nat. gov.",
              "bal_terr" = "Territorial balance (A vs. B)",
              "log(area_sqkm_cntr_b)" = "Area state B (sqkm, log)",
              "log(area_sqkm_ag_alt_a)" = "Area leading aggr. group A (sqkm, log)",
              "log(area_sqkm_ag_alt_b)" = "Area leading aggr. group B (sqkm, log)",
              "terr_share_ag_alt_a" = "Share of A's leading AG territory in A",
              "terr_share_ag_in_b" = "Share of A's leading AG territory in B",
              "terr_frac_cntr_alt_a" = "Territorial fractionalization state A",
              "terr_frac_cntr_alt_b" = "Territorial fractionalization state B",
              "neighbors_ab" = "Neighboring dyad (y/n)",
              "log(1+dist_km_ab)" = "Minimum distance A to B (km, log)",
              "warhist_mid" = "MID history (1816 to $t-1$)",
              "warhist_terr_claim" = "Terr. claim history (1816 to $t-1$)",
              "id_cap_rule_corr_a" = "State A",
              "id_cap_rule_corr_b" = "State B",
              "dyad_id_cow_directed" = "Directed Dyad ID",
              "year" = "Year", "time_since_mid_initiator_dyad_fatal" = "Peace years (MID)",
              "time_since_terr_claim_identity_a" = "Peace years (TC)",
              "time_in_current_border_a"="Border duration A",
              "time_in_current_border_b"="Border duration B",
              "I(100*mid_onset_initiator_dyad_fatal)"="MID $\\times 100$",
              "I(100*terr_claim_identity_a_onset)"="TC $\\times 100$"
)


m.list <- summary(.l(list(m1,m2,m3,m4)), 
                  cluster=c("dyad_id_cow_directed","id_cap_rule_corr_a","id_cap_rule_corr_b"))

etable(m.list,tex=T, order = c("lost","Lost","Regional","National","LHR","LUO"),
       keep = c("lost","Lost","Regional","National","LHR","LUO"),
       dict = var.labs,
       title="Golden Ages & Interstate Conflict: Timing",
       label="tab:mid_tc_ia_ext",
       style.tex = style.tex(main="base"), 
       file=file.path(tab.path,"table_4.tex"),replace=T,
       extralines = list("-^Extended controls"=rep("Yes",4),
                         "-^Baseline controls"=rep("Yes",4)),
       signif.code = c("***"=0.001,"**"=0.01,"*"=0.05,"+"=0.1),
       fitstat = c("n"),
       notes = c("\\parbox[t]{\\width=\\textwidth}{\\textbf{Notes:} OLS estimates of fatal MID iniation (Columns 1 and 2) 
       and territorial claim onset (Columns 3 and 4). 
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






