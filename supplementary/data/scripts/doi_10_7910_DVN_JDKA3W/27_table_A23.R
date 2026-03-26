##### run models with temporally disaggregated golden ages
m3 <- mdlr.conflict(dv="I(100*cw_onset_combined)",
                    x= c("lost_ga_16",
                         "lost_ga_1555_1790",
                         "lost_ga_1100_1550"),
                    cntr=c("bal_terr","tek_link","terr_maj_ag_23",
                           "log(area_sqkm_cntr)","log(area_sqkm_ag_corr)","log(area_sqkm_corr)",
                           "terr_frac_ag_alt","terr_frac_cntr_alt",
                           "log(1+dist_capital_km)","war_hist_cw_yrs"),
                    fe= c("id_cap_rule_corr","year","time_since_cw_combined","time_in_current_border"),
                    model="ols",
                    dat=segments.df)
summary(m3,cluster=c("id_cap_rule_corr","ag_id"))

m4 <- mdlr.conflict(dv="I(100*cw_onset_combined)",
                    x= c("lost_ga_01_10",
                         "lost_ga_11_50",
                         "lost_ga_51_100",
                         "lost_ga_101_200",
                         "lost_ga_201_400",
                         "lost_ga_401_plus"),
                    cntr=c("bal_terr","tek_link","terr_maj_ag_23",
                           "log(area_sqkm_cntr)","log(area_sqkm_ag_corr)","log(area_sqkm_corr)",
                           "terr_frac_ag_alt","terr_frac_cntr_alt",
                           "log(1+dist_capital_km)","war_hist_cw_yrs"),
                    fe= c("id_cap_rule_corr","year","time_since_cw_combined","time_in_current_border"),
                    model="ols",
                    dat=segments.df)
summary(m4,cluster=c("id_cap_rule_corr","ag_id"))

m3.terr <- mdlr.conflict(dv="I(100*cw_terr_onset_combined)",
                         x= c("lost_ga_16",
                              "lost_ga_1555_1790",
                              "lost_ga_1100_1550"),
                         cntr=c("bal_terr","tek_link","terr_maj_ag_23",
                                "log(area_sqkm_cntr)","log(area_sqkm_ag_corr)","log(area_sqkm_corr)",
                                "terr_frac_ag_alt","terr_frac_cntr_alt",
                                "log(1+dist_capital_km)","war_hist_cw_terr_yrs"),
                         fe= c("id_cap_rule_corr","year","time_since_cw_terr_combined","time_in_current_border"),
                         model="ols",
                         dat=segments.df)
summary(m3.terr,cluster=c("id_cap_rule_corr","ag_id"))

m4.terr <- mdlr.conflict(dv="I(100*cw_terr_onset_combined)",
                         x= c("lost_ga_01_10",
                              "lost_ga_11_50",
                              "lost_ga_51_100",
                              "lost_ga_101_200",
                              "lost_ga_201_400",
                              "lost_ga_401_plus"),
                         cntr=c("bal_terr","tek_link","terr_maj_ag_23",
                                "log(area_sqkm_cntr)","log(area_sqkm_ag_corr)","log(area_sqkm_corr)",
                                "terr_frac_ag_alt","terr_frac_cntr_alt",
                                "log(1+dist_capital_km)","war_hist_cw_terr_yrs"),
                         fe= c("id_cap_rule_corr","year","time_since_cw_terr_combined","time_in_current_border"),
                         model="ols",
                         dat=segments.df)
summary(m4.terr,cluster=c("id_cap_rule_corr","ag_id"))

m.list <- summary(.l(list(m3,m4,m3.terr,m4.terr)), 
                  cluster=c("id_cap_rule_corr","ag_id"))


var.labs <- c("lost_ga" = "Lost home rule or lost unity",
              "lost_ga_16" = "Lost home rule or lost unity (post-1816)",
              "lost_ga_pre16" = "Lost home rule or lost unity (pre-1816)",
              "log(1 + battle_count_dincecco)" = "No. battles (1000-1800, log)",
              "lost_capital_only" = "Lost home rule only",
              "lost_unity_only" = "Lost unity only",
              "lost_both" = "Lost home rule & lost unity",
              "id_cap_rule_corr" = "State","ag_id" = "Aggregate group",
              "year" = "Year", "time_since_cw_combined" = "Peace year",
              "time_in_current_border"="Border duration",
              "I(100*cw_onset_combined)"="CW onset $\\times 100$",
              "I(100*cw_terr_onset_combined)"="Terr. CW onset $\\times 100$",
              "lost_ga_1555_1790" = "Lost home rule or lost unity (1555-1790)",
              "lost_ga_1100_1550" = "Lost home rule or lost unity (1100-1550)",
              "lost_ga_01_10" = "Lost home rule or lost unity ($t\\_{-1}-t\\_{-10}$)",
              "lost_ga_11_50" = "Lost home rule or lost unity ($t\\_{-11}-t\\_{-50}$)",
              "lost_ga_51_100" = "Lost home rule or lost unity ($t\\_{-51}-t\\_{-100}$)",
              "lost_ga_101_200" = "Lost home rule or lost unity ($t\\_{-101}-t\\_{-200}$)",
              "lost_ga_201_400" = "Lost home rule or lost unity ($t\\_{-201}-t\\_{-400}$)",
              "lost_ga_401_plus" = "Lost home rule or lost unity ($t\\_{-401}-t\\_{-917}$)"
)



etable(m.list,tex=T, keep = c("lost","Lost"),
       dict = var.labs,
       title="Civil War Onset: Temporal Depth of Golden Ages",
       label="tab:cw_ga_depth",
       style.tex = style.tex(main="base"), 
       file=file.path(tab.path,"table_A23.tex"),replace=T,
       extralines = list("-_Baseline controls"=rep("Yes",4),
                         "-_Extended controls"=rep(c("Yes","Yes","Yes","Yes","Yes"),1)),
       signif.code = c("***"=0.001,"**"=0.01,"*"=0.05,"+"=0.1),
       fitstat = c("n"),
       notes = c("\\parbox[t]{\\width=\\textwidth}{\\textbf{Notes:} OLS estimates of Civil War Onsets. The unit of analysis is the ethnic segment year. 
       Baseline controls: segment area relative to state-leading group, ethnic division dummy, national unity dummy. 
       Extended controls: logged country, aggregate group, and segment size in sqkm; ethnic fractionalization of country and aggregate group;
       logged distance to capital; war history (past years with ongoing civil war);
       time since last border change (FE). Standard errors clustered on country and aggregate ethnic group in parentheses.
       Significance codes: ***: 0.001, **: 0.01, *: 0.05, +: 0.1}"))
