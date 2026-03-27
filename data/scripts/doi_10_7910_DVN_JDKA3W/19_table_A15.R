##### run models on data from first ethnic map

m1 <- mdlr.conflict(dv="I(100*cw_onset_combined)",
                    x= c("lost_ga"),
                    cntr=c("bal_terr","tek_link","terr_maj_ag_23"),
                    fe= c("year","time_since_cw_combined"),
                    model="ols",
                    dat=segments.df.first
)
summary(m1,cluster=c("id_cap_rule_corr","ag_id"))


m2 <- mdlr.conflict(dv="I(100*cw_onset_combined)",
                    x= c("lost_ga"),
                    cntr=c("bal_terr","tek_link","terr_maj_ag_23"),
                    fe= c("year","id_cap_rule_corr","time_since_cw_combined"),
                    model="ols",
                    dat=segments.df.first
)
summary(m2,cluster=c("id_cap_rule_corr","ag_id"))


m3 <- mdlr.conflict(dv="I(100*cw_onset_combined)",
                    x= c("lost_ga"),
                    cntr=c("bal_terr","tek_link","terr_maj_ag_23",
                           "log(area_sqkm_cntr)","log(area_sqkm_ag_corr)","log(area_sqkm_corr)",
                           "terr_frac_ag_alt","terr_frac_cntr_alt",
                           "log(1+dist_capital_km)","war_hist_cw_yrs"),
                    fe= c("year","id_cap_rule_corr","time_since_cw_combined","time_in_current_border"),
                    model="ols",
                    dat=segments.df.first)
summary(m3,cluster=c("id_cap_rule_corr","ag_id"))


m4 <- mdlr.conflict(dv="I(100*cw_onset_combined)",
                    x = c("lost_capital_only","lost_unity_only","lost_both"),
                    cntr=c("bal_terr","tek_link","terr_maj_ag_23",
                           "log(area_sqkm_cntr)","log(area_sqkm_ag_corr)","log(area_sqkm_corr)",
                           "terr_frac_ag_alt","terr_frac_cntr_alt",
                           "log(1+dist_capital_km)","war_hist_cw_yrs"),
                    fe= c("year","id_cap_rule_corr","time_since_cw_combined","time_in_current_border"),
                    model="ols",
                    dat=segments.df.first)
summary(m4,cluster=c("id_cap_rule_corr","ag_id"))


m5 <- mdlr.conflict(dv="I(100*cw_onset_combined)",
                    x= c("lost_ga_16",
                         "lost_ga_pre16"),
                    cntr=c("bal_terr","tek_link","terr_maj_ag_23",
                           "log(area_sqkm_cntr)","log(area_sqkm_ag_corr)","log(area_sqkm_corr)",
                           "terr_frac_ag_alt","terr_frac_cntr_alt",
                           "log(1+dist_capital_km)","war_hist_cw_yrs"),
                    fe= c("year","id_cap_rule_corr","time_since_cw_combined","time_in_current_border"),
                    model="ols",
                    dat=segments.df.first)
summary(m5,cluster=c("id_cap_rule_corr","ag_id"))



m.list <- summary(.l(list(m1,m2,m3,m4,m5)), 
                  cluster=c("id_cap_rule_corr","ag_id"))


var.labs <- c("lost_ga" = "Lost home rule or lost unity",
              "lost_ga_16" = "Lost home rule or lost unity (post-1816)",
              "lost_ga_pre16" = "Lost home rule or lost unity (pre-1816)",
              "lost_capital_only" = "Lost home rule only",
              "lost_unity_only" = "Lost unity only",
              "lost_both" = "Lost home rule & lost unity",
              "id_cap_rule_corr" = "State","ag_id" = "Aggregate group",
              "year" = "Year", "time_since_cw_combined" = "Peace year",
              "time_in_current_border"="Border duration",
              "I(100*cw_onset_combined)"="Ethnic civil war onset $\\times 100$"
)



etable(m.list,tex=T, keep = c("lost","Lost"),
       dict = var.labs,
       title="Civil War Onset: Earliest Ethnic Maps",
       label="tab:cw_first_map",
       style.tex = style.tex(main="base"), 
       file=file.path(tab.path,"table_A15.tex"),replace=T,
       extralines = list("-_Baseline controls"=rep("Yes",5),
                         "-_Extended controls"=rep(c("","Yes","Yes","Yes","Yes"),1)),
       signif.code = c("***"=0.001,"**"=0.01,"*"=0.05,"+"=0.1),
       fitstat = c("n"),
       notes = c("\\parbox[t]{\\width=\\textwidth}{\\textbf{Notes:} OLS estimates of Civil War Onsets. The unit of analysis is the ethnic segment year. 
       Baseline controls: segment area relative to state-leading group, transborder ethnic kin dummy, national unity dummy. 
       Extended controls: logged country and aggregate group size; ethnic fractionalization of country and aggregate group;
       logged distance to capital; war history (past years with ongoing civil war);
       time since last border change (FE). Standard errors clustered on country and aggregate ethnic group in parentheses.
       Significance codes: ***: 0.001, **: 0.01, *: 0.05, +: 0.1}"))
