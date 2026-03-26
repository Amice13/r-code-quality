### Table A25: cross-sectional models.

## prepare segments cross section
first.years <- segments.df%>%group_by(seg_id)%>%mutate(
  year_first = min(year)
)
first.years <- first.years[first.years$year_first==first.years$year,]
first.years$lost_ga_any <- ifelse(first.years$lost_ga==1,1,0)

segments.cs <- segments.df%>%group_by(seg_id,ag_id,id_cap_rule_corr)%>%summarise(
  cw_onset_combined_mean = mean(cw_onset_combined,na.rm=T)
)

segments.cs <- left_join(segments.cs, first.years[,c("seg_id","ag_id","id_cap_rule_corr","lost_ga_any")])

#### Estimate CW models on CS data
clust.vars.cw <- c("id_cap_rule_corr","ag_id")

m1 <- mdlr.conflict(dv="I(100*cw_onset_combined_mean)",
                    x= c("lost_ga_any"),
                    cntr=c("0"),
                    #fe= c("0"),
                    model="ols",
                    dat=segments.cs
)
summary(m1,cluster=clust.vars.cw)


m2 <- mdlr.conflict(dv="I(100*cw_onset_combined_mean)",
                    x= c("lost_ga_any"),
                    cntr=c("0"),
                    fe= c("id_cap_rule_corr"),
                    model="ols",
                    dat=segments.cs
)
summary(m2,cluster=clust.vars.cw)



## prepare dyads cross section
first.years <- dyad.df%>%group_by(dyad_id_cow_directed)%>%mutate(
  year_first = min(year)
)
first.years <- first.years[first.years$year_first==first.years$year,]
first.years$lost_capital_any <- ifelse(first.years$lost_capital==1,1,0)

dyads.cs <- dyad.df%>%group_by(dyad_id_cow_directed,id_cap_rule_corr_a,id_cap_rule_corr_b)%>%summarise(
  mid_onset_initiator_dyad_fatal_mean = mean(mid_onset_initiator_dyad_fatal,na.rm=T),
  terr_claim_identity_a_onset_mean = mean(terr_claim_identity_a_onset,na.rm=T),
  neighbors_ab_mean=mean(neighbors_ab)
)

dyads.cs <- left_join(dyads.cs, first.years[,c("dyad_id_cow_directed","id_cap_rule_corr_a","id_cap_rule_corr_b","lost_capital_any")])

clust.vars <- c("dyad_id_cow_directed","id_cap_rule_corr_a","id_cap_rule_corr_b")

m1.dyad.tc <- mdlr.conflict(dv="I(100*terr_claim_identity_a_onset_mean)",
                            x= c("lost_capital_any"),
                            cntr=c("neighbors_ab_mean"),
                            fe= c("0"),
                            model="ols",
                            dat=dyads.cs)
summary(m1.dyad.tc,cluster=clust.vars)

m1.dyad.tc.fe <- mdlr.conflict(dv="I(100*terr_claim_identity_a_onset_mean)",
                               x= c("lost_capital_any"),
                               cntr=c("neighbors_ab_mean"),
                               fe= c("id_cap_rule_corr_a","id_cap_rule_corr_b"),
                               model="ols",
                               dat=dyads.cs)
summary(m1.dyad.tc.fe,cluster=clust.vars)



m1.dyad.mid <- mdlr.conflict(dv="I(100*mid_onset_initiator_dyad_fatal_mean)",
                             x= c("lost_capital_any"),
                             cntr=c("neighbors_ab_mean"),
                             fe= c("0"),
                             model="ols",
                             dat=dyads.cs)
summary(m1.dyad.mid,cluster=clust.vars)

m1.dyad.mid.fe <- mdlr.conflict(dv="I(100*mid_onset_initiator_dyad_fatal_mean)",
                                x= c("lost_capital_any"),
                                cntr=c("neighbors_ab_mean"),
                                fe= c("id_cap_rule_corr_a","id_cap_rule_corr_b"),
                                model="ols",
                                dat=dyads.cs)
summary(m1.dyad.mid.fe,cluster=clust.vars)


##### prepare table
m.list.cw <- summary(.l(list(m1,m2)), 
                     cluster=clust.vars.cw)
m.list.dyads <- summary(.l(list(m1.dyad.mid, m1.dyad.mid.fe, m1.dyad.tc,m1.dyad.tc.fe)), 
                        cluster=clust.vars)

m.list <- c(m.list.cw,m.list.dyads)


var.labs <- c("lost_ga_any" = "Lost home rule or lost unity",
              "lost_capital_any" = "Lost home rule and lost unity",
              "id_cap_rule_corr" = "State","ag_id" = "Aggregate group",
              "year" = "Year", "time_since_cw_combined" = "Peace year",
              "id_cap_rule_corr_a" = "State A",
              "id_cap_rule_corr_b" = "State B",
              "time_in_current_border"="Border duration",
              "dyad_id_cow_directed"="Dyad ID",
              "I(100*cw_onset_combined_max)"="CW onset (Y/N) $\\times 100$",
              "I(100*cw_onset_combined_mean)"="Avg. CW onset $\\times 100$",
              "I(100*mid_onset_initiator_dyad_fatal_mean)"="Avg. MID onset $\\times 100$",
              "I(100*terr_claim_identity_a_onset_mean)"="Avg. TC onset $\\times 100$"
              
              
)



etable(m.list,tex=T, keep = c("lost","Lost"),
       dict = var.labs,
       title="Conflict Onset: Cross-Sectional Analysis",
       label="tab:all_cs",
       style.tex = style.tex(main="base"), 
       file=file.path(tab.path,"table_A25.tex"),replace=T,
       extralines = list("-_Baseline controls"==rep(c("No","No","No","No","No","No","No"),1),
                         "-_Extended controls"=rep(c("No","No","No","No","No","No","No"),1)),
       signif.code = c("***"=0.001,"**"=0.01,"*"=0.05,"+"=0.1),
       fitstat = c("n"),
       notes = c("\\parbox[t]{\\width=\\textwidth}{\\textbf{Notes:} OLS estimates of mean conflict onset per segment/dyad. 
       The sample is a cross-section of all ethnic segments/dyads observed for at least one year in the period between 1816 and 2017.
       The outcome in Columns 1 and 2 captures the share of years under observation in which a segment experienced a civil war onset.
       The outcome in Columns 3 and 4 measures the share of years under observation in which State A initiated a fatal MID against State B.
       The outcome in Columns 5 and 6 measures the share of years under observation in which State A initiated an identity-related territorial claim against State B.
       The main explanatory variable indicates whether the segment/dyad in question has experienced a golden age loss prior to the first year it is observed in our sample.
       Standard errors clustered on country and aggregate ethnic group (Columns 1 and 2) or in dyad, State A and State B (Columns 3-6) in parentheses.
       Significance codes: ***: 0.001, **: 0.01, *: 0.05, +: 0.1}"))




