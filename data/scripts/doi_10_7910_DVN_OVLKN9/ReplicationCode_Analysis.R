
############################################
############################################
# Replication code:                       ##
# Depolarization, Repolarization          ##
# and  Redistributive Ideological Change  ##
# in Britain, 1983-2016                   ##
# Cohen and Cohen (2019)                  ##
############################################
############################################

# First run ReplicationCode_DataPrep.R and ReplicationCode_Function.R

library(car)
library(xtable)
library(scales)
library(popbio)
library(rlang)
library(rstanarm)
library(tidyverse)
library(broom)

##----Elite Polarization ----

plotdat<-chts %>%
  mutate(partyname=toupper(partyname)) %>%
  filter(partyname %in% c("CONS", "LAB")) %>%
  dplyr::select(year, partyname, lrgen) %>%
  mutate(lrgen=round(lrgen, 3), period=year<2006) %>%
  dplyr::distinct()%>%
  spread(partyname, lrgen) %>%
  mutate(polarization=CONS-LAB) 

##----figure 1----
# pdf(paste0(i.path, "/expert_party_polarization.pdf"), width=3, height=3)
plotdat%>%
  ggplot(aes(year, polarization)) +
  geom_point() +
  stat_smooth(data=plotdat[plotdat$year<2007,], size=.2, colour="grey", alpha=.5, method="lm", se=FALSE) +
  stat_smooth(data=plotdat[plotdat$year>2005,], size=.2, colour="grey", alpha=.5, method="lm", se=FALSE)+
  #geom_line()+
  theme_classic()+
  scale_x_continuous(breaks=c(1985, 1990, 1995, 2000, 2005, 2010, 2015), minor_breaks = waiver())
# dev.off()

##---- Was Partisan Polarization Associated with Elite Polarization ----

bsa_cpc<-correlate.polchoice(bsac, useful.wave, pid.cols="pid_labcon", cols=c(bsa.lrvars)) %>%
  mutate(source="bsa")
bsa_cpc<-split.dat(bsa_cpc, "useful.wave", short.factor.levels="lc",  pid.cols ="pid_labcon", party.contrast.labels = "lc", balance.no = 3)
bhps_cpc<-correlate.polchoice(bhps, useful_wave,pid.cols="pid_labcon", cols=bhps.lr) %>%
  mutate(source="bhps")
bes_cpc<-correlate.polchoice(bes, useful_wave,pid.cols="pid_labcon", cols=beslrvars) %>%
  mutate(source="bes")

cpc<-bind_rows(bes_cpc,
               bhps_cpc,
               bsa_cpc$corpuseful.wave_lc_83_07_lc_b)

cpc_p1_overall<-stan_lmer(value~ndecade + (1+ndecade|attitude), cpc, cores=4)
cpc_p1_bsa<-stan_lmer(value~ndecade + (1+ndecade|attitude), bsa_cpc$corpuseful.wave_lc_83_07_lc_b, cores=4)
cpc_p1_bhps<-stan_lmer(value~ndecade + (1+ndecade|attitude), bhps_cpc, cores=4, adapt_delta = .99)
cpc_p1_bes<-stan_lmer(value~ndecade + (1+ndecade|attitude), bes_cpc, cores=4, adapt_delta = .99)

cpc_p2_overall<-stan_lmer(value~ndecade + (1+ndecade|attitude), bsa_cpc$corpuseful.wave_lc_07_16_lc_b, cores=4)

##----table 1----
myregtable(list(cpc_p1_overall, cpc_p1_bsa, cpc_p1_bhps, cpc_p1_bes, cpc_p2_overall), model.names=c("All Surveys", "BSA", "BHPS", "BES", "BSA"), extraheader1=c("$\\rho =$ attitude  $\\times $ partisanship"), extraheader1colspans = 5, extraheader2=c("1983-2007", "2007-2016"), extraheader2colspans=c(4, 1), caption="Results of Multi-Level Models With Dependent Variable as Attitude Partisanship (Correlation of Attitude with Labour (v. Conservative) Partisanship) in Period 1983-2007 and 2007-2016. Models show results across all three surveys and in each survey independently.", label = "tab:attitude_partisanship")

##----figure 2----
# colour plot
ranef_scatterplot_timecombine(cpc_p1_overall, cpc_p2_overall, fix_interact = "none", facet.on.source = TRUE, ncol=9, source.order=c("bsa", "bhps", "bes"), recode.attitude.function = "not.none", graph.name = "trends_cpc.pdf", pdf.output = TRUE, gheight=9, gwidth=7, ylab="correlation", palette_name = "Set1")

# black and white plot
ranef_scatterplot_timecombine(cpc_p1_overall, cpc_p2_overall, fix_interact = "none", facet.on.source = TRUE, ncol=9, source.order=c("bsa", "bhps", "bes"), recode.attitude.function = "not.none", graph.name = "trends_cpc_bw.pdf", pdf.output = TRUE, gheight=9, gwidth=7, ylab="correlation", palette_name = "my_bw")



## ---- Was Ideological Polarization Associated with Elite Polarization ----

bsa_trends<-trend.attitudes(bsac, none, c(bsa.lrvars), myfuns=c("sd", "prop_extremism")) %>%
  mutate(source="bsa")
bes_trends<-trend.attitudes(bes, none, beslrvars, myfuns=c("sd", "prop_extremism")) %>%
  mutate(source="bes")
bhps_trends<-trend.attitudes(bhps, none, bhps.lr, myfuns=c("sd", "prop_extremism")) %>%
  mutate(source="bhps")

bsa_funs<-split.fun.dat(bsa_trends, "none", c("none"), fun_col="fun")
bes_funs<-split.fun.dat(bes_trends, "none", c("none"), fun_col="fun")
bhps_funs<-split.fun.dat(bhps_trends, "none", c("none"), fun_col="fun")

sd_dat_1<-bind_rows(bsa_funs$sd_none_83_07_none_b,
                    bes_funs$sd_none_83_07_none,
                    bhps_funs$sd_none_83_07_none)

sd_dat_2<-bind_rows(bsa_funs$sd_none_07_16_none_b)

extreme_dat_1<-bind_rows(bsa_funs$prop_none_83_07_none_b,
                         bes_funs$prop_none_83_07_none,
                         bhps_funs$prop_none_83_07_none)

extreme_dat_2<-bind_rows(bsa_funs$prop_none_07_16_none_b)

fit_sd_p1<-stan_lmer(value~1+ndecade + (1+ndecade|attitude), sd_dat_1, cores=4)
fit_sd_bes<-stan_lmer(value~1+ndecade + (1+ndecade|attitude), bes_funs$sd_none_83_07_none, cores=4, adapt_delta = .99)
fit_sd_bhps<-stan_lmer(value~1+ndecade + (1+ndecade|attitude), bhps_funs$sd_none_83_07_none, cores=4, adapt_delta = .99)
fit_sd_bsa<-stan_lmer(value~1+ndecade + (1+ndecade|attitude), bsa_funs$sd_none_83_07_none_b, cores=4)

fit_sd_p2<-stan_lmer(value~1+ndecade + (1+ndecade|attitude), sd_dat_2, cores=4, adapt_delta = .99)

##----figure 3----
# colour
ranef_scatterplot_timecombine(fit_sd_p1, fit_sd_p2, fix_interact = "none", facet.on.source = TRUE, ncol=8, source.order=c("bsa", "bhps", "bes"), recode.attitude.function = "not.none", graph.name = "trends_sd.pdf", pdf.output = TRUE, gheight=9, gwidth=7, ylab="standard deviation")

# black and white
ranef_scatterplot_timecombine(fit_sd_p1, fit_sd_p2, fix_interact = "none", facet.on.source = TRUE, ncol=8, source.order=c("bsa", "bhps", "bes"), recode.attitude.function = "not.none", graph.name = "trends_sd_bw.pdf", pdf.output = TRUE, gheight=9, gwidth=7, ylab="standard deviation", palette_name = "my_bw")


##----Appendix table 7----
myregtable(list(fit_sd_p1, fit_sd_bsa, fit_sd_bhps, fit_sd_bes, fit_sd_p2), model.names=c("All Surveys", "BSA", "BHPS", "BES", "BSA"), extraheader1=c("$\\sigma$ of redistributive attitudes"), extraheader1colspans = 5, extraheader2=c("1983-2007", "2007-2016"), extraheader2colspans=c(4, 1), caption="Results of Multi-Level Models With Dependent Variable as Standard Deviation of Redistributive Attitudes in Period 1983-2007. Models show results across all three surveys and in each survey independently.", label="tab:sd_trends")



fit_extreme_vals_p1<-stan_lmer(value~1+ndecade + (1+ndecade|attitude), extreme_dat_1, cores=4)
fit_extreme_bes_p1<-stan_lmer(value~1+ndecade + (1+ndecade|attitude), bes_funs$prop_none_83_07_none, cores=4, adapt_delta = .999)
fit_extreme_bhps_p1<-stan_lmer(value~1+ndecade + (1+ndecade|attitude), bhps_funs$prop_none_83_07_none, cores=4, adapt_delta = .99)
fit_extreme_bsa_p1<-stan_lmer(value~1+ndecade + (1+ndecade|attitude), bsa_funs$prop_none_83_07_none_b, cores=4)

fit_extreme_vals_p2<-stan_lmer(value~1+ndecade + (1+ndecade|attitude), extreme_dat_2, cores=4)

##----Appendix figure 4----
ranef_scatterplot_timecombine(fit_extreme_vals_p1, fit_extreme_vals_p2, fix_interact = "none", facet.on.source = TRUE, ncol=8, source.order=c("bsa", "bhps", "bes"), recode.attitude.function = "not.none", graph.name = "trends_extreme.pdf", pdf.output = TRUE, gheight=9, gwidth=7, ylab="proportion of extreme values")

##----Appendix table 5----
myregtable(list(fit_extreme_vals_p1, fit_extreme_bsa_p1, fit_extreme_bhps_p1, fit_extreme_bes_p1, fit_extreme_vals_p2), model.names=c("All Surveys", "BSA", "BHPS", "BES", "BSA"), extraheader1=c("Proportion of extreme responses in redistributive attitudes"), extraheader1colspans = 5, extraheader2=c("1983-2007", "2007-2016"), extraheader2colspans=c(4, 1), caption="Results of Multi-Level Models With Dependent Variable as proportion of Extreme Responses in Redistributive Attitudes in Period 1983-2007. Models show results across all three surveys and in each survey independently.", label="tab:extreme")

bsa_corattitudes<-correlate.attitudes(bsac, c, bsa.lrvars)%>%
  mutate(source="bsa")
bsa_coratt<-split.constraint.dat(bsa_corattitudes, "c", short.factor.levels="nolev", balance.no = 3,core.vars = bsa.lrscale)
bhps_coratt<-correlate.attitudes(bhps, useful_wave, bhps.lr)%>%
  mutate(source="bhps")
bes_coratt<-correlate.attitudes(bes, useful_wave, beslrvars)%>%
  mutate(source="bes")

coratt_p1<-bind_rows(bhps_coratt,
                     bes_coratt, 
                     bsa_coratt$s_att_c_83_07_nolev_b)

coratt_p2<-bsa_coratt$s_att_c_07_16_nolev_b

s_coratt_1<-stan_lmer(value~1+ndecade+(1+ndecade|bothvars), coratt_p1, cores=4)
s_coratt_1_bsa<-stan_lmer(value~1+ndecade+(1+ndecade|bothvars), bsa_coratt$s_att_c_83_07_nolev_b, cores=4)
s_coratt_1_bhps<-stan_lmer(value~1+ndecade+(1+ndecade|bothvars), bhps_coratt, cores=4)
s_coratt_1_bes<-stan_lmer(value~1+ndecade+(1+ndecade|bothvars), bes_coratt, cores=4, adapt_delta = .99)

s_coratt_2<-stan_lmer(value~1+ndecade+(1+ndecade|bothvars), coratt_p2, cores=4)

##----table 2----
myregtable(list(fit_extreme_vals_p1, fit_sd_p1, s_coratt_1, fit_extreme_vals_p2,  fit_sd_p2, s_coratt_2), model.names=c("extreme", "$\\sigma$", "constraint", "extreme", "$\\sigma$", "constraint"), extraheader1=c(" "), extraheader1colspans = 6, extraheader2=c("1983-2007 (surveys combined)", "2007-2016 (BSAS only)"), extraheader2colspans=c(3, 3), caption="Results of Multi-Level Models of Ideological Depolarization with Dependent Variable as proportion of extreme responses, standard deviation of responses, constraint between attitude pairs in Period 1983-2007 and 2007-16.", label="tab:trends_ideopol")

##----Appendix figure 5----
reduced.bothvars<-unique(c(bsa_coratt$s_att_c_83_07_nolev_core$bothvars, bhps_coratt$bothvars, bes_coratt$bothvars))
#reduced.bothvars<-unique(c(bsa_coratt$s_att_c_07_16_nolev_b$bothvars))

ranef_scatterplot_timecombine(s_coratt_1, s_coratt_2, fix_interact="none", selectvars = reduced.bothvars, ncol=11, facet.on.source=TRUE, source.order=c("bsa", "bhps", "bes"), ylab="correlation", graph.name = "trends_constraint.pdf", pdf.output = TRUE, gheight = 9, palette_name = "Set1")

##----Appendix table 6----
myregtable(list(s_coratt_1, s_coratt_1_bsa, s_coratt_1_bhps, s_coratt_1_bes, s_coratt_2), model.names=c("All Surveys", "BSA", "BHPS", "BES", "BSA"), extraheader1=c("$\\rho =$ attitude  $\\times $ attitude"), extraheader1colspans = 5, extraheader2=c("1983-2007", "2007-2016"), extraheader2colspans=c(4, 1), caption="Results of Multi-Level Models With Dependent Variable as Correlation between Two Redistributive Attitudes in Period 1983-2007. Models show results across all three surveys and in each survey independently.", label="tab:trends_constraint")

## ---- Was partisan depolarization caused by ideological or partisanship change----

# ----observed partisan polarization----

val_with_pid_conlab<-bhps %>%
  filter(PID %in% balanced.pid) %>%
  mutate(year = factor(wave))  %>%
  mutate(constant=1) %>%
  mutate(ndecade=(year-1991)/10)

raw<-correlate.polchoice(val_with_pid_conlab, constant, "pid_labcon", cols = bhps.lr) %>%
  mutate(ndecade=(year-1991)/10)


small_raw<-raw %>%
  add_column(dataset="observed") %>%
  dplyr::select(year, attitude, value, ndecade, dataset) %>%
  dplyr::mutate(variable=attitude)

##----holding constant pid ----
long_dat<-bhps %>%
  dplyr::select(PID, wave, one_of(vote.vars), one_of(bhps.redist.attitudes), one_of(bhps.lr)) %>%
  gather(variable, value, one_of(bhps.redist.attitudes), one_of(bhps.lr), one_of(vote.vars)) 

value_dat<-long_dat %>%
  filter(!variable %in% vote.vars) %>%
  mutate(wavew=paste0("w", wave)) 

long_vote<-long_dat %>%
  filter(variable %in% vote.vars)%>%
  mutate(wavep=paste0("party", wave)) %>%
  dplyr::rename(party=value) %>%
  dplyr::rename(votevar=variable)

vote_dat<- long_vote %>%
  spread(votevar, party)

val_with_vote<-left_join(value_dat, vote_dat, by=c("PID", "wave"))

vote_spread <- long_vote %>%
  dplyr::select(-wavep) %>%
  unite(varwave, votevar, wave) %>%
  spread(varwave, party)

value_spread <- value_dat %>%
  dplyr::select(-wavew) %>%
  unite(varwave, variable, wave) %>%
  spread(varwave, value) 

val_long_with_votehistory<-left_join(value_dat, vote_spread)

val_with_constant_pid <- val_long_with_votehistory %>%
  filter(PID %in% balanced.pid) %>%
  mutate(year = factor(wave)) %>%
  mutate(constant=1) 

constant_pid<-correlate.polchoice(val_with_constant_pid, variable, "pid_labcon_1991", cols = "value")%>%
  mutate(ndecade=(year-1991)/10) %>%
  filter(variable %in% bhps.lr)

small_constant_pid<-constant_pid %>%
  mutate(dataset="const_partisanship") %>%
  dplyr::select(year, variable, value, ndecade, dataset) %>%
  mutate(attitude=variable)
##----holding constant ideology----
pid_with_constant_val <- vote_dat %>%
  left_join(value_spread) %>%
  filter(PID %in% balanced.pid) %>%
  mutate(year = factor(wave)) %>%
  mutate(constant=1)

constant_val<-correlate.polchoice(pid_with_constant_val, constant, "pid_labcon", cols = paste0(bhps.lr, "_1991")) %>%
  separate(attitude, c("attitude", "wave"), "_")%>%
  mutate(ndecade=(year-1991)/10)

small_constant_val<-constant_val %>%
  mutate(dataset="const_ideology") %>%
  separate(attitude, c("variable", "inityear"), "_") %>%
  dplyr::select(year, variable, value, ndecade, dataset) %>%
  mutate(attitude=variable)



variants<-bind_rows(small_raw, small_constant_pid, small_constant_val) %>%
  filter(attitude %in% bhps.lr) %>%
  mutate(attitude=paste(attitude, dataset)) %>%
  mutate(area="redistribution", domain="redistribution")
variants$dataset<-relevel(factor(variants$dataset), ref="observed")


sfff4<-stan_lmer(value~ndecade*dataset+(1+ndecade|attitude), variants, cores=4)

##----table 4----
myregtable(list(sfff4), ordermodel = 1, covariate.relabels=c("Intercept", "Time (decades)", "fixed ideology", "fixed partisanship", "Time $\\times$ fixed ideology", "Time $\\times$ fixed partisanship", "Residual SD:", "\\hskip .5cm Intercepts", "\\hskip .5cm Trends", "\\hskip .5cm Data", "N", "Groups"), model.names = c(""), extraheader1 = "$\\rho = $ attitude  $\\times $ partisanship", extraheader1colspans = c(1), caption="Results of Multi-Level Models With Dependent Variable as Correlation between Redistributive Attitude and Partisanship in the BHPS 1991-2007. The models show trends in the observed data, stable (1991) ideology, stable (1991) partisanship and with dummy variable indicating the condition.", label="tab:const_ideo_pid")



##----figure 5-----
relabel_fix_facet<-as_labeller(function(x){
  case_when(
    x=="const_ideology" ~ "fixed (1991) ideology; changing partisanship",
    x=="const_partisanship"~ "changing ideology; fixed (1991) partisanship",
    TRUE ~ x
  )
})

ranef_scatterplot_dataset(sfff4, fix_interact = "dataset", graph.name = "bhps_partisandepolarization_corr_fixints.pdf", pdf.output = FALSE, gheight=9, gwidth=7, legend.position = "right")

##---- did partisanship cause depolarization -----
library(MatchIt)
set.seed(1234)
matchdat<-left_join(value_spread, vote_spread) %>%
  filter(PID %in% balanced.pid) %>%
  dplyr::select(PID, pid_lab_1991, pid_con_1991, vote_lab_1991, vote_con_1991, one_of(paste0(bhps.lr, "_1991"))) %>%
  filter(pid_con_1991==vote_con_1991, pid_lab_1991==vote_lab_1991) %>%
  drop_na()

labmatchdat <- matchdat %>%
  filter(pid_con_1991==0)
labmatchdat$matchno<-row.names(labmatchdat)
m.lab<-matchit(pid_lab_1991~ fairshare_1991 + onelaw_1991 + privateent_1991 + stateown_1991 + gvtprovjob_1991 + strngtu_1991, labmatchdat, replace=TRUE)

labtreat<-as_data_frame(m.lab$match.matrix)
labtreat$treatment <- row.names(m.lab$match.matrix)
names(labtreat)[1]<-"control"

labmatched<-inner_join(labtreat, labmatchdat, by=c("control"="matchno"))%>%
  mutate(pid_con_match=0, pid_lab_match=1)
labtreatments<-labmatchdat[labmatchdat$pid_lab_1991==1,]

conmatchdat <- matchdat %>%
  filter(pid_lab_1991==0)
conmatchdat$matchno<-row.names(conmatchdat)

m.con<-matchit(pid_con_1991~ fairshare_1991 + onelaw_1991 + privateent_1991 + stateown_1991 + gvtprovjob_1991 + strngtu_1991, conmatchdat, replace=TRUE)

contreat<-as_data_frame(m.con$match.matrix)
contreat$treatment <- row.names(m.con$match.matrix)
names(contreat)[1]<-"control"

conmatched<-inner_join(contreat, conmatchdat, by=c("control"="matchno")) %>%
  mutate(pid_con_match=1, pid_lab_match=0)

thetreated <- bind_rows(labmatchdat[labmatchdat$pid_lab_1991==1,],
                        conmatchdat[conmatchdat$pid_con_1991==1,]) %>%
  mutate(pid_con_match=0, pid_lab_match=0)

analysisdat<-bind_rows(labmatched,
                       conmatched,
                       thetreated)

myreg<-function(x){
  t.test(x$value[x$partisan=="partisan"], x$value[x$partisan=="non.partisan"], paired = FALSE)
}
mysummary<-function(x){
  x%>%
    group_by(partisan) %>%
    summarize(mu=mean(as.numeric(value))) %>%
    spread(partisan, mu) %>%
    mutate(difference= partisan - non.partisan)
}
mystars<-function(x){
  y<-rep("", length(x))
  y[x<.001] <- "***"
  y[x<.01] <- "**"
  y[x<.05] <- "*"
  y
}


balance_table<-analysisdat %>%
  mutate(leftlab=pid_lab_1991+pid_lab_match, rightcon=pid_con_1991+pid_con_match, partisan=ifelse(pid_lab_1991+pid_con_1991, "partisan", "non.partisan")) %>%
  gather(variable, value, -control, -treatment, -pid_lab_1991, -pid_con_1991, -vote_lab_1991, -vote_con_1991, -pid_lab_match, -pid_con_match, -PID, -matchno, -leftlab, -rightcon, -partisan) %>%
  group_by(leftlab, rightcon, variable) %>%
  nest() %>%
  mutate(summary=map(data, mysummary)) %>%
  mutate(wilcox=map(data, myreg)) %>%
  mutate(wilcox=map(wilcox, broom::tidy)) %>%
  dplyr::select(-data) %>%
  unnest() %>%
  mutate(case=ifelse(leftlab, "Labour and Labour Matches", "Conservative and Conservative Matches"), sig.diff=mystars(p.value)) %>%
  separate(variable, "_", into=c("variable", "year")) %>%
  dplyr::select(case, variable, non.partisan, partisan, difference, sig.diff)

##----Appendix table 9 ----
print(xtable::xtable(balance_table, caption="Balance check on ideological matching. Significance indicated by Mann-Whitney Test. * indicates p<.05."), include.rownames=FALSE, floating=TRUE, file="clipboard")

byval_spread<-value_dat %>%
  dplyr::select(-wave) %>%
  spread(wavew, value) 

bothdat<-value_dat %>%
  filter(variable %in% bhps.lr) %>%
  left_join(vote_dat, by=c("PID", "wave")) %>%
  left_join(vote_spread, by="PID") %>%
  left_join(byval_spread) %>%
  mutate(ndecade=(wave-1991)/10)

analysisdat2 <- inner_join(analysisdat, bothdat) %>%
  mutate(id_type=case_when(
    pid_lab_1991==1 ~ "lab",
    pid_con_1991==1 ~ "con",
    pid_lab_match==1 ~ "lab match",
    pid_con_match==1 ~ "con match"
  ))

rescale_0_1<-function(x){
  x<- (x-min(x, na.rm=TRUE))/(1 + max(x, na.rm=TRUE)-min(x, na.rm=TRUE))
  x
}

##----figure 6a----
# pdf(paste0(i.path, "/partisan_control_trajectories.pdf"))
analysisdat2 %>%
  group_by(variable) %>%
  mutate(value=rescale_0_1(value)) %>%
  group_by(id_type, variable, wave, ndecade) %>%
  summarize(meanval=mean(value, na.rm=TRUE)) %>%
  ggplot(aes(ndecade, meanval, linetype=id_type, shape=id_type))+
  facet_grid(.~variable)+
  geom_point()+
  geom_line()+
  ylab("position")+
  xlab("decades after 1991")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), legend.title = element_blank(), panel.grid = element_blank())
# dev.off()

analysis3<-analysisdat2 %>%
  spread(variable, value) %>%
  mutate(year=wave)

analysis3<-analysis3 %>%
  mutate(year=wave, control_case=ifelse(id_type %in% c("lab match", "con match"), "matches", "partisans")) %>%
  mutate(concontrolcon =pid_con_1991 + pid_con_match, labcontrollab =pid_lab_1991 + pid_lab_match)
analysis4<-correlate.polchoice(analysis3, group.col = control_case, pid.cols="labcontrollab", cols=bhps.lr, wave.col=wave) %>%
  mutate(ndecade=(wave-1991)/10)

analysis5<-analysis4 %>%
  mutate(variable=attitude, dataset=control_case, attdata=paste(attitude, dataset), attitude=attdata)

sf3a<-stan_lmer(value~1+ndecade*dataset +(1+ndecade|attitude),analysis5, cores=4)

##----figure 6b----
ranef_scatterplot_dataset2(sf3a, fix_interact = "dataset", legend.position="right", graph.name = "partisan_control_polarization.pdf", pdf.output = F, gheight=7, gwidth=7)

##----table 5 ----
myregtable(list(sf3a), 
           covariate.relabels=c("Intercept", "Time (decades)", "partisans", "Time $\\times$ fixed partisanship", "Residual SD:", "\\hskip .5cm Intercepts", "\\hskip .5cm Trends", "\\hskip .5cm Data", "N", "Groups"), ordermodel = 1, model.names = c( "partisans and ideologists"), extraheader1=("$\\rho =$ attitude  $\\times $ Labour or left non-partisan"), extraheader1colspans = c(1), caption="Results of Multi-Level Models With Dependent Variable as Correlation between Redistributive Attitude and Fixed Group Partisanship and Ideologist Dummy Variables in BHPS 1991-2007.", label="tab:partisan_control")

##----Appendix Table 8 ----
# Additional evidence on ideological depolarization amongst partisans and non-partisans

matchdat<-left_join(value_spread, vote_spread) %>%
  filter(PID %in% balanced.pid) %>%
  dplyr::select(PID, pid_lab_1991, pid_con_1991, vote_lab_1991, vote_con_1991, one_of(paste0(bhps.lr, "_1991"))) %>%
  filter(pid_con_1991==vote_con_1991, pid_lab_1991==vote_lab_1991) %>%
  drop_na()

pid_1991 <- matchdat %>%
  filter(pid_lab_1991==1|pid_con_1991==1) %>%
  dplyr::select(PID) %>%
  dplyr::left_join(val_with_vote) %>%
  dplyr::filter(variable %in% bhps.lr) %>%
  dplyr::mutate_at("value", rescale_0_1) %>%
  dplyr::group_by(wave, variable) %>%
  dplyr::summarize(sd=sd(value, na.rm=TRUE))%>%
  dplyr::mutate(ndecade=(wave-1991)/10)

nopid_1991 <- matchdat %>%
  filter(pid_lab_1991==0&pid_con_1991==0) %>%
  dplyr::select(PID) %>%
  dplyr::left_join(val_with_vote) %>%
  dplyr::filter(variable %in% bhps.lr) %>%
  dplyr::group_by(wave, variable) %>%
  dplyr::summarize(sd=sd(value, na.rm=TRUE)) %>%
  dplyr::mutate(ndecade=(wave-1991)/10)



prop_extremism_bhps<-function(x, na.rm=TRUE){
  if(na.rm){
    x<-x[!is.na(x)]
  }
  highest_val<-max(x)
  lowest_val<-min(x)
  second_highest<-sort(unique(x), decreasing=TRUE)[2]
  second_lowest<-sort(unique(x))[2]
  if (length(unique(x))>9){
    high_vals<-c(highest_val, second_highest)
    low_vals<-c(lowest_val, second_lowest)
  } else {
    high_vals<-c(highest_val)
    low_vals<-c(lowest_val)
  }
  (sum(x %in% high_vals)+sum(x %in% low_vals))/length(x)
}
scale_length<-function(x, na.rm=TRUE){
  if(na.rm){
    x<-x[!is.na(x)]
  }
  (length(unique(x)))
}

nopid_1991 <- matchdat %>%
  filter(pid_lab_1991==0&pid_con_1991==0) %>%
  dplyr::select(PID) %>%
  dplyr::left_join(val_with_vote) %>%
  dplyr::filter(variable %in% bhps.lr) %>%
  dplyr::mutate_at("value", rescale_0_1) %>%
  dplyr::group_by(wave, variable) %>%
  dplyr::summarize(sd=sd(value, na.rm=TRUE), extremism=prop_extremism_bhps(value)) %>%
  dplyr::mutate(ndecade=(wave-1991)/10)

pid_1991 <- matchdat %>%
  filter(pid_lab_1991==1|pid_con_1991==1) %>%
  dplyr::select(PID) %>%
  dplyr::left_join(val_with_vote) %>%
  dplyr::filter(variable %in% bhps.lr) %>%
  dplyr::mutate_at("value", rescale_0_1) %>%
  dplyr::group_by(wave, variable) %>%
  dplyr::summarize(sd=sd(value, na.rm=TRUE), extremism=prop_extremism_bhps(value)) %>%
  dplyr::mutate(ndecade=(wave-1991)/10)

nopid_1991 %>%
  ggplot(aes(wave, extremism))+
  facet_wrap(~variable)+
  geom_point()

nopid_1991_dat <- matchdat %>%
  filter(PID %in% balanced.pid) %>%
  dplyr::select(PID, pid_lab_1991, pid_con_1991, vote_lab_1991, vote_con_1991, one_of(paste0(bhps.lr, "_1991"))) %>%
  filter(pid_lab_1991==0&pid_con_1991==0) %>%
  dplyr::select(PID, pid_lab_1991, pid_con_1991) %>%
  dplyr::left_join(val_with_vote) %>%
  dplyr::filter(variable %in% bhps.lr) %>%
  spread(variable, value) %>%
  mutate(thegroup="no_pid") %>%
  mutate(year=wave, ndecade=(wave-1991)/10)

pid_1991_dat <- matchdat %>%
  filter(PID %in% balanced.pid) %>%
  dplyr::select(PID, pid_lab_1991, pid_con_1991, vote_lab_1991, vote_con_1991, one_of(paste0(bhps.lr, "_1991"))) %>%
  filter(pid_lab_1991==1|pid_con_1991==1) %>%
  dplyr::select(PID, pid_lab_1991, pid_con_1991) %>%
  dplyr::left_join(val_with_vote) %>%
  dplyr::filter(variable %in% bhps.lr) %>%
  spread(variable, value) %>%
  mutate(thegroup="no_pid") %>%
  mutate(year=wave, ndecade=(wave-1991)/10)

nopid_constraint<-correlate.attitudes(nopid_1991_dat, thegroup, cols=bhps.lr)
pid_constraint<-correlate.attitudes(pid_1991_dat, thegroup, cols=bhps.lr)

sf1<-stan_lmer(sd~ndecade+(1+ndecade|variable), nopid_1991, cores=4)
sf2<-stan_lmer(sd~ndecade+(1+ndecade|variable), pid_1991, cores=4)
sf3<-stan_lmer(extremism~ndecade+(1+ndecade|variable), nopid_1991, cores=4)
sf4<-stan_lmer(extremism~ndecade+(1+ndecade|variable), pid_1991, cores=4, adapt_delta = .99)
sf5<-stan_lmer(value~ndecade + (1+ndecade|bothvars), nopid_constraint, cores=4)
sf6<-stan_lmer(value~ndecade + (1+ndecade|bothvars), pid_constraint, cores=4)

myregtable(list(sf1, sf2, sf3, sf4, sf5, sf6), digits=2, model.names=c("non-partisan", "partisans", "non-partisan", "partisans", "non-partisan", "partisans"), extraheader1=c(""), extraheader1colspans=c(6), extraheader2 = c("$\\sigma$", "extremism", "constraint"), extraheader2colspans = c(2,2, 2), caption="Results of Multi-Level Model With dependent variable as standard deviation, extremism and constraint of responses amongst the fixed group of partisans (partisan in initial wave) and the fixed group of non-partisans (non-partisan in initial wave) in the BHPS 1991-2007.", label= "tab:ideology_trend_bypartisanship")


##----LCA similarity [comparison to Evans and Neundorf (2018)] ----

# To examine the implications of the cross-lagged models in Evans and Neundorf (2018)
# for the three mechanisms 

dist_to_optim_simple <- function(parameters=c(centre=.58, left=.20, right=.22), tr_p=t(en_simple_transition), overall_distribution1=overall_distribution_simple){
  this_init_pop<-force(t(t(c(parameters))))
  this_init_pop<-this_init_pop/sum(this_init_pop)
  rownames(this_init_pop) <-c("left", "centre", "right")
  model_res1<-globalenv()$popmodel(this_init_pop, tr_p, 6)
  compare_model_overallactual(model_res1, overall_distribution1)
}
dist_to_optim <- function(parameters=c(labour_left=1503, nopid_left=664, con_left=194, lab_centre=1361, nonpid_centre=1640, con_centre=650, lab_right=55, nopid_right=416, con_right=2436), tr_p=t_p, overall_distribution1=overall_distribution){
  this_init_pop<-force(t(t(c(parameters))))
  this_init_pop<-this_init_pop/sum(this_init_pop)
  rownames(this_init_pop) <-c("lab_left", "nopid_left", "con_left", "lab_centre", "nopid_centre", "con_centre", "lab_right", "nopid_right", "con_right")
  model_res1<-globalenv()$popmodel(this_init_pop, tr_p, 6)
  compare_model_overallactual(model_res1, overall_distribution1)
}

simdat<-function(res){
  df <-data.frame(res) %>%
    rownames_to_column() %>%
    separate(rowname, into=c("pid", "ideo"))
  simdf(df)
}

simdf<-function (df){
  
  returndf<-data.frame(matrix(NA, ncol=3, nrow=sum(df[,3])))
  names(returndf)<-c("pid", "ideo")
  startr <- 1
  
  for (r in 1:nrow(df)){
    lengthr<-round(as.numeric(df[r, 3]))
    if (lengthr>0){
      numeric_pid<-ifelse(df[r,1]=="lab", 1, ifelse(df[r,1]=="con", -1, 0))
      numeric_ideo<-ifelse(df[r,2]=="left", 1, ifelse(df[r,2]=="right", 0, 0.5))
      returndf[startr:(startr+lengthr-1),1]<-rep(numeric_pid, lengthr)
      returndf[startr:(startr+lengthr-1),2]<-rep(numeric_ideo, lengthr)
      startr<-startr+lengthr
    }
  }
  returndf
}
cordf<-function(df){
  theresults<-simdf(df)
  cor(theresults$pid, theresults$ideo, use="pairwise.complete.obs")
}
meanposdf<-function(df){
  theresults<-simdf(df)
  mean(theresults$ideo, na.rm=TRUE)
}

##---- simple ideology model----
en_simple_transition<-(matrix(c(c(.97, .13, .07),
                                c(.02, .87, .00),
                                c(.01, .00, .93)), nrow=3))

init_pop_simple<-(matrix(c(centre=1,left=1,right=1)))
rownames(init_pop_simple)<-c("_centre", "_left", "_right")
aa<-popmodel(init_pop_simple, t(en_simple_transition), 6)
overall_distribution_simple<-c(.58, .20, .22)

ab<-optim(par=rep(.33, 3), dist_to_optim_simple)
init_pop_optim_simple<-matrix(ab$par/sum(ab$par))
rownames(init_pop_optim_simple)<-c("_centre", "_left", "_right")
estmodel<-popmodel(init_pop_optim_simple, t(en_simple_transition), 6)

smallpopmodel<-function(init_pop, transposed_transition, n_waves, popnames=c("0_centre" , "1_leftist",  "-1_rightist")){
  current_pop<-init_pop
  res<-as.data.frame(matrix(NA, nrow=length(init_pop), ncol=n_waves+2))
  res[,1]<-popnames
  res[,2]<-as.numeric(current_pop)
  names(res)<-c("rowname", "wave_0", paste0("wave_", 1:n_waves))
  for (wave in 1:n_waves){
    current_pop<-transposed_transition %*% current_pop
    res[,wave+2]<-as.numeric(current_pop)
  }
  res <- res %>%
    separate(rowname, into=c("pid", "ideo"), sep="_")
  res
}

transition_matrix<-en_simple_transition
ei<- eigen(t(transition_matrix))
first=Re(ei$vectors[,1])
mu = first/sum(first)

current_pop<-c(.333, .333, .333)
current_pop<-c(.98, .01,  .01)
current_depolarized<-c(1, .0,  .0)
current_polarized<-c(.49, 0.28,  0.23)
current_equil<-mu
from_polarized<-smallpopmodel(current_polarized, t(transition_matrix), 6) %>%
  mutate(case="polarized (actual) inits")
from_depolarized<-smallpopmodel(current_depolarized, t(transition_matrix), 6) %>%
  mutate(case="depolarized inits")
from_equil<-smallpopmodel(current_equil, t(transition_matrix), 6) %>%
  mutate(case="equilibrium inits")

demo_dat<-
  bind_rows(from_polarized, from_depolarized, from_equil)

##----Appendix figure 1 ----
pdf(paste0(i.path, "/en_ambig_ideo_change.pdf"), width=5, height=5)
demo_dat%>%
  gather(wave, perc, -pid, -ideo, -case) %>%
  separate(wave, into=c("f", "wave")) %>%
  mutate(wave=as.numeric(wave)+1) %>%
  mutate(ideo=factor(ideo, levels=c("leftist", "centre", "rightist"))) %>%
  ggplot(aes(wave, perc)) +
  facet_grid(ideo~case, scale="free")+
  geom_point()+
  geom_line()+
  theme_bw()
dev.off()


##----figure 4----
pdf(paste0(i.path, "/en_ideo_change.pdf"), height=3, width=3)
estmodel %>%
  gather(wave, value, -ideo, -pid) %>%
  dplyr::group_by(wave) %>%
  tidyr::separate(wave, into=c("var", "wave"))%>%
  dplyr::mutate(wave=as.numeric(wave)+1)%>%
  dplyr::group_by(wave) %>%
  dplyr::mutate(tot=sum(value)) %>%
  dplyr::group_by(wave, ideo) %>%
  dplyr::summarize(value=sum(value)/tot[1])%>%
  mutate(ideo = fct_relevel(ideo, "left", "centre", "right")) %>%
  ggplot(aes(wave, value))+
  facet_grid(. ~ ideo)+
  scale_y_continuous(labels = scales::percent, limits = c(0, NA))+
  ylab("percentage of population")+
  geom_point()+
  geom_line()+
  theme_bw() +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(), legend.title=element_blank())
dev.off()

##----mainmodel----

# DV = core values

cv_param<-c("right_coef", "right_se", "centre_coef", "centre_se", "left_coef", "left_se")
cv_intercept<-c(-.89, .27, 1.69, .22, -.79, .39)
cv_con_lag<-c(-.06, .1, .17, .06, -.11, .09)
cv_nopid_lag<-c(-.07, .08, -.06, .05, .13, .06)
cv_lab_lag<-c(.13, .11, -.11, .06, -.02, .08)
cv_right_lag<-c(4.47, .44, -.9, .41, -3.57, .79)
cv_centre_lag<-c(-.94, .29, 1.26, .23, -.32, .41)
cv_left_lag<-c(-3.53, .45, -.36, .28, 3.89, .44)

model_ideochange<-t(data_frame(
  cv_intercept,
  cv_con_lag,
  cv_nopid_lag,
  cv_lab_lag,
  cv_right_lag,
  cv_centre_lag,
  cv_left_lag))
colnames(model_ideochange)<-cv_param 
model_ideochange<-data.frame(model_ideochange)
model_ideochange$parameter<- rownames(model_ideochange)

model_ideochange <- model_ideochange%>%
  separate(col=parameter, into=c("model", "parameter"), sep=3)

coef_cols<-c(1,3,5)
se_cols<-c(2,4,6)

# DV = partisanship
p_param<-c("con_coef", "con_se", "nopid_coef", "nopid_se", "lab_coef", "lab_se")
p_intercept<-c(-.61, .06, .72, .04, -.11, .04)
p_right_lag <- c(.80, .05, -.11, .04, -.69, .06)
p_centre_lag <- c(-.1, .04, .01, .03, .09, .04)
p_left_lag <- c(-.71, .07, .11, .04, .60, .05)
p_con_lag <- c(2.48, .06, -.62, .05, -1.86, .07)
p_nopid_lag <- c(-.66, .07, 1.31, .05, -.65, .05)
p_lab_lag <- c(-1.82, .10, -.69, .06, 2.51, .06)

model_pidchange <- t(data_frame(
  p_intercept,
  p_con_lag,
  p_nopid_lag,
  p_lab_lag,
  p_right_lag,
  p_centre_lag,
  p_left_lag))

colnames(model_pidchange) <- p_param
model_pidchange<-data.frame(model_pidchange)
model_pidchange$parameter<- rownames(model_pidchange) 
model_pidchange <- model_pidchange%>%
  separate(col=parameter, into=c("model", "parameter"), sep=2)

make_transitionprobs<- function(model, randomize=FALSE){
  model<-model %>%
    separate(parameter, c("case", "tail"), "_", remove=FALSE)
  coef_cols <- grepl("coef", names(model))
  se_cols <- grepl("se", names(model))
  coef_col_names<-names(model)[coef_cols]
  se_col_names<-names(model)[se_cols]
  if(randomize){
    for(x in 1:length(coef_col_names)){
      model[,coef_col_names[x]]<-rnorm(nrow(model), model[,coef_col_names[x]], model[,se_col_names[x]])
    }
  }
  
  int <-model[model$parameter=="intercept", coef_cols]
  int_means <-model[model$parameter=="intercept", coef_cols]
  int_se <-model[model$parameter=="intercept", se_cols]
  
  
  these_cases<-as_tibble(expand.grid(pid_lag=c("lab", "nopid", "con"), ideo_lag=c("left", "centre", "right")))
  
  my_parameters<-apply(these_cases, 1, function (x) {
    pid <- x["pid_lag"]
    ideo <- x["ideo_lag"]
    data.frame(pid_lag=pid, ideo_lag=ideo, model[model$case==pid, coef_cols ] + model[model$case==ideo, coef_cols] + int)
  })
  cc<-do.call(rbind, my_parameters)
  transition_probs<-cc
  transition_probs[,3:5]<-exp(cc[,3:5])/rowSums(exp(cc[,3:5]))
  transition_probs
}
ideo_transition<-make_transitionprobs(model_ideochange)
pid_transition<-make_transitionprobs(model_pidchange)

make_threecases <- function (ideo_transition, pid_transition){
  these_cases<-as_tibble(expand.grid(pid_lag=c("lab", "nopid", "con"), ideo_lag=c("left", "centre", "right"), pid=c("lab", "nopid", "con"), ideo=c("left", "centre", "right")))
  
  names(ideo_transition)<-gsub("_coef", "", names(ideo_transition))
  names(pid_transition)<-gsub("_coef", "", names(pid_transition))
  
  find_ideo_p<-function(pid_lag, ideo_lag, to_ideo, ideo_transition){
    ideo_p<-ideo_transition[ideo_transition$pid_lag==pid_lag & ideo_transition$ideo_lag==ideo_lag, to_ideo]
    ideo_p 
  }
  find_pid_p<-function(pid_lag, ideo_lag, to_pid, pid_transition){
    pid_p<-pid_transition[pid_transition$pid_lag==pid_lag & pid_transition$ideo_lag==ideo_lag, to_pid]
    pid_p 
  }
  add_column(these_cases, stable_ideo=NA, stable_pid=NA, prob=NA)
  for (n in 1:nrow(these_cases)){
    this_pid_lag<-as.character(these_cases$pid_lag[n])
    this_ideo_lag<-as.character(these_cases$ideo_lag[n])
    this_pid<-as.character(these_cases$pid[n])
    this_ideo<-as.character(these_cases$ideo[n])
    this_stable_ideo_p<-ifelse(this_ideo_lag==this_ideo, 1, 0)
    this_stable_pid_p<-ifelse(this_pid_lag==this_pid, 1, 0)
    
    this_pid_p<-find_pid_p(this_pid_lag, this_ideo_lag, this_pid, pid_transition)
    this_ideo_p<-find_ideo_p(this_pid_lag, this_ideo_lag, this_ideo, ideo_transition)
    these_cases[n, "prob"] =this_pid_p*this_ideo_p
    these_cases[n, "stable_ideo"] = this_pid_p*this_stable_ideo_p
    these_cases[n, "stable_pid"] = this_stable_pid_p*this_ideo_p
  }
  
  these_cases
}

bb<-make_threecases(ideo_transition, pid_transition)

myinits<-c(1,1,1,1,1,1,1,1,1)

centrist_pid_comp<-c(lab=.27, no_pid=.52, con=.2)
leftist_pid_comp<-c(lab=.58, no_pid=.37, con=.05)
rightist_pid_comp<-c(lab=.05, no_pid=.28, con=.66)

centrist_overall_comp<-centrist_pid_comp * .58
leftist_overall_comp<-leftist_pid_comp * .20
rightist_overall_comp<- rightist_pid_comp * .22

tot_comp<-sum(c(centrist_overall_comp, leftist_overall_comp, rightist_overall_comp))

centrist_target<-centrist_overall_comp/tot_comp
leftist_target<-leftist_overall_comp/tot_comp
rightist_target<-rightist_overall_comp/tot_comp

target_distribution<-c(.58, .37, .05, .27, .52, .20, .05, .28, .66)
target_ideo_distribution<-c(.58, .20, .22)

overall_target_norm<-c(leftist_target, centrist_target, rightist_target)
myfulltrans<-function(params=c(lab_left, nopid_left, con_left, lab_centre, nopid_centre, con_centre, lab_right, nopid_right, con_right), t_p, overall_target=overall_target_norm, within_ideo_target=target_distribution, n_transition){
  params<-exp(params)/sum(exp(params))
  #print(params)
  #print(sum(params))
  inits<-init_pop_actual<-t(t(c(lab_left=params[1], nopid_left=params[2], con_left=params[3], lab_centre=params[4], nopid_centre=params[5], con_centre=params[6], lab_right=params[7], nopid_right=params[8], con_right=params[9])))
  res_df<-popmodel(inits, t_p, n_transition) %>%
    rowid_to_column("res_order")
  
  res_mat <- res_df[,4:(3+n_transition)]
  res_df<-res_df %>%
    gather(wave, value, -pid, -ideo, -res_order) %>%
    dplyr::group_by(pid, ideo, res_order) %>%
    dplyr::summarize(pid_ideo_tot=sum(value)) %>%
    dplyr::group_by(ideo) %>%
    dplyr::mutate(ideo_tot=sum(pid_ideo_tot), ideo_prop=pid_ideo_tot/ideo_tot) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(tot_sum=sum(pid_ideo_tot), pid_ideo_prop=pid_ideo_tot/tot_sum) %>%
    arrange(res_order)
  #print(res_df)
  #print(res_df$pid_ideo_prop)
  #print(overall_target)
  
  
  print(res_df)
  #print((res_df$pid_ideo_prop - overall_target))
  print(sum((res_df$pid_ideo_prop - overall_target)^2))
  sum((res_df$pid_ideo_prop - overall_target)^2) + sum((res_df$ideo_prop - within_ideo_target)^2)
}

myfulltrans(params=myinits, t_p=t_p, overall_target=overall_target_norm, n_transition=6)

init_pop_optim_detail<-optim(myinits, myfulltrans, t_p=t_p, overall_target=overall_target_norm, n_transition=6)
init_pop_optim_size<-init_pop_actual
init_pop_optim_size[,1]<-10000*(exp(init_pop_optim_detail$par)/sum(exp(init_pop_optim_detail$par)))

myfulltrans(params=init_pop_optim_detail$par, t_p=t_p, overall_target=overall_target_norm, n_transition=6)

full_model_frompol<-popmodel(init_pop = init_pop_optim_size, t_p, 6) %>%
  mutate(model="full", case="mainly ideology (actual)")
stable_ideo_frompol<-popmodel(init_pop = init_pop_optim_size, t_i, 6)%>%
  mutate(model="stable ideology; changing partisanship", case="mainly ideology (actual)")
stable_pid_frompol<-popmodel(init_pop = init_pop_optim_size, t_pid, 6)%>%
  mutate(model="stable partisanship; changing ideology", case="mainly ideology (actual)")


full_model_frompol %>%
  gather(wave, value, -pid, -ideo, -model, -case) %>%
  dplyr::group_by(ideo, wave) %>%
  dplyr::summarize(sum=sum(value)) %>%
  dplyr::group_by(ideo) %>%
  dplyr::summarize(mean(sum))

assess_it<-full_model_frompol %>%
  rowid_to_column(var="res_order") %>%
  gather(wave, value, -pid, -ideo, -model, -case, -res_order) %>%
  dplyr::group_by(ideo, pid, res_order) %>%
  dplyr::summarize(tot_value=sum(value)) %>%
  dplyr::group_by(ideo) %>%
  dplyr::mutate(tot_ideo=sum(tot_value), prop_ideo=tot_value/tot_ideo) %>%
  ungroup() %>%
  dplyr::mutate(tot=sum(tot_value), prop_tot=tot_value/tot) %>%
  arrange(res_order)


background_model_pol<-full_model_frompol %>%
  gather(wave, value, -ideo, -pid, -model, -case) %>%
  mutate(model=NULL) %>%
  group_by(wave, case) %>%
  nest() %>%
  mutate(correlation=map(data, cordf)) %>%
  dplyr::select(-data) %>%
  unnest()%>%
  separate(wave, into=c("var", "wave"))%>%
  mutate(wave=as.numeric(wave))

both_res<-bind_rows(stable_pid_frompol,
                    stable_ideo_frompol)

both_res %>%
  gather(wave, value, -ideo, -pid, -model, -case) %>%
  group_by(model, wave, case) %>%
  nest() %>%
  mutate(correlation=map(data, cordf)) %>%
  dplyr::select(-data) %>%
  unnest() %>%
  separate(wave, into=c("var", "wave"))%>%
  mutate(wave=as.numeric(wave))%>%
  ggplot(aes(wave, correlation))+
  facet_wrap(~model)+
  geom_point(data=background_model_pol, colour="grey")+
  geom_line(data=background_model_pol, colour="grey") +
  ylab("ideology-party id correlation")+
  geom_point()+
  geom_line() +
  theme_bw() 

init_pop_equi<-t(t(c(lab_left=447, nopid_left=439, con_left=37, lab_centre=2495, nopid_centre=1000, con_centre=700, lab_right=500, nopid_right=1000, con_right=1500)))

init_pop_equi<-t(t(c(lab_left=447, nopid_left=439, con_left=37, lab_centre=2495, nopid_centre=1000, con_centre=700, lab_right=500, nopid_right=1000, con_right=1500)))
init_pop_equi[,1]<-round(popbio::stable.stage(t_p)*10000)
full_model_fromequi<-popmodel(init_pop = init_pop_equi, t_p, 6) %>%
  mutate(model="full",case="both required")
stable_ideo_fromequi<-popmodel(init_pop = init_pop_equi, t_i, 6)%>%
  mutate(model="stable ideology; changing partisanship", case="both required")
stable_pid_fromequi<-popmodel(init_pop = init_pop_equi, t_pid, 6)%>%
  mutate(model="stable partisanship; changing ideology", case="both required")

background_model_equi<-full_model_fromequi %>%
  gather(wave, value, -ideo, -pid, -model, -case) %>%
  mutate(model=NULL) %>%
  group_by(wave, case) %>%
  nest() %>%
  mutate(correlation=map(data, cordf)) %>%
  dplyr::select(-data) %>%
  unnest()%>%
  separate(wave, into=c("var", "wave"))%>%
  mutate(wave=as.numeric(wave))

both_res<-bind_rows(stable_pid_fromequi,
                    stable_ideo_fromequi)


init_pop_depol<-t(t(c(lab_left=200, nopid_left=200, con_left=1000, lab_centre=200, nopid_centre=1000, con_centre=200, lab_right=1000, nopid_right=200, con_right=200)))

full_model_fromdepol<-popmodel(init_pop = init_pop_depol, t_p, 6) %>%
  mutate(model="full", case="mainly partisanship")
stable_ideo_fromdepol<-popmodel(init_pop = init_pop_depol, t_i, 6)%>%
  mutate(model="stable ideology; changing partisanship", case="mainly partisanship")
stable_pid_fromdepol<-popmodel(init_pop = init_pop_depol, t_pid, 6)%>%
  mutate(model="stable partisanship; changing ideology", case="mainly partisanship")

background_model_depol<-full_model_fromdepol %>%
  gather(wave, value, -ideo, -pid, -model, -case) %>%
  mutate(model=NULL) %>%
  group_by(wave, case) %>%
  nest() %>%
  mutate(correlation=map(data, cordf)) %>%
  dplyr::select(-data) %>%
  unnest()%>%
  separate(wave, into=c("var", "wave"))%>%
  mutate(wave=as.numeric(wave))

background_model<-bind_rows(background_model_pol,
                            background_model_equi,
                            background_model_depol) %>%
  mutate(wave=wave+1) %>%
  mutate(case=factor(case, levels=c("mainly ideology (actual)", "mainly partisanship", "both required")))

both_res<-bind_rows(stable_pid_frompol,
                    stable_ideo_frompol,
                    stable_pid_fromequi,
                    stable_ideo_fromequi,
                    stable_pid_fromdepol,
                    stable_ideo_fromdepol
)

##----Appendix figure 2 ----
pdf(paste0(i.path, "/en_which_causes_partisanpolarization.pdf"), height=7)
both_res %>%
  gather(wave, value, -ideo, -pid, -model, -case) %>%
  group_by(model, wave, case) %>%
  nest() %>%
  mutate(correlation=map(data, cordf)) %>%
  dplyr::select(-data) %>%
  unnest() %>%
  separate(wave, into=c("var", "wave"))%>%
  mutate(wave=as.numeric(wave))%>%
  mutate(wave=wave+1) %>%
  mutate(case=factor(case, levels=c("mainly ideology (actual)", "mainly partisanship", "both required"))) %>%
  ggplot(aes(wave, correlation))+
  facet_grid(case~model, scales = "free")+
  geom_point(data=background_model, colour="grey")+
  geom_line(data=background_model, colour="grey") +
  ylab("ideology-party id correlation")+
  geom_point()+
  geom_line() +
  theme_bw() 
dev.off()


init_pop_labour<-t(t(c(lab_left=init_pop_optim_size[["lab_left", 1]], nopid_left=0, con_left=0, lab_centre=init_pop_optim_size[["lab_centre", 1]], nopid_centre=0, con_centre=0, lab_right=init_pop_optim_size[["lab_right", 1]], nopid_right=0, con_right=0)))
init_pop_labourmatch<-t(t(c(lab_left=0, nopid_left=init_pop_optim_size[["lab_left", 1]], con_left=0, lab_centre=0, nopid_centre=init_pop_optim_size[["lab_centre", 1]], con_centre=0, lab_right=0, nopid_right=init_pop_optim_size[["lab_right", 1]], con_right=0)))

init_pop_con<-t(t(c(lab_left=0, nopid_left=0, con_left=init_pop_optim_size[["con_left", 1]], lab_centre=0, nopid_centre=0, con_centre=init_pop_optim_size[["con_centre", 1]], lab_right=0, nopid_right=0, con_right=init_pop_optim_size[["con_right", 1]])))
init_pop_conmatch<-t(t(c(lab_left=0, nopid_left=init_pop_optim_size[["con_left", 1]], con_left=0, lab_centre=0, nopid_centre=init_pop_optim_size[["con_centre", 1]], con_centre=0, lab_right=0, nopid_right=init_pop_optim_size[["con_right", 1]],con_right=0)))

labour_dynamics<-popmodel(init_pop = init_pop_labour, t_p, 6) %>%
  mutate(model=" partisans", group="lab")
labourmatch_dynamics<-popmodel(init_pop = init_pop_labourmatch, t_p, 6) %>%
  mutate(model="match", group="lab match")
con_dynamics<-popmodel(init_pop = init_pop_con, t_p, 6) %>%
  mutate(model=" partisans", group="con")
conmatch_dynamics<-popmodel(init_pop = init_pop_conmatch, t_p, 6) %>%
  mutate(model="match", group="con match")

matched_res<-bind_rows(labour_dynamics,
                       labourmatch_dynamics,
                       con_dynamics,
                       conmatch_dynamics)
ideo_to_numeric<-function(x){
  ifelse(x=="left", 1, 
         ifelse(x=="right", 0,
                ifelse(x=="centre", .5, NA_real_)))
}

group_to_process<-function(x){
  case_when(
    x %in% c("lab", "lab match") ~ "lab",
    x %in% c("con", "con match") ~ "con"
  )
}
##----Appendix figure 3----
pdf(paste0(i.path, "/en_matched_ideo_depolarize.pdf"), height=3)
matched_res %>%
  mutate(pid=group_to_process(group)) %>%
  gather(wave, value, -ideo, -pid, -model, -group) %>%
  dplyr::select(-group) %>%
  group_by(model, wave) %>%
  nest() %>%
  mutate(result=map(data, cordf)) %>%
  dplyr::select(-data) %>%
  unnest() %>%
  separate(wave, into=c("var", "wave"))%>%
  mutate(wave=as.numeric(wave)+1)%>%
  ggplot(aes(wave, result))+
  facet_grid(.~model)+
  geom_point()+
  geom_line()+
  ylab("ideology-group correlation") +
  theme_bw()+
  theme(legend.title = element_blank())
dev.off()
