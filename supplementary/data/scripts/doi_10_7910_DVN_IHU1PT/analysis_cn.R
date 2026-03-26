###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##    Selective Protection in China
##    Duy Trinh
##    Created date: 02/19/2019
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# LOAD DATA #####
#################

load("../Datasets/Trinh_China_selective_protection.RData")

# ANALYSIS #####
################

# Descriptive statistics #####

## Descriptive stats for province-level IVs and DVs ####
# Table A6
stargazer(subset(df.count, select=c(case_count,
                                    sanction_count,
                                    tie_incumbent,
                                    acc,corrupexp,BureauIntegr,revenue_logged,population_logged,transfer_share)),
          summary.stat=c("n","mean","sd","min","max"), digits=2,
          covariate.labels=c("Number of individuals investigated",
                             "Number of investigation cases",
                             "Province's ties to incumbent GS",
                             "Anti-corruption campaign dummy",
                             "Corruption experience",
                             "Bureaucratic integration",
                             "Government revenue (logged)",
                             "Population (logged)",
                             "Central-to-local transfer (share of total revenue)"
                             ),
          type = "text",
          title="Descriptive statistics for China's provincial covariates",
          out=c("../Tables/desc_stat_cn.txt",
                "../Tables/desc_stat_cn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="desc_stat_cn")

## Descriptive stats for individual-level IVs and DVs ####
# Table A7
dfcox %>%
  select(case_advanced,
         date,
         tie_incumbent,
         status_type,gender,age,local,retired) %>%
  mutate(case_advanced = as.numeric(case_advanced),
         status_type = ifelse(status_type=="Tiger",1,0),
         gender = ifelse(gender=="Male",1,0),
         local = as.numeric(local),
         retired = as.numeric(retired)) %>%
  as.data.frame () %>%
  stargazer(summary.stat=c("n","mean","sd","min","max"), digits=2,
            covariate.labels=c("Case results in sanction dummy",
                               "Investigation duration (days)",
                               "Province's ties to incumbent GS",
                               "Tiger status",
                               "Male",
                               "Age",
                               "Native official dummy",
                               "Official left investigated position dummy"
                               ),
            type = "text",
            title="Descriptive statistics for China's individual covariates",
            out=c("../Tables/desc_stat_cn_ind.txt",
                  "../Tables/desc_stat_cn_ind.tex"),
            font.size = "scriptsize",
            column.sep.width = "1pt",
            label="desc_stat_cn_ind")

## Number of individuals investigated by factional affiliation ####
# Table A8
df.count %>%
  filter(!is.na(tie_incumbent)) %>%
  group_by(investigated_year, tie_incumbent) %>%
  summarize(sanction_count = sum(sanction_count)) %>%
  pivot_wider(names_from = "tie_incumbent",
              values_from = "sanction_count") %>%
  mutate(investigated_year = as.character(investigated_year)) %>%
  as.data.frame() %>%
  stargazer(summary = F,
            rownames = F,
            covariate.labels = c("Year","Individual count (no tie)","Individual count (tie)"),
            type = "text",
            title="Number of investigated officials in China, by year and factional ties to incumbent GS",
            out=c("../Tables/desc_cn_invest_ind_over_time.txt",
                  "../Tables/desc_cn_invest_ind_over_time.tex"),
            font.size = "scriptsize",
            column.sep.width = "1pt",
            label="desc_cn_invest_ind_over_time")

# Figure A3 - top
df %>%
  filter(!is.na(tie_incumbent)) %>%
  group_by(investigated_year,tie_incumbent) %>%
  dplyr::summarize(count=n()) %>%
  ggplot(aes(x=investigated_year, y=count,fill=factor(tie_incumbent))) +
  geom_bar(stat="identity") +
  theme_classic(base_size=24) +
  theme(legend.position="bottom") +
  scale_fill_brewer(palette="Set1",name="Ties to incumbent GS", labels=c("No","Yes")) +
  labs(title="Number of individuals investigated",
       x="Year",y="Number of individuals")
ggsave("../Graphs/Investigations_tie_incumbent_cn.png", width=20,height=10,units="in")

## Number of cases investigated by factional affiliation ####
# Table A9
df.count %>%
  filter(!is.na(tie_incumbent)) %>%
  group_by(investigated_year, tie_incumbent) %>%
  summarize(case_count = sum(case_count)) %>%
  pivot_wider(names_from = "tie_incumbent",
              values_from = "case_count") %>%
  mutate(investigated_year = as.character(investigated_year)) %>%
  as.data.frame() %>%
  stargazer(summary = F,
            rownames = F,
            covariate.labels = c("Year","Case count (no tie)","Case count (tie)"),
            type = "text",
            title="Number of investigation cases in China, by year and factional ties to incumbent GS",
            out=c("../Tables/desc_cn_invest_case_over_time.txt",
                  "../Tables/desc_cn_invest_case_over_time.tex"),
            font.size = "scriptsize",
            column.sep.width = "1pt",
            label="desc_cn_invest_case_over_time")

# Figure A3 - bottom
df.count %>%
  filter(!is.na(tie_incumbent)) %>%
  ggplot(aes(x=investigated_year, y=case_count,fill=factor(tie_incumbent))) +
  geom_bar(stat="identity") +
  theme_classic(base_size=24) +
  theme(legend.position="bottom") +
  scale_fill_brewer(palette="Set1",name="Province's ties to incumbent GS", labels=c("No","Yes")) +
  labs(title="Number of investigation cases",
       x="Year",y="Number of cases")
ggsave("../Graphs/Cases_tie_incumbent_cn.png", width=20,height=10,units="in")

## Lengths of investigations by factional affiliation ####
# Figure A4
ggplot(df, aes(x=Inv_to_Sanc)) +
  geom_histogram(data=subset(df, tie_incumbent==0), aes(fill="No")) +
  geom_histogram(data=subset(df, tie_incumbent==1), aes(fill="Yes")) +
  geom_vline(xintercept=mean(df$Inv_to_Sanc[df$tie_incumbent==0], na.rm=T),linetype="dashed", size=2) +
  geom_vline(xintercept=mean(df$Inv_to_Sanc[df$tie_incumbent==1], na.rm=T),linetype="dotted", size=2) +
  theme_classic(base_size=28) +
  theme(legend.position="bottom") +
  scale_fill_manual(name="Province's ties to incumbent GS",values=c(No="#e41a1c",Yes="#377eb8")) +
  labs(title="Duration of investigations conducted by China's CCDI",
       x="Days",y="Number of individuals")
ggsave("../Graphs/Investigations_speed_histogram_incumbent_cn.png", width=20,height=10,units="in")

## Factional affiliation by province and date ####
# Figure A6
panelview(investigated_year~tie_incumbent,
          data = subset(elites_prov_cn, investigated_year>=1997),
          index = c("corruption_location", "investigated_year"),
          xlab = "Year", ylab = "Province",
          main = "",
          background = "white",
          legend.labs = c("No", "Yes",""),
          cex.main=28, cex.main.sub = 24, cex.axis=16, cex.lab=24, cex.legend=24)
ggsave("../Graphs/Ties_incumbent_province_year_cn.png", width=20,height=10,units="in")


# Ex ante protection ####

## Main regression ####
count1 <- lm(case_count~tie_incumbent+corrupexp+acc, data=df.count)
count1.r <- coeftest(count1, vcov=vcovHC, type="HC1")
count1.r
count2 <- lm(case_count~tie_incumbent+corrupexp+acc+revenue_logged+population_logged+transfer_share, data=df.count)
count2.r <- coeftest(count2, vcov=vcovHC, type="HC1")
count2.r
count3 <- lm(case_count~tie_incumbent+corrupexp+acc+revenue_logged+population_logged+transfer_share+factor(investigated_year), data=df.count)
count3.r <- coeftest(count3, vcov=vcovHC, type="HC1")
count3.r

count4 <- lm(sanction_count~tie_incumbent+corrupexp+acc, data=df.count)
count4.r <- coeftest(count4, vcov=vcovHC, type="HC1")
count4.r
count5 <- lm(sanction_count~tie_incumbent+corrupexp+acc+revenue_logged+population_logged+transfer_share, data=df.count)
count5.r <- coeftest(count5, vcov=vcovHC, type="HC1")
count5.r
count6 <- lm(sanction_count~tie_incumbent+corrupexp+acc+revenue_logged+population_logged+transfer_share+factor(investigated_year), data=df.count)
count6.r <- coeftest(count6, vcov=vcovHC, type="HC1")
count6.r

# Table 3
stargazer(count1,count2,count3,count4,count5,
          count6,
          keep.stat = c("n", "adj.rsq"),
          omit="factor",
          add.lines=list(c("Year FE",rep("",2),"Yes",rep("",2),"Yes")),
          se=list(count1.r[,2],count2.r[,2],count3.r[,2],count4.r[,2],count5.r[,2],
                  count6.r[,2]),
          order=c("^tie.incumbent"),
          dep.var.labels = c("No. of investigations","No. of individuals investigated"),
          covariate.labels = c("Province's ties to incumbent GS",
                               "Corruption experience",
                               "Anti-corruption campaign",
                               "Revenue (logged)",
                               "Population (logged)",
                               "Transfer from center (share of revenue)"),
          digits=3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title="Factional ties to incumbent CCP GS uncorrelated with frequency of investigations",
          out=c("../Tables/regress_exante_main_cn.txt",
                "../Tables/regress_exante_main_cn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="regress_exante_main_cn")

results.count <- data.frame(model=c(rep("Model 1",nrow(count1.r)),
                                    rep("Model 2",nrow(count2.r)),
                                    rep("Model 3",nrow(count3.r)),
                                    rep("Model 4",nrow(count4.r)),
                                    rep("Model 5",nrow(count5.r)),
                                    rep("Model 6",nrow(count6.r))),
                            variable = c(rownames(count1.r),rownames(count2.r),
                                         rownames(count3.r),rownames(count4.r),
                                         rownames(count5.r),rownames(count6.r)),
                            coef=c(count1.r[,1],count2.r[,1],count3.r[,1],
                                   count4.r[,1],count5.r[,1],count6.r[,1]),
                            se=c(count1.r[,2],count2.r[,2],count3.r[,2],
                                 count4.r[,2],count5.r[,2],count6.r[,2])) %>%
  subset(!grepl("corruption|Intercept|native|factor",variable))
results.count$variable <- factor(results.count$variable,
                                     levels=rev(c("tie_incumbent","corrupexp","accTRUE","revenue_logged",
                                                  "population_logged","transfer_share")),
                                     labels=rev(c("Province's ties to incumbent CCP GS",
                                                  "Corruption experience",
                                                  "Anti-corruption campaign",
                                                  "Revenue (logged)",
                                                  "Population (logged)",
                                                  "Transfer from center (share of revenue)")))
# Figure 3
results.count %>%
  ggplot(aes(x=variable, y=coef, color=model)) +
  geom_point(size=4, position=position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin=coef-se*1.96, ymax=coef+se*1.96),
                width=0, size=2,
                position=position_dodge(width=0.5)) +
  geom_hline(yintercept = 0, colour ="black", lty = 2, lwd=1) +
  theme_classic(base_size=28) +
  theme(legend.title=element_blank()) +
  scale_color_brewer(palette="Set1") +
  labs(title="",
       x="",y="Annual count of formal investigations") +
  coord_flip()
ggsave("../Graphs/coef_count_cn.png", width=15,height=10,units="in")

# Ex post protection ####

## Status ####
status1 <- lm(case_advanced~tie_incumbent, data=df)
status1.r <- coeftest(status1, vcov=vcovHC, type="HC1")
status1.r
status2 <- lm(case_advanced~tie_incumbent+status_type+gender+age+local+retired, data=df)
status2.r <- coeftest(status2, vcov=vcovHC, type="HC1")
status2.r
status3 <- lm(case_advanced~tie_incumbent+status_type+gender+age+local+retired+corrupexp+revenue_logged+population_logged+transfer_share, df)
status3.r <- coeftest(status3, vcov=vcovHC, type="HC1")
status3.r
status4 <- lm(case_advanced~tie_incumbent+year_f+status_type+gender+age+local+retired+corrupexp+revenue_logged+population_logged+transfer_share, df)
status4.r <- coeftest(status4, vcov=vcovHC, type="HC1")
status4.r

# Table 4
stargazer(status1,status2,status3,status4,
          omit="year_*|native.*|corruption.*",
          add.lines=list(c("Year FE",rep("",3),"Yes")),
          keep.stat = c("n", "adj.rsq"),
          se=list(status1.r[,2],status2.r[,2],status3.r[,2],status4.r[,2]),
          order=c("^tie.incumbent"),
          dep.var.labels = "Probability of jail sentence or expulsion",
          covariate.labels = c("Province's ties to incumbent GS",
                               "Tiger","Male","Age","Native official","Retired",
                               "Corruption experience",
                               "Revenue (logged)",
                               "Population (logged)",
                               "Transfer from center (share of revenue)"),
          digits=3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title="Factional ties to incumbent CCP GS associated with lower likelihood of serious punishment",
          out=c("../Tables/regress_expost_lpm_cn.txt",
                "../Tables/regress_expost_lpm_cn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="regress_expost_lpm_cn")

results.status <- data.frame(model=c(rep("Model 1",length(status1$coef)),
                                    rep("Model 2",length(status2$coef)),
                                    rep("Model 3",length(status3$coef)),
                                    rep("Model 4",length(status4$coef))),
                            variable = c(names(status1$coef),names(status2$coef),
                                         names(status3$coef),names(status4$coef)),
                            coef=c(status1$coef,status2$coef,status3$coef,
                                   status4$coef),
                            se=c(status1.r[,2],status2.r[,2],status3.r[,2],
                                 status4.r[,2])) %>%
  subset(!grepl("corruption|Intercept|native|year",variable))
results.status$variable <- factor(results.status$variable,
                                  levels=rev(c("tie_incumbent","investigated_date","status_typeTiger","genderMale","age","localTRUE","retiredTRUE",
                                               "corrupexp","revenue_logged","population_logged","transfer_share")),
                                  labels=rev(c("Province's ties to incumbent GS",
                                               "Time trend (day)",
                                               "Tiger","Male","Age","Native official","Retired",
                                               "Corruption experience",
                                               "Revenue (logged)",
                                               "Population (logged)",
                                               "Transfer from center (share of revenue)")))
# Figure not included in manuscript
results.status %>%
  ggplot(aes(x=variable, y=coef, color=model)) +
  geom_point(size=4, position=position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin=coef-se*1.96, ymax=coef+se*1.96),
                width=0, size=2,
                position=position_dodge(width=0.5)) +
  geom_hline(yintercept = 0, colour ="black", lty = 2, lwd=1) +
  theme_classic(base_size=28) +
  theme(legend.title=element_blank()) +
  scale_color_brewer(palette="Set1") +
  labs(title="Effect of factional ties on \nsanction level in the CCP",
       x="",y="Probability of sentence/expulsion") +
  coord_flip()
ggsave("../Graphs/coef_status_cn.png", width=16,height=10,units="in")

# Predicted probabilities plot
new.df <- model.frame(status3) %>% mutate(tie_incumbent=as.numeric(tie_incumbent==0))

status.predict.plot <- data.frame("predicted"=c(predict(update(status3, na.action=na.exclude),newdata = model.frame(status3)),
                                                predict(update(status3, na.action=na.exclude),newdata = new.df)),
                                  "tie_incumbent_org"=rep(model.frame(status3)$tie_incumbent,2),
                                  "tie_incumbent_new"=c(model.frame(status3)$tie_incumbent,new.df$tie_incumbent)) %>%
  subset(!is.na(tie_incumbent_org))
status.predict.plot$tie_incumbent_new <- ifelse(status.predict.plot$tie_incumbent_org==status.predict.plot$tie_incumbent_new,
                                      "Original","Switched")
status.predict.plot$tie_incumbent_org <- ifelse(status.predict.plot$tie_incumbent_org==0,"Without factional ties","With factional ties")

# Figure 4
status.predict.plot %>%
  subset(tie_incumbent_new=="Original") %>%
  ggplot(aes(x=tie_incumbent_org,y=predicted)) +
  geom_boxplot(fill="#e41a1c", width=0.5) +
  theme_classic(base_size=28) +
  theme(legend.title=element_blank()) +
  scale_fill_brewer(palette="Set2") +
  ylim(c(0,1)) +
  labs(title="",
       x="Province's factional ties to incumbent GS",y="Predicted probability")
ggsave("../Graphs/predict_caseadvanced_cn.png", width=10,height=10,units="in")

## Survival analysis ####
survival1 <- coxph(Surv(date, case_advanced) ~ tie_incumbent + investigated_date, data = dfcox)
survival1
survival2 <- coxph(Surv(date, case_advanced) ~ tie_incumbent + investigated_date + status_type+gender+age+local+retired, data = dfcox)
survival2
survival3 <- coxph(Surv(date, case_advanced) ~ tie_incumbent + investigated_date + status_type+gender+age+local+retired+corrupexp+revenue_logged+population_logged+transfer_share, data = dfcox)
survival3

# Table 5
stargazer(survival1,survival2,survival3,
          omit="native.*|corruption.*",
          keep.stat = c("n","rsq"),
          order=c("^tie_incumbent$"),
          dep.var.labels = "Expulsion/Sentence log hazard ratio",
          covariate.labels = c("Province's ties to incumbent GS",
                               "Time trend (day)",
                               "Tiger","Male","Age","Native official","Retired",
                               "Corruption experience",
                               "Revenue (logged)",
                               "Population (logged)",
                               "Transfer from center (share of revenue)"),
          digits=3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title="Factional ties to incumbent CCP GS associated with longer investigation delays",
          out=c("../Tables/regress_expost_cox_cn.txt",
                "../Tables/regress_expost_cox_cn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="regress_expost_cox_cn")

# Create coef plot
results.survival1 <- data.frame(model=c(rep("Model 1",length(survival1$coef)),
                                     rep("Model 2",length(survival2$coef)),
                                     rep("Model 3",length(survival3$coef))),
                             variable = c(names(survival1$coef),names(survival2$coef),
                                          names(survival3$coef)),
                             coef=c(survival1$coef,survival2$coef,survival3$coef),
                             se=c(sqrt(diag(survival1$var)),sqrt(diag(survival2$var)),sqrt(diag(survival3$var)))) %>% subset(!grepl("corruption|Intercept|native",variable))
results.survival1$variable <- factor(results.survival1$variable,
                                  levels=rev(c("tie_incumbent","investigated_date","status_typeTiger","genderMale","age","localTRUE","retiredTRUE",
                                               "corrupexp","revenue_logged","population_logged","transfer_share")),
                                  labels=rev(c("Province's ties to incumbent GS",
                                               "Time trend (day)",
                                               "Tiger","Male","Age","Native official","Retired",
                                               "Corruption experience",
                                               "Revenue (logged)",
                                               "Population (logged)",
                                               "Transfer from center (share of revenue)")))
# Figure not included in manuscript
results.survival1 %>%
  ggplot(aes(x=variable, y=coef, color=model)) +
  geom_point(size=4, position=position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin=coef-se*1.96, ymax=coef+se*1.96),
                width=0, size=2,
                position=position_dodge(width=0.5)) +
  geom_hline(yintercept = 0, colour ="black", lty = 2, lwd=1) +
  theme_classic(base_size=28) +
  theme(legend.title=element_blank()) +
  scale_color_brewer(palette="Set1") +
  labs(title="",
       x="",y="Expulsion/Sentence hazard ratio") +
  coord_flip()
ggsave("../Graphs/coef_survival_cn.png", width=15,height=10,units="in")

# Create survival plot
data_fit <- dfcox %>%
  ungroup() %>%
  summarise_at(.vars=c("investigated_date","revenue_logged","population_logged","transfer_share","age","corrupexp"), mean, na.rm=T) %>%
  cbind(data.frame("tie_incumbent"=c(0,1),"status_type"="Fly","gender"="Male","local"=TRUE,"retired"=FALSE))

# Figure 5
ggsurvplot(survfit(survival3, newdata =  data_fit),
           data=data_fit,conf.int = T, conf.int.alpha = 0.2,conf.int.style="ribbon",
           ggtheme = theme_classic( base_size=28), palette = "Set1",
           xlab="Number of days", break.x.by=500,
           title="",
           legend.title="Ties to incumbent GS", legend.labs=c("No", "Yes"),
           xlim=c(0,2000))
ggsave("../Graphs/Survival_plot_mean_cn.png", width=16,height=8,units="in")

# ROBUSTNESS CHECKS ####
########################

# Ex ante protection ####

## Case count ####
count3 %>% summary()
count3.r

### Replace logged revenue with alternative measures ####
check.count3a <- update(count3, . ~. - revenue_logged +expenditure_logged)
check.count3a.r <- coeftest(check.count3a, vcov=vcovHC, type="HC1")
check.count3a.r

check.count3b <- update(count3, . ~. - revenue_logged +log(revenue_l1y))
check.count3b.r <- coeftest(check.count3b, vcov=vcovHC, type="HC1")
check.count3b.r

check.count3c <- update(count3, . ~. - revenue_logged +log(expenditure_l1y))
check.count3c.r <- coeftest(check.count3c, vcov=vcovHC, type="HC1")
check.count3c.r

# Table A33
stargazer(count3,check.count3a,check.count3b,check.count3c,
          keep.stat = c("n","adj.rsq"),
          omit="factor",
          table.placement = "H",
          add.lines=list(c("Year FE",rep("Yes",4))),
          se=list(count3.r[,2],check.count3a.r[,2],check.count3b.r[,2],
                  check.count3c.r[,2]),
          order=c("^tie.incumbent"),
          dep.var.labels = c("No. of investigations"),
          covariate.labels = c("Province's ties to incumbent GS",
                               "Corruption experience",
                               "Anti-corruption campaign",
                               "Revenue (logged)",
                               "Population (logged)",
                               "Transfer from center (share of revenue)",
                               "Expenditure (logged)",
                               "Revenue (logged, lagged)",
                               "Expenditure (logged, lagged)"
                               ),
          digits=3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title="No evidence of ex ante protection in China across all specifications of economic controls",
          out=c("../Tables/robust_exante_alt_econ_cn.txt",
                "../Tables/robust_exante_alt_econ_cn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="robust_exante_alt_econ_cn")

### Replace corrupexp with alternative measures ####
check.count3j <- update(count3, . ~. - corrupexp + corruppercep)
check.count3j.r <- coeftest(check.count3j, vcov=vcovHC, type="HC1")
check.count3j.r

check.count3k <- update(count3, . ~. - corrupexp + corruption2)
check.count3k.r <- coeftest(check.count3k, vcov=vcovHC, type="HC1")
check.count3k.r

check.count3l <- update(count3, . ~. - corrupexp + corruption3)
check.count3l.r <- coeftest(check.count3l, vcov=vcovHC, type="HC1")
check.count3l.r

# Table A34
stargazer(count3,check.count3j,check.count3k,check.count3l,
          keep.stat = c("n","adj.rsq"),
          omit="factor",
          table.placement = "H",
          add.lines=list(c("Year FE",rep("Yes",4))),
          se=list(count3.r[,2],check.count3j.r[,2],check.count3k.r[,2],
                  check.count3l.r[,2]),
          order=c("^tie.incumbent"),
          dep.var.labels = c("No. of investigations","No. of individuals investigated"),
          covariate.labels = c("Province's ties to incumbent GS",
                               "Corruption experience",
                               "Anti-corruption campaign",
                               "Revenue (logged)",
                               "Population (logged)",
                               "Transfer from center (share of revenue)",
                               "Perceived corruption",
                               "Recovered corrupt funds per Capita (log)",
                               "Senior cadres disciplined per 10,000 public employees (log)"
                               ),
          digits=3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title="No evidence of ex ante protection in China across alternative measures of corruption levels. All corruption measures are taken from Zhu (2017)",
          out=c("../Tables/robust_exante_alt_corr_cn.txt",
                "../Tables/robust_exante_alt_corr_cn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="robust_exante_alt_corr_cn")


### Replace transfer_share with alternative measures ####
check.count3m <- update(count3, . ~. - transfer_share + BureauIntegr)
check.count3m.r <- coeftest(check.count3m, vcov=vcovHC, type="HC1")
check.count3m.r

# Table A35
stargazer(count3,check.count3m,
          keep.stat = c("n","adj.rsq"),
          omit="factor",
          table.placement = "H",
          add.lines=list(c("Year FE",rep("Yes",2))),
          se=list(count3.r[,2],check.count3m.r[,2]),
          order=c("^tie.incumbent"),
          dep.var.labels = c("No. of investigations","No. of individuals investigated"),
          covariate.labels = c("Province's ties to incumbent GS",
                               "Corruption experience",
                               "Anti-corruption campaign",
                               "Revenue (logged)",
                               "Population (logged)",
                               "Transfer from center (share of revenue)",
                               "Bureaucratic integration"
                               # "Perceived corruption",
                               # "Recovered corrupt funds per Capita (log)",
                               # "Senior cadres disciplined per 10,000 public employees (log)"
          ),
          digits=3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title="No evidence of ex ante protection in China across alternative measures of central-local independence",
          out=c("../Tables/robust_exante_alt_inte_cn.txt",
                "../Tables/robust_exante_alt_inte_cn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="robust_exante_alt_inte_cn")

### Dummy for Beijing and Shanghai ####
df.count$BJ_SH <- df.count$corruption_location %in% c("Beijing","Shanghai")
table(df.count$BJ_SH)
check.count3n <- update(count3, . ~ . + BJ_SH)
check.count3n.r <- coeftest(check.count3n, vcov=vcovHC, type="HC1")
check.count3n.r

### Dummy for whether Party Secretary is also in Politburo ####
df.count$PS_Politburo <- (df.count$investigated_year %in% 2007:2018 & df.count$corruption_location %in% c("Guangdong","Shanghai","Tianjin","Xinjiang","Chongqing")) |
  (df.count$investigated_year %in% 2002:2018 & df.count$corruption_location %in% c("Guangdong","Shanghai","Tianjin","Xinjiang","Hubei")) |
  (df.count$investigated_year %in% 2002:2018 & df.count$corruption_location %in% c("Guangdong","Shandong","Beijing","Shanghai"))
table(df.count$PS_Politburo)
check.count3o <- update(count3, . ~ . + PS_Politburo)
check.count3o.r <- coeftest(check.count3o, vcov=vcovHC, type="HC1")
check.count3o.r

# Table A36
stargazer(count3,check.count3n,check.count3o,
          keep.stat = c("n","adj.rsq"),
          omit="factor",
          table.placement = "H",
          add.lines=list(c("Year FE",rep("Yes",3))),
          se=list(count3.r[,2],check.count3n.r[,2],
                  check.count3o.r[,2]),
          order=c("^tie.incumbent"),
          dep.var.labels = c("No. of investigations","No. of individuals investigated"),
          covariate.labels = c("Province's ties to incumbent GS",
                               "Corruption experience",
                               "Anti-corruption campaign",
                               "Revenue (logged)",
                               "Population (logged)",
                               "Transfer from center (share of revenue)",
                               "Beijing and Shanghai dummy",
                               "Party Secretary in Politburo"
                               # "Perceived corruption",
                               # "Recovered corrupt funds per Capita (log)",
                               # "Senior cadres disciplined per 10,000 public employees (log)"
          ),
          digits=3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title="No robust evidence of ex ante protection in China across alternative measures of local leaders' clout",
          out=c("../Tables/robust_exante_alt_localpwr_cn.txt",
                "../Tables/robust_exante_alt_localpwr_cn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="robust_exante_alt_localpwr_cn")

### Ties with incumbent Premier and CCDI chief ####
check.count3t <- update(count3, . ~ . -tie_incumbent+tie_incumbent_premier)
check.count3t.r <- coeftest(check.count3t, vcov=vcovHC, type="HC1")
check.count3t.r

check.count3u <- update(count3, . ~ . -tie_incumbent+tie_incumbent_ccdi)
check.count3u.r <- coeftest(check.count3u, vcov=vcovHC, type="HC1")
check.count3u.r

check.count3v <- update(count3, . ~ . -tie_incumbent+tie_previous)
check.count3v.r <- coeftest(check.count3v, vcov=vcovHC, type="HC1")
check.count3v.r

# Table A37
stargazer(count3,check.count3t,check.count3u,check.count3v,
          keep.stat = c("n","adj.rsq"),
          omit="factor",
          table.placement = "H",
          add.lines=list(c("Year FE",rep("Yes",4))),
          se=list(count3.r[,2],check.count3t.r[,2],
                  check.count3u.r[,2],check.count3v.r[,2]),
          order=c("^tie."),
          dep.var.labels = c("No. of investigations","No. of individuals investigated"),
          covariate.labels = c("Province's ties to incumbent GS",
                               "Province's ties to incumbent Premier",
                               "Province's ties to incumbent CCDI head",
                               "Province's ties to previous GS",
                               "Corruption experience",
                               "Anti-corruption campaign",
                               "Revenue (logged)",
                               "Population (logged)",
                               "Transfer from center (share of revenue)"
          ),
          digits=3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title="No evidence of ex ante protection in China by other elites",
          out=c("../Tables/robust_exante_alt_ties_cn.txt",
                "../Tables/robust_exante_alt_ties_cn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="robust_exante_alt_ties_cn")

### Interact ACC with factional tie ####
check.count3w <- update(count3, data = df.count %>% filter(investigated_year>2012))
check.count3w.r <- coeftest(check.count3w, vcov=vcovHC, type="HC1")
check.count3w.r
check.count3w2 <- update(count3, . ~ . +tie_incumbent*acc)
check.count3w2.r <- coeftest(check.count3w2, vcov=vcovHC, type="HC1")
check.count3w2.r

# Table A38
stargazer(count3,check.count3w,check.count3w2,
          keep.stat = c("n","adj.rsq"),
          omit="factor",
          table.placement = "H",
          se=list(count3.r[,2],check.count3w.r[,2],check.count3w2.r[,2]),
          order=c("^tie."),
          dep.var.labels = c("No. of investigations"),
          covariate.labels = c("Province's ties to incumbent GS",
                               "Province's ties to incumbent GS x ACC",
                               "Corruption experience",
                               "Anti-corruption campaign",
                               "Revenue (logged)",
                               "Population (logged)",
                               "Transfer from center (share of revenue)"
          ),
          add.lines = list(c("","Original model","Only post-ACC obs","ACC x Ties"),
                           c("Year FE",rep("Yes",3))),
          digits=3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title="Factional ties to incumbent GS are linked with fewer cases only in pre-campaign period",
          out=c("../Tables/robust_exante_interact_acc_cn.txt",
                "../Tables/robust_exante_interact_acc_cn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="robust_exante_interact_acc_cn")

# Ex post protection ####

## Status ####
status4 %>% summary()
status4.r

### Replace logged revenue with alternative measures ####
check.status4a <- update(status4, . ~. - revenue_logged +expenditure_logged)
check.status4a.r <- coeftest(check.status4a, vcov=vcovHC, type="HC1")
check.status4a.r

check.status4b <- update(status4, . ~. - revenue_logged +log(revenue_l1y))
check.status4b.r <- coeftest(check.status4b, vcov=vcovHC, type="HC1")
check.status4b.r

check.status4c <- update(status4, . ~. - revenue_logged +log(expenditure_l1y))
check.status4c.r <- coeftest(check.status4c, vcov=vcovHC, type="HC1")
check.status4c.r

# Table A39
stargazer(status4,check.status4a,check.status4b,check.status4c,
          omit = c("year_f"),
          keep.stat = c("n","adj.rsq"),
          table.placement = "H",
          add.lines=list(c("Year FE",rep("Yes",4))),
          se=list(status4.r[,2],check.status4a.r[,2],check.status4b.r[,2],
                  check.status4c.r[,2]),
          order=c("^tie.incumbent"),
          dep.var.labels = "Probability of jail sentence or expulsion",
          covariate.labels = c("Province's ties to incumbent GS",
                               "Tiger","Male","Age","Native official","Retired",
                               "Corruption experience",
                               "Revenue (logged)",
                               "Population (logged)",
                               "Transfer from center (share of revenue)",
                               "Expenditure (logged)",
                               "Revenue (logged, lagged)",
                               "Expenditure (logged, lagged)"),
          digits=3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title="Robust evidence of ex post protection in China across all specifications of economic controls",
          out=c("../Tables/robust_expost_alt_econ_cn.txt",
                "../Tables/robust_expost_alt_econ_cn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="robust_expost_alt_econ_cn")

### Replace corrupexp with alternative measures ####
check.status4j <- update(status4, . ~. - corrupexp + corruppercep)
check.status4j.r <- coeftest(check.status4j, vcov=vcovHC, type="HC1")
check.status4j.r

check.status4k <- update(status4, . ~. - corrupexp + corruption2)
check.status4k.r <- coeftest(check.status4k, vcov=vcovHC, type="HC1")
check.status4k.r

check.status4l <- update(status4, . ~. - corrupexp + corruption3)
check.status4l.r <- coeftest(check.status4l, vcov=vcovHC, type="HC1")
check.status4l.r

# Table A40
stargazer(status4,check.status4j,check.status4k,check.status4l,
          omit = c("year_f"),
          keep.stat = c("n","adj.rsq"),
          table.placement = "H",
          add.lines=list(c("Year FE",rep("Yes",4))),
          se=list(status4.r[,2],check.status4j.r[,2],check.status4k.r[,2],
                  check.status4l.r[,2]),
          order=c("^tie.incumbent"),
          dep.var.labels = "Probability of jail sentence or expulsion",
          covariate.labels = c("Province's ties to incumbent GS",
                               "Tiger","Male","Age","Native official","Retired",
                               "Corruption experience",
                               "Revenue (logged)",
                               "Population (logged)",
                               "Transfer from center (share of revenue)",
                               "Perceived corruption",
                               "Recovered corrupt funds per Capita (log)",
                               "Senior cadres disciplined per 10,000 public employees (log)"
                               ),
          digits=3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title="Robust evidence of ex post protection in China across all specifications of corruption levels. All corruption measures are taken from Zhu (2017)",
          out=c("../Tables/robust_expost_alt_corr_cn.txt",
                "../Tables/robust_expost_alt_corr_cn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="robust_expost_alt_corr_cn")

### Replace transfer_share with alternative measures ####
check.status4m <- update(status4, . ~. - transfer_share + BureauIntegr)
check.status4m.r <- coeftest(check.status4m, vcov=vcovHC, type="HC1")
check.status4m.r

# Table A41
stargazer(status4,check.status4m,
          omit = c("year_f"),
          keep.stat = c("n","adj.rsq"),
          table.placement = "H",
          add.lines=list(c("Year FE",rep("Yes",2))),
          se=list(status4.r[,2],check.status4m.r[,2]),
          order=c("^tie.incumbent"),
          dep.var.labels = "Probability of jail sentence or expulsion",
          covariate.labels = c("Province's ties to incumbent GS",
                               "Tiger","Male","Age","Native official","Retired",
                               "Corruption experience",
                               "Revenue (logged)",
                               "Population (logged)",
                               "Transfer from center (share of revenue)",
                               "Bureaucratic integration"
          ),
          digits=3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title="Robust evidence of ex post protection in China across all specifications of central-local independence",
          out=c("../Tables/robust_expost_alt_inde_cn.txt",
                "../Tables/robust_expost_alt_inde_cn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="robust_expost_alt_inde_cn")

### Dummy for Beijing and Shanghai ####
df$BJ_SH <- df$corruption_location %in% c("Beijing","Shanghai")
table(df$BJ_SH)
check.status4n <- update(status4, . ~ . + BJ_SH)
check.status4n.r <- coeftest(check.status4n, vcov=vcovHC, type="HC1")
check.status4n.r

### Dummy for whether Party Secretary is also in Politburo ####
df$PS_Politburo <- (df$investigated_year %in% 2007:2018 & df$corruption_location %in% c("Guangdong","Shanghai","Tianjin","Xinjiang","Chongqing")) |
  (df$investigated_year %in% 2002:2018 & df$corruption_location %in% c("Guangdong","Shanghai","Tianjin","Xinjiang","Hubei")) |
  (df$investigated_year %in% 2002:2018 & df$corruption_location %in% c("Guangdong","Shandong","Beijing","Shanghai"))
table(df$PS_Politburo)
check.status4o <- update(status4, . ~ . + PS_Politburo)
check.status4o.r <- coeftest(check.status4o, vcov=vcovHC, type="HC1")
check.status4o.r

# Table A42
stargazer(status4,check.status4n,check.status4o,
          omit = c("year_f"),
          keep.stat = c("n","adj.rsq"),
          table.placement = "H",
          add.lines=list(c("Year FE",rep("Yes",3))),
          se=list(status4.r[,2],check.status4n.r[,2],check.status4o.r[,2]),
          order=c("^tie.incumbent"),
          dep.var.labels = "Probability of jail sentence or expulsion",
          covariate.labels = c("Province's ties to incumbent GS",
                               "Tiger","Male","Age","Native official","Retired",
                               "Corruption experience",
                               "Revenue (logged)",
                               "Population (logged)",
                               "Transfer from center (share of revenue)",
                               "Beijing and Shanghai dummy",
                               "Party Secretary in Politburo"
          ),
          digits=3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title="Robust evidence of ex post protection in China across all specifications of local leaders' clout",
          out=c("../Tables/robust_expost_alt_localpwr_cn.txt",
                "../Tables/robust_expost_alt_localpwr_cn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="robust_expost_alt_localpwr_cn")


### Change DV to expulsion only instead of all case advanced ####
df$expelled <- !is.na(df$expelled_date)
table(df$expelled)
check.status4p <- update(status4, expelled ~ .)
check.status4p.r <- coeftest(check.status4p, vcov=vcovHC, type="HC1")
check.status4p.r

df$arrested <- !is.na(df$sentenced_date)
table(df$arrested)
check.status4q <- update(status4, arrested ~ .)
check.status4q.r <- coeftest(check.status4q, vcov=vcovHC, type="HC1")
check.status4q.r

# Table A43
stargazer(status4,check.status4p,check.status4q,
          omit = c("year_f"),
          keep.stat = c("n","adj.rsq"),
          table.placement = "H",
          add.lines=list(c("Year FE",rep("Yes",3))),
          se=list(status4.r[,2],check.status4p.r[,2],check.status4q.r[,2]),
          order=c("^tie.incumbent"),
          dep.var.caption = c("Dependent variable: Probability of:"),
          dep.var.labels = c("Jail sentence or expulsion","Party expulsion only","Jail sentence only"),
          covariate.labels = c("Province's ties to incumbent GS",
                               "Tiger","Male","Age","Native official","Retired",
                               "Corruption experience",
                               "Revenue (logged)",
                               "Population (logged)",
                               "Transfer from center (share of revenue)"
          ),
          digits=3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title="Robust evidence of ex post protection in China across alternative specifications of outcome variable",
          out=c("../Tables/robust_expost_alt_dv_cn.txt",
                "../Tables/robust_expost_alt_dv_cn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="robust_expost_alt_dv_cn")

### Ties with incumbent Premier and CCDI chief ####
check.status4t <- update(status4, . ~ . +tie_incumbent_premier)
check.status4t.r <- coeftest(check.status4t, vcov=vcovHC, type="HC1")
check.status4t.r

check.status4u <- update(status4, . ~ . +tie_incumbent_ccdi)
check.status4u.r <- coeftest(check.status4u, vcov=vcovHC, type="HC1")
check.status4u.r

check.status4v <- update(status4, . ~ . +tie_previous)
check.status4v.r <- coeftest(check.status4v, vcov=vcovHC, type="HC1")
check.status4v.r

# Table A44
stargazer(status4,check.status4t,check.status4u,check.status4v,
          omit = c("year_f"),
          keep.stat = c("n","adj.rsq"),
          table.placement = "H",
          add.lines=list(c("Year FE",rep("Yes",4))),
          se=list(status4.r[,2],check.status4t.r[,2],check.status4u.r[,2],
                  check.status4v.r[,2]),
          order=c("^tie"),
          dep.var.labels = "Probability of jail sentence or expulsion",
          covariate.labels = c("Province's ties to incumbent GS",
                               "Province's ties to incumbent Premier",
                               "Province's ties to incumbent CCDI head",
                               "Province's ties to previous GS",
                               "Tiger","Male","Age","Native official","Retired",
                               "Corruption experience",
                               "Revenue (logged)",
                               "Population (logged)",
                               "Transfer from center (share of revenue)"
          ),
          digits=3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title="Robust evidence of ex post protection by regime leader in China when controlling for other central elites' influence",
          out=c("../Tables/robust_expost_alt_ties.txt",
                "../Tables/robust_expost_alt_ties_cn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="robust_expost_alt_ties_cn")

### Interact ACC with factional tie ####
df$acc <- df$investigated_year>2012
check.status4w <- update(status4, data = df %>% filter(acc==T))
check.status4w.r <- coeftest(check.status4w, vcov=vcovHC, type="HC1")
check.status4w.r
check.status4w2 <- update(status4, . ~ . -year_f+tie_incumbent:acc)
check.status4w2.r <- coeftest(check.status4w2, vcov=vcovHC, type="HC1")
check.status4w2.r

# Table A45
stargazer(status4,check.status4w,check.status4w2,
          omit = c("year_f"),
          keep.stat = c("n","adj.rsq"),
          table.placement = "H",
          add.lines=list(c("","Original model","Only post-ACC obs","ACC x Ties"),
                         c("Year FE",rep("Yes",2),"")),
          se=list(status4.r[,2],check.status4w.r[,2],check.status4w2.r[,2]),
          order=c("^tie."),
          dep.var.labels = "Probability of jail sentence or expulsion",
          covariate.labels = c("Province's ties to incumbent GS",
                               "Province's ties to incumbent GS X ACC",
                               "Tiger","Male","Age","Native official","Retired",
                               "Corruption experience",
                               "Revenue (logged)",
                               "Population (logged)",
                               "Transfer from center (share of revenue)"
          ),
          digits=3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title="Ex post protection when interacting ties with ACC",
          out=c("../Tables/robust_expost_interact_acc_cn.txt",
                "../Tables/robust_expost_interact_acc_cn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="robust_expost_interact_acc_cn")

## Survival ####
survival3 %>% summary()
### Proportional hazard assumption ####
hazard.test3 <- cox.zph(survival3,transform = "identity")
hazard.test3
hazard.test3.table <- hazard.test3$table %>%
  as.data.frame() %>%
  mutate(p = round(p, digits = 3) %>%
           as.character() %>%
           dplyr::recode("0" = "<0.001"))
rownames(hazard.test3.table) <- c("Province's ties to incumbent GS",
                                  "Investigation date",
                                  "Tiger","Male","Age","Native official","Retired",
                                  "Corruption experience",
                                  "Revenue (logged)",
                                  "Population (logged)",
                                  "Transfer from center (share of revenue)",
                                  "GLOBAL TEST")
# Table A46
stargazer(hazard.test3.table,
          table.placement = "H",
          summary = F,
          title = "Result of Cox proportional hazard ratio assumption test",
          font.size = "scriptsize",
          column.sep.width = "1pt",
          type = "text",
          out=c("../Tables/cox_hazard_test.txt",
                "../Tables/cox_hazard_test.tex"),
          label = "cox_hazard_test")

survival3_hzcorrecteda <- update(survival3, . ~ .
                                 + investigated_date*acc
                                 + status_type*acc
                                 + local*acc
                                 + retired*acc)
survival3_hzcorrectedb <- update(survival3, . ~ .
                                + investigated_date*as.numeric(year_f)
                                + status_type*as.numeric(year_f)
                                + local*as.numeric(year_f)
                                + retired*as.numeric(year_f))

# Table A47
stargazer(survival1,survival3_hzcorrecteda,survival3_hzcorrectedb,
          keep= c("tie_incumbent","investigated_date$"),
          keep.stat = c("n","rsq"),
          table.placement = "H",
          order=c("^tie_incumbent$"),
          dep.var.labels = "Expulsion/Sentence log hazard ratio",
          covariate.labels = c("Province's ties to incumbent GS",
                               "Time trend (day)"),
          digits=3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title="Evidence of ex post protection on investigation delays remains robust after adjusting for violation of proportional hazard ratio assumption",
          notes = c("Model 1 is the uncorrected model. Model 2 includes interactions",
                    "of investigated date, tiger status, native official status, and",
                    "retirement status with anti-corruption campaign dummy.",
                    "Model 3 interacts the same variables with year."),
          out=c("../Tables/robust_expost_cox_zph_cn.txt",
                "../Tables/robust_expost_cox_zph_cn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="regress_expost_cox_zph_cn")

# Most influential observations/outliers
ggcoxdiagnostics(survival3,type = "deviance", ggtheme=theme_bw())

### Replace logged revenue with alternative measures ####
check.survival3a <- update(survival3, . ~. - revenue_logged +expenditure_logged)
check.survival3a

check.survival3b <- update(survival3, . ~. - revenue_logged +log(revenue_l1y))
check.survival3b

check.survival3c <- update(survival3, . ~. - revenue_logged +log(expenditure_l1y))
check.survival3c

# Table A48
stargazer(survival3,check.survival3a,check.survival3b,check.survival3c,
          keep.stat = c("n","rsq"),
          table.placement = "H",
          order=c("^tie_incumbent$"),
          dep.var.labels = "Expulsion/Sentence log hazard ratio",
          covariate.labels = c("Province's ties to incumbent GS",
                               "Time trend (day)",
                               "Tiger","Male","Age","Native official","Retired",
                               "Corruption experience",
                               "Revenue (logged)",
                               "Population (logged)",
                               "Transfer from center (share of revenue)",
                               "Revenue (logged, lagged)",
                               "Expenditure (logged)",
                               "Expenditure (logged, lagged)"),
          digits=3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title="Evidence of ex post protection on investigation delays remains robust after adjusting for violation of proportional hazard ratio assumption",
          out=c("../Tables/robust_expost_cox_alt_econ_cn.txt",
                "../Tables/robust_expost_cox_alt_econ_cn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="regress_expost_alt_econ_cn")

### Replace corrupexp with alternative measures ####
check.survival3j <- update(survival3, . ~. - corrupexp + corruppercep)
check.survival3j

check.survival3k <- update(survival3, . ~. - corrupexp + corruption2)
check.survival3k

check.survival3l <- update(survival3, . ~. - corrupexp + corruption3)
check.survival3l

# Table A49
stargazer(survival3,check.survival3j,check.survival3k,check.survival3l,
          keep.stat = c("n","rsq"),
          table.placement = "H",
          order=c("^tie_incumbent$"),
          dep.var.labels = "Expulsion/Sentence log hazard ratio",
          covariate.labels = c("Province's ties to incumbent GS",
                               "Investigation date",
                               "Tiger","Male","Age","Native official","Retired",
                               "Corruption experience",
                               "Revenue (logged)",
                               "Population (logged)",
                               "Transfer from center (share of revenue)",
                               "Perceived corruption",
                               "Recovered corrupt funds per Capita (log)",
                               "Senior cadres disciplined per 10,000 public employees (log)"
          ),
          digits=3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title="Robust evidence of ex post protection in China across all specifications of corruption levels. All corruption measures are taken from Zhu (2017)",
          out=c("../Tables/robust_expost_cox_alt_corr_cn.txt",
                "../Tables/robust_expost_cox_alt_corr_cn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="robust_expost_cox_alt_corr_cn")

### Replace transfer_share with alternative measures ####
check.survival3m <- update(survival3, . ~. - transfer_share + BureauIntegr)
check.survival3m

### Dummy for Beijing and Shanghai ####
dfcox$BJ_SH <- dfcox$corruption_location %in% c("Beijing","Shanghai")
table(dfcox$BJ_SH)
check.survival3n <- update(survival3, . ~ . + BJ_SH)
check.survival3n

### Dummy for whether Party Secretary is also in Politburo ####
dfcox$PS_Politburo <- (dfcox$investigated_year %in% 2007:2018 & dfcox$corruption_location %in% c("Guangdong","Shanghai","Tianjin","Xinjiang","Chongqing")) |
  (dfcox$investigated_year %in% 2002:2018 & dfcox$corruption_location %in% c("Guangdong","Shanghai","Tianjin","Xinjiang","Hubei")) |
  (dfcox$investigated_year %in% 2002:2018 & dfcox$corruption_location %in% c("Guangdong","Shandong","Beijing","Shanghai"))
table(dfcox$PS_Politburo)
check.survival3o <- update(survival3, . ~ . + PS_Politburo)
check.survival3o

### Change DV to expulsion only instead of all case advanced ####
dfcox$expelled <- !is.na(dfcox$expelled_date)
table(dfcox$expelled)
check.survival3p <- update(survival3, Surv(date, expelled)  ~ .)
check.survival3p

dfcox$arrested <- !is.na(dfcox$sentenced_date)
table(dfcox$arrested)
check.survival3q <- update(survival3, Surv(date, arrested) ~ .)
check.survival3q

### Interact date and status with ACC and Party Congresses ####
check.survival3r <- update(survival3, .  ~ . + investigated_date*acc+status_type*acc)
check.survival3r

check.survival3s <- update(survival3, .  ~ . + investigated_date*congress+status_type*congress)
check.survival3s

### Ties with incumbent Premier and CCDI chief ####
check.survival3t <- update(survival3, . ~ . -tie_incumbent+tie_incumbent_premier)
check.survival3t

check.survival3u <- update(survival3, . ~ . -tie_incumbent+tie_incumbent_ccdi)
check.survival3u

check.survival3v <- update(survival3, . ~ . -tie_incumbent+tie_previous)
check.survival3v

### Interact ACC with factional tie ####
check.survival3w <- update(survival3, data = dfcox %>% filter(acc==T))
check.survival3w
check.survival3w2 <- update(survival3, . ~ . +tie_incumbent:acc)
check.survival3w2

# Table A50
stargazer(survival3,check.survival3w,check.survival3w2,
          omit="native.*|corruption.*",
          keep.stat = c("n","rsq"),
          table.placement = "H",
          order=c("^tie_incumbent"),
          dep.var.labels = "Expulsion/Sentence log hazard ratio",
          covariate.labels = c("Province's ties to incumbent GS",
                               "Province's ties to incumbent GS x ACC",
                               "Time trend (day)",
                               "Tiger","Male","Age","Native official","Retired",
                               "Corruption experience",
                               "Revenue (logged)",
                               "Population (logged)",
                               "Transfer from center (share of revenue)"),
          digits=3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title="Robustness to subsetting post-ACC period and interacting ACC with ties",
          out=c("../Tables/robust_expost_cox_interact_acc_cn.txt",
                "../Tables/robust_expost_cox_interact_acc_cn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="robust_expost_cox_interact_acc_cn_surv")


# SENSITIVITY ANALYSIS #####
############################

# Ex ante protection ####

## Remove provinces one by one - case ####
count.missing.province <- data.frame("province"=unique(df.count$corruption_location),
                               "coef"=NA, "se"=NA, "p"=NA)
for (i in 1:nrow(count.missing.province)){
  tryCatch({
    sensitive.count3 <- update(count3, data=subset(df.count, corruption_location!=count.missing.province$province[i]))
    sensitive.count3.r <- coeftest(sensitive.count3, vcov=vcovHC, type="HC1")
    count.missing.province$coef[i] <- sensitive.count3.r["tie_incumbent",1]
    count.missing.province$se[i] <- sensitive.count3.r["tie_incumbent",2]
    count.missing.province$p[i] <- sensitive.count3.r["tie_incumbent",4] < 0.05
  }, error=function(e){})}

# Figure A8 - top left
count.missing.province %>%
  ggplot(aes(x=province, y=coef, color=p)) +
  geom_point(size=4) +
  geom_errorbar(aes(ymin=coef-se*1.96, ymax=coef+se*1.96),
                width=0, size=2,
                position=position_dodge(width=0.5)) +
  geom_hline(yintercept = 0, colour ="black", lty = 2, lwd=1) +
  theme_classic(base_size=32) +
  scale_color_manual(name="Significant at alpha=.05",
                     values=c("TRUE"="#377eb8","FALSE"="#e41a1c"), drop=FALSE) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title="Effect of factional ties to CCP GS on investigation counts",
       subtitle = "Excluding one province at a time",
       x="Province excluded",y="Effect size")
ggsave("../Graphs/coef_exante_exclude_province_cn.png", width=20,height=10,units="in")


## Remove year one by one - case count ####
count.missing.year <- data.frame("year"=unique(df.count$investigated_year),
                                     "coef"=NA, "se"=NA, "p"=NA)
for (i in 1:nrow(count.missing.year)){
  tryCatch({
    sensitive.count3 <- update(count3, data=subset(df.count, investigated_year!=count.missing.year$year[i]))
    sensitive.count3.r <- coeftest(sensitive.count3, vcov=vcovHC, type="HC1")
    count.missing.year$coef[i] <- sensitive.count3.r["tie_incumbent",1]
    count.missing.year$se[i] <- sensitive.count3.r["tie_incumbent",2]
    count.missing.year$p[i] <- sensitive.count3.r["tie_incumbent",4] < 0.05
  }, error=function(e){})}

# Figure A8 - top right
count.missing.year %>%
  ggplot(aes(x=year, y=coef, color=p)) +
  geom_point(size=4) +
  geom_errorbar(aes(ymin=coef-se*1.96, ymax=coef+se*1.96),
                width=0, size=2,
                position=position_dodge(width=0.5)) +
  geom_hline(yintercept = 0, colour ="black", lty = 2, lwd=1) +
  theme_classic(base_size=32) +
  scale_color_manual(name="Significant at alpha=.05",
                     values=c("TRUE"="#377eb8","FALSE"="#e41a1c"), drop=FALSE) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title="Effect of factional ties to CCP GS on investigation counts",
       subtitle = "Excluding one year at a time",
       x="Year excluded",y="Effect size")
ggsave("../Graphs/coef_exante_exclude_year_cn.png", width=20,height=10,units="in")

## Moving 2-year bands - case count ####
count.2year.band <- data.frame("year"=2012:2016,
                                 "coef"=NA, "se"=NA, "p"=NA) %>%
  mutate(year_end = year + 2,
         year_range = str_c(year, year_end, sep = "-"))
for (i in 1:nrow(count.2year.band)){
  tryCatch({
    sensitive.count3 <- update(count3, data=subset(df.count, investigated_year %in% count.2year.band$year[i]:count.2year.band$year_end[i]))
    sensitive.count3.r <- coeftest(sensitive.count3, vcov=vcovHC, type="HC1")
    count.2year.band$coef[i] <- sensitive.count3.r["tie_incumbent",1]
    count.2year.band$se[i] <- sensitive.count3.r["tie_incumbent",2]
    count.2year.band$p[i] <- sensitive.count3.r["tie_incumbent",4] < 0.05
  }, error=function(e){})}

# Figure A9 - top
count.2year.band %>%
  ggplot(aes(x=year_range, y=coef, color=p)) +
  geom_point(size=4) +
  geom_errorbar(aes(ymin=coef-se*1.96, ymax=coef+se*1.96),
                width=0, size=2,
                position=position_dodge(width=0.5)) +
  geom_hline(yintercept = 0, colour ="black", lty = 2, lwd=1) +
  theme_classic(base_size=32) +
  scale_color_manual(name="Significant at alpha=.05",
                     values=c("TRUE"="#377eb8","FALSE"="#e41a1c"), drop=FALSE) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title="Effect of factional ties to CCP GS on investigation counts",
       subtitle = "Limiting data to 2-year bands",
       x="Year band",y="Effect size")
ggsave("../Graphs/coef_exante_2year_band_cn.png", width=20,height=10,units="in")

# Ex post protection #####

## Remove provinces one by one - status ####
status.missing.province <- data.frame("province"=unique(df$corruption_location),
                                      "coef"=NA, "se"=NA, "p"=NA)
for (i in 1:nrow(status.missing.province)){
  tryCatch({
    sensitive.status4 <- update(status4, data=subset(df, corruption_location!=status.missing.province$province[i]))
    sensitive.status4.r <- coeftest(sensitive.status4, vcov=vcovHC, type="HC1")
    status.missing.province$coef[i] <- sensitive.status4.r["tie_incumbent",1]
    status.missing.province$se[i] <- sensitive.status4.r["tie_incumbent",2]
    status.missing.province$p[i] <- sensitive.status4.r["tie_incumbent",4] < 0.05
  }, error=function(e){})}

# Figure A8 - middle left
status.missing.province %>%
  ggplot(aes(x=province, y=coef, color=p)) +
  geom_point(size=4) +
  geom_errorbar(aes(ymin=coef-se*1.96, ymax=coef+se*1.96),
                width=0, size=2,
                position=position_dodge(width=0.5)) +
  geom_hline(yintercept = 0, colour ="black", lty = 2, lwd=1) +
  theme_classic(base_size=32) +
  scale_color_manual(name="Significant at alpha=.05",
                     values=c("TRUE"="#377eb8","FALSE"="#e41a1c"), drop=FALSE) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title="Effect of factional ties to CCP GS on jail sentence/Party expulsion probability",
       subtitle = "Excluding one province at a time",
       x="Province excluded",y="Effect size")
ggsave("../Graphs/coef_expost_status_exclude_province_cn.png", width=20,height=10,units="in")

## Remove year one by one - status ####
status.missing.year <- data.frame("year"=unique(df$investigated_year),
                                  "coef"=NA, "se"=NA, "p"=NA)
for (i in 1:nrow(status.missing.year)){
  tryCatch({
    sensitive.status4 <- update(status4, data=subset(df, investigated_year!=status.missing.year$year[i]))
    sensitive.status4.r <- coeftest(sensitive.status4, vcov=vcovHC, type="HC1")
    status.missing.year$coef[i] <- sensitive.status4.r["tie_incumbent",1]
    status.missing.year$se[i] <- sensitive.status4.r["tie_incumbent",2]
    status.missing.year$p[i] <- sensitive.status4.r["tie_incumbent",4] < 0.05
  }, error=function(e){})}

# Figure A8 - middle right
status.missing.year %>%
  ggplot(aes(x=year, y=coef, color=p)) +
  geom_point(size=4) +
  geom_errorbar(aes(ymin=coef-se*1.96, ymax=coef+se*1.96),
                width=0, size=2,
                position=position_dodge(width=0.5)) +
  geom_hline(yintercept = 0, colour ="black", lty = 2, lwd=1) +
  theme_classic(base_size=32) +
  scale_color_manual(name="Significant at alpha=.05",
                     values=c("TRUE"="#377eb8","FALSE"="#e41a1c"), drop=FALSE) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title="Effect of factional ties to CCP GS on jail sentence/Party expulsion probability",
       subtitle = "Excluding one year at a time",
       x="Year excluded",y="Effect size")
ggsave("../Graphs/coef_expost_status_exclude_year_cn.png", width=20,height=10,units="in")

## Moving 2-year bands - status ####
status.2year.band <- data.frame("year"=2012:2016,
                               "coef"=NA, "se"=NA, "p"=NA) %>%
  mutate(year_end = year + 2,
         year_range = str_c(year, year_end, sep = "-"))
for (i in 1:nrow(status.2year.band)){ # Not enough data for first bands; start with i=12 instead
  tryCatch({
    sensitive.status4 <- update(status4, data=subset(df, investigated_year %in% status.2year.band$year[i]:status.2year.band$year_end[i]))
    sensitive.status4.r <- coeftest(sensitive.status4)
    status.2year.band$coef[i] <- sensitive.status4.r["tie_incumbent",1]
    status.2year.band$se[i] <- sensitive.status4.r["tie_incumbent",2]
    status.2year.band$p[i] <- sensitive.status4.r["tie_incumbent",4] < 0.05
  }, error=function(e){})}

# Figure A9 - middle
status.2year.band %>%
  ggplot(aes(x=year_range, y=coef, color=p)) +
  geom_point(size=4) +
  geom_errorbar(aes(ymin=coef-se*1.96, ymax=coef+se*1.96),
                width=0, size=2,
                position=position_dodge(width=0.5)) +
  geom_hline(yintercept = 0, colour ="black", lty = 2, lwd=1) +
  theme_classic(base_size=32) +
  scale_color_manual(name="Significant at alpha=.05",
                     values=c("TRUE"="#377eb8","FALSE"="#e41a1c"), drop=FALSE) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title="Effect of factional ties to CCP GS on jail sentence/Party expulsion probability",
       subtitle = "Limiting data to 2-year bands",
       x="Year band",y="Effect size")
ggsave("../Graphs/coef_expost_status_2year_band_cn.png", width=20,height=10,units="in")

## Remove provinces one by one - survival ####
survival.missing.province <- data.frame("province"=unique(dfcox$corruption_location),
                                        "coef"=NA, "se"=NA, "p"=NA)
for (i in 1:nrow(survival.missing.province)){
  tryCatch({
    sensitive.survival3 <- update(survival3, data=subset(dfcox, corruption_location!=survival.missing.province$province[i]))
    survival.missing.province$coef[i] <- summary(sensitive.survival3)$coefficients["tie_incumbent",1]
    survival.missing.province$se[i] <- summary(sensitive.survival3)$coefficients["tie_incumbent",3]
    survival.missing.province$p[i] <- summary(sensitive.survival3)$coefficients["tie_incumbent",5] < 0.05
  }, error=function(e){})}

# Figure A8 - bottom left
survival.missing.province %>%
  ggplot(aes(x=province, y=coef, color=p)) +
  geom_point(size=4) +
  geom_errorbar(aes(ymin=coef-se*1.96, ymax=coef+se*1.96),
                width=0, size=2,
                position=position_dodge(width=0.5)) +
  geom_hline(yintercept = 0, colour ="black", lty = 2, lwd=1) +
  theme_classic(base_size=32) +
  scale_color_manual(name="Significant at alpha=.05",
                     values=c("TRUE"="#377eb8","FALSE"="#e41a1c"), drop=FALSE) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title="Effect of factional ties to CCP GS on jail/expulsion hazard",
       subtitle = "Excluding one province at a time",
       x="Province excluded",y="Effect size")
ggsave("../Graphs/coef_expost_survival_exclude_province_cn.png", width=20,height=10,units="in")

## Remove year one by one - survival ####
survival.missing.year <- data.frame("year"=unique(dfcox$investigated_year),
                                    "coef"=NA, "se"=NA, "p"=NA)
for (i in 1:nrow(survival.missing.year)){
  tryCatch({
    sensitive.survival3 <- update(survival3, data=subset(dfcox, investigated_year!=survival.missing.year$year[i]))
    survival.missing.year$coef[i] <- summary(sensitive.survival3)$coefficients["tie_incumbent",1]
    survival.missing.year$se[i] <- summary(sensitive.survival3)$coefficients["tie_incumbent",3]
    survival.missing.year$p[i] <- summary(sensitive.survival3)$coefficients["tie_incumbent",5] < 0.05
  }, error=function(e){})}

# Figure A8 - bottom right
survival.missing.year %>%
  ggplot(aes(x=year, y=coef, color=p)) +
  geom_point(size=4) +
  geom_errorbar(aes(ymin=coef-se*1.96, ymax=coef+se*1.96),
                width=0, size=2,
                position=position_dodge(width=0.5)) +
  geom_hline(yintercept = 0, colour ="black", lty = 2, lwd=1) +
  theme_classic(base_size=32) +
  scale_color_manual(name="Significant at alpha=.05",
                     values=c("TRUE"="#377eb8","FALSE"="#e41a1c"), drop=FALSE) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title="Effect of factional ties to CCP GS on jail/expulsion hazard",
       subtitle = "Excluding one year at a time",
       x="Year excluded",y="Effect size")
ggsave("../Graphs/coef_expost_survival_exclude_year_cn.png", width=20,height=10,units="in")

## Moving 2-year bands - survival ####
survival.2year.band <- data.frame("year"=2012:2016,
                                "coef"=NA, "se"=NA, "p"=NA) %>%
  mutate(year_end = year + 2,
         year_range = str_c(year, year_end, sep = "-"))
for (i in 1:nrow(survival.2year.band)){ # Not enough data for first bands; start with i=12 instead
  tryCatch({
    sensitive.survival3 <- update(survival3, data=subset(dfcox, investigated_year %in% survival.2year.band$year[i]:survival.2year.band$year_end[i]))
    sensitive.survival3.r <- coeftest(sensitive.survival3)
    survival.2year.band$coef[i] <- sensitive.survival3.r["tie_incumbent",1]
    survival.2year.band$se[i] <- sensitive.survival3.r["tie_incumbent",2]
    survival.2year.band$p[i] <- sensitive.survival3.r["tie_incumbent",4] < 0.05
  }, error=function(e){})}

# Figure A9 - bottom
survival.2year.band %>%
  ggplot(aes(x=year_range, y=coef, color=p)) +
  geom_point(size=4) +
  geom_errorbar(aes(ymin=coef-se*1.96, ymax=coef+se*1.96),
                width=0, size=2,
                position=position_dodge(width=0.5)) +
  geom_hline(yintercept = 0, colour ="black", lty = 2, lwd=1) +
  theme_classic(base_size=32) +
  scale_color_manual(name="Significant at alpha=.05",
                     values=c("TRUE"="#377eb8","FALSE"="#e41a1c"), drop=FALSE) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title="Effect of factional ties to CCP GS on jail sentence/Party expulsion hazard",
       subtitle = "Limiting data to 2-year bands",
       x="Year band",y="Effect size")
ggsave("../Graphs/coef_expost_survival_2year_band_cn.png", width=20,height=10,units="in")


## Remove longest-surviving observations one by one - survival ####
colnames(dfcox)
dfcox$end_date <- dfcox$sanction_date
dfcox$end_date[is.na(dfcox$end_date)] <- as.Date("2018-07-31")
dfcox$survival_time <- dfcox$end_date-dfcox$investigated_date
dfcox.sensitive <-  dfcox[order(-dfcox$survival_time),]

sensitive.missing.longest.survival <- data.frame("Missing"=1:nrow(dfcox),
                                                 coef=NA, se=NA,p=NA)
for (i in 1:nrow(sensitive.missing.longest.survival)){
  tryCatch({
    sensitive.survival3 <- update(survival3, data=dfcox.sensitive %>% tail(-i))
    sensitive.missing.longest.survival$coef[i] <- summary(sensitive.survival3)$coefficients["tie_incumbent",1]
    sensitive.missing.longest.survival$se[i] <- summary(sensitive.survival3)$coefficients["tie_incumbent",3]
    sensitive.missing.longest.survival$p[i] <- summary(sensitive.survival3)$coefficients["tie_incumbent",5] < 0.05
  }, error=function(e){})}

findgroup <- function(x){
  group <- vector(length=length(x))
  for (i in 2:length(x)) {
    if (x[i]!=x[i-1]) {group[i] <- group[i-1] + 1}
    else group[i] <- group[i-1]
  }
  return(factor(group))
}

# Figure A10
sensitive.missing.longest.survival %>%
  slice(1:(n()-100)) %>%
  mutate(period = findgroup(p)) %>%
  ggplot( aes(x=Missing, y = coef, group=period)) +
  geom_point(size=1) +
  #geom_path(size=1) +
  geom_ribbon(aes(ymin = coef-1.96*se, ymax = coef+1.96*se, fill=p),
              alpha = 0.3) +
  geom_hline(yintercept = 0, linetype=2, size = 1) +
  theme_classic(base_size=24) +
  theme(legend.position="bottom") +
  scale_fill_brewer(palette="Set1",name="p<0.05", labels=c("Yes","No")) +
  labs(title="Effect size of factional ties consistently indistinguishable\nfrom zero only after >500 longest-surviving observations are deleted",
       x="Number of observations excluded",y="Coefficient estimates for jail/expulsion hazard")
ggsave("../Graphs/robust_expost_cox_longest_survival_cn.png", width=20,height=10,units="in")

# SAVE WORKSPACE #####
######################

save.image(file = "workspace_china.RData")

