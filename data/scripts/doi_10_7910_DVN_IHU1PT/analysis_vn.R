###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##    Selective Protection in Vietnam
##    Duy Trinh
##    Created date: 05/02/2020
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# LOAD DATA ####
###~~~~~~~~~~~~~

load("../Datasets/Ties_ProvincePS1520_with_PB.RData")
load("../Datasets/Trinh_VN_ex_ante_data.RData")
load("../Datasets/Trinh_VN_ex_post_data.RData")

# ANALYZE EX-ANTE PROTECTION ####
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Desc plots and stats ####
### Provinces with tie to Trong over time ####
# Figure A5
panelview(year~tie_a_pb_NguyenPhuTrong,
          data=ProvinceData,
          index=c("province_eng","year"),
          xlab="Year",ylab="Province name",
          main="",
          background="white",
          color = RColorBrewer::brewer.pal(3,"Blues"),
          legend.labs=c("No Ties","Ties (part of year)", "Ties"),
          cex.main=28, cex.main.sub = 24, cex.axis=20, cex.lab=24, cex.legend=24)
ggsave("../Graphs/Ties_Trong_province_year_vn.png", width=15,height=20,units="in", bg="transparent")

### Descriptive stats for provincial IVs and DVs ####
# Table A3
stargazer(subset(ProvinceData, select=c(mentions.sanction, mentions, tie_a_pb_NguyenPhuTrong,
                                        population_2011,GDP_l1y, target.trans_share_l1y, pci,unweighted_papi)),
          summary.stat = c("n","mean","sd","min","max"), digits=2,
          covariate.labels = c("Count of CCDI investigations in province",
                               "Count of all CCDI activities in province",
                               "Province's ties to incumbent GS",
                               "2011 population (thousands)",
                               "Lagged GDP (billion VND)",
                               "Lagged targeted transfer (share of total revenue)",
                               "PCI score",
                               "Unweighted PAPI score"),
          type = "text",
          title = "Descriptive statistics for Vietnam's provincial covariates",
          out = c("../Tables/desc_stat_vn.txt",
                "../Tables/desc_stat_vn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label = "desc_stat_vn")

## Regressions ####
### Mentions in sanction announcements as DV - panel ####
# Main results
mentions.sanction.a <- lm(mentions.sanction~tie_a_pb_NguyenPhuTrong+mentions, data=ProvinceData)
mentions.sanction.a.r <- coeftest(mentions.sanction.a, vcov=vcovHC, type="HC1")
mentions.sanction.a.r
mentions.sanction.b <- lm(mentions.sanction~tie_a_pb_NguyenPhuTrong+mentions+log(GDP_l1y)+log(population_2011), data=ProvinceData)
mentions.sanction.b.r <- coeftest(mentions.sanction.b, vcov=vcovHC, type="HC1")
mentions.sanction.b.r
mentions.sanction.c <- lm(mentions.sanction~tie_a_pb_NguyenPhuTrong+mentions+log(GDP_l1y)+log(population_2011)+target.trans_share_l1y, data=ProvinceData)
mentions.sanction.c.r <- coeftest(mentions.sanction.c, vcov=vcovHC, type="HC1")
mentions.sanction.c.r
mentions.sanction.d <- lm(mentions.sanction~tie_a_pb_NguyenPhuTrong+mentions+log(GDP_l1y)+log(population_2011)+target.trans_share_l1y+pci, data=ProvinceData)
mentions.sanction.d.r <- coeftest(mentions.sanction.d, vcov=vcovHC, type="HC1")
mentions.sanction.d.r
mentions.sanction.e <- lm(mentions.sanction~tie_a_pb_NguyenPhuTrong+mentions+log(GDP_l1y)+log(population_2011)+target.trans_share_l1y+pci+factor(year), data=ProvinceData)
mentions.sanction.e.r <- coeftest(mentions.sanction.e, vcov=vcovHC, type="HC1")
mentions.sanction.e.r

# Table 1
stargazer(mentions.sanction.a,mentions.sanction.b,mentions.sanction.c,mentions.sanction.d,mentions.sanction.e,
          keep.stat = c("n", "adj.rsq"),
          omit="factor",
          add.lines=list(c("Year FE",rep("",4),"Yes")
                         ),
          se=list(mentions.sanction.a.r[,2],mentions.sanction.b.r[,2],mentions.sanction.c.r[,2],mentions.sanction.d.r[,2],mentions.sanction.e.r[,2]),
          dep.var.labels = "Count of CCDI investigations in province",
          covariate.labels = c("Province's ties to incumbent GS",
                               "Count of all CCDI activities",
                               "Lagged GDP (logged)",
                               "2011 population (logged)",
                               "Lagged targeted transfer",
                               "PCI score"),
          digits=2,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type="text",
          title="Factional ties to VCP GS associated with fewer investigations",
          out=c("../Tables/regress_exante_main_vn.txt",
                "../Tables/regress_exante_main_vn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="regress_exante_main_vn")

# Create coef plot
coef.mentions.sanction <- data.frame(model=c(rep("1.Base model",length(mentions.sanction.a$coef)),
                                             rep("2.+ Socioeconomic controls",length(mentions.sanction.b$coef)),
                                             rep("3.+ Local autonomy",length(mentions.sanction.c$coef)),
                                             rep("4.+ Governance",length(mentions.sanction.d$coef)),
                                             rep("5.+ Year FE",length(mentions.sanction.e$coef))),
                                     variable = c(names(mentions.sanction.a$coef),names(mentions.sanction.b$coef),
                                                  names(mentions.sanction.c$coef),names(mentions.sanction.d$coef),
                                                  names(mentions.sanction.e$coef)),
                                     coef=c(mentions.sanction.a$coef,mentions.sanction.b$coef,mentions.sanction.c$coef, mentions.sanction.d$coef,
                                            mentions.sanction.e$coef),
                                     se=c(mentions.sanction.a.r[,2],mentions.sanction.b.r[,2],mentions.sanction.c.r[,2],mentions.sanction.d.r[,2],
                                          mentions.sanction.e.r[,2])) %>%
  subset(!grepl("factor|Intercept|revenue",variable))
coef.mentions.sanction$variable <- factor(coef.mentions.sanction$variable,
                                          levels=rev(c("tie_a_pb_NguyenPhuTrong","mentions","log(GDP_l1y)","log(population_2011)",
                                                       "target.trans_share_l1y","pci")),
                                          labels=rev(c("Province's ties to incumbent GS",
                                                       "Count of all CCDI activities",
                                                       "GDP (lagged log)","Population (2011, log)",
                                                       "Targeted transfer share","PCI Score")))
# Figure 2
coef.mentions.sanction %>%
  ggplot(aes(x=variable, y=coef, color=model)) +
  geom_point(size=4, position=position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin=coef-se*1.96, ymax=coef+se*1.96),
                width=0, size=2,
                position=position_dodge(width=0.5)) +
  geom_hline(yintercept = 0, lty = 2, lwd=1) +
  theme_classic(base_size=28) +
  theme(legend.title=element_blank()) +
  scale_color_brewer(palette="Set1") +
  labs(title="",
       x="",y="Count of formal investigations per year") +
  coord_flip()
ggsave("../Graphs/coef_mentions_sanction_vn.png", width=16,height=10,units="in")

# ANALYZE EX POST PROTECTION ####
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Desc. plots and stats ####
### Descriptive stats for individual IVs and DVs ####
# Table A4
IncidentList_all$serious <- IncidentList_all$seriousness != "No mention"
IncidentList_all %>%
  mutate(punishment2 = as.numeric(punishment2),
         office_level_ward = as.numeric(office_level=="ward"),
         office_level_district = as.numeric(office_level=="district"),
         office_level_province = as.numeric(office_level=="province"),
         office_level_national = as.numeric(office_level=="national"),
         past_offence = as.numeric(past_offence=="no"),
         ubkt_level_ward = as.numeric(ubkt_level=="ward"),
         ubkt_level_district = as.numeric(ubkt_level=="district"),
         ubkt_level_province = as.numeric(ubkt_level=="province"),
         ubkt_level_national = as.numeric(ubkt_level=="national"),
         GDP_l1y = log(GDP_l1y),
         population_2011 = log(population_2011),
         serious = as.numeric(serious)) %>%
  select(punishment2,contains("office_level_"),contains("ubkt_level_"),
         past_offence, serious) %>%
  as.data.frame() %>%
  stargazer(
          summary.stat=c("n","mean","sd","min","max"), digits=2,
          covariate.labels=c("Investigation resulted in major sanction",
                             "Investigated office level: Ward",
                             "Investigated office level: District",
                             "Investigated office level: Province",
                             "Investigated office level: National",
                             "IC level: Ward",
                             "IC level: District",
                             "IC level: Province",
                             "IC level: National",
                             "Official left investigated position",
                             "Serious violation designation"),
          type = "text",
          title = "Descriptive statistics for Vietnam's individual covariates",
          out = c("../Tables/desc_stat_vn_ind.txt",
                "../Tables/desc_stat_vn_ind.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label = "desc_stat_vn_ind")

### Count of cases over time ####
desc_vn_invest <- IncidentList_all %>% expand(year, tie_a_pb_NguyenPhuTrong)
# Table A5
IncidentList_all %>%
  group_by(year,tie_a_pb_NguyenPhuTrong) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  right_join( desc_vn_invest ) %>%
  mutate(count = replace_na(count,0)) %>%
  pivot_wider(names_from = "tie_a_pb_NguyenPhuTrong",
              values_from = "count") %>%
  stargazer(summary = F,
            rownames = F,
            covariate.labels = c("Year",
                                 "No tie",
                                 "Tie (part of year)",
                                 "Tie"),
            type = "text",
            title="Number of investigated officials in Vietnam by year and factional tie to incumbent GS",
            out=c("../Tables/desc_vn_invest_ind_over_time.txt",
                  "../Tables/desc_vn_invest_ind_over_time.tex"),
            font.size = "scriptsize",
            column.sep.width = "1pt",
            label="desc_vn_invest_ind_over_time")

## Regressions ####

### Ordered logistics model ####
punish1a.polr <- polr(punishment~tie_a_pb_NguyenPhuTrong, Hess=T, data=subset(IncidentList_all, !is.na(punishment) & punishment!="Sanction pending"))
punish1a.polr %>% summary()
punish1b.polr <- polr(punishment~tie_a_pb_NguyenPhuTrong+office_level+past_offence, Hess=T, data=subset(IncidentList_all, !is.na(punishment) & punishment!="Sanction pending"))
punish1b.polr %>% summary()
punish1c.polr <- polr(punishment~tie_a_pb_NguyenPhuTrong+office_level+past_offence+ubkt_level+serious, Hess=T, data=subset(IncidentList_all, !is.na(punishment) & punishment!="Sanction pending"))
punish1c.polr %>% summary()
punish1d.polr <- polr(punishment~tie_a_pb_NguyenPhuTrong+office_level+past_offence+ubkt_level+serious+log(GDP_l1y)+log(population_2011)+target.trans_share_l1y+pci, Hess=T, data=subset(IncidentList_all, !is.na(punishment) & punishment!="Sanction pending"))
punish1d.polr %>% summary()
punish1e.polr <- polr(punishment~tie_a_pb_NguyenPhuTrong+factor(year)+office_level+past_offence+ubkt_level+serious+log(GDP_l1y)+log(population_2011)+target.trans_share_l1y+pci, Hess=T, data=subset(IncidentList_all, !is.na(punishment) & punishment!="Sanction pending"))
punish1e.polr %>% summary()

# Table 2
stargazer(punish1a.polr,punish1b.polr,punish1c.polr,punish1d.polr,punish1e.polr,
          omit=c("factor*","Constant"),
          p.auto=FALSE,
          keep.stat = c("n","adj.rsq"),
          add.lines=list(c("Year FE",rep("",4),"Yes")
                         ),
          dep.var.labels = c("Higher sanction log odds"),
          covariate.labels = c("Province's ties to incumbent GS",
                               "Investigated office level (district)","Investigated office level (province)","Investigated office level (center)",
                               "Official left office",
                               "IC level (district)","IC level (province)","IC level (center)",
                               "Serious violation",
                               "Lagged GDP (logged)","2011 population (logged)","Lagged targeted transfer","PCI score"),
          digits=3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title="Factional ties to incumbent VCP GS uncorrelated with severity of sanction",
          out=c("../Tables/regress_expost_main_vn.txt",
                "../Tables/regress_expost_main_vn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="regress_expost_main_vn")


# ROBUSTNEST CHECKS #####
###~~~~~~~~~~~~~~~~~~~~~~

## Ex Ante Protection ####

### Provincial variations in sanction counts ####
# Mentions.inspect instead of all mentions
check.mentions.a1 <- update(mentions.sanction.d, . ~. - mentions + mentions.inspect)
check.mentions.a1.r <- coeftest(check.mentions.a1, vcov=vcovHC, type="HC1")
check.mentions.a1.r
check.mentions.a2 <- update(mentions.sanction.e, . ~. - mentions + mentions.inspect)
check.mentions.a2.r <- coeftest(check.mentions.a2, vcov=vcovHC, type="HC1")
check.mentions.a2.r
# No mentions at all
check.mentions.a3 <- update(mentions.sanction.d, . ~. - mentions)
check.mentions.a3.r <- coeftest(check.mentions.a3, vcov=vcovHC, type="HC1")
check.mentions.a3.r
check.mentions.a4 <- update(mentions.sanction.e, . ~. - mentions)
check.mentions.a4.r <- coeftest(check.mentions.a4, vcov=vcovHC, type="HC1")
check.mentions.a4.r

# Table A10
stargazer(check.mentions.a1,check.mentions.a2,
          check.mentions.a3,check.mentions.a4,
          keep.stat = c("n", "adj.rsq"), omit="factor",
          table.placement = "H",
          add.lines=list(c("Year FE","","Yes","","Yes")
                         ),
          se=list(check.mentions.a1.r[,2],check.mentions.a2.r[,2],
                  check.mentions.a3.r[,2],check.mentions.a4.r[,2]),
          dep.var.labels = "Count of CCDI investigations in province",
          covariate.labels = c("Province's ties to incumbent GS",
                               "Lagged GDP (logged)",
                               "2011 population (logged)",
                               "Lagged targeted transfer",
                               "PCI score",
                               "Count of CCDI routine inspection actitivies"),
          digits=2,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title="Evidence of ex ante protection in Vietnam robust to changing/dropping province mentions",
          out=c("../Tables/robust_exante_mention_vn.txt",
                "../Tables/robust_exante_mention_vn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="robust_exante_mention_vn")

### Replace GDP with alternative measures ####
check.mentions.b1 <- update(mentions.sanction.e, . ~. - log(GDP_l1y) + log(total.rev_l1y))
check.mentions.b1.r <- coeftest(check.mentions.b1, vcov=vcovHC, type="HC1")
check.mentions.b1.r

check.mentions.b2 <- update(mentions.sanction.e, . ~. - log(GDP_l1y) + log(total.exp_l1y))
check.mentions.b2.r <- coeftest(check.mentions.b2, vcov=vcovHC, type="HC1")
check.mentions.b2.r

ProvinceData$gdp_cap_l1y <- ProvinceData$GDP_l1y / ProvinceData$population_2011
check.mentions.b3 <- update(mentions.sanction.e, . ~. - log(population_2011) -log(GDP_l1y) + gdp_cap_l1y)
check.mentions.b3.r <- coeftest(check.mentions.b3, vcov=vcovHC, type="HC1")
check.mentions.b3.r

# Table A11
stargazer(check.mentions.b1,check.mentions.b2,check.mentions.b3,
          keep.stat = c("n", "adj.rsq"), omit="factor",
          table.placement = "H",
          add.lines=list(c("Year FE",rep("Yes",3))
                         ),
          se=list(check.mentions.b1.r[,2],check.mentions.b2.r[,2],check.mentions.b3.r[,2]),
          dep.var.labels = "Count of CCDI investigations in province",
          covariate.labels = c("Province's ties to incumbent GS",
                               "Count of all CCDI activities",
                               "Lagged GDP (logged)",
                               "2011 population (logged)",
                               "Lagged targeted transfer",
                               "PCI score",
                               "Lagged revenue (logged)",
                               "Lagged expenditure (logged)",
                               "Lagged GDP per capita (logged)"),
          digits=2,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title="Evidence of ex ante protection in Vietnam robust to alternative economic controls",
          out=c("../Tables/robust_exante_econ_vn.txt",
                "../Tables/robust_exante_econ_vn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="robust_exante_econ_vn")

### Replace PCI and PAPI with some subscores ####
check.mentions.e <- update(mentions.sanction.e, . ~. - pci + rank)
check.mentions.e.r <- coeftest(check.mentions.e, vcov=vcovHC, type="HC1")
check.mentions.e.r

check.mentions.e2 <- update(mentions.sanction.e, . ~. - pci + transparency)
check.mentions.e2.r <- coeftest(check.mentions.e2, vcov=vcovHC, type="HC1")
check.mentions.e2.r

check.mentions.e3 <- update(mentions.sanction.e, . ~. - pci + informal_charges)
check.mentions.e3.r <- coeftest(check.mentions.e3, vcov=vcovHC, type="HC1")
check.mentions.e3.r

check.mentions.e4 <- update(mentions.sanction.e, . ~. - pci + bias)
check.mentions.e4.r <- coeftest(check.mentions.e4, vcov=vcovHC, type="HC1")
check.mentions.e4.r

check.mentions.e5 <- update(mentions.sanction.e, . ~. - pci + legal_institutions)
check.mentions.e5.r <- coeftest(check.mentions.e5, vcov=vcovHC, type="HC1")
check.mentions.e5.r

# Table A12
stargazer(check.mentions.e,check.mentions.e2,check.mentions.e3,check.mentions.e4,check.mentions.e5,
          keep.stat = c("n", "adj.rsq"), omit="factor",
          table.placement = "H",
          add.lines=list(c("Year FE",rep("Yes",5))
                         ),
          se=list(check.mentions.e.r[,2],check.mentions.e2.r[,2],check.mentions.e3.r[,2],check.mentions.e4.r[,2],check.mentions.e5.r[,2]),
          dep.var.labels = "Count of CCDI investigations in province",
          covariate.labels = c("Province's ties to incumbent GS",
                               "Count of all CCDI activities",
                               "Lagged GDP (logged)",
                               "2011 population (logged)",
                               "Lagged targeted transfer",
                               "PCI rank",
                               "PCI transparency subscore",
                               "PCI informal charges subscore",
                               "PCI bias subscore",
                               "PCI legal institutions subscore"
                               ),
          digits=2,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title="Evidence of ex ante protection in Vietnam robust to alternate governance measures - PCI subscores",
          out=c("../Tables/robust_exante_pci_vn.txt",
                "../Tables/robust_exante_pci_vn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="robust_exante_pci_vn")


check.mentions.f <- update(mentions.sanction.e, . ~. - pci + unweighted_papi)
check.mentions.f.r <- coeftest(check.mentions.f, vcov=vcovHC, type="HC1")
check.mentions.f.r

check.mentions.f2 <- update(mentions.sanction.e, . ~. - pci + vertical_accountability)
check.mentions.f2.r <- coeftest(check.mentions.f2, vcov=vcovHC, type="HC1")
check.mentions.f2.r

check.mentions.f3 <- update(mentions.sanction.e, . ~. - pci + control_of_corruption)
check.mentions.f3.r <- coeftest(check.mentions.f3, vcov=vcovHC, type="HC1")
check.mentions.f3.r

check.mentions.f4 <- update(mentions.sanction.e, . ~. - pci + limits_on_public_sector_corruption)
check.mentions.f4.r <- coeftest(check.mentions.f4, vcov=vcovHC, type="HC1")
check.mentions.f4.r

check.mentions.f5 <- update(mentions.sanction.e, . ~. - pci + willingness_to_fight_corruption)
check.mentions.f5.r <- coeftest(check.mentions.f5, vcov=vcovHC, type="HC1")
check.mentions.f5.r

check.mentions.f6 <- update(mentions.sanction.e, . ~. - pci + basic_infrastructure)
check.mentions.f6.r <- coeftest(check.mentions.f6, vcov=vcovHC, type="HC1")
check.mentions.f6.r

# Table A13
stargazer(check.mentions.f,check.mentions.f2,check.mentions.f3,check.mentions.f4,check.mentions.f5,check.mentions.f6,
          keep.stat = c("n", "adj.rsq"), omit="factor",
          table.placement = "H",
          add.lines=list(c("Year FE",rep("Yes",6))
                         ),
          se=list(check.mentions.f.r[,2],check.mentions.f2.r[,2],check.mentions.f3.r[,2],check.mentions.f4.r[,2],check.mentions.f5.r[,2],check.mentions.f6.r[,2]),
          dep.var.labels = "Count of CCDI investigations in province",
          covariate.labels = c("Province's ties to incumbent GS",
                               "Count of all CCDI activities",
                               "Lagged GDP (logged)",
                               "2011 population (logged)",
                               "Lagged targeted transfer",
                               "PAPI unweighted score",
                               "PAPI vertical accountability",
                               "PAPI control of corruption",
                               "PAPI limits on public sector corruption",
                               "PAPI willingness to fight corruption",
                               "PAPI basic infrastructure"
          ),
          digits=2,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title="Evidence of ex ante protection in Vietnam robust to alternate governance measures - PAPI subscores",
          out=c("../Tables/robust_exante_papi_vn.txt",
                "../Tables/robust_exante_papi_vn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="robust_exante_papi_vn")

## Other measures instead of transfer share
check.mentions.g <- update(mentions.sanction.e, . ~. - target.trans_share_l1y + ratio_l1y)
check.mentions.g.r <- coeftest(check.mentions.g, vcov=vcovHC, type="HC1")
check.mentions.g.r

check.mentions.h <- update(mentions.sanction.e, . ~. - target.trans_share_l1y + balance.trans_share_l1y)
check.mentions.h.r <- coeftest(check.mentions.h, vcov=vcovHC, type="HC1")
check.mentions.h.r

check.mentions.i <- update(mentions.sanction.e, . ~. - target.trans_share_l1y + project.trans_share_l1y)
check.mentions.i.r <- coeftest(check.mentions.i, vcov=vcovHC, type="HC1")
check.mentions.i.r

check.mentions.j <- update(mentions.sanction.e, . ~. - target.trans_share_l1y + net.trans_share_l1y)
check.mentions.j.r <- coeftest(check.mentions.j, vcov=vcovHC, type="HC1")
check.mentions.j.r

# Table A14
stargazer(check.mentions.g,check.mentions.h,check.mentions.i,check.mentions.j,
          keep.stat = c("n", "adj.rsq"), omit="factor",
          table.placement = "H",
          add.lines=list(c("Year FE",rep("Yes",4))
                         ),
          se=list(check.mentions.g.r[,2],check.mentions.h.r[,2],check.mentions.i.r[,2],check.mentions.j.r[,2]),
          dep.var.labels = "Count of CCDI investigations in province",
          covariate.labels = c("Province's ties to incumbent GS",
                               "Count of all CCDI activities",
                               "Lagged GDP (logged)",
                               "2011 population (logged)",
                               "PCI score",
                               "Lagged transfer ratio",
                               "Lagged balance transfer (share of revenue)",
                               "Lagged project transfer (share of revenue)",
                               "Lagged net transfer (share of revenue)"
                               ),
          digits=2,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title="Evidence of ex ante protection in Vietnam robust to alternative measures of central-local independence",
          out=c("../Tables/robust_exante_inde_vn.txt",
                "../Tables/robust_exante_inde_vn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="robust_exante_inde_vn")

## Dummy for Southern provinces
check.mentions.k <- update(mentions.sanction.e, . ~. + south)
check.mentions.k.r <- coeftest(check.mentions.k, vcov=vcovHC, type="HC1")
check.mentions.k.r

ProvinceData$HN <- ProvinceData$province_eng == "Ha Noi"
ProvinceData$HCMC <- ProvinceData$province_eng == "TP HCM"
check.mentions.l1 <- update(mentions.sanction.e, . ~.  + HCMC)
check.mentions.l1.r <- coeftest(check.mentions.l1, vcov=vcovHC, type="HC1")
check.mentions.l1.r
check.mentions.l2 <- update(mentions.sanction.e, . ~.  + HN)
check.mentions.l2.r <- coeftest(check.mentions.l2, vcov=vcovHC, type="HC1")
check.mentions.l2.r
check.mentions.l3 <- update(mentions.sanction.e, . ~.  + HN_HCMC)
check.mentions.l3.r <- coeftest(check.mentions.l3, vcov=vcovHC, type="HC1")
check.mentions.l3.r

## Dummy for concurrent PS/People's Council/People's Committee
check.mentions.m <- update(mentions.sanction.e, . ~.  + PS_concurrent)
check.mentions.m.r <- coeftest(check.mentions.m, vcov=vcovHC, type="HC1")
check.mentions.m.r

# Table A15
stargazer(check.mentions.k,check.mentions.l1,check.mentions.l2,check.mentions.l3,check.mentions.m,
          keep.stat = c("n", "adj.rsq"), omit="factor",
          table.placement = "H",
          add.lines=list(c("Year FE",rep("Yes",6))
                         ),
          se=list(check.mentions.k.r[,2],check.mentions.l1.r[,2],check.mentions.l2.r[,2],check.mentions.l3.r[,2],check.mentions.m.r[,2]),
          dep.var.labels = "Count of CCDI investigations in province",
          covariate.labels = c("Province's ties to incumbent GS",
                               "Count of all CCDI activities",
                               "Lagged GDP (logged)",
                               "2011 population (logged)",
                               "Lagged targeted transfer",
                               "PCI score",
                               "Southern province dummy",
                               "HCM City dummy",
                               "Hanoi dummy",
                               "Hanoi + HCM City dummy",
                               "PS is People's Council head"
          ),
          digits=2,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title="Evidence of ex ante protection in Vietnam robust to alternative measures of local elites' clout",
          out=c("../Tables/robust_exante_localpwr_vn.txt",
                "../Tables/robust_exante_localpwr_vn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="robust_exante_inde_vn")

## Ties with Nguyen Tan Dung, Nguyen Xuan Phuc and Tran Quoc Vuong
check.mentions.n1 <- update(mentions.sanction.e, . ~.  -tie_a_pb_NguyenPhuTrong + tie_a_pb_NguyenTanDung) # Nguyen Tan Dung
check.mentions.n1.r <- coeftest(check.mentions.n1, vcov=vcovHC, type="HC1")
check.mentions.n1.r
check.mentions.n2 <- update(mentions.sanction.e, . ~.  + tie_a_pb_NguyenTanDung) # Nguyen Tan Dung
check.mentions.n2.r <- coeftest(check.mentions.n2, vcov=vcovHC, type="HC1")
check.mentions.n2.r
check.mentions.o1 <- update(mentions.sanction.e, . ~.  -tie_a_pb_NguyenPhuTrong + tie_a_pb_NguyenXuanPhuc) # Nguyen Xuan Phuc
check.mentions.o1.r <- coeftest(check.mentions.o1, vcov=vcovHC, type="HC1")
check.mentions.o1.r
check.mentions.o2 <- update(mentions.sanction.e, . ~.   + tie_a_pb_NguyenXuanPhuc) # Nguyen Xuan Phuc
check.mentions.o2.r <- coeftest(check.mentions.o2, vcov=vcovHC, type="HC1")
check.mentions.o2.r
check.mentions.p1 <- update(mentions.sanction.e, . ~.  -tie_a_pb_NguyenPhuTrong + tie_a_pb_TranQuocVuong) # Tran Quoc Vuong
check.mentions.p1.r <- coeftest(check.mentions.p1, vcov=vcovHC, type="HC1")
check.mentions.p1.r
check.mentions.p2 <- update(mentions.sanction.e, . ~.   + tie_a_pb_TranQuocVuong) # Tran Quoc Vuong
check.mentions.p2.r <- coeftest(check.mentions.p2, vcov=vcovHC, type="HC1")
check.mentions.p2.r

# Table A16
stargazer(check.mentions.n1,check.mentions.n2,check.mentions.o1,check.mentions.o2,check.mentions.p1,check.mentions.p2,
          keep.stat = c("n", "adj.rsq"), omit="factor",
          table.placement = "H",
          add.lines=list(c("Year FE",rep("Yes",6))),
          se=list(check.mentions.n1.r[,2],check.mentions.n2.r[,2],check.mentions.o1.r[,2],check.mentions.o2.r[,2],check.mentions.p1.r[,2],check.mentions.p2.r[,2]),
          dep.var.labels = "Count of CCDI investigations in province",
          covariate.labels = c("Province's ties to incumbent GS",
                               "Count of all CCDI activities",
                               "Lagged GDP (logged)",
                               "2011 population (logged)",
                               "Lagged targeted transfer",
                               "PCI Score",
                               "Province's ties to outgoing PM",
                               "Province's ties to incumbent PM",
                               "Province's ties to incumbent CCDI head"
          ),
          digits=2,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title="Evidence of ex ante protection by regime leader in Vietnam robust after controlling for other central elites' ties",
          out=c("../Tables/robust_exante_ties_vn.txt",
                "../Tables/robust_exante_ties_vn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="robust_exante_ties_vn")

## Different types of ties with Nguyen Phu Trong
check.mentions.q <- update(mentions.sanction.e, . ~.  -tie_a_pb_NguyenPhuTrong + tie_w_pb_NguyenPhuTrong) # Work ties only
check.mentions.q.r <- coeftest(check.mentions.q, vcov=vcovHC, type="HC1")
check.mentions.q.r
check.mentions.r <- update(mentions.sanction.e, . ~.  -tie_a_pb_NguyenPhuTrong + tie_b_pb_NguyenPhuTrong) # Birthplace ties only
check.mentions.r.r <- coeftest(check.mentions.r, vcov=vcovHC, type="HC1")
check.mentions.r.r
check.mentions.s <- update(mentions.sanction.e, . ~.  -tie_a_pb_NguyenPhuTrong + tie_u_pb_NguyenPhuTrong) # School ties only
check.mentions.s.r <- coeftest(check.mentions.s, vcov=vcovHC, type="HC1")
check.mentions.s.r

# Table A17
stargazer(check.mentions.q,check.mentions.r,check.mentions.s,
          keep.stat = c("n", "adj.rsq"), omit="factor",
          table.placement = "H",
          order = "tie_",
          add.lines=list(c("Year FE",rep("Yes",6))),
          se=list(check.mentions.q.r[,2],check.mentions.r.r[,2],check.mentions.s.r[,2]),
          dep.var.labels = "Count of CCDI investigations in province",
          covariate.labels = c("Province's workplace ties to incumbent GS",
                               "Province's hometown ties to incumbent GS",
                               "Province's education ties to incumbent GS",
                               "Count of all CCDI activities",
                               "Lagged GDP (logged)",
                               "2011 population (logged)",
                               "Lagged targeted transfer",
                               "Province's ties to outgoing PM",
                               "Province's ties to incumbent PM",
                               "Province's ties to incumbent CCDI head"
          ),
          digits=2,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title="Evidence of ex ante protection in Vietnam robust to alternative coding of factional ties",
          out=c("../Tables/robust_exante_trong_vn.txt",
                "../Tables/robust_exante_trong_vn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="robust_exante_trong_vn")

## Ex Post Protection ####
# Models to start with
punish1e.polr %>% summary()
### Mentions.inspect instead of all mentions ####
check.punish1.a <- update(punish1e.polr, . ~. - mentions + mentions.inspect)
check.punish1.a %>% summary()

### Replace GDP with alternative measures ####
check.punish1.b <- update(punish1e.polr, . ~. - log(GDP_l1y) + log(total.rev_l1y))
check.punish1.b %>% summary()

check.punish1.b2 <- update(punish1e.polr, . ~. - log(GDP_l1y) + log(total.exp_l1y))
check.punish1.b2 %>% summary()

IncidentList_all$gdp_cap_l1y <- IncidentList_all$GDP_l1y / IncidentList_all$population_2011
check.punish1.b3 <- update(punish1e.polr, . ~.  -log(GDP_l1y) + log(gdp_cap_l1y))
check.punish1.b3 %>% summary()

# Table A18
stargazer(check.punish1.b,check.punish1.b2,check.punish1.b3,
          omit="factor*",
          table.placement = "H",
          add.lines=list(c("Year FE",rep("Yes",4))),
          p.auto=FALSE, #report="vc*",
          dep.var.labels = "Sanction level log odds",
          covariate.labels = c("Province's ties to incumbent GS",
                               "Investigated office level (district)","Investigated office level (province)","Investigated office level (center)",
                               "Official left office",
                               "IC level (district)","IC level (province)","IC level (center)",
                               "Serious violation",
                               "Lagged GDP (logged)",
                               "2011 population (logged)",
                               "Lagged targeted transfer",
                               "PCI score",
                               "Lagged revenue (logged)",
                               "Lagged expenditure (logged)",
                               "Lagged GDP per capita (logged)"
                               ),
          digits=3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title="No evidence of ex post protection in Vietnam across all alternative economic controls",
          out=c("../Tables/robust_expost_econ_vn.txt",
                "../Tables/robust_expost_econ_vn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="robust_expost_econ_vn")

### No seriousness ####
check.punish1.c <- update(punish1e.polr, . ~.  -serious)
check.punish1.c %>% summary()

# Table A19
stargazer(punish1e.polr, check.punish1.c,
          omit="factor*",
          table.placement = "H",
          add.lines=list(c("Year FE",rep("Yes",4))),
          p.auto=FALSE, #report="vc*",
          dep.var.labels = "Sanction level log odds",
          covariate.labels = c("Province's ties to incumbent GS",
                               "Investigated office level (district)","Investigated office level (province)","Investigated office level (center)",
                               "Official left office",
                               "IC level (district)","IC level (province)","IC level (center)",
                               "Serious violation",
                               "Lagged GDP (logged)",
                               "2011 population (logged)",
                               "Lagged targeted transfer",
                               "PCI score",
                               "Lagged revenue (logged)",
                               "Lagged expenditure (logged)",
                               "Lagged GDP per capita (logged)"
          ),
          digits=3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title="No evidence of ex post protection in Vietnam with or without seriousness indicator",
          out=c("../Tables/robust_expost_serious_vn.txt",
                "../Tables/robust_expost_serious_vn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="robust_expost_serious_vn")

### Replace PCI and PAPI with some subscores ####
check.punish1.e <- update(punish1e.polr, . ~. - pci + rank)
check.punish1.e %>% summary()

check.punish1.e2 <- update(punish1e.polr, . ~. - pci + transparency)
check.punish1.e2 %>% summary()

check.punish1.e3 <- update(punish1e.polr, . ~. - pci + informal_charges)
check.punish1.e3 %>% summary()

check.punish1.e4 <- update(punish1e.polr, . ~. - pci + bias)
check.punish1.e4 %>% summary()

check.punish1.e5 <- update(punish1e.polr, . ~. - pci + legal_institutions)
check.punish1.e5 %>% summary()

# Table A20
stargazer(check.punish1.e,check.punish1.e2,check.punish1.e3,check.punish1.e4,check.punish1.e5,
          omit="factor*",
          table.placement = "H",
          add.lines=list(c("Year FE",rep("Yes",5))),
          p.auto=FALSE, #report="vc*",
          dep.var.labels = "Sanction level log odds",
          covariate.labels = c("Province's ties to incumbent GS",
                               "Investigated office level (district)","Investigated office level (province)","Investigated office level (center)",
                               "Official left office",
                               "IC level (district)","IC level (province)","IC level (center)",
                               "Serious violation",
                               "Lagged GDP (logged)",
                               "2011 population (logged)",
                               "Lagged targeted transfer",
                               "PCI rank",
                               "PCI transparency subscore",
                               "PCI informal charges subscore",
                               "PCI bias subscore",
                               "PCI legal institutions subscore"
          ),
          digits=3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title="No evidence of ex post protection in Vietnam across all PCI subscore specifications",
          out=c("../Tables/robust_expost_pci_vn.txt",
                "../Tables/robust_expost_pci_vn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="robust_expost_pci_vn")

check.punish1.f <- update(punish1e.polr, . ~. - pci + unweighted_papi)
check.punish1.f %>% summary()

check.punish1.f2 <- update(punish1e.polr, . ~. - pci + vertical_accountability)
check.punish1.f2 %>% summary()

check.punish1.f3 <- update(punish1e.polr, . ~. - pci + control_of_corruption)
check.punish1.f3 %>% summary()

check.punish1.f4 <- update(punish1e.polr, . ~. - pci + limits_on_public_sector_corruption)
check.punish1.f4 %>% summary()

check.punish1.f5 <- update(punish1e.polr, . ~. - pci + willingness_to_fight_corruption)
check.punish1.f5 %>% summary()

check.punish1.f6 <- update(punish1e.polr, . ~. - pci + basic_infrastructure)
check.punish1.f6 %>% summary()

# Table A21
stargazer(check.punish1.f,check.punish1.f2,check.punish1.f3,check.punish1.f4,check.punish1.f5,check.punish1.f6,
          omit="factor*",
          table.placement = "H",
          add.lines=list(c("Year FE",rep("Yes",6))),
          p.auto=FALSE, #report="vc*",
          dep.var.labels = "Sanction level log odds",
          covariate.labels = c("Province's ties to incumbent GS",
                               "Investigated office level (district)","Investigated office level (province)","Investigated office level (center)",
                               "Official left office",
                               "IC level (district)","IC level (province)","IC level (center)",
                               "Serious violation",
                               "Lagged GDP (logged)",
                               "2011 population (logged)",
                               "Lagged targeted transfer",
                               "PAPI unweighted score",
                               "PAPI vertical accountability",
                               "PAPI control of corruption",
                               "PAPI limits on public sector corruption",
                               "PAPI willingness to fight corruption",
                               "PAPI basic infrastructure"
          ),
          digits=3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title="No evidence of ex post protection in Vietnam across all PAPI subscore specifications",
          out=c("../Tables/robust_expost_papi_vn.txt",
                "../Tables/robust_expost_papi_vn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="robust_expost_papi_vn")

### Other measures instead of transfer share ####
check.punish1.g <- update(punish1e.polr, . ~. - target.trans_share_l1y + ratio_l1y)
check.punish1.g %>% summary()

check.punish1.h <- update(punish1e.polr, . ~. - target.trans_share_l1y + balance.trans_share_l1y)
check.punish1.h %>% summary()

check.punish1.i <- update(punish1e.polr, . ~. - target.trans_share_l1y + project.trans_share_l1y)
check.punish1.i %>% summary()

check.punish1.j <- update(punish1e.polr, . ~. - target.trans_share_l1y + net.trans_share_l1y)
check.punish1.j %>% summary()

# Table A22
stargazer(check.punish1.g,check.punish1.h,check.punish1.i,check.punish1.j,
          omit="factor*",
          table.placement = "H",
          add.lines=list(c("Year FE",rep("Yes",5))),
          p.auto=FALSE, #report="vc*",
          dep.var.labels = "Sanction level log odds",
          covariate.labels = c("Province's ties to incumbent GS",
                               "Investigated office level (district)","Investigated office level (province)","Investigated office level (center)",
                               "Official left office",
                               "IC level (district)","IC level (province)","IC level (center)",
                               "Serious violation",
                               "Lagged GDP (logged)",
                               "2011 population (logged)",
                               "PCI score",
                               "Lagged transfer ratio",
                               "Lagged balance transfer (share of revenue)",
                               "Lagged project transfer (share of revenue)",
                               "Lagged net transfer (share of revenue)"

          ),
          digits=3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title="No evidence of ex post protection in Vietnam across alternative measures of central-local independence",
          out=c("../Tables/robust_expost_inde_vn.txt",
                "../Tables/robust_expost_inde_vn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="robust_expost_inde_vn")

### Dummy for Southern provinces ####
check.punish1.k <- update(punish1e.polr, . ~. + south)
check.punish1.k %>% summary()

check.punish1.l <- update(punish1e.polr, . ~.  + HN_HCMC)
check.punish1.l %>% summary()

### Dummy for concurrent PS/People's Council/People's Committee ####
check.punish1.m <- update(punish1e.polr, . ~.  + PS_concurrent)
check.punish1.m %>% summary()

# Table A23
stargazer(check.punish1.k,check.punish1.l,check.punish1.m,
          omit="factor*",
          table.placement = "H",
          add.lines=list(c("Year FE",rep("Yes",5))),
          p.auto=FALSE, #report="vc*",
          dep.var.labels = "Sanction level log odds",
          covariate.labels = c("Province's ties to incumbent GS",
                               "Investigated office level (district)","Investigated office level (province)","Investigated office level (center)",
                               "Official left office",
                               "IC level (district)","IC level (province)","IC level (center)",
                               "Serious violation",
                               "Lagged GDP (logged)",
                               "2011 population (logged)",
                               "PCI score",
                               "Lagged targeted transfer",
                               "Southern province dummy",
                               "Hanoi + HCM City dummy",
                               "PS is People's Council head"

          ),
          digits=3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title="No evidence of ex post protection in Vietnam across alternative measures of central-local independence",
          out=c("../Tables/robust_expost_localpwr_vn.txt",
                "../Tables/robust_expost_localpwr_vn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="robust_expost_localpwr_vn")

### Ties with Nguyen Tan Dung, Nguyen Xuan Phuc and Tran Quoc Vuong ####
check.punish1.n <- update(punish1e.polr, . ~.  -tie_a_pb_NguyenPhuTrong + tie_a_pb_NguyenTanDung) # Nguyen Tan Dung
check.punish1.n %>% summary()
check.punish1.o <- update(punish1e.polr, . ~.  -tie_a_pb_NguyenPhuTrong + tie_a_pb_NguyenXuanPhuc) # Nguyen Xuan Phuc
check.punish1.o %>% summary()
check.punish1.p <- update(punish1e.polr, . ~.  -tie_a_pb_NguyenPhuTrong + tie_a_pb_TranQuocVuong) # Tran Quoc Vuong
check.punish1.p %>% summary()

# Table A24
stargazer(check.punish1.n,check.punish1.o,check.punish1.p,
          omit="factor*",
          table.placement = "H",
          order = "tie",
          add.lines=list(c("Year FE",rep("Yes",5))),
          p.auto=FALSE, #report="vc*",
          dep.var.labels = "Sanction level log odds",
          covariate.labels = c("Province's ties to outgoing PM",
                               "Province's ties to incumbent PM",
                               "Province's ties to incumbent CCDI head",
                               "Investigated office level (district)","Investigated office level (province)","Investigated office level (center)",
                               "Official left office",
                               "IC level (district)","IC level (province)","IC level (center)",
                               "Serious violation",
                               "Lagged GDP (logged)",
                               "2011 population (logged)",
                               "PCI score",
                               "Lagged targeted transfer",
                               "Southern province dummy",
                               "Hanoi + HCM City dummy",
                               "PS is People's Council head"

          ),
          digits=3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title="No evidence of ex post protection by other central elites in Vietnam",
          out=c("../Tables/robust_expost_ties_vn.txt",
                "../Tables/robust_expost_ties_vn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="robust_expost_ties_vn")

### Linear probability model ####

#### Main models ####
punish3a <- lm(punishment2~tie_a_pb_NguyenPhuTrong, data=subset(IncidentList_all, !is.na(punishment)))
punish3a.r <- coeftest(punish3a, vcovHC="HC1")
punish3a.r
punish3b <- lm(punishment2~tie_a_pb_NguyenPhuTrong+office_level+past_offence, data=subset(IncidentList_all, !is.na(punishment)))
punish3b.r <- coeftest(punish3b, vcovHC="HC1")
punish3b.r
punish3c <- lm(punishment2~tie_a_pb_NguyenPhuTrong+office_level+past_offence+ubkt_level+serious, data=subset(IncidentList_all, !is.na(punishment)))
punish3c.r <- coeftest(punish3c, vcovHC="HC1")
punish3c.r
punish3d <- lm(punishment2~tie_a_pb_NguyenPhuTrong+office_level+past_offence+ubkt_level+serious+log(GDP_l1y)+log(population_2011)+target.trans_share_l1y+pci, data=subset(IncidentList_all, !is.na(punishment)))
punish3d.r <- coeftest(punish3d, vcovHC="HC1")
punish3d.r
punish3e <- lm(punishment2~tie_a_pb_NguyenPhuTrong+factor(year)+office_level+past_offence+ubkt_level+serious+log(GDP_l1y)+log(population_2011)+target.trans_share_l1y+pci, data=subset(IncidentList_all, !is.na(punishment)))
punish3e.r <- coeftest(punish3e, vcovHC="HC1")
punish3e.r

# Table A25
stargazer(punish3a,punish3b,punish3c,punish3d,punish3e,
          omit=c("factor*","Constant"),
          table.placement = "H",
          p.auto=FALSE,
          omit.stat = c("f","ser","rsq","adj.rsq"),
          add.lines=list(c("Year FE",rep("",4),"Yes","No")),
          se=list(punish3a.r[,2],punish3b.r[,2],punish3c.r[,2],punish3d.r[,2],punish3e.r[,2]),
          dep.var.labels = c("Probability of serious sanction"),
          covariate.labels = c("Province's ties to incumbent GS",
                               "Investigated office level (district)","Investigated office level (province)","Investigated office level (center)",
                               "Official left office",
                               "IC level (district)","IC level (province)","IC level (center)",
                               "Serious violation",
                               "Lagged GDP (logged)","2011 population (logged)","Lagged targeted transfer","PCI score"),
          digits=3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          # notes = "Results reported with odds ratio adjusted standard errors",
          title="No evidence of ex post protection in Vietnam - binary DV",
          out=c("../Tables/robust_expost_lpm_vn.txt",
                "../Tables/robust_expost_lpm_vn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="robust_expost_lpm_vn")

#### Mentions.inspect instead of all mentions ####
check.punish3.a <- update(punish3e, . ~. - mentions + mentions.inspect)
check.punish3.a.r <- coeftest(check.punish3.a, vcov=vcovHC, type="HC1")
check.punish3.a.r

#### Replace GDP with alternative measures ####
check.punish3.b <- update(punish3e, . ~. - log(GDP_l1y) + log(total.rev_l1y))
check.punish3.b.r <- coeftest(check.punish3.b, vcov=vcovHC, type="HC1")
check.punish3.b.r

check.punish3.c <- update(punish3e, . ~. - log(GDP_l1y) + log(total.exp_l1y))
check.punish3.c.r <- coeftest(check.punish3.c, vcov=vcovHC, type="HC1")
check.punish3.c.r

ProvinceData$gdp_cap_l1y <- ProvinceData$GDP_l1y / ProvinceData$population_2011
check.punish3.d <- update(punish3e, . ~. - log(population_2011) -log(GDP_l1y) + gdp_cap_l1y)
check.punish3.d.r <- coeftest(check.punish3.d, vcov=vcovHC, type="HC1")
check.punish3.d.r

# Table A26
stargazer(check.punish3.b,check.punish3.c,check.punish3.d,
          se=list(check.punish3.b.r[,2],check.punish3.c.r[,2],check.punish3.d.r[,2]),
          omit="factor*", keep.stat = c("n", "adj.rsq"),
          table.placement = "H",
          add.lines=list(c("Year FE",rep("Yes",3))),
          p.auto=FALSE, #report="vc*",
          dep.var.labels = "Probability of dismissal/expulsion",
          covariate.labels = c("Province's ties to incumbent GS",
                               "Investigated office level (district)","Investigated office level (province)","Investigated office level (center)",
                               "Official left office",
                               "IC level (district)","IC level (province)","IC level (center)",
                               "Serious violation",
                               "Lagged GDP (logged)",
                               "2011 population (logged)",
                               "Lagged targeted transfer",
                               "PCI score",
                               "Lagged revenue (logged)",
                               "Lagged expenditure (logged)",
                               "Lagged GDP per capita (logged)"
          ),
          digits=3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title="No evidence of ex post protection in Vietnam across all alternative economic controls - binary DV",
          out=c("../Tables/robust_expost_econ_lpm_vn.txt",
                "../Tables/robust_expost_econ_lpm_vn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="robust_expost_econ_lpm_vn")

#### No seriousness ####
check.punish3.q <- update(punish3e, . ~.  -serious)
check.punish3.q.r <- coeftest(check.punish3.q, vcov=vcovHC, type="HC1")
check.punish3.q.r

# Table A27
stargazer(punish3e, check.punish3.q,
          se=list(punish3e.r[,2],check.punish3.q.r[,2]),
          omit="factor*", keep.stat = c("n", "adj.rsq"),
          table.placement = "H",
          add.lines=list(c("Year FE",rep("Yes",2))),
          p.auto=FALSE, #report="vc*",
          dep.var.labels = "Probability of dismissal/expulsion",
         covariate.labels = c("Province's ties to incumbent GS",
                               "Investigated office level (district)","Investigated office level (province)","Investigated office level (center)",
                               "Official left office",
                               "IC level (district)","IC level (province)","IC level (center)",
                               "Serious violation",
                               "Lagged GDP (logged)",
                               "2011 population (logged)",
                               "Lagged targeted transfer",
                               "PAPI unweighted score",
                               "PAPI vertical accountability",
                               "PAPI control of corruption",
                               "PAPI limits on public sector corruption",
                               "PAPI willingness to fight corruption",
                               "PAPI basic infrastructure"
          ),
          digits=3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title="No evidence of ex post protection in Vietnam with or without seriousness indicator - binary DV",
          out=c("../Tables/robust_expost_serious_lpm_vn.txt",
                "../Tables/robust_expost_serious_lpm_vn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="robust_expost_serious_lpm_vn")

#### Replace PCI and PAPI with some subscores ####
check.punish3.e <- update(punish3e, . ~. - pci + rank)
check.punish3.e.r <- coeftest(check.punish3.e, vcov=vcovHC, type="HC1")
check.punish3.e.r

check.punish3.e2 <- update(punish3e, . ~. - pci + transparency)
check.punish3.e2.r <- coeftest(check.punish3.e2, vcov=vcovHC, type="HC1")
check.punish3.e2.r

check.punish3.e3 <- update(punish3e, . ~. - pci + informal_charges)
check.punish3.e3.r <- coeftest(check.punish3.e3, vcov=vcovHC, type="HC1")
check.punish3.e3.r

check.punish3.e4 <- update(punish3e, . ~. - pci + bias)
check.punish3.e4.r <- coeftest(check.punish3.e4, vcov=vcovHC, type="HC1")
check.punish3.e4.r

check.punish3.e5 <- update(punish3e, . ~. - pci + legal_institutions)
check.punish3.e5.r <- coeftest(check.punish3.e5, vcov=vcovHC, type="HC1")
check.punish3.e5.r

# Table A28
stargazer(check.punish3.e,check.punish3.e2,check.punish3.e3,check.punish3.e4,check.punish3.e5,
          se=list(check.punish3.e.r[,2],check.punish3.e2.r[,2],check.punish3.e3.r[,2],check.punish3.e4.r[,2],check.punish3.e5.r[,2]),
          omit="factor*", keep.stat = c("n", "adj.rsq"),
          table.placement = "H",
          add.lines=list(c("Year FE",rep("Yes",5))),
          p.auto=FALSE,
          dep.var.labels = "Probability of dismissal/expulsion",
          covariate.labels = c("Province's ties to incumbent GS",
                               "Investigated office level (district)","Investigated office level (province)","Investigated office level (center)",
                               "Official left office",
                               "IC level (district)","IC level (province)","IC level (center)",
                               "Serious violation",
                               "Lagged GDP (logged)",
                               "2011 population (logged)",
                               "Lagged targeted transfer",
                               "PCI rank",
                               "PCI transparency subscore",
                               "PCI informal charges subscore",
                               "PCI bias subscore",
                               "PCI legal institutions subscore"
          ),
          digits=3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title="No evidence of ex post protection in Vietnam across all PCI subscore specifications - binary DV",
          out=c("../Tables/robust_expost_pci_lpm_vn.txt",
                "../Tables/robust_expost_pci_lpm_vn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="robust_expost_pci_lpm_vn")

check.punish3.f <- update(punish3e, . ~. - pci + unweighted_papi)
check.punish3.f.r <- coeftest(check.punish3.f, vcov=vcovHC, type="HC1")
check.punish3.f.r

check.punish3.f2 <- update(punish3e, . ~. - pci + vertical_accountability)
check.punish3.f2.r <- coeftest(check.punish3.f2, vcov=vcovHC, type="HC1")
check.punish3.f2.r

check.punish3.f3 <- update(punish3e, . ~. - pci + control_of_corruption)
check.punish3.f3.r <- coeftest(check.punish3.f3, vcov=vcovHC, type="HC1")
check.punish3.f3.r

check.punish3.f4 <- update(punish3e, . ~. - pci + limits_on_public_sector_corruption)
check.punish3.f4.r <- coeftest(check.punish3.f4, vcov=vcovHC, type="HC1")
check.punish3.f4.r

check.punish3.f5 <- update(punish3e, . ~. - pci + willingness_to_fight_corruption)
check.punish3.f5.r <- coeftest(check.punish3.f5, vcov=vcovHC, type="HC1")
check.punish3.f5.r

check.punish3.f6 <- update(punish3e, . ~. - pci + basic_infrastructure)
check.punish3.f6.r <- coeftest(check.punish3.f6, vcov=vcovHC, type="HC1")
check.punish3.f6.r

# Table A29
stargazer(check.punish3.f,check.punish3.f2,check.punish3.f3,check.punish3.f4,check.punish3.f5,check.punish3.f6,
          se=list(check.punish3.f.r[,2],check.punish3.f2.r[,2],check.punish3.f3.r[,2],check.punish3.f4.r[,2],check.punish3.f5.r[,2],check.punish3.f6.r[,2]),
          omit="factor*", keep.stat = c("n", "adj.rsq"),
          table.placement = "H",
          add.lines=list(c("Year FE",rep("Yes",6))),
          p.auto=FALSE,
          dep.var.labels = "Probability of dismissal/expulsion",
          covariate.labels = c("Province's ties to incumbent GS",
                               "Investigated office level (district)","Investigated office level (province)","Investigated office level (center)",
                               "Official left office",
                               "IC level (district)","IC level (province)","IC level (center)",
                               "Serious violation",
                               "Lagged GDP (logged)",
                               "2011 population (logged)",
                               "Lagged targeted transfer",
                               "PAPI unweighted score",
                               "PAPI vertical accountability",
                               "PAPI control of corruption",
                               "PAPI limits on public sector corruption",
                               "PAPI willingness to fight corruption",
                               "PAPI basic infrastructure"
          ),
          digits=3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title="No evidence of ex post protection in Vietnam across all PAPI subscore specifications - binary DV",
          out=c("../Tables/robust_expost_papi_lpm_vn.txt",
                "../Tables/robust_expost_papi_lpm_vn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="robust_expost_papi_lpm_vn")

#### Other measures instead of transfer share ####
check.punish3.g <- update(punish3e, . ~. - target.trans_share_l1y + ratio_l1y)
check.punish3.g.r <- coeftest(check.punish3.g, vcov=vcovHC, type="HC1")
check.punish3.g.r

check.punish3.h <- update(punish3e, . ~. - target.trans_share_l1y + balance.trans_share_l1y)
check.punish3.h.r <- coeftest(check.punish3.h, vcov=vcovHC, type="HC1")
check.punish3.h.r

check.punish3.i <- update(punish3e, . ~. - target.trans_share_l1y + project.trans_share_l1y)
check.punish3.i.r <- coeftest(check.punish3.i, vcov=vcovHC, type="HC1")
check.punish3.i.r

check.punish3.j <- update(punish3e, . ~. - target.trans_share_l1y + net.trans_share_l1y)
check.punish3.j.r <- coeftest(check.punish3.j, vcov=vcovHC, type="HC1")
check.punish3.j.r

# Table A30
stargazer(check.punish3.g,check.punish3.h,check.punish3.i,check.punish3.j,
          se=list(check.punish3.g.r[,2],check.punish3.h.r[,2],check.punish3.i.r[,2],check.punish3.j.r[,2]),
          omit="factor*", keep.stat = c("n", "adj.rsq"),
          table.placement = "H",
          add.lines=list(c("Year FE",rep("Yes",5))),
          p.auto=FALSE,
          dep.var.labels = "Probability of dismissal/expulsion",
          covariate.labels = c("Province's ties to incumbent GS",
                               "Investigated office level (district)","Investigated office level (province)","Investigated office level (center)",
                               "Official left office",
                               "IC level (district)","IC level (province)","IC level (center)",
                               "Serious violation",
                               "Lagged GDP (logged)",
                               "2011 population (logged)",
                               "PCI score",
                               "Lagged transfer ratio",
                               "Lagged balance transfer (share of revenue)",
                               "Lagged project transfer (share of revenue)",
                               "Lagged net transfer (share of revenue)"

          ),
          digits=3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title="No evidence of ex post protection in Vietnam across alternative measures of central-local independence - binary DV",
          out=c("../Tables/robust_expost_inde_lpm_vn.txt",
                "../Tables/robust_expost_inde_lpm_vn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="robust_expost_inde_lpm_vn")

#### Dummy for Southern provinces ####
check.punish3.k <- update(punish3e, . ~. + south)
check.punish3.k.r <- coeftest(check.punish3.k, vcov=vcovHC, type="HC1")
check.punish3.k.r

check.punish3.l <- update(punish3e, . ~.  + HN_HCMC)
check.punish3.l.r <- coeftest(check.punish3.l, vcov=vcovHC, type="HC1")
check.punish3.l.r

#### Dummy for concurrent PS/People's Council/People's Committee ####
check.punish3.m <- update(punish3e, . ~.  + PS_concurrent)
check.punish3.m.r <- coeftest(check.punish3.m, vcov=vcovHC, type="HC1")
check.punish3.m.r

# Table A31
stargazer(check.punish3.k,check.punish3.l,check.punish3.m,
          se=list(check.punish3.k.r[,2],check.punish3.l.r[,2],check.punish3.m.r[,2]),
          omit="factor*", keep.stat = c("n", "adj.rsq"),
          table.placement = "H",
          add.lines=list(c("Year FE",rep("Yes",3))),
          p.auto=FALSE,
          dep.var.labels = "Probabilty of dismissal/expulsion",
          covariate.labels = c("Province's ties to incumbent GS",
                               "Investigated office level (district)","Investigated office level (province)","Investigated office level (center)",
                               "Official left office",
                               "IC level (district)","IC level (province)","IC level (center)",
                               "Serious violation",
                               "Lagged GDP (logged)",
                               "2011 population (logged)",
                               "PCI score",
                               "Lagged targeted transfer",
                               "Southern province dummy",
                               "Hanoi + HCM City dummy",
                               "PS is People's Council head"

          ),
          digits=3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title="No evidence of ex post protection in Vietnam across alternative measures of central-local independence - binary DV",
          out=c("../Tables/robust_expost_localpwr_lpm_vn.txt",
                "../Tables/robust_expost_localpwr_lpm_vn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="robust_expost_localpwr_lpm_vn")

#### Ties with Nguyen Tan Dung, Nguyen Xuan Phuc and Tran Quoc Vuong ####
check.punish3.n <- update(punish3e, . ~.  -tie_a_pb_NguyenPhuTrong + tie_a_pb_NguyenTanDung) # Nguyen Tan Dung
check.punish3.n.r <- coeftest(check.punish3.n, vcov=vcovHC, type="HC1")
check.punish3.n.r
check.punish3.o <- update(punish3e, . ~.  -tie_a_pb_NguyenPhuTrong + tie_a_pb_NguyenXuanPhuc) # Nguyen Xuan Phuc
check.punish3.o.r <- coeftest(check.punish3.o, vcov=vcovHC, type="HC1")
check.punish3.o.r
check.punish3.p <- update(punish3e, . ~.  -tie_a_pb_NguyenPhuTrong + tie_a_pb_TranQuocVuong) # Tran Quoc Vuong
check.punish3.p.r <- coeftest(check.punish3.p, vcov=vcovHC, type="HC1")
check.punish3.p.r

# Table A32
stargazer(check.punish3.n,check.punish3.o,check.punish3.p,
          se=list(check.punish3.n.r[,2],check.punish3.o.r[,2],check.punish3.p.r[,2]),
          omit="factor*", keep.stat = c("n", "adj.rsq"),
          table.placement = "H",
          order = "tie",
          add.lines=list(c("Year FE",rep("Yes",5))),
          p.auto=FALSE,
          dep.var.labels = "Probability of dismissal/expulsion",
          covariate.labels = c("Province's ties to outgoing PM",
                               "Province's ties to incumbent PM",
                               "Province's ties to incumbent CCDI head",
                               "Investigated office level (district)","Investigated office level (province)","Investigated office level (center)",
                               "Official left office",
                               "IC level (district)","IC level (province)","IC level (center)",
                               "Serious violation",
                               "Lagged GDP (logged)",
                               "2011 population (logged)",
                               "PCI score",
                               "Lagged targeted transfer",
                               "Southern province dummy",
                               "Hanoi + HCM City dummy",
                               "PS is People's Council head"

          ),
          digits=3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title="No evidence of ex post protection by other central elites in Vietnam - binary DV",
          out=c("../Tables/robust_expost_ties_lpm_vn.txt",
                "../Tables/robust_expost_ties_lpm_vn.tex"),
          font.size = "scriptsize",
          column.sep.width = "1pt",
          label="robust_expost_ties_lpm_vn")

# SENSITIVITY ANALYSIS ####
###~~~~~~~~~~~~~~~~~~~~~~~~

##### Ex ante protection #####
### Remove provinces one by one - mentions of sanctions
mentions.missing.province <- data.frame("province"=unique(ProvinceData$province_eng),
                                        "coef"=NA, "se"=NA, "p"=NA)
for (i in 1:nrow(mentions.missing.province)){
  tryCatch({
    sensitive.mentions <- update(mentions.sanction.e, data=subset(ProvinceData, province_eng!=mentions.missing.province$province[i]))
    sensitive.mentions.r <- coeftest(sensitive.mentions, vcov=vcovHC, type="HC1")
    mentions.missing.province$coef[i] <- sensitive.mentions.r["tie_a_pb_NguyenPhuTrong",1]
    mentions.missing.province$se[i] <- sensitive.mentions.r["tie_a_pb_NguyenPhuTrong",2]
    mentions.missing.province$p[i] <- sensitive.mentions.r["tie_a_pb_NguyenPhuTrong",4] < 0.05
  }, error=function(e){})}
# Figure A7 - first
mentions.missing.province %>%
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
  labs(title="Effect of factional ties to VCP GS on number of investigations",
       subtitle = "Excluding one province at a time",
       x="Province excluded",y="Effect size")
ggsave("../Graphs/coef_exante_exclude_province_vn.png", width=20,height=10,units="in")

### Remove years one by one - mentions of sanctions
# Figure A7 - second
mentions.missing.year <- data.frame("year"=unique(ProvinceData$year),
                                    "coef"=NA, "se"=NA, "p"=NA)
for (i in 1:nrow(mentions.missing.year)){
  tryCatch({
    sensitive.mentions <- update(mentions.sanction.e, data=subset(ProvinceData, year!=mentions.missing.year$year[i]))
    sensitive.mentions.r <- coeftest(sensitive.mentions, vcov=vcovHC, type="HC1")
    mentions.missing.year$coef[i] <- sensitive.mentions.r["tie_a_pb_NguyenPhuTrong",1]
    mentions.missing.year$se[i] <- sensitive.mentions.r["tie_a_pb_NguyenPhuTrong",2]
    mentions.missing.year$p[i] <- sensitive.mentions.r["tie_a_pb_NguyenPhuTrong",4] < 0.05
  }, error=function(e){})}

mentions.missing.year %>%
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
  labs(title="Effect of factional ties to VCP GS on number of investigations",
       subtitle = "Excluding one year at a time",
       x="Year excluded",y="Effect size")
ggsave("../Graphs/coef_exante_exclude_year_vn.png", width=20,height=10,units="in")

##### Ex post protection #####

### Remove provinces one by one - punishment level, polr
# Figure A7 - third
punish1.missing.province <- data.frame("province"=unique(IncidentList_all$province_eng),
                                       "coef"=NA, "se"=NA, "p"=NA)
for (i in 1:nrow(punish1.missing.province)){
  tryCatch({
    sensitive.punish1 <- update(punish1d.polr, data=subset(IncidentList_all, province_eng!=punish1.missing.province$province[i]))
    sensitive.punish1.r <- coeftest(sensitive.punish1)
    punish1.missing.province$coef[i] <- sensitive.punish1.r["tie_a_pb_NguyenPhuTrong",1]
    punish1.missing.province$se[i] <- sensitive.punish1.r["tie_a_pb_NguyenPhuTrong",2]
    punish1.missing.province$p[i] <- sensitive.punish1.r["tie_a_pb_NguyenPhuTrong",4] < 0.05
  }, error=function(e){})}
punish1.missing.province %>%
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
  labs(title="Effect of factional ties to VCP GS on odds of severe punishments",
       subtitle = "Excluding one province at a time",
       x="Province excluded",y="Effect size")
ggsave("../Graphs/coef_expost_exclude_province_vn.png", width=20,height=10,units="in")

### Remove years one by one - punishment level, polr
# Figure A7 - bottom
punish1.missing.year <- data.frame("year"=unique(IncidentList_all$year),
                                   "coef"=NA, "se"=NA, "p"=NA)
for (i in 1:nrow(punish1.missing.year)){
  tryCatch({
    sensitive.punish1 <- update(punish1d.polr, data=subset(IncidentList_all, year!=punish1.missing.year$year[i]))
    sensitive.punish1.r <- coeftest(sensitive.punish1)
    punish1.missing.year$coef[i] <- sensitive.punish1.r["tie_a_pb_NguyenPhuTrong",1]
    punish1.missing.year$se[i] <- sensitive.punish1.r["tie_a_pb_NguyenPhuTrong",2]
    punish1.missing.year$p[i] <- sensitive.punish1.r["tie_a_pb_NguyenPhuTrong",4] < 0.05
  }, error=function(e){})}
punish1.missing.year %>%
  ggplot(aes(x=as.factor(year), y=coef, color=p)) +
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
  labs(title="Effect of factional ties to VCP GS on odds of severe punishment",
       subtitle = "Excluding one year at a time",
       x="Year excluded",y="Effect size")
ggsave("../Graphs/coef_expost_exclude_year_vn.png", width=20,height=10,units="in")

# SAVE WORKSPACE #####
###~~~~~~~~~~~~~~~~~~~
save.image(file = "workspace_vietnam.RData")
