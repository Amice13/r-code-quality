library(here)
dir <- here()
setwd(dir)
source("0-init.R")





##### Load data #####

dat <- readRDS("congressional-district-data.rds")
ideal_points <- readRDS("ideal-points.rds")





##### Table 1 #####

votes_table <-
  dat %>%
  filter(party=="Republican") %>%
  group_by(is_tea_party_incum2008) %>%
  summarize(
    mean.HR3221 = mean(vote_HR3221=="Yea", na.rm=TRUE),
    mean.HR1424 = mean(vote_HR1424=="Yea", na.rm=TRUE)
  ) %>% ungroup()

votes_table %<>% mutate(
  is_tea_party_incum2008 = ifelse(is_tea_party_incum2008, "Tea Party", "Non-Tea Party")
)
names(votes_table) <-
  c("House GOP candidate type", "\\% Yea on H.R.3221", "\\% Yea on H.R.1424")
votes_table$`\\% Yea on H.R.3221` %<>% {paste0(round(., digits=2)*100, "\\%")}
votes_table$`\\% Yea on H.R.1424` %<>% {paste0(round(., digits=2)*100, "\\%")}

votes_table %>%
  xtable(
    align=c("llcc"),
    label="table:votes",
    caption="Roll-Call Records on Key Bills Related to the Foreclosure Crisis"
  ) %>%
  print.xtable(include.rownames = FALSE, comment=FALSE, type="latex",
               sanitize.text.function=function(x){x},
               hline.after = c(-1, 0, 2),
               file=paste0("table-1.tex"),
               caption.placement = "top",
               table.placement="H"
  )





##### Tables 8 & A16 #####

mod1 <- 
  dat %>%
  feols(is_tea_party_challpri2010 ~ chHOMdefrate0208 + is_tea_party_incum2010 + open_seat | state, data = ., cluster="state")

mod2 <- 
  dat %>%
  feols(
    is_tea_party_challpri2010 ~ chHOMdefrate0208  + is_tea_party_incum2010 + open_seat + district_pres_vs + ave_mean_ideology | state, data = ., cluster="state")

mod3 <- 
  dat %>%
  feols(as.formula(paste0(
      "is_tea_party_challpri2010 ~ chHOMdefrate0208  + is_tea_party_incum2010 + open_seat + district_pres_vs + ave_mean_ideology + ", 
      paste0(names(dat)[13:27], collapse=" + "), 
      " | state")), 
    data = ., cluster="state")

setFixest_dict(c(is_tea_party_challpri2010 = "Has Non-incumbent Tea Party Candidate",
                 state = "State",
                 chHOMdefrate0208 = "Change in Default Rate, 2001-07"
)
)

setFixest_etable(fitstat = ~ n)

tablestyle  = style.tex(main="aer",
                        fixef.suffix = " FEs",
                        fixef.where ="var",
                        fixef.title = "\\midrule",
                        stats.title = "\\midrule",
                        tablefoot=F,
                        yesNo="\\checkmark")

etable(mod1, mod2, mod3, 
       file = paste0("table-8.tex"),
       title = "Emergence of Tea Party Nonincumbent Candidates",
       cluster=~state,
       drop = "(Intercept)",
       group=list("\\midrule Electoral Controls"=c("is_tea_party_incum2010", "open_seat"),
                  "Political Controls"=c("district_pres_vs", "ave_mean_ideology"),
                  "Demographic Controls"=names(dat)[13:27]),
       label = "table:selection-paper",
       digits=3,
       digits.stats=2,
       style.tex=tablestyle,
       replace = TRUE,
       depvar = FALSE
)

setFixest_dict(c(is_tea_party_challpri2010 = "Has Non-incumbent Tea Party Candidate",
                 is_tea_party_incum2010 = "Has Tea Party Incumbent",
                 state = "State",
                 chHOMdefrate0208 = "Change in Default Rate, 2001-07",
                 district_pres_vs = "'08 Democratic Presidential Vote Shares",
                 ave_mean_ideology = "Past Ave. Republican Candidate CFScore",
                 white = "White",
                 black = "Black",
                 hispanic = "Hispanic",
                 lmedhhinc = "Logged Median Household Income",
                 educ_lths = "Less Than High School",
                 educ_hs = "High School Diplomas",
                 educ_somecol = "Some College",
                 educ_col = "College Degrees",
                 age15_24 = "Age 15-24",
                 age25_34 = "Age 25-34",
                 age35_44 = "Age 35-44",
                 age45_54 = "Age 45-54",
                 age55_59 = "Age 55-59",
                 age60_64 = "Age 60-64",
                 age65_74 = "Age 65-74",
                 open_seat = "Open Seat"
)
)

setFixest_etable(fitstat = ~ n)

tablestyle  = style.tex(main="aer",
                        fixef.suffix = " FEs",
                        fixef.where ="var",
                        fixef.title = "\\midrule",
                        stats.title = "\\midrule",
                        tablefoot=F,
                        yesNo="\\checkmark")

etable(mod1, mod2, mod3,
       file = paste0("table-A16.tex"),
       title = "Emergence of Tea Party Nonincumbent Candidate",
       cluster=~state,
       drop = "(Intercept)",
       label = "table:selection-appendix",
       fontsize="small",
       digits=3,
       digits.stats=2,
       style.tex=tablestyle,
       replace = TRUE,
       depvar = FALSE
)





##### Tables 9 & A17 #####

mod1 <- 
  dat %>%
  feols(
    as.formula(paste0(
      "tea_party_share ~ chHOMdefrate0208 + is_tea_party_incum2010 + open_seat", 
      " | state")), 
    data = ., cluster="state")

mod2 <- 
  dat %>%
  feols(
    as.formula(paste0(
      "tea_party_share ~ chHOMdefrate0208 + is_tea_party_incum2010 + open_seat + district_pres_vs + ave_mean_ideology ", 
      " | state")), 
    data = ., cluster="state")

mod3 <- dat %>%
  feols(
    as.formula(paste0(
      "tea_party_share ~ chHOMdefrate0208 + is_tea_party_incum2010 + open_seat + district_pres_vs + ave_mean_ideology + ", 
      paste0(names(dat)[13:27], collapse=" + "), 
      " | state")), 
    data = ., cluster="state")

setFixest_dict(c(is_tea_party_challpri2010 = "Has Non-incumbent Tea Party Candidate",
                 tea_party_share = "Tea Party Candidate's Primary Vote Share",
                 state = "State",
                 chHOMdefrate0208 = "Change in Default Rate, 2001-07"
)
)

setFixest_etable(fitstat = ~ n)

tablestyle  = style.tex(main="aer",
                        fixef.suffix = " FEs",
                        fixef.where ="var",
                        fixef.title = "",
                        stats.title = "\\midrule",
                        tablefoot=F,
                        yesNo="\\checkmark")

etable(mod1, mod2, mod3, 
       file = paste0("table-9.tex"),
       title = "Tea Party Candidates' Primary Vote Shares",
       cluster=~state,
       drop = "(Intercept)",
       group=list("\\midrule Electoral Controls"=c("is_tea_party_incum2010", "open_seat"),
                  "Political Controls"=c("district_pres_vs", "ave_mean_ideology"),
                  "Demographic Controls"=names(dat)[13:27]),
       label = "table:primary-paper",
       digits=3,
       digits.stats=2,
       style.tex=tablestyle,
       replace = T,
       depvar = F
)

setFixest_dict(c(is_tea_party_challpri2010 = "Has Non-incumbent Tea Party Candidate",
                 is_tea_party_incum2010 = "Has Tea Party Incumbent",
                 tea_party_share = "Tea Party Candidate's Primary Vote Share",
                 state = "State",
                 chHOMdefrate0208 = "Change in Default Rate, 2001-07",
                 district_pres_vs = "'08 Democratic Presidential Vote Shares",
                 ave_mean_ideology = "Past Ave. Republican Candidate CFScore",
                 white = "White",
                 black = "Black",
                 hispanic = "Hispanic",
                 lmedhhinc = "Logged Median Household Income",
                 educ_lths = "Less Than High School",
                 educ_hs = "High School Diplomas",
                 educ_somecol = "Some College",
                 educ_col = "College Degrees",
                 age15_24 = "Age 15-24",
                 age25_34 = "Age 25-34",
                 age35_44 = "Age 35-44",
                 age45_54 = "Age 45-54",
                 age55_59 = "Age 55-59",
                 age60_64 = "Age 60-64",
                 age65_74 = "Age 65-74",
                 open_seat = "Open Seat"
)
)

setFixest_etable(fitstat = ~ n)

tablestyle  = style.tex(main="aer",
                        fixef.suffix = " FEs",
                        fixef.where ="var",
                        fixef.title = "\\midrule",
                        stats.title = "\\midrule",
                        tablefoot=F,
                        tabular="X",
                        yesNo="\\checkmark")

etable(mod1, mod2, mod3,
       file = paste0("table-A17.tex"),
       title = "Tea Party Candidates' Primary Vote Shares",
       cluster=~state,
       drop = "(Intercept)",
       # group=list("\\midrule"),
       label = "table:primary-appendix",
       fontsize="small",
       digits=3,
       digits.stats=2,
       style.tex=tablestyle,
       replace = T,
       depvar = F
)





##### Tables A14 & A15 #####

mod3221_1 <- 
  dat %>% 
  mutate(dv3221 = (vote_HR3221=="Yea")) %>%
  filter(party=="Republican") %>%
  mutate(
    white2 = (as.character(white_national_cut_3)=="2"),
    white3 = (as.character(white_national_cut_3)=="3"),
    tea_party_white2 = is_tea_party_incum2008 * white2,
    tea_party_white3 = is_tea_party_incum2008 * white3,
    tea_party_default = is_tea_party_incum2008 * chHOMdefrate0208,
    default_white2 = chHOMdefrate0208 * white2,
    default_white3 = chHOMdefrate0208 * white3,
    tea_party_default_white2 = is_tea_party_incum2008 * chHOMdefrate0208 * white2,
    tea_party_default_white3 = is_tea_party_incum2008 * chHOMdefrate0208 * white3
  ) %>%
  feols (dv3221 ~ chHOMdefrate0208 + is_tea_party_incum2008 + white2 + white3 + tea_party_default + default_white2 + default_white3 + tea_party_white2 + tea_party_white3 + tea_party_default_white2 + tea_party_default_white3, data=., cluster="state") 
mod3221_1 %<>% summary()

mod1424_1 <- 
  dat %>% 
  mutate(dv1424 = (vote_HR1424=="Yea")) %>%
  filter(party=="Republican") %>%
  mutate(
    white2 = (as.character(white_national_cut_3)=="2"),
    white3 = (as.character(white_national_cut_3)=="3"),
    tea_party_white2 = is_tea_party_incum2008 * white2,
    tea_party_white3 = is_tea_party_incum2008 * white3,
    tea_party_default = is_tea_party_incum2008 * chHOMdefrate0208,
    default_white2 = chHOMdefrate0208 * white2,
    default_white3 = chHOMdefrate0208 * white3,
    tea_party_default_white2 = is_tea_party_incum2008 * chHOMdefrate0208 * white2,
    tea_party_default_white3 = is_tea_party_incum2008 * chHOMdefrate0208 * white3
  ) %>%
  feols (dv1424 ~ chHOMdefrate0208 + is_tea_party_incum2008 + white2 + white3 + tea_party_default + default_white2 + default_white3 + tea_party_white2 + tea_party_white3 + tea_party_default_white2 + tea_party_default_white3, data=., cluster="state") 
mod1424_1 %<>% summary()

mod3221_3 <- 
  dat %>% 
  mutate(dv3221 = (vote_HR3221=="Yea")) %>%
  filter(party=="Republican") %>%
  mutate(
    white2 = (as.character(white_national_cut_3)=="2"),
    white3 = (as.character(white_national_cut_3)=="3"),
    tea_party_white2 = is_tea_party_incum2008 * white2,
    tea_party_white3 = is_tea_party_incum2008 * white3,
    tea_party_default = is_tea_party_incum2008 * chHOMdefrate0208,
    default_white2 = chHOMdefrate0208 * white2,
    default_white3 = chHOMdefrate0208 * white3,
    tea_party_default_white2 = is_tea_party_incum2008 * chHOMdefrate0208 * white2,
    tea_party_default_white3 = is_tea_party_incum2008 * chHOMdefrate0208 * white3
  ) %>%
  feols (dv3221 ~ chHOMdefrate0208 + is_tea_party_incum2008 + white2 + white3 + tea_party_default + default_white2 + default_white3 + tea_party_white2 + tea_party_white3 + tea_party_default_white2 + tea_party_default_white3 + Fincom + logaggsec2 + margin06 + notrunning, data=., cluster="state") 
mod3221_3 %<>% summary()

mod1424_3 <- 
  dat %>% 
  mutate(dv1424 = (vote_HR1424=="Yea")) %>%
  filter(party=="Republican") %>%
  mutate(
    white2 = (as.character(white_national_cut_3)=="2"),
    white3 = (as.character(white_national_cut_3)=="3"),
    tea_party_white2 = is_tea_party_incum2008 * white2,
    tea_party_white3 = is_tea_party_incum2008 * white3,
    tea_party_default = is_tea_party_incum2008 * chHOMdefrate0208,
    default_white2 = chHOMdefrate0208 * white2,
    default_white3 = chHOMdefrate0208 * white3,
    tea_party_default_white2 = is_tea_party_incum2008 * chHOMdefrate0208 * white2,
    tea_party_default_white3 = is_tea_party_incum2008 * chHOMdefrate0208 * white3
  ) %>%
  feols (dv1424 ~ chHOMdefrate0208 + is_tea_party_incum2008 + white2 + white3 + tea_party_default + default_white2 + default_white3 + tea_party_white2 + tea_party_white3 + tea_party_default_white2 + tea_party_default_white3 + Fincom + logaggsec2 + margin06 + notrunning, data=., cluster="state") 
mod1424_3 %<>% summary()

mod3221_4 <-
  dat %>%
  mutate(dv3221 = (vote_HR3221=="Yea")) %>%
  filter(party=="Republican") %>%
  mutate(
    white2 = (as.character(white_national_cut_3)=="2"),
    white3 = (as.character(white_national_cut_3)=="3"),
    tea_party_white2 = is_tea_party_incum2008 * white2,
    tea_party_white3 = is_tea_party_incum2008 * white3,
    tea_party_default = is_tea_party_incum2008 * chHOMdefrate0208,
    default_white2 = chHOMdefrate0208 * white2,
    default_white3 = chHOMdefrate0208 * white3,
    tea_party_default_white2 = is_tea_party_incum2008 * chHOMdefrate0208 * white2,
    tea_party_default_white3 = is_tea_party_incum2008 * chHOMdefrate0208 * white3
  ) %>%
  feols (dv3221 ~ chHOMdefrate0208 + is_tea_party_incum2008 + white2 + white3 + tea_party_default + default_white2 + default_white3 + tea_party_white2 + tea_party_white3 + tea_party_default_white2 + tea_party_default_white3 + Fincom + logaggsec2 + margin06 + notrunning | state, data=., cluster="state")
mod3221_4 %<>% summary()

mod1424_4 <-
  dat %>%
  mutate(dv1424 = (vote_HR1424=="Yea")) %>%
  filter(party=="Republican") %>%
  mutate(
    white2 = (as.character(white_national_cut_3)=="2"),
    white3 = (as.character(white_national_cut_3)=="3"),
    tea_party_white2 = is_tea_party_incum2008 * white2,
    tea_party_white3 = is_tea_party_incum2008 * white3,
    tea_party_default = is_tea_party_incum2008 * chHOMdefrate0208,
    default_white2 = chHOMdefrate0208 * white2,
    default_white3 = chHOMdefrate0208 * white3,
    tea_party_default_white2 = is_tea_party_incum2008 * chHOMdefrate0208 * white2,
    tea_party_default_white3 = is_tea_party_incum2008 * chHOMdefrate0208 * white3
  ) %>%
  feols (dv1424 ~ chHOMdefrate0208 + is_tea_party_incum2008 + white2 + white3 + tea_party_default + default_white2 + default_white3 + tea_party_white2 + tea_party_white3 + tea_party_default_white2 + tea_party_default_white3 + Fincom + logaggsec2 + margin06 + notrunning | state, data=., cluster="state")
mod1424_4 %<>% summary()

setFixest_dict(c(dv3221 = "% Yea on H.R.3221",
                 dv1424 = "% Yea on H.R.1424",
                 state = "State",
                 chHOMdefrate0208 = "Change in Default Rate, 2001-07",
                 is_tea_party_incum2008TRUE = "Tea Party",
                 white2TRUE = "Medium % White",
                 white3TRUE = "High % White",
                 tea_party_default = "Tea Party $\\times$ Change in Default Rate, 2001-07",
                 default_white2 = "Medium % White $\\times$ Change in Default Rate, 2001-07",
                 default_white3 = "High % White $\\times$ Change in Default Rate, 2001-07",
                 tea_party_white2 = "Tea Party $\\times$ Medium % White",
                 tea_party_white3 = "Tea Party $\\times$ High % White",
                 tea_party_default_white2 = "Tea Party $\\times$ Medium % White $\\times$ Change in Default Rate, 2001-07",
                 tea_party_default_white3 = "Tea Party $\\times$ High % White $\\times$ Change in Default Rate, 2001-07",
                 Fincom = "Comm. Financial Services",
                 logaggsec2 = "Logged Campaign Contributions From the Financial Sector, 2007-08",
                 margin06 = "Vote Margin in 2006",
                 notrunning = "Not Running for Reelection"
)
)

setFixest_etable(fitstat = ~ n)

tablestyle  = style.tex(main="aer",
                        fixef.suffix = " FEs",
                        fixef.where ="var",
                        fixef.title = "\\midrule",
                        stats.title = "\\midrule",
                        tablefoot=F,
                        yesNo="\\checkmark")

etable(mod3221_1, mod3221_3, mod3221_4,
       file = paste0("table-A14.tex"),
       title = "House Republicans' Roll-Call Votes on the American Housing Rescue and Foreclosure Prevention Act of 2008 (H.R.3221)",
       cluster=~state,
       drop = "(Intercept)",
       label = "table:hr3221-appendix",
       fontsize="small",
       digits=3,
       digits.stats=2,
       style.tex=tablestyle,
       replace = T,
       depvar = F
)

etable(mod1424_1, mod1424_3, mod1424_4,
       file = paste0("table-A15.tex"),
       title = "House Republicans' Roll-Call Votes on the Emergency Economic Stabilization Act of 2008 (H.R.1424)",
       cluster=~state,
       drop = "(Intercept)",
       label = "table:hr1424-appendix",
       fontsize="small",
       digits=3,
       digits.stats=2,
       style.tex=tablestyle,
       replace = T,
       depvar = F
)





##### Figure 1 #####

min.dwnom1 <- min(ideal_points$dwnom1, na.rm=TRUE)
min.dwnom1 <- ifelse(min.dwnom1<0, -round(abs(min.dwnom1), digits=2), round(abs(min.dwnom1), digits=2))
max.dwnom1 <- max(ideal_points$dwnom1, na.rm=TRUE)
max.dwnom1 <- ifelse(max.dwnom1<0, -round(abs(max.dwnom1), digits=2), round(abs(max.dwnom1), digits=2))

p.dwnom <-
  ideal_points %>%
  mutate(
    is_tea_party=ifelse(is_tea_party, "Tea Party", "Non-Tea Party"),
    is_tea_party=factor(is_tea_party, levels=c("Tea Party", "Non-Tea Party"))
  ) %>%
  ggplot(mapping=aes(x=dwnom1, group=is_tea_party, fill=is_tea_party))+
  scale_fill_grey()+
  geom_density(alpha=0.5)+
  scale_x_continuous(limits=c(-1,1))+
  theme_light()+
  labs(
    fill = "GOP House Candidate Type",
    title= "1st-Dimension DW-NOMINATE"
  )+
  theme(
    axis.title = element_blank(),
    axis.text = element_text(size=13),
    plot.title = element_text(size=15, hjust=0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )+
  annotate(geom="text", x=-0.41, y=2.5, label="Tea Party median: 0.59 \n Non-Tea Party median: 0.39", size=3.8)

p.cfscore <-
  ideal_points %>%
  mutate(
    is_tea_party=ifelse(is_tea_party, "Tea Party", "Non-Tea Party"),
    is_tea_party=factor(is_tea_party, levels=c("Tea Party", "Non-Tea Party"))
  ) %>%
  ggplot(mapping=aes(x=recipient_cfscore, group=is_tea_party, fill=is_tea_party))+
  scale_fill_grey()+
  geom_density(alpha=0.5)+
  scale_x_continuous(limits=c(-2,2))+
  theme_light()+
  labs(
    fill = "GOP House Candidate Type",
    title= "Recipient CFscores"
  )+
  theme(
    axis.title = element_blank(),
    axis.text = element_text(size=13),
    plot.title = element_text(size=15, hjust=0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )+
  annotate(geom="text", x=-0.78, y=1.72, label="Tea Party median: 1.13 \n Non-Tea Party median: 1.00", size=3.8)

ggarrange(p.dwnom, p.cfscore, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
ggsave(paste0("figure-1.pdf"), width=7, height=2.5, device=cairo_pdf)

ideal_points %$% ks.test(dwnom1[is_tea_party], dwnom1[!is_tea_party])
ideal_points %$% ks.test(recipient_cfscore[is_tea_party], recipient_cfscore[!is_tea_party])

