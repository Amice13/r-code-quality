#_____________________________________________________________________________#
#  File-Name:   replicationFile_voicesOfThePartyBase.r   		       		
#  Date:        25/11/17                		
#_____________________________________________________________________________#
pkgs <- c('ggplot2','dplyr','tidyverse','estimatr','conflicted','cregg','Hmisc',
          'haven','magrittr','tidyr','texreg','qwraps2','xtable','gridExtra',
          'knitr')
# Comment in below if packages need to be installed
# for (pkg in pkgs) install.packages(pkg, character.only = TRUE)
for (pkg in pkgs) library(pkg, character.only = TRUE)
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("recode", "dplyr")
conflict_prefer("lag", "dplyr")
# Color palette for some of the figures in the supplementary appendix
cbbPalette <- c("#000000", "#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00", 
  "#CC79A7", "#F0E442")
# Global for a set of the most prominent party families
family_selection <- c("Christian democracy","Green/Ecologist","Social democracy",
                      "Far right","Conservative")

#_____________________________________________________________________________#
#_____________________________________________________________________________#
#_____________________________________________________________________________#
# Loading data ----
#_____________________________________________________________________________#
data <- read.csv('voicesOfThePartyBase.csv', stringsAsFactors=T) 

#_____________________________________________________________________________#
#_____________________________________________________________________________#
#______________________________________________________________________________#
# Policy divisions among party supporters ====
#______________________________________________________________________________#
# Figure 2: ====
# How respondents position themselves towards extreme statements by policy area
# and family of preferred party.
#______________________________________________________________________________#
pdf("figure2.pdf")
data %>%
  filter(party_family%in%family_selection) %>%
  droplevels() %>%
  pivot_longer(
    cols=policy_climate:policy_freedom,
    names_to="policy_area",
    values_to="policy_position") %>%
  mutate(
    policy_area=case_when(
      grepl(pattern="climate",x=policy_area) ~ "Climate",
      grepl(pattern="tech",x=policy_area) ~ "Tech companies",
      grepl(pattern="freedom",x=policy_area) ~ "Health and freedom",
      grepl(pattern="immi",x=policy_area) ~ "Immigrants' voting rights"),
    party_family=factor(recode(party_family,
      'Christian democracy'='Christian\ndemocracy',
      'Green/Ecologist'='Green/\nEcologist',
      'Social democracy'='Social\ndemocracy'),
      levels=c('Social\ndemocracy','Green/\nEcologist','Far right',
        'Conservative','Christian\ndemocracy')),
    policy_position=factor(policy_position,
      levels=c(as.character(unique(policy_position[policy_position!="neither" & 
      policy_position!="don't know"])),"neither","don't know"))) %>%
  ggplot() +
  geom_bar(aes(x=policy_position, y=after_stat(prop), group=1)) +
  facet_grid(party_family~policy_area,scales="free")+
  labs(title="",y="Share of respondents by preferred party", x="") +
  theme_bw() +
  theme(
    axis.text = element_text(size=10),
    text=element_text(family="serif"),
    axis.text.x=element_text(angle=45, hjust=0.9))
dev.off()
ggsave(filename="figure2.png", plot=last_plot(),device="png",width=15,height=18, 
  unit="cm",dpi=600)

#______________________________________________________________________________#
# Unified and divided party responses ====
#______________________________________________________________________________#
# Figure 3: ====
# Marginal means for how citizens want their most preferred party to respond to
# challengers.
#______________________________________________________________________________#
mms <- data %>% 
  droplevels() %>%
  cj(.,outcome~united,id=~id,estimate="mm")

pdf("figure3.pdf",height=2) 
mms %>% 
  filter(feature=="united") %>%
  mutate(level=factor(level,levels=c('ignore','divided','united'))) %>%
  ggplot(aes(x=level,y=estimate,ymin=lower,ymax=upper)) +
  geom_pointrange(position=position_dodge2(width=.6),size=.3) +
  coord_flip() + 
  labs(x='',
       y='How much do you like the reaction?\n1 (Do not like at all) - 7 (Like at lot)') + 
  theme_bw() +
  theme(
    legend.position='bottom',
    legend.title=element_blank(),
    text=element_text(family="serif"),
    axis.text = element_text(size=11),
    strip.background=element_blank())
dev.off()
ggsave(filename="figure3.png", plot=last_plot(),device="png",width=20,height=4, 
  unit="cm",dpi=600)

#______________________________________________________________________________#
# Supporters’ policy and response preferences ==== 
#______________________________________________________________________________#
# Figure 4: ====
# Marginal means for how citizens want their most preferred party to respond to 
# new parties depending on whether they agree with the proposal of the new party.
#______________________________________________________________________________#
mms <- data %>% 
  filter(united_detail!="") %>%
  droplevels() %>%
  cj(.,outcome~ united_detail,
     id=~id,
     by=~congruence,
     estimate="mm") 

pdf("figure4.pdf",height=5)
mms %>% 
  filter(feature=="united_detail") %>%
  mutate(
    level=factor(level,
      levels=c('ignore','adversarial + ignore',
      'adversarial + accommodative','adversarial', 'accommodative + ignore',
      'accommodative')),
     united=factor(ifelse(level=='accommodative'|level=='adversarial'|
      level=='ignore','United','Divided'),levels=c('')),
    congruence=factor(congruence,levels=c('same extreme position',
      'neither same nor opposite','opposite extreme position',
      "doesn't know"))) %>%
  ggplot(aes(x=level,y=estimate,ymin=lower,ymax=upper)) +
  facet_wrap(~factor(congruence),ncol=2) +
  geom_pointrange(position=position_dodge2(width=.6),size=.3) +
  coord_flip() + 
  labs(x='',
       y='How much do you like the reaction?\n1 (Do not like at all) - 7 (Like at lot)') + 
  theme_bw() +
  theme(text=element_text(family="serif"),
    axis.text = element_text(size=10),
    strip.background=element_blank()) 
dev.off()
ggsave(filename="figure4.png", plot=last_plot(),device="png",width=15,height=8, 
  unit="cm",dpi=600)

#______________________________________________________________________________#
# Divided electorates and best responses ====
#______________________________________________________________________________#
# prepare data
best_responses <- data %>% 
  # filter only parfams and issues of interest
  filter(
    (party_family=="Social democracy" & policy=="immi vote con") |
      (party_family=="Social democracy" & policy=="immi vote pro") |
      (party_family=="Christian democracy" & policy=="immi vote con") |
      (party_family=="Christian democracy" & policy=="immi vote pro") |
      (party_family=="Conservative" & policy=="immi vote con") |
      (party_family=="Conservative" & policy=="immi vote pro") |
      (party_family=="Social democracy" & policy=="freedom con") |
      (party_family=="Social democracy" & policy=="freedom pro") |
      (party_family=="Christian democracy" & policy=="freedom con") |
      (party_family=="Christian democracy" & policy=="freedom pro") |
      (party_family=="Conservative" & policy=="freedom con") |
      (party_family=="Conservative" & policy=="freedom pro")) %>%
  droplevels() %>%
  # merge Conservatives and Christ dem parties
  mutate(party_family=recode(party_family, 
    'Conservative'="Conservatives and Christian democracy",
    'Christian democracy'="Conservatives and Christian democracy")) %>%
  group_by(country, party_chosen) %>% 
  # Calculate party measures of division and residual categories chosen
  mutate(
    policy_freedom_pro = sum(policy_freedom=="never restrict")/n(),
    policy_freedom_con = sum(policy_freedom=="always restrict")/n(),
    policy_freedom_res = sum(policy_freedom=="neither" | policy_freedom=="don't know")/n(),
    policy_immigration_pro = sum(policy_immigration=="allowed to vote")/n(),
    policy_immigration_con = sum(policy_immigration=="not allowed to vote")/n(),
    policy_immigration_res = sum(policy_immigration=="neither" | 
      policy_immigration=="don't know")/n()) %>%
  group_by(country, party_chosen) %>% 
  # Recode by policy issue and party: 1 is perfect disunity, 0 is perfect unity
  mutate(
    policy_immigration=min(cbind(policy_immigration_pro,policy_immigration_con))/
      max(cbind(policy_immigration_pro,policy_immigration_con)),
    policy_freedom=min(cbind(policy_freedom_pro,policy_freedom_con))/
      max(cbind(policy_freedom_pro,policy_freedom_con)),
    policy_immigration_res=policy_immigration_res,
    policy_freedom_res=policy_freedom_res) %>%
  ungroup() %>%
  # choose corresponding measurements for each vignette seen
  mutate(
    policy_res=case_when(
      grepl(pattern="freedom",x=policy) ~ policy_freedom_res,
      grepl(pattern="immi",x=policy) ~ policy_immigration_res),
    policy_division=case_when(
      grepl(pattern="freedom",x=policy) ~ policy_freedom,
      grepl(pattern="immi",x=policy) ~ policy_immigration),
    groups=paste0(party_family, " vs ", policy)) %>%
  select(country,party_chosen,groups,id,united_detail,outcome,
    policy,policy_topic,policy_division,policy_res,
    policy_freedom_pro,policy_freedom_con,policy_immigration_pro,
    policy_immigration_con,populism) %>%
  # respondents choosing one category
  # set division
  mutate(
    policy_side = ifelse(grepl(pattern="pro",x = policy), "pro", "con"),
    electorate_side = case_when(
      policy_topic=="Immi voting"&policy_immigration_pro>policy_immigration_con~"pro",
      policy_topic=="Immi voting"&policy_immigration_pro<policy_immigration_con~"con",
      policy_topic=="Health and freedom"&policy_freedom_pro>policy_freedom_con~"pro",
      policy_topic=="Health and freedom"&policy_freedom_pro<policy_freedom_con~"con",
      TRUE ~ NA),
    policy_division_group=case_when(
      policy_division>0.5 ~"divided electorate",
      policy_division<=0.5&policy_side==electorate_side~"united electorate\nsupports the new\n party's proposal",
      policy_division<=0.5&policy_side!=electorate_side~"united electorate\nrejects the new\n party's proposal",
      TRUE ~ NA),
    policy_division_group=factor(policy_division_group,
      levels=c("united electorate\nsupports the new\n party's proposal",
        "united electorate\nrejects the new\n party's proposal",
        "divided electorate"))) %>%
  droplevels()

#______________________________________________________________________________#
# Figure 5: ====
# Support for the party reaction of Christian democratic and conservative parties 
# when faced with new parties proposing measures concerning the voting rights of 
# immigrants or basic rights in times of pandemics averaging across new parties' 
# attacks.
#______________________________________________________________________________#
r <- best_responses  %>%
  group_by(groups,policy_division_group) %>%
  do(lm_robust(outcome~0+united_detail, data=., clusters=id) %>% tidy()) %>%
  mutate(
    party_family=ifelse(grepl(pattern="Cons",x=groups),
      "Conservative and Christian democracy", "Social democracy"),
    policy=factor(case_when(
      grepl(pattern="freedom pro", x=groups) ~ "against restrictions",
      grepl(pattern="freedom con", x=groups) ~ "for restrictions",
      grepl(pattern="vote con", x=groups) ~ "against voting rights",
      grepl(pattern="vote pro", x=groups) ~ "for voting rights"),
      levels=c("for restrictions", "against restrictions",
        "for voting rights","against voting rights")))

pdf("figure5.pdf",height=4)
r %>%
  mutate(strategy=factor(str_sub(term,14,-1),
    levels=c('ignore','adversarial + ignore','adversarial + accommodative',
    'adversarial', 'accommodative + ignore','accommodative'))) %>%
  filter(!is.na(strategy)&party_family=="Conservative and Christian democracy") %>%
  ggplot(aes(x=strategy,y=estimate,ymin=conf.low,ymax=conf.high)) +
  facet_grid(policy_division_group~policy,switch='y') +
  geom_pointrange(position=position_dodge2(width=.6),size=.1) +
  coord_flip() + 
  scale_x_discrete(position="top") +
  labs(x='',
    y='How much do you like the reaction?\n1 (Do not like at all) - 7 (Like at lot)', 
    subtitle='New party is ...') + 
  theme_bw() +
  theme(text=element_text(family="serif"),axis.text = element_text(size=10),
    strip.background=element_blank(),strip.text.y=element_text(angle=0),
    plot.title=element_text(size=10,angle=90,hjust=-10),
    plot.subtitle=element_text(size=10),legend.position='bottom')
dev.off()
ggsave(filename="figure5.png", plot=last_plot(),device="png",width=15,height=10, 
  unit="cm",dpi=600)

#______________________________________________________________________________#
# Figure 6: ====
# Support for the party reaction of Social democratic parties 
# when faced with new parties proposing measures concerning the voting rights of 
# immigrants or basic rights in times of pandemics averaging across new parties' 
# attacks.
#______________________________________________________________________________#
pdf("figure6.pdf",height=4)
r %>%
  mutate(strategy=factor(str_sub(term,14,-1),
    levels=c('ignore','adversarial + ignore','adversarial + accommodative',
    'adversarial', 'accommodative + ignore','accommodative'))) %>%
  filter(!is.na(strategy)&party_family=="Social democracy") %>%
  ggplot(aes(x=strategy,y=estimate,ymin=conf.low,ymax=conf.high)) +
  facet_grid(policy_division_group~policy,switch='y') +
  geom_pointrange(position=position_dodge2(width=.6),size=.1) +
  coord_flip() + 
  scale_x_discrete(position="top") + labs(x='',
    y='How much do you like the reaction?\n1 (Do not like at all) - 7 (Like at lot)', 
    subtitle='New party is ...') + 
  theme_bw() +
  theme(
    text=element_text(family="serif"),axis.text = element_text(size=10),
    axis.title.y = element_text(angle = 0, hjust = 0.5, vjust = -1),
    strip.background=element_blank(),strip.text.y=element_text(angle=0),
    plot.title=element_text(size=10,angle=90,hjust=-10),
    plot.subtitle=element_text(size=10),legend.position='bottom')
dev.off()
ggsave(filename="figure6.png", plot=last_plot(),device="png",width=15,height=10, 
  unit="cm",dpi=600)

#_______________________________________________________________________________
# Figure 7: ----
# Marginal means for how citizens want their most preferred party to respond to 
# new parties in each of the countries under study
#_______________________________________________________________________________
mms <- data %>% 
  droplevels() %>%
  cj(.,outcome~united,
     id=~id,
     by=~country,
     estimate="mm")

pdf("figure7.pdf",height=6) 
mms %>% 
  filter(feature!="policy") %>%
  mutate(level=factor(level,levels=c('united','divided','ignore'))) %>%
  ggplot(aes(x=reorder(level,desc(level)),y=estimate,ymin=lower,ymax=upper)) +
  facet_wrap(~country,ncol = 3) +
  geom_pointrange(position=position_dodge2(width=.6),size=.3) +
  coord_flip() + 
  scale_color_manual(values=cbbPalette) +
  labs(x='',
       y='How much do you like the reaction?\n1 (Do not like at all) - 7 (Like at lot)') + 
  theme_bw() +
  theme(legend.position='bottom',
        legend.title=element_blank(),
        text=element_text(family="serif"),
        axis.text = element_text(size=10),
        strip.background=element_blank())
dev.off()
ggsave(filename="figure7.png", plot=last_plot(),device="png",width=15,height=12, 
       unit="cm",dpi=600)

#______________________________________________________________________________#
# Figure 8: ----
# Marginal means for how citizens want their most preferred party to respond to
# new parties depending on whether they agree with the proposal of the new party 
# for Western/Northern, Southern (ES, GR, IT, PT), and Central/Eastern Europe 
# (HU, PL, RO) separately.Central/Eastern Europe Southern Europe Western/Northern 
# Europe
#______________________________________________________________________________#
mms <- data %>% 
  mutate(region=factor(ifelse(country=="AT"|country=="DE"|country=="DK"|
    country=="FR"|country=="IE"|country=="NL"|country=="SE",'Western/Northern Europe',
    ifelse(country=="ES"|country=="GR"|country=="IT"|country=="PT",'Southern Europe',
    "Central/Eastern Europe")))) %>%
  filter(united_detail!="") %>%
  droplevels() %>%
  cj(.,outcome~ united_detail+vignette,
     id=~id,
     by=~congruence+region,
     estimate="mm")

pdf("figure8.pdf",height=5)
mms %>% 
  filter(feature=="united_detail") %>%
  mutate(congruence=factor(congruence,level=c('same extreme position',
    'opposite extreme position','neither same nor opposite',
    "doesn't know"))) %>%
  ggplot(aes(x=level,y=estimate,ymin=lower,ymax=upper,
    color=congruence,fill=congruence,shape=congruence)) +
  facet_grid(factor(level,levels=c(
    'accommodative','accommodative + ignore',
    'adversarial','adversarial + accommodative',
    'adversarial + ignore','ignore'))~region,scale='free') +
  geom_pointrange(position=position_dodge2(width=.6),size=.3) +
  scale_color_manual(values=cbbPalette) +
  scale_fill_manual(values=cbbPalette) +
  scale_shape_manual(values=c(24,21,25,21)) +
  coord_flip() + 
  labs(x='',
    y='How much do you like the reaction?\n1 (Do not like at all) - 7 (Like at lot)',
    color="Respondent vis-à-vis\nnew party",
    fill="Respondent vis-à-vis\nnew party",
    shape="Respondent vis-à-vis\nnew party") + 
  theme_bw() +
  theme(legend.position='bottom',
    legend.text = element_text(size=10),
    text=element_text(family="serif"),
    axis.text = element_text(size=10),
    strip.background=element_blank(),
    strip.text.y=element_blank()) +
  guides(color=guide_legend(ncol=2))
dev.off()
ggsave(filename="figure8.png", plot=last_plot(),device="png",width=15,height=12, 
  unit="cm",dpi=600)

#_______________________________________________________________________________
# Supplementary material
#_______________________________________________________________________________
# A Experimental design appendix ----
#_______________________________________________________________________________
# Table A.2: ----
# Number of observations and relative frequency of party reactions 
#_______________________________________________________________________________
t <- data %>% 
  group_by(united) %>%
  summarise(n=n()) %>%
  mutate(N=max(cumsum(n)),prop=n/N)
p <- xtable(t)
names(p) <- c("Attribute level","Number of realizations",
  "Total number of realizations","Relative frequency")
print(xtable(p,type='latex',align="rllll",digits=2),include.rownames=F,
  floating=F,sanitize.text.function= function(x){x},
  file='tableA2.tex')

#_______________________________________________________________________________
# Table A.3: ----
# Number of observations and relative frequency of detailed party reactions 
#_______________________________________________________________________________
t <- data %>% 
  group_by(united_detail) %>%
  summarise(n=n()) %>%
  mutate(N=max(cumsum(n)),prop=n/N,
    united_detail=factor(united_detail,levels=c('ignore','accommodative',
    'accommodative + ignore','adversarial','adversarial + accommodative',
    'adversarial + ignore')))
p <- xtable(t)
names(p) <- c("Attribute level","Number of realizations",
  "Total number of realizations","Relative frequency")
print(xtable(p,type='latex',align="rllll",digits=2),include.rownames=F,
  floating=F,sanitize.text.function= function(x){x},
  file='tableA3.tex')

#_______________________________________________________________________________
# Table A.4 and 5: ----
# Number of observations and relative frequency of detailed party reactions by 
# country
#_______________________________________________________________________________
t <- data %>% 
  group_by(country,united_detail) %>%
  summarise(n=n()) %>%
  mutate(N=max(cumsum(n)),prop=n/N,
    united_detail=factor(united_detail,levels=c('ignore','accommodative',
      'accommodative + ignore','adversarial','adversarial + accommodative',
      'adversarial + ignore'))) 
p <- xtable(t)
names(p) <- c('Country',"Attribute level","Number of realizations",
  "Total number of realizations","Relative frequency")
print(xtable(p,type='latex',align="rrllll",digits=2),include.rownames=F,
  floating=F,sanitize.text.function= function(x){x},
  file='tableA4_5.tex')

#_______________________________________________________________________________
# Table A.6 - A.7: ----
# Number of observations and relative frequency of most preferred party by 
# country. Respondents first ranked party was filled into the vignette.
#_______________________________________________________________________________
t <- data %>% 
  group_by(country,party_chosen) %>%
  summarise(n=n()/4) %>%
  mutate(N=max(cumsum(n)),prop=n/N)
p <- xtable(t)
names(p) <- c('Country',"Most preferred party","n","N","Rel. Freq.")
print(xtable(p,type='latex',align="rrllll",digits=2),include.rownames=F,
  floating=F,sanitize.text.function= function(x){x},
  file='tableA6_7.tex')

#_______________________________________________________________________________
# Figure A.1 ----
# ivisions among voters of specific parties for each policy issue chosen across
# countries.
#_______________________________________________________________________________
pdf('figureA1.pdf',width=12,height=12)
data %>%
  filter(party_family%in%family_selection) %>%
  droplevels() %>%
  mutate(party_family=factor(recode(party_family,
    'Christian democracy'='Christian\ndemocracy',
    'Green/Ecologist'='Green/\nEcologist',
    'Social democracy'='Social\ndemocracy'),
    levels=c('Social\ndemocracy','Green/\nEcologist','Far right',
    'Conservative','Christian\ndemocracy'))) %>%
  group_by(country, party_family, party_chosen) %>%
  reframe(
    policy_climate_pro = sum(policy_climate=="take more measures")/n(),
      policy_climate_con = sum(policy_climate=="stop measures")/n(),
      policy_climate_res = sum(policy_climate=="neither" | policy_climate=="don't know")/n(),
      policy_tech_pro = sum(policy_tech=="pay additional taxes")/n(),
      policy_tech_con = sum(policy_tech=="not pay taxes")/n(),
      policy_tech_res = sum(policy_tech=="neither" | policy_tech=="don't know")/n(),
      policy_freedom_pro = sum(policy_freedom=="never restrict")/n(),
      policy_freedom_con = sum(policy_freedom=="always restrict")/n(),
      policy_freedom_res = sum(policy_freedom=="neither" | policy_freedom=="don't know")/n(),
      policy_immigration_pro = sum(policy_immigration=="allowed to vote")/n(),
      policy_immigration_con = sum(policy_immigration=="not allowed to vote")/n(),
      policy_immigration_res = sum(policy_immigration=="neither" | policy_immigration=="don't know")/n()) %>%
  group_by(country, party_family, party_chosen) %>% # Now, 1 is perfect disunity, 0 is perfect unity
  reframe(
    policy_climate=min(cbind(policy_climate_pro,policy_climate_con))/max(cbind(policy_climate_pro,policy_climate_con)),
      policy_tech=min(cbind(policy_tech_pro,policy_tech_con))/max(cbind(policy_tech_pro,policy_tech_con)),
      policy_immigration=min(cbind(policy_immigration_pro,policy_immigration_con))/max(cbind(policy_immigration_pro,policy_immigration_con)),
      policy_freedom=min(cbind(policy_freedom_pro,policy_freedom_con))/max(cbind(policy_freedom_pro,policy_freedom_con)),
      policy_climate_res=policy_climate_res,
      policy_tech_res=policy_tech_res,
      policy_immigration_res=policy_immigration_res,
      policy_freedom_res=policy_freedom_res) %>%
  pivot_longer(policy_climate:policy_freedom,names_to="policy_area",
    values_to="division") %>%
  group_by(country,party_family,party_chosen) %>%
  reframe(
    policy_res=case_when(
      grepl(pattern="climate",x=policy_area) ~ policy_climate_res,
      grepl(pattern="tech",x=policy_area) ~ policy_tech_res,
      grepl(pattern="freedom",x=policy_area) ~ policy_freedom_res,
      grepl(pattern="immi",x=policy_area) ~ policy_immigration_res),
    policy_area=case_when(
      grepl(pattern="climate",x=policy_area) ~ "Climate",
      grepl(pattern="tech",x=policy_area) ~ "Tech companies",
      grepl(pattern="freedom",x=policy_area) ~ "Health and freedom",
      grepl(pattern="immi",x=policy_area) ~ "Immigrants' voting rights"),
    division=division) %>%
  ggplot() +
  geom_point(aes(x=policy_area, y=division, color=policy_res)) +
  facet_grid(country~party_family)+
  scale_color_gradient(low="black",high="lightgrey") +
  labs(title="",
    y="Voter division", x="", color="Percent of voters\nchoosing no extreme\nstatement") +
  theme_bw() +
  theme(axis.text = element_text(size=10),
    text=element_text(family="serif"),
    axis.text.x=element_text(angle=45, hjust=0.9),
    legend.position='bottom')
dev.off()
ggsave(filename="figureA1.png", plot=last_plot(),device="png",width=15,height=18, 
  unit="cm",dpi=600)

#_______________________________________________________________________________
# Table A.8: ----
# Number of vignettes underlying the regression results for each group studied 
# in Figures 5 and 6
#_______________________________________________________________________________
t <- best_responses %>%
  mutate(
    cat=ifelse(grepl('united',policy_division_group)==T,'United electorate',
      'Divided electorate'),
    groups=factor(recode(groups,
      'Conservatives and Christian democracy vs freedom con'='Cons and Christ dem vs For restrictions of basic rights',
      'Conservatives and Christian democracy vs freedom pro'='Cons and Christ dem vs Against restrictions of basic rights',
      'Conservatives and Christian democracy vs immi vote con'='Cons and Christ dem vs Against voting rights',
      'Conservatives and Christian democracy vs immi vote pro'='Cons and Christ dem vs For voting rights',
      'Social democracy vs freedom con'='Soc dem vs For restrictions of basic rights',
      'Social democracy vs freedom pro'='Soc dem vs Against restrictions of basic rights',
      'Social democracy vs immi vote con'='Soc dem vs Against voting rights',
      'Social democracy vs immi vote pro'='Soc dem vs For voting rights'),
      levels=c('Cons and Christ dem vs For restrictions of basic rights',
        'Cons and Christ dem vs Against restrictions of basic rights',
        'Cons and Christ dem vs Against voting rights',
        'Cons and Christ dem vs For voting rights',
        'Soc dem vs For restrictions of basic rights',
        'Soc dem vs Against restrictions of basic rights',
        'Soc dem vs Against voting rights',
        'Soc dem vs For voting rights'))) %$% 
  table(groups,cat) 
p <- xtable(t)
names(p) <- c('Divided electorate','United electorate')
print(xtable(p,type='latex',align="rll",digits=2),include.rownames=T,
  floating=F,sanitize.text.function= function(x){x},
  file='tableA8.tex')

#_______________________________________________________________________________
# Table A.9: ----
# Summary statistics for the strength of divisions among the different 
# sub-groups of voters studied in Figures 5 and 6. See notes to Figure A.1 for 
# details on the measurement.
#_______________________________________________________________________________
t <- best_responses %>%
  mutate(
    new_groups=factor(recode(paste0(groups,"-",policy_division_group),
      'Conservatives and Christian democracy vs freedom con-divided electorate'=
        'Divided cons and Christ dem vs For restrictions of basic rights',
      "Conservatives and Christian democracy vs freedom con-united electorate\nrejects the new\n party's proposal"=  
        'United cons and Christ dem vs For restrictions of basic rights',
      "Conservatives and Christian democracy vs freedom con-united electorate\nsupports the new\n party's proposal"=  
        'United cons and Christ dem vs For restrictions of basic rights',
      'Conservatives and Christian democracy vs freedom pro-divided electorate'=
        'Divided cons and Christ dem vs Against restrictions of basic rights',
      "Conservatives and Christian democracy vs freedom pro-united electorate\nrejects the new\n party's proposal"=
        'United cons and Christ dem vs Against restrictions of basic rights',
      "Conservatives and Christian democracy vs freedom pro-united electorate\nsupports the new\n party's proposal"=
        'United cons and Christ dem vs Against restrictions of basic rights',
      'Conservatives and Christian democracy vs immi vote con-divided electorate'=
        'Divided cons and Christ dem vs Against voting rights',
      "Conservatives and Christian democracy vs immi vote con-united electorate\nrejects the new\n party's proposal"=
        'United cons and Christ dem vs Against voting rights',
      "Conservatives and Christian democracy vs immi vote con-united electorate\nsupports the new\n party's proposal"=
        'United cons and Christ dem vs Against voting rights',
      'Conservatives and Christian democracy vs immi vote pro-divided electorate'=
        'Divided cons and Christ dem vs For voting rights',
      "Conservatives and Christian democracy vs immi vote pro-united electorate\nrejects the new\n party's proposal"=
        'United cons and Christ dem vs For voting rights',
      "Conservatives and Christian democracy vs immi vote pro-united electorate\nsupports the new\n party's proposal"=
        'United cons and Christ dem vs For voting rights',  
      'Social democracy vs freedom con-divided electorate'=
        'Divided soc dem vs For restrictions of basic rights',
      "Social democracy vs freedom con-united electorate\nsupports the new\n party's proposal"=
        'United soc dem vs For restrictions of basic rights',
      "Social democracy vs freedom con-united electorate\nrejects the new\n party's proposal"=
        'United soc dem vs For restrictions of basic rights',      
      'Social democracy vs freedom pro-divided electorate'=
        'Divided soc dem vs Against restrictions of basic rights',
      "Social democracy vs freedom pro-united electorate\nsupports the new\n party's proposal"=
        'United soc dem vs Against restrictions of basic rights',
      "Social democracy vs freedom pro-united electorate\nrejects the new\n party's proposal"=
        'United soc dem vs Against restrictions of basic rights',
      'Social democracy vs immi vote con-divided electorate'=
        'Divided soc dem vs Against voting rights',
      "Social democracy vs immi vote con-united electorate\nrejects the new\n party's proposal"=
        'United soc dem vs Against voting rights',
      "Social democracy vs immi vote con-united electorate\nsupports the new\n party's proposal"=
        'United soc dem vs Against voting rights',  
      'Social democracy vs immi vote pro-divided electorate'=
        'Divided soc dem vs For voting rights',
      "Social democracy vs immi vote pro-united electorate\nrejects the new\n party's proposal"=
        'United soc dem vs For voting rights',
      "Social democracy vs immi vote pro-united electorate\nsupports the new\n party's proposal"=
        'United soc dem vs For voting rights'),
      levels=c(
       'Divided cons and Christ dem vs For restrictions of basic rights',
       'United cons and Christ dem vs For restrictions of basic rights',
       'Divided cons and Christ dem vs Against restrictions of basic rights',
       'United cons and Christ dem vs Against restrictions of basic rights',
       'Divided cons and Christ dem vs Against voting rights',
       'United cons and Christ dem vs Against voting rights',
       'Divided cons and Christ dem vs For voting rights',
       'United cons and Christ dem vs For voting rights',
       'Divided soc dem vs For restrictions of basic rights',
       'United soc dem vs For restrictions of basic rights',
       'Divided soc dem vs Against restrictions of basic rights',
       'United soc dem vs Against restrictions of basic rights',
       'Divided soc dem vs Against voting rights',
       'United soc dem vs Against voting rights',
       'Divided soc dem vs For voting rights',
       'United soc dem vs For voting rights'))) %>%
  group_by(new_groups) %>%
  summarise(
    min=min(policy_division),
    max=max(policy_division),
    mean=mean(policy_division),
    median=median(policy_division))
p <- xtable(t)
names(p) <- c('Min','Max','Mean','Median')
print(xtable(p,type='latex',align="rrllll",digits=2),include.rownames=F,
  floating=F,sanitize.text.function= function(x){x},
  file='tableA9.tex')

#_______________________________________________________________________________
# B Statistical appendix ----
#_______________________________________________________________________________
# B.1 Summary statistics ----
#_______________________________________________________________________________
# Table B.1 ----
# Sample characteristics by country 
#_______________________________________________________________________________
out <- data %>% 
  distinct(id,age,gender,edu,size_locality,work_status,turnout,lr,country) %>%
  group_by(country)

t <-
  list(
    "Age" = list(
      "mean (sd)" = ~ mean_sd(age[age>=18]),
      "min" = ~ min(age[age>=18]),
      "max" = ~ max(age[age>=18])),
    "Gender" = list(
      "Male" = ~ n_perc(gender==0&!is.na(gender)),
      "Female" = ~ n_perc(gender==1&!is.na(gender)),
      "Neither" = ~ n_perc(gender==2&!is.na(out))),
    "Education" = list(
      "No schooling" = ~ n_perc(edu==0),
      "Primary school" = ~ n_perc(edu==1),
      "High school degree or equivalent" = ~ n_perc(edu>=2&edu<=3),
      "Trade school degree or equivalent" = ~ n_perc(edu>=4&edu<=5),
      "Bachelors degree or equivalent"  = ~ n_perc(edu==6),
      "Post-graduate degree" = ~ n_perc(edu>=7)),
    "Size of locality" = list(
      "Less than 2,000 inhabitants" = ~ n_perc(size_locality==1&!is.na(size_locality)),
      "2,000 to less than 5,000" = ~ n_perc(size_locality==2&!is.na(size_locality)),
      "5,000 to less than 20,000" = ~ n_perc(size_locality==3&!is.na(size_locality)),
      "20,000 to less than 100,000" = ~ n_perc(size_locality==4&!is.na(size_locality)),
      "100,000 to less than 1 million"  = ~ n_perc(size_locality==5&!is.na(size_locality)),
      "1 million or more" = ~ n_perc(size_locality==6&!is.na(size_locality))),
    "Work status" = list(
      "Paid work" = ~ n_perc(work_status=='paid work'&!is.na(work_status)),
      "Education" = ~ n_perc(work_status=='education'&!is.na(work_status)),
      "Unemployed" = ~ n_perc(work_status=='unemployed'&!is.na(work_status)),
      "Care/domestic work" = ~ n_perc(work_status=='care/domestic work'&!is.na(work_status)),
      "Retired"  = ~ n_perc(work_status=='retired'&!is.na(work_status)),
      "Other" = ~ n_perc(work_status=='other'&!is.na(work_status))),
    "Turnout" = list(
      "Always vote" = ~ n_perc(turnout=='always vote'&!is.na(turnout)),
      "Sometimes vote" = ~ n_perc(turnout=='sometimes vote'&!is.na(turnout)),
      "Rarely vote" = ~ n_perc(turnout=='rarely vote'&!is.na(turnout)),
      "Recently eligible" = ~ n_perc(turnout=='recently eligble'&!is.na(turnout)),
      "Not eligible"  = ~ n_perc(turnout=='not eligible'&!is.na(turnout))),
    "Left-right" = list(
      "mean (sd)" = ~ mean_sd(lr[!is.na(lr)]),
      "min" = ~ min(lr,na.rm=T),
      "max" = ~ max(lr,na.rm=T)))

p <- summary_table(group_by(out,country),t)
kable(p,format="latex",booktabs=T) %>%
  cat(file= "tableB1.tex")

#_______________________________________________________________________________
# Figure B.1  ----
# Distribution of the outcome variable by country
#_______________________________________________________________________________
pdf('figureB1.pdf', height=4) 
data %>% 
  group_by(country,outcome) %>%
  summarise(n=n()) %>%
  mutate(N=max(cumsum(n)),prop=n/N) %>%
  ggplot(aes(x=outcome,y=prop)) +
  facet_wrap(~country,ncol=5) +  
  geom_col() + 
  labs(
    y='Relative frequency',
    x='How much do you like the reaction?\n1 (Do not like at all) − 7 (Like at lot)') +
  scale_x_continuous(breaks=seq(1,7)) +
  theme_bw() +
  theme(strip.text.y=element_blank(),strip.background.y=element_blank())
dev.off()  

#_______________________________________________________________________________
# B.2 Main analysis and robustness of findings ----
#_______________________________________________________________________________
# B.2.1 Marginal means for all attribute levels ----
#_______________________________________________________________________________
# Figure B.2 ----
#  Marginal means for how citizens want their most preferred party to respond to
# new parties for all attributes.
#_______________________________________________________________________________
mms <- data %>% 
  droplevels() %>%
  cj(.,outcome~populism+policy_topic+united_detail,id=~id,estimate="mm")

pdf('figureB2.pdf',height=4) 
mms %>% 
  mutate(
    level=factor(recode(level,'Immi voting'="Immigrants' voting rights"),
      levels=c('anti-government','populist','Climate','Health and freedom',
        "Immigrants' voting rights",'Tech companies','ignore',
        'accommodative','accommodative + ignore','adversarial',
        'adversarial + accommodative','adversarial + ignore'))) %>%
  ggplot(aes(x=level,y=estimate,ymin=lower,ymax=upper)) +
  geom_pointrange(position=position_dodge2(width=.6),size=.3) +
  coord_flip() + 
  labs(x='',
    y='How much do you like the reaction?\n1 (Do not like at all) - 7 (Like at lot)') + 
  theme_bw() +
  theme(legend.position='bottom',
    legend.title=element_blank(),
    text=element_text(family="serif"),
    axis.text = element_text(size=11),
    strip.background=element_blank())
dev.off()

#_______________________________________________________________________________
# B.2.2 Regression analysis accompanying figures in the main text ----
#_______________________________________________________________________________
# Table B.2: ----
# OLS regression of the support for party reaction on indicator for united, 
# divided, ignore (reference category ignore, see Figure 3).
#_______________________________________________________________________________
m <- data %>% 
  lm_robust(outcome~relevel(united,ref='ignore'),data=.,clusters=id)
texreg(m,digits=3,single.row=F,include.ci=F,stars=c(0.01, 0.05, 0.1),
  reorder.coef=c(2,3,1),
  custom.coef.names=c('constant','divided','united'),
  table=F,file='tableB2.tex')

#_______________________________________________________________________________
# Table B.3: ----
# OLS regression of support for the party reaction on indicator for united, 
# divided, ignore (reference category ignore) by country (see Figure 7).
#_______________________________________________________________________________
m <- data %>% 
  group_by(country) %>%
  do(m=lm_robust(outcome~united, data=., clusters=id))

texreg(list(m$m[[1]],m$m[[2]],m$m[[3]],m$m[[4]],m$m[[5]],m$m[[6]],m$m[[7]]),
  digits=3,single.row=F,include.ci=F,stars=c(0.01, 0.05, 0.1),
  reorder.coef=c(2,3,1),
  custom.coef.names=c('constant','divided','united'),
  custom.model.names=c('AT','DE','DK','ES','FR','GR','HU'),
  table=F,file='tableB3_1.tex')
texreg(list(m$m[[8]],m$m[[9]],m$m[[10]],m$m[[11]],m$m[[12]],m$m[[13]],m$m[[14]]),
  digits=3,single.row=F,include.ci=F,stars=c(0.01, 0.05, 0.1),
  reorder.coef=c(2,3,1),
  custom.coef.names=c('constant','divided','united'),
  custom.model.names=c('IE','IT','NL','PL','PT','RO','SE'),
  table=F,file='tableB3_2.tex')

#_______________________________________________________________________________
# Table B.4: ----
# OLS regression of support for the party reaction on an indicator of an 
# accommodative, accommodative + ignore, adversarial, adversarial + 
# accommodative, and adversarial + ignore reaction (reference category ignore) 
# by congruence of respondents with the policy statement of the new party (see 
# Figure 4).
#_______________________________________________________________________________
m <- data %>% 
  group_by(congruence) %>%
  do(m=lm_robust(outcome~relevel(united_detail,ref='ignore'),data=.,clusters=id))
texreg(list(m$m[[1]],m$m[[2]],m$m[[3]],m$m[[4]]),
  digits=3,single.row=F,include.ci=F,stars=c(0.01, 0.05, 0.1),
  reorder.coef=c(2,3,4,5,6,1),
  custom.coef.names=c('constant','accommodative','accommodative + ignore',
    'adversarial','adversarial + accommodative','adversarial + ignore'),
  reorder.model=c(4,2,3,1),
  custom.model.names=c('same extreme position','neither same nor opposite',
    'opposite extreme position',"doesn't know"),
  table=F,file='tableB4.tex')   

#_______________________________________________________________________________
# Table B.5: ----
# OLS regression of support for the party reaction of Christian democratic and 
# conservative parties on an indicator of an accommodative, accommodative + 
# ignore, adversarial, adversarial + accommodative, and adversarial + ignore 
# reaction (reference category ignore) when faced with new parties proposing 
# measures concerning the voting rights of immigrants or basic rights in times 
# of pandemics averaging across new parties’ attacks (see Figure 5). An
# accommodative response is the reference category.
#_______________________________________________________________________________
m <- best_responses  %>%
  group_by(groups,policy_division_group) %>%
  do(m=lm_robust(outcome~united_detail, data=., clusters=id)) %>%
  mutate(
    party_family=ifelse(grepl(pattern="Cons",x=groups),
      "Conservative and Christian democracy", "Social democracy"),
    policy=factor(case_when(
      grepl(pattern="freedom pro", x=groups) ~ "against restrictions",
      grepl(pattern="freedom con", x=groups) ~ "for restrictions",
      grepl(pattern="vote con", x=groups) ~ "against voting rights",
      grepl(pattern="vote pro", x=groups) ~ "for voting rights"),
      levels=c("for restrictions", "against restrictions",
        "for voting rights","against voting rights")))

texreg(list(m$m[[3]],m$m[[6]],m$m[[12]],m$m[[9]]),
  digits=3,single.row=F,include.ci=F,stars=c(0.01, 0.05, 0.1),
  reorder.coef=c(2,3,4,5,6,1),
  custom.coef.names=c('constant','accommodative','accommodative + ignore',
    'adversarial','adversarial + accommodative','adversarial + ignore'),
  reorder.model=c(4,2,3,1),
  custom.model.names=c('for restrictions','against restrictions',
    'for voting rights','against voting rights'),
  table=F,file='tableB5_1.tex')  

texreg(list(m$m[[2]],m$m[[5]],m$m[[11]],m$m[[8]]),
  digits=3,single.row=F,include.ci=F,stars=c(0.01, 0.05, 0.1),
  reorder.coef=c(2,3,4,5,6,1),
  custom.coef.names=c('constant','accommodative','accommodative + ignore',
    'adversarial','adversarial + accommodative','adversarial + ignore'),
  reorder.model=c(4,2,3,1),
  custom.model.names=c('for restrictions','against restrictions',
    'for voting rights','against voting rights'),
  table=F,file='tableB5_2.tex')  

texreg(list(m$m[[1]],m$m[[4]],m$m[[10]],m$m[[7]]),
  digits=3,single.row=F,include.ci=F,stars=c(0.01, 0.05, 0.1),
  reorder.coef=c(2,3,4,5,6,1),
  custom.coef.names=c('constant','accommodative','accommodative + ignore',
    'adversarial','adversarial + accommodative','adversarial + ignore'),
  reorder.model=c(4,2,3,1),
  custom.model.names=c('for restrictions','against restrictions',
    'for voting rights','against voting rights'),
  table=F,file='tableB5_3.tex')  

#_______________________________________________________________________________
# Table B.6: ----
# OLS regression of support for the party reaction of Social democratic parties 
# on an indicator of an accommodative, accommodative + ignore, adversarial, 
# adversarial + accommodative, and adversarial + ignore reaction (reference 
# category ignore) when faced with new parties proposing measures concerning the 
# voting rights of immigrants or basic rights in times of pandemics averaging 
# across new parties' attacks  (see Figure 6). An accommodative response is the 
# reference category.
#_______________________________________________________________________________
texreg(list(m$m[[15]],m$m[[18]],m$m[[24]],m$m[[21]]),
  digits=3,single.row=F,include.ci=F,stars=c(0.01, 0.05, 0.1),
  reorder.coef=c(2,3,4,5,6,1),
  custom.coef.names=c('constant','accommodative','accommodative + ignore',
    'adversarial','adversarial + accommodative','adversarial + ignore'),
  reorder.model=c(4,2,3,1),
  custom.model.names=c('for restrictions','against restrictions',
    'for voting rights','against voting rights'),
  table=F,file='tableB6_1.tex')  

texreg(list(m$m[[14]],m$m[[17]],m$m[[23]],m$m[[20]]),
  digits=3,single.row=F,include.ci=F,stars=c(0.01, 0.05, 0.1),
  reorder.coef=c(2,3,4,5,6,1),
  custom.coef.names=c('constant','accommodative','accommodative + ignore',
    'adversarial','adversarial + accommodative','adversarial + ignore'),
  reorder.model=c(4,2,3,1),
  custom.model.names=c('for restrictions','against restrictions',
    'for voting rights','against voting rights'),
  table=F,file='tableB6_2.tex')  

texreg(list(m$m[[13]],m$m[[16]],m$m[[22]],m$m[[19]]),
  digits=3,single.row=F,include.ci=F,stars=c(0.01, 0.05, 0.1),
  reorder.coef=c(2,3,4,5,6,1),
  custom.coef.names=c('constant','accommodative','accommodative + ignore',
    'adversarial','adversarial + accommodative','adversarial + ignore'),
  reorder.model=c(4,2,3,1),
  custom.model.names=c('for restrictions','against restrictions',
    'for voting rights','against voting rights'),
  table=F,file='tableB6_3.tex')  

#______________________________________________________________________________#
# Figures B.3 and B.4: ====
# Support for the party reaction of Christian democratic and conservative parties 
# when faced with new parties proposing measures concerning the voting rights of 
# immigrants or basic rights in times of pandemics averaging across new parties' 
# attacks.
#______________________________________________________________________________#
r <- best_responses %>%
  group_by(groups,policy_division_group) %>%
  do(lm_robust(outcome~0+united_detail+country, data=., clusters=id) %>% tidy()) %>%
  mutate(
    party_family=ifelse(grepl(pattern="Cons",x=groups),
      "Conservative and Christian democracy", "Social democracy"),
    policy=factor(case_when(
      grepl(pattern="freedom pro", x=groups) ~ "against restrictions",
      grepl(pattern="freedom con", x=groups) ~ "for restrictions",
      grepl(pattern="vote con", x=groups) ~ "against voting rights",
      grepl(pattern="vote pro", x=groups) ~ "for voting rights"),
      levels=c("for restrictions", "against restrictions",
        "for voting rights","against voting rights")),
    policy_division_group=factor(recode(policy_division_group,
      "united electorate\nsupports the new\n party's proposal"=
      'united electorate\nin favour',
      "united electorate\nrejects the new\n party's proposal"=
      'united electorate\nagainst'),
      levels=c('divided electorate','united electorate\nagainst',
        'united electorate\nin favour')))

# Panel 1: Conservatives and Christian democracy
pdf('figureB3.pdf',height=4)
r %>%
  mutate(strategy=factor(str_sub(term,14,-1),
    levels=c('ignore','adversarial + ignore','adversarial + accommodative',
      'adversarial', 'accommodative + ignore','accommodative'))) %>%
  filter(!is.na(strategy)&party_family=="Conservative and Christian democracy") %>%
  ggplot(aes(x=strategy,y=estimate,ymin=conf.low,ymax=conf.high)) +
  facet_grid(policy_division_group~policy,switch='y') +
  geom_pointrange(position=position_dodge2(width=.6),size=.1) +
  coord_flip() + 
  scale_x_discrete(position="top") +
  labs(x='',
    y='How much do you like the reaction?\n1 (Do not like at all) - 7 (Like at lot)', 
    subtitle='New party is ...') + 
  theme_bw() +
  theme(
    text=element_text(family="serif"),axis.text = element_text(size=10),
    strip.background=element_blank(),strip.text.y=element_text(angle=0),
    plot.title=element_text(size=10,angle=90,hjust=-10),
    plot.subtitle=element_text(size=10),legend.position='bottom')
dev.off()

# Panel 2: Social democracy
pdf('figureB4.pdf',height=4)
r %>%
  mutate(strategy=factor(str_sub(term,14,-1),
    levels=c('ignore','adversarial + ignore','adversarial + accommodative',
      'adversarial', 'accommodative + ignore','accommodative'))) %>%
  filter(!is.na(strategy)&party_family=="Social democracy") %>%
  ggplot(aes(x=strategy,y=estimate,ymin=conf.low,ymax=conf.high)) +
  facet_grid(policy_division_group~policy,switch='y') +
  geom_pointrange(position=position_dodge2(width=.6),size=.1) +
  coord_flip() + 
  scale_x_discrete(position="top") +
  labs(x='',
    y='How much do you like the reaction?\n1 (Do not like at all) - 7 (Like at lot)', 
    subtitle='New party is ...') + 
  theme_bw() +
  theme(
    text=element_text(family="serif"),axis.text = element_text(size=10),
    axis.title.y = element_text(angle = 0, hjust = 0.5, vjust = -1),
    strip.background=element_blank(),strip.text.y=element_text(angle=0),
    plot.title=element_text(size=10,angle=90,hjust=-10),
    plot.subtitle=element_text(size=10),legend.position='bottom')
dev.off()

#______________________________________________________________________________#
# Figures B.5 and B.6: ====
# Support for the party reaction of Christian democratic and conservative parties 
# when faced with new parties proposing measures concerning the voting rights of 
# immigrants or basic rights in times of pandemics averaging across new parties' 
# attacks weighted inversely by share of undecided.
#______________________________________________________________________________#
r <- rbind(
  best_responses %>%
    mutate(weights=(1-policy_res)) %>% 
    group_by(groups,policy_division_group) %>%
    do(lm_robust(outcome~0+united_detail,data=.,clusters=id,weights=weights) %>% 
      tidy()) %>%
    mutate(weighted='Weighted'),
  best_responses %>%
    group_by(groups,policy_division_group) %>%
    do(lm_robust(outcome~0+united_detail,data=.,clusters=id) %>% tidy()) %>%
    mutate(weighted='Unweighted')) %>%
  mutate(
    party_family=ifelse(grepl(pattern="Cons",x=groups),
      "Conservative and Christian democracy", "Social democracy"),
    policy=factor(case_when(
      grepl(pattern="freedom pro", x=groups) ~ "against restrictions",
      grepl(pattern="freedom con", x=groups) ~ "for restrictions",
      grepl(pattern="vote con", x=groups) ~ "against voting rights",
      grepl(pattern="vote pro", x=groups) ~ "for voting rights"),
      levels=c("for restrictions", "against restrictions",
        "for voting rights","against voting rights")),
    strategy=factor(str_sub(term,14,-1),
      levels=c('ignore','adversarial + ignore','adversarial + accommodative',
        'adversarial', 'accommodative + ignore','accommodative')),
    policy_division_group=factor(recode(policy_division_group,
      "united electorate\nsupports the new\n party's proposal"=
        'united electorate\nin favour',
      "united electorate\nrejects the new\n party's proposal"=
        'united electorate\nagainst'),
      levels=c('divided electorate','united electorate\nagainst',
        'united electorate\nin favour')))

# Panel 1: Conservatives and Christian democracy
pdf('figureB5.pdf',height=4)
r %>%
  filter(!is.na(strategy)&party_family=="Conservative and Christian democracy") %>%
  ggplot(aes(x=strategy,y=estimate,ymin=conf.low,ymax=conf.high,
             color=weighted,fill=weighted)) +
  facet_grid(policy_division_group~policy,switch='y') +
  geom_pointrange(position=position_dodge2(width=.6),size=.1) +
  coord_flip() + 
  scale_x_discrete(position="top") +
  scale_color_manual(values=c('gray','black')) +
  scale_fill_manual(values=c('gray','black')) +
  labs(x='',
       y='How much do you like the reaction?\n1 (Do not like at all) - 7 (Like at lot)', 
       subtitle='New party is ...') + 
  theme_bw() +
  theme(
    text=element_text(family="serif"),axis.text = element_text(size=10),
    strip.background=element_blank(),strip.text.y=element_text(angle=0),
    plot.title=element_text(size=10,angle=90,hjust=-10),
    plot.subtitle=element_text(size=10),legend.position='bottom',
    legend.title=element_blank()) 
dev.off()

# Panel 2: Social democracy
pdf('figureB6.pdf',height=4)
r %>%
  filter(!is.na(strategy)&party_family=="Social democracy") %>%
  ggplot(aes(x=strategy,y=estimate,ymin=conf.low,ymax=conf.high,
    color=weighted,fill=weighted)) +
  facet_grid(policy_division_group~policy,switch='y') +
  geom_pointrange(position=position_dodge2(width=.6),size=.3) +
  coord_flip() + 
  scale_x_discrete(position="top") +
  scale_color_manual(values=c('gray','black')) +
  scale_fill_manual(values=c('gray','black')) +
  labs(x='',
    y='How much do you like the reaction?\n1 (Do not like at all) - 7 (Like at lot)', 
    subtitle='New party is ...') + 
  theme_bw() +
  theme(
    text=element_text(family="serif"),axis.text = element_text(size=10),
    strip.background=element_blank(),strip.text.y=element_text(angle=0),
    plot.title=element_text(size=10,angle=90,hjust=-10),
    plot.subtitle=element_text(size=10),legend.position='bottom',
    legend.title=element_blank())
dev.off()

#______________________________________________________________________________#
# Figures B.7 and B.8: ====
# Difference in support for the party reaction of Christian democratic and 
# conservative parties when faced with new parties proposing measures concerning 
# the voting rights of immigrants or basic rights in times of pandemics averaging 
# across new parties' attacks between divided vs united electorates. 
# Results are taken from a regression of the outcome measure support for the 
# party reaction on an indicator of divided vs united for the new party's proposal 
# and vs united against the proposal.
#______________________________________________________________________________#
r <- best_responses  %>%
  group_by(groups,united_detail) %>%
  do(lm_robust(outcome~relevel(policy_division_group,ref='divided electorate'),
    data=.,clusters=id) %>% tidy()) %>%
  filter(term!='(Intercept)') %>%
  mutate(
    party_family=ifelse(grepl(pattern="Cons",x=groups),
      "Conservative and Christian democracy", "Social democracy"),
    policy=factor(case_when(
      grepl(pattern="freedom pro", x=groups) ~ "against restrictions",
      grepl(pattern="freedom con", x=groups) ~ "for restrictions",
      grepl(pattern="vote con", x=groups) ~ "against voting rights",
      grepl(pattern="vote pro", x=groups) ~ "for voting rights"),
      levels=c("for restrictions", "against restrictions",
        "for voting rights","against voting rights")),
    term=factor(recode(term,
      "relevel(policy_division_group, ref = \"divided electorate\")united electorate\nsupports the new\n party's proposal"=
        'divided vs\nunited\nelectorate\nin favour',
      "relevel(policy_division_group, ref = \"divided electorate\")united electorate\nrejects the new\n party's proposal"=
        'divided vs\nunited\nelectorate\nagainst')),
    united_detail=factor(united_detail,levels=c('ignore','accommodative',
      'accommodative + ignore','adversarial','adversarial + accommodative',
      'adversarial + ignore')))

# Panel 1: Conservatives and Christian democracy
pdf('figureB7.pdf',height=4)
r %>%
  filter(party_family=="Conservative and Christian democracy") %>%
  ggplot(aes(x=term,y=estimate,ymin=conf.low,ymax=conf.high,color=united_detail,
    fill=united_detail)) +
  facet_grid(~policy,switch='y') +
  geom_hline(aes(yintercept=0),color='red') + 
  geom_pointrange(position=position_dodge2(width=.6),size=.1) +
  coord_flip() + 
  scale_x_discrete(position="top") +
  scale_color_manual(values=c('black','blue','steelblue3','steelblue1','darkgray','gray')) +
  scale_fill_manual(values=c('black','blue','steelblue3','steelblue1','darkgray','gray')) +
  labs(x='',
       y='How much do you like the reaction?\n1 (Do not like at all) - 7 (Like at lot)', 
       subtitle='New party is ...') + 
  theme_bw() +
  theme(
    text=element_text(family="serif"),axis.text = element_text(size=10),
    strip.background=element_blank(),strip.text.y=element_text(angle=0),
    plot.title=element_text(size=10,angle=90,hjust=-10),
    plot.subtitle=element_text(size=10),legend.position='bottom',
    legend.title=element_blank())
dev.off()

# Panel 2: Social democracy
pdf('figureB8.pdf',height=4)
r %>%
  filter(party_family=="Social democracy"&term!='(Intercept)') %>%
  ggplot(aes(x=term,y=estimate,ymin=conf.low,ymax=conf.high,color=united_detail,
    fill=united_detail)) +
  facet_grid(~policy,switch='y') +
  geom_hline(aes(yintercept=0),color='red') + 
  geom_pointrange(position=position_dodge2(width=.6),size=.1) +
  coord_flip() + 
  scale_x_discrete(position="top") +
  scale_color_manual(values=c('black','blue','steelblue3','steelblue1','darkgray','gray')) +
  scale_fill_manual(values=c('black','blue','steelblue3','steelblue1','darkgray','gray')) +
  labs(x='',
    y='How much do you like the reaction?\n1 (Do not like at all) - 7 (Like at lot)', 
    subtitle='New party is ...') + 
  theme_bw() +
  theme(
    text=element_text(family="serif"),axis.text = element_text(size=10),
    strip.background=element_blank(),strip.text.y=element_text(angle=0),
    plot.title=element_text(size=10,angle=90,hjust=-10),
    plot.subtitle=element_text(size=10),legend.position='bottom',
    legend.title=element_blank())
dev.off()

#_______________________________________________________________________________
# B.2.3 Robustness to re-coding ignore as united ----
#_______________________________________________________________________________
# Figure B.9: ----
# Marginal means for how citizens want their most preferred party to respond to
# challengers: divided vs united with ignore.
#_______________________________________________________________________________
mms <- data %>% 
  mutate(united=recode(united,'ignore'='united')) %>% 
  droplevels() %>%
  cj(.,outcome~united,id=~id,estimate="mm")

pdf('figureB9.pdf',height=1.5) 
mms %>% 
  filter(feature=="united") %>%
  ggplot(aes(x=reorder(level,desc(level)),y=estimate,ymin=lower,ymax=upper)) +
  geom_pointrange(position=position_dodge2(width=.6),size=.3) +
  coord_flip() + 
  labs(x='',
    y='How much do you like the reaction?\n1 (Do not like at all) - 7 (Like at lot)') + 
  theme_bw() +
  theme(legend.position='bottom',legend.title=element_blank(),
    text=element_text(family="serif"),axis.text = element_text(size=11),
    strip.background=element_blank())
dev.off()

#_______________________________________________________________________________
# B.2.4 Additional figures for the main analysis ----
#_______________________________________________________________________________
# Figure B.10: ----
# Marginal means for how citizens want their most preferred party to respond to
# new parties by the three levels of treatment manipulation (whether the 
# response is united, divided or ignore, whether the new party is populist or 
# anti-government, and by policy issue) as well as whether the proposal of the new 
# party is congruent with the citizens policy position and party family. We omit 
# when respondents did not state a policy position.
#_______________________________________________________________________________
r <- data %>% 
  filter(united_detail!=""&!is.na(outcome)&!is.na(populism)) %>%
  droplevels() %>%
  group_by(united,populism,policy_topic,party_family,congruence) %>%
  do(lm_robust(outcome~1,clusters=id,data=.) %>% tidy())

pdf('figureB10.pdf',height=13)
r %>% 
  filter(term=='(Intercept)'&congruence!='neither same nor opposite'&
    congruence!="doesn't know") %>%
  mutate(
    congruence=factor(congruence,levels=c('same extreme position',
      'opposite extreme position')),
    united=factor(united,levels=c('ignore','divided','united'))) %>%
  ggplot(aes(x=united,y=estimate,ymin=conf.low,ymax=conf.high,
    color=congruence,fill=congruence,shape=populism)) +
  facet_grid(party_family~policy_topic) +
  geom_pointrange(position=position_dodge2(width=.6),size=.3) +
  scale_color_manual(values=c('black','gray')) +
  scale_fill_manual(values=c('black','gray')) +
  coord_flip() + 
  labs(x='',
    y='How much do you like the reaction?\n1 (Do not like at all) - 7 (Like at lot)') + 
  theme_bw() +
  theme(legend.position='bottom',legend.title=element_blank(),
    text=element_text(family="serif"),axis.text = element_text(size=10),
    strip.background=element_blank())
dev.off()

#_______________________________________________________________________________
# Figure B.11: ----
# Marginal means for how citizens want their most preferred party to respond to
# new parties by the three levels of treatment manipulation (whether the 
# response is accommodative, adversarial or ignores or a combination thereof, 
# whether the new party is populist or anti-government, and by policy issue) as 
# well as whether the proposal of the new party is congruent with the citizens 
# policy position and party family. We omit when respondents did not state a 
# policy position.
#_______________________________________________________________________________
r <- data %>% 
  filter(united_detail!=""&!is.na(outcome)&!is.na(populism)) %>%
  droplevels() %>%
  group_by(united_detail,populism,policy_topic,party_family,congruence) %>%
  do(lm_robust(outcome~1,clusters=id,data=.) %>% tidy())

pdf('figureB11.pdf',height=10)
r %>% 
  filter(term=='(Intercept)'&congruence!='neither same nor opposite'&
    congruence!="doesn't know"&party_family!='Communist/Socialist'&
    party_family!='Special issue'&party_family!='Liberal') %>%
  mutate(
    united_detail=factor(united_detail,levels=c('ignore',
      'adversarial + ignore','adversarial + accommodative',
      'adversarial','accommodative + ignore','accommodative')),
    congruence=factor(congruence,levels=c('same extreme position',
      'opposite extreme position'))) %>%
  ggplot(aes(x=united_detail,y=estimate,ymin=conf.low,ymax=conf.high,
    color=congruence,fill=congruence,shape=populism)) +
  facet_grid(party_family~policy_topic) +
  geom_pointrange(position=position_dodge2(width=.6),size=.3) +
  scale_color_manual(values=c('black','gray')) +
  scale_fill_manual(values=c('black','gray')) +
  coord_flip() + 
  labs(x='',
    y='How much do you like the reaction?\n1 (Do not like at all) - 7 (Like at lot)') + 
  theme_bw() +
  theme(legend.position='bottom',legend.title=element_blank(),
    text=element_text(family="serif"),axis.text = element_text(size=10),
    strip.background=element_blank())
dev.off()

#_______________________________________________________________________________
# Figure B.12: ----
# Marginal means for how citizens want their most preferred (Christian 
# democratic and conservative) party to respond to new parties when faced with 
# new parties proposing measures concerning the voting rights of immigrants, 
# climate change, or basic rights in times of pandemics averaging across new 
# parties' attacks.
#_______________________________________________________________________________
out <- data %>%
  filter(
    (party_family=="Christian democracy" & policy=="immi vote con") |
      (party_family=="Christian democracy" & policy=="climate con") |
      (party_family=="Conservative" & policy=="immi vote con") |
      (party_family=="Conservative" & policy=="climate con") |
      (party_family=="Christian democracy" & policy=="freedom pro") |
      (party_family=="Conservative" & policy=="freedom pro")) %>%
  droplevels() %>%
  # merge Conservatives and Christ dem parties
  mutate(party_family=recode(party_family, 
    'Conservative'="Conservatives and Christian democracy",
    'Christian democracy'="Conservatives and Christian democracy")) %>%
  group_by(country, party_chosen) %>% 
  # Calculate party measures of division and residual categories chosen
  mutate(
    policy_freedom_pro = sum(policy_freedom=="never restrict")/n(),
    policy_freedom_con = sum(policy_freedom=="always restrict")/n(),
    policy_freedom_res = sum(policy_freedom=="neither" | 
      policy_freedom=="don't know")/n(),
    policy_immigration_pro = sum(policy_immigration=="allowed to vote")/n(),
    policy_immigration_con = sum(policy_immigration=="not allowed to vote")/n(),
    policy_immigration_res = sum(policy_immigration=="neither" | 
      policy_immigration=="don't know")/n(),
    policy_climate_pro = sum(policy_climate=="take more measures")/n(),
    policy_climate_con = sum(policy_climate=="stop measures")/n(),
    policy_climate_res = sum(policy_climate=="neither" | 
      policy_climate=="don't know")/n()) %>%
  group_by(country, party_chosen) %>% 
  # Recode by policy issue and party: 1 is perfect disunity, 0 is perfect unity
  mutate(
    policy_immigration=min(cbind(policy_immigration_pro,policy_immigration_con))/
      max(cbind(policy_immigration_pro,policy_immigration_con)),
    policy_freedom=min(cbind(policy_freedom_pro,policy_freedom_con))/
      max(cbind(policy_freedom_pro,policy_freedom_con)),
    policy_climate=min(cbind(policy_climate_pro,policy_climate_con))/
      max(cbind(policy_climate_pro,policy_climate_con)),
    policy_immigration_res=policy_immigration_res,
    policy_freedom_res=policy_freedom_res,
    policy_climate_res=policy_climate_res) %>%
  ungroup() %>%
  # choose corresponding measurements for each vignette seen
  mutate(
    policy_res=case_when(
      grepl(pattern="freedom",x=policy) ~ policy_freedom_res,
      grepl(pattern="immi",x=policy) ~ policy_immigration_res,
      grepl(pattern="climate",x=policy) ~ policy_climate_res),
    policy_division=case_when(
      grepl(pattern="freedom",x=policy) ~ policy_freedom,
      grepl(pattern="immi",x=policy) ~ policy_immigration,
      grepl(pattern="climate",x=policy) ~ policy_climate),
    groups=paste0(party_family, " vs ", policy)) %>%
  select(country,party_chosen,groups,id,united_detail,outcome,
    policy,policy_topic,policy_division,policy_res,populism) %>%
  # set division
  mutate(policy_division_group=ifelse(policy_division>0.5,"divided electorate",
    "united electorate")) %>%
  droplevels()

r <- out %>% 
  filter(united_detail!="") %>%
  droplevels() %>%
  group_by(policy,populism,policy_division_group) %>%
  do(lm_robust(outcome~united_detail-1,clusters=id,data=.) %>% tidy())

pdf('figureB12.pdf',height=5)
r %>%
  mutate(
    strategy=factor(str_sub(term,14,-1),
      levels=c('ignore','adversarial + ignore','adversarial + accommodative',
        'adversarial', 'accommodative + ignore','accommodative')),
    policy=factor(recode(policy,
      "freedom pro"="against restrictions",
      "immi vote con"="against voting rights",
      "climate con"="against measures"),
      levels=c("against restrictions","against voting rights",
      'against measures'))) %>%
  ggplot(aes(x=strategy,y=estimate,ymin=conf.low,ymax=conf.high,
    color=policy_division_group,fill=policy_division_group)) +
  facet_grid(populism~policy,switch='y') +
  geom_pointrange(position=position_dodge2(width=.6),size=.3) +
  coord_flip() + 
  scale_x_discrete(position="top") +
  scale_color_manual(values=c('gray','black')) +
  scale_fill_manual(values=c('gray','black')) +
  labs(x='',y='How much do you like the reaction?\n1 (Do not like at all) - 7 (Like at lot)', 
       subtitle='New party is ...') + 
  guides(color=guide_legend(title='Conservative and Christian\ndemocracy with a ...'),
         fill=guide_legend(title='Conservative and Christian\ndemocracy with a ...')) +
  theme_bw() +
  theme(text=element_text(family="serif"),axis.text = element_text(size=10),
        strip.background=element_blank(),strip.text.y=element_text(angle=0),
        plot.title=element_text(size=10,angle=90,hjust=-10),
        plot.subtitle=element_text(size=10),legend.position='bottom')
dev.off()

#_______________________________________________________________________________
# B.3 Subgroup analysis ----
#_______________________________________________________________________________
# B.3.1 Responses by region ----
#______________________________________________________________________________#
# Figure B.13: ----
# Marginal means for how citizens want their most preferred party to respond to 
# new parties for Western/Northern, Southern (ES, GR, IT, PT), and Central/
# Eastern Europe (HU, PL, RO) separately.
#______________________________________________________________________________#
mms <- data %>%
  mutate(region=factor(ifelse(country=="AT"|country=="DE"|country=="DK"|
      country=="FR"|country=="IE"|country=="NL"|country=="SE",'Western/Northern Europe',
    ifelse(country=="ES"|country=="GR"|country=="IT"|country=="PT",'Southern Europe',
      "Central/Eastern Europe")))) %>%
  droplevels() %>%
  cj(.,outcome~united,id=~id,by=~region,estimate="mm")

pdf('figureB13.pdf',height=2) 
mms %>% 
  filter(feature=="united") %>%
  mutate(level=factor(level,levels=c('ignore','divided','united'))) %>%
  ggplot(aes(x=level,y=estimate,ymin=lower,ymax=upper,
    color=region,fill=region,shape=region)) +
  geom_pointrange(position=position_dodge2(width=.6),size=.3) +
  coord_flip() + 
  geom_pointrange(position=position_dodge2(width=.6),size=.3) +
  scale_color_manual(values=cbbPalette) +
  scale_fill_manual(values=cbbPalette) +
  scale_shape_manual(values=c(24,21,25)) +
  labs(x='',
    y='How much do you like the reaction?\n1 (Do not like at all) - 7 (Like at lot)') + 
  theme_bw() +
  theme(legend.position='bottom',
    legend.title=element_blank(),
    text=element_text(family="serif"),
    axis.text = element_text(size=11),
    strip.background=element_blank())
dev.off()

#_______________________________________________________________________________
# B.3.2 Responses by party family ----
#_______________________________________________________________________________
# Figure B.14: ----
# Marginal means for how citizens want their most preferred party to respond to
# challengers by party family of the most preferred party.
#_______________________________________________________________________________
mms <- data %>% 
  droplevels() %>%
  cj(.,outcome~united,id=~id,by=~party_family,estimate="mm")

pdf('figureB14.pdf',height=2.5) 
mms %>% 
  mutate(level=factor(level,levels=c('ignore','divided','united'))) %>%
  ggplot(aes(x=level,y=estimate,ymin=lower,ymax=upper)) +
  geom_pointrange(position=position_dodge2(width=.6),size=.2) +
  facet_wrap(~party_family,ncol=4) +
  scale_color_manual(values=cbbPalette) +
  coord_flip() + 
  labs(x='',y='How much do you like the reaction?\n1 (Do not like at all) - 7 (Like at lot)') + 
  theme_bw() +
  theme(legend.position='bottom',legend.title=element_blank(),
    axis.text.y=element_text(size=8),strip.background=element_blank())
dev.off()

#_______________________________________________________________________________
# Figure B.15: ----
# Marginal means for how citizens want their most preferred party 
# to respond to new parties depending on whether they like the proposal of the 
# new party by party family of the most preferred party.
#_______________________________________________________________________________
mms <- data %>% 
  filter(united_detail!="") %>%
  droplevels() %>%
  cj(.,outcome~ united_detail,
     id=~id,
     by=~congruence+party_family,
     estimate="mm") 

pdf('figureB15.pdf',height=4)
mms %>% 
  filter(feature=="united_detail") %>%
  mutate(
    level=factor(level,levels=c('ignore','adversarial + ignore',
      'adversarial + accommodative','adversarial', 'accommodative + ignore',
      'accommodative')),
    congruence=factor(congruence,levels=c('same extreme position',
      'neither same nor opposite','opposite extreme position',"doesn't know"))) %>%
  ggplot(aes(x=level,y=estimate,ymin=lower,ymax=upper,
    color=congruence,fill=congruence,shape=congruence)) +
  facet_wrap(~party_family,ncol=4) +
  geom_pointrange(position=position_dodge2(width=.6),size=.3) +
  scale_color_manual(values=cbbPalette) +
  scale_fill_manual(values=cbbPalette) +
  scale_shape_manual(values=c(24,21,25,21)) +
  coord_flip() + 
  labs(x='',
    y='How much do you like the reaction?\n1 (Do not like at all) - 7 (Like at lot)',
    color="Respondent vis-à-vis\nnew party",
    fill="Respondent vis-à-vis\nnew party",
    shape="Respondent vis-à-vis\nnew party") + 
  theme_bw() +
  theme(legend.position='bottom',legend.text = element_text(size=10),
    text=element_text(family="serif"),axis.text = element_text(size=10),
    strip.background=element_blank()) +
  guides(color=guide_legend(ncol=2))
dev.off()

#_______________________________________________________________________________
# B.3.3 Responses by government vs. opposition parties ----
#_______________________________________________________________________________
# Figure B.16: ----
# Marginal means for how citizens want their most preferred party to respond to
# challengers by government vs opposition status of most preferred party.
#_______________________________________________________________________________
mms <- data %>% 
  mutate(cabinet_party=as.factor(ifelse(cabinet_party==1,"Government","Opposition"))) %>%
  droplevels() %>%
  cj(.,outcome~united,id=~id,by=~cabinet_party,estimate="mm")

pdf('figureB16.pdf',height=1.5) 
mms %>% 
  mutate(level=factor(level,levels=c('ignore','divided','united'))) %>%
  ggplot(aes(x=level,y=estimate,ymin=lower,ymax=upper)) +
  geom_pointrange(position=position_dodge2(width=.6),size=.2) +
  facet_wrap(~cabinet_party) +
  scale_color_manual(values=cbbPalette) +
  coord_flip() + 
  labs(x='',y='How much do you like the reaction?\n1 (Do not like at all) - 7 (Like at lot)') + 
  theme_bw() +
  theme(legend.position='bottom',legend.title=element_blank(),
    axis.text.y=element_text(size=8),strip.background=element_blank())
dev.off()

#_______________________________________________________________________________
# Figure B.17: ----
# Marginal means for how citizens want their most preferred party 
# to respond to new parties depending on whether they like the proposal of the 
# new party by government vs opposition status of most preferred party.
#_______________________________________________________________________________
mms <- data %>% 
  filter(united_detail!="") %>%
  mutate(cabinet_party=as.factor(ifelse(cabinet_party==1,"Government","Opposition"))) %>%
  droplevels() %>%
  cj(.,outcome~ united_detail,
     id=~id,
     by=~congruence+cabinet_party,
     estimate="mm") 

pdf('figureB17.pdf',height=3)
mms %>% 
  filter(feature=="united_detail") %>%
  mutate(
    level=factor(level,levels=c('ignore','adversarial + ignore',
      'adversarial + accommodative','adversarial', 'accommodative + ignore',
      'accommodative')),
    congruence=factor(congruence,levels=c('same extreme position',
      'neither same nor opposite','opposite extreme position',"doesn't know"))) %>%
  ggplot(aes(x=level,y=estimate,ymin=lower,ymax=upper,
    color=congruence,fill=congruence,shape=congruence)) +
  facet_wrap(~cabinet_party) +
  geom_pointrange(position=position_dodge2(width=.6),size=.3) +
  scale_color_manual(values=cbbPalette) +
  scale_fill_manual(values=cbbPalette) +
  scale_shape_manual(values=c(24,21,25,21)) +
  coord_flip() + 
  labs(x='',
    y='How much do you like the reaction?\n1 (Do not like at all) - 7 (Like at lot)',
    color="Respondent vis-à-vis\nnew party",
    fill="Respondent vis-à-vis\nnew party",
    shape="Respondent vis-à-vis\nnew party") + 
  theme_bw() +
  theme(legend.position='bottom',legend.text = element_text(size=10),
    text=element_text(family="serif"),axis.text = element_text(size=10),
    strip.background=element_blank()) +
  guides(color=guide_legend(ncol=2))
dev.off()

#_______________________________________________________________________________
# B.3.4 Responses by party unity among party leadership ----
#_______________________________________________________________________________
# Figure B.18: ----
# Marginal means for how citizens want their most preferred party to respond to
# new parties by experts' perceptions (CHES) of actual division within the party 
# leadership.
#_______________________________________________________________________________
mms <- data %>% 
  droplevels() %>%
  filter(!is.na(party_dissent)) %>%
  mutate(party_dissent=as.factor(ifelse(party_dissent>mean(party_dissent),
    "Divided","United"))) %>%
  cj(.,outcome~united,id=~id,by=~party_dissent,estimate="mm")

pdf('figureB18.pdf',height=1.5) 
mms %>% 
  mutate(level=factor(level,levels=c('ignore','divided','united'))) %>%
  ggplot(aes(x=level,y=estimate,ymin=lower,ymax=upper)) +
  geom_pointrange(position=position_dodge2(width=.6),size=.2) +
  facet_wrap(~party_dissent) +
  scale_color_manual(values=cbbPalette) +
  coord_flip() + 
  labs(x='',y='How much do you like the reaction?\n1 (Do not like at all) - 7 (Like at lot)') + theme_bw() +
  theme(legend.position='bottom',legend.title=element_blank(),
        axis.text.y=element_text(size=8),strip.background=element_blank())
dev.off()

#_______________________________________________________________________________
# Figure B.19: ----
# Marginal means for how citizens want their most preferred party to respond
# to new parties depending on whether they like the proposal of the new party 
# by experts' perceptions (CHES) of actual division within the party leadership.
#_______________________________________________________________________________
mms <- data %>% 
  filter(!is.na(party_dissent)) %>%
  mutate(party_dissent=as.factor(ifelse(party_dissent>mean(party_dissent),
    "Divided","United"))) %>%
  droplevels() %>%
  cj(.,outcome~ united_detail,
     id=~id,
     by=~congruence+party_dissent,
     estimate="mm") 

pdf('figureB19.pdf',height=3)
mms %>% 
  filter(feature=="united_detail") %>%
  mutate(
    level=factor(level,levels=c('ignore','adversarial + ignore',
      'adversarial + accommodative','adversarial', 'accommodative + ignore',
      'accommodative')),
    congruence=factor(congruence,levels=c('same extreme position',
      'neither same nor opposite','opposite extreme position',"doesn't know"))) %>%
  ggplot(aes(x=level,y=estimate,ymin=lower,ymax=upper,
    color=congruence,fill=congruence,shape=congruence)) +
  facet_wrap(~party_dissent) +
  geom_pointrange(position=position_dodge2(width=.6),size=.3) +
  scale_color_manual(values=cbbPalette) +
  scale_fill_manual(values=cbbPalette) +
  scale_shape_manual(values=c(24,21,25,21)) +
  coord_flip() + 
  labs(x='',
    y='How much do you like the reaction?\n1 (Do not like at all) - 7 (Like at lot)',
    color="Respondent vis-à-vis\nnew party",
    fill="Respondent vis-à-vis\nnew party",
    shape="Respondent vis-à-vis\nnew party") + 
  theme_bw() +
  theme(legend.position='bottom',legend.text = element_text(size=10),
    text=element_text(family="serif"),axis.text = element_text(size=10),
    strip.background=element_blank()) +
  guides(color=guide_legend(ncol=2))
dev.off()

#_______________________________________________________________________________
# B.3.5 Responses by strength of party attachment ----
#_______________________________________________________________________________
# Figure B.20: ----
# Marginal means for how citizens want their most preferred party to respond to
# new parties by stronger vs weaker party attachment by the respondent to the 
# most preferred party.
#_______________________________________________________________________________
mms <- data %>% 
  filter(!is.na(party_ident_pre)) %>%
  group_by(country) %>%
  mutate(party_ident=factor(ifelse(party_ident_pre>mean(party_ident_pre),
    "Strongly attached","Weakly attached"))) %>%
  ungroup() %>%
  cj(.,outcome~united,id=~id,by=~party_ident,estimate="mm")

pdf('figureB20.pdf',height=1.5) 
mms %>% 
  mutate(level=factor(level,levels=c('ignore','divided','united'))) %>%
  ggplot(aes(x=level,y=estimate,ymin=lower,ymax=upper)) +
  geom_pointrange(position=position_dodge2(width=.6),size=.2) +
  facet_wrap(~party_ident) +
  scale_color_manual(values=cbbPalette) + 
  coord_flip() + 
  labs(x='',y='How much do you like the reaction?\n1 (Do not like at all) - 7 (Like at lot)') + 
  theme_bw() +
  theme(legend.position='bottom',legend.title=element_blank(),
    axis.text.y=element_text(size=8),strip.background=element_blank())
dev.off()

#_______________________________________________________________________________
# Figure B.21: ----
# Marginal means for how citizens want their most preferred party to respond to
# new parties depending on whether they like the proposal of the new party by 
# stronger vs weaker party attachment by the respondent to the most preferred 
# party.
#_______________________________________________________________________________
mms <- data %>% 
  filter(!is.na(party_ident_pre)) %>%
  group_by(country) %>%
  mutate(party_ident=factor(ifelse(party_ident_pre>mean(party_ident_pre),
    "Strongly attached","Weakly attached"))) %>%
  ungroup() %>%
  droplevels() %>%
  cj(.,outcome~ united_detail,
     id=~id,
     by=~congruence+party_ident,
     estimate="mm") 

pdf('figureB21.pdf',height=3)
mms %>% 
  filter(feature=="united_detail") %>%
  mutate(
    level=factor(level,levels=c('ignore','adversarial + ignore',
      'adversarial + accommodative','adversarial', 'accommodative + ignore',
      'accommodative')),
    congruence=factor(congruence,levels=c('same extreme position',
      'neither same nor opposite','opposite extreme position',"doesn't know"))) %>%
  ggplot(aes(x=level,y=estimate,ymin=lower,ymax=upper,
    color=congruence,fill=congruence,shape=congruence)) +
  facet_wrap(~party_ident) +
  geom_pointrange(position=position_dodge2(width=.6),size=.3) +
  scale_color_manual(values=cbbPalette) +
  scale_fill_manual(values=cbbPalette) +
  scale_shape_manual(values=c(24,21,25,21)) +
  coord_flip() + 
  labs(x='',
    y='How much do you like the reaction?\n1 (Do not like at all) - 7 (Like at lot)',
    color="Respondent vis-à-vis\nnew party",
    fill="Respondent vis-à-vis\nnew party",
    shape="Respondent vis-à-vis\nnew party") + 
  theme_bw() +
  theme(legend.position='bottom',legend.text = element_text(size=10),
    text=element_text(family="serif"),axis.text = element_text(size=10),
    strip.background=element_blank()) +
  guides(color=guide_legend(ncol=2))
dev.off()

#_______________________________________________________________________________
# B.3.6 Responses by pre- vs post-elicitation of party identification ----
#_______________________________________________________________________________
# Figure B.22: ----
# Marginal means for how citizens want their most preferred party to respond to
# new parties by pre- vs post-experiment elicitation of party identification.
#_______________________________________________________________________________
mms <- data %>%
  droplevels() %>%
  mutate(
    pre_elicit=factor(ifelse(is.na(party_ident_post),'Before experiment',
      'After experiment'),levels=c('Before experiment','After experiment'))) %>%
  cj(.,outcome~united,id=~id,by=~pre_elicit,estimate="mm")

pdf('figureB22.pdf',height=1.5) 
mms %>% 
  mutate(level=factor(level,levels=c('ignore','divided','united'))) %>%
  ggplot(aes(x=level,y=estimate,ymin=lower,ymax=upper)) +
  geom_pointrange(position=position_dodge2(width=.6),size=.2) +
  facet_wrap(~pre_elicit) +
  scale_color_manual(values=cbbPalette) + 
  coord_flip() + 
  labs(x='',y='How much do you like the reaction?\n1 (Do not like at all) - 7 (Like at lot)') + 
  theme_bw() +
  theme(legend.position='bottom',legend.title=element_blank(),
    axis.text.y=element_text(size=8),strip.background=element_blank())
dev.off()

#_______________________________________________________________________________
# Figure B.23: ----
# Marginal means for how citizens want their most preferred party to respond to
# new parties depending on whether they like the proposal of the new party by 
# pre- vs post-experiment elicitation of party identification
#_______________________________________________________________________________
mms <- data %>% 
  droplevels() %>%
  mutate(
    pre_elicit=factor(ifelse(is.na(party_ident_post),'Before experiment',
      'After experiment'),levels=c('Before experiment','After experiment'))) %>%
  cj(.,outcome~ united_detail,
     id=~id,
     by=~congruence+pre_elicit,
     estimate="mm") 

pdf('figureB23.pdf',height=3)
mms %>% 
  filter(feature=="united_detail") %>%
  mutate(
    level=factor(level,levels=c('ignore','adversarial + ignore',
      'adversarial + accommodative','adversarial', 'accommodative + ignore',
      'accommodative')),
    congruence=factor(congruence,levels=c('same extreme position',
      'neither same nor opposite','opposite extreme position',"doesn't know"))) %>%
  ggplot(aes(x=level,y=estimate,ymin=lower,ymax=upper,
    color=congruence,fill=congruence,shape=congruence)) +
  facet_wrap(~pre_elicit) +
  geom_pointrange(position=position_dodge2(width=.6),size=.3) +
  scale_color_manual(values=cbbPalette) +
  scale_fill_manual(values=cbbPalette) +
  scale_shape_manual(values=c(24,21,25,21)) +
  coord_flip() + 
  labs(x='',
    y='How much do you like the reaction?\n1 (Do not like at all) - 7 (Like at lot)',
    color="Respondent vis-à-vis\nnew party",
    fill="Respondent vis-à-vis\nnew party",
    shape="Respondent vis-à-vis\nnew party") + 
  theme_bw() +
  theme(legend.position='bottom',legend.text = element_text(size=10),
    text=element_text(family="serif"),axis.text = element_text(size=10),
    strip.background=element_blank()) +
  guides(color=guide_legend(ncol=2))
dev.off()

#_______________________________________________________________________________
# B.3.7 Responses by issue salience ----
#_______________________________________________________________________________
# Figure B.24: ----
# Marginal means for how citizens want their most preferred party to respond to
# new parties by issue salience.
#_______________________________________________________________________________
out <- data %>%
  mutate(
    policy_climate_dk=ifelse(policy_climate=="don't know",1,0),
    policy_immigration_dk=ifelse(policy_immigration=="don't know",1,0),
    policy_tech_dk=ifelse(policy_tech=="don't know",1,0),
    policy_freedom_dk=ifelse(policy_freedom=="don't know",1,0)) %>%
  group_by(country) %>%
  mutate(across(ends_with('_dk'),~mean(.x,na.rm=T))) %>%
  ungroup() %>%
  mutate(
    p_cl_dkSplit=ifelse(policy_climate_dk<median(policy_climate_dk,na.rm=T),
      'More salient','Less salient'),
    p_im_dkSplit=ifelse(policy_immigration_dk<median(policy_immigration_dk,na.rm=T),
      'More salient','Less salient'),
    p_te_dkSplit=ifelse(policy_tech_dk<median(policy_tech_dk,na.rm=T),
      'More salient','Less salient'),
    p_fr_dkSplit=ifelse(policy_freedom_dk<median(policy_freedom_dk,na.rm=T),
      'More salient','Less salient'),
    policy_dkSplit=factor(ifelse(policy_topic=='Climate',p_cl_dkSplit,
      ifelse(policy_topic=='Health and freedom',p_fr_dkSplit,
      ifelse(policy_topic=='Immi voting',p_im_dkSplit,p_te_dkSplit)))))

mms <- out %>%
  droplevels() %>%
  group_by(policy_topic) %>%
  do(cj(.,outcome~united,id=~id,by=~policy_dkSplit,estimate="mm"))

pdf('figureB24.pdf',height=2) 
mms %>% 
  mutate(level=factor(level,levels=c('ignore','divided','united'))) %>%
  ggplot(aes(x=level,y=estimate,ymin=lower,ymax=upper,color=policy_dkSplit,
    fill=policy_dkSplit)) +
  facet_grid(~policy_topic) +
  geom_pointrange(position=position_dodge2(width=.6),size=.2) +
  scale_color_manual(values=cbbPalette) + 
  coord_flip() + 
  labs(x='',y='How much do you like the reaction?\n1 (Do not like at all) - 7 (Like at lot)') + 
  theme_bw() +
  theme(legend.position='bottom',legend.title=element_blank(),
    axis.text.y=element_text(size=8),strip.background=element_blank())
dev.off()

#_______________________________________________________________________________
# Figure B.25: ----
#  Marginal means for how citizens want their most preferred party to respond to
# new parties depending on whether they like the proposal of the new party by 
# issue salience.
#_______________________________________________________________________________
mms <- out %>% 
  droplevels() %>%
  group_by(policy_topic) %>%
  do(cj(.,outcome~united_detail,
    id=~id,
    by=~congruence+policy_dkSplit,
    estimate="mm"))

pdf('figureB25.pdf',height=3)
mms %>% 
  filter(feature=="united_detail") %>%
  mutate(
    level=factor(level,levels=c('ignore','adversarial + ignore',
      'adversarial + accommodative','adversarial', 'accommodative + ignore',
      'accommodative')),
    congruence=factor(congruence,levels=c('same extreme position',
      'neither same nor opposite','opposite extreme position',"doesn't know"))) %>%
  ggplot(aes(x=level,y=estimate,ymin=lower,ymax=upper,
    color=congruence,fill=congruence,shape=congruence)) +
  facet_wrap(~policy_dkSplit) +
  geom_pointrange(position=position_dodge2(width=.6),size=.3) +
  scale_color_manual(values=cbbPalette) +
  scale_fill_manual(values=cbbPalette) +
  scale_shape_manual(values=c(24,21,25,21)) +
  coord_flip() + 
  labs(x='',
    y='How much do you like the reaction?\n1 (Do not like at all) - 7 (Like at lot)',
    color="Respondent vis-à-vis\nnew party",
    fill="Respondent vis-à-vis\nnew party",
    shape="Respondent vis-à-vis\nnew party") + 
  theme_bw() +
  theme(legend.position='bottom',legend.text = element_text(size=10),
    text=element_text(family="serif"),axis.text = element_text(size=10),
    strip.background=element_blank()) +
  guides(color=guide_legend(ncol=2))
dev.off()

#_______________________________________________________________________________
# B.3.8 Responses by respondents' populist attitude ----
#_______________________________________________________________________________
# Figure B.26: ----
# Marginal means for how citizens want their most preferred party to respond to
# new parties by more vs less populist attitudes of the respondent.
#_______________________________________________________________________________
mms <- data %>% 
  cj(.,outcome~united,id=~id,by=~populist_respondent,estimate="mm")

pdf('figureB26.pdf',height=1.5) 
mms %>% 
  mutate(level=factor(level,levels=c('ignore','divided','united'))) %>%
  ggplot(aes(x=level,y=estimate,ymin=lower,ymax=upper)) +
  geom_pointrange(position=position_dodge2(width=.6),size=.2) +
  facet_wrap(~populist_respondent) +
  scale_color_manual(values=cbbPalette) + 
  coord_flip() + 
  labs(x='',y='How much do you like the reaction?\n1 (Do not like at all) - 7 (Like at lot)') + theme_bw() +
  theme(legend.position='bottom',legend.title=element_blank(),
    axis.text.y=element_text(size=8),strip.background=element_blank())
dev.off()

#_______________________________________________________________________________
# Figure B.27: ----
# Marginal means for how citizens want their most preferred party to respond to
# new parties depending on whether they like the proposal of the new party by 
# more vs less populist attitudes of the respondent.
#_______________________________________________________________________________
mms <- data %>% 
  droplevels() %>%
  cj(.,outcome~ united_detail,
     id=~id,
     by=~congruence+populist_respondent,
     estimate="mm") 

pdf('figureB27.pdf',height=3)
mms %>% 
  filter(feature=="united_detail") %>%
  mutate(
    level=factor(level,levels=c('ignore','adversarial + ignore',
      'adversarial + accommodative','adversarial', 'accommodative + ignore',
      'accommodative')),
    congruence=factor(congruence,levels=c('same extreme position',
      'neither same nor opposite','opposite extreme position',"doesn't know"))) %>%
  ggplot(aes(x=level,y=estimate,ymin=lower,ymax=upper,
    color=congruence,fill=congruence,shape=congruence)) +
  facet_wrap(~populist_respondent) +
  geom_pointrange(position=position_dodge2(width=.6),size=.3) +
  scale_color_manual(values=cbbPalette) +
  scale_fill_manual(values=cbbPalette) +
  scale_shape_manual(values=c(24,21,25,21)) +
  coord_flip() + 
  guides(color=guide_legend(ncol=2),fill=guide_legend(ncol=2)) +
  labs(x='',
    y='How much do you like the reaction?\n1 (Do not like at all) - 7 (Like at lot)',
    color="Respondent vis-à-vis\nnew party",
    fill="Respondent vis-à-vis\nnew party",
    shape="Respondent vis-à-vis\nnew party") + 
  theme_bw() +
  theme(legend.position='bottom',legend.text = element_text(size=10),
    text=element_text(family="serif"),axis.text = element_text(size=10),
    strip.background=element_blank()) +
  guides(color=guide_legend(ncol=2))
dev.off()

#_______________________________________________________________________________
# B.3.9 Responses by gender ----
#_______________________________________________________________________________
# Figure B.28: ----
#  Marginal means for how citizens want their most preferred party to respond to
# new parties by respondents' gender.
#_______________________________________________________________________________
mms <- data %>%
  mutate(gender=factor(recode(gender,`0`='Male',`1`='Female'),
    levels=c('Male','Female'))) %>%
  droplevels() %>%
  cj(.,outcome~united,id=~id,by=~gender,estimate="mm")

pdf('figureB28.pdf',height=2) 
mms %>% 
  mutate(level=factor(level,levels=c('ignore','divided','united'))) %>%
  ggplot(aes(x=level,y=estimate,ymin=lower,ymax=upper,color=gender,
    fill=gender)) +
  geom_pointrange(position=position_dodge2(width=.6),size=.2) +
  scale_color_manual(values=cbbPalette) + 
  coord_flip() + 
  labs(x='',y='How much do you like the reaction?\n1 (Do not like at all) - 7 (Like at lot)') + 
  theme_bw() +
  theme(legend.position='bottom',legend.title=element_blank(),
    axis.text.y=element_text(size=8),strip.background=element_blank())
dev.off()

#_______________________________________________________________________________
# Figure B.29: ----
# Marginal means for how citizens want their most preferred party to respond to
# new parties depending on whether they like the proposal of the new party by 
# respondents' gender.
#_______________________________________________________________________________
mms <- data %>%
  mutate(gender=factor(recode(gender,`0`='Male',`1`='Female'),
    levels=c('Male','Female'))) %>%
  droplevels() %>%
  cj(.,outcome~united_detail,
     id=~id,
     by=~congruence+gender,
     estimate="mm") 

pdf('figureB29.pdf',height=3)
mms %>% 
  filter(feature=="united_detail") %>%
  mutate(
    level=factor(level,levels=c('ignore','adversarial + ignore',
      'adversarial + accommodative','adversarial', 'accommodative + ignore',
      'accommodative')),
    congruence=factor(congruence,levels=c('same extreme position',
      'neither same nor opposite','opposite extreme position',"doesn't know"))) %>%
  ggplot(aes(x=level,y=estimate,ymin=lower,ymax=upper,
    color=congruence,fill=congruence,shape=congruence)) +
  facet_wrap(~gender) +
  geom_pointrange(position=position_dodge2(width=.6),size=.3) +
  scale_color_manual(values=cbbPalette) +
  scale_fill_manual(values=cbbPalette) +
  scale_shape_manual(values=c(24,21,25,21)) +
  coord_flip() + 
  labs(x='',
    y='How much do you like the reaction?\n1 (Do not like at all) - 7 (Like at lot)',
    color="Respondent vis-à-vis\nnew party",
    fill="Respondent vis-à-vis\nnew party",
    shape="Respondent vis-à-vis\nnew party") + 
  theme_bw() +
  theme(legend.position='bottom',legend.text = element_text(size=10),
    text=element_text(family="serif"),axis.text = element_text(size=10),
    strip.background=element_blank()) +
  guides(color=guide_legend(ncol=2))
dev.off()

#_______________________________________________________________________________
# B.3.10 Responses to vignettes by populist vs anti-government statement voiced 
# by the new party ----
#_______________________________________________________________________________
# Figure B.30: ----
# Marginal means for how citizens want their most preferred party to respond to
# new parties by populist vs anti-government attacks of the new party.
#_______________________________________________________________________________
mms <- data %>%
  droplevels() %>%
  cj(.,outcome~united,id=~id,by=~populism,estimate="mm")

pdf('figureB30.pdf',height=2) 
mms %>% 
  mutate(level=factor(level,levels=c('ignore','divided','united'))) %>%
  ggplot(aes(x=level,y=estimate,ymin=lower,ymax=upper,color=populism,
    fill=populism)) +
  geom_pointrange(position=position_dodge2(width=.6),size=.2) +
  scale_color_manual(values=cbbPalette) + 
  coord_flip() + 
  labs(x='',y='How much do you like the reaction?\n1 (Do not like at all) - 7 (Like at lot)') + 
  theme_bw() +
  theme(legend.position='bottom',legend.title=element_blank(),
    axis.text.y=element_text(size=8),strip.background=element_blank())
dev.off()

#_______________________________________________________________________________
# Figure B.25: ----
# Marginal means for how citizens want their most preferred party to respond to
# new parties depending on whether they like the proposal of the new party by 
# populist vs anti-government attacks of the new party.
#_______________________________________________________________________________
mms <- data %>% 
  droplevels() %>%
  cj(.,outcome~ united_detail,
     id=~id,
     by=~congruence+populism,
     estimate="mm") 

pdf('figureB31.pdf',height=3)
mms %>% 
  filter(feature=="united_detail") %>%
  mutate(
    level=factor(level,levels=c('ignore','adversarial + ignore',
      'adversarial + accommodative','adversarial', 'accommodative + ignore',
      'accommodative')),
    congruence=factor(congruence,levels=c('same extreme position',
      'neither same nor opposite','opposite extreme position',"doesn't know"))) %>%
  ggplot(aes(x=level,y=estimate,ymin=lower,ymax=upper,
    color=congruence,fill=congruence,shape=congruence)) +
  facet_wrap(~populism) +
  geom_pointrange(position=position_dodge2(width=.6),size=.3) +
  scale_color_manual(values=cbbPalette) +
  scale_fill_manual(values=cbbPalette) +
  scale_shape_manual(values=c(24,21,25,21)) +
  coord_flip() + 
  labs(x='',
    y='How much do you like the reaction?\n1 (Do not like at all) - 7 (Like at lot)',
    color="Respondent vis-à-vis\nnew party",
    fill="Respondent vis-à-vis\nnew party",
    shape="Respondent vis-à-vis\nnew party") + 
  theme_bw() +
  theme(legend.position='bottom',legend.text = element_text(size=10),
    text=element_text(family="serif"),axis.text = element_text(size=10),
    strip.background=element_blank()) +
  guides(color=guide_legend(ncol=2))
dev.off()

#_______________________________________________________________________________
# Figure B.32: ----
# Marginal means for how citizens want their most preferred party to respond to
# challengers depending on whether they show more or less populist attitudes, 
# averaging across policy areas and challenger attacks by populist vs 
# anti-government statement.
#_______________________________________________________________________________
mms <- data %>% 
  droplevels() %>%
  cj(.,outcome~ united_detail,
     id=~id,
     by=~populist_respondent+populism,
     estimate="mm") 

pdf('figureB32.pdf',height=3)
mms %>% 
  filter(feature=="united_detail") %>%
  mutate(level=factor(level,levels=c(
    'ignore','adversarial + ignore','adversarial + accommodative','adversarial', 
    'accommodative + ignore','accommodative'))) %>%
  ggplot(aes(x=level,y=estimate,ymin=lower,ymax=upper,
    color=populist_respondent,fill=populist_respondent)) +
  facet_wrap(~populism) +
  geom_pointrange(position=position_dodge2(width=.6),size=.3) +
  scale_color_manual(values=cbbPalette) +
  scale_fill_manual(values=cbbPalette) +
  scale_shape_manual(values=c(24,21,25,21)) +
  coord_flip() + 
  labs(x='',
    y='How much do you like the reaction?\n1 (Do not like at all) - 7 (Like at lot)',
    color="Respondent is",
    fill="Respondent is") + 
  theme_bw() +
  theme(legend.position='bottom',legend.text = element_text(size=10),
    text=element_text(family="serif"),axis.text = element_text(size=10),
    strip.background=element_blank()) +
  guides(color=guide_legend(ncol=2))
dev.off()

#_______________________________________________________________________________
# B.3.11 Responses by policy area ----
#_______________________________________________________________________________
# Figure B.33: ----
# Marginal means for how citizens want their most preferred party to respond to
# new parties by policy area.
#_______________________________________________________________________________
mms <- data %>% 
  mutate(policy_topic=recode(policy_topic,'Immi voting'="Immigrants' voting rights")) %>%
  cj(.,outcome~united,id=~id,by=~policy_topic,estimate="mm")

pdf('figureB33.pdf',height=1.5) 
mms %>% 
  mutate(level=factor(level,levels=c('ignore','divided','united'))) %>%
  ggplot(aes(x=level,y=estimate,ymin=lower,ymax=upper)) +
  geom_pointrange(position=position_dodge2(width=.6),size=.2) +
  facet_grid(~policy_topic) +
  scale_color_manual(values=cbbPalette) + 
  coord_flip() + 
  labs(x='',y='How much do you like the reaction?\n1 (Do not like at all) - 7 (Like at lot)') + 
  theme_bw() +
  theme(legend.position='bottom',legend.title=element_blank(),
    axis.text.y=element_text(size=8),strip.background=element_blank())
dev.off()

#_______________________________________________________________________________
# Figure B.34: ----
# Marginal means for how citizens want their most preferred party to respond to
# new parties depending on whether they like the proposal of the new party by 
# policy area.
#_______________________________________________________________________________
mms <- data %>% 
  droplevels() %>%
  mutate(policy_topic=recode(policy_topic,'Immi voting'="Immigrants' voting rgths")) %>%
  cj(.,outcome~ united_detail,
     id=~id,
     by=~congruence+policy_topic,
     estimate="mm") 

pdf('figureB34.pdf',height=3)
mms %>% 
  filter(feature=="united_detail") %>%
  mutate(
    level=factor(level,levels=c('ignore','adversarial + ignore',
      'adversarial + accommodative','adversarial', 'accommodative + ignore',
      'accommodative')),
    congruence=factor(congruence,levels=c('same extreme position',
      'neither same nor opposite','opposite extreme position',"doesn't know"))) %>%
  ggplot(aes(x=level,y=estimate,ymin=lower,ymax=upper,
    color=congruence,fill=congruence,shape=congruence)) +
  facet_grid(~policy_topic) +
  geom_pointrange(position=position_dodge2(width=.6),size=.3) +
  scale_color_manual(values=cbbPalette) +
  scale_fill_manual(values=cbbPalette) +
  scale_shape_manual(values=c(24,21,25,21)) +
  coord_flip() + 
  labs(x='',
    y='How much do you like the reaction?\n1 (Do not like at all) - 7 (Like at lot)',
    color="Respondent vis-à-vis\nnew party",
    fill="Respondent vis-à-vis\nnew party",
    shape="Respondent vis-à-vis\nnew party") + 
  theme_bw() +
  theme(legend.position='bottom',legend.text = element_text(size=10),
    text=element_text(family="serif"),axis.text = element_text(size=10),
    strip.background=element_blank()) +
  guides(color=guide_legend(ncol=2))
dev.off()

#______________________________________________________________________________#
#______________________________________________________________________________#
#______________________________________________________________________________#
