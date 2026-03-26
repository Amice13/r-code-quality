
# Project: Filling the Tax Gap - Replication Supplementary Information (Appendix)
# Author: Simone Paci 
# Date: 07/07/2025

# Housecleaning ----------------------------------------------------------------

rm(list=ls())
set.seed(123456789)
options(scipen=999)
options(warn=-1)
time1 = Sys.time()

# Set Working Directory
#setwd("")

packages <- c("lme4", "ggplot2", "tidyverse", "prediction", "RColorBrewer",
              "stringi", "stringr", "XML", "stargazer", "ggrepel", "cjoint")

# Install (only if needed)
# installed <- packages %in% rownames(installed.packages())
# if (any(!installed)) install.packages(packages[!installed])

# Load
lapply(packages, library, character.only = TRUE)

# SI Replication  ---------------------------------------------------------------------

# Loading data ---------------------------------------------------------------------

data = readRDS('Data/municipalities_panel_rep.RDS')
drives = readRDS('Data/donation_drives_rep.RDS')
survey_full = readRDS('Data/survey_rep.RDS')
survey = survey_full[survey_full$att_check==2,]

## Table A1: Summary Stats ----------------------------------------------------------

df = data[,c('vat_pub_t','vat_rec_t',
             'amount','amount_social','amount_econ','n_don','population', 
             'unemp_p', 'HHI_gen','edu11_high','edu11_low',
             "mun_revenue_tax","mun_tax_claims","mun_exp_t")]

stargazer(
  as.data.frame(
    df), 
  digits = 1,
  type='latex',
  title = 'Summary Statistics',
  omit.summary.stat = c('p25','p75'),
  covariate.labels = c(
    'Noncompliance Disclosures',
    'Enforcement Disclosures',
    'Donations (total, EUR)',
    'Donations (social-benefit, EUR)',
    'Donations (local-economy, EUR)',
    '\\# Beneficiaries',
    'Population','Unemployment (\\%)',
    'Diversity Index (HHI)',
    'College Graduates (\\%)',
    'No HS Diploma (\\%)',
    'Municipal Tax Revenue',
    'Municipal Tax Gap', 
    'Municipal Expenditure'),
  label = 'summary_stats',
  out = 'Output/summary_stats.tex')


## Figure A3: Number VAT Events ------------------------

data$n_vat_events = sapply(1:nrow(data), function (x)
  ifelse(is.na(data$vat_pub_dates[x]),0,
         length(unique(unlist(str_split(data$vat_pub_dates[x],';'))))))

p = ggplot(data[!duplicated(data$mun_clean),],
           aes(n_vat_events)) +
  geom_bar() +
  scale_x_continuous(breaks = 0:9) +
  labs(x='Total Number of VAT Tax Enforcement Events (2009-2021)',
       y = 'Number of Municipalities') +
  theme_classic() +
  theme(text=element_text(size=20))

df_plot <- data %>%
  filter(!duplicated(mun_clean)) %>%
  mutate(
    vat_event_bin = case_when(
      n_vat_events == 0 ~ "0",
      n_vat_events == 1 ~ "1",
      n_vat_events == 2 ~ "2",
      n_vat_events == 3 ~ "3",
      n_vat_events == 4 ~ "4",
      n_vat_events == 5 ~ "5",
      n_vat_events >= 6 & n_vat_events <= 10 ~ "6–10",
      n_vat_events >= 11 & n_vat_events <= 20 ~ "11–20",
      n_vat_events >= 21 & n_vat_events <= 50 ~ "21–50",
      n_vat_events > 50 ~ "51+"
    ),
    vat_event_bin = factor(vat_event_bin, levels = c("0", "1", "2", "3", "4", "5", "6–10", "11–20", "21–50", "51+"))
  )

# Plot
p = ggplot(df_plot, aes(x = vat_event_bin)) +
  geom_bar(fill = "black", color = "black") +
  labs(
    x='Total Number of VAT Tax Enforcement Events (2009-2021)',
    y = 'Number of Municipalities'
  ) +
  theme_classic(base_size = 20)

pdf(file = 'Output/SK_enf_histogram.pdf', width=12,height=7)
p
dev.off()

## Figure A4: Distribution of Enforcement Disclosures ---------------------------------

dates_rec = unique(data$vat_rec_dates)
dates_rec = sapply(1:length(dates_rec), function(x) 
  unique(unlist(strsplit(dates_rec[x],';'))))
dates_rec = unlist(strsplit(unlist(dates_rec),';'))

dates_rec = sapply(1:length(dates_rec), function(x){
  y = str_sub(dates_rec[x],7,10)
  return(as.numeric(as.Date(dates_rec[x],format = '%d.%m.%Y')-
                      as.Date(paste0('01/01/',y), format = '%d/%m/%Y')))
})
df = data.frame(dates_rec=dates_rec)
df$group = ifelse(df$dates_rec<=90,'All Treated', 
                  ifelse(df$dates_rec>90 & df$dates_rec<=120,'Employees Treated / VAT Control',
                         'All Control'))
df$group = factor(df$group, levels =c('All Treated','Employees Treated / VAT Control',
                                      'All Control'))

p=ggplot(df,aes(dates_rec, fill=group)) +
  geom_histogram(binwidth = 10) +
  labs(y ='VAT Enforcement Disclosures (#)',
       x = '') +
  scale_x_continuous(breaks = c(0,91,181,271,360), 
                     labels = c('January','April','July','October','December')) +
  geom_vline(xintercept=90,linetype='dashed') +
  ggplot2::annotate(geom = "text", x = 90, y = 1000, 
                    label = "Donation Deadline \n VAT Taxpayers (March 31st)",
                    angle=90,size=4) +
  geom_vline(xintercept=120,linetype='dashed') +
  ggplot2::annotate(geom = "text", x = 130, y = 1000, 
                    label = "Donation Deadline \n Employees (April 30th)",
                    angle=90,size=4) +
  theme_classic() +
  scale_fill_manual(values = c("All Treated" = "black", 
                               "Employees Treated / VAT Control" = "gray50", 
                               "All Control" = "lightgray"), name = "") +
  guides(fill = guide_legend(title = " ")) +
  theme(text=element_text(size=20),
        legend.position = 'bottom')

pdf(file = 'Output/hist_enforcement.pdf', width=12,height=7)
p
dev.off()

## Table A2: Predictors of Noncompliance --------------------------------------------

m1 = lmer(scale(vat_eva_thisyear) ~ 
            scale(pop) + scale(HHI_gen) +  
            scale(edu11_high) + scale(edu11_low) +
            scale(unemp_p)  + scale(n_don) +
            scale(taxoff_dist) + taxoff_type +
            scale(mun_revenue_tax) + 
            scale(mun_tax_claims) + scale(mun_exp_t) + 
            sin_civ + sin_right + sin_left + sin_popul + 
            (1 | mun_clean) + (1|year), 
          data)

m2 = lmer(scale(treat_pub_n) ~  
            scale(pop) + scale(HHI_gen) +  
            scale(edu11_high) + scale(edu11_low) +
            scale(unemp_p)  + scale(n_don) +
            scale(taxoff_dist) + taxoff_type +
            scale(mun_revenue_tax) + 
            scale(mun_tax_claims) + scale(mun_exp_t) + 
            sin_civ + sin_right + sin_left + sin_popul + 
            (1 | mun_clean) + (1|year), 
          data)

m3 = lmer(scale(treat_rec_n) ~  
            scale(pop) + scale(HHI_gen) +  
            scale(edu11_high) + scale(edu11_low) +
            scale(unemp_p)  + scale(n_don) +
            scale(taxoff_dist) + taxoff_type +
            scale(mun_revenue_tax) + 
            scale(mun_tax_claims) + scale(mun_exp_t) + 
            sin_civ + sin_right + sin_left + sin_popul + 
            (1 | mun_clean) + (1|year), 
          data)

stargazer::stargazer(
  m1,m2,m3,
  title = 'Observational Results',
  dep.var.caption = 'Tax Noncompliance:',
  dep.var.labels = c('Undetected',
                     'Disclosure',
                     'Enforcement'),
  covariate.labels = c(
    'Population','Diversity Index (HHI)',
    'Edu: Graduates (\\%)', 'Edu: No HS (\\%)',
    'Unemployment (\\%)','\\# NGO Beneficiaries',
    'Tax Office Distance','Tax Office: Branch',
    'Tax Office: Contact Point',
    'Municipal Tax Revenue','Municipal Tax Gap', 
    'Municipal Expenditure',
    'Nonpartisan Mayor','Right-Wing Mayor',
    'Left-Wing Mayor','Populist Mayor'),
  keep.stat = c('n'),
  add.lines = list(c("Levels (Mun, Year):",
                     rep("Fixed",4))),
  omit = 'Constant',
  star.cutoffs = c(0.05, 0.01, 0.001),
  no.space = T,
  label = 'noncompliance_predictors',
  out = 'Output/res_eva_pred.tex')

## Figure A5: Noncompliance List Knowledge --------------------------------------------

# List Knowledge

cols = c('vat_list_know','list_know_online','list_know_dir','list_know_indir',
         'list_know_soc_media','list_know_news','list_know_mun')
cols_name = c('Know \n the list?','List \n Website',
              'Know \n someone \n on the list',
              'Heard of \n someone \n on the tist',
              'Social \n Media','Local \n News',
              'Town \n Website')

df <- data.frame(Column = character(), number = numeric(), label = character())

for (col in cols){
  total_values <- ifelse(col=='vat_list_know',nrow(survey),
                         nrow(survey[survey$vat_list_know==1,]))
  number <- sum(survey[[col]] == 1,na.rm=T)
  percent <- round(sum(survey[[col]] == 1,na.rm=T) / total_values,3) * 100
  label <- paste0(round(sum(survey[[col]] == 1,na.rm=T) / total_values,3) * 100,'%')
  df <- rbind(df, data.frame(Column = col,
                             number = number,
                             label=label))
}
df$Column = cols_name
df$Column = factor(df$Column, levels= c('Know \n the list?','List \n Website',
                                        'Know \n someone \n on the list',
                                        'Heard of \n someone \n on the tist',
                                        'Social \n Media','Local \n News',
                                        'Town \n Website'))
df <- df[order(df$number, decreasing = TRUE), ]

p = ggplot(df, aes(x = reorder(Column,-number), y = number)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = label), vjust = -0.5,size=5) +
  geom_vline(xintercept=1.5,linetype='dashed') +
  labs(x = "", y = "Respondents", title = "") +
  theme_minimal()+
  theme(text = element_text(size=20))

pdf("Output/list_know.pdf", 15, 9.5) 
print(p)
dev.off()   

## Figure A6: Noncompliance Reason Open-Ended Categories -------------------------------------------

survey$noncomp_reason_ENG = tolower(survey$noncomp_reason_ENG)

lang_dk = c("don't know",'do not know','no opinion',
            "don't understand",'neutral',"can't think",'unknown',"didn't know",'did not know',
            'have no idea', 'i have no','had no idea',
            'no comment',"can't express","don't even know","can't comment",
            'can not decide','do not have','do not understand',"can't understand",
            "haven't heard",'never heard',"don't want to say","don't comment",
            "don't recognize","don't really understand", "can't determine")

lang_greed = c('greed','get rich','profit','more money','rob','cheat',
               'glutton','deceive','save','earn more','avarice','criminal',
               'more left','get more','more','themselves','extra money',
               'i steal','^money$','keep','pocket','fraud',
               'give up money','thief','increase','enrichment','habit',
               'stupid','much property','irresponsibility',"don't want to pay",
               'rich',"to lose",'personal gain','make money',"don't like it",
               'speculat','earning money',"don't want to",'stingy','for yourself',
               'own interests','stolen','they benefit','want to pay less',
               'people lie','cheeky','want money','better earning','earned little',
               'saving','cunning','smart','financial gain','own income',
               'not honest')

lang_poor = c('poverty',"don't have money",'little left','enough money','poor',
              'insufficient income','need money','small income','lack of money',
              'no money','expensive','lack of funds','limited resources',
              'little money',"don't have enough",'income is not enough',
              "don't have",'because less income','small earnings',
              'financial situation','earn anything','salaries are low',
              'drawdown','standard of living','low income',"can't afford",
              'earn little','little income','little net income','small salaries',
              'support the family','debt','means to pay','low wages','have little',
              'shortage')

lang_hightax = c('high tax','high taxes','high rates','large tax','high','unfair',
                 'additional taxes','lot of money','pay a lot','burdened',
                 'much money','big tax','size of the tax',"it's a lot",'tax rate',
                 'tall','heavy load','large amount','paying too much',
                 'amount of taxes','excessive taxes','amount of tax',
                 'big burden','they are big','too much of their income')

lang_state = c('corrupt','acquaint','politicians','allow','get away','laws',
               'government','legal system','state','control','penalties',
               'personal purposes','no benefits','can be done','possible',
               'waste of money','dissatisfaction','parasites','fico','use of tax',
               'insufficient support','nothing will happen','tax office',
               'because they can','worth a try','not used for statutory',
               'tax system','used wisely','wasted money','steal their money anyway',
               'legislat','right places','difficult to recover','enforceability',
               'inspections')

survey$noncomp_reason_dk = as.numeric(grepl(paste0(lang_dk,collapse='|'),survey$noncomp_reason_ENG))
survey$noncomp_reason_hightax = as.numeric(grepl(paste0(lang_greed,collapse='|'),survey$noncomp_reason_ENG))
survey$noncomp_reason_greed = as.numeric(grepl(paste0(lang_hightax,collapse='|'),survey$noncomp_reason_ENG))
survey$noncomp_reason_state = as.numeric(grepl(paste0(lang_state,collapse='|'),survey$noncomp_reason_ENG))
survey$noncomp_reason_poor = as.numeric(grepl(paste0(lang_poor,collapse='|'),survey$noncomp_reason_ENG))


out = c("noncomp_reason_hightax","noncomp_reason_state","noncomp_reason_greed", 
        "noncomp_reason_poor","noncomp_reason_dk")
out_labels = c("High \n Taxes", "Bad \n Governance","Greed", "Poverty","Don't \n Know")

df <- survey %>%
  pivot_longer(cols = all_of(out)) %>%
  group_by(name, treat) %>%
  summarise(count = sum(value == 1, na.rm = TRUE),
            total = n(), 
            percent = count/total,
            se = sqrt((percent) * (1 - percent) / total)) %>%
  ungroup() %>%
  mutate(name = factor(name, levels = out, labels = out_labels)) %>%
  arrange(desc(total))

# To order outcomes by total count, first calculate total counts for each outcome
totals <- df %>%
  group_by(name) %>%
  summarise(total_count = sum(count)) %>%
  arrange(-total_count) %>%
  pull(name)

# Use fct_reorder to reorder the factor levels based on totals
df$name = factor(df$name,levels=totals)
df$treat = ifelse(df$treat==1,'Treated','Control')

p=ggplot(df, aes(x = reorder(df$name, df$total, .desc = TRUE), 
                 y = percent, fill = as.factor(treat))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = percent - se * 1.96, ymax = percent + se * 1.96),
                position = position_dodge(width = 0.8), width = 0.25) +
  scale_fill_manual(values = c("Treated" = "red", "Control" = "blue"), name = "Treatment") +
  labs(x = "", y = "Respondents (%)") +
  theme_minimal() +
  theme(text = element_text(size=20),
        legend.position = 'bottom',
        legend.title = element_blank())

pdf("Output/noncompliance_reasons.pdf", 15, 9.5) 
print(p)
dev.off()   


## Table A3: Social Service NGO Types --------------------------------

tot_social_categorized = sum(data$amount_social_elderly) + sum(data$amount_social_family) +
  sum(data$amount_social_health) + sum(data$amount_social_welfare) +
  sum(data$amount_social_other)

df = data.frame(
  t = c('Elderly Care','Family Services','Healthcare',
        'Marginalized Communities','Other'),
  p = c(100*sum(data$amount_social_elderly)/tot_social_categorized,
        100*sum(data$amount_social_family)/tot_social_categorized,
        100*sum(data$amount_social_health)/tot_social_categorized,
        100*sum(data$amount_social_welfare)/tot_social_categorized,
        100*sum(data$amount_social_other)/tot_social_categorized))

df = df %>% arrange(desc(p))
df$p = paste0(round(df$p,2),'%')

colnames(df) = c('Social Service Type','% of Donations to Social Services')

sink(file='Output/donation_type_social_table.txt')
print(xtable::xtable(df),
      include.rownames=F)
sink()

## Table A5: Why do people donate to NGOs -------------------------------------------

m1 = lmer(Lottery_1 ~ public_serv_town + serv_pref_noprof +
            bel_redistr + bel_peopleresp +  bel_govresp + 
            concern_poverty + concern_corrupt + concern_crime +
            age + male + income + edu + pol_lr + 
            (1|ip_region),
          survey)

stargazer(m1,
          title = 'Why Do People Donate to NGOs?',
          dep.var.caption = c('Public Contribution Game:'),
          dep.var.labels = c('Donations (Euros)'),
          covariate.labels = c('My community relies on public services',
                               "NGOS should provide more services",
                               "Incomes should be more equal",
                               'People responsible for themselves',
                               "Government reponsible for people",
                               'Concerned about local poverty',
                               'Concerned about state corruption',
                               'Concerned about local crime',
                               'Age','Male','Income','Education',
                               'Left-wing'),
          keep.stat = c('n'),
          add.lines = list(c("Mean Donation:",round(mean(m1@frame$Lottery_1),2)),
                           c("FE:",rep("Region",1))),
          omit = 'Constant',
          star.cutoffs = c(0.05, 0.01, 0.001),
          no.space = T,
          label = 'predictors_donate',
          out = 'Output/predictors_donations.tex' )


## Table A6: Tax Donations Full ----------------------------------------------------------

m1 = lmer(scale(amount) ~ treat_pub + 
            scale(vat_pub_lasttaxyear_pre1) +scale(vat_rec_lasttaxyear_pre1) + 
            scale(taxoff_dist) + taxoff_type +
            log(pop) + scale(HHI_gen) +  
            scale(edu11_high) + scale(edu11_low) +
            scale(mun_revenue_tax) + 
            scale(mun_tax_claims) + scale(mun_exp_t) + 
            sin_civ + sin_right + sin_left + sin_popul + 
            (1|year) + (1 | mun_clean), 
          data)

m2 = lmer(scale(amount) ~ treat_pub*scale(treat_pub_n)+
            scale(vat_pub_lasttaxyear_pre1) +scale(vat_rec_lasttaxyear_pre1) + 
            scale(taxoff_dist) + taxoff_type +
            scale(pop) + scale(HHI_gen) +  
            scale(edu11_high) + scale(edu11_low) +
            scale(mun_revenue_tax) + 
            scale(mun_tax_claims) + scale(mun_exp_t) + 
            sin_civ + sin_right + sin_left + sin_popul + 
            (1|year) + (1 | mun_clean), 
          data)

m3 = lmer(scale(amount) ~ treat_rec  +
            scale(vat_pub_lasttaxyear_pre1) +scale(vat_rec_lasttaxyear_pre1) + 
            scale(taxoff_dist) + taxoff_type +
            scale(pop) + scale(HHI_gen) +  
            scale(edu11_high) + scale(edu11_low) +
            scale(unemp_p) + 
            scale(mun_revenue_tax) + 
            scale(mun_tax_claims) + scale(mun_exp_t) + 
            sin_civ + sin_right + sin_left + sin_popul + 
            (1|year) + (1 | mun_clean), 
          data)

m4 = lmer(scale(amount) ~ treat_rec*scale(treat_rec_n) + 
            scale(vat_pub_lasttaxyear_pre1) +scale(vat_rec_lasttaxyear_pre1) + 
            scale(taxoff_dist) + taxoff_type +
            scale(pop) + scale(HHI_gen) +  
            scale(edu11_high) + scale(edu11_low) +
            scale(mun_revenue_tax) + 
            scale(mun_tax_claims) + scale(mun_exp_t) + 
            sin_civ + sin_right + sin_left + sin_popul + 
            (1|year) + (1 | mun_clean), 
          data)

stargazer(m1,m2,m3,m4,
          title = 'VAT Disclosures and Tax Donations',
          dep.var.caption = '',
          dep.var.labels = c('Tax Donations'),
          font.size = 'footnotesize',
          covariate.labels = c('Treated (Noncompliance)',
                               'Intensity (\\# NC Disclosures)',
                               'Treated (Enforcement)',
                               'Intensity (\\# Enf Disclosures)',
                               'NC Disclosures (Year Prior)',
                               'Enf Disclosures (Year Prior)',
                               'Tax Office Distance',
                               'Tax Office: Branch',
                               'Tax Office: Contact Point',
                               'Population','HHI Index',
                               'College Degree (\\%)',
                               'High-School (\\%)',
                               'Unemployed (\\%)',
                               'Town Tax Revenue',
                               'Town Tax Gap',
                               'Town Expenditure',
                               'Nonpartisan Mayor',
                               'Right-Wing Mayor',
                               'Left-Wing Mayor',
                               'Populist Mayor',
                               'Intensity (NC)*Treated (NC)',
                               'Intensity (Enf)*Treated (Enf)'),
          keep.stat = c('n'),
          add.lines = list(c("Controls:",rep("Yes",4)),
                           c("FE:",rep("Year/Town",4))),
          omit = 'Constant',
          star.cutoffs = c(0.05, 0.01, 0.001),
          no.space = T,
          label = 'natural_main_full',
          out = 'Output/natural_exp_tax_don_full.tex')


## Table A7: Donation Drives Full --------------------------------------------------------

m5 = lmer(scale(amount) ~ scale(vat_pub_lasttaxyear) + scale(vat_rec_lasttaxyear) + 
            (1|year), 
          drives)

m6 = lmer(scale(amount) ~ scale(vat_pub_lasttaxyear) + scale(vat_rec_lasttaxyear) + 
            scale(taxoff_dist) + taxoff_type +
            scale(pop) + scale(HHI_gen) +  
            scale(edu11_high) + scale(edu11_low) +
            scale(mun_revenue_tax) + 
            scale(mun_tax_claims) + scale(mun_exp_t) + 
            sin_civ + sin_right + sin_left + sin_popul + 
            (1|year), 
          drives)

m7 = lmer(scale(amount) ~ treat_pub + 
            (1|year), 
          drives)

m8 = lmer(scale(amount) ~ treat_pub + 
            scale(taxoff_dist) + taxoff_type +
            scale(pop) + scale(HHI_gen) +  
            scale(edu11_high) + scale(edu11_low) +
            scale(mun_revenue_tax) + 
            scale(mun_tax_claims) + scale(mun_exp_t) + 
            sin_civ + sin_right + sin_left + sin_popul + 
            (1|year), 
          drives)

m9 = lmer(scale(amount) ~ treat_rec +
            (1|year), 
          drives)

m10 = lmer(scale(amount) ~ treat_rec +
             scale(taxoff_dist) + taxoff_type +
             scale(pop) + scale(HHI_gen) +  
             scale(edu11_high) + scale(edu11_low) +
             scale(mun_revenue_tax) + 
             scale(mun_tax_claims) + scale(mun_exp_t) + 
             sin_civ + sin_right + sin_left + sin_popul + 
             (1|year), 
           drives)


stargazer(m5,m6,m7,m8,m9,m10,
          title = 'VAT Disclosures and NGO Fundraisers',
          dep.var.caption = c(''),
          dep.var.labels = c('NGO Donation Drives'),
          covariate.labels = c('Noncompliance (Last Year)',
                               'Enforcement (Last Year)',
                               'Tax Office Distance',
                               'Tax Office: Branch',
                               'Tax Office: Contact Point',
                               'Population',
                               'HHI Index',
                               'College Degree (\\%)',
                               'High-School (\\%)',
                               'Town Tax Revenue',
                               'Town Tax Gap',
                               'Town Expenditure',
                               'Nonpartisan Mayor',
                               'Right-Wing Mayor',
                               'Left-Wing Mayor',
                               'Populist Mayor',
                               'Treated (Noncompliance)',
                               'Treated (Enforcement)'),
          font.size = 'footnotesize',
          keep.stat = c('n'),
          add.lines = list(c("Controls:",rep(c('No',"Yes"),3)),
                           c("FE:",rep("Year",6))),
          omit = 'Constant',
          star.cutoffs = c(0.05, 0.01, 0.001),
          no.space = T,
          label = 'natural_drives_full',
          out = 'Output/natural_exp_don_drives_full.tex')

## Figure A7: Types of NGO Public Services ---------------------------------------------------

out = c("amount_social_welfare","amount_social_elderly","amount_social_family",
        "amount_social_health",'amount_social_other')
out_lb = c('Welfare','Elderly \n Care','Family \n Service','Healthcare',
           'Other')

res = data.frame(out = rep(out,2),out_lb=rep(out_lb,2),
                 type=rep(c('Noncompliance \n Disclosure','Enforcement \n Disclosure'),
                          each=length(out)),
                 b=NA,se=NA,t=NA)
res$out_lb = factor(res$out_lb,levels= c('Welfare','Elderly \n Care','Family \n Service','Healthcare',
                                         'Other'))
res$type = factor(res$type,levels=c('Noncompliance \n Disclosure','Enforcement \n Disclosure'))

for(i in 1:nrow(res)){
  print(i)
  if(res$type[i]=='Noncompliance \n Disclosure'){
    f = paste0(res$out[i],' ~ treat_pub + 
            scale(vat_pub_lasttaxyear_pre1) + scale(vat_rec_lasttaxyear_pre1) + 
            scale(taxoff_dist) + taxoff_type +
            scale(pop) + scale(HHI_gen) +  
            scale(edu11_high) + scale(edu11_low) +
            scale(mun_revenue_tax) + 
            scale(mun_tax_claims) + scale(mun_exp_t) + 
            sin_civ + sin_right + sin_left + sin_popul + 
            (1|year) + (1 | mun_clean)')}
  else{
    f = paste0(res$out[i],' ~ treat_rec + 
   scale(vat_pub_lasttaxyear_pre1) +scale(vat_rec_lasttaxyear_pre1) + 
            scale(taxoff_dist) + taxoff_type +
            scale(pop) + scale(HHI_gen) +  
            scale(edu11_high) + scale(edu11_low) +
            scale(mun_revenue_tax) + 
            scale(mun_tax_claims) + scale(mun_exp_t) + 
            sin_civ + sin_right + sin_left + sin_popul + 
            (1|year) + (1 | mun_clean)')}
  
  m = lmer(as.formula(f),data)
  n = nrow(summary(m)$coefficients)
  res$b[i] = summary(m)$coefficients[2,1]
  res$se[i] = summary(m)$coefficients[2,2]
  res$t[i] = summary(m)$coefficients[2,3]
  
}

p=ggplot(res, aes(y=out_lb,x=b,color=type)) +
  geom_errorbar(aes(xmax=b+1.96*se,xmin=b-1.96*se),width=0.1,
                position = position_dodge(width=0.2))+
  geom_point(position = position_dodge(width=0.2))+
  geom_vline(xintercept=0,linetype='dashed')+
  coord_flip()+
  theme_classic() +
  scale_color_manual(values = c("Noncompliance \n Disclosure" = "Red", 
                                "Enforcement \n Disclosure" = "Blue"), name = "") +
  guides(fill = guide_legend(title = " ")) +
  labs(y=' ',x='SD Change in Donations')+
  theme(text=element_text(size=20),
        legend.position = 'bottom')

pdf("Output/donation_types_social.pdf", 15, 9.5) 
print(p)
dev.off()   

## Figure A8: Firm Types & Local News -------------------------------------------------

lb = c('Firm Sales \n Volume','Number of \n Employees','Costumer-Facing \n Firms',
       'Town with \n Newspaper')

m1 =  lmer(scale(amount) ~ treat_pub*scale(treat_pub_sales) + 
             scale(vat_pub_lasttaxyear_pre1) +scale(vat_rec_lasttaxyear_pre1) + 
             scale(taxoff_dist) + taxoff_type +
             scale(pop) + scale(HHI_gen) +  
             scale(edu11_high) + scale(edu11_low) +
             scale(unemp_p) + 
             scale(mun_revenue_tax) + 
             scale(mun_tax_claims) + scale(mun_exp_t) + 
             sin_civ + sin_right + sin_left + sin_popul + 
             (1|year) + (1 | mun_clean), 
           data)

m2 = lmer(scale(amount) ~ treat_rec*scale(treat_rec_sales) + 
            scale(vat_pub_lasttaxyear_pre1) +scale(vat_rec_lasttaxyear_pre1) + 
            scale(taxoff_dist) + taxoff_type +
            scale(pop) + scale(HHI_gen) +  
            scale(edu11_high) + scale(edu11_low) +
            scale(unemp_p) + 
            scale(mun_revenue_tax) + 
            scale(mun_tax_claims) + scale(mun_exp_t) + 
            sin_civ + sin_right + sin_left + sin_popul + 
            (1|year) + (1 | mun_clean), 
          data)

m3 =  lmer(scale(amount) ~ treat_pub*scale(treat_pub_employee) + 
             scale(vat_pub_lasttaxyear_pre1) +scale(vat_rec_lasttaxyear_pre1) + 
             scale(taxoff_dist) + taxoff_type +
             scale(pop) + scale(HHI_gen) +  
             scale(edu11_high) + scale(edu11_low) +
             scale(unemp_p) + 
             scale(mun_revenue_tax) + 
             scale(mun_tax_claims) + scale(mun_exp_t) + 
             sin_civ + sin_right + sin_left + sin_popul + 
             (1|year) + (1 | mun_clean), 
           data)

m4 = lmer(scale(amount) ~ treat_rec*scale(treat_rec_employee) + 
            scale(vat_pub_lasttaxyear_pre1) +scale(vat_rec_lasttaxyear_pre1) + 
            scale(taxoff_dist) + taxoff_type +
            scale(pop) + scale(HHI_gen) +  
            scale(edu11_high) + scale(edu11_low) +
            scale(unemp_p) + 
            scale(mun_revenue_tax) + 
            scale(mun_tax_claims) + scale(mun_exp_t) + 
            sin_civ + sin_right + sin_left + sin_popul + 
            (1|year) + (1 | mun_clean), 
          data)

m5 =  lmer(scale(amount) ~ treat_pub*scale(treat_pub_cost_facing) + 
             scale(vat_pub_lasttaxyear_pre1) +scale(vat_rec_lasttaxyear_pre1) + 
             scale(taxoff_dist) + taxoff_type +
             scale(pop) + scale(HHI_gen) +  
             scale(edu11_high) + scale(edu11_low) +
             scale(unemp_p) + 
             scale(mun_revenue_tax) + 
             scale(mun_tax_claims) + scale(mun_exp_t) + 
             sin_civ + sin_right + sin_left + sin_popul + 
             (1|year) + (1 | mun_clean), 
           data)

m6 = lmer(scale(amount) ~ treat_rec*scale(treat_rec_cost_facing) + 
            scale(vat_pub_lasttaxyear_pre1) +scale(vat_rec_lasttaxyear_pre1) + 
            scale(taxoff_dist) + taxoff_type +
            scale(pop) + scale(HHI_gen) +  
            scale(edu11_high) + scale(edu11_low) +
            scale(unemp_p) + 
            scale(mun_revenue_tax) + 
            scale(mun_tax_claims) + scale(mun_exp_t) + 
            sin_civ + sin_right + sin_left + sin_popul + 
            (1|year) + (1 | mun_clean), 
          data)

m7 =  lmer(scale(amount) ~ treat_pub*news_mun + 
             scale(vat_pub_lasttaxyear_pre1) +scale(vat_rec_lasttaxyear_pre1) + 
             scale(taxoff_dist) + taxoff_type +
             scale(pop) + scale(HHI_gen) +  
             scale(edu11_high) + scale(edu11_low) +
             scale(unemp_p) + 
             scale(mun_revenue_tax) + 
             scale(mun_tax_claims) + scale(mun_exp_t) + 
             sin_civ + sin_right + sin_left + sin_popul + 
             (1|year) + (1 | mun_clean), 
           data)

m8 = lmer(scale(amount) ~ treat_rec*news_mun + 
            scale(vat_pub_lasttaxyear_pre1) +scale(vat_rec_lasttaxyear_pre1) + 
            scale(taxoff_dist) + taxoff_type +
            scale(pop) + scale(HHI_gen) +  
            scale(edu11_high) + scale(edu11_low) +
            scale(unemp_p) + 
            scale(mun_revenue_tax) + 
            scale(mun_tax_claims) + scale(mun_exp_t) + 
            sin_civ + sin_right + sin_left + sin_popul + 
            (1|year) + (1 | mun_clean), 
          data)

n = which(grepl(':',rownames(summary(m1)$coefficients)))
res = data.frame(cov = rep(lb,2),
                 b=c(summary(m1)$coefficients[n,1],
                     summary(m2)$coefficients[n,1],
                     summary(m3)$coefficients[n,1],
                     summary(m4)$coefficients[n,1],
                     summary(m5)$coefficients[n,1],
                     summary(m6)$coefficients[n,1],
                     summary(m7)$coefficients[n,1],
                     summary(m8)$coefficients[n,1]),
                 se=c(summary(m1)$coefficients[n,2],
                      summary(m2)$coefficients[n,2],
                      summary(m3)$coefficients[n,2],
                      summary(m4)$coefficients[n,2],
                      summary(m5)$coefficients[n,2],
                      summary(m6)$coefficients[n,2],
                      summary(m7)$coefficients[n,2],
                      summary(m8)$coefficients[n,2]),
                 type = rep(c('Noncompliance \n Disclosure','Enforcement \n Disclosure'),
                            each=length(lb)))
res$cov = factor(res$cov,levels=lb)
res$type = factor(res$type,levels=c('Noncompliance \n Disclosure','Enforcement \n Disclosure'))

p=ggplot(res, aes(x = b, y = cov, color = type)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width=0.2)) +
  geom_errorbarh(aes(xmin = b-1.96*se, xmax = b+1.96*se), height = 0.1,position = position_dodge(width=0.2)) +
  theme_minimal() +
  coord_flip() +
  labs(x = "Standardized Logit Coefficient",y='', title = "")+
  scale_color_manual(values = c("Noncompliance \n Disclosure" = "Red", 
                                "Enforcement \n Disclosure" = "Blue"), name = "") +
  theme(text = element_text(size=20),
        legend.position='bottom')

pdf("Output/nat_exp_moderators.pdf", 15, 9.5) 
print(p)
dev.off()   

## Table A8: Employees tax donations -------------------------------------------------

m1 = lmer(scale(amount) ~ treat_pub_emp + 
            scale(vat_pub_lasttaxyear_pre1) +scale(vat_rec_lasttaxyear_pre1) + 
            scale(taxoff_dist) + taxoff_type +
            scale(pop) + scale(HHI_gen) +  
            scale(edu11_high) + scale(edu11_low) +
            scale(mun_revenue_tax) + 
            scale(mun_tax_claims) + scale(mun_exp_t) + 
            sin_civ + sin_right + sin_left + sin_popul + 
            (1|year) + (1 | mun_clean), 
          data)

m2 = lmer(scale(amount) ~ treat_pub_emp*scale(treat_pub_n_emp)+
            scale(vat_pub_lasttaxyear_pre1) +scale(vat_rec_lasttaxyear_pre1) + 
            scale(taxoff_dist) + taxoff_type +
            scale(pop) + scale(HHI_gen) +  
            scale(edu11_high) + scale(edu11_low) +
            scale(mun_revenue_tax) + 
            scale(mun_tax_claims) + scale(mun_exp_t) + 
            sin_civ + sin_right + sin_left + sin_popul + 
            (1|year) + (1 | mun_clean), 
          data)

m3 = lmer(scale(amount) ~ treat_rec_emp  +
            scale(vat_pub_lasttaxyear_pre1) +scale(vat_rec_lasttaxyear_pre1) + 
            scale(taxoff_dist) + taxoff_type +
            scale(pop) + scale(HHI_gen) +  
            scale(edu11_high) + scale(edu11_low) +
            scale(unemp_p) + 
            scale(mun_revenue_tax) + 
            scale(mun_tax_claims) + scale(mun_exp_t) + 
            sin_civ + sin_right + sin_left + sin_popul + 
            (1|year) + (1 | mun_clean), 
          data)

m4 = lmer(scale(amount) ~ treat_rec_emp*scale(treat_rec_n_emp) + 
            scale(vat_pub_lasttaxyear_pre1) +scale(vat_rec_lasttaxyear_pre1) + 
            scale(taxoff_dist) + taxoff_type +
            scale(pop) + scale(HHI_gen) +  
            scale(edu11_high) + scale(edu11_low) +
            scale(mun_revenue_tax) + 
            scale(mun_tax_claims) + scale(mun_exp_t) + 
            sin_civ + sin_right + sin_left + sin_popul + 
            (1|year) + (1 | mun_clean), 
          data)

stargazer(m1,m2,m3,m4,
          title = 'VAT Disclosures and Tax Donations (Employees only)',
          dep.var.caption = c(' '),
          dep.var.labels = c('Tax Donations'),
          keep = c('treat_pub','scale(treat_pub_n)','treat_pub:scale(treat_pub_n)',
                   'treat_rec','scale(treat_rec_n)','treat_rec:scale(treat_rec_n)'),
          order = c('treat_pub','scale(treat_pub_n)','treat_pub:scale(treat_pub_n)',
                    'treat_rec','scale(treat_rec_n)','treat_rec:scale(treat_rec_n)'),
          covariate.labels = c('Treated (Noncompliance)',
                               'Intensity (\\# NC Disclosures)',
                               'Intensity (NC)*Treated (NC)',
                               'Treated (Enforcement)',
                               'Intensity (\\# Enf Disclosures)',
                               'Intensity (Enf)*Treated (Enf)'),
          keep.stat = c('n'),
          add.lines = list(c("Controls:",rep("Yes",4)),
                           c("FE:",rep("Year/Town",4))),
          omit = 'Constant',
          star.cutoffs = c(0.05, 0.01, 0.001),
          no.space = T,
          label = 'natural_main_employees',
          out = 'Output/natural_exp_tax_don_employees.tex')

## Table A9: Mun Size ----------------------------------------------------------

data$treat_pub_p2 = data$treat_pub_n/log(data$pop)
data$treat_rec_p2 = data$treat_rec_n/log(data$pop)

data$treat_pub_p3 = -log(data$treat_pub_n/data$pop)
data$treat_rec_p3 = -log(data$treat_rec_n/data$pop)

data$treat_pub_p_bi = as.numeric(data$treat_pub_p>mean(
  data[data$treat_pub_p>0,]$treat_pub_p,na.rm=T))
data$treat_rec_p_bi = as.numeric(data$treat_rec_p>mean(
  data[data$treat_rec_p>0,]$treat_rec_p,na.rm=T))

m1 = lmer(scale(amount) ~ treat_pub*treat_pub_p2+ 
            scale(vat_pub_lasttaxyear_pre1) +scale(vat_rec_lasttaxyear_pre1) + 
            scale(taxoff_dist) + taxoff_type +
            log(pop) + scale(HHI_gen) +  
            scale(edu11_high) + scale(edu11_low) +
            scale(mun_revenue_tax) + 
            scale(mun_tax_claims) + scale(mun_exp_t) + 
            sin_civ + sin_right + sin_left + sin_popul + 
            (1|year) + (1 | mun_clean), 
          data)

m2 = lmer(scale(amount) ~ treat_pub*treat_pub_p_bi+ 
            scale(vat_pub_lasttaxyear_pre1) +scale(vat_rec_lasttaxyear_pre1) + 
            scale(taxoff_dist) + taxoff_type +
            log(pop) + scale(HHI_gen) +  
            scale(edu11_high) + scale(edu11_low) +
            scale(mun_revenue_tax) + 
            scale(mun_tax_claims) + scale(mun_exp_t) + 
            sin_civ + sin_right + sin_left + sin_popul + 
            (1|year) + (1 | mun_clean), 
          data)

m3 = lmer(scale(amount) ~ treat_rec*treat_rec_p2+  
            scale(vat_pub_lasttaxyear_pre1) +scale(vat_rec_lasttaxyear_pre1) + 
            scale(taxoff_dist) + taxoff_type +
            log(pop) + scale(HHI_gen) +  
            scale(edu11_high) + scale(edu11_low) +
            scale(mun_revenue_tax) + 
            scale(mun_tax_claims) + scale(mun_exp_t) + 
            sin_civ + sin_right + sin_left + sin_popul + 
            (1|year) + (1 | mun_clean), 
          data)

m4 = lmer(scale(amount) ~ treat_rec*treat_rec_p_bi+  
            scale(vat_pub_lasttaxyear_pre1) +scale(vat_rec_lasttaxyear_pre1) + 
            scale(taxoff_dist) + taxoff_type +
            log(pop) + scale(HHI_gen) +  
            scale(edu11_high) + scale(edu11_low) +
            scale(mun_revenue_tax) + 
            scale(mun_tax_claims) + scale(mun_exp_t) + 
            sin_civ + sin_right + sin_left + sin_popul + 
            (1|year) + (1 | mun_clean), 
          data)

stargazer(m1,m2,m3,m4,
          title = 'VAT Disclosures by Municipal Population and Tax Donations',
          dep.var.caption = c(''),
          dep.var.labels = c('Tax Donations'),
          keep = c('treat_pub','treat_pub_p2',
                   'treat_pub:treat_pub_p2',
                   'treat_pub_p_bi',
                   'treat_pub:treat_pub_p_bi',
                   'treat_rec','treat_rec_p2',
                   'treat_rec:treat_rec_p2',
                   'treat_rec_p_bi',
                   'treat_pub:treat_rec_p_bi'),
          order = c('treat_pub','treat_pub_p2',
                    'treat_pub:treat_pub_p2',
                    'treat_pub_p_bi',
                    'treat_pub:treat_pub_p_bi',
                    'treat_rec','treat_rec_p2',
                    'treat_rec:treat_rec_p2',
                    'treat_rec_p_bi',
                    'treat_pub:treat_rec_p_bi'),
          covariate.labels = c('Treated (Noncompliance)',
                               'Log Intensity (\\# NC/log(Pop))',
                               'Binary Intensity (\\# NC/Pop above mean)',
                               'Treated (NC)*Log Intensity (NC)',
                               'Treated (NC)*Binary Intensity (NC)',
                               'Treated (Enforcement)',
                               'Log Intensity (\\# Enf/log(Pop))',
                               'Binary Intensity (\\# Enf/Pop above mean)',
                               'Treated (Enf)*Log Intensity (Enf)',
                               'Treated (Enf)*Binary Intensity (Enf)'),
          keep.stat = c('n'),
          add.lines = list(c("Controls:",rep("Yes",2)),
                           c("FE:",rep("Year/Town",2))),
          omit = 'Constant',
          star.cutoffs = c(0.05, 0.01, 0.001),
          no.space = T,
          label = 'natural_NC_by_pop',
          out = 'Output/natural_exp_tax_don_by_pop.tex')


## Table A10: Survey Treatment and Mun Size---------------------------------------------

survey$pop_bi = as.numeric(survey$pop>100000) 
survey$pop_log = log(survey$pop)

m1 = lmer(Lottery_1 ~ treat + pop_bi +
            scale(age) + male + income + edu + pol_lr + emp_cat + rel_cat +
            public_serv_town +
            scale(unemp_p) + scale(HHI_lang) + log(mun_revenue_t) +
            (1|ip_region),
          survey)

m2 = lmer(Lottery_1 ~ treat*pop_bi +
            scale(age) + male + income + edu + pol_lr + emp_cat + rel_cat +
            public_serv_town +
            scale(unemp_p) + scale(HHI_lang) + log(mun_revenue_t) +
            (1|ip_region),
          survey)

stargazer(m1, m2,
          title = 'Does the Effect of Noncompliance Depend on Population Size?',
          dep.var.labels = c('Donation by Others in the Community'),
          keep = c('treat','pop_bi','treat:pop_bi'),
          covariate.labels = c('Treated',
                               'Population Binary (above 100k)',
                               'Treated*Population Binary (above 100k)'),
          keep.stat = c('n'),
          add.lines = list(c("Controls:",rep("Yes",2)),
                           c("FE:",rep("Region",2))),
          omit = 'Constant',
          star.cutoffs = c(0.05, 0.01, 0.001),
          no.space = T,
          label = 'survey_exp_tax_don_by_pop',
          out = 'Output/survey_exp_tax_don_by_pop.tex' )


## Table A11: Disclosures & # Drives --------------------------------------------------

m1_drives = lmer(scale(drives_n) ~ scale(vat_pub_lasttaxyear_pre1) +  scale(vat_rec_lasttaxyear_pre1) +
                   (1|year)+ (1 | mun_clean), 
                 data)

m2_drives = lmer(scale(drives_n) ~ 
                   scale(vat_pub_lasttaxyear_pre1) + scale(vat_rec_lasttaxyear_pre1) + 
                   scale(taxoff_dist) + taxoff_type +
                   scale(pop) + scale(HHI_gen) +  
                   scale(edu11_high) + scale(edu11_low) +
                   scale(unemp_p) + 
                   scale(mun_revenue_tax) + 
                   scale(mun_tax_claims) + scale(mun_exp_t) + 
                   sin_civ + sin_right + sin_left + sin_popul + 
                   (1|year)+ (1 | mun_clean), 
                 data)

stargazer(m1_drives,m2_drives,
          title = 'VAT Disclosures and Number of NGO Donation Drives',
          dep.var.caption = c(' '),
          dep.var.labels = c('NGO Donation Drives'),
          keep = c('vat_pub_lasttaxyear','vat_rec_lasttaxyear'),
          covariate.labels = c('Noncompliance (Last Year)',
                               'Enforcement (Last Year)'),
          keep.stat = c('n'),
          add.lines = list(c("Controls:",c('No',"Yes")),
                           c("FE:",rep(c("Town, Year"),2))),
          omit = 'Constant',
          star.cutoffs = c(0.05, 0.01, 0.001),
          no.space = T,
          label = 'natural_drives_number',
          out = 'Output/natural_exp_don_drives_n.tex')

## Table A12: Neighboring Disclosures Spillover Check ---------------------------------------------------

data$pub_neighbor_bi = ifelse(data$treat_pub_neighbor>0,1,0)
data$rec_neighbor_bi = ifelse(data$treat_rec_neighbor>0,1,0)

m1 = lmer(scale(amount) ~ pub_neighbor_bi +
            (1|year) + (1 | mun_clean),
          data)

m2 = lmer(scale(amount) ~ pub_neighbor_bi +
            scale(taxoff_dist) + taxoff_type +
            scale(pop) + scale(HHI_gen) +
            scale(edu11_high) + scale(edu11_low) +
            scale(mun_revenue_tax) +
            scale(mun_tax_claims) + scale(mun_exp_t) +
            sin_civ + sin_right + sin_left + sin_popul +
            (1|year) + (1 | mun_clean), 
          data)

m3 = lmer(scale(amount) ~ rec_neighbor_bi  +
            (1|year) + (1 | mun_clean), 
          data)

m4 = lmer(scale(amount) ~ rec_neighbor_bi + 
            scale(taxoff_dist) + taxoff_type +
            scale(pop) + scale(HHI_gen) +
            scale(edu11_high) + scale(edu11_low) +
            scale(mun_revenue_tax) +
            scale(mun_tax_claims) + scale(mun_exp_t) +
            sin_civ + sin_right + sin_left + sin_popul +
            (1|year) + (1 | mun_clean), 
          data)

stargazer(m1,m2,m3,m4,
          title = 'VAT Disclosures in Neighboring Towns and Tax Donations',
          dep.var.caption = c(''),
          dep.var.labels = c('Tax Donations'),
          keep = c('pub_neighbor_bi','rec_neighbor_bi'),
          order = c('pub_neighbor_bi','rec_neighbor_bi'),
          covariate.labels = c('Noncompliance in Neighboring Towns',
                               'Enforcement in Neighboring Towns'),
          keep.stat = c('n'),
          add.lines = list(c("Controls:",rep(c("No","Yes"),2)),
                           c("FE:",rep("Year/Town",4))),
          omit = 'Constant',
          star.cutoffs = c(0.05, 0.01, 0.001),
          no.space = T,
          label = 'natural_main_neighbor_control',
          out = 'Output/natural_exp_tax_don_neighbor_control.tex')

## Table A13: Main Nat Exp with controls for neighbors last year -------------------------------

m1 = lmer(scale(amount) ~ treat_pub + 
            neigh_pub_lastyear +
            scale(vat_pub_lasttaxyear_pre1) +scale(vat_rec_lasttaxyear_pre1) + 
            scale(taxoff_dist) + taxoff_type +
            scale(pop) + scale(HHI_gen) +  
            scale(edu11_high) + scale(edu11_low) +
            scale(mun_revenue_tax) + 
            scale(mun_tax_claims) + scale(mun_exp_t) + 
            sin_civ + sin_right + sin_left + sin_popul + 
            (1|year) + (1 | mun_clean), 
          data)

m2 = lmer(scale(amount) ~ treat_pub*scale(treat_pub_n)+
            neigh_pub_lastyear +
            scale(vat_pub_lasttaxyear_pre1) +scale(vat_rec_lasttaxyear_pre1) + 
            scale(taxoff_dist) + taxoff_type +
            scale(pop) + scale(HHI_gen) +  
            scale(edu11_high) + scale(edu11_low) +
            scale(mun_revenue_tax) + 
            scale(mun_tax_claims) + scale(mun_exp_t) + 
            sin_civ + sin_right + sin_left + sin_popul + 
            (1|year) + (1 | mun_clean), 
          data)

m3 = lmer(scale(amount) ~ treat_rec  +
            neigh_rec_lastyear +
            scale(vat_pub_lasttaxyear_pre1) +scale(vat_rec_lasttaxyear_pre1) + 
            scale(taxoff_dist) + taxoff_type +
            scale(pop) + scale(HHI_gen) +  
            scale(edu11_high) + scale(edu11_low) +
            scale(unemp_p) + 
            scale(mun_revenue_tax) + 
            scale(mun_tax_claims) + scale(mun_exp_t) + 
            sin_civ + sin_right + sin_left + sin_popul + 
            (1|year) + (1 | mun_clean), 
          data)

m4 = lmer(scale(amount) ~ treat_rec*scale(treat_rec_n) + 
            neigh_rec_lastyear +
            scale(vat_pub_lasttaxyear_pre1) +scale(vat_rec_lasttaxyear_pre1) + 
            scale(taxoff_dist) + taxoff_type +
            scale(pop) + scale(HHI_gen) +  
            scale(edu11_high) + scale(edu11_low) +
            scale(mun_revenue_tax) + 
            scale(mun_tax_claims) + scale(mun_exp_t) + 
            sin_civ + sin_right + sin_left + sin_popul + 
            (1|year) + (1 | mun_clean), 
          data)

stargazer(m1,m2,m3,m4,
          title = 'VAT Disclosures and Tax Donations, controlling for neighbors (year prior)',
          dep.var.caption = c(''),
          dep.var.labels = c('Tax Donations'),
          keep = c('treat_pub','scale(treat_pub_n)','treat_pub:scale(treat_pub_n)',
                   'treat_rec','scale(treat_rec_n)','treat_rec:scale(treat_rec_n)'),
          order = c('treat_pub','scale(treat_pub_n)','treat_pub:scale(treat_pub_n)',
                    'treat_rec','scale(treat_rec_n)','treat_rec:scale(treat_rec_n)'),
          covariate.labels = c('Treated (Noncompliance)',
                               'Intensity (\\# NC Disclosures)',
                               'Intensity (NC)*Treated (NC)',
                               'Treated (Enforcement)',
                               'Intensity (\\# Enf Disclosures)',
                               'Intensity (Enf)*Treated (Enf)'),
          keep.stat = c('n'),
          add.lines = list(c("Controls:",rep("Yes",4)),
                           c("FE:",rep("Year/Town",4))),
          omit = 'Constant',
          star.cutoffs = c(0.05, 0.01, 0.001),
          no.space = T,
          label = 'natural_main_control_neighbors_prior',
          out = 'Output/natural_exp_tax_don_control_neighbors_prior.tex')

## Figure A9: Simulation of SUTVA interference ---------------------------------------

m0 = lmer(amount ~ treat_pub + 
            scale(vat_pub_lasttaxyear_pre1) +scale(vat_rec_lasttaxyear_pre1) + 
            scale(taxoff_dist) + taxoff_type +
            scale(pop) + scale(HHI_gen) +  
            scale(edu11_high) + scale(edu11_low) +
            scale(mun_revenue_tax) + 
            scale(mun_tax_claims) + scale(mun_exp_t) + 
            sin_civ + sin_right + sin_left + sin_popul + 
            (1|year) + (1 | mun_clean), 
          data)

df = m0@frame

colnames(df) = c("amount","treat_pub","vat_pub_lasttaxyear_pre1", 
                 "vat_rec_lasttaxyear_pre1","taxoff_dist","taxoff_type",                    
                 "pop","HHI_gen","edu11_high","edu11_low",               
                 "mun_revenue_tax","mun_tax_claims",          
                 "mun_exp_t","sin_civ","sin_right","sin_left",                       
                 "sin_popul","year","mun_clean")

# ATE
T_dir = summary(m0)$coefficients[2,1]
# indirect effect due to interference
T_ind_values = seq(-2*T_dir, 2*T_dir, length.out = 5)  
# interference proportion of control
p_interference <- seq(0, 1, length.out = 50)
# interference overlap with treatment
p_overlap = 0

# Remove estimated ATE from real data
df$amount = df$amount - T_dir*df$treat_pub

# check null result

m <- lmer(amount ~ treat_pub + 
            scale(vat_pub_lasttaxyear_pre1) +scale(vat_rec_lasttaxyear_pre1) + 
            scale(taxoff_dist) + taxoff_type +
            scale(pop) + scale(HHI_gen) +  
            scale(edu11_high) + scale(edu11_low) +
            scale(mun_revenue_tax) + 
            scale(mun_tax_claims) + scale(mun_exp_t) + 
            sin_civ + sin_right + sin_left + sin_popul + 
            (1|year) + (1 | mun_clean), 
          df)


# Create a grid of p and T_ind values
param_grid <- expand.grid(p = p_interference, T_ind = T_ind_values)

# Initialize a vector to store the results
results <- data.frame(
  p = param_grid$p,
  T_ind = param_grid$T_ind,
  observed_effect = NA,
  observed_se = NA
)


for (i in 1:nrow(param_grid)) {
  
  print(i/nrow(param_grid))
  
  df1 = df
  N = nrow(df1)
  p <- param_grid$p[i]
  T_ind <- param_grid$T_ind[i]
  
  df1$interfered = ifelse(df1$treat_pub==1, rbinom(N, 1, p*p_overlap),
                          rbinom(N, 1, p))
  
  df1$amount = df1$amount + df1$interfered*rnorm(N, mean = T_ind, sd = abs(T_ind*0.2))
  
  
  m <- lmer(amount ~ treat_pub + 
              scale(vat_pub_lasttaxyear_pre1) +scale(vat_rec_lasttaxyear_pre1) + 
              scale(taxoff_dist) + taxoff_type +
              scale(pop) + scale(HHI_gen) +  
              scale(edu11_high) + scale(edu11_low) +
              scale(mun_revenue_tax) + 
              scale(mun_tax_claims) + scale(mun_exp_t) + 
              sin_civ + sin_right + sin_left + sin_popul + 
              (1|year) + (1 | mun_clean), 
            df1)
  
  results$observed_effect[i] <- summary(m)$coefficients[2,1]
  results$observed_se[i] <- summary(m)$coefficients[2,2]
}



# Run simulations

estimated_ATE = T_dir

p = ggplot(results, aes(x = p, y = observed_effect, color = (T_ind))) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin = observed_effect - observed_se, 
                    ymax = observed_effect + observed_se), width = 0.01) +
  geom_hline(aes(yintercept = estimated_ATE), linetype = "dotted", color = "red") +
  geom_text(aes(x = .1, y = estimated_ATE, label = "Estimated Effect"), vjust = -1, color = "red") +
  scale_x_continuous(labels = scales::percent) +
  theme_minimal(base_size = 20) +
  labs(
    title = "",
    x = "Proportion of Control Exposed to Indirect Treatment",
    y = "Observed Direct Effect",
    color = "Indirect Treatment Effect"
  )


pdf("Output/sutva_simulation.pdf", 15, 9.5) 
print(p)
dev.off()   

# Simulated Thresholds where model recovers similar effect (within 1 SE of estimated effect)

# with assumed negative spillover 2x
min(results[results$observed_effect+results$observed_se>=estimated_ATE &
          results$T_ind==unique(results$T_ind)[1],]$p)

# with assumed negative spillover 1x
min(results[results$observed_effect+results$observed_se>=estimated_ATE &
              results$T_ind==unique(results$T_ind)[2],]$p)

## Figure A10: Covariate Balance: Tax Donations ----------------------------------------

m1 = glmer(treat_pub ~ scale(vat_pub_lasttaxyear_pre1) +scale(vat_rec_lasttaxyear_pre1) + 
             scale(taxoff_dist) + taxoff_type +
             scale(pop) + scale(HHI_gen) +  
             scale(edu11_high) + scale(edu11_low) +
             scale(unemp_p) + 
             scale(mun_revenue_tax) + 
             scale(mun_tax_claims) + scale(mun_exp_t) + 
             sin_civ + sin_right + sin_left + sin_popul + 
             (1|year) + (1 | mun_clean), family='binomial', data)

m2 = glmer(treat_rec ~ scale(vat_pub_lasttaxyear_pre1) +scale(vat_rec_lasttaxyear_pre1) + 
             scale(taxoff_dist) + taxoff_type +
             scale(pop) + scale(HHI_gen) +  
             scale(edu11_high) + scale(edu11_low) +
             scale(unemp_p) + 
             scale(mun_revenue_tax) + 
             scale(mun_tax_claims) + scale(mun_exp_t) + 
             sin_civ + sin_right + sin_left + sin_popul + 
             (1|year) + (1 | mun_clean), family='binomial', data)

covs_lab = c('Noncompliance Disclosures (Year Prior)','Enforcement Disclosures (Year Prior)',
             'Tax Office Distance','Tax Office: Branch','Tax Office: Contact Point'
             ,'Population',
             'Diversity (HHI)','Share Degree-Holders','Share No Diploma','Unemployment',
             'Municipal Tax Revenue','Municipal Tax Claims','Municipal Expenditure',
             'Civic Mayor','Right-wing Mayor','Left-wing Mayor','Populist Mayor')

bal = data.frame(cov = rep(covs_lab,2),
                 b=c(summary(m1)$coefficients[2:(length(covs_lab)+1),1],
                     summary(m2)$coefficients[2:(length(covs_lab)+1),1]),
                 se=c(summary(m1)$coefficients[2:(length(covs_lab)+1),2],
                      summary(m2)$coefficients[2:(length(covs_lab)+1),2]),
                 size=c(summary(m1)$coefficients[2:(length(covs_lab)+1),3],
                        summary(m2)$coefficients[2:(length(covs_lab)+1),3]),
                 type = rep(c('Noncompliance \n Disclosure','Enforcement \n Disclosure'),each=length(covs_lab)))
bal$size = ifelse(abs(bal$size)<2,0,1)
bal$cov = factor(bal$cov,levels=covs_lab)
bal$type = factor(bal$type,levels=c('Noncompliance \n Disclosure','Enforcement \n Disclosure'))

p=ggplot(bal, aes(x = b, y = cov, color = type)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width=0.2)) +
  geom_errorbarh(aes(xmin = b-1.96*se, xmax = b+1.96*se), height = 0.1,position = position_dodge(width=0.2)) +
  theme_minimal() +
  labs(x = "Standardized Logit Coefficient",y='', title = "")+
  scale_color_manual(values = c("Noncompliance \n Disclosure" = "Red", 
                                "Enforcement \n Disclosure" = "Blue"), name = "") +
  theme(text = element_text(size=20),
        legend.position='bottom')

pdf("Output/covariate_balance.pdf", 15, 9.5) 
print(p)
dev.off()   


## Figure A11: Covariate Balance (Drives) ----------------------------------------------

m1 = glmer(treat_pub ~ scale(vat_pub_lasttaxyear_pre1) +
             scale(vat_rec_lasttaxyear_pre1) + 
             scale(taxoff_dist) + taxoff_type +
             scale(pop) + scale(HHI_gen) +  
             scale(edu11_high) + scale(edu11_low) +
             scale(unemp_p) + 
             scale(mun_revenue_tax) + 
             scale(mun_tax_claims) + scale(mun_exp_t) + 
             sin_civ + sin_right + sin_left + sin_popul + 
             (1|year) + (1 | mun_clean), family='binomial', drives)

m2 = glmer(treat_rec ~ scale(vat_pub_lasttaxyear_pre1) +
             scale(vat_rec_lasttaxyear_pre1) + 
             scale(taxoff_dist) + taxoff_type +
             scale(pop) + scale(HHI_gen) +  
             scale(edu11_high) + scale(edu11_low) +
             scale(unemp_p) + 
             scale(mun_revenue_tax) + 
             scale(mun_tax_claims) + scale(mun_exp_t) + 
             sin_civ + sin_right + sin_left + sin_popul + 
             (1|year) + (1 | mun_clean), family='binomial', drives)

covs_lab = c('Noncompliance Disclosures (Year Prior)','Enforcement Disclosures (Year Prior)',
             'Tax Office Distance','Tax Office: Branch','Tax Office: Contact Point'
             ,'Population',
             'Diversity (HHI)','Share Degree-Holders','Share No Diploma','Unemployment',
             'Municipal Tax Revenue','Municipal Tax Claims','Municipal Expenditure',
             'Civic Mayor','Right-wing Mayor','Left-wing Mayor','Populist Mayor')

bal = data.frame(cov = rep(covs_lab,2),
                 b=c(summary(m1)$coefficients[2:(length(covs_lab)+1),1],
                     summary(m2)$coefficients[2:(length(covs_lab)+1),1]),
                 se=c(summary(m1)$coefficients[2:(length(covs_lab)+1),2],
                      summary(m2)$coefficients[2:(length(covs_lab)+1),2]),
                 size=c(summary(m1)$coefficients[2:(length(covs_lab)+1),3],
                        summary(m2)$coefficients[2:(length(covs_lab)+1),3]),
                 type = rep(c('Noncompliance \n Disclosure','Enforcement \n Disclosure'),each=length(covs_lab)))
bal$size = ifelse(abs(bal$size)<2,0,1)
bal$cov = factor(bal$cov,levels=covs_lab)
bal$type = factor(bal$type,levels=c('Noncompliance \n Disclosure','Enforcement \n Disclosure'))
bal = bal[bal$se<50,]

p=ggplot(bal, aes(x = b, y = cov, color = type)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width=0.2)) +
  geom_errorbarh(aes(xmin = b-1.96*se, xmax = b+1.96*se), height = 0.1,position = position_dodge(width=0.2)) +
  theme_minimal() +
  labs(x = "Standardized Logit Coefficient",y='', title = "")+
  scale_color_manual(values = c("Noncompliance \n Disclosure" = "Red", 
                                "Enforcement \n Disclosure" = "Blue"), name = "") +
  theme(text = element_text(size=20),
        legend.position='bottom')

pdf("Output/covariate_balance_drives.pdf", 15, 9.5) 
print(p)
dev.off()   


## Figure A12: Treatment Window ------------------------------------------------------------

res = data.frame(k = numeric(), b = numeric(), se = numeric(), 
                 treat = character(), type =character())

n = 30
s = 15
f = 165
j = 1 # row counter

for(i in 1:n){
  
  k = seq(s,f,by=(f-s)/n)[i] # treatment window
  
  month = 0 # if April 30th, keep 0, if March 31st, make 30
  c_length = 240/k # ratio with treatment: set rest of the year to control
  
  data$treat_pub_n = sapply(1:nrow(data), function(x){
    n = as.numeric(unlist(str_split(data$dist_pub[x],';')))-month
    if(is.na(n[1])){return(NA)}
    else{if(sum(n>=0 & n<=k)>0){return(sum(n>=0 & n<=k))}
      else{return(sum(n<=-month & n>=-k*c_length))}}})
  data$treat_rec_n = sapply(1:nrow(data), function(x){
    n = as.numeric(unlist(str_split(data$dist_rec[x],';')))-month
    if(is.na(n[1])){return(NA)}
    else{if(sum(n>=0 & n<=k)>0){return(sum(n>=0 & n<=k))}
      else{return(sum(n<=-month & n>=-k*c_length))}}})
  
  data$treat_pub = sapply(1:nrow(data), function(x){
    n = as.numeric(unlist(str_split(data$dist_pub[x],';')))-month
    if(is.na(n[1])){return(NA)}
    else{ifelse(sum(n>=0 & n<=k)>0,1,
                ifelse(sum(n<=-month & n>=-k*c_length)>0,0,NA))}})
  data$treat_rec = sapply(1:nrow(data), function(x){
    n = as.numeric(unlist(str_split(data$dist_rec[x],';')))-month
    if(is.na(n[1])){return(NA)}
    else{ifelse(sum(n>=0 & n<=k)>0,1,
                ifelse(sum(n<=-month & n>=-k*c_length)>0,0,NA))}})
  
  
  m1 = lmer(scale(amount) ~ treat_pub + 
              scale(vat_pub_lasttaxyear_pre1) +scale(vat_rec_lasttaxyear_pre1) + 
              scale(taxoff_dist) + taxoff_type +
              scale(pop) + scale(HHI_gen) +  
              scale(edu11_high) + scale(edu11_low) +
              scale(mun_revenue_tax) + 
              scale(mun_tax_claims) + scale(mun_exp_t) + 
              sin_civ + sin_right + sin_left + sin_popul + 
              (1|year) + (1 | mun_clean), 
            data)
  res[j,] = c(k,summary(m1)$coefficients[2,1],summary(m1)$coefficients[2,2],
              'Noncompliance \n Disclosure','direct')
  j=j+1
  
  m2 = lmer(scale(amount) ~ treat_pub*scale(treat_pub_n)+
              scale(vat_pub_lasttaxyear_pre1) +scale(vat_rec_lasttaxyear_pre1) + 
              scale(taxoff_dist) + taxoff_type +
              scale(pop) + scale(HHI_gen) +  
              scale(edu11_high) + scale(edu11_low) +
              scale(mun_revenue_tax) + 
              scale(mun_tax_claims) + scale(mun_exp_t) + 
              sin_civ + sin_right + sin_left + sin_popul + 
              (1|year) + (1 | mun_clean), 
            data)
  l = nrow(summary(m2)$coefficients)
  res[j,] = c(k,summary(m2)$coefficients[l,1],summary(m2)$coefficients[l,2],
              'Noncompliance \n Disclosure','int')
  j=j+1
  
  m3 = lmer(scale(amount) ~ treat_rec  +
              scale(vat_pub_lasttaxyear_pre1) +scale(vat_rec_lasttaxyear_pre1) + 
              scale(taxoff_dist) + taxoff_type +
              scale(pop) + scale(HHI_gen) +  
              scale(edu11_high) + scale(edu11_low) +
              scale(unemp_p) + 
              scale(mun_revenue_tax) + 
              scale(mun_tax_claims) + scale(mun_exp_t) + 
              sin_civ + sin_right + sin_left + sin_popul + 
              (1|year) + (1 | mun_clean), 
            data)
  res[j,] = c(k,summary(m3)$coefficients[2,1],summary(m3)$coefficients[2,2],
              'Enforcement \n Disclosure','direct')
  j=j+1
  
  m4 = lmer(scale(amount) ~ treat_rec*scale(treat_rec_n) + 
              scale(vat_pub_lasttaxyear_pre1) +scale(vat_rec_lasttaxyear_pre1) + 
              scale(taxoff_dist) + taxoff_type +
              scale(pop) + scale(HHI_gen) +  
              scale(edu11_high) + scale(edu11_low) +
              scale(mun_revenue_tax) + 
              scale(mun_tax_claims) + scale(mun_exp_t) + 
              sin_civ + sin_right + sin_left + sin_popul + 
              (1|year) + (1 | mun_clean), 
            data)
  l = nrow(summary(m4)$coefficients)
  res[j,] = c(k,summary(m4)$coefficients[l,1],summary(m4)$coefficients[l,2],
              'Enforcement \n Disclosure','int')
  j=j+1
  
}

res$b = as.numeric(res$b)
res$k = as.numeric(res$k)
res$se = as.numeric(res$se)
# res$treat = c(rep(c('Noncompliance \n Disclosure','Enforcement \n Disclosure'),each=2))
res$treat = factor(res$treat,levels=c('Noncompliance \n Disclosure','Enforcement \n Disclosure'))

p=ggplot(res[res$type=='direct',], 
         aes(x = k, y = b, color = treat)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width=1)) +
  geom_errorbar(aes(ymin = b-1.96*se, ymax = b+1.96*se), 
                height = 0.1,position = position_dodge(width=1)) +
  theme_classic() +
  labs(x = "Direct Effect on Tax Donations",y='', title = "")+
  scale_color_manual(values = c("Noncompliance \n Disclosure" = "Red", 
                                "Enforcement \n Disclosure" = "Blue"), name = "") +
  theme(text = element_text(size=20),
        legend.position='bottom')

pdf("Output/nat_exp_window_direct.pdf", 15, 9.5) 
print(p)
dev.off()   


## Figure A13: Treatment Window (interaction) ------------------------------------------------------------

p=ggplot(res[res$type=='int',], 
         aes(x = k, y = b, color = treat)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width=1)) +
  geom_errorbar(aes(ymin = b-1.96*se, ymax = b+1.96*se), 
                height = 0.1,position = position_dodge(width=1)) +
  theme_classic() +
  labs(x = "Effect on Tax Donations Conditional on \\# Firms on Disclosure",y='', title = "")+
  scale_color_manual(values = c("Noncompliance \n Disclosure" = "Red", 
                                "Enforcement \n Disclosure" = "Blue"), name = "") +
  theme(text = element_text(size=20),
        legend.position='bottom')

pdf("Output/nat_exp_window_int.pdf", 15, 9.5) 
print(p)
dev.off()   

## Figure A14: Diff-in-Diff Donations -----------------------------------------------------------

df = data.frame(year = rep(c(0,-1:-5,1:5),each=2), 
                tax = rep(c('Noncompliance \n Disclosures','Enforcement \n Disclosures'),11),
                b = NA, se = NA)
df$tax =factor(df$tax,levels = c('Noncompliance \n Disclosures','Enforcement \n Disclosures'))

controls='+ scale(taxoff_dist) + taxoff_type +
            scale(pop) + scale(HHI_gen) +  
            scale(edu11_high) + scale(edu11_low) +
            scale(unemp_p) + 
            scale(mun_revenue_tax) + 
            scale(mun_tax_claims) + scale(mun_exp_t) + 
            sin_civ + sin_right + sin_left + sin_popul + 
            (1 | mun_clean) + (1 | year)'

for(i in -5:4){
  print(i)
  k=6+i
  t1 = 'vat_pub_post'
  t2 = 'vat_res_post'
  if(i<0){
    t1 = paste0('vat_pub_pre',abs(i))
    t2 = paste0('vat_res_pre',abs(i))}
  if(i>0){
    t1 = paste0('vat_pub_post',i)
    t2 = paste0('vat_res_post',i)}
  
  f1 = as.formula(paste0('scale(amount) ~ ',t1,'*scale(vat_published_dd)',controls))
  f2 = as.formula(paste0('scale(amount) ~ ',t2,'*scale(vat_restored_dd)',controls))
  
  m1 = lmer(f1,data[!is.na(data$vat_detected_event2),])
  m2 = lmer(f2,data[!is.na(data$vat_restored_event2),])
  
  n1 = which(grepl(':',rownames(summary(m1)$coefficients)))
  n2 = which(grepl(':',rownames(summary(m2)$coefficients)))
  
  df[df$year==i & df$tax=="Noncompliance \n Disclosures",
     c('b','se')] = c(summary(m1)$coefficients[n1,1],
                      summary(m1)$coefficients[n1,2])
  df[df$year==i & df$tax=='Enforcement \n Disclosures',
     c('b','se')] = c(summary(m2)$coefficients[n2,1],
                      summary(m2)$coefficients[n2,2])
  
}

df$group = ifelse(df$year>=0,'Treated','Control')
df$group = factor(df$group,levels=c('Treated','Control'))

p = ggplot(data=df[df$tax=="Noncompliance \n Disclosures",],
           aes(x=year,y=b,ymin=b-1.96*se,ymax=b+1.96*se,color=group)) +
  geom_pointrange(position = position_dodge(width = .1)) +
  geom_line(position = position_dodge(width = .1)) + 
  geom_hline(yintercept=0,linetype='dotted') +
  scale_color_manual(values=c('Treated'='red','Control'='black'),name = " ") +
  labs(title='', x ='Period', y='SD Change in Donation (Euros)') +
  scale_x_continuous(breaks=-5:5,labels=as.character(-5:5)) +
  theme_classic()+
  theme(legend.position = 'bottom',
        text = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) 

pdf("Output/dd_noncomp.pdf", 15, 9.5) 
print(p)
dev.off()   

## Figure A15: Diff-in-Diff Donations -----------------------------------------------------------

p = ggplot(data=df[df$tax=="Enforcement \n Disclosures",],
           aes(x=year,y=b,ymin=b-1.96*se,ymax=b+1.96*se,color=group)) +
  geom_pointrange(position = position_dodge(width = .1)) +
  geom_line(position = position_dodge(width = .1)) + 
  geom_hline(yintercept=0,linetype='dotted') +
  scale_color_manual(values=c('Treated'='red','Control'='black'),name = " ") +
  labs(title='', x ='Period', y='SD Change in Donation (Euros)') +
  scale_x_continuous(breaks=-5:5,labels=as.character(-5:5)) +
  theme_classic()+
  theme(legend.position = 'bottom',
        text = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) 

pdf("Output/dd_enf.pdf", 15, 9.5) 
print(p)
dev.off()   

## Figure A16: Event Study Taxes -----------------------------------------------------------

df = data.frame(year = rep(c(0,-1:-5,1:5),each=2), 
                tax = rep(c('Noncompliance \n Disclosures','Enforcement \n Disclosures'),11),
                b = NA, se = NA)
df$tax =factor(df$tax,levels = c('Noncompliance \n Disclosures','Enforcement \n Disclosures'))

controls='+ scale(taxoff_dist) + taxoff_type +
            scale(pop) + scale(HHI_gen) +  
            scale(edu11_high) + scale(edu11_low) +
            scale(unemp_p) + 
            scale(mun_revenue_tax) + scale(mun_exp_t) + 
            sin_civ + sin_right + sin_left + sin_popul + 
            (1 | mun_clean) + (1 | year)'

for(i in -5:4){
  print(i)
  k=6+i
  t1 = 'vat_pub_post'
  t2 = 'vat_res_post'
  if(i<0){
    t1 = paste0('vat_pub_pre',abs(i))
    t2 = paste0('vat_res_pre',abs(i))}
  if(i>0){
    t1 = paste0('vat_pub_post',i)
    t2 = paste0('vat_res_post',i)}
  
  f1 = as.formula(paste0('scale(mun_tax_claims) ~ ',t1,'*scale(vat_published_dd)',controls))
  f2 = as.formula(paste0('scale(mun_tax_claims) ~ ',t2,'*scale(vat_restored_dd)',controls))
  
  m1 = lmer(f1,data[!is.na(data$vat_detected_event2),])
  m2 = lmer(f2,data[!is.na(data$vat_restored_event2),])
  
  n1 = which(grepl(':',rownames(summary(m1)$coefficients)))
  n2 = which(grepl(':',rownames(summary(m2)$coefficients)))
  
  df[df$year==i & df$tax=="Noncompliance \n Disclosures",
     c('b','se')] = c(summary(m1)$coefficients[n1,1],
                      summary(m1)$coefficients[n1,2])
  df[df$year==i & df$tax=='Enforcement \n Disclosures',
     c('b','se')] = c(summary(m2)$coefficients[n2,1],
                      summary(m2)$coefficients[n2,2])
  
}

df$group = ifelse(df$year>=0,'Treated','Control')
df$group = factor(df$group,levels=c('Treated','Control'))

p = ggplot(data=df[df$tax=="Noncompliance \n Disclosures",],
           aes(x=year,y=b,ymin=b-1.96*se,ymax=b+1.96*se,color=group)) +
  geom_pointrange(position = position_dodge(width = .1)) +
  geom_line(position = position_dodge(width = .1)) + 
  geom_hline(yintercept=0,linetype='dotted') +
  scale_color_manual(values=c('Treated'='red','Control'='black'),name = " ") +
  labs(title='', x ='Period', y='SD Change in Tax Claims by Municipality') +
  scale_x_continuous(breaks=-5:5,labels=as.character(-5:5)) +
  theme_classic()+
  theme(legend.position = 'bottom',
        text = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) 

pdf("Output/dd_discl_tax_claims.pdf", 15, 9.5) 
print(p)
dev.off()   

## Figure A17: Event Study Taxes -----------------------------------------------------------

p = ggplot(data=df[df$tax=="Enforcement \n Disclosures",],
           aes(x=year,y=b,ymin=b-1.96*se,ymax=b+1.96*se,color=group)) +
  geom_pointrange(position = position_dodge(width = .1)) +
  geom_line(position = position_dodge(width = .1)) + 
  geom_hline(yintercept=0,linetype='dotted') +
  scale_color_manual(values=c('Treated'='red','Control'='black'),name = " ") +
  labs(title='', x ='Period', y='SD Change in Donation (Euros)') +
  scale_x_continuous(breaks=-5:5,labels=as.character(-5:5)) +
  theme_classic()+
  theme(legend.position = 'bottom',
        text = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) 

pdf("Output/dd_enf_tax_claims.pdf", 15, 9.5) 
print(p)
dev.off()   


## Table A14: Survey Summary Stats -------------------------------------

df = survey_full[,c('male','age','income','edu','pol_lr',
                  'emp_full','emp_self','emp_retir',
                  'public_serv_you','public_serv_fam','public_serv_town',
                  'Lottery_1','corrupt_tax','noncomp_unpun_reason_auth',
                  'behav_just_taxcheat','behav_just_benefitcheat')]

stargazer::stargazer(
  as.data.frame(df), 
  digits = 1,
  type='latex',
  title = 'Summary Statistics',
  omit.summary.stat = c('p25','p75'),
  covariate.labels = c('Male','Age','Income','Education (1-7)','Left-Right (1-10)',
                       'Employed','Self-Employed','Retired',
                       'Rely Public Serv. (You)','Rely Public Serv. (Family)','Rely Public Serv. (Town)',
                       'Donation','Tax System Corrupt?','Tax Cheats Allowed?',
                       'Tax Cheat Justified?','Welfare Fraud Justified?'),
  label = 'summary_stats_survey',
  out = 'Output/summary_stats_survey.tex')

## Table A15: Attention Check and Quality of Responses -----------------------

m1 = lm(duration~att_check_pass,
        survey_full[survey_full$duration<5000,])
m2 = lm(text_length~att_check_pass,survey_full)
m3 = lm(percent_same~att_check_pass,survey_full)
m4 = lm(dem_tgm_mismatch~att_check_pass,survey_full)

stargazer::stargazer(m1,m2,m3,m4,
                     title = 'Attention Check and Response Quality',
                     dep.var.labels = c('Survey Duration','Open-Ended Length',
                                        'Repeated Answer Patterns (%)',
                                        'Demographics Mismatch'),
                     covariate.labels = c('Passed Check'),
                     keep.stat = c('n'),
                     omit = 'Constant',
                     star.cutoffs = c(0.05, 0.01, 0.001),
                     no.space = T,
                     label = 'tab:att_check',
                     out = 'Output/attention_check.tex' )


## Figure A19: Survey Covariate Balance ---------------------------------------

covs = c('male','age','edu','income','pol_lr',
         'bel_redistr','bel_govresp','concern_corrupt','concern_poverty')
covs_lab = c('Male','Age','Education','Income','Partisanship',
             'Pro \n Redistribution','Pro Social \n Subsidies','Corruption \n Concern','Poverty \n Concern')
label_mapping <- setNames(covs_lab, covs)

survey$group <- ifelse(survey$treat == 1, "Treatment", "Control")


results <- data.frame(Covariate = character(), Label = character(), 
                      MeanDiff = numeric(), CI_Lower = numeric(), CI_Upper = numeric())
data_long <- survey %>%
  pivot_longer(cols = covs, names_to = "Covariate", values_to = "Value")

for(covariate in covs) { 
  data_cov <- filter(data_long, Covariate == covariate)
  t_test_result <- t.test(scale(Value) ~ group, data = data_cov)
  mean_diff <- t_test_result$estimate[1] - t_test_result$estimate[2] 
  ci_lower <- t_test_result$conf.int[1]
  ci_upper <- t_test_result$conf.int[2]
  cov_label <- label_mapping[covariate] 
  results <- rbind(results, data.frame(Covariate = covariate, Label = cov_label, MeanDiff = mean_diff, CI_Lower = ci_lower, CI_Upper = ci_upper))
}

results$Label <- factor(results$Label, levels = covs_lab)

p = ggplot(results, aes(x = MeanDiff, y = Label)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point() +
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.1) +
  theme_minimal() +
  labs(x = "Standardized Difference in Means (Control-Treated)",y='', title = "")+
  theme(text = element_text(size=20))

pdf("Output/covariate_balance_survey.pdf", 15, 9.5) 
print(p)
dev.off()   


##Table A16: Manipulation Checks & noncompliance beliefs -------------------------------------------

m1 = lmer(noncom_bel_vat_mun_p ~ treat +
            scale(age) + male + income + edu + pol_lr + emp_cat + rel_cat +
            (1|ip_region),
          survey)
m2 = lmer(noncom_bel_vat_mun_mg ~ treat +
            scale(age) + male + income + edu + pol_lr + emp_cat + rel_cat +
            (1|ip_region),
          survey)
m3 = lmer(noncom_bel_vat_mun_unpun ~ treat +
            scale(age) + male + income + edu + pol_lr + emp_cat + rel_cat +
            (1|ip_region),
          survey)

stargazer(m1,m2,m3,
          title = 'Treatment Effects on Noncompliance Beliefs',
          dep.var.caption = c('Consider VAT Taxpayers in your Town:'),
          dep.var.labels = c('How many evade?','How much?',
                             'How many are caught?'),
          keep = c('treat'),
          covariate.labels = c('Treated'),
          keep.stat = c('n'),
          add.lines = list(c("Controls:",rep("Yes",3)),
                           c("FE:",rep("Region",3))),
          omit = 'Constant',
          star.cutoffs = c(0.05, 0.01, 0.001),
          no.space = T,
          label = 'tab:treat_manip1',
          out = 'Output/effects_manipulation1.tex' )


##Table A17: Manipulation Checks & noncompliance beliefs -------------------------------------------

m4 = lmer(noncom_bel_p_av ~ treat +
            scale(age) + male + income + edu + pol_lr + emp_cat + rel_cat +
            (1|ip_region),
          survey)
m5 = lmer(noncom_bel_mg_av ~ treat +
            scale(age) + male + income + edu + pol_lr + emp_cat + rel_cat +
            (1|ip_region),
          survey)
m6 = lmer(noncom_bel_unpun_av ~ treat +
            scale(age) + male + income + edu + pol_lr + emp_cat + rel_cat +
            (1|ip_region),
          survey)

stargazer(m4,m5,m6,
          title = 'Treatment Effects on Noncompliance Beliefs',
          dep.var.caption = c('Consider All Taxpayers in Slovakia:'),
          dep.var.labels = c('How many evade?','How much?',
                             'How many are caught?'),
          keep = c('treat'),
          covariate.labels = c('Treated'),
          keep.stat = c('n'),
          add.lines = list(c("Controls:",rep("Yes",3)),
                           c("FE:",rep("Region",3))),
          omit = 'Constant',
          star.cutoffs = c(0.05, 0.01, 0.001),
          no.space = T,
          label = 'tab:treat_manip2',
          out = 'Output/effects_manipulation2.tex' )

## Figure A20: Treatment Reaction -------------------------------------------

survey$react_ENG = tolower(survey$react_ENG)

lang_dk = c("don't know",'do not know','no opinion','none',
            "don't understand",'interesting','neutral',"doesn't tell",
            "can't think",'unknown','not familiar',"didn't know",'did not know',
            'have no idea','have no experience', 'i have no','had no idea',
            'no comment',"can't express","don't even know","can't comment",
            'can not decide','do not have','do not understand',"can't understand",
            "haven't heard",'never heard',"don't want to say","don't comment",
            "don't recognize",'not citizens',"don't really understand",
            "can't determine",'from our village')

lang_uninterested = c("not interested",'do not care',"doesn't affect",
                      'expected','whatever','no reaction','nothing',
                      "didn't register","doesn't surprise",'reality','normal',
                      'unimportant','unsurprisingly',"wasn't surprising",
                      "don't care", 'do not care',"didn't interest",
                      "not something i'm interested in",'disinterest',
                      'not at all interested','so what','typical',
                      'alright','not very interested',"don't really care",
                      "don't recognize",'will not comment')

lang_surprise = c('surprised','shocked','surprise','shock','surprising','wow',
                  'astonishing','did not expect')

lang_negative = c('sad','fined','penalt','shame','fines','horror','terrible',
                  'do not like','not nice','astonishment','bad','harm','thieves',
                  'speechless','punished','disgust','irresponsib','rob','dismay',
                  'prosecuted','extorted','pigs','slackers','enforced','punish',
                  'pillaged','fraudsters','not honest','parasites','not responsible',
                  'not satisfied','not right','steal','unfathomable','frustrating',
                  'fraud','sanctions','criminals','delete companies','foreclosures',
                  'lying','disappointment','cancel','take away','disaster','close',
                  'negative','scandalous','penalize','close and recover')

lang_positive = c('good','ok','great',"it's fine",'fine','awesome')

lang_fake = c('fake','not true','not truth','do not fit','real data','made up','false','invented',
              'fictitious','do not believe',"lie",'do not exist','do not even exist',
              'bullshit',"don't seem real",'untrue','disagreement','not sure',
              'strange combination','nonsense','veracity','fabricated')

survey$react_cat = ifelse(grepl(paste0(lang_dk,collapse='|'),survey$react_ENG),'dk',
                        ifelse(grepl(paste0(lang_uninterested,collapse='|'),survey$react_ENG),'uninterested',
                               ifelse(grepl(paste0(lang_surprise,collapse='|'),survey$react_ENG),'surprise',
                                      ifelse(grepl(paste0(lang_negative,collapse='|'),survey$react_ENG),'negative',
                                             ifelse(grepl(paste0(lang_positive,collapse='|'),survey$react_ENG),'positive',
                                                    ifelse(grepl(paste0(lang_fake,collapse='|'),survey$react_ENG),'fake',
                                                           ifelse(is.na(survey$react_ENG),NA,'')))))))


survey$react_cat_dk = as.numeric(grepl(paste0(lang_dk,collapse='|'),survey$react_ENG))
survey$react_cat_uninterested = as.numeric(grepl(paste0(lang_uninterested,collapse='|'),survey$react_ENG))
survey$react_cat_surprise = as.numeric(grepl(paste0(lang_surprise,collapse='|'),survey$react_ENG))
survey$react_cat_negative = as.numeric(grepl(paste0(lang_negative,collapse='|'),survey$react_ENG))
survey$react_cat_positive = as.numeric(grepl(paste0(lang_positive,collapse='|'),survey$react_ENG))
survey$react_cat_fake = as.numeric(grepl(paste0(lang_fake,collapse='|'),survey$react_ENG))


cols = c("react_cat_uninterested","react_cat_surprise", "react_cat_negative",
         "react_cat_positive","react_cat_fake")
cols_name = c("Uninterested","Surprised",'Upset','Happy','Disbelief')

df <- data.frame(Column = character(), number = numeric(), label = character())

for (col in cols){
  total_values <- ifelse(col=='vat_list_know',nrow(survey),
                         nrow(survey[survey$vat_list_know==1,]))
  number <- sum(survey[[col]] == 1,na.rm=T)
  percent <- round(sum(survey[[col]] == 1,na.rm=T) / total_values,3) * 100
  label <- paste0(round(sum(survey[[col]] == 1,na.rm=T) / total_values,3) * 100,'%')
  df <- rbind(df, data.frame(Column = col,
                             number = number,
                             label=label))
}
df$Column = cols_name
df$Column = factor(df$Column, levels= c("Uninterested","Surprised",'Upset','Happy','Disbelief'))
df <- df[order(df$number, decreasing = TRUE), ]

p = ggplot(df, aes(x = reorder(Column,-number), y = number)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = label), vjust = -0.5,size=5) +
  labs(x = "", y = "Respondents", title = "") +
  theme_minimal()+
  theme(text = element_text(size=20))

pdf("Output/treat_react.pdf", 15, 9.5) 
print(p)
dev.off()   

## Figure A21: Noncompliance List Surprise -------------------------------------------

data_summary <- survey %>%
  filter(!is.na(surprise)) %>%
  group_by(surprise) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

p=ggplot(data_summary, aes(x = as.factor(surprise), y = count)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = sprintf("%.2f%%", percentage)), vjust = -0.5,size=5) +
  labs(x = "", y = "Respondents",  title = "") +
  scale_x_discrete(breaks = 1:7, 
                   labels = c('Not at all \n Surprised','Very little','Somewhat',
                              'Average','Quite','Very','Extremely \n Surprised')) +
  theme_minimal()+
  theme(text = element_text(size=20))

pdf("Output/list_surprise.pdf", 15, 9.5) 
print(p)
dev.off()  

## Figure A22: Donation Distribution -------------------------------------------

p = ggplot(survey, aes(x = Lottery_1, fill = as.factor(treat))) +
  geom_histogram(position = position_dodge(width = 8), binwidth = 10, aes(y = ..count..)) +
  scale_fill_manual(values = c("blue", "red"), labels = c('Control','Treated')) +
  labs(title = "", 
       x = "Donation \n (out of 100 EUR)", y = "Respondents", fill = "Treat Group") +
  theme_minimal()+
  theme(text = element_text(size=20),
        legend.position = 'bottom',
        legend.title = element_blank())

pdf("Output/donation_distributions.pdf", 15, 9.5) 
print(p)
dev.off()   

## Figure A23: Effect Interaction with Community Need  -------------------------------------------

# Effect Interaction Plot

m = lmer(Lottery_1 ~ treat*public_serv_town +
           age + male + income + edu + pol_lr + 
           govres_index + concern_poverty + concern_corrupt +
           (1|ip_region),
         survey)
summary(m)

# Step 1: Simulation of Predictions

df = m@frame
df$don_pred <- predict(m, newdata = m@frame) 

df = df %>% group_by(public_serv_town,treat) %>%
  summarise(don_pred_m=mean(don_pred),
            don_pred_se = sd(don_pred))

# Step 2: Plot the interaction effect
p=ggplot(df, aes(x = public_serv_town, y = don_pred_m, color = factor(treat))) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin=don_pred_m-don_pred_se,ymax=don_pred_m+don_pred_se,
                  fill = factor(treat)),alpha=0.1,linetype = 0) +
  labs(x = "Town Reliance on Public Services", y = "Predicted Donation", color = "", fill='') +
  scale_color_manual(values = c("blue", "red"), labels = c("Control", "Treatment")) +
  scale_fill_manual(values = c("blue", "red"), labels = c("Control", "Treatment")) +
  ggtitle("") +
  scale_x_continuous(breaks = 1:7, 
                     labels = paste0(c(0,20,40,50,60,80,100),'%'))+
  theme_minimal() + 
  theme(text = element_text(size=20),
        legend.position = 'bottom',
        legend.title = element_blank())

pdf("Output/effect_interaction.pdf", 15, 9.5) 
print(p)
dev.off()   

## Figure A24: Donation Beneficiary Types -------------------------------------------

out = c("lott_ben_edu","lott_ben_health", "lott_ben_cult","lott_ben_church",
        "lott_ben_busin","lott_ben_sports", "lott_ben_env","lott_ben_mun")
out_labels = c("Local \n Schools","Public \n Health", "Local \n Culture","Local \n Churches",
               "Local \n Businesses","Local \n Sports", "Local \n Environment","Local \n Government")

df <- survey %>%
  pivot_longer(cols = all_of(out)) %>%
  group_by(name, treat) %>%
  summarise(count = sum(value == 1, na.rm = TRUE),
            total = n(), 
            percent = count/total,
            se = sqrt((percent) * (1 - percent) / total)) %>%
  ungroup() %>%
  mutate(name = factor(name, levels = out, labels = out_labels)) %>%
  arrange(desc(total))

# To order outcomes by total count, first calculate total counts for each outcome
totals <- df %>%
  group_by(name) %>%
  summarise(total_count = sum(count)) %>%
  arrange(-total_count) %>%
  pull(name)

# Use fct_reorder to reorder the factor levels based on totals
df$name = factor(df$name,levels=totals)
df$treat = ifelse(df$treat==1,'Treated','Control')

p=ggplot(df, aes(x = reorder(df$name, df$total, .desc = TRUE), 
                 y = percent, fill = as.factor(treat))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = percent - se * 1.96, ymax = percent + se * 1.96),
                position = position_dodge(width = 0.8), width = 0.25) +
  scale_fill_manual(values = c("Treated" = "red", "Control" = "blue"), name = "Treatment") +
  labs(x = "", y = "Respondents (%)") +
  theme_minimal() +
  theme(text = element_text(size=20),
        legend.position = 'bottom',
        legend.title = element_blank())

pdf("Output/donation_beneficiaries.pdf", 15, 9.5) 
print(p)
dev.off()   

## Figure A25: Donation Beneficiary Link Types -------------------------------------------

out = c('lott_link_no','lott_link_work','lott_link_know','lott_link_benef')
out_labels = c("No Link \n with NGO","Work \n for NGO", "Know \n NGO","Benefit \n from NGO")

df <- survey %>%
  pivot_longer(cols = all_of(out)) %>%
  group_by(name, treat) %>%
  summarise(count = sum(value == 1, na.rm = TRUE),
            total = n(), 
            percent = count/total,
            se = sqrt((percent) * (1 - percent) / total)) %>%
  ungroup() %>%
  mutate(name = factor(name, levels = out, labels = out_labels)) %>%
  arrange(desc(total))

# To order outcomes by total count, first calculate total counts for each outcome
totals <- df %>%
  group_by(name) %>%
  summarise(total_count = sum(count)) %>%
  arrange(-total_count) %>%
  pull(name)

# Use fct_reorder to reorder the factor levels based on totals
df$name = factor(df$name,levels=totals)
df$treat = ifelse(df$treat==1,'Treated','Control')

p=ggplot(df, aes(x = reorder(df$name, df$total, .desc = TRUE), 
                 y = percent, fill = as.factor(treat))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = percent - se * 1.96, ymax = percent + se * 1.96),
                position = position_dodge(width = 0.8), width = 0.25) +
  scale_fill_manual(values = c("Treated" = "red", "Control" = "blue"), name = "Treatment") +
  labs(x = "", y = "Respondents (%)") +
  theme_minimal() +
  theme(text = element_text(size=20),
        legend.position = 'bottom',
        legend.title = element_blank())

pdf("Output/donation_beneficiaries_link.pdf", 15, 9.5) 
print(p)
dev.off()   


## Table A18: Interaction with Income and Other "Neediness" Proxies  -------------------------------------------

survey$unemployed = as.numeric(survey$emp_cat=='Unemp')
survey$retired = as.numeric(survey$emp_cat=='Retired')

m1 = lmer(Lottery_1 ~ treat*income +
            scale(age) + male + income + edu + pol_lr + emp_cat + rel_cat +
            (1|ip_region),
          survey)
m2 = lmer(Lottery_1 ~ treat*class_self +
            scale(age) + male + edu + pol_lr + emp_cat  + rel_cat +
            (1|ip_region),
          survey)
m3 = lmer(Lottery_1 ~ treat*unemployed +
            scale(age) + male + income + edu + pol_lr + rel_cat +
            (1|ip_region),
          survey)
m4 = lmer(Lottery_1 ~ treat*retired +
            scale(age) + male + income + edu + pol_lr + rel_cat +
            (1|ip_region),
          survey)

stargazer(m1,m2,m3,m4,
          title = 'Does Material Need Increase Donations',
          dep.var.labels = c('Donation'),
          keep = c('treat','treat:income','treat:class_self','treat:unemployed',
                   'treat:retired'),
          covariate.labels = c('Treated','Treated*Income','Treated*Class ID (Self)',
                               'Treated*Unemployed','Treated*Retired'),
          keep.stat = c('n'),
          add.lines = list(c("Controls:",rep("Yes",4)),
                           c("FE:",rep("Region",4))),
          omit = 'Constant',
          star.cutoffs = c(0.05, 0.01, 0.001),
          no.space = T,
          label = 'survey_int_income',
          out = 'Output/interaction_proxy_material.tex' )


## Table A19: Interaction with Normative Reasoning Proxies  -------------------------------------------

m1 = lmer(Lottery_1 ~ treat*bel_redistr +
            scale(age) + male + income + edu + pol_lr + emp_cat + rel_cat +
            (1|ip_region),
          survey)
m2 = lmer(Lottery_1 ~ treat*bel_govresp +
            scale(age) + male + income + edu + pol_lr + emp_cat  + rel_cat +
            (1|ip_region),
          survey)
m3 = lmer(Lottery_1 ~ treat*pol_lr +
            scale(age) + male + income + edu  + emp_cat + pol_lr + rel_cat +
            (1|ip_region),
          survey)
m4 = lmer(Lottery_1 ~ treat*edu +
            scale(age) + male + income + edu  + emp_cat + pol_lr + rel_cat +
            (1|ip_region),
          survey)

stargazer(m1,m2,m3,m4,
          title = 'Do Personal Norms Increase Donations',
          dep.var.labels = c('Donation'),
          keep = c('treat','treat:bel_redistr','treat:bel_govresp',
                   'treat:pol_lr','treat:edu'),
          covariate.labels = c('Treated','Treated*Redistributive Belief',
                               'Treated*Government Belief',
                               'Treated*Left-Wing','Treated*Education'),
          keep.stat = c('n'),
          add.lines = list(c("Controls:",rep("Yes",4)),
                           c("FE:",rep("Region",4))),
          omit = 'Constant',
          star.cutoffs = c(0.05, 0.01, 0.001),
          no.space = T,
          label = 'survey_int_norm',
          out = 'Output/interaction_proxy_normative.tex' )


## Figure A26: Conjoint & treatment -------------------------------------------

data_cj = readRDS('Data/data_cj.RDS')

baselines = list()
baselines$area = 'National'
baselines$dontype = 'Anonymous'
baselines$prov = 'Church'
baselines$serv = 'Local Business'

m1 = amce(chose ~ prov:Group + serv:Group + area:Group + dontype:Group, 
          data=data_cj[complete.cases(data_cj) & data_cj$att_check==2,], 
          na.ignore = T,design = 'uniform',
          cluster=TRUE, respondent.id="id",
          baselines = baselines,
          respondent.varying = "Group")

pdf("Output/conjoint.pdf", 12, 8) 
plot.amce(m1,
          xlab="Change in Pr(Chosen Beneficiary)",
          main = '',plot.display="interaction")
dev.off()   



## Table A20: Donations by Others  -------------------------------------------

m1 = lmer(lottery_others_mun ~ treat +
            scale(age) + male + income + edu + pol_lr + emp_cat + rel_cat +
            (1|ip_region),
          survey)

m2 = lmer(lottery_others_mun ~ treat*public_serv_town +
            scale(age) + male + income + edu + pol_lr + emp_cat + rel_cat +
            (1|ip_region),
          survey)

stargazer(m1,m2,
          title = 'Does Noncompliance Increase Perceived Donations by Others?',
          dep.var.labels = c('Donation by Others in the Community'),
          keep = c('treat','treat:public_serv_town'),
          covariate.labels = c('Treated','Treated*Rely on Public (Town)'),
          keep.stat = c('n'),
          add.lines = list(c("Controls:",rep("Yes",2)),
                           c("FE:",rep("Region",2))),
          omit = 'Constant',
          star.cutoffs = c(0.05, 0.01, 0.001),
          no.space = T,
          label = 'survey_don_by_others',
          out = 'Output/survey_don_by_others.tex' )


## Table A21: Mun Covariates and Donations -------------------------------------------

m1 = lmer(Lottery_1 ~ vat_pub_scaled +
            (1|ip_region),
          survey)
m2 = lmer(Lottery_1 ~ vat_pub_scaled +
            scale(age) + male + class_self + pol_lr  + rel_cat + emp_cat +
            scale(pop) + scale(unemp_p) + scale(mun_tax_claims) + scale(mun_revenue_t) +
            (1|ip_region),
          survey)
m3 = lmer(Lottery_1 ~ vat_rec_scaled +
            (1|ip_region),
          survey)
m4 = lmer(Lottery_1 ~ vat_rec_scaled +
            scale(age) + male + class_self + pol_lr  + rel_cat + emp_cat +
            scale(pop) + scale(unemp_p) + scale(mun_tax_claims) + scale(mun_revenue_t) +
            (1|ip_region),
          survey)

stargazer(m1,m2,m3,m4,
          title = 'Noncompliance Disclosures and Donation Levels',
          dep.var.labels = c('Donations'),
          keep = c('vat_pub_scaled','vat_rec_scaled'),
          covariate.labels = c('Noncompliance Disclosures','Enforcement Disclosures'),
          keep.stat = c('n'),
          add.lines = list(c("Controls:",c('','Yes','','Yes')),
                           c("FE:",rep("Region",4))),
          omit = 'Constant',
          star.cutoffs = c(0.05, 0.01, 0.001),
          no.space = T,
          label = 'real_lists',
          out = 'Output/real_list_donations.tex' )

## Table A22: EVS - Volunteering and Tax Cheating Perceptions ------------------------

evs = readRDS('Data/data_evs.RDS')

m1 = lmer(ngo_index_local ~ compcheat + 
            (1|region) + (1|country),evs)

m2 = lmer(ngo_index_local ~ compcheat + 
            compavoid + compfraud +
            justified_cheat + justified_avoid + justified_fraud + trust_ind +
            ideo + income + gender + age + edu + emp + town_size +
            (1|region) + (1|country),evs)

m3 = lmer(ngo_index_global ~ compcheat + 
            (1|region) + (1|country),evs)

m4 = lmer(ngo_index_global ~ compcheat + 
            compavoid + compfraud +
            justified_cheat + justified_avoid + justified_fraud + trust_ind +
            ideo + income + gender + age + edu + emp + town_size +
            (1|region) + (1|country),evs)


stargazer(m1,m2,m3,m4,
          title = 'Perceptions of Tax Cheating and Volunteering',
          dep.var.caption = c(''),
          dep.var.labels = c('Volunteering Index (Local NGOs)','Volunteering Index (Global NGOs)'),
          covariate.labels = c('Tax Cheating by Con.',
                               'Tax Avoidance by Con.',
                               'Benefit Fraud by Con.',
                               'Cheat Justified?',
                               'Avoid Justified?',
                               'Fraud Justified?',
                               'Trust Index',
                               'Partisanship (L-R)',
                               'Income',
                               'Male',
                               'Age',
                               'Employed',
                               'Town Size'),
          font.size = 'footnotesize',
          keep.stat = c('n'),
          add.lines = list(c("Controls:",rep(c('No',"Yes"),2)),
                           c("FE:",rep("Year",4))),
          omit = 'Constant',
          star.cutoffs = c(0.05, 0.01, 0.001),
          no.space = T,
          label = 'tab:ext_val_evs_full',
          out = 'Output/cross_country_evs_full.tex')

## Figure A27: EU Perceptions of Cheating ------------------------------------------------

evs$country = ifelse(evs$country=='Russian Federation','Russia',
                     ifelse(evs$country=='Czech Republic','Czechia',
                            ifelse(evs$country=='Northern Ireland','N. Ireland',evs$country)))
evs$compcheat <- factor(evs$compcheat, levels = 1:4, labels = c("None", "Some", "Many", "Most"))
evs$compcheat <- factor(evs$compcheat, labels = c("None", "Some", "Many", "Most"))

evs_summary <- evs %>%
  group_by(country, compcheat) %>%
  summarise(count = n()) %>%
  filter(count>0 & !is.na(compcheat)) %>%
  mutate(percentage = count / sum(count)) %>%
  ungroup()

# Calculate the ordering based on "Many" + "Most"
country_order <- evs_summary %>%
  filter(compcheat %in% c("Many", "Most")) %>%
  group_by(country) %>%
  summarise(total = sum(percentage)) %>%
  arrange(desc(total)) %>%
  pull(country)

# Calculate the average across countries of "Many" + "Most"
avg=evs_summary %>% group_by(country) %>% 
  filter(compcheat %in% c("Many", "Most")) %>%
  summarise(percentage = sum(percentage)) %>%
  summarise(sum = mean(percentage)) %>% as.numeric()

evs_summary$order <- sapply(1:nrow(evs_summary), function(x) which(evs_summary$country[x]==country_order))

# Plot
p=ggplot(evs_summary, aes(fill = compcheat, y = percentage, 
                          x = reorder(country,order))) +
  geom_bar(stat = "identity",position = "stack",) +
  geom_hline(yintercept=avg,linetype='dashed') +
  scale_fill_manual(values = c("Most" = "black", "Many" = "darkgray", "Some" = "gray", "None" = "lightgray")) +
  scale_y_continuous(labels = scales::percent_format(accuracy=1), 
                     breaks=c(0,0.25,0.5,avg,0.75,1)) + # Format y-axis as percentages
  labs(x = "", y = "", fill = "How Many Conationals Cheat of Their Taxes?") +
  annotate('text',y=avg+0.02,x=15,label='Most + Many') +
  theme_classic() +
  theme(legend.position = "bottom",
        text = element_text(size=20),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle=90,vjust = 0.2,
                                   color = ifelse(country_order == "Slovakia", "black", "lightgray")))

pdf("Output/EU_perc_eva.pdf", 15, 9.5) 
p
dev.off()   

## Figure A28: Slovakia Donations Ext Validity ---------------

dict = readRDS('Data/data_dictator.RDS')

dict$Country = factor(dict$Country)
dict$SK = ifelse(dict$Country=='Slovakia','Slovakia','Others')
dict = dict[dict$Country!='',]

p5=ggplot(dict, aes(x=reorder(Country,-mean_don,.desc = TRUE), y= mean_don, fill = SK)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("Slovakia" = "black", "Others" = "lightgray")) +
  theme_classic() +
  labs(y='Mean Donation (Dictator Game)',
       x='',       title = '') +
  theme(legend.position = "none",
        text = element_text(size=20),
        axis.text.x = element_text(angle=90))

pdf("Output/SK_ext_validity_don.pdf", 15, 9.5) 
p5
dev.off()   

## Figure A29: Slovakia In Cross-Country Comparison -----------------------------------

evs = readRDS('Data/data_evs.RDS')
issp = readRDS('Data/data_issp.RDS')
wvs = readRDS('Data/data_wvs.RDS')
vdem = readRDS('Data/vdem.rds')
cross= readRDS('Data/data_cross.RDS')
options(ggrepel.max.overlaps = Inf)

countries = c('Slovakia','Algeria','Australia','Bangladesh','Indonesia',
              'Brazil','Canada','Chile','South Africa',
              'Italy','Germany','France','Poland','Nigeria',
              'Norway','Mexico','China','Russia','South Korea','Japan',
              'Peru','Philippines','India','Sweden','UK','Great Britain',
              'Turkey','United States','United States of America','Vietnam')
countries_eu = c(countries,'Greece','Great Britain','Belgium','Russian Federation',
                 'Finland','Spain','Lithuania','Czech Republic','Denmark','Netherlands')

evs_c = evs %>% group_by(country) %>%
  summarize_all(~mean(.x, na.rm = TRUE))
evs_c$countries = ifelse(evs_c$country %in% countries_eu,evs_c$country,'')
evs_c$SK = ifelse(evs_c$country=='Slovakia','Slovakia','Others')

p1=ggplot(evs_c[evs_c$countries!='Hungary',], 
          aes(compcheat, justified_cheat, color = SK)) +
  geom_point() +
  geom_text_repel(aes(label = countries), box.padding = 0.5, point.padding = 0.3) + 
  scale_color_manual(values = c("Slovakia" = "black", "Others" = "lightgray")) +
  theme_classic() +
  labs(x='How many compatriots cheat on taxes?',
       y='How justified is it to cheat on taxes?',
       title = 'Tax Morale (EVS, 1999)') +
  theme(legend.position = "none",
        text = element_text(size=20))

issp_c = issp %>% group_by(country) %>%
  summarize_all(~mean(.x, na.rm = TRUE))
issp_c$countries = ifelse(issp_c$country %in% countries,issp_c$country,'')
issp_c$SK = ifelse(issp_c$country=='Slovakia','Slovakia','Others')

p2=ggplot(issp_c, aes(tax_enf, tax_fair, color = SK)) +
  geom_point() +
  geom_text_repel(aes(label = countries), 
                  box.padding = 0.5, point.padding = 0.3) + 
  scale_color_manual(values = c("Slovakia" = "black", "Others" = "lightgray")) +
  theme_classic() +
  labs(x='Do tax authorities make everyone pay?',
       y='How fair is the tax system?',
       title = 'Tax System (iSSP, 2017-18)') +
  theme(legend.position = "none",
        text = element_text(size=20))

wvs_c = wvs %>% group_by(country) %>%
  summarize_all(~mean(.x, na.rm = TRUE))
wvs_c$countries = ifelse(wvs_c$country %in% countries,wvs_c$country,'')
wvs_c$SK = ifelse(wvs_c$country=='Slovakia','Slovakia','Others')

p3 = ggplot(wvs_c, aes(trust_ngos_index, trust_state_index, color = SK)) +
  geom_point() +
  geom_text_repel(aes(label = countries), 
                  box.padding = 0.5, point.padding = 0.3) + 
  scale_color_manual(values = c("Slovakia" = "black", "Others" = "lightgray")) +
  theme_classic() +
  labs(x='Do you trust NGOs?',
       y='Do you trust state institutions?',
       title = 'NGO/State Trust (WVS, 2017-22)') +
  theme(legend.position = "none",
        text = element_text(size=20))

vdem_c = vdem %>%
  filter(year>2000) %>%
  dplyr::select(v2csstruc_1,v2csstruc_2,v2csstruc_3,country_name,v2csprtcpt_mean) %>%
  group_by(country_name) %>%
  summarize_all(~mean(.x, na.rm = TRUE))

vdem_c$SK = ifelse(vdem_c$country_name=='Slovakia','Slovakia','Others')
vdem_c$ngo_size = vdem_c$v2csstruc_1*1+vdem_c$v2csstruc_2*2+vdem_c$v2csstruc_3*3
vdem_c$countries = ifelse(vdem_c$country_name%in%countries,vdem_c$country_name,'')

p4 = ggplot(vdem_c, aes(ngo_size, v2csprtcpt_mean, color = SK)) +
  geom_point(size=2) +
  geom_text_repel(aes(label = countries), 
                  box.padding = 0.5, point.padding = 0.3) + 
  scale_color_manual(values = c("Slovakia" = "black", "Others" = "lightgray")) +
  theme_classic() +
  labs(x='Civil Society Structure Index (Large-Small NGOs)',
       y='Civil Society Participation Index',
       title = 'NGO Environment (V-Dem, 2000-22)') +
  theme(legend.position = "none",
        text = element_text(size=20))

pdf("Output/SK_ext_validity.pdf", 15, 9.5) 
cowplot::plot_grid(p1,p2,p3,p4)
dev.off()   

# SI Calculations --------------------------------------------

# Ecological inference (page 25)

m1 = lmer(amount ~ treat_pub*(treat_pub_n) + treat_rec*(treat_rec_n) + 
            scale(vat_pub_lasttaxyear_pre1) +scale(vat_rec_lasttaxyear_pre1) + 
            scale(taxoff_dist) + taxoff_type +
            log(pop) + scale(HHI_gen) +  
            scale(edu11_high) + scale(edu11_low) +
            scale(mun_revenue_tax) + 
            scale(mun_tax_claims) + scale(mun_exp_t) + 
            sin_civ + sin_right + sin_left + sin_popul + 
            (1|year) + (1 | mun_clean), 
          data)

# Estimated increase in donations for 1 firm disclosed & enforced simultaneously
# page 25
sum(summary(m1)$coefficients[c(2:5,22,23),1])

# Necessary firm income for effect above to be explained by firm's donation
vat = .22 # VAT tax rate in Slovakia (2025)
donation = .02 # amount donated through tax donation system
sum(summary(m1)$coefficients[c(2:5,22,23),1])/vat/donation

## Conjoint Church-Run NGOs (page 38)

baselines = list()
baselines$area = 'National'
baselines$dontype = 'Anonymous'
baselines$prov = 'Church'
baselines$serv = 'Local Business'

m1 = amce(chose ~ prov:Group + serv:Group + area:Group + dontype:Group, 
          data=data_cj[complete.cases(data_cj) & data_cj$att_check==2,], 
          na.ignore = T,design = 'uniform',
          cluster=TRUE, respondent.id="id",
          baselines = baselines,
          respondent.varying = "Group")

summary(m1)$Group1amce[2,2:4]
paste0(100*round(summary(m1)$Group1amce[2,3],2),'%')

# Runtime
time2 = Sys.time()
time1-time2