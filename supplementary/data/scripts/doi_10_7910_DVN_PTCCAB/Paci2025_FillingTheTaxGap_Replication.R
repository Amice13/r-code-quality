
# Project: Filling the Tax Gap - Replication Administrative Data
# Author: Simone Paci 
# Date: 02/28/2025

# Housecleaning ----------------------------------------------------------------

rm(list=ls())
set.seed(123456789)
options(scipen=999)
options(warn=-1)

t1 = Sys.time()

# Set Working Directory
#setwd("")

# Packages
packages <- c("lme4", "ggplot2", "tidyverse", "prediction", "RColorBrewer",
              "stringi", "stringr", "XML", "stargazer", "ggrepel", "cjoint",'sf',
              'cowplot')

# Named vector of required versions
required_versions <- c(
  lme4         = "1.1-35.5",
  ggplot2      = "3.5.1",
  tidyverse    = "2.0.0",
  dplyr        = "1.1.4",
  tidyr        = "1.3.1",
  purrr        = "1.0.2",
  prediction   = "0.3.18",
  RColorBrewer = "1.1-3",
  stringi      = "1.8.4",
  stringr      = "1.5.1",
  XML          = "3.99-0.17",
  stargazer    = "5.2.3",
  ggrepel      = "0.9.6",
  cjoint       = "2.1.1",
  sf           = "1.0-17",
  cowplot      = "1.1.3"
)

ensure_versions <- function(pkgs) {
  if (!requireNamespace("remotes", quietly = TRUE))
    install.packages("remotes")
  
  need <- vapply(names(pkgs), function(p)
    !requireNamespace(p, quietly = TRUE) ||
      as.character(utils::packageVersion(p)) != pkgs[[p]],
    logical(1))
  
  if (any(need)) {
    message("Installing / aligning versions for: ",
            paste(names(pkgs)[need], collapse = ", "))
    mapply(function(p, v)
      remotes::install_version(p, version = v, upgrade = "never",
                               repos = "https://cran.r-project.org"),
      names(pkgs)[need], pkgs[need])
  } else {
    message("All package versions already match.")
  }
}

# Install (version-control) if needed
ensure_versions(required_versions)

# Then load what you actually need
invisible(lapply(names(required_versions), require, character.only = TRUE))

# Loading data -----------------------------------------------------------------

data = readRDS('Data/municipalities_panel_rep.RDS')
drives = readRDS('Data/donation_drives_rep.RDS')
survey = readRDS('Data/survey_rep.RDS')
survey = survey[survey$att_check==2,] # see SI replication file for robustness with full data
evs = readRDS('Data/data_evs.RDS')

# Main Text Replication ------------------------------------------------------------------

## Figure 1: Map ---------------------------------------------------------------

data$vat_events_over_pop = ifelse(data$pop==0|is.na(data$vat_pub_t),0,1000*data$vat_pub_t/data$pop)
  
map <- st_read("Data/shape/obec_0.shp")
map <- st_make_valid(map)
df = data[!duplicated(data[,c('mun_clean')]),
          c('mun_clean','vat_events_over_pop')]

map$mun_clean =  tolower(iconv(map$NM4, from = "UTF-8",to = "ASCII//TRANSLIT"))
map$district_clean =  tolower(iconv(map$NM3, from = "UTF-8",to = "ASCII//TRANSLIT"))
map$mun_clean = sapply(1:nrow(map), function(x)
  ifelse(map$mun_clean[x] %in% df$mun_clean, map$mun_clean[x],
         ifelse(paste0(map$mun_clean[x],';',map$district_clean[x]) %in% df$mun_clean,
                paste0(map$mun_clean[x],';',map$district_clean[x]),map$mun_clean[x])))

map = left_join(map,df)

map <- map %>%
  mutate(
    vat_events_bin = case_when(
      vat_events_over_pop == 0 | is.na(vat_events_over_pop) ~ "0",
      vat_events_over_pop <= 1 ~ "0–1",
      vat_events_over_pop <= 2 ~ "1–2",
      vat_events_over_pop <= 5 ~ "2–5",
      vat_events_over_pop <= 10 ~ "5–10",
      vat_events_over_pop > 10 ~ ">10"
    ),
    vat_events_bin = factor(vat_events_bin, levels = c("0", "0–1", "1–2", "2–5", "5–10", ">10"))
  )

p <- ggplot(map) +
  geom_sf(aes(fill = vat_events_bin), color = NA) +
  scale_fill_brewer(palette = "Greys", name = "Disclosures/1000 inhabitants", na.translate = FALSE) +
  theme_void(base_size = 14) +
  theme(legend.position = "bottom")

pdf(file = 'Output/SK_map.pdf', width=12,height=7)
p
dev.off()

## Table 1: Types of NGOS ---------------------------------------------------------

df = data.frame(
  t = c('Social Services','Education','Culture','Local Clubs',
        'Local Business','Religious','Environmental Work','Other'),
  p = c(100*sum(data$amount_social)/sum(data$amount),
        100*sum(data$amount_edu)/sum(data$amount),
        100*sum(data$amount_culture)/sum(data$amount),
        100*sum(data$amount_clubs)/sum(data$amount),
        100*sum(data$amount_econ)/sum(data$amount),
        100*sum(data$amount_religious)/sum(data$amount),
        100*sum(data$amount_env)/sum(data$amount),
        100*sum(data$amount_other)/sum(data$amount)))

df = df %>% arrange(desc(p))
df$p = round(df$p,2)
df$p[1] = paste0(df$p[1],'%')

types = read.csv('Data/Fundraiser_type_encoding.csv')

types$amount = sapply(1:nrow(types), function(x)
  sum(drives[grepl(types$Name[x],drives$Purposes),]$amount,na.rm=T)
)

types = types %>% group_by(Recode) %>% summarize(amount=mean(amount))
types$amount = 100*types$amount/sum(types$amount)
types = types %>% arrange(desc(amount))
types$amount = round(types$amount,2)
types$amount[1] = paste0(types$amount[1],'%')
types = types[1:(nrow(types))-1,] # remove disaster relief for better visualization (just 0.4%)

types_t = cbind(df,types)
types_t = rbind(colnames(types_t),
                types_t)
colnames(types_t) = c('Tax Donations','','Fundraisers','')

sink(file='Output/donation_type_table.txt')
print(xtable::xtable(types_t),
      include.rownames=F)
sink()


## Table 2: Observational Analysis  ------------------------------------------------

m1 =  lmer(scale(amount) ~ scale(vat_pub_lasttaxyear) + 
             scale(vat_rec_lasttaxyear) +
             (1|year) + (1 | mun_clean), 
           data)

m2 =  lmer(scale(amount) ~ scale(vat_pub_lasttaxyear) + 
             scale(vat_rec_lasttaxyear) +
             scale(taxoff_dist) + taxoff_type +
             scale(pop) + scale(HHI_gen) +  
             scale(edu11_high) + 
             scale(unemp_p) + 
             scale(mun_revenue_tax) + 
             scale(mun_tax_claims) + scale(mun_exp_t) + 
             sin_civ + sin_right + sin_left + sin_popul + 
             (1|year) + (1 | mun_clean), 
           data)

pub_sd = c(round(mean(data[rownames(m1@frame),]$vat_pub_lasttaxyear,na.rm=T),0),
            round(mean(data[rownames(m2@frame),]$vat_pub_lasttaxyear,na.rm=T),0))
rec_sd = c(round(mean(data[rownames(m1@frame),]$vat_rec_lasttaxyear,na.rm=T),0),
            round(mean(data[rownames(m2@frame),]$vat_rec_lasttaxyear,na.rm=T),0))
dv_mean = c(round(mean(data[rownames(m1@frame),]$amount,na.rm=T),0),
          round(mean(data[rownames(m2@frame),]$amount,na.rm=T),0))
dv_sd = c(round(sd(data[rownames(m1@frame),]$amount,na.rm=T),0),
          round(sd(data[rownames(m2@frame),]$amount,na.rm=T),0))

dv_mean = scales::comma(dv_mean)
dv_sd = scales::comma(dv_sd)

stargazer(m1,m2,
          title = 'VAT Disclosures and Tax Donations (Observational)',
          dep.var.caption = c(''),
          dep.var.labels = c('Tax donations'),
          covariate.labels = c('Noncompliance disclosures last year',
                               'Enforcement disclosures last year',
                               'Tax office distance',
                               'Tax office: branch',
                               'Tax office: contact point',
                               'Population',
                               'HHI index',
                               'College degree (\\%)',
                               'Unemployment (\\%)',
                               'Town tax revenue',
                               'Town tax gap',
                               'Town expenditure',
                               'Nonpartisan mayor',
                               'Right-wing mayor',
                               'Left-wing mayor',
                               'Populist mayor'),
          font.size = 'footnotesize',
          keep.stat = c('n'),
          add.lines = list(c("Controls",'No','Yes'),
                           c("FE",rep("Year/Town",4)),
                           c('Outcome mean',dv_mean),
                           c('Outcome SD',dv_sd)),
          omit = 'Constant',
          star.cutoffs = c(0.1,0.05, 0.01),
          star.char = c('\\dag','*','**'),
          no.space = T,
          notes.append = FALSE, notes.align = "l",
          notes = "$^{\\dag}$p$<$.10; $^{*}$p$<$.05; $^{**}$p$<$.01",
          label = 'observational_full',
          out = 'Output/observational_tax_don_full.tex')

## Figure 2: Distribution of Disclosures ---------------------------------------

dates_pub = unique(data$vat_pub_dates)
dates_pub = sapply(1:length(dates_pub), function(x) 
  unique(unlist(strsplit(dates_pub[x],';'))))
dates_pub = unlist(strsplit(unlist(dates_pub),';'))

dates_pub = sapply(1:length(dates_pub), function(x){
  y = str_sub(dates_pub[x],7,10)
  return(as.numeric(as.Date(dates_pub[x],format = '%d.%m.%Y')-
                      as.Date(paste0('01/01/',y), format = '%d/%m/%Y')))
})
df = data.frame(dates_pub=dates_pub)
df$group = ifelse(df$dates_pub<=90,'All treated', 
                  ifelse(df$dates_pub>90 & df$dates_pub<=120,'Employees treated / VAT control',
                         'All control'))
df$group = factor(df$group, levels =c('All treated','Employees treated / VAT control',
                                      'All control'))

p=ggplot(df,aes(dates_pub, fill=group)) +
  geom_histogram(binwidth = 10) +
  labs(y ='VAT noncompliance disclosures (#)',
       x = '') +
  scale_x_continuous(breaks = c(0,91,181,271,360), 
                     labels = c('January','April','July','October','December')) +
  scale_y_continuous(labels = scales::comma) +
  geom_vline(xintercept=90,linetype='dashed') +
  ggplot2::annotate(geom = "text", x = 90, y = 600, 
                    label = "Donation deadline \n VAT taxpayers (March 31)",angle=90,size=5) +
  geom_vline(xintercept=120,linetype='dashed') +
  ggplot2::annotate(geom = "text", x = 130, y = 600, 
                    label = "Donation deadline \n employees (April 30)",angle=90,size=5) +
  theme_classic() +
  scale_fill_manual(values = c("All treated" = "black", 
                               "Employees treated / VAT control" = "gray50", 
                               "All control" = "lightgray"), name = "") +
  guides(fill = guide_legend(title = " ")) +
  theme(axis.title.y = element_text(margin = margin(r = 15)),
        axis.text = element_text(color='black'),
        axis.text.x = element_text(margin = margin(t = 10)),
        text=element_text(size=20, color='black'),
        legend.position = "bottom",
        legend.text = element_text(margin = margin(r = 50, unit = "pt")),
        legend.spacing.x = unit(25, 'pt')) +
  guides(fill = guide_legend(byrow = TRUE))

pdf(file = 'Output/hist_disclosures.pdf', width=12,height=7)
p
dev.off()

## Table 3: Tax Donations ----------------------------------------------------------

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

dv_mean = c(round(mean(data[rownames(m1@frame),]$amount,na.rm=T),0),
            round(mean(data[rownames(m2@frame),]$amount,na.rm=T),0),
            round(mean(data[rownames(m3@frame),]$amount,na.rm=T),0),
            round(mean(data[rownames(m4@frame),]$amount,na.rm=T),0))
dv_sd = c(round(sd(data[rownames(m1@frame),]$amount,na.rm=T),0),
          round(sd(data[rownames(m2@frame),]$amount,na.rm=T),0),
          round(sd(data[rownames(m3@frame),]$amount,na.rm=T),0),
          round(sd(data[rownames(m4@frame),]$amount,na.rm=T),0))

dv_mean = scales::comma(dv_mean)
dv_sd = scales::comma(dv_sd)

stargazer(m1,m2,m3,m4,
          title = 'VAT Disclosures and Tax Donations',
          dep.var.caption = c(''),
          dep.var.labels = c('Tax donations'),
          keep = c('treat_pub','scale(treat_pub_n)','treat_pub:scale(treat_pub_n)',
                   'treat_rec','scale(treat_rec_n)','treat_rec:scale(treat_rec_n)'),
          order = c('treat_pub','scale(treat_pub_n)','treat_pub:scale(treat_pub_n)',
                    'treat_rec','scale(treat_rec_n)','treat_rec:scale(treat_rec_n)'),
          covariate.labels = c('Treated (noncompliance)',
                               'Intensity (\\# NC disclosures)',
                               'Intensity (NC)*treated (NC)',
                               'Treated (enforcement)',
                               'Intensity (\\# enf disclosures)',
                               'Intensity (enf)*treated (enf)'),
          keep.stat = c('n'),
          add.lines = list(c("Controls",rep("Yes",4)),
                           c("FE",rep("Year/Town",4)),
          c('Outcome mean',dv_mean),
          c('Outcome SD',dv_sd)),
          omit = 'Constant',
          star.cutoffs = c(0.1,0.05, 0.01),
          star.char = c('\\dag','*','**'),
          no.space = T,
          notes.append = FALSE, notes.align = "l",
          notes = "$^{\\dag}$p$<$.10; $^{*}$p$<$.05; $^{**}$p$<$.01",
          label = 'natural_main',
          out = 'Output/natural_exp_tax_don.tex')


## Table 4: Donation Drives --------------------------------------------------------

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

dv_mean = c(round(mean(data[rownames(m5@frame),]$amount,na.rm=T),0),
            round(mean(data[rownames(m6@frame),]$amount,na.rm=T),0),
            round(mean(data[rownames(m7@frame),]$amount,na.rm=T),0),
            round(mean(data[rownames(m8@frame),]$amount,na.rm=T),0),
            round(mean(data[rownames(m9@frame),]$amount,na.rm=T),0),
            round(mean(data[rownames(m10@frame),]$amount,na.rm=T),0))
dv_sd = c(round(sd(data[rownames(m5@frame),]$amount,na.rm=T),0),
          round(sd(data[rownames(m6@frame),]$amount,na.rm=T),0),
          round(sd(data[rownames(m7@frame),]$amount,na.rm=T),0),
          round(sd(data[rownames(m8@frame),]$amount,na.rm=T),0),
          round(sd(data[rownames(m9@frame),]$amount,na.rm=T),0),
          round(sd(data[rownames(m10@frame),]$amount,na.rm=T),0))

dv_mean = scales::comma(dv_mean)
dv_sd = scales::comma(dv_sd)

stargazer(m5,m6,m7,m8,m9,m10,
          title = 'VAT Disclosures and NGO Fundraisers',
          dep.var.caption = c(' '),
          dep.var.labels = c('NGO Donation Drives'),
          keep = c('vat_pub_lasttaxyear','vat_rec_lasttaxyear',
                   'treat_pub','treat_rec'),
          covariate.labels = c('Noncompliance (Last Year)',
                               'Enforcement (Last Year)',
                               'Treated (Noncompliance)',
                               'Treated (Enforcement)'),
          keep.stat = c('n'),
          add.lines = list(c("Controls",rep(c('No',"Yes"),3)),
                           c("FE",rep("Year",6)),
                           c('Outcome mean',dv_mean),
                           c('Outcome SD',dv_sd)),
          omit = 'Constant',
          star.cutoffs = c(0.1,0.05, 0.01),
          star.char = c('\\dag','*','**'),
          no.space = T,
          notes.append = FALSE, notes.align = "l",
          notes = "$^{\\dag}$p$<$.10; $^{*}$p$<$.05; $^{**}$p$<$.01",
          label = 'natural_drives',
          out = 'Output/natural_exp_don_drives.tex')


## Figure 3: Types of NGO Services ---------------------------------------------------

out = c("amount_social","amount_edu","amount_culture","amount_clubs",
        "amount_econ","amount_religious")
out_lb = c('Social \n services','Education','Culture','Clubs',
           'Local \n business','Religious')

res = data.frame(out = rep(out,2),out_lb=rep(out_lb,2),
                 type=rep(c('Noncompliance \n disclosure','Enforcement \n disclosure'),
                          each=length(out)),
                 b=NA,se=NA,t=NA)
res$out_lb = factor(res$out_lb,levels= c('Social \n services','Education','Culture','Clubs',
                                         'Local \n business','Religious'))
res$type = factor(res$type,levels=c('Noncompliance \n disclosure','Enforcement \n disclosure'))

for(i in 1:nrow(res)){
  print(i)
  if(grepl('Noncompliance',res$type[i])){
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
  scale_x_continuous(labels = scales::comma) +
  scale_color_manual(values = c("Noncompliance \n disclosure" = "Red", 
                                "Enforcement \n disclosure" = "Blue"), name = "") +
  guides(fill = guide_legend(title = " ",
                             label.position = "right", 
                             label.hjust = 0, 
                             title.position = "top",
                             keywidth = unit(3, "lines"))) +
  labs(y=' ',x='Change in donations (euros)')+
  theme(text=element_text(size=20, color='black'),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.text.x = element_text(margin = margin(t = 10)),
        axis.text = element_text(color='black', margin = margin(b = 30)),
        legend.position = 'bottom',
        legend.text = element_text(margin = margin(r = 50, unit = "pt")),
        legend.spacing.x = unit(25, 'pt'))


pdf("Output/donation_types.pdf", 12, 7) 
print(p)
dev.off()   


## Table 5: Survey Treatment & Fiscal Contract --------------------------------------------

m1 = lmer(corrupt_tax ~ treat +
            scale(age) + male + income + edu + pol_lr + emp_cat + rel_cat +
            (1|ip_region),
          survey)
m2 = lmer(noncomp_unpun_reason_auth ~ treat +
            scale(age) + male + income + edu + pol_lr + 
            (1|ip_region),
          survey)
m3 = lmer(behav_just_taxcheat ~ treat +
            scale(age) + male + income + edu + pol_lr + emp_cat + rel_cat +
            (1|ip_region),
          survey)
m4 = lmer(behav_just_benefitcheat ~ treat +
            scale(age) + male + income + edu + pol_lr + emp_cat + rel_cat +
            (1|ip_region),
          survey)


dv_mean = c(round(mean(survey[rownames(m1@frame),]$corrupt_tax,na.rm=T),2),
            round(mean(survey[rownames(m2@frame),]$noncomp_unpun_reason_auth,na.rm=T),2),
            round(mean(survey[rownames(m3@frame),]$behav_just_taxcheat,na.rm=T),2),
            round(mean(survey[rownames(m4@frame),]$behav_just_benefitcheat,na.rm=T),2))
dv_sd = c(round(sd(survey[rownames(m1@frame),]$corrupt_tax,na.rm=T),2),
          round(sd(survey[rownames(m2@frame),]$noncomp_unpun_reason_auth,na.rm=T),2),
          round(sd(survey[rownames(m3@frame),]$behav_just_taxcheat,na.rm=T),2),
          round(sd(survey[rownames(m4@frame),]$behav_just_benefitcheat,na.rm=T),2))


dv_mean = scales::comma(dv_mean)
dv_sd = scales::comma(dv_sd)

stargazer(m1,m2,m3,m4,
          title = 'Disclosures Decrease Faith in the Tax System',
          dep.var.caption = c('Tax authorities / How justified: '),
          dep.var.labels = c('Are corrupt','Allow evasion',
                             'Cheating on taxes','Cheating on benefits'),
          keep = c('treat'),
          covariate.labels = c('Treated'),
          keep.stat = c('n'),
          add.lines = list(c("Controls",rep("Yes",4)),
                           c("FE",rep("Region",4)),
                           c('Outcome mean',dv_mean),
                           c('Outcome SD',dv_sd)),
          omit = 'Constant',
          star.cutoffs = c(0.1,0.05, 0.01),
          star.char = c('\\dag','*','**'),
          no.space = T,
          notes.append = FALSE, notes.align = "l",
          notes = "$^{\\dag}$p$<$.10; $^{*}$p$<$.05; $^{**}$p$<$.01",
          label = 'fiscal_contract',
          out = 'Output/effects_fiscal_contract.tex')

## Table 6: Survey Treatment & Giving -----------------------------------------------------


m1 = lmer(Lottery_1 ~ treat +
            scale(age) + male + income + edu + pol_lr + emp_cat + rel_cat +
            (1|ip_region),
          survey)
m2 = lmer(Lottery_1 ~ treat*public_serv_you +
            scale(age) + male + income + edu + pol_lr + emp_cat + rel_cat +
            (1|ip_region),
          survey)
m3 = lmer(Lottery_1 ~ treat*public_serv_fam +
            scale(age) + male + income + edu + pol_lr + emp_cat + rel_cat +
            (1|ip_region),
          survey)
m4 = lmer(Lottery_1 ~ treat*public_serv_town +
            scale(age) + male + income + edu + pol_lr + emp_cat + rel_cat +
            (1|ip_region),
          survey)

dv_mean = c(round(mean(survey[rownames(m1@frame),]$Lottery_1,na.rm=T),2),
            round(mean(survey[rownames(m2@frame),]$Lottery_1,na.rm=T),2),
            round(mean(survey[rownames(m3@frame),]$Lottery_1,na.rm=T),2),
            round(mean(survey[rownames(m4@frame),]$Lottery_1,na.rm=T),2))
dv_sd = c(round(sd(survey[rownames(m1@frame),]$Lottery_1,na.rm=T),2),
          round(sd(survey[rownames(m2@frame),]$Lottery_1,na.rm=T),2),
          round(sd(survey[rownames(m3@frame),]$Lottery_1,na.rm=T),2),
          round(sd(survey[rownames(m4@frame),]$Lottery_1,na.rm=T),2))

dv_mean = scales::comma(dv_mean)
dv_sd = scales::comma(dv_sd)

stargazer(m1,m2,m3,m4,
          title = 'Noncompliance List, Donation Levels, and Reliance on Public Services',
          dep.var.labels = c('Donation'),
          keep = c('treat','treat:public_serv_you','treat:public_serv_fam',
                   'treat:public_serv_town'),
          covariate.labels = c('Treated','Treated*rely on public (you)',
                               'Treated*rely on public (family)',
                               'Treated*rely on public (town)'),
          keep.stat = c('n'),
          add.lines = list(c("Controls",rep("Yes",4)),
                           c("FE",rep("Region",4)),
                           c('Outcome mean',dv_mean),
                           c('Outcome SD',dv_sd)),
          omit = 'Constant',
          star.cutoffs = c(0.1,0.05, 0.01),
          star.char = c('\\dag','*','**'),
          no.space = T,
          notes.append = FALSE, notes.align = "l",
          notes = "$^{\\dag}$p$<$.10; $^{*}$p$<$.05; $^{**}$p$<$.01",
          label = 'survey_res_main',
          out = 'Output/effects_donations.tex' )

## Table 7: EVS Volunteering and Tax Cheating Perceptions ------------------------

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


dv_mean = c(round(mean(evs[rownames(m1@frame),]$ngo_index_local,na.rm=T),2),
            round(mean(evs[rownames(m2@frame),]$ngo_index_local,na.rm=T),2),
            round(mean(evs[rownames(m3@frame),]$ngo_index_global,na.rm=T),2),
            round(mean(evs[rownames(m4@frame),]$ngo_index_global,na.rm=T),2))
dv_sd = c(round(sd(evs[rownames(m1@frame),]$ngo_index_local,na.rm=T),2),
          round(sd(evs[rownames(m2@frame),]$ngo_index_local,na.rm=T),2),
          round(sd(evs[rownames(m3@frame),]$ngo_index_global,na.rm=T),2),
          round(sd(evs[rownames(m4@frame),]$ngo_index_global,na.rm=T),2))

dv_mean = scales::comma(dv_mean)
dv_sd = scales::comma(dv_sd)

stargazer(m1,m2,m3,m4,
          title = 'Perceptions of Tax Cheating and Volunteering',
          dep.var.caption = c(''),
          dep.var.labels = c('Volunteering index (local NGOs)',
                             'Volunteering index (global NGOs)'),
          keep = c('compcheat'),
          covariate.labels = c('Tax cheating by connationals'),
          keep.stat = c('n'),
          add.lines = list(c("Controls",rep(c('No',"Yes"),2)),
                           c("FE",rep("Year",4)),
                           c('Outcome mean',dv_mean),
                           c('Outcome SD',dv_sd)),
          omit = 'Constant',
          star.cutoffs = c(0.1,0.05, 0.01),
          star.char = c('\\dag','*','**'),
          no.space = T,
          notes.append = FALSE, notes.align = "l",
          notes = "$^{\\dag}$p$<$.10; $^{*}$p$<$.05; $^{**}$p$<$.01",
          label = 'tab:ext_val_evs',
          out = 'Output/cross_country_evs.tex')


# Effect Magnitude Calculations --------------------------------------------

## Effect of survey treatment 

m1 = lmer(Lottery_1 ~ treat +
            scale(age) + male + income + edu + pol_lr + emp_cat + rel_cat +
            (1|ip_region),
          survey)

paste0((round(summary(m1)$coefficients[2,1]/mean(m1@frame[m1@frame$treat==0,]$Lottery_1),4)*100),'%')

## Effect of anonymous donations (conjoint, see figure A26 in appendix)

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

# Difference in amce of "public donation" between treated & control
# page 31 main text
paste0(round((summary(m1)$Group2amce[9,3]-summary(m1)$Group1amce[9,3]),2)*100,'%')

# Difference in SD estimated effect across NGO types (page 27)

out = c("amount_social","amount_edu","amount_culture","amount_clubs",
        "amount_econ","amount_religious")
out_lb = c('Social \n services','Education','Culture','Clubs',
           'Local \n business','Religious')

res = data.frame(out = rep(out,2),out_lb=rep(out_lb,2),
                 type=rep(c('Noncompliance \n disclosure','Enforcement \n disclosure'),
                          each=length(out)),
                 b=NA,se=NA,t=NA)
res$out_lb = factor(res$out_lb,levels= c('Social \n services','Education','Culture','Clubs',
                                         'Local \n business','Religious'))
res$type = factor(res$type,levels=c('Noncompliance \n disclosure','Enforcement \n disclosure'))

for(i in 1:nrow(res)){
  print(i)
  if(grepl('Noncompliance',res$type[i])){
    f = paste0('scale(',res$out[i],') ~ treat_pub +
            scale(vat_pub_lasttaxyear_pre1) + scale(vat_rec_lasttaxyear_pre1) + 
            scale(taxoff_dist) + taxoff_type +
            scale(pop) + scale(HHI_gen) +  
            scale(edu11_high) + scale(edu11_low) +
            scale(mun_revenue_tax) + 
            scale(mun_tax_claims) + scale(mun_exp_t) + 
            sin_civ + sin_right + sin_left + sin_popul + 
            (1|year) + (1 | mun_clean)')}
  else{
    f = paste0('scale(',res$out[i],') ~ treat_rec +
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
print(res)

t2 = Sys.time()
t1-t2
