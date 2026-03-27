setwd("Drive:/Location with replication files") ### Set directory ###

###############################
#### Install/Load Packages ####
###############################

pacman::p_load(pacman, tm, lfe, plm, data.table, dplyr, tidyr, ggplot2, lubridate, countrycode, sandwich, zoo, texreg, car, lmtest, pastecs, Hmisc,
               corrplot,sjPlot,AER,sandwich, ggeffects, ggpubr) 

#############################################################
#### Loading and Cleaning Raw Aid Data for Figure 1 Plot ####
#############################################################

ad <- read.csv('adcfa.csv', as.is = T, header=T) %>% ### Load raw aid data ###
  select(flow_class, year, usd_current, status, recipient_imf_code) %>% ### Select relevant columns ###
  filter(!status %in% c('Suspended','Cancelled','Pipeline: Pledge')) %>% ### Filter out aid projects not meant for research ###
  filter(!grepl(';', recipient_imf_code)) %>% ### Remove regional projects ###
  filter(year %in% 2000:2014) %>% ### Filter years ###
  mutate(usdbls = usd_current/1000000000) ### Convert dollar amounts to billion USD ###

adag <- aggregate(usdbls~flow_class, data=ad, FUN=sum) ### Aggregate by flow-type ###
ad$flow_class <- ifelse(ad$flow_class=='Vague (Official Finance)', 'VOF-like', ad$flow_class) ### Rename VOF ###

#########################################
#### Figure 1 Plot Aid Flows by Type ####
#########################################

ggplot(data=ad, aes(x=flow_class, y=usdbls, fill=flow_class)) + 
  geom_bar(stat="identity") + theme_bw() + scale_fill_grey(labels=c())+ theme(legend.position="none") +
  labs(y='Chinese Aid (Billions US$ Current)', x='Flow-type')

###########################
#### Load main dataset ####
###########################

data <- readRDS('fpadata.RDS') 

#####################################################
#### Create Figure 2 Instrumental Variable Plots ####
#####################################################

ggarrange(readRDS('steeldata.RDS') %>% ### Load Steel Data ###
            filter(year %in% 2000:2015) %>% ### Filter by Relevant Years ###
            ggplot(aes(x=year, y=log(crude), colour=1)) +
            geom_line() +
            theme_classic() +
            labs(y='Chinese Steel Production (log)', x='Year') +
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=8),
                  legend.position = '') +
            scale_x_continuous(breaks=seq(2000, 2015, 1)),
          data %>%
            mutate(oofproblvl = ifelse(oof_prob > median(oof_prob), 'H','L')) %>% ### Create Probability of Aid ###
            ggplot(aes(x=year, y=ideal_china, group=oofproblvl, colour=oofproblvl)) +
            stat_summary(geom='line',fun=mean) +
            theme_classic() +
            labs(y='Ideal Point Distance', x='Year') +
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=8),
                  legend.position = 'right') +
            scale_color_discrete(name='Aid Prob.', labels=c('High','Low')),
          ncol=1,
          nrow=2)

#########################
#### Table 1 Results ####
#########################

bi1 <-  felm(ideal_china ~ oof2|year|0|
               0, data=data)
summary(bi1, robust=F)

m1 <-  felm(ideal_china ~ oof2 +
              oda2+
              boix_dem +
              import_dep_lag_1+
              export_dep_lag_1+
              gdppc_wdi_lag_1+
              usaid2+
              cinc|year|0|
              ccode, data=data)
summary(m1, robust=F)

bi2 <-  felm(ideal_china ~ oof2*boix_dem|year|0|
               ccode, data=data)
summary(bi2, robust=F)

m2 <-  felm(ideal_china ~ oof2*boix_dem +
              oda2+
              import_dep_lag_1+
              export_dep_lag_1+
              gdppc_wdi_lag_1+
              usaid2+
              cinc|year|0|
              ccode, data=data)
summary(m2, robust=F)

#######################################################
#### Create Regime Type Category for Figure 3 Plot ####
#######################################################

data$bd <- ifelse(data$boix_dem==0, 'Autocracy', 'Democracy') 

################################################
#### lm() for Figure 3 Marginal Effect Plot ####
################################################

plotint <- lm(ideal_china ~ oof2*bd + oda2 + gdppc_wdi_lag_1 + gdppc_wdi + import_dep_lag_1 + export_dep_lag_1
              + cinc, data=data)

##############################################
#### Create Figure 3 Marginal Effect Plot ####
##############################################

intplot <- plot_model(plotint, type='pred', terms=c('oof2[0,2]', 'bd'))
intplot + theme_bw() + labs(title='', x='OOF 2-year Rolling Average (billions US$)', y='Ideal Point Distance from China', colour='BMR Regime-type') 
 
#########################
#### Table 2 Results ####
#########################

stg1 <- felm(oof2 ~ oof_iv3|year|0|ccode, data=data)
summary(stg1)

stg2 <- felm(oof2 ~ oof_iv3+
               oda2+
               boix_dem++
               import_dep_lag_1+
               export_dep_lag_1+
               gdppc_wdi_lag_1+
               usaid2+
               cinc|year|0|ccode, data=data)
summary(stg2)

oofiv1a <- felm(ideal_china~1|year|(oof2 ~ I(log(0.01+crude3)*oof_prob))|
                  ccode, data=data)
summary(oofiv1a, robust=F)

oofiv1b <-  felm(ideal_china ~ oda2 +
                   boix_dem+
                   import_dep_lag_1+
                   export_dep_lag_1+
                   gdppc_wdi_lag_1+
                   usaid2+
                   cinc|year|(oof2 ~ I(log(0.01+crude3)*oof_prob))|
                   ccode, data=data)
summary(oofiv1b, robust=F)

######################################
#### VOF/OOF Results for Table A5 ####
######################################

oof3 <-  felm(ideal_china ~ vofoof2 +
                oda2 +
                import_dep_lag_1+
                export_dep_lag_1+
                gdppc_wdi_lag_1+
                boix_dem+
                usaid2+
                cinc|year|0|
                ccode, data=data)
summary(oof3, robust=F)

oof4 <-  felm(ideal_china ~ vofoof2*boix_dem +
                oda2+
                import_dep_lag_1+
                export_dep_lag_1+
                gdppc_wdi_lag_1+
                usaid2+
                cinc|year|0|
                ccode, data=data)
summary(oof4, robust=F)

#################################
#### Project Values Table A6 ####
#################################

prjct1 <- felm(ideal_china ~ oofprjct_lag_1 +
                odaprjct_lag_1+
                 boix_dem+
                gdppc_wdi_lag_1+
                import_dep_lag_1+
                export_dep_lag_1+
                usaid2+
                cinc|year|0|
                ccode, data=data)
summary(prjct1, robust=F)

prjct2 <-  felm(ideal_china ~ oofprjct*boix_dem +
                  odaprjct_lag_1+
                  boix_dem+
                  gdppc_wdi_lag_1+
                  import_dep_lag_1+
                  export_dep_lag_1+
                  usaid2+
                  cinc|year|0|
                  ccode, data=data)
summary(prjct2, robust=F)

#####################################################
#### Alternative Democracy Measures for Table A7 ####
#####################################################

demo1 <-  felm(ideal_china ~ oof2 +
                 oda2 +
                 import_dep_lag_1+
                 export_dep_lag_1+
                 gdppc_wdi_lag_1+
                 politybinary+
                 usaid2+
                 cinc|year|0|
                 ccode, data=data)
summary(demo1, robust=F)

demo2 <-  felm(ideal_china ~ oof2*politybinary +
                 oda2+
                 import_dep_lag_1+
                 export_dep_lag_1+
                 gdppc_wdi_lag_1+
                 usaid2+
                 cinc|year|0|
                 ccode, data=data)
summary(demo2, robust=F)

demo3 <-  felm(ideal_china ~ oof2 +
                 oda2 +
                 import_dep_lag_1+
                 export_dep_lag_1+
                 gdppc_wdi_lag_1+
                 vdembinary+
                 usaid2+
                 cinc|year|0|
                 ccode, data=data)
summary(demo3, robust=F)

demo4 <-  felm(ideal_china ~ oof2*vdembinary +
                 oda2+
                 import_dep_lag_1+
                 export_dep_lag_1+
                 gdppc_wdi_lag_1+
                 usaid2+
                 cinc|year|0|
                 ccode, data=data)
summary(demo4, robust=F)

##########################################
#### Additional Controls for Table A8 ####
##########################################

plus1 <- felm(ideal_china ~ oof2 +
                oda2 +
                import_dep_lag_1+
                export_dep_lag_1+
                gdppc_wdi_lag_1+
                boix_dem+
                usaid2+
                cinc+
                corruption|year|0|
                ccode, data)
summary(plus1, robust=F)

plus2 <- felm(ideal_china ~ oof2 +
                oda2+
                import_dep_lag_1+
                export_dep_lag_1+
                gdppc_wdi_lag_1+
                boix_dem+
                usaid2+
                cinc+
                g7dacoda_lag_1+
                mldacoda_lag_1|year|0|
                ccode, data=data)
summary(plus2, robust=F)

plus3 <- felm(ideal_china ~ oof2 +
                oda2+
                import_dep_lag_1+
                export_dep_lag_1+
                gdppc_wdi_lag_1+
                boix_dem+
                usaid2+
                cinc+
                corruption+
                g7dacoda_lag_1+
                mldacoda_lag_1|year|0|
                ccode, data=data)
summary(plus3, robust=F)

plus4 <- felm(ideal_china ~ oof2*boix_dem +
                oda2+
                import_dep_lag_1+
                export_dep_lag_1+
                gdppc_wdi_lag_1+
                usaid2+
                cinc+
                corruption|year|0|
                ccode, data=data)
summary(plus4, robust=F)

plus5 <- felm(ideal_china ~ oof2*boix_dem +
                oda2+
                import_dep_lag_1+
                export_dep_lag_1+
                gdppc_wdi_lag_1+
                usaid2+
                cinc+
                g7dacoda_lag_1+
                mldacoda_lag_1|year|0|
                ccode, data=data)
summary(plus5, robust=F)

plus6 <- felm(ideal_china ~ oof2*boix_dem +
                oda2+
                import_dep_lag_1+
                export_dep_lag_1+
                gdppc_wdi_lag_1+
                usaid2+
                cinc+
                corruption+
                g7dacoda_lag_1+
                mldacoda_lag_1|year|0|
                ccode, data=data)
summary(plus6, robust=F)

######################################
#### Alternative Samples Table A9 ####
######################################

micro <- data %>% filter(population_wdi>200000) #### Create data omitting microstates ####
deving <- data %>% filter(deving!=0) #### Create data omitting developed countries ####
africa <- data %>% filter(continent2=='Africa') #### Create data with just African countries ####

micro1 <-  felm(ideal_china ~ oof2 +
                  oda2+
                  import_dep_lag_1+
                  export_dep_lag_1+
                  gdppc_wdi_lag_1+
                  boix_dem+
                  usaid2+
                  cinc|year|0|
                  ccode, data=micro)
summary(micro1, robust=F)

micro2 <-  felm(ideal_china ~ oof2*boix_dem +
                  oda2+
                  import_dep_lag_1+
                  export_dep_lag_1+
                  gdppc_wdi_lag_1+
                  usaid2+
                  cinc|year|0|
                  ccode, data=micro)
summary(micro2, robust=F)

dev1 <-  felm(ideal_china ~ oof2 +
                oda2+
                import_dep_lag_1+
                export_dep_lag_1+
                gdppc_wdi_lag_1+
                boix_dem+
                usaid2+
                cinc|year|0|
                ccode, data=deving)
summary(dev1, robust=F)

dev2 <-  felm(ideal_china ~ oof2*boix_dem +
                oda2+
                gdppc_wdi_lag_1+
                import_dep_lag_1+
                export_dep_lag_1+
                usaid2+
                cinc|year|0|
                ccode, data=deving)
summary(dev2, robust=F)

africa1 <-  felm(ideal_china ~ oof2 +
                   oda2+
                   import_dep_lag_1+
                   export_dep_lag_1+
                   gdppc_wdi_lag_1+
                   boix_dem+
                   usaid2+
                   cinc|year|0|
                   ccode, data=africa)
summary(africa1, robust=F)

africa2 <-  felm(ideal_china ~ oof2*boix_dem +
                   oda2+
                   gdppc_wdi_lag_1+
                   import_dep_lag_1+
                   export_dep_lag_1+
                   gdppc_wdi_lag_1+
                   usaid2+
                   cinc|year|0|
                   ccode, data=africa)
summary(africa2, robust=F)

#####################################
#### 2way FEs and PCSE Table A10 ####
#####################################

pdata <- pdata.frame(data, index=c('ccode','year')) #### Create pdata.frame for use in the PCSE models ####

fe1 <- felm(ideal_china ~ oof2 +
              oda2+
              import_dep_lag_1+
              export_dep_lag_1+
              gdppc_wdi_lag_1+
              boix_dem+
              usaid2+
              cinc|ccode+year|0|ccode, data=data)
summary(fe1, robust=F)

fe2 <- felm(ideal_china ~ oof2*boix_dem +
              oda2+
              import_dep_lag_1+
              export_dep_lag_1+
              gdppc_wdi_lag_1+
              usaid2+
              cinc|ccode+year|0|ccode, data=data)
summary(fe2, robust=F)

pcse1 <- plm(ideal_china ~ oof2 +
               oda2+
               import_dep_lag_1+
               export_dep_lag_1+
               gdppc_wdi_lag_1+
               boix_dem+
               usaid2+
               cinc, data=pdata, model='pooling')
pcse1s <- coeftest(pcse1, vcov.=function(x) vcovBK(x, type="HC1", cluster='time'))

pcse2 <- plm(ideal_china ~ oof2*boix_dem +
               oda2+
               import_dep_lag_1+
               export_dep_lag_1+
               gdppc_wdi_lag_1+
               usaid2+
               cinc, model='pooling', data=pdata)
pcse2s <- coeftest(pcse2, vcov.=function(x) vcovBK(x, type="HC1", cluster='time'))

###################################
#### Single Aid Type Table A11 ####
###################################

oofonly1 <-  felm(ideal_china ~ oof2+
                    import_dep_lag_1+
                    export_dep_lag_1+
                    gdppc_wdi_lag_1+
                    boix_dem+
                    usaid2+
                    cinc|year|0|
                    ccode, data=data)
summary(oofonly1, robust=F)

oofonly2 <-  felm(ideal_china ~ oof2*boix_dem+
                    gdppc_wdi_lag_1+
                    import_dep_lag_1+
                    export_dep_lag_1+
                    gdppc_wdi_lag_1+
                    usaid2+
                    cinc|year|0|
                    ccode, data=data)
summary(oofonly2, robust=F)


odaonly1 <-  felm(ideal_china ~ oda2+
                    import_dep_lag_1+
                    export_dep_lag_1+
                    gdppc_wdi_lag_1+
                    boix_dem+
                    usaid2+
                    cinc|year|0|
                    ccode, data=data)
summary(odaonly1, robust=F)

################################
#### Varying Lags Table A12 ####
################################

lags1 <- felm(ideal_china ~ oof+
                oda+
                import_dep_lag_1+
                export_dep_lag_1+
                gdppc_wdi_lag_1+
                boix_dem+
                usaid2+
                cinc|year|0|
                ccode, data=data)
summary(lags1, robust=F)

lags2 <- felm(ideal_china ~ oof*boix_dem+
                oda+
                import_dep_lag_1+
                export_dep_lag_1+
                gdppc_wdi_lag_1+
                usaid2+
                cinc|year|0|
                ccode, data=data)
summary(lags2, robust=F)

lags3 <- felm(ideal_china ~ oof_lag_1+
                oda_lag_1+
                import_dep_lag_1+
                export_dep_lag_1+
                gdppc_wdi_lag_1+
                boix_dem+
                usaid2+
                cinc|year|0|
                ccode, data=data)
summary(lags3, robust=F)

lags4 <- felm(ideal_china ~ oof_lag_1*boix_dem+
                oda_lag_1+
                import_dep_lag_1+
                export_dep_lag_1+
                gdppc_wdi_lag_1+
                usaid2+
                cinc|year|0|
                ccode, data=data)
summary(lags4, robust=F)

##################################
#### Extended Period with WPI ####
##################################

wpi1 <- felm(ideal_china ~ oof2 +
               oda2+
               import_dep_lag_1+
               export_dep_lag_1+
               gdppc_wdi_lag_1+
               boix_dem+
               usaid2+
               wpi|year|0|
               ccode, data=data)
summary(wpi1, robust=F)

wpi2 <- felm(ideal_china ~ oof2*boix_dem+
               oda2+
               import_dep_lag_1+
               export_dep_lag_1+
               gdppc_wdi_lag_1+
               usaid2+
               wpi|year|0|
               ccode, data=data)
summary(wpi2, robust=F)

###########################
#### Sectors Table A14 ####
###########################

oofpi1 <- felm(ideal_china ~ oofpi2 +
                 oda2+
                 import_dep_lag_1+
                 export_dep_lag_1+
                 gdppc_wdi_lag_1+
                 boix_dem+
                 usaid2+
                 cinc|year|0|
                 ccode, data=data)
summary(oofpi1, robust=F)

oofpi2 <- felm(ideal_china ~ oofpi2*boix_dem +
                 oda2+
                 import_dep_lag_1+
                 export_dep_lag_1+
                 gdppc_wdi_lag_1+
                 usaid2+
                 cinc|year|0|
                 ccode, data=data)
summary(oofpi2, robust=F)

oofsi1 <- felm(ideal_china ~ oofsi2 +
                 oda2+
                 import_dep_lag_1+
                 export_dep_lag_1+
                 gdppc_wdi_lag_1+
                 boix_dem+
                 usaid2+
                 cinc|year|0|
                 0, data=data)
summary(oofsi1, robust=F)

oofsi2 <- felm(ideal_china ~ oofsi2*boix_dem +
                 oda2+
                 import_dep_lag_1+
                 export_dep_lag_1+
                 gdppc_wdi_lag_1+
                 usaid2+
                 cinc|year|0|
                 ccode, data=data)
summary(oofsi2, robust=F)

################################
#### Code for Annex 3 Plots ####
################################



data %>%
  filter(CountryName == 'Romania') %>% # Each plot can be created by altering the CountryName and by adjusting the... #
  ggplot() +
  geom_point(aes(x=year, y=ideal_us, colour='USA')) +
  geom_line(aes(x=year, y=ideal_us, group=1, colour='USA')) +
  geom_line(aes(x=year, y=ideal_china, group=1, colour='China')) +
  geom_point(aes(x=year, y=ideal_china, colour='China')) +
  geom_line(aes(x=year, y=oof*7, group=1, linetype='OOF'), colour='black', alpha=0.5) + #....amount that oof is multiplied by here and...#
  scale_y_continuous(name = 'Ideal Point Distance', sec.axis = sec_axis(~./7, name = 'OOF (Billions USD)')) + #...the amount that the second axis is divided by here. #
  labs(x='Year',y='Ideal Pt. Distance', title='Figure A6: OOF and Ideal Pt. Distance: Romania', colour='') +
  scale_color_manual(name='Ideal Pt. Dist.',
                     breaks=c('China','USA'),
                     values=c('#ea7871','#055cb3')) +
  scale_linetype_manual(name='Aid Flow',
                        values=c('dashed')) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=8),
        plot.title = element_text(size=11, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = 'right')
