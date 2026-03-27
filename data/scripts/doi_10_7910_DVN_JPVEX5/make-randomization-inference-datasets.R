library(magrittr)
library(plyr)
library(tidyverse)
library(parallel)
library(doParallel)
library(haven)
library(patchwork)


rm(list=ls())
home = 'C:/Users/jdt34/Dropbox/VNA_Responsiveness/20191201_LaborLawMechanisms/LaborCode_2019/'


pool = delegates = paste0(home, 'delegate-data.Rds') %>%
  readRDS %>%
  rbind(data.frame(ID=148,
                   Province='Binh Dinh',
                   Name_VN="Nguy???n Van C???nh",
                   Name_EN='Nguyen Van Canh',
                   FullTime=1,
                   CentNom=1,
                   Competitive=0,
                   stringsAsFactors=F)) %>%
  mutate(Bucket=NA)
covariates = paste0(home, 'provincial-matching-data.Rds') %>%
  readRDS %>%
  mutate(Bucket=NA)


DoOneRandomization = function(pool, delegates, covariates) {
  treatments = c(rep(c(rep(' 25% Treated', 2), 
                       rep(' 50% Treated', 1), 
                       rep('100% Treated', 3)), 10), 
                 ' 25% Treated', 
                 rep('100% Treated', 2))
  
  
  # Assign each province to one of three buckets (25% treated, 50% treated, 100% treated)
  for(i in 1:nrow(covariates)) {
    drawnTreatment = sample(x=length(treatments), 
                            size=1)
    drawnDelegate = sample_n(tbl=pool, 
                             size=1)
    delegates$Bucket[delegates$Province==drawnDelegate$Province] = treatments[drawnTreatment]
    covariates$Bucket[covariates$Province==drawnDelegate$Province] = treatments[drawnTreatment]
    pool = subset(pool, Province!=drawnDelegate$Province)
    treatments = treatments[-drawnTreatment]
  }; rm(i); rm(drawnDelegate); rm(drawnTreatment); rm(pool)
  
  
  # Assign each delegate in the 25% and 50% treated buckets to treatment or control
  delegates = delegates %>%
    mutate(Treated=as.integer(ifelse(Bucket=='100% Treated', 1, 0)))
  delegates$Treated[delegates$Bucket==' 25% Treated'] = rbinom(n=sum(delegates$Bucket==' 25% Treated'), size=1, prob=0.25)
  delegates$Treated[delegates$Bucket==' 50% Treated'] = rbinom(n=sum(delegates$Bucket==' 50% Treated'), size=1, prob=0.5)
  
  
  # Assign each treated delegate to citizen or firm treatment
  delegates$Treatment = 'Control'
  delegates$Treatment[delegates$Treated==1] = sample(x=c('Citizen','Firm'), 
                                                     size=sum(delegates$Treated==1), 
                                                     replace=T, 
                                                     prob=c(0.5, 0.5))
  delegates$Treatment = factor(x=delegates$Treatment, 
                               levels=c('Control','Citizen','Firm'))
  delegates$Citizen = mapvalues(x=delegates$Treatment,
                                from=c('Control','Citizen','Firm'),
                                to=c(0, 1, 0)) %>%
    as.character %>%
    as.integer
  delegates$Firm = mapvalues(x=delegates$Treatment,
                             from=c('Control','Citizen','Firm'),
                             to=c(0, 0, 1)) %>%
    as.character %>%
    as.integer
  
  
  # Assign each treated delegate to electoral accountability or upward accountability prime
  delegates$Prime = 'Control'
  delegates$Prime[delegates$Treated==1] = sample(x=c('Electoral','Upward'), 
                                                 size=sum(delegates$Treated==1), 
                                                 replace=T, 
                                                 prob=c(0.5, 0.5))
  delegates$Prime = factor(x=delegates$Prime, 
                           levels=c('Control','Electoral','Upward'))
  delegates$Electoral = mapvalues(x=delegates$Prime,
                                  from=c('Control','Electoral','Upward'),
                                  to=c(0, 1, 0)) %>%
    as.character %>%
    as.integer
  delegates$Upward = mapvalues(x=delegates$Prime,
                               from=c('Control','Electoral','Upward'),
                               to=c(0, 0, 1)) %>%
    as.character %>%
    as.integer
  
  
  # Calculate provincial treatment and prime shares
  provinces = ddply(.data=delegates, 
                    .variables='Province',
                    .fun=function(x) {
                      data.frame(Prop.Citizen=mean(x$Citizen==1),
                                 Prop.Firm=mean(x$Firm==1),
                                 Prop.Electoral=mean(x$Electoral==1),
                                 Prop.Upward=mean(x$Upward==1))
                    }, .inform=T)
  delegates = merge(x=delegates,
                    y=provinces,
                    by='Province',
                    all.x=T) %>%
    mutate(Iteration=ii)
  return(delegates)
}


# Takes less than 10 minutes
iterations = 1e4
cl = makeCluster(6)
registerDoParallel(cl)

set.seed(31415)
timed = Sys.time()
assignments = foreach(ii=icount(iterations), 
                      .packages=c('magrittr','plyr','tidyverse'), 
                      .combine=rbind, 
                      .inorder=F) %dopar% { DoOneRandomization(pool, delegates, covariates) }
timed = diff(c(timed, Sys.time()))
timed; rm(timed)

stopCluster(cl); rm(cl); rm(iterations)
paste0(home, 'RI-assignments-seeded.Rds') %>%
  saveRDS(object=assignments,
          file=.)

colnames(assignments)[colnames(assignments)=='Prop.Citizen'] = 'PropCitizen'
colnames(assignments)[colnames(assignments)=='Prop.Firm'] = 'PropFirm'
colnames(assignments)[colnames(assignments)=='Prop.Electoral'] = 'PropElectoral'
colnames(assignments)[colnames(assignments)=='Prop.Upward'] = 'PropUpward'
paste0(home, 'RI-assignments-seeded.dta') %>%
  write_dta(data=assignments,
            path=.)




# Check cell sizes
checks = count(assignments, Iteration, Treatment, Prime) %>%
  ddply(c('Iteration','Treatment'), function(x) {
    mutate(x, nTreatment=sum(x$n))
  }, .progress='text') %>%
  ddply(c('Iteration','Prime'), function(x) {
    mutate(x, nPrime=sum(x$n))
  }, .progress='text')
treatment_panel = checks %>%
  subset(Prime!='Electoral') %>%
  ggplot(aes(x=nTreatment)) +
  geom_histogram(binwidth=1) +
  facet_wrap(~Treatment, nrow=1) +
  coord_fixed(ratio=1/4, xlim=c(100, 200), ylim=c(0, 400)) +
  labs(x='# of delegates', y='# of realizations', title='Infographic treatments') +
  theme_bw()
prime_panel = checks %>%
  subset(Treatment!='Citizen') %>%
  ggplot(aes(x=nPrime)) +
  geom_histogram(binwidth=1) +
  facet_wrap(~Prime, nrow=1) +
  coord_fixed(ratio=1/4, xlim=c(100, 200), ylim=c(0, 400)) +
  labs(x='# of delegates', y='# of realizations', title='Prime treatments') +
  theme_bw()
wrap_plots(treatment_panel, prime_panel, ncol=1)



