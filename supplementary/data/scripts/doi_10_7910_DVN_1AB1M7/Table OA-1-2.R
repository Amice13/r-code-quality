library(tidyverse)
library(stargazer)

#####################################
# Appendix B5                       #
#####################################

formatTable <- function(df, cov_list, eligible = FALSE){
  if (eligible == TRUE){
    elig <- bind_rows(data.frame(Site = 'Eligible Population'), 
                      as.data.frame(apply(df[!(df$Site %in% exclude), cov_list], 2, summary, na.rm = TRUE)) %>% rownames_to_column('Site'))
  }else{
    elig <- NULL
  }
  temp <- bind_rows(data.frame(Site = 'Selected Sites, Country-Specific'),
                    df[df$Site %in% selected,], 
                    data.frame(Site = 'Selected Sites'), 
                    as.data.frame(apply(df[df$Site %in% selected, cov_list], 2, summary, na.rm = TRUE)) %>% rownames_to_column('Site'),
                    data.frame(Site = 'Target Population'), 
                    as.data.frame(apply(df[, cov_list], 2, summary, na.rm = TRUE)) %>% rownames_to_column('Site'),
                    elig,
                    data.frame(Site = 'Non-Selected Sites'),
                    as.data.frame(apply(df[!df$Site %in% selected, cov_list], 2, summary, na.rm = TRUE)) %>% rownames_to_column('Site')) %>%
    dplyr::select(Site, everything()) %>%
    filter(!(Site %in% c('1st Qu.', '3rd Qu.'))) %>%
    mutate(Site = gsub('\\.', '', Site)) %>%
    mutate(Site = ifelse(!grepl('Sites|Population', Site), paste0(' ', Site), Site)) %>%
    mutate_at(vars(matches('Population|GDP')), ~formatC(.x, format="d", big.mark=",")) %>% 
    mutate_if(is.character, ~ifelse(is.na(.x)|.x=='NA', '', .x))
  return(temp)
}

#####################################
# Table OA-1                        #
#####################################

load("../Data/Applications/naumann_target.rdata")
df <- data.frame(df)
attributes(df)$name <- NULL
country_list <- unique(df$nation)
cov_use <- c("gdp", "SM.POP.TOTL.ZS", "SL.UEM.TOTL.NE.ZS", "fe4", 'imm_s_avg', "age", "education") 
x <- plyr::ldply(1:15, function(i){
  data_x <- df[df$nation == country_list[i], c("cntry", cov_use)]
  apply(data_x[, cov_use], 2, mean, na.rm = TRUE)
})
x$cntry <- country_list
colnames(x) <- c("GDP per capita", "Immigration", "Unemployment", "Fiscal Exposure",  "Immi-Support", "Age", "Education", 'Site')
selected <- c("Sweden", "Denmark", "Spain", "Switzerland", "Czech Republic", "UK")  

naumann <- formatTable(df = x, cov_list = colnames(x)[1:7])
stargazer(naumann, summary = F, rownames = F, out = '../Tables/Table OA-1.tex', digits = 2)

naumann

#####################################
# Table OA-2                        #
#####################################

load("../Data/Applications/metaketa_target.rdata")
df <- df[df$pop > 1e6 & (df$reg_af == 1 | df$reg_am == 1 | df$reg_as == 1),]
df$gdp <- exp(df$log_gdp)/1e6
df$police_personel <- exp(df$log_police_personel)
df$crime <- exp(df$log_crime)
sps_var0 <- c("regime_type", 'freedom_score', 'corruption_score', 'criminal_justice_score', 'crime', 'police_personel', 'gini', 'gdp')
sps_var  <- c("Regime Type", 'Freedom Score', 'Corruption', 'Criminal Justice', 'Crime Rate', 'Police Personnel', 'Gini', 'GDP')
x <- as.data.frame(df[,sps_var0])
colnames(x) <- sps_var
x$Site <- df$country
selected <- c("Bolivia", "China", "Liberia", "Pakistan", "Uruguay", "South Africa")
exclude <- df$country[df$partner_egap==0]

metaketa <- formatTable(df = x, cov_list = sps_var, eligible = T)  
stargazer(metaketa, summary = F, rownames = F, out = '../Tables/Table OA-2.tex', digits = 2)

metaketa
