# Code for CPS Figure 1

# List of required packages
required_packages <- c("tidyr", "dplyr", "srvyr", "survey", "ggpubr", "gtsummary", "jtools", "ggplot2", "viridis")

# Install missing packages
missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(missing_packages)) {
  install.packages(missing_packages)
}

# Load all required libraries
lapply(required_packages, library, character.only = TRUE)

setwd('~/Dropbox/CPS/CPS_Materials') # set working directory for the project

# Note, we have no right to this ESS file so anyone looking to replicate should download ESS rounds 1-5 directly from the ESS site by registering their email.
# https://ess.sikt.no/en/data-builder/
# ESS_unweighted<- read.csv("ESS1-ESS5.csv", stringsAsFactors=FALSE, fileEncoding="latin1")

##convert ESSround to factor variable
ESS_unweighted$essround <-as.factor(ESS_unweighted$essround)

ESS_unweighted$ESS_year[ESS_unweighted$essround==1] <- 2002
ESS_unweighted$ESS_year[ESS_unweighted$essround==2] <- 2004
ESS_unweighted$ESS_year[ESS_unweighted$essround==3] <- 2006
ESS_unweighted$ESS_year[ESS_unweighted$essround==4] <- 2008
ESS_unweighted$ESS_year[ESS_unweighted$essround==5] <- 2010

# First we run the Hungary analysis, 2002-2010. Here, we just use the post-stratification weight single it's a single country.
ESS_pspwghtweighted_survey <- ESS_unweighted %>%
  as_survey_design(ids = idno,
                   strata = cntry,
                   nest = TRUE,
                   weights =pspwght)

ESS_hu <-ESS_pspwghtweighted_survey %>% 
  filter(cntry=="HU") 

ESS_protest_hu <- ESS_hu %>% 
  srvyr::group_by(pbldmn, lrscale) %>% 
  srvyr::summarize(surveycount = survey_total())%>% 
  mutate(pbldmn= ifelse(pbldmn > 2, NA, pbldmn))%>% 
  mutate(lrscale= ifelse(lrscale > 10, NA, lrscale))

ESS_protesthu_means <- ESS_protest_hu %>%  
  srvyr::group_by(lrscale) %>% 
  dplyr::mutate(perc = 100*surveycount/sum(surveycount))%>% 
  filter(pbldmn==1) 

protest_Hun_aggyear <- ggplot(data= ESS_protesthu_means, aes(x=lrscale, y=perc)) +
  geom_line() +
  geom_point(size=3)+
  scale_color_viridis(discrete = TRUE) +
  ggtitle("") +
  theme_classic(base_size = 12) +
  ylab("% who report protesting in the last year")+
  xlab("Ideology (left-right)")+
  ylim(0,15)+
  scale_x_continuous(limits = c(0, 10),
                       breaks = seq(0,10,1))
protest_Hun_aggyear 

# Second, we run the rest of Eastern EU analysis, 2002-2010. 
# Here, we combine the population weight and post-stratification weight single it's multi-country.
ESS_unweighted$full_weight <- ESS_unweighted$pweight*ESS_unweighted$pspwght

ESS_fullweighted_survey <- ESS_unweighted %>%
  as_survey_design(ids = idno,
                   strata = cntry,
                   nest = TRUE,
                   weights = full_weight)

Western <- c("AT", "BE", "DE", "DK", "ES", "FI", "FR", "GB", "GR","IE", "IT", "LU", "NL", "PT", "SE") 
Eastern <- c("BG","CY", "CZ", "EE", "HR", "HU", "LT", "LV",  "PL","RO", "SI", "SK")

ESS_all <- ESS_fullweighted_survey %>%
  dplyr::mutate(Region = case_when(
    cntry %in% Western ~ "Western Europe",
    cntry %in% Eastern ~ "Eastern Europe"
  )) %>%
  filter(!is.na(Region)) 

ESS_protestall <- ESS_all %>% 
  srvyr::group_by(pbldmn, cntry, lrscale) %>% 
  srvyr::summarize(surveycount = survey_total())%>% 
  mutate(pbldmn= ifelse(pbldmn > 2, NA, pbldmn))%>% 
  mutate(lrscale= ifelse(lrscale > 10, NA, lrscale))

ESS_protestall_means <- ESS_protestall %>%  
  srvyr::group_by(lrscale, cntry) %>% 
  dplyr::mutate(perc = 100*surveycount/sum(surveycount))%>% 
  filter(pbldmn==1) %>% 
  filter(cntry!="HU")  

ESS_protest_EastEU<- ESS_protestall_means %>%
  mutate(Region = case_when(
    cntry %in% Western ~ "Western Europe",
    cntry %in% Eastern ~ "Eastern Europe",
  ))%>% 
  filter(Region=="Eastern Europe")

ESS_protest_EastEU_aggyear<- ESS_protest_EastEU   %>% 
  srvyr::group_by(lrscale) %>% 
  dplyr::summarise(mean_aggprotest= mean(perc))

protest_EastEU_aggyear <- ggplot(data=ESS_protest_EastEU_aggyear, aes(x=lrscale, y=mean_aggprotest)) +
  geom_line() +
  geom_point(size=3)+
  scale_color_viridis(discrete = TRUE) +
  ggtitle("") +
  theme_classic(base_size = 12) +
  ylim(0,15)+
  scale_x_continuous(limits = c(0, 10),
                     breaks = seq(0,10,1))+
  ylab("% who report protesting in the last year")+
  xlab("Ideology (left-right)")
protest_EastEU_aggyear 


ESS_Eastaggyearly <- annotate_figure(
  ggarrange(
    protest_Hun_aggyear, protest_EastEU_aggyear,
    labels = c("Hungary (2002-2010)", "all other Eastern EU (2002-2010)"),
    ncol = 2, nrow = 1,
    common.legend = FALSE,
    font.label = list(size = 14, color = "black", face = "plain"),
    legend = "right"
  ),
  bottom = text_grob(
    paste("Source: European Social Survey, Rounds 1-5"),
    size = 10,
    hjust = 0,
    x = unit(8.5, "pt")
  )
)

ESS_Eastaggyearly
ggsave(ESS_Eastaggyearly, file="./Figure1.pdf", height = 6, width = 12, dpi = 250)


