# --------------------------------------------------------------------- # 
# Federalism and Democratic Backsliding from a Comparative Perspective 
# Kaufman, Kelemen, Kolcak 
# Descriptive Stats
# --------------------------------------------------------------------- # 

rm(list = ls())

## set working directory, e.g. 
#setwd("~/Dropbox/fedbacksliding replication")

## Packages
library(dplyr)
library(ggplot2)
library(gghighlight)
library(gridExtra)
library(stringr)
library(reshape2)
library(ggthemes)
library(pander)
library(cowplot)
library(ggpubr)

## Load the main dataset
dems_grand <- read.csv('dems_grand.csv')

#######################################
## Visualize final list of backsliders 
# Appendix 7 
#######################################

backsliders <- dems_grand %>% filter(backslider == 1)
backsliders_unique <- unique(backsliders$country_name)

plotsbacksliders <- dems_grand[dems_grand$country_name %in% backsliders_unique,]
plotsbacksliders <- plotsbacksliders[order(plotsbacksliders$country_name), ]

plots <- list()

for (i in unique(plotsbacksliders$country_name)) {
  assign(paste0("dat"), plotsbacksliders[plotsbacksliders$country_name == i, ])
  
  assign(paste0("Highlight"), dat[dat$ld_sig2 >= 1, ] )
  
  if(nrow(Highlight) > 0){
    plots[[i]] <- ggplot(dat, aes(x = year , y = v2x_libdem)) +
      geom_point(aes(x = year , y = v2x_libdem),size = 0.2) +
      geom_point(data=Highlight , aes(x = year,y = v2x_libdem), colour = 'red', size=0.2) +
      geom_errorbar(aes(ymax = v2x_libdem_codehigh, ymin = v2x_libdem_codelow, width = 0.3)) +
      geom_errorbar(data=Highlight, aes(ymax = v2x_libdem_codehigh, ymin = v2x_libdem_codelow,
                                        colour = 'red', width = 0.3)) +
      scale_y_continuous(limits = c(0.00,1.00), breaks = seq (0.00, 1, by = 0.25)) +
      scale_x_continuous(breaks = seq(1974, 2021, by = 5)) +
      theme_bw() +
      theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank()) +
      ggtitle(i) +   theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold")) +
      labs(y = "V-Dem LD Score", x = "Year") +
      theme(legend.position = "none") +
      theme(axis.text=element_text(size=9))
    
  }
  else{
    plots[[i]] <- ggplot(dat, aes(x = year , y = v2x_libdem)) +
      geom_point(aes(x = year , y = v2x_libdem),size = 0.2) +
      geom_errorbar(aes(ymax = v2x_libdem_codehigh, ymin = v2x_libdem_codelow, width = 0.3)) +
      scale_y_continuous(limits = c(0.00,1.00), breaks = seq (0.00, 1, by = 0.25)) +
      scale_x_continuous(breaks = seq(1974, 2019, by = 5)) +
      theme_bw() +
      theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank()) +
      ggtitle(i) +   theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold")) +
      labs(y = "V-Dem LD Score", x = "year") +
      theme(legend.position = "none") +
      theme(axis.text=element_text(size=9))
  }
}

export <- marrangeGrob(plots, nrow = 3, ncol = 3,
                       top = quote(paste("Final List of Backsliders \n Movements of Liberal Democracy Index (LDI), Democracies (N=21)")) ,
                       bottom = quote(paste(npages)))


#ggsave("backsliders_final.pdf", export, width=11, height=8.5)

## Compute Regime change based on 0.5 ED threshold 

# For those included cases, we code them as undergoing either erosion,
# which we define as a decline in their liberal or electoral democracy
# classification while remaining democratic (ie. above either our liberal or electoral
# democracy thresholds); or reversion, which is backsliding to authoritarian
# rule (below the electoral democracy threshold).

dems_grand$regimechange <- ifelse(dems_grand$country_name %in% 
                                    c('Bolivia' , 'Brazil', 'Croatia', 'Dominican Republic',
                                      'Ecuador', 'Ghana', 'Greece', 'Poland', 'United States of America', 
                                      'Slovenia'), 'erosion',
                                  ifelse(dems_grand$country_name %in% c('Benin', 'El Salvador', 
                                                                        'Hungary', 'India', 'Mali', 'Nicaragua',
                                                                        'Philippines', 'Poland', 'Serbia','Turkey', 'Venezuela', 'Zambia'), 'reversion',
                                         'nan'))

regime_change_erosion <- unique(dems_grand$country_name[dems_grand$regimechange == "erosion"])
regime_change_reversion <- unique(dems_grand$country_name[dems_grand$regimechange == "reversion"])

##########################
## General Descriptives
#########################

##################################################
## Q1: All democracies, federal vs. unitary states
## Main Text Table 1 
##################################################

all_democracies_Q1 <- dems_grand %>% group_by(country_name, fedvsuni, backslider)  %>%
  summarise(mean_democracy = mean(v2x_libdem))
table1 <- table(all_democracies_Q1$backslider, all_democracies_Q1$fedvsuni) # feduni vs. backslider table
prop_table1_column <- prop.table(table1,2)
round(prop_table1_column, digits = 2) # column percentages
## fisher's exact test 
fisher.test(table1)

##################################################
## Q4: Reversion to Autocracy 
## Main Text Quant Section 
##################################################
## Based on Haggard/Kaufman measure (electoral democrach threshold - 0.5)
dems_grand_change <- dems_grand %>% filter(regimechange == "erosion" | regimechange == "reversion") %>%
  filter(year == 2021)
all_democracies_Q2 <- dems_grand_change %>% group_by(fedvsuni, regimechange)  %>%
  summarise(total_count=n()) %>% 
  mutate(freq = total_count / sum(total_count)) %>% mutate(perc = total_count / sum(total_count)*100)
all_democracies_Q2 <- as_tibble(all_democracies_Q2) %>% mutate_at(vars(perc), funs(round(., 0)))
## fisher 
fisher.test(all_democracies_Q2$fedvsuni, all_democracies_Q2$total_count)

##################################################
## Q2: Severity of Backsliding 
# Appendix 3: Tables A7 & A8 
##################################################
## Brazil 2016 - 2021 
brazil_Q2 <- dems_grand %>% filter(country_name == "Brazil") %>% 
  dplyr::select(country_name, year, ld, ld_sig, backslider)
brazil_score_Q2 <- brazil_Q2$ld[42] - min(brazil_Q2$ld[43:48])
## India 2015 - 2021 
india_Q2 <- dems_grand %>% filter(country_name == "India") %>% 
  dplyr::select(country_name, year, ld, ld_sig, backslider, ed)
india_score_Q2 <- india_Q2$ld[41] - min(india_Q2$ld[42:48])
## United States 2017 - 2021 
us_Q2 <- dems_grand %>% filter(country_name == "United States of America") %>% 
  dplyr::select(country_name, year, ld, ld_sig, backslider)
us_score_Q2 <- us_Q2$ld[43] - min(us_Q2$ld[44:48])
## Venezuela 1999 - 2021 
venezuela_Q2 <- dems_grand %>% filter(country_name == "Venezuela") %>% 
  dplyr::select(country_name, year, ld, ld_sig, backslider)
venezuela_score_Q2 <- venezuela_Q2$ld[25] - min(venezuela_Q2$ld[26:48])
## Bolivia 2010 - 2021 
bolivia_Q2 <- dems_grand %>% filter(country_name == "Bolivia") %>% 
  dplyr::select(country_name, year, ld, ld_sig, backslider)
bolivia_score_Q2 <- bolivia_Q2$ld[36] - min(bolivia_Q2$ld[37:48])
## Croatia 2017 - 2019 
croatia_Q2 <- dems_grand %>% filter(country_name == "Croatia") %>% 
  dplyr::select(country_name, year, ld, ld_sig, backslider)
croatia_score_Q2 <- croatia_Q2$ld[26] - min(croatia_Q2$ld[27:29])
## Czech Republic 2018 - 2021 
czech_Q2 <- dems_grand %>% filter(country_name == "Czech Republic") %>% 
  dplyr::select(country_name, year, ld, ld_sig, backslider)
czech_score_Q2 <- czech_Q2$ld[44] - min(czech_Q2$ld[45:48])
## Dominican Republic 2002 - 2020 
dominican_Q2 <- dems_grand %>% filter(country_name == "Dominican Republic") %>% 
  dplyr::select(country_name, year, ld, ld_sig, backslider)
dominican_score_Q2 <- dominican_Q2$ld[28] - min(dominican_Q2$ld[29:47])
## Ecuador 2008 - 2017 
ecuador_Q2 <- dems_grand %>% filter(country_name == "Ecuador") %>% 
  dplyr::select(country_name, year, ld, ld_sig, backslider)
ecuador_score_Q2 <- ecuador_Q2$ld[34] - min(ecuador_Q2$ld[35:44])
## Hungary 2011 - 2021 
hungary_Q2 <- dems_grand %>% filter(country_name == "Hungary") %>% 
  dplyr::select(country_name, year, ld, ld_sig, backslider)
hungary_score_Q2 <- hungary_Q2$ld[37] - min(hungary_Q2$ld[38:48])
## Nicaragua 2006 - 2021 
nicaragua_Q2 <- dems_grand %>% filter(country_name == "Nicaragua") %>% 
  dplyr::select(country_name, year, ld, ld_sig, backslider)
nicaragua_score_Q2 <- nicaragua_Q2$ld[32] - min(nicaragua_Q2$ld[33:48])
## Macedonia 2011 - 2016 
macedonia_Q2 <- dems_grand %>% filter(country_name == "North Macedonia") %>% 
  dplyr::select(country_name, year, ld, ld_sig, backslider)
macedonia_score_Q2 <- macedonia_Q2$ld[20] - min(macedonia_Q2$ld[21:27])
## Philippines 2018 - 2021 
phil_Q2 <- dems_grand %>% filter(country_name == "Philippines") %>% 
  dplyr::select(country_name, year, ld, ld_sig, backslider)
phil_score_Q2 <- phil_Q2$ld[44] - min(phil_Q2$ld[45:48])
## Poland 2016 - 2021 
poland_Q2 <- dems_grand %>% filter(country_name == "Poland") %>% 
  dplyr::select(country_name, year, ld, ld_sig, backslider)
poland_score_Q2 <- poland_Q2$ld[42] - min(poland_Q2$ld[43:48])
## Serbia 2013 - 2021 
serbia_Q2 <- dems_grand %>% filter(country_name == "Serbia") %>% 
  dplyr::select(country_name, year, ld, ld_sig, backslider)
serbia_score_Q2 <- serbia_Q2$ld[39] - min(serbia_Q2$ld[40:48])
## Turkey 2010 - 2021 
turkey_Q2 <- dems_grand %>% filter(country_name == "Turkey") %>% 
  dplyr::select(country_name, year, ld, ld_sig, backslider)
turkey_score_Q2 <- turkey_Q2$ld[36] - min(turkey_Q2$ld[37:48])
## Zambia 2016 2021 
zambia_Q2 <- dems_grand %>% filter(country_name == "Zambia") %>% 
  dplyr::select(country_name, year, ld, ld_sig, backslider)
zambia_score_Q2 <- zambia_Q2$ld[42] - min(zambia_Q2$ld[43:48])
## Mali score 
mali_Q2 <- dems_grand %>% filter(country_name == "Mali") %>% 
  dplyr::select(country_name, year, ld, ld_sig, backslider)
mali_score_Q2 <-  mali_Q2$ld[44] - min(zambia_Q2$ld[45:48])
## fed vs. uni 
fed_Q2 <- c(brazil_score_Q2, india_score_Q2, us_score_Q2, venezuela_score_Q2)
unitary_Q2 <- c(bolivia_score_Q2, croatia_score_Q2, czech_score_Q2, 
             dominican_score_Q2, ecuador_score_Q2, hungary_score_Q2,
             nicaragua_score_Q2, macedonia_score_Q2, mali_score_Q2, phil_score_Q2, 
             poland_score_Q2, serbia_score_Q2, turkey_score_Q2, 
             zambia_score_Q2)
t.test(fed_Q2, unitary_Q2, var.equal = F)
## fed vs. uni (fed without Venezuela)
fed_Q2_wo_vez <- c(brazil_score_Q2, india_score_Q2, us_score_Q2)
t.test(fed_Q2_wo_vez, unitary_Q2, var.equal = F)

##################################################
## Q3: Speed of Backsliding 
##################################################
brazil_speed_Q3 <- brazil_score_Q2/6
india_speed_Q3 <- india_score_Q2/7
us_speed_Q3  <- us_score_Q2/5
venezuela_speed_Q3  <- venezuela_score_Q2/23
bolivia_speed_Q3  <- bolivia_score_Q2/12
croatia_speed_Q3  <- croatia_score_Q2/4
czech_speed_Q3  <- czech_score_Q2/4
dominican_speed_Q3  <- dominican_score_Q2/19
ecuador_speed_Q3  <- ecuador_score_Q2/10
hungary_speed_Q3  <- hungary_score_Q2/11
nicaragua_speed_Q3  <- nicaragua_score_Q2/16
macedonia_speed_Q3  <- macedonia_score_Q2/7
mali_speed_Q3  <- mali_score_Q2/6
phil_speed_Q3  <- phil_score_Q2/4
poland_speed_Q3  <- poland_score_Q2/6
serbia_speed_Q3  <- serbia_score_Q2/9
turkey_speed_Q3  <- turkey_score_Q2/12
zambia_speed_Q3  <- zambia_score_Q2/6

fed_Q3 <- c(brazil_speed_Q3, india_speed_Q3, us_speed_Q3, venezuela_speed_Q3)
uni_Q3 <-   c(bolivia_speed_Q3, croatia_speed_Q3, czech_speed_Q3, 
              dominican_speed_Q3, ecuador_speed_Q3, hungary_speed_Q3,
              nicaragua_speed_Q3, macedonia_speed_Q3, mali_speed_Q3, phil_speed_Q3, 
              poland_speed_Q3, serbia_speed_Q3, turkey_speed_Q3, 
              zambia_speed_Q3)
## fed vs. uni 
t.test(fed_Q3, uni_Q3, var.equal = F)

################################################
## Fig 1. Visualization of Federal Backsliders 
################################################

######
## US 
vdem12_desc_us <- dems_grand %>% 
  dplyr::select(country_name, year, ld, v2x_libdem,v2x_regime,v2x_libdem_codehigh,
         v2x_libdem_codelow, ld_sig2, regimechange, backslider, ed) %>% 
  filter(country_name == "United States of America") 

us_desc <- vdem12_desc_us %>% 
  ggplot(aes(x = year , y = v2x_libdem)) +
  geom_line(aes(x = year , y = v2x_libdem),linewidth = 0.4) +
  geom_ribbon(aes(ymin = v2x_libdem_codelow, ymax = v2x_libdem_codehigh), 
              linetype = 2, alpha=0.6, fill = "#F98400") + 
  scale_y_continuous(limits = c(0.00,1.00), breaks = seq (0.00, 1, by = 0.2)) +
  scale_x_continuous(breaks = seq(1974, 2021, by =5)) + theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) + 
  ggtitle('United States') + 
  ylab("Liberal Democracy Score") + xlab("Year") + 
  geom_vline(xintercept = 2017, linetype="dashed", col = 'black', alpha = 0.6)

###########
## Brazil 
vdem12_desc_brazil <- dems_grand %>% 
  dplyr::select(country_name, year, ld, v2x_libdem,v2x_regime,v2x_libdem_codehigh,
         v2x_libdem_codelow, ld_sig2, regimechange, backslider, ed) %>% 
  filter(country_name == "Brazil") 

brazil_desc <- vdem12_desc_brazil %>% 
  ggplot(aes(x = year , y = v2x_libdem)) +
  geom_line(aes(x = year , y = v2x_libdem),linewidth = 0.4) +
  geom_ribbon(aes(ymin=v2x_libdem_codelow, ymax= v2x_libdem_codehigh), 
              linetype = 2, alpha=0.6, fill = '#F98400') + 
  scale_y_continuous(limits = c(0.00,1.00), breaks = seq (0.00, 1, by = 0.2)) +
  scale_x_continuous(breaks = seq(1974, 2021, by =5)) + theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) + 
  ggtitle('Brazil') + 
  ylab("Liberal Democracy Score") + xlab("Year") + 
  geom_vline(xintercept = 2016, linetype="dashed", col = 'black', alpha = 0.6)

#############
## Venezuela 
vdem12_desc_venezuela <- dems_grand %>% 
  dplyr::select(country_name, year, ld, v2x_libdem,v2x_regime,v2x_libdem_codehigh,
         v2x_libdem_codelow, ld_sig2, regimechange, backslider, ed) %>% 
  filter(country_name == "Venezuela") 

vez_desc <- vdem12_desc_venezuela %>% 
  ggplot(aes(x = year , y = v2x_libdem)) +
  geom_line(aes(x = year , y = v2x_libdem), linewidth = 0.4) +
  geom_ribbon(aes(ymin = v2x_libdem_codelow, ymax = v2x_libdem_codehigh), 
              linetype = 2, alpha=0.6, fill = '#F98400') + 
  scale_y_continuous(limits = c(0.00,1.00), breaks = seq (0.00, 1, by = 0.2)) +
  scale_x_continuous(breaks = seq(1974, 2021, by = 5)) + theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) + 
  ggtitle('Venezuela') + 
  ylab("Liberal Democracy Score") + xlab("Year") + 
  geom_vline(xintercept = 1999, linetype="dashed", col = 'black', alpha = 0.6) +
  geom_vline(xintercept = 2003, linetype="solid", col = 'black', alpha = 0.6) 

#vdem12_desc_venezuela %>% dplyr::select(year, ed, ld, ld_sig2)

##########
## India 
vdem12_desc_india <- dems_grand %>% 
  dplyr::select(country_name, year, ld, v2x_libdem,v2x_regime,v2x_libdem_codehigh,
         v2x_libdem_codelow, ld_sig2, regimechange, backslider, ed) %>% 
  filter(country_name == "India") 

india_desc <- vdem12_desc_india %>% 
  ggplot(aes(x = year , y = v2x_libdem)) +
  geom_line(aes(x = year , y = v2x_libdem),linewidth = 0.4) +
  geom_ribbon(aes(ymin=v2x_libdem_codelow, ymax= v2x_libdem_codehigh), 
              linetype = 2, alpha=0.6, fill = '#F98400') + 
  scale_y_continuous(limits = c(0.00,1.00), breaks = seq (0.00, 1, by = 0.2)) +
  scale_x_continuous(breaks = seq(1974, 2021, by =5)) + theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) + 
  ggtitle('India') + 
  ylab("Liberal Democracy Score") + xlab("Year") + 
  geom_vline(xintercept = 2015, linetype="dashed", col = 'black', alpha = 0.6) +
  geom_vline(xintercept = 2019, linetype="solid", col = 'black', alpha = 0.6) 

## get all of them together in a four by four 
all_plots <- grid.arrange(us_desc, brazil_desc, vez_desc, india_desc,
                          nrow = 2)

