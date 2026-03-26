library(tidyverse)
library(ggplot2)
library(gridExtra)
library(grid)
library(gridtext)
library(stargazer)
library(magrittr)

#####################################
# Appendix D1                       #
#####################################

# Journal Impact Factor:
jf <- read.csv('../Data/LitReview/JIF.csv') %>%
  mutate_at(vars(matches('JIF$|Total')), ~as.numeric(gsub(',','',.x))) %>%
  mutate(inc = as.numeric(grepl('^(AMERICAN POLITICAL SCIENCE REVIEW|Journal of politics|american journal of political science|journal of experimental political science|international organization|COMPARATIVE POLITICAL STUDIES|political behavior|BRITISH JOURNAL OF POLITICAL SCIENCE|WORLD POLITICS|quarterly journal of political science)$',
                                Journal.name, ignore.case = T)))
# "these 10 journals together have total citations of over 7,800 on average compared to 1,315 average"
# Total citations of selected 10 journals:
round(mean(jf$Total.Citations[jf$inc==1]),0)
# Total citations of all journals:
round(mean(jf$Total.Citations),0)

# "the 5-year journal impact factor among these 10 journals is 5.8 on average"
# Journal Impact Factor of selected 10 journals: 
round(mean(jf$X5.Year.JIF[jf$inc==1], na.rm = T),1)
# Journal Impact Factor of all journals:
round(mean(jf$X5.Year.JIF, na.rm = T),1)

#####################################
# Appendix D1.1                     #
#####################################

multi <- read.csv('../Data/LitReview/MultisiteExp.csv')
raw <- multi %>%
  mutate(exp_type = ifelse(exp_type == '', NA, exp_type)) %>%
  mutate(exp_type = coalesce(exp_type, identification_strategy)) %>%
  separate(exp_type, paste0('v', 1:3), sep = ';', remove = F) %>%
  pivot_longer(v1:v3, values_to = 'type_s')  %>%
  drop_na(type_s) %>%
  mutate_if(is.character, trimws) %>%
  mutate(def  = str_to_title(ifelse(grepl('county|district', site_def), 'county/\ndistrict',
                                    ifelse(site_def == 'states', 'state',
                                           ifelse(grepl('school|region|province|district|chief', site_def), 'other', site_def)))),
         type_s = ifelse(grepl('lab', type_s), 'Lab',
                         ifelse(grepl('survey|conjoint|list', type_s), 'Survey',
                                ifelse(grepl('field|audit', type_s), 'Field',
                                       ifelse(grepl('natural exp', type_s, ignore.case = T), 'Natural Experiment', type_s)))),
         div_n = as.numeric(diversify_cov_n)) %>%
  arrange(type, type_s, year, author, journal) %>%
  filter(type_s != 'CI')
lastname <- stringr::str_extract_all(raw$author, '([a-zA-Z]+),')
raw$author_cln <- NA
for (i in 1:length(lastname)) {
  if (length(lastname[[i]])>2)  { raw$author_cln[i] <- paste(gsub(',', '', lastname[[i]][1]), 'et al.') }
  if (length(lastname[[i]])==1) { raw$author_cln[i] <- gsub(',', '', lastname[[i]][1]) }
  if (length(lastname[[i]])==2) { raw$author_cln[i] <- paste(gsub(',', '', lastname[[i]][1]), 'and', gsub(',', '', lastname[[i]][2])) }
}
raw$author_yr <- paste0(raw$author_cln, ' (', raw$year, ")")
raw$author_yr_doi <- paste0('\\href{https://www.doi.org/', raw$doi, '}{', raw$author_yr, '}')

toprint <- function(t, exptype){
  x <- as.data.frame(raw[which(raw$type == t & raw$type_s==exptype), 'author_yr_doi'])
  names(x) <- c("Authors (Year; Journal)")
  rownames(x) <- NULL
  x$n <- 1:nrow(x)
  x$combined <- paste0(x$n, ' & ', x$`Authors (Year; Journal)`, ' & ', x$Title, ' \\\\ \n')
  print(cat(x$combined))
}

#####################################
# Table OA-3                        #
#####################################

toprint('Experimental', 'Survey')
toprint('Experimental', 'Field')
toprint('Experimental', 'Lab')

#####################################
# Table OA-4                        #
#####################################

toprint('Observational', 'DID')
toprint('Observational', 'IV')
toprint('Observational', 'RDD')
toprint('Observational', 'Natural Experiment')

#####################################
# Appendix D2                       #
#####################################

# Total number of multi-site studies
nrow(multi)

# Number of studies using random sampling
sum(multi$random_sample)

# % of studies relying on purposive sampling
x <- addmargins(table(multi$type[multi$justify_sample==1], multi$diversification[multi$justify_sample==1]))
round(x[3,2] / x[3,3] * 100, -1)

# % of studies implemented at the country-level
round(100*mean(multi$site_def == 'country'))

# Median and 80th percentile number of study sites
q = quantile(multi$site_n, probs = seq(0.1, 0.9, 0.1))
q[5]
q[8]

#####################################
# Figure OA-7                      #
#####################################

vars  <- c('def', 'site_n')
labs  <- c('Geographic unit', 'Number of study sites')
comb <- list()
for (v in 1:2){
  temp <- raw[which(!is.na(raw[[vars[v]]])),] %>%
    filter(!(exp_type == 'audit; survey' & type_s == 'Field')) %>%
    distinct(type, author_yr, journal, doi, .keep_all = T)
  temp$xvar <- temp[[vars[v]]]
  temp <- temp %>%
    drop_na(xvar) %>%
    group_by(type, xvar) %>%
    summarize(n = n()) %>%
    group_by(xvar) %>%
    mutate(pct = paste0(round(100* (sum(n) / nrow(temp)), 0), '%'),
           texty = sum(n))
  if (vars[v] == 'def') temp %<>% mutate(xvar = factor(xvar, levels = c('Other', 'County/\nDistrict', 'City', 'State', 'Country')))
  if (vars[v] == 'site_n') temp %<>% group_by(type) %>% complete(xvar = 2:27) %>% mutate(xvar = factor(xvar, levels = c(182, 181, 50, 48, 27:2)))
  
  temp$type <- factor(temp$type, levels = c('Observational', 'Experimental'))
  nudge <- ifelse(vars[v] == 'def', 4, 2.9)
  
  comb[[vars[v]]] <-
    ggplot(data = temp, aes(x = xvar, y = n, group = type, fill = type)) +
    geom_col(position = 'stack') +
    xlab(labs[v]) + ylab('Frequency counts') +
    coord_flip() +
    scale_fill_manual(name = '', values = c('red', 'blue')) +
    geom_text(aes(label = pct, y = texty),
              color = 'black',
              nudge_y = nudge,
              size = 2.7) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          legend.position = 'bottom',
          legend.title = element_blank(),
          axis.text = element_text(color = 'black'),
          axis.title = element_text(color = 'black'))
}


pdf('../Figures/Figure OA-7.pdf', height = 5, width = 4.7)
print(comb$def + theme(legend.position = c(0.8, 0.12),
                       legend.background = element_rect(color = 'black',linewidth = 0.2)))
print(comb$site_n + xlab('Number of Study Sites') +theme(legend.position = 'none'))
dev.off()


