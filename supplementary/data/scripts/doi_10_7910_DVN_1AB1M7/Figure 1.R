library(tidyverse)
library(ggplot2)
library(gridExtra)
library(grid)
library(gridtext)
library(stargazer)
library(magrittr)

####################################
# Read in data
####################################

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

#####################################
# Figure 1                          #
#####################################

df <- read.csv('../Data/LitReview/OtherEmpiricalMethods.csv') %>%
  mutate(subtype = (gsub('Multi-Site|\\(|\\)', '', type)),
         subtype = ifelse(subtype == ' Experiments', ' Experimental', subtype),
         type = ifelse(grepl('Multi', type), 'Multi-Site Studies', type)) %>%
  group_by(year, type, subtype) %>%
  summarize(n = n())
df$type <- stringr::str_to_sentence(df$type)
label <- c('a', 'b', 'c', 'd', 'e', 'f')
text <- c('Multi-site studies', 'Conjoint analysis', 'Text analysis', 'Instrumental variable',
          'Regression discontinuity', 'Difference-in-difference')
for (i in 1:length(text)){
  df$type <- ifelse(df$type == text[i], paste0('(', label[i], ') ', text[i]), df$type)
}
df$type <- factor(df$type, levels = c('(a) Multi-site studies',
                                      '(b) Conjoint analysis',
                                      '(c) Text analysis',
                                      '(d) Instrumental variable',
                                      '(e) Regression discontinuity',
                                      '(f) Difference-in-difference'))
df$subtype <- factor(df$subtype, levels = c(' Observational', ' Experimental', levels(df$type)[2:6]))
types <- levels(df$type)
pcol  <- c('red', 'blue', rep('gray40', 5))
p <- list()
for (i in 1:length(types)){
    base <- df %>%
      filter(type == types[i]) %>%
      ggplot() +
      geom_col(aes(x = year, y = n, fill = subtype)) +
      ylab('') + xlab('') +
      scale_x_continuous(limits = c(1999, 2023), breaks = seq(2000, 2022, 5)) +
      scale_y_continuous(limits = c(0, 34), breaks = seq(0, 35, 5)) +
      facet_wrap(vars(type)) +
      theme_bw() +
      theme(axis.text = element_text(color = 'black'),
            axis.title = element_text(color = 'black'))
    tmargin = 18
    if (i <= 3){ tmargin = 0}
    if (i == 1){
      p[[paste0('p', i)]] <- base +
        scale_fill_manual(name = '', values = pcol[1:2]) +
        theme(legend.title = element_blank(),
              legend.justification = c(-0.1, 1.2),
              legend.position = c(0, 1),
              legend.box.margin=margin(c(10,10,10,10)),
              legend.background = element_rect(fill = "white", color = "white", linewidth = 0.4),
              panel.border = element_rect(fill = NA, color = 'black', linewidth = 1.1),
              strip.background = element_blank(),
              strip.text = element_text(color = 'black', size = 14, family = 'Times',
                                        margin = margin(t = tmargin, r = 0, b = 11, l = 0, unit = "pt")),
              plot.margin = unit(c(0, 0, -0.4, -0.4), "cm"),
              panel.margin = unit(c(0, 0, 0, 0), "cm"),
              panel.grid = element_blank())
    }
    else{
      p[[paste0('p', i)]] <- base +
        scale_fill_manual(name = '', values = pcol[i+1]) +
        theme(legend.position = 'none',
              plot.margin = unit(c(0, 0, -0.4, -0.4), "cm"),
              panel.margin = unit(c(0, 0, 0, 0), "cm"),
              panel.grid = element_blank(),
              strip.background = element_blank(),
              strip.text = element_text(color = 'black', size = 14, family = 'Times',
                                        margin = margin(t = tmargin, r = 0, b = 11, l = 0, unit = "pt")))
  }
}

pdf('../Figures/Figure 1.pdf', width = 8.5, height = 4.75)
grid.arrange(p[[1]] + theme(axis.text.x = element_text(color = 'white'),
                             axis.ticks.x = element_blank(),
                             plot.margin = unit(c(0, 0, -0.7, -0.4), "cm")),
             p[[2]] + theme(axis.text = element_text(color = 'white'),
                             axis.ticks = element_blank(),
                             plot.margin = unit(c(0, 0, -0.7, -0.47), "cm")),
             p[[3]] + theme(axis.text = element_text(color = 'white'),
                             axis.ticks = element_blank(),
                             plot.margin = unit(c(0, 0, -0.7, -0.47), "cm")),
             p[[4]] + theme(axis.text.x = element_text(size = 9.5)) ,
             p[[5]] + theme(axis.text.x = element_text(size = 9.5),
                            axis.text.y = element_text(color = 'white'),
                             axis.ticks.y = element_blank(),
                             plot.margin = unit(c(0, 0, -0.4, -0.47), "cm")),
             p[[6]] + theme(axis.text.x = element_text(size = 9.5),
                            axis.text.y = element_text(color = 'white'),
                             axis.ticks.y = element_blank(),
                             plot.margin = unit(c(0, 0, -0.4, -0.47), "cm")),
             nrow = 2,
             left = textGrob("Number of articles", rot = 90, gp = gpar(fontsize = 12)),
             bottom = textGrob("Year", gp = gpar(fontsize = 12)))
dev.off()


#####################################
# In-Text Analyses                  #
#####################################

# Articles Using Random Sampling:
# "only two studies use random sampling (less than 2% of all multi-site experiments we reviewed)"
x <- addmargins(table(multi$type, multi$random_sample))

# No. of articles using random sampling:
x[3,2]

# % of articles using random sampling: 
round(x[3,2] / x[3,3] * 100, 0)

# Median Number of Sampled Sites:
# "the median number of sites is 3 and the 80th percentile is 6.6"
sumdf <- NULL
for (t in c('All', unique(multi$type))){
  multi$exp_type <- ifelse(is.na(multi$exp_type), '', multi$exp_type)
  if (t == 'All') var <- as.numeric(multi[, 'site_n'])
  if (t != 'All') var <- as.numeric(multi[multi$type == t, 'site_n'])
  
  q = quantile(var, probs = seq(0.1, 0.9, 0.1))
  sumdf <- bind_rows(sumdf,
                     data.frame(type = t,
                                var = 'site_n',
                                mean = mean(var),
                                min = min(var),
                                max = max(var)) %>%
                       cbind(data.frame(t(q))))
}
sumdf %<>% dplyr::select(var, type, mean, min, X10.:X90., max) %>%
  arrange(var, type) %>%
  rename_at(vars(starts_with('X')), ~gsub('X', 'pct', .x)) %>%
  mutate(var = ifelse(var == 'site_n', 'No. of Sites', 'Site-Level Var'))

# Median number of sites: 
round(sumdf$pct50.[1],2)
# 80th percentile number of sites: 
round(sumdf$pct80.[1],2)

# Articles Justifying Site Selection via Diversification:
# "about 80% of multi-site experiments justify their site selection in this manner"
x <- addmargins(table(multi$type[multi$justify_sample==1], multi$diversification[multi$justify_sample==1]))

# % of articles justifying site selection: 
round(x[3,2] / x[3,3] * 100, -1)
    
# Average Number of Covariates Diversified:
# "the average number of covariates researchers diversify is 2.17"
multi_div <- multi[which(multi$diversification == 1), ]
x <- round(tapply(multi_div$diversify_cov_n, multi_div$type, mean),2)

# Average number of covariates diversified: 
round(mean(multi_div$diversify_cov_n),2)

# Studies with More than 10 Sites:
# "...whereas only 13% of multi-site experiments in political science have more than 10 sites", page 32
x <- addmargins(table(multi$type, multi$site_n>10))

# Share of studies with more than 10 sites: 
round(x[3,2] / x[3,3] * 100, 1)
