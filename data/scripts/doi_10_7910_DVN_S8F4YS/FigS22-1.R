## THIS SCRIPT OUTPUTS FIGURE S22, WHICH REPORTS CORRELATIONS BETWEEN HAVING A BLACK NEIGHBOR
## AT THE 10 DIFFERENT NEIGHBOR POSITIONS

rm(list=ls())
## LOAD PACKAGES
require(sandwich)
require(lmtest)
require(lfe)
require(gtools)
require(tidyverse)
require(stargazer)
require(reshape)
require(ggcorrplot)
require(GGally)
require(data.table)


## LOAD DATA


###################################
#### 2005/2009 SAMPLE ANALYSIS ####
###################################
load("02-data/befm-main-analysis.Rdata")
df = as.data.table(df)
df = df[(df$state.vfile == 'CA05' & age <=35) |(df$state.vfile == 'NC09' & age <=31),]



#### HELPER FUNCTIONS ####
remove_opposite <- function(x){
  return(gsub(x,pattern = "opposite1_",replacement = ""))
}

#### SELECT DATA ####
df_trim <- df %>%
  filter(race == 100) %>%
  select(opposite1_dist_1,opposite1_dist_2,opposite1_dist_3,
         opposite1_dist_4,opposite1_dist_5,opposite1_dist_6,
         opposite1_dist_7,opposite1_dist_8,opposite1_dist_8,
         opposite1_dist_9,opposite1_dist_10) %>%
  rename_all(remove_opposite)

#### CORRELATION MATRIX (OVERALL) ####

# estimate correlation
corr <- round(cor(df_trim), 2)

colnames(corr) <- paste0("K ",1:10)
rownames(corr) <- paste0("K ",1:10)

# plot it and highlight correlations greater than 0.5
source("01-code/r_utils.R")

corr_p <- ggcorr(corr, geom = "blank", label = TRUE, hjust = 0.75) +
  geom_point(size = 10,alpha = 0.05) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  guides(color = FALSE, alpha = FALSE)+
  theme_shom()+
  NULL
ggsave(corr_p,filename = "03-output/01-plots/FigS22.jpeg",
       dpi = 600, width = 8, height = 8 ,units = 'in')

