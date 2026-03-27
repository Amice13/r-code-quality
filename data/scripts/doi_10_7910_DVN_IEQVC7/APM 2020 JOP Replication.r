### The Burden of Election Logistics
### Election Ballots and the Territorial Influence of Party Machines in Colombia

### The Journal of Politics, forthcoming.

### Santiago Alles  <santiago.alles@voxel-consulting.com>
### Monica Pachon   <mopachon@uniandes.edu.co>
### Manuela Munoz   <manuela.munozf@tamu.edu>

### updated: 04/20/2020


### Preliminary:
### Install needed R-packages

r_packs <- c( 'tidyverse', 'dplyr', 
              'betareg', 'emmeans', 'lme4', 'arm', 'optimx', 
              'stargazer', 
              'cowplot', 'ggplot2', 'ggsci', 
              'rgdal', 'maptools', 'knitr')

if(T %in% (r_packs %in% ls())) install.packages(r_packs)

rm(r_packs)



### Loading
### Packages

require(dplyr)



rm(list = ls())



### Setting
### Directories

## Main Directory
dir <- ''       ## set directory path here


## Output :: Results & Figures
mod_dir  <- paste(dir, "Model Results", sep = "/")
plot_dir <- paste(dir, "Figures", sep = "/")

dir.create( mod_dir,  recursive = T, showWarnings = F)
dir.create( plot_dir, recursive = T, showWarnings = F)


### Load
### Data

## Concentration Data
G_idx_store <- tbl_df(read.csv(paste(dir, 'APM_GiniIdx_RepData.csv', sep = '/')))

## Party Performance Data (selected examples)
LQ_idx_store <- tbl_df(read.csv(paste(dir, 'APM_LQIdx_RepData.csv', sep = '/')))


## Vote Waste Data
wVote_dpt_store <- tbl_df(read.csv(paste(dir, 'APM_wVotes_dpt_RepData.csv', sep = '/')))
wVote_mjP_store <- tbl_df(read.csv(paste(dir, 'APM_wVotes_mjP_RepData.csv', sep = '/')))




### Running
### Analysis

## Years
year_min <- 1970
year_max <- 2002

## Departments
dept_filter <- 11                  ## excludes Bogota, DC


## Model estimation
source(paste(dir, 'apm_jop_models.r', sep = '/'))

# Article table:   Table 1
# Appendix tables: Table III-1, Table III-2, Table III-3, Table IV-1

# Article figure:  Figure 3
# Appendix figure: Figure III-1



### Descriptive
### Plots

## (a) Article Figures :: Figure 5
source(paste(dir, 'apm_jop_descPlot_1.r', sep = '/' ))

## (b) Article Maps :: Figure 4
source(paste(dir, 'apm_jop_descMap_1.r', sep = '/' ))

## (c) Appendix Figures :: Figure II-1
source(paste(dir, 'apm_jop_descPlot_2.r', sep = '/' ))


### Appendix
### Tables

## Appendix Tables :: Table I-1
source(paste(dir, 'apm_jop_appTab.r', sep = '/' ))





