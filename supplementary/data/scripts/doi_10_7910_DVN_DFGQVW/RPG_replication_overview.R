########################
# Replication for "Who Answers for the Government?
# "Bureaucrats, Ministers, and Responsible Parties"
#   Max Goplerud and Dan Smith: 17 June 2021
#
########################

# Replication conducted under R 3.6.3 (Windows)
# And for R 4.0.2 (Windows)
# NEEDS dplyr 1.0.0 or higher

rm(list=ls())
sessionInfo()

# Please uncomment to install packages if necessary
# install.packages(c('dplyr', 'reshape2', 'stringr', 'plm', 'ggplot2', 'glue', 'ggpubr', 'xtable', 'readxl'))

# Load required packages
require(dplyr)
require(reshape2)
require(stringr)
require(plm)
require(ggplot2)
require(glue)
require(ggpubr)
require(xtable)
require(readxl)

# To replicate all tables and figures,
# please run this file. It will "source"
# all of the following scripts and write
# all figures into a "figures" subfolder.
# It assumes the data is stored in a "final_data" subfolder.

# final_data/ contains three .RDS files that can be loaded into modern version
# of R. They can be loaded using readRDS(...).
# Please see the codebook for full details of each column.
#   - markov_output.RDS (counts of speeches by committee/cabinet/chamber/triple)
#   - panel_mp_markov.RDS (counts of speeches  by committee/cabinet/chamber/MP/role)
#   - duo_participation.RDS (counts of meetings where minister/bureaucrats participated)
#   - The codebooks are in the same folder as the "R" scripts are labelled
#   codebook_<file>.pdf. The script to create them is also included.
# Note that:
#   'RPG_replication_aux_functions.R'
#   contains a variety of functions used to build and collapse
#   our data into cabinet/role level data
#   It contains the ability to create many different periodizations
#   of the data as per Table A.5

# Stop dplyr messages
options(dplyr.summarise.inform = FALSE)

# Shared options for formatting plots
theme.plot <- list(
  geom_point(position = position_dodge(width = .9)),
  geom_errorbar(position = position_dodge(width = .9)),
  geom_hline(aes(yintercept=0), col = 'black', linetype = 'dashed'),
  xlab('Time Period'),
  ylab('Change in Proportion'),
  scale_linetype_manual(values = c('solid', 'dashed')),
  theme_bw(),
  theme(legend.position = 'bottom', text = element_text(size = 12)),
  labs(pch = 'Chamber:', linetype = 'Chamber:') 
)

# Replicates the tables and figures in the paper
# Note that all scripts begin by clearing the environment except
# theme.plot

# Create the main tables/figures and some SI figures
# In order, create: Figures 1, A.1, 2, 4, A.2, A.6, A.14, 3
source(echo = TRUE, 'RPG_replication_main.R')

########
# Supporting Information
########

# Tables A.1-A.4

# This may need to be run by copy and pasting the code vs. source
# depending on the operating system.
source(echo = TRUE, 'RPG_replication_misc.R')
# The following line will work on Windows if the above fails to generate correct
# tables.
# eval(parse('RPG_replication_misc.R', encoding="UTF-8"))


# Figure A.1 - See replication_main
# Figure A.2 - See replication_main

# Figure A.3 - DV as "characters"
source(echo = TRUE, 'RPG_replication_character.R')

# Figures A.4 and A.5 - Effects by Cabinet
source(echo = TRUE, 'RPG_replication_time.R')

# Figures A.6 - See replication_main

# Figures A.7 and A.8 - Regression by standing committee
source(echo = TRUE, 'RPG_replication_standing.R')

# Figures A.9 and A.10 - Different periodizations (see Table A.5)
source(echo = TRUE, 'RPG_replication_periodization.R')

# Figures A.11 and A.12 - Content Analysis of Speeches
source(echo = TRUE, 'RPG_replication_content_analysis.R')

# Figure A.13 - Uses MP-level panel data version of data
source(echo = TRUE, 'RPG_replication_panel.R')

# Figure A.14 - See replication_main

# An analysis for a comment in the first footnote of the main text
source(echo = TRUE, 'RPG_replication_duo.R')