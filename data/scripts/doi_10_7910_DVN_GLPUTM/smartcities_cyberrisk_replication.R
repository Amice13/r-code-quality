rm(list=ls())
library(ggplot2)
library(plyr)
library(tidyr)
library(stringr)
library(anytime)
library(dplyr)
library(comperank)
library(reshape2)
library(gplots)
library(PMCMRplus)
library(RColorBrewer)
library(PMCMRplus)
library(dunn.test)

# Replication code for: "How do Cyber-risks Vary Across Smart City Technologies?"
# Published in the Journal of Urban Technology
# Authors: Giselle Mendonça Abreu, Alexandra Pan, Alison E. Post, Nathan Malkin, and Karen Trapenberg Frick


# Type in filename and path
file <- "YOUR_FILE_LOCATION"

# CLEAN DATA
raw_data <- read.csv(file, header = T, stringsAsFactors = F)
info <- raw_data[1:2,]
df <- raw_data[3:nrow(raw_data),]

# Limit dataset to respondents who completed 75% or more of survey
df$Progress <- as.numeric(df$Progress)
df <- df[df['Progress'] > 75,]

# Remove respondents who did not complete the familiarity section (i.e., we do not know what their familiarity with each of the cybersecurity technologies is)
df <- df[df['familiarity_1'] != "",]

# List of technologies in the survey
tech_list <- c('Street Video Surveillance', 'Gunshot Detection','Emergency or Security Alerts','Public Transit Open Data',
               'Smart Traffic Lights/Signals','Smart Tolling','Water Consumption Tracking',
               'Satellite Water Leakage Detection','Smart Waste or Recycling Bins')

### Sources ###
# Table A.2
source <- df %>% group_by(source) %>% summarise(n = n())
source[order(source$n, decreasing = TRUE),]

##########################
### ANALYSIS FUNCTIONS ### Run analysis functions to be used later
##########################

# Create matrix-type plot for familiarity question

# Calculate percentage of respondents for each combination
# of familiarity and type of technology

calc_pct <- function(df, tech, num_level, text_level, returnList) {
  m <- vector()
  all_n <- vector()
  for (i in 1:length(df)){
    temp <- vector()
    for (j in 1:length(num_level)){
      temp[j] <- round(sum(df[i] == num_level[j])/dim(df)[1]*100,0)
    }
    curr_tech <- rep(tech[i],times=length(num_level))
    n_tech <- sum(df[i] != "")
    m_add <- data.frame(x = text_level, y = curr_tech, pct = temp, n = n_tech)
    m <- rbind(m, m_add)
    all_n <- rbind(all_n, n_tech)
  }
  
  returnList <- list("m" = m, "all_n" = all_n)
  return(returnList)
}

# Create matrix-style plot

plot_matrix <- function(df, title, labels, tech) {
  # Add n to labels
  tech_labels <- vector()
  for (i in 1:length(tech)){
    curr_label <- paste(tech[i], "\n(N = ", df$all_n[i], ")", sep = "")
    tech_labels <- cbind(tech_labels, curr_label)
  }
  
  plot <- ggplot(df$m, aes(x = x, y = factor(y,levels = tech_list))) + 
    geom_tile(aes(fill = pct)) + 
    geom_text(aes(label = paste(pct, "%", sep = "")), size = 3.5) +
    scale_fill_distiller(palette = "Blues", direction = 1) +
    scale_x_discrete(limits = labels,
                     position = 'top') +
    scale_y_discrete(labels = tech_labels) +
    ggtitle(title) +
    theme(axis.text.x = element_text(angle = 30, hjust = 0, size = 11),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5))
  return(plot)
}

# Function to compute Markov ranking
# Used for vulnerability and impactful survey questions

markov_ranking <- function(df, tech_list){
  # Below is the scores or voting matrix
  # Records the number of times each technology is ranked as more vulnerable or more impactful than another
  scores <- matrix(0,9,9)
  for (i in 1:9){
    for (j in 1:9){
      n <- sum(!is.na(df[,i]) & !is.na(df[,j]))
      scores[i,j] <- round(length(which(df[,i] < df[,j]))/n*100,2)
    }
  }
  dimnames(scores) <- list(tech_list, tech_list)
  
  scores_prob <- prop.table(scores,2)
  
  # Markov algorithm
  r <- Re(eigen(scores_prob)$vectors[,1])
  norm_r <- r / sum(r)
  
  ranking <- data.frame(tech = tech_list, rank = norm_r)
  ranking <- ranking[order(ranking$rank),]
  
  return(ranking)
}

markov_score <- function(df, tech_list){
  scores <- matrix(0,9,9)
  for (i in 1:9){
    for (j in 1:9){
      n <- sum(!is.na(df[,i]) & !is.na(df[,j]))
      scores[i,j] <- round(length(which(df[,i] < df[,j]))/n*100,2)
    }
  }
  dimnames(scores) <- list(tech_list, tech_list)
  
  scores_prob <- prop.table(scores,2)
  
  return(scores)
}


# Demographics plots

# Calculate percentage of respondents for each demographic
demo_pct <- function(df, col_name, num_level, text_level, summary_df) {
  col_number <- which(colnames(df) == col_name)
  temp <- vector()
  for (i in 1:length(num_level)){
    temp_add <- round(sum(df[,col_number] == num_level[i])/dim(df)[1]*100,0)
    temp <- rbind(temp, temp_add)
  }
  
  summary_df <- data.frame(x = text_level, pct = temp)
  
  return(summary_df)
}

# Create demographics plot
demo_plot <- function(summary_df, title, label_height, sort_responses){
  
  if (sort_responses == 1){
    summary_df <- summary_df[order(-summary_df$pct),]
    summary_df$x <- factor(summary_df$x, levels = unique(summary_df$x))
    
    plot <- ggplot(summary_df, aes(x = x, y = pct)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste(pct, "%", sep = "")), size = 3.5, nudge_y = label_height) +
      ggtitle(title) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 12),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_text(hjust = 0.5))
  }else{
    summary_df$x <- factor(summary_df$x, levels = unique(summary_df$x))
    
    plot <- ggplot(summary_df, aes(x = x, y = pct)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste(pct, "%", sep = "")), size = 3.5, nudge_y = label_height) +
      ggtitle(title) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 12),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_text(hjust = 0.5))
  }
  
  return(plot)

}


###################
### Familiarity ###
###################

# Select columns with data for familiarity question

fam <- df %>% 
  dplyr:: select(starts_with("familiarity")) %>%
  mutate_all(as.numeric)

# Set column names to be the types of technology
colnames(fam) <- tech_list

# Re-code response numbers to qualitative familiarity 
fam_number <- as.character(c(1:5))
fam_level <- c('Not familiar','Heard of','Know how it works','Worked with but\nnot on security','Worked on security')
fam_labels <- c('1' = 'Not familiar', '2' = 'Heard of', '3' = 'Know how it works',
                '4' = 'Worked with but\nnot on security', '5' = 'Worked on security')

# Use calc_pct function defined earlier
# fam_df$m is a matrix where each row represents a level of familiarity, type of technology
# percentage of respondents, and total number of survey respondents
fam_df <- calc_pct(fam, tech_list, fam_number, fam_level)

# Use plot_matrix function defined earlier to create plot
# This plot is basis of last paragraph in paper section titled "Survey with cybersecurity experts"
fam_plot <- plot_matrix(fam_df, 'Familiarity with Technology', fam_labels, tech_list)
fam_plot

#####################
### Vulnerability ###
#####################

# Select columns with data for technical vulnerability question

vul <- df %>%
  dplyr:: select(starts_with("vulnerability")) %>%
  mutate_all(as.numeric)
vul <- vul[1:9]

colnames(vul) <- tech_list

# Calculate Markov ranking using markov_ranking function defined above
ranking_vul <- markov_ranking(vul,tech_list)

# Rank in descending order
ranking_vul_desc <- ranking_vul[order(ranking_vul$rank, decreasing = TRUE),]

# Markov score - create matrix with head-to-head comparisons
# Summarizes the comparative vulnerability of each pair of cybersecurity technologies
# Uses markov_score function defined earlier 

ranking_score <- as.data.frame(markov_score(vul,ranking_vul_desc$tech))

# Reformat ranking_score dataframe to make a nice matrix plot
melt_rankingscore <- melt(ranking_score)
melt_rankingscore$tech <- rep(row.names(ranking_score),9)
melt_rankingscore$tech <- factor(melt_rankingscore$tech, levels = ranking_vul_desc$tech)
melt_rankingscore$variable <- factor(melt_rankingscore$variable, levels = ranking_vul_desc$tech)

vul_matrix_plot <- ggplot(melt_rankingscore, aes(tech,factor(variable,levels = rev(ranking_vul_desc$tech)))) +
                    geom_tile(aes(fill = value), color='white') +
                    geom_text(aes(label = paste(round(value), "%", sep = "")), size = 5.5) +
                    scale_fill_distiller(palette = 'Blues', direction = 1, limits = c(15,85)) + 
                    scale_x_discrete(labels = ranking_vul_desc$tech, position = 'top') +
                    theme(axis.text.x = element_text(angle = 30, hjust = 0, size = 16),
                          axis.text.y = element_text(size=16),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank(),
                          plot.margin = margin(0.5,4.5,0.5,0.5,'cm'))

# View vulnerability matrix plot
# This is FIGURE 1 in the paper
vul_matrix_plot

# Conduct Dunn test for statistical significance of vulnerability rankings
# This is the basis of Table A.3

vul$ranker <- c(1:nrow(vul))

vul_dunn <- list(vul[,1][!is.na(vul[,1])],
                 vul[,2][!is.na(vul[,2])],
                 vul[,3][!is.na(vul[,3])],
                 vul[,4][!is.na(vul[,4])],
                 vul[,5][!is.na(vul[,5])],
                 vul[,6][!is.na(vul[,6])],
                 vul[,7][!is.na(vul[,7])],
                 vul[,8][!is.na(vul[,8])],
                 vul[,9][!is.na(vul[,9])])

vul_dunn_results <- dunn.test(x = vul_dunn, wrap = TRUE, method = 'bonferroni')


##############
### Impact ###
##############

# Select columns with data for impact question

impact <- df %>%
  dplyr:: select(starts_with("impact")) %>% mutate_all(as.numeric)
impact <- impact[1:9]
colnames(impact) <- tech_list

# Calculate Markov ranking using markov_ranking function defined above
ranking_impact <- markov_ranking(impact,tech_list)

# Normalize by number of technologies
num_tech <- 9-rowSums(is.na(impact))
impact <- 1-impact/num_tech
impact.s <- stack(impact)

# Rank in descending order
ranking_impact_desc <- ranking_impact[order(ranking_impact$rank, decreasing = TRUE),]

# Markov score - create matrix with head-to-head comparisons
# Summarizes the comparative impact of attack for each pair of cybersecurity technologies
# Uses markov_score function defined earlier 
ranking_score <- as.data.frame(markov_score(impact,ranking_impact_desc$tech))

# Format data to make matrix-style plot
melt_rankingscore <- melt(ranking_score)
melt_rankingscore$tech <- rep(row.names(ranking_score),9)
melt_rankingscore$tech <- factor(melt_rankingscore$tech, levels = ranking_impact_desc$tech)

impact_matrix_plot <- ggplot(melt_rankingscore, aes(variable,factor(tech,levels = rev(ranking_impact_desc$tech)))) +
  geom_tile(aes(fill = value), color='white') + 
  geom_text(aes(label = paste(round(value), "%", sep = "")), size = 6.5) +
  scale_fill_distiller(palette = 'Blues', direction = 1, limits = c(10,90)) + 
  scale_x_discrete(labels = ranking_impact_desc$tech, position = 'top') +
  theme(axis.text.x = element_text(angle = 30, hjust = 0, size = 16),
        axis.text.y = element_text(size=16),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(0.5,4.5,0.5,0.5,'cm'))

# View impact matrix plot
# This is FIGURE 2 in the paper
impact_matrix_plot

# Conduct Dunn test for statistical significance of impact rankings
# This is the basis of Table A.4

impact_dunn <- list(impact[,1][!is.na(impact[,1])],
                    impact[,2][!is.na(impact[,2])],
                    impact[,3][!is.na(impact[,3])],
                    impact[,4][!is.na(impact[,4])],
                    impact[,5][!is.na(impact[,5])],
                    impact[,6][!is.na(impact[,6])],
                    impact[,7][!is.na(impact[,7])],
                    impact[,8][!is.na(impact[,8])],
                    impact[,9][!is.na(impact[,9])])

impact_dunn_results <- dunn.test(x = impact_dunn, wrap = TRUE, method = 'bonferroni')

#####################
### Effectiveness ###
#####################

threat_actors <- c('Insider threats','Thrill-seekers','Cybercriminals',
                   'Terrorist groups','Hacktivists','Nation-states')

# Select columns with data for threat actor effectiveness question
eff <- df %>% 
  dplyr:: select(starts_with("effectiveness")) %>% mutate_all(as.numeric)

eff <- eff[c(1:6)]/10
colnames(eff) <- threat_actors

# Create box plot for effectiveness responses
eff.s <- stack(eff)

eff_plot <- ggplot(eff.s, aes(x= reorder(ind,values,median, na.rm=TRUE), y = values)) +
  geom_boxplot() + 
  coord_flip() +
  labs(y="Effectiveness (0=less effective, 1=more effective)") +
  theme(axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        plot.title = element_text(hjust = 0.5))

# View box plot
# This data is the basis of the first paragraph of the "Threat actors" paper section
eff_plot 

# Friedman test for statistical significance
# This is used for Table A.5

eff$ranker <- c(1:nrow(eff))
eff_long <- pivot_longer(eff, cols = 1:6)

friedman.test(value ~ name | ranker, data = eff_long)

# Post-hoc test
ph_eff <- frdAllPairsNemenyiTest(value ~ name | ranker, data = eff_long)
pval_eff <- as.data.frame(round(ph_eff$p.value, digits=4))

pval_eff[(pval_eff < 0.01)] <- '***'
pval_eff[(pval_eff < 0.05) & (pval_eff > 0.01)] <- '**'
pval_eff[(pval_eff < 0.1) & (pval_eff > 0.05)] <- '*' 

#############################
### Threat Actor Interest ### 
#############################

# Select columns with data for threat actor interest question

interest <- df %>% 
  dplyr:: select(starts_with("interest"))

# Recode actors
actor_number <- c(1,2,5,6,7,8,9)
actors <- c(threat_actors, 'No one')

# Respondents could select more than one threat actor for each technology
# The loop below splits each response into individual threat actors
# Then counts how many times threat actors were selected for each technology

pct <- vector()

for (i in 1:length(interest)){
  curr_interest <- str_split(interest[,i], ',')
  all_actors <- as.numeric(unlist(curr_interest))
  
  temp <- vector()
  for (j in 1:7){
    temp[j] <- round(sum(all_actors == actor_number[j], na.rm = T)/dim(df)[1]*100,2)
  }
  pct <- cbind(pct,temp)
}

dimnames(pct) <- list(actors,tech_list)

# Creates matrix where each row is a threat actor, technology, and percentage of respondents selecting that combination
melt_pct <- melt(pct)

actors <- factor(actors, levels = actors)

# Create matrix plot
actorint_plot <- ggplot(melt_pct,aes(Var2, Var1))+ 
  geom_tile(aes(fill = value)) + 
  geom_text(aes(label = paste(value, "%", sep = "")), size = 3.5) +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  scale_x_discrete(labels = tech_list,
                   position = 'top') +
  scale_y_discrete(limits = rev(levels(actors))) +
  theme(axis.text.x = element_text(angle = 30, hjust = 0, size = 11),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_blank())

# View matrix plot - this is the basis for Table 2
actorint_plot

####################
### Demographics ###
####################

# Employer
employer <- c('Public sector,\nfederal','Public sector,\nstate','Public sector,\nlocal',
              'Private sector', 'Not-for-profit','Self-employed or\nindependent contractor',
              'Educational institution',"Don't know",'Other')
employer_number <- as.character(c(1:9))

emp_pct <- demo_pct(df, 'employer', employer_number, employer)

emp_plot <- demo_plot(emp_pct, "Primary Employer", 2, 1)

emp_plot

# Employer industry
industry <- c('Utility','Hardware design\nand manufacture','Software','Telecommunications',
              'Information Services\nand Data Processing','Consulting','Manufacturing (general)',
              'Finance or insurance','Construction','Real estate','Retail','Wholesale trade',
              'Health care','Transportation','Other')
industry_number <- as.character(c(1:15))

ind_pct <- demo_pct(df, 'employer_industry', industry_number,industry)

ind_plot <- demo_plot(ind_pct,"Employment Industry", 0.5, 1)

ind_plot

# Employer size
emp_size <- c('Self-employed','1-49\nemployees','50-499\nemployees','500-999\nemployees','1,000-4,999\nemployees','5,000 or more\nemployees',"Don't know")
emp_size_num <- as.character(c(1:7))

emp_size_pct <- demo_pct(df, 'employer_size', emp_size_num, emp_size)
emp_size_plot <- demo_plot(emp_size_pct, "Employer Size", 1, 0)
emp_size_plot

# Does your job involve cybersecurity
job_cyb <- c('Yes','No','Other')
jobcyb_num <- as.character(c(1:3))

jobcyb_pct <- demo_pct(df, 'job_security', jobcyb_num, job_cyb)
jobcyb_plot <- demo_plot(jobcyb_pct, "Does your job involve (cyber)security?", 2, 0)
jobcyb_plot

# Age
age_lab <- c('17 or younger','18-24','25-34','35-44','45-54','55 or older')
age_num <- as.character(c(4:9))

age_pct <- demo_pct(df, 'age', age_num, age_lab)
age_plot <- demo_plot(age_pct, "Age", 1, 0)
age_plot

# gender
gender_lab <- c('Female','Male','Non-binary','Prefer not to answer','Prefer to self-describe')
gender_num <- as.character(c(1:5))

gender_pct <- demo_pct(df, 'gender', gender_num, gender_lab)
gender_plot <- demo_plot(gender_pct, "Gender", 2, 0)
gender_plot

# race
race_lab <- c('American Indian or\nAlaska Native','Asian or\nAsian American','Black or\nAfrican American',
              'Native Hawaiian or\nOther Pacific Islander','White/Caucasian','Multiracial or Other','Hispanic or\nLatino',
              'Prefer not to answer')
race_num <- as.character(c(1:8))

race_pct <- demo_pct(df, 'race', race_num, race_lab)
race_plot <- demo_plot(race_pct, "Race/Ethnicity", 2, 0)
race_plot