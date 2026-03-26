##########################################################################################################################

#   MANUSCRIPT 
#
#   Longitudinal analysis reveals high prevalence of Epstein-Barr Virus associated with multiple sclerosis,
#   VirScan Data Analyses                            

##########################################################################################################################

#   STUDY DETAILS
#
#   Design:           Nested case-control study within military cohort 
#   Study population: 60 individuals (30 cases, 30 matched controls) 
#   Serum samples:    Pre- and post-onset samples
#   Program:          Written by Marianna Cortese, last updated November 16, 2021

###########################################################################################################################

#   PROGRAM CONTENT
#
# 1. Load dataset and create file with metadata only 
# 2. Figure 4, A-D: Scatter plots of antibody binding (z-score) by peptide 
# 3. Figure 4 E-F: Analysis of case-control difference in positive hits by peptide and mapping to viral species
#           - Output Supplemental Table S1
#           - Output Supplemental Table S1
# 4. Figure 4 G-H: Analysis of case-control difference in positive hits by EBV antigens and mapping to viral species
# 5. Arrange Figure 4

###########################################################################################################################



library(tidyverse)
library(writexl)

######################################################
# 1. Load dataset and create file with metadata only 
######################################################

VirScan_dataset <- read_csv("Virscan_dataset.csv")

Metadata <- VirScan_dataset %>% 
  select(-id1, -casestat, -timeser, -zscore, -repnum) %>% 
  group_by(pep_id) %>% 
  slice_head(n = 1) %>% 
  mutate(pep_id = as.character(pep_id)) 

##########################################################################
# 2. Figure 4, A-D: Scatter plot of antibody binding (z-score) by peptide 
##########################################################################

# Analysis: a. Calculate person-and-peptide-specific mean z-score using identical sample replicates 
#           b. Calculate peptide-specific mean z-score in cases and controls 

########################### PRE-ONSET SAMPLES ###########################

pre_mean_z <- VirScan_dataset %>% 
  filter(timeser=="last") %>% 
  mutate(pep_id = as.character(pep_id)) %>% 
  group_by(casestat, id1, Species, serotype, Protein_name, pep_id) %>% 
  mutate(rep_mean_z_score = mean(zscore)) %>% 
  ungroup() %>% 
  filter(repnum==1) %>% #information double, so filter away one row
  group_by(casestat, pep_id, Protein_name, Species, serotype) %>% 
  summarise(pep_mean_z_score = mean(rep_mean_z_score)) %>% 
  ungroup() 

labels <- pre_mean_z %>%  # create labels for coloring of peptides with high z-scores
  filter(pep_mean_z_score>=30) %>% 
  group_by(Species) %>% 
  filter(n()>1) %>% 
  distinct(Species) 

mycols <- c("#009E73", "#0072B2", "#F0E442", "#999999", "#D55E00", "#56B4E9", "#E69F00", "#CC79A7", "black")

  #########
  # CASES #
  #########

A <- pre_mean_z %>% 
  filter(casestat == 1) %>% 
  mutate(
    pep_id = as.character(pep_id),
    ebv = if_else(Species =="Human herpesvirus 4", 1, 0),
    label = if_else(Species %in% labels$Species, Species, "Other")
  ) %>% 
  mutate(label = factor(label, levels=c("Enterovirus B", "Enterovirus C", "Human adenovirus C", "Human herpesvirus 1", 
                                        "Human herpesvirus 4", "Human respiratory syncytial virus", "Rhinovirus A",
                                        "Rhinovirus B", "Other"))) %>% 
  arrange(ebv, Species, Protein_name) %>% 
  mutate(sort = row_number()) %>% 
  ggplot(aes(x = sort, y=pep_mean_z_score, color = factor(label))) + 
  geom_point(size = 0.6) +
  scale_color_manual(values = mycols, labels=c("Enterovirus B", "Enterovirus C", "Human adenovirus C", "Human herpesvirus 1", 
             "Epstein-Barr virus", "Human respiratory syncytial virus", "Rhinovirus A",
             "Rhinovirus B", "Other")) +
  xlab("Peptides") +
  ylab("Antibody binding (z-scores)") +
  labs(title = 'Pre-onset in Cases') + 
  expand_limits(y = 150) +
  guides(color = guide_legend(override.aes = list(size = 1.8))) +
  theme_bw() +
  theme(
    plot.margin = margin(1.0, 0.5, 0, 0.5, "cm"),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.title = element_text(size = 9),
    plot.title = element_text(size = 9),
    legend.title = element_blank(),
    legend.position = "bottom"
  )
A

ggsave(A, file="Fig4A.pdf", width=8, height=6)

  ############
  # CONTROLS #
  ############

C <- pre_mean_z %>% 
  filter(casestat == 0) %>% 
  mutate(
    pep_id = as.character(pep_id),
    ebv = if_else(Species =="Human herpesvirus 4", 1, 0),
    label = if_else(Species %in% labels$Species, Species, "Other")
  ) %>% 
  mutate(label = factor(label, levels=c("Enterovirus B", "Enterovirus C", "Human adenovirus C", "Human herpesvirus 1", 
                                        "Human herpesvirus 4", "Human respiratory syncytial virus", "Rhinovirus A",
                                        "Rhinovirus B", "Other"))) %>% 
  arrange(ebv, Species, Protein_name) %>% 
  mutate(sort = row_number()) %>% 
  ggplot(aes(x = sort, y=pep_mean_z_score, color = factor(label))) + 
  geom_point(size = 0.6) +
  expand_limits(y = 150) +
  scale_color_manual(values = mycols, labels=c("Enterovirus B", "Enterovirus C", "Human adenovirus C", "Human herpesvirus 1", 
             "Epstein-Barr virus", "Human respiratory syncytial virus", "Rhinovirus A",
             "Rhinovirus B", "Other")) +
  xlab("Peptides") +
  ylab("Antibody binding (z-scores)") +
  labs(title = 'Pre-onset in Controls') + 
  guides(color = guide_legend(override.aes = list(size = 1.8))) +
  theme_bw() +
  theme(
    plot.margin = margin(1.0, 0.5, 0, 0.5, "cm"),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.title = element_text(size = 9),
    plot.title = element_text(size = 9),
    legend.title = element_blank(),
    legend.position = "bottom"
  )
C

ggsave(C, file="Fig4C.pdf", width=8, height=6)

########################### POST-ONSET SAMPLES ###########################

post_mean_z <- VirScan_dataset %>% 
  filter(timeser=="after") %>% 
  mutate(pep_id = as.character(pep_id)) %>% 
  group_by(casestat, id1, Species, Protein_name, pep_id) %>% 
  mutate(rep_mean_z_score = mean(zscore)) %>% 
  ungroup() %>% 
  filter(repnum==1) %>% #information double, so filter away one row
  group_by(casestat, pep_id, Protein_name, Species) %>% 
  summarise(pep_mean_z_score = mean(rep_mean_z_score)) %>% 
  ungroup() 

labels <- post_mean_z %>%  # create labels for coloring of peptides with high z-scores
  filter(pep_mean_z_score>=30) %>% 
  group_by(Species) %>% 
  filter(n()>1) %>% 
  distinct(Species)

  #########
  # CASES #
  #########

B <- post_mean_z %>% 
  filter(casestat == 1) %>% 
  mutate(
    pep_id = as.character(pep_id),
    ebv = if_else(Species =="Human herpesvirus 4", 1, 0),
    label = if_else(Species %in% labels$Species, Species, "Other")
  ) %>% 
  mutate(label = factor(label, levels=c("Enterovirus B", "Enterovirus C", "Human adenovirus C", "Human herpesvirus 1", 
                                        "Human herpesvirus 4", "Human respiratory syncytial virus", "Rhinovirus A",
                                        "Rhinovirus B", "Other"))) %>% 
  arrange(ebv, Species, Protein_name) %>% 
  mutate(sort = row_number()) %>% 
  ggplot(aes(x = sort, y=pep_mean_z_score, color = factor(label))) + 
  geom_point(size = 0.6) +
  expand_limits(y = 150) +
  scale_color_manual(values = mycols, labels=c("Enterovirus B", "Enterovirus C", "Human adenovirus C", "Human herpesvirus 1", 
                                                "Epstein-Barr virus", "Human respiratory syncytial virus", "Rhinovirus A",
                                                "Rhinovirus B", "Other")) +
  xlab("Peptides") +
  ylab("Antibody binding (z-scores)") +
  labs(title = 'Post-onset in Cases') + 
  guides(color = guide_legend(override.aes = list(size = 1.8))) +
  theme_bw() +
  theme(
    plot.margin = margin(1.0, 0.5, 0, 0.5, "cm"),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.title = element_text(size = 9),
    plot.title = element_text(size = 9),
    legend.title = element_blank(),
    legend.position = "bottom"
  )
B

ggsave(B, file="Fig4B.pdf", width=8, height=6)

  ############
  # CONTROLS #
  ############

D <- post_mean_z %>% 
  filter(casestat == 0) %>% 
  mutate(
    pep_id = as.character(pep_id),
    ebv = if_else(Species =="Human herpesvirus 4", 1, 0),
    label = if_else(Species %in% labels$Species, Species, "Other")
  ) %>% 
  mutate(label = factor(label, levels=c("Enterovirus B", "Enterovirus C", "Human adenovirus C", "Human herpesvirus 1", 
                                        "Human herpesvirus 4", "Human respiratory syncytial virus", "Rhinovirus A",
                                        "Rhinovirus B", "Other"))) %>% 
  arrange(ebv, Species, Protein_name) %>% 
  mutate(sort = row_number()) %>% 
  ggplot(aes(x = sort, y=pep_mean_z_score, color = factor(label))) + 
  geom_point(size = 0.6) +
  scale_color_manual(values = mycols, labels=c("Enterovirus B", "Enterovirus C", "Human adenovirus C", "Human herpesvirus 1", 
                                                "Epstein-Barr virus", "Human respiratory syncytial virus", "Rhinovirus A",
                                                "Rhinovirus B", "Other")) +
  xlab("Peptides") +
  ylab("Antibody binding (z-scores)") +
  labs(title = 'Post-onset in Controls') + 
  expand_limits(y = 150) +
  guides(color = guide_legend(override.aes = list(size = 1.8))) +
  theme_bw() +
  theme(
    plot.margin = margin(1.0, 0.5, 0, 0.5, "cm"),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.title = element_text(size = 9),
    plot.title = element_text(size = 9),
    legend.title = element_blank(),
    legend.position = "bottom"
  )
D

ggsave(D, file="Fig4D.pdf", width=8, height=6)

#################################################################################################################
# 3. Figure 4 E-F: Analysis of case-control difference in positive hits by peptide and mapping to viral species
#################################################################################################################

########################### PRE-ONSET SAMPLES ###########################

# Defining epitope hits using both replicates and proportions of cases and controls with a hit

epihit_bothreps_preonset <- VirScan_dataset %>% 
  filter(timeser=="last") %>%  
  select(casestat, id1, pep_id, repnum, timeser, zscore) %>% 
  arrange(pep_id, id1) %>% 
  mutate(
    caco = if_else(casestat == 1, "case", "control"),
    epi_hit = if_else(zscore>3.5, 1, 0)
  ) %>% 
  group_by(id1, pep_id) %>% 
  mutate(hit_both = if_else(sum(epi_hit)==2, 1, 0)) 

hit_in_both_pre <- epihit_bothreps_preonset %>% 
  ungroup() %>% 
  filter(repnum ==1) %>% # information is in both rep 1 and 2; therefore keeping one of the rows
  group_by(pep_id) %>% 
  filter(sum(hit_both)>=1) 

hit_in_both_final_pre <- hit_in_both_pre %>% 
  group_by(pep_id, casestat) %>% 
  summarise(prop_with_hit = mean(hit_both>=1)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = casestat, values_from = prop_with_hit) %>% 
  rename(Prop_controls = "0", Prop_cases = "1") %>% 
  mutate(
    Cases_with_hit = Prop_cases*30,
    Cases_without_hit = 30 - Cases_with_hit,
    Controls_with_hit = Prop_controls*30,
    Controls_without_hit = 30 - Controls_with_hit
  ) %>% 
  select(-Prop_cases, -Prop_controls)

# Fisher's Exact Test 

hit_in_both_final_fisher_pre <- hit_in_both_final_pre %>% 
  group_by(pep_id) %>% 
  nest() %>% 
  mutate(matrix = map(data, ~matrix(unlist(.x), nrow = 2)))%>% 
  mutate(fisher = map(matrix, ~fisher.test(.x))) %>% 
  mutate(stats = map(fisher, ~broom::tidy(.x))) %>% 
  unnest(c(data, stats)) %>% 
  select(pep_id, Cases_with_hit, Controls_with_hit, p.value) %>% 
  mutate(
    Prop_cases = Cases_with_hit/30,
    Prop_controls = Controls_with_hit/30,
    pep_id = as.character(pep_id)
  ) %>% 
  mutate(High_in_cases = if_else(Prop_cases>Prop_controls, 1, 0)) %>% 
  select(pep_id, High_in_cases, Prop_cases, Prop_controls, p.value) %>% 
  ungroup() %>% 
  arrange(p.value) %>% 
  left_join(Metadata, by ="pep_id") 

# Filter significant results

fisher1_pre <- hit_in_both_final_fisher_pre %>% 
  filter(p.value<=0.05) %>% 
  mutate(serotype = str_replace(serotype, "Human immunodeficiency virus", "HIV")) %>% 
  mutate(serotype = if_else(serotype=="BK polyomavirus (BKPyV)", "BK polyomavirus", serotype))

# Make Supplemental Table S1

pre_suppl_table <- fisher1_pre %>% 
  select(pep_id, serotype, Organism, Protein_name, UniProt_acc, pro_len, start, end, Prop_cases, Prop_controls, p.value) %>%
  mutate(
    Prop_cases = round(Prop_cases, 2),
    Prop_controls = round(Prop_controls, 2),
    p.value = signif(p.value, 2)
  ) %>% 
  mutate(
    Organism = str_replace(Organism, ".*strain ", ""),
    Organism = str_replace(Organism, "\\).*$", ""),
    Protein_name = str_replace(Protein_name, "\\).*$",")"),
    Protein_name = str_replace(Protein_name, "Cleaved.*$", ""),
    Protein_name = str_replace_all(Protein_name, "_", " ")
  ) %>% 
  select(-pep_id) %>% 
  rename(
    "Viral species" = serotype,
    Strain = Organism, 
    Protein = Protein_name,
    "UniProt ID" = UniProt_acc,
    "Protein length" = pro_len,
    "Peptide start" = start,
    "Peptide end" = end,
    "Proportion in Cases" = Prop_cases,
    "Proportion in Controls" = Prop_controls,
    "p-value" = p.value
  ) %>% 
  arrange("p-value")

write_xlsx(pre_suppl_table, "Table S1.xlsx")  

# Plot number of peptides with significantly different binding between cases and controls by viral species

E <- fisher1_pre %>%  
  ggplot(aes(fct_infreq(serotype), fill = ifelse(High_in_cases==1, "Higher in cases", "Higher in controls"))) +
  geom_bar() +
  scale_fill_manual(values = c("#D55E00", "#0072B2")) +
  labs(x='Viral species', y='Number of peptides', title = 'Pre-onset samples') + 
  coord_flip() +
  scale_y_continuous(breaks =seq(0, 30, 2)) +
  expand_limits(y=30) +
  guides(color = guide_legend(override.aes = list(size = 0.5))) +
  theme_bw() +
  theme(
    plot.margin = margin(1.0, 0.5, 0, 0.5, "cm"),
    legend.title = element_blank(),
    axis.title = element_text(size = 9),
    plot.title = element_text(size = 9),
    legend.position = "bottom",
    axis.text = element_text(size = 7)
  )  
E

ggsave(E, file="Fig4E.pdf", width = 8, height = 2.5)

########################### POST-ONSET SAMPLES ###########################

# Defining epitope hits using both replicates and proportions of cases and controls with a hit

epihit_bothreps_postonset <- VirScan_dataset %>% 
  filter(timeser=="after") %>%  
  select(casestat, id1, pep_id, repnum, timeser, zscore) %>% 
  arrange(pep_id, id1) %>% 
  mutate(
    caco = if_else(casestat == 1, "case", "control"),
    epi_hit = if_else(zscore>3.5, 1, 0)
  ) %>% 
  group_by(id1, pep_id) %>% 
  mutate(hit_both = if_else(sum(epi_hit)==2, 1, 0)) 

hit_in_both_post <- epihit_bothreps_postonset %>% 
  ungroup() %>% 
  filter(repnum ==1) %>% # information is in both rep 1 and 2; therefore keeping one of the rows
  group_by(pep_id) %>% 
  filter(sum(hit_both)>=1) 

hit_in_both_final_post <- hit_in_both_post %>% 
  group_by(pep_id, casestat) %>% 
  summarise(prop_with_hit = mean(hit_both>=1)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = casestat, values_from = prop_with_hit) %>% 
  rename(Prop_controls = "0", Prop_cases = "1") %>% 
  mutate(
    Cases_with_hit = Prop_cases*30,
    Cases_without_hit = 30 - Cases_with_hit,
    Controls_with_hit = Prop_controls*30,
    Controls_without_hit = 30 - Controls_with_hit
  ) %>% 
  select(-Prop_cases, -Prop_controls)

# Fisher's Exact Test

hit_in_both_final_fisher_post <- hit_in_both_final_post %>% 
  group_by(pep_id) %>% 
  nest() %>% 
  mutate(matrix = map(data, ~matrix(unlist(.x), nrow = 2)))%>% 
  mutate(fisher = map(matrix, ~fisher.test(.x))) %>% 
  mutate(stats = map(fisher, ~broom::tidy(.x))) %>% 
  unnest(c(data, stats)) %>% 
  select(pep_id, Cases_with_hit, Controls_with_hit, p.value) %>% 
  mutate(Prop_cases = Cases_with_hit/30,
         Prop_controls = Controls_with_hit/30,
         pep_id = as.character(pep_id)) %>% 
  mutate(High_in_cases = if_else(Prop_cases>Prop_controls, 1, 0)) %>% 
  select(pep_id, High_in_cases, Prop_cases, Prop_controls, p.value) %>% 
  ungroup() %>% 
  arrange(p.value) %>% 
  left_join(Metadata, by ="pep_id") 

# Filter significant results

fisher1_post <- hit_in_both_final_fisher_post %>% 
  filter(p.value<=0.05) %>% 
  mutate(serotype = str_replace(serotype, "Human immunodeficiency virus", "HIV"))

# Make Supplemental Table S2

post_suppl_table <- fisher1_post %>% 
  select(pep_id, serotype, Organism, Protein_name, UniProt_acc, pro_len, start, end, Prop_cases, Prop_controls, p.value) %>%
  mutate(
    Prop_cases = round(Prop_cases, 2),
    Prop_controls = round(Prop_controls, 2),
    p.value = signif(p.value, 2)
  ) %>% 
  mutate(
    Organism = str_replace(Organism, ".*strain ", ""),
    Organism = str_replace(Organism, "\\).*$", ""),
    Protein_name = str_replace(Protein_name, "\\).*$",")"),
    Protein_name = str_replace(Protein_name, "Cleaved.*$", ""),
    Protein_name = str_replace_all(Protein_name, "_", " ")
  ) %>% 
  select(-pep_id) %>% 
  rename(
    "Viral species" = serotype,
    Strain = Organism, 
    Protein = Protein_name,
    "UniProt ID" = UniProt_acc,
    "Protein length" = pro_len,
    "Peptide start" = start,
    "Peptide end" = end,
    "Proportion in Cases" = Prop_cases,
    "Proportion in Controls" = Prop_controls,
    "p-value" = p.value
  ) %>% 
  arrange("p-value")

write_xlsx(post_suppl_table, "Table S2.xlsx") 

# Plot number of peptides with significantly different binding between cases and controls by viral species

F <- fisher1_post %>%  
  ggplot(aes(fct_infreq(serotype), fill = ifelse(High_in_cases==1, "Higher in cases", "Higher in controls"))) +
  geom_bar() +
  scale_fill_manual(values = c("#D55E00", "#0072B2")) +
  labs(x='Viral species', y='Number of peptides',
       title = 'Post-onset samples') + 
  coord_flip() +
  scale_y_continuous(breaks =seq(0, 30, 2)) +
  expand_limits(y=30) +
  guides(color = guide_legend(override.aes = list(size = 0.5))) +
  theme_bw() +
  theme(
    plot.margin = margin(1.0, 0.5, 0, 0.5, "cm"),
    legend.title = element_blank(),
    legend.position = "bottom", 
    axis.title = element_text(size = 9),
    plot.title = element_text(size = 9),
    axis.text = element_text(size = 7)
  )  
F

ggsave(F, file="Fig4F.pdf", width = 8, height = 2.5)

####################################################################################################################
# 4. Figure 4 G-H: Analysis of case-control difference in positive hits by EBV antigens and mapping to viral species
#####################################################################################################################

########################### PRE-ONSET SAMPLES ###########################

sign_ebv_pre <- fisher1_pre %>% 
  select(pep_id, serotype, Organism, Protein_name, UniProt_acc, pro_len, start, end, Prop_cases, Prop_controls, p.value) %>%
  mutate(
    Prop_cases = round(Prop_cases, 2),
    Prop_controls = round(Prop_controls, 2),
    p.value = signif(p.value, 2)
  ) %>% 
  mutate(
    Organism = str_replace(Organism, ".*strain ", ""),
    Organism = str_replace(Organism, "\\).*$", ""),
    Protein_name = str_replace(Protein_name, "\\).*$",")"),
    Protein_name = str_replace(Protein_name, "Cleaved.*$", ""),
    Protein_name = str_replace_all(Protein_name, "_", " ")
  ) %>% 
  select(-pep_id) %>% 
  filter(serotype == "Epstein-Barr virus") %>% 
  rename(
    "Viral species" = serotype,
    Strain = Organism, 
    Protein = Protein_name,
    "UniProt ID" = UniProt_acc,
    "Protein length" = pro_len,
    "Peptide start" = start,
    "Peptide end" = end,
    "Proportion in Cases" = Prop_cases,
    "Proportion in Controls" = Prop_controls,
    "p-value" = p.value
  ) %>% 
  mutate(
    Protein = if_else(Protein=="Epstein-Barr nuclear antigen 1 (EBNA-1)", "EBNA-1", Protein),
    Protein = if_else(Protein=="Epstein-Barr nuclear antigen 2 (EBNA-2)", "EBNA-2", Protein),
    Protein = if_else(Protein=="Epstein-Barr virus nuclear antigen (EBNA2)", "EBNA-2", Protein),
    Protein = if_else(Protein=="Epstein-Barr nuclear antigen 3 (EBNA-3)", "EBNA-3", Protein),
    Protein = if_else(Protein=="EBNA3B", "EBNA-3B", Protein),
    Protein = if_else(Protein=="Epstein-Barr nuclear antigen 4 (EBNA-4)", "EBNA-4", Protein),
    Protein = if_else(Protein=="Epstein-Barr nuclear antigen 6 (EBNA-6)", "EBNA-6", Protein),
    Protein = if_else(Protein=="EBNA1 (Fragment)", "EBNA-1", Protein),
    Protein = if_else(Protein=="Nuclear antigen EBNA-1 (Fragment)", "EBNA-1", Protein),
    Protein = if_else(Protein=="Nuclear antigen 1 (Fragment)", "EBNA-1", Protein),
    Protein = if_else(Protein=="Envelope glycoprotein M (gM)", "Envelope glycoprotein M", Protein)
  ) %>% 
  arrange("p-value")

# Plot number of significant EBV peptides with significantly different binding between cases and controls

G <- sign_ebv_pre %>%  
  ggplot(aes(fct_infreq(Protein))) +
  geom_bar(fill = "#009E73") +
  labs(x='EBV antigens', y='Number of peptides',
       title = 'Pre-onset samples') + 
  coord_flip() +
  scale_y_continuous(breaks =seq(0, 13, 1)) +
  expand_limits(y=13) +
  theme_bw() +
  theme(
    plot.margin = margin(1.0, 0.5, 0, 0.5, "cm"),
    axis.title = element_text(size = 9),
    plot.title = element_text(size = 9),
    axis.text = element_text(size = 7),
    legend.position = "none"
  )  
G

ggsave(G, file="Fig4G.pdf", width = 8, height = 1.8)

########################### POST-ONSET SAMPLES ###########################

sign_ebv_post <- fisher1_post %>% 
  select(pep_id, serotype, Organism, Protein_name, UniProt_acc, pro_len, start, end, Prop_cases, Prop_controls, p.value) %>%
  mutate(
    Prop_cases = round(Prop_cases, 2),
         Prop_controls = round(Prop_controls, 2),
         p.value = signif(p.value, 2)
    ) %>% 
  mutate(
    Organism = str_replace(Organism, ".*strain ", ""),
         Organism = str_replace(Organism, "\\).*$", ""),
         Protein_name = str_replace(Protein_name, "\\).*$",")"),
         Protein_name = str_replace(Protein_name, "Cleaved.*$", ""),
         Protein_name = str_replace_all(Protein_name, "_", " ")
    ) %>% 
  select(-pep_id) %>% 
  filter(serotype == "Epstein-Barr virus") %>% 
  rename(
    "Viral species" = serotype,
         Strain = Organism, 
         Protein = Protein_name,
         "UniProt ID" = UniProt_acc,
         "Protein length" = pro_len,
         "Peptide start" = start,
         "Peptide end" = end,
         "Proportion in Cases" = Prop_cases,
         "Proportion in Controls" = Prop_controls,
         "p-value" = p.value
  ) %>% 
  mutate(Protein = if_else(Protein=="Epstein-Barr nuclear antigen 1 (EBNA-1)", "EBNA-1", Protein),
         Protein = if_else(Protein=="Epstein-Barr nuclear antigen 2 (EBNA-2)", "EBNA-2", Protein), 
         Protein = if_else(Protein=="Epstein-Barr virus nuclear antigen (EBNA2)", "EBNA-2", Protein),
         Protein = if_else(Protein=="Epstein-Barr nuclear antigen 3 (EBNA-3)", "EBNA-3", Protein),
         Protein = if_else(Protein=="EBNA3B", "EBNA-3", Protein),
         Protein = if_else(Protein=="Epstein-Barr nuclear antigen 4 (EBNA-4)", "EBNA-4", Protein),
         Protein = if_else(Protein=="Epstein-Barr nuclear antigen 6 (EBNA-6)", "EBNA-6", Protein),
         Protein = if_else(Protein=="EBNA1 (Fragment)", "EBNA-1", Protein),
         Protein = if_else(Protein=="Nuclear antigen 1 (Fragment)", "EBNA-1", Protein),
         Protein = if_else(Protein=="Nuclear antigen EBNA-1 (Fragment)", "EBNA-1", Protein),
         Protein = if_else(Protein=="EBNA-1 protein (Fragment)", "EBNA-1", Protein),
         Protein = if_else(Protein=="Latent membrane protein 1 (Fragment)", "Latent membrane protein 1", Protein)
  ) %>% 
  arrange("p-value")

# Plot number of significant EBV peptides with significantly different binding between cases and controls

H <- sign_ebv_post %>%  
  ggplot(aes(fct_infreq(Protein))) +
  geom_bar(fill = "#009E73") +
  labs(x='EBV antigens', y='Number of peptides',
       title = 'Post-onset samples') + 
  coord_flip() +
  scale_y_continuous(breaks =seq(0, 13, 1)) +
  expand_limits(y=13) +
  theme_bw() +
  theme(plot.margin = margin(1.0, 0.5, 0, 0.5, "cm"),
        axis.title = element_text(size = 9),
        plot.title = element_text(size = 9),
        axis.text = element_text(size = 7),
        legend.position = "none")  
H

ggsave(H, file="Fig4H.pdf", width = 8, height = 1.8)

#######################
# 5. Arrange Figure 4
#######################

library(ggpubr)

final_panel <- ggarrange(ncol=1, nrow=3, heights = c(1, 0.7, 0.38),
                         ggarrange(A, B, C, D, align = "v", common.legend = TRUE, legend = "bottom",
                                   ncol = 2, nrow = 2, labels = c("A", "B", "C", "D")),
                         ggarrange(E, F, align = "v", common.legend = TRUE, legend = "bottom",
                                   ncol = 2, nrow = 1, labels = c("E", "F")),
                         ggarrange(G, H, align = "v", common.legend = TRUE, legend = "bottom",
                                   ncol = 2, nrow = 1, labels = c("G", "H")))

ggsave(final_panel, file = "Figure 4.pdf", height = 11, width = 9, device = cairo_pdf)
