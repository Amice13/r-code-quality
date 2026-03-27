#####################################################
# Project:    Emergency Debate contribution
# Task:       Scale speech texts along emergency dict
# Author:     Christian Rauh (09.04.2021)
#####################################################

# R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"

# Packages 
library(tidyverse) # 1.3.0
library(quanteda) # 2.1.2
library(ggrepel) # 0.8.2


# Joint corpus ####
# See preceeding script 

speech <- read_rds("./Corpora/EmergencyCorp.rds")
speech$id <- 1:nrow(speech)
table(speech$institution)



# Normality-Emergency Scale ####

# Derived from Word Vector model of HoC speeches 1985-2019
# Starts from vectors of seed terms
# emergency <- c("crisis", "emergency", "danger", "peril", "hazard", "threat", "risk", "disaster", "uncertainty", "uncertain")
# normality <- c("normality", "normal", "safety", "stability", "regularity", "routine", "calm", "usual", "certainty", "certain")
# Then measures cosine similarity of each word to to average vector of each seed 
# Polarity: Differences in similarity on term level
# See main article text for details

nc <- read.csv2("./HoCWordVectors/EmergencyScale250.csv")
nc <- nc %>% filter(!str_detect(term, "Â")) # Encoding error in HOC text data


# Illustrate Emergency scale term weights ####

# Figure 1 in main text
# Note that vertical order is randomized

set.seed(2020905)
nc$y <- sample(1:20000, 499, replace = T)# Random y axis
nc$hl <- sample(1:4, 499, replace = T) # Highlight random words (1/3)
nc$hl <- nc$hl == 1 # Highlight markers as logical
nc$fac <- nc$dif > 0 # Facet of pos/neg terms

fac.labels <- c("Normality\n(Seed terms: normality, normal, safety, stability,\nregularity, routine, calm, usual, certainty, certain)", 
                "Emergency\n(Seed terms: emergency, crisis, danger, peril,\nhazard, threat, risk, disaster, uncertainty, uncertain)")
names(fac.labels) <- c("FALSE", "TRUE")

# Reduce term list for larger plot

top <- nc %>% filter(abs(diff)>.3) # Want to keep the most extreme ones
bottom <- nc %>% filter(abs(diff)<=.3) %>% sample_n(94) # Sample examples with lower levels

nc2 <- rbind(top,bottom)

# Plot

ggplot(data = nc2, aes(x=diff, y=y))+
  geom_text(aes(label = term), color = "grey40", alpha = .6, size = 3.5) +
  geom_text(aes(label = term, alpha = hl), size = 3.5) +
  scale_alpha_manual(values = c(0, 1))+
  labs(x = "\nDifference in semantic similarity\nto normality and emergency vectors",
       y = "Exemplary terms\ndrawn from the HoC Word Vector model\n")+
  facet_wrap(.~fac, scales = "free_x", labeller = labeller(fac = fac.labels))+
  scale_x_continuous(expand = expand_scale(mult = c(.2, .2)))+
  theme_minimal()+
  theme(legend.position = "none",
        text = element_text(family = "serif"),
        axis.text.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 12, face = "bold"))

ggsave("./Plots/EmergencyScaling.png", width = 26, height = 12, units = "cm")



# Scale speech texts along these terms ####

# Named vector of nc weigths
cnw <- nc$diff
names(cnw) <- nc$term

# DFM
corp <- corpus(speech$text, docvars = data.frame(speech$id))
m <- dfm(corp, tolower = T, select = names(cnw)) # Only the words on the nc scale, otherwise too large 
rm(corp)

# Weigth DFM with NC scale
m2 <- dfm_weight(m, weights = cnw)
head(m2)
sparsity(m2)

m3 <- convert(m2, to = "matrix")
m3[m3==0] <- NA # Need to set zeros to NAs, otherwise I draw the rowmean to zero

# Write average NC scale to corpus 
speech$ne.scale <- rowMeans(m3, na.rm = T)
hist(speech$ne.scale)

# Clean up
# rm(list = c('m','m2', 'm3', 'nc', 'cnw', 'fac.labels'))


# Export ####
write_rds(speech, "./Corpora/EmergencyCorpData.rds")