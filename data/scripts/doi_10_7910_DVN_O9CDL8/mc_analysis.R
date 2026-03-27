# Analyze shifts in speech intensity across country groups (developed vs. developing)
# around Cancun (2003) and Hong Kong (2005) Ministerial Conferences

# Wordscores(ws_doha) are computed using reference positions from Doha QUAD members (US/EU) and BRA_IND (Brazil/India)
# Analysis at the individual member level; the EU is considered only for wordscore calculation

library(openxlsx)
library(dplyr)

# Clear all objects from the workspace
rm(list = ls())  

# Set working directory to results folder
dir <-"C://Users//MC//result_st"
setwd(dir)

# Load the dataset
my_df<-read.xlsx("data_mc.xlsx")

# Create log-transformed variables for count and trade for regression analysis
my_df <-my_df %>%
  mutate(ln_count=log(count), ln_trade=log(trade))

# Identify members ranked in the top and bottom 10 based on both Wordscore or GDP per capita in 2001(Doha MC),
# to classify members with extreme negotiation positions

# Bottom 10 by minimum Doha wordscore
cou_bottom<- my_df %>% 
  group_by(cou_iso) %>% 
  summarize(rank_ws_doha = min(ws_doha))%>% 
  arrange(rank_ws_doha)%>%
  filter(rank_ws_doha != "")%>%
  head(10)

# Top 10 by maximum Doha wordscore
cou_top<- my_df %>% 
  group_by(cou_iso) %>% 
  summarize(rank_ws_doha = max(ws_doha))%>% 
  arrange(desc(rank_ws_doha))%>%
  filter(rank_ws_doha != "")%>%
  head(10)

# Top 10 GDP per capita members in 2001
cou_top_gdppc<- my_df %>%
  filter(year==2001 & gdppc != "") %>% 
  arrange(desc(gdppc)) %>%
  slice_head(n = 10)

# Bottom 10 GDP per capita members in 2001
cou_bottom_gdppc<- my_df %>% 
  filter(year==2001 & gdppc != "") %>% 
  arrange(gdppc) %>%
  slice_head(n = 10)

# Store ISO codes for top/bottom members
tmp_bottom <- cou_bottom$cou_iso
tmp_top <- cou_top$cou_iso
tmp_bottom_gdppc <- cou_bottom_gdppc$cou_iso
tmp_top_gdppc <- cou_top_gdppc$cou_iso

# Label members in the dataset: wordscore/GDPPC alignment (0 = bottom, 1 = top)
my_df[my_df$cou_iso %in% tmp_bottom, "wordscore_doha"] <- 0
my_df[my_df$cou_iso %in% tmp_top, "wordscore_doha"] <- 1
my_df[my_df$cou_iso %in% tmp_bottom_gdppc, "gdppc_doha"] <- 0
my_df[my_df$cou_iso %in% tmp_top_gdppc, "gdppc_doha"] <- 1

# Subset: only countries in top/bottom of position distribution
my_df_doha <- subset(my_df, wordscore_doha==1 | wordscore_doha==0)
my_df_doha_gdppc <- subset(my_df, gdppc_doha==1 | gdppc_doha==0)

######## Table 3 Regression Models ########

# (1) Interaction between Cancun participation and Doha wordscore
model_Cancun_wordscore <- lm(ln_count~Cancun*wordscore_doha, data=my_df_doha)
summary(model_Cancun_wordscore)

# (2) Interaction between Hong Kong participation and Doha wordscore
model_HK_wordscore <- lm(ln_count~HK*wordscore_doha, data=my_df_doha)
summary(model_HK_wordscore)

# (3) Interaction between Cancun and log of GDP per capita
model_Cancun_gdppc <- lm(ln_count~Cancun*gdppc_doha, data=my_df_doha_gdppc)
summary(model_Cancun_gdppc)

# (4) Interaction between Hong Kong and log of GDP per capita
model_HK_gdppc <- lm(ln_count~HK*gdppc_doha, data=my_df_doha_gdppc)
summary(model_HK_gdppc)

# (5) Interaction between Cancun and Doha wordscore, controlling for trade
model_Cancun_wordscore2 <- lm(ln_count~Cancun*wordscore_doha+ln_trade, data=my_df_doha)
summary(model_Cancun_wordscore2)

# (6) Interaction between Hong Kong and Doha wordscore, controlling for trade
model_HK_wordscore2 <- lm(ln_count~HK*wordscore_doha+ln_trade, data=my_df_doha)
summary(model_HK_wordscore2)

capture.output(summary(model_Cancun_wordscore), 
               file = "table3.txt", 
               append = FALSE)

capture.output(summary(model_HK_wordscore), 
               file = "table3.txt", 
               append = TRUE)

capture.output(summary(model_Cancun_gdppc), 
               file = "table3.txt", 
               append = TRUE)

capture.output(summary(model_HK_gdppc), 
               file = "table3.txt", 
               append = TRUE)

capture.output(summary(model_Cancun_wordscore2), 
               file = "table3.txt", 
               append = TRUE)

capture.output(summary(model_HK_wordscore2), 
               file = "table3.txt", 
               append = TRUE)


