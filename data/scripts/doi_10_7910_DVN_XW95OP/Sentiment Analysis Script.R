## Mitchell Ransden
## Calibrating the Mudslinger Replication Code
## Sentiment Analysis Script
## Last edited: 4-27-2023

## Relevant packages--------------
library(tidyverse)
library(haven)
library(readxl)
library(vader) # VADER sentiment analysis
library(quanteda) # Benoit & Soroka's text analysis package (for LIWC-alike)
library(devtools) # needed to install quanteda liwcalike
library(LIWCalike) # uses R build for LIWC-alike functionality (free, for testing)
library(corpus)
library(sandwich) # need for bootstrap clustered errors
library(lmtest)
library(lme4) # multi-level modeling
library(lmeresampler) # bootstrapping multilevel models
library(texreg) # making tables for Overleaf
library(GGally) # to plot bootstrap coefs!
library(gridExtra) # for displaying multiple plots side by side
library(ggbreak) # to insert breakages in plots
library(patchwork) # a better two-variable plot
library(qdap)
library(qdapRegex) # useful for cleaning out Twitter text
library(quanteda.textstats)
library(quanteda.dictionaries)

## Sentiment analysis----
statements <- read_xlsx("Data/All Statements For Sentiment Analysis.xlsx") # load statements

statements$text_processed <- statements$`Statement Text`%>% # process and remove Twitter info
  rm_hash(., replace="\\3") %>%
  rm_url(.) %>%
  rm_tag(., pattern="@rm_tag2") %>%
  rm_non_ascii(.) %>%
  rm_default(., pattern = "RT") 

statements <- statements %>%
  na.omit() %>% # remove rows with empty statement text cells

full_corpus <- corpus(statements$text_processed, docnames = c(statements$ID)) # construct a "corpus" object (separated, text-only column of dataset)

## VADER
vader_data <- statements
vader_model <- vader_df(vader_data$text_processed) # run VADER analysis on text column (word level)
colnames(vader_model)[1] = "text_processed" # change column name to match
vader_results <- dplyr::inner_join(x = vader_data, y = vader_model, by = "text_clean") # merge vader analysis with dataset
distinct_all() 


## LIWC
liwc_data <- statements
liwcdict2015 <- dictionary(file = "Data/LIWC2015_English.dic", 
                           format = "LIWC") # load 2015 LIWC dictionary (provided by Charles Crabtree)
liwc_model <- quanteda.dictionaries::liwcalike(full_corpus, dictionary = liwcdict2015) %>% # run liwc-alike analysis
  dplyr::rename(ID = docname) %>% # change names for merging with VADER
  select(ID, WC, WPS,`affect (affect).posemo (positive emotions)`, # only select relevant linguistic dims
         `affect (affect).negemo (negative emotions).anx (anx)`,
         `affect (affect).negemo (negative emotions).anger (anger)`,
         `affect (affect).negemo (negative emotions).sad (sad)`,
         `affect (affect).negemo (negative emotions)`,
         `affect (affect)`) 

liwc_results <- merge (x = liwc_data, y = liwc_model, by = "ID", 
                      all = TRUE)

# combine measures into one dataset
sent_analyses <- dplyr::inner_join(liwc_results, vader_results, by = "text_processed") %>%
  distinct_all()

writexl::write_xlsx(prelim_sentiments, "Statistical Analyses/full sentiment analyses.xlsx") # write to Excel


## Results dataset----
results <- read_xlsx("Data/full_statement_scores.xlsx") %>% # load dataset, which has been cleaned in Excel
  dplyr::select(ID, Candidate, Election, Year, Source, `Number of Candidates`, `Number of Rounds`,
                `Ranking Restrictions`,`Number Ranked`,`Incumbent Presence`, `Incumbent Status`,Female, Competition, `Positive Words (LIWC)`, `Negative Words (LIWC)`, 
                `Compound Sentiment (VADER)`) %>%  # grab relevant variables
  mutate(LIWC_sent = `Positive Words (LIWC)`-`Negative Words (LIWC)`) %>%  # Positivity score
  mutate(Twitter = ifelse(Source == "Twitter", 1, 0)) # indicator for whether a statement is a tweet


# Election-level clusters
results_elec <- read_xlsx("Data/full_statement_scores.xlsx") %>% # data with IVs coded for each election/candidate
  dplyr::select(Election, Year, `Number of Candidates`, `Number of Rounds`,
                `Ranking Restrictions`,`Number Ranked`,
                `Incumbent Presence`,Female, Competition, `Positive Words (LIWC)`, `Negative Words (LIWC)`, 
                `Compound Sentiment (VADER)`) %>% # grab relevant variables
  group_by(Election) %>%
  mutate(LIWC_sent = `Positive Words (LIWC)`-`Negative Words (LIWC)`) %>%  # Positivity score
  dplyr::summarise(across(everything(), mean, na.rm=TRUE)) %>%# summarizing values to generate descriptive means
  ungroup() 

# Candidate-level clusters
results_cand <- read_xlsx("Data/full_statement_scores.xlsx") %>%
  dplyr::select(Election, Year, Candidate,Source, `Number of Candidates`, `Number of Rounds`,
                `Ranking Restrictions`,`Number Ranked`,
                `Incumbent Presence`,Female, Competition, `Incumbent Status`,
                `Positive Words (LIWC)`, `Negative Words (LIWC)`, 
                `Compound Sentiment (VADER)`) %>%
  group_by(Candidate) %>%
  mutate(Twitter = ifelse(Source== "Twitter",1,0)) %>% # create indicator to tell if a candidate Tweeted
  ungroup() %>%
  dplyr::select(-Source) %>%
  group_by(Candidate, Election) %>%
  mutate(LIWC_sent = `Positive Words (LIWC)`-`Negative Words (LIWC)`) %>%
  dplyr::summarise(across(everything(), mean, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(Tweeted = ceiling(Twitter)) # dichotomizes continuous variable (if candidate issued any tweets, rounded to 1, otherwise 0)

# Restricted datasets
# Model 2 (3 or more candidates, effective ranking restrictions)
model2results <- results %>%
  filter(`Number of Candidates` >= 3,
         `Ranking Restrictions` == 0 | `Ranking Restrictions` == 1 & `Number of Candidates` > `Number Ranked`,
  ) 

model2results_elec <- results_elec %>% # election-level
  filter(`Number of Candidates` >= 3, 
         `Ranking Restrictions` == 0 | `Ranking Restrictions` == 1 & `Number of Candidates` > `Number Ranked`,
  )

model2results_cand <- results_cand %>% # Candidate-level 
  filter(`Number of Candidates` >= 3,
         `Ranking Restrictions` == 0 | `Ranking Restrictions` == 1 & `Number of Candidates` > `Number Ranked`,
  )

# Model 3 (guaranteed RCV tabulation)
model3results <- model2results %>%
  filter(`Number of Rounds` >= 2) 

model3results_elec <- model2results_elec %>% # election-level
  filter(`Number of Rounds` >= 2)

model3results_cand <- model2results_cand %>% # candidate-level 
  filter(`Number of Rounds` >= 2) 

twitterresults <- results %>% # Twitter subsample
  filter(Twitter == 1)

debateresults <- results %>% # debate subsample
  filter(Twitter == 0)

## LIWC and VADER correlation (Table 2.2)----
# Statement-level
cor.test(results$LIWC_sent, results$`Compound Sentiment (VADER)`) # Model 1
cor.test(model2results$LIWC_sent, model2results$`Compound Sentiment (VADER)`) # Model 2
cor.test(model3results$LIWC_sent, model3results$`Compound Sentiment (VADER)`) # Model 3

# Candidate-level
cor.test(results_cand$LIWC_sent, results_cand$`Compound Sentiment (VADER)`) # Model 1
cor.test(model2results_cand$LIWC_sent, model2results_cand$`Compound Sentiment (VADER)`) # Model 2
cor.test(model3results_cand$LIWC_sent, model3results_cand$`Compound Sentiment (VADER)`) # Model 3

# Election-level
cor.test(results_elec$LIWC_sent, results_elec$`Compound Sentiment (VADER)`) # Model 1
cor.test(model2results_elec$LIWC_sent, model2results_elec$`Compound Sentiment (VADER)`) # Model 2
cor.test(model3results_elec$LIWC_sent, model3results_elec$`Compound Sentiment (VADER)`) # Model 3

## Descriptive information and tables----
# Elections by year (Figure 1.1)
dist_elec_year <- data.frame(table(results_elec$Year)) %>%
  add_row(Var1 = "2012", Freq = 0, .before=4) # add empty row for 2012! No RCV elections that year in dataset
dist_elec_year_p <- ggplot(dist_elec_year, aes(x=Var1, y=Freq)) + theme_bw() +
  geom_bar(stat="identity") + ylim(0,8) + labs(x = "Year", y = "Number of RCV elections") +
  theme(text = element_text(size = 22)) 

# Distribution of number of candidates (Figure 1.2)
dist_number_cand <- data.frame(table(results_elec$`Number of Candidates`))
dist_number_cand_p <- ggplot(dist_number_cand, aes(x=Var1, y = Freq)) +
  geom_bar(stat="identity", fill="steelblue") +  geom_text(aes(label = Freq), vjust = -0.2) +
  theme_bw() + labs(x = "Number of candidates", y="Number of elections") +  theme(text = element_text(size = 24))

# Distribution of ranking restrictions (Figure 1.3)
dist_restrict <- data.frame(table(results_elec$`Number Ranked`))
dist_restrict_p <- ggplot(dist_restrict, aes(x=Var1, y = Freq)) +
  geom_bar(stat="identity", fill="steelblue") +  geom_text(aes(label = Freq), vjust = -0.2) +
  theme_bw() + labs(x = "Maximum number of ranked candidates", y="Number of elections") +  theme(text = element_text(size = 24))

# Competition (binned) (Figure 1.4)
dist_comp <- data.frame(table(results_elec$Competition)) %>%
  mutate(Var1 = as.numeric(as.character(Var1))) # convert to continuous object for binned plotting
dist_comp_p <- ggplot(dist_comp, aes(x=Var1,y=Freq)) +
  geom_bar(stat="identity",fill="steelblue") + 
  scale_x_binned() +
  labs(x = "Mean two-cycle percentage point margin of victory", y="Number of elections") + 
  theme_bw() + theme(text = element_text(size = 24))

# Number of candidates that use Twitter per year (Figure 1.6)
tweeters_per_year <- results_cand %>% # take dataset with tweeted indicator
  dplyr::select(Candidate, Election, Year, `Number of Candidates`, `Tweeted`) %>%# grab only necessary variables
  group_by(Election) %>% 
  mutate(totaltweeters = sum(Tweeted))%>%
  dplyr::summarise(across(everything(), mean,na.rm=TRUE)) %>%
  ungroup() %>%
  group_by(Year) %>%
  dplyr::summarise(across(c(`Number of Candidates`, totaltweeters), sum, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(proptweeters = (totaltweeters/`Number of Candidates`),
         Year = as.character(Year)) # proportion of candidates who used Twitter by year

tweeter_year_p <- ggplot(data=tweeters_per_year, aes(x=Year, y=proptweeters)) +
  geom_bar(stat="identity") + theme_bw() + labs(x="Year", y = "Proportion") + 
  ylim(0,1) + theme(text=element_text(size=20))

# Candidates by year
# Get total candidates in a year
results_year <- results_cand %>%
  group_by(Year) %>%
  dplyr::summarise(sum(unique(`Number of Candidates`))) %>%
  dplyr::rename(`Total Number of Candidates` = `sum(unique(\`Number of Candidates\`))`) %>%
  mutate(Year = as.character(Year))

dist_cand_year_p <- ggplot(results_year, aes(x=Year, y=`Total Number of Candidates`)) +
  geom_bar(stat="identity") + theme_bw() + geom_text(aes(label = `Total Number of Candidates`), vjust=-0.2) +
  theme(text = element_text(size = 20))

# Statements per debate (Figure 1.7)
dist_debate_statements <- data.frame(table(debateresults$Election))
dist_debate_statements_p <- ggplot(dist_debate_statements, aes(x=Freq)) +
  geom_histogram(binwidth = 100,color="white",fill="steelblue") + labs(x="Number of statements",y="Number of debates") + 
  scale_x_continuous(breaks=seq(150,750,by=100)) +
  scale_y_continuous(breaks=seq(2,14,by=2)) +
  theme_bw() + theme(text = element_text(size = 24))

## Multilevel models (VADER)----
# Construct base models (no bootstraps)
# Model 1
multimodel1_v  <- lmer(`Compound Sentiment (VADER)` ~ `Number of Candidates` +
                                  `Ranking Restrictions` + `Incumbent Presence` + Competition +
                                  `Incumbent Status` + Female  +
                                  (1|Election) + (1|Election:Candidate), data = results,
                                REML = FALSE) # explicitly nested. 151 Election:candidate nesting

# Model 1A: Debates
multimodel1A_v  <- lmer(`Compound Sentiment (VADER)` ~ `Number of Candidates` +
                                         `Ranking Restrictions` + `Incumbent Presence` + Competition +
                                         `Incumbent Status` + Female + 
                                         (1|Election) + (1|Election:Candidate), data = debateresults,
                                       REML = FALSE) 

# Model 1B: Twitter
multimodel1B_v  <- lmer(`Compound Sentiment (VADER)` ~ `Number of Candidates` +
                                          `Ranking Restrictions` + `Incumbent Presence` + Competition +
                                          `Incumbent Status` + Female + 
                                          (1|Election) + (1|Election:Candidate), data = twitterresults,
                                        REML = FALSE) 

# Model 2
multimodel2_v <- lmer(`Compound Sentiment (VADER)` ~ `Number of Candidates` +
                                        `Ranking Restrictions` + `Incumbent Presence` + Competition +
                                        `Incumbent Status` + Female + 
                                        (1|Election) + (1|Election:Candidate), data = model2results,
                                      REML = FALSE) # explicitly nested. 151 Election:candidate nesting

# Model 3
multimodel3_v <- lmer(`Compound Sentiment (VADER)` ~ `Number of Candidates` +
                                            `Ranking Restrictions` + `Incumbent Presence` + Competition +
                                            `Incumbent Status` + Female + 
                                            (1|Election) + (1|Election:Candidate), data = model3results,
                                          REML = FALSE) 

# Bootstrapping (this takes a while to run with B=1000)
bootmultimodel1_v <- lmeresampler::bootstrap(multimodel1_v, .f = fixef,
                                             type = "case", B = 1000, resample = c(TRUE, TRUE, FALSE))

bootmultimodel2_v <- lmeresampler::bootstrap(multimodel2_v, .f = fixef,
                                             type = "case", B = 1000, resample = c(TRUE, TRUE, FALSE))

bootmultimodel3_v <- lmeresampler::bootstrap(multimodel3_v, .f = fixef,
                                             type = "case", B = 1000, resample = c(TRUE, TRUE, FALSE)) # Returns NA for several variables(ranking restrictions, incumbent presence, incumbent status)

bootmultimodel1A_v <- lmeresampler::bootstrap(multimodel1A_v, .f = fixef,
                                              type = "case", B = 1000, resample = c(TRUE, TRUE, FALSE))

bootmultimodel1B_v <- lmeresampler::bootstrap(multimodel1B_v, .f = fixef,
                                              type = "case", B = 1000, resample = c(TRUE, TRUE, FALSE))

## Visualizations (VADER)----
# gender and incumbency interaction (Figure 2.8)
interact <- results %>%
  # head(n=-10L) %>% # remove full NA cases
  mutate(Female = case_when(Female == 1 ~ "Female",
                            Female == 0 ~ "Male"))

`Incumbency` <- as.factor(interact$`Incumbent Status`) # name this for legend (color, can't solve with labs())

interact_p <- ggplot(full_interact_results) +
  aes(x = Female, group = `Incumbent Status`, y = `Compound Sentiment (VADER)`, linetype=Incumbency) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line") + theme_bw() + 
  scale_linetype_manual(values=rep(c("solid", "dashed"))) +
  xlab("Candidate gender") + ylab("Average statement sentiment") + 
  theme(text = element_text(size = 20))+
  scale_linetype_discrete(name = " ", labels = c("Challenger", "Incumbent"), guide = guide_legend(reverse=TRUE)) +
  theme(legend.position = c(0.82, 0.3)) 

# Table 2.4 (Main results)
multimodel1Vrename <- confint(bootmultimodel1_v,type="norm") %>%
  dplyr::rename(conf.low = lower,
                conf.high = upper) %>%
  mutate(term = case_when(term == "`Incumbent Status`" ~ "Incumbent Status",
                          term == "Competition" ~ "`Competition`",
                          term == "`Number of Candidates`" ~ "`Number of Candidates",
                          term == "`Ranking Restrictions`" ~ "`Ranking Restrictions`",
                          term == "`Incumbent Presence`" ~ "`Incumbent Presence`",
                          term == "(Intercept)" ~ "(Intercept)",
                          term == "Female" ~ "Female")) # Rename to order plot (cumbersome)

# Model 1A (VADER)
multimodel1AVrename <- confint(bootmultimodel1A_v,type="norm") %>%
  dplyr::rename(conf.low = lower,
                conf.high = upper)

# Model 1B (VADER)
multimodel1BVrename <- confint(bootmultimodel1B_v,type="norm") %>%
  dplyr::rename(conf.low = lower,
                conf.high = upper)

# Model 2 (VADER)
multimodel2Vrename <- confint(bootmultimodel2_v,type="norm") %>%
  dplyr::rename(conf.low = lower,
                conf.high = upper) %>%
  mutate(term = case_when(term == "`Incumbent Status`" ~ "Incumbent Status",
                          term == "Competition" ~ "`Competition`",
                          term == "`Number of Candidates`" ~ "`Number of Candidates",
                          term == "`Ranking Restrictions`" ~ "`Ranking Restrictions`",
                          term == "`Incumbent Presence`" ~ "`Incumbent Presence`",
                          term == "(Intercept)" ~ "(Intercept)",
                          term == "Female" ~ "Female"))

# Model 3 (VADER)
multimodel3Vrename <- confint(bootmultimodel3_v,type="norm",level=0.95) %>%
  dplyr::rename(conf.low = lower,
                conf.high = upper) %>%
  mutate(term = case_when(term == "`Incumbent Status`" ~ "Incumbent Status",
                          term == "Competition" ~ "`Competition`",
                          term == "`Number of Candidates`" ~ "`Number of Candidates",
                          term == "`Ranking Restrictions`" ~ "`Ranking Restrictions`",
                          term == "`Incumbent Presence`" ~ "`Incumbent Presence`",
                          term == "(Intercept)" ~ "(Intercept)",
                          term == "Female" ~ "Female"))
# Output results to table!


# Plot with ggcoef
multimodel1_v_plot <- ggcoef(multimodel1Vrename, exclude_intercept = TRUE,  # Model 1
                                 conf.int = TRUE, conf.level = 0.95) + 
  coord_flip() + labs(x="Estimate", y = "Variable") + 
  theme(plot.title = element_text(hjust=0.5)) + 
  theme_bw() + xlim(-0.2,0.2) + theme(text = element_text(size = 16))

multimodel2_v_plot <- ggcoef(multimodel2Vrename, exclude_intercept = TRUE, # Model 3
                                 conf.int = TRUE, conf.level = 0.95) + 
  coord_flip() + labs(x="Estimate", y = "Variable") + 
  theme(plot.title = element_text(hjust=0.5)) + 
  theme_bw() + xlim(-0.2,0.2) + theme(text = element_text(size = 16))

multimodel3_v_plot <- ggcoef(multimodel3Vrename, exclude_intercept = TRUE, # Model 3
                                 conf.int = TRUE, conf.level = 0.95) + 
  coord_flip() + labs(x="Estimate", y = "Variable") + 
  theme(plot.title = element_text(hjust=0.5)) + 
  theme_bw() + xlim(-0.2,0.2) + theme(text = element_text(size = 16))

multimodel1A_v_plot <- ggcoef(multimodel1AVrename, exclude_intercept = TRUE, # Model 1A
                                  conf.int = TRUE, conf.level = 0.95) + 
  coord_flip() + labs(x="Estimate", y = "Variable") +
  theme(plot.title = element_text(hjust=0.5)) + 
  theme_bw() + xlim(-0.2,0.2) + theme(text = element_text(size = 16))

multimodel1B_v_plot <- ggcoef(multimodel1BVrename, exclude_intercept = TRUE, # Model 1B
                                  conf.int = TRUE, conf.level = 0.95) + 
  coord_flip() + labs(x="Estimate", y = "Variable") +
  theme(plot.title = element_text(hjust=0.5)) +
  theme_bw() + xlim(-0.2,0.2) + theme(text = element_text(size = 16))

## LIWC models and visualizations----
multimodel1_L  <- lmer(LIWC_sent ~ `Number of Candidates` +
                         `Ranking Restrictions` + `Incumbent Presence` + Competition +
                         `Incumbent Status` + Female  +
                         (1|Election) + (1|Election:Candidate), data = results,
                       REML = FALSE) # explicitly nested. 151 Election:candidate nesting

multimodel1A_L  <- lmer(LIWC_sent ~ `Number of Candidates` +
                          `Ranking Restrictions` + `Incumbent Presence` + Competition +
                          `Incumbent Status` + Female + 
                          (1|Election) + (1|Election:Candidate), data = debateresults,
                        REML = FALSE) 

multimodel1B_L  <- lmer(LIWC_sent ~ `Number of Candidates` +
                          `Ranking Restrictions` + `Incumbent Presence` + Competition +
                          `Incumbent Status` + Female + 
                          (1|Election) + (1|Election:Candidate), data = twitterresults,
                        REML = FALSE) 

multimodel2_L <- lmer(LIWC_sent ~ `Number of Candidates` +
                        `Ranking Restrictions` + `Incumbent Presence` + Competition +
                        `Incumbent Status` + Female + 
                        (1|Election) + (1|Election:Candidate), data = model2results,
                      REML = FALSE) # explicitly nested. 151 Election:candidate nesting

multimodel3_L <- lmer(LIWC_sent ~ `Number of Candidates` +
                        `Ranking Restrictions` + `Incumbent Presence` + Competition +
                        `Incumbent Status` + Female + 
                        (1|Election) + (1|Election:Candidate), data = model3results,
                      REML = FALSE) 

# Bootstrapping (this takes a while to run with B=1000)
bootmultimodel1_L <- lmeresampler::bootstrap(multimodel1_L, .f = fixef,
                                             type = "case", B = 1000, resample = c(TRUE, TRUE, FALSE))

bootmultimodel2_L <- lmeresampler::bootstrap(multimodel2_L, .f = fixef,
                                             type = "case", B = 1000, resample = c(TRUE, TRUE, FALSE))

bootmultimodel3_L <- lmeresampler::bootstrap(multimodel3_L, .f = fixef,
                                             type = "case", B = 1000, resample = c(TRUE, TRUE, FALSE)) # Returns NA for several variables(ranking restrictions, incumbent presence, incumbent status)

bootmultimodel1A_L <- lmeresampler::bootstrap(multimodel1A_L, .f = fixef,
                                              type = "case", B = 1000, resample = c(TRUE, TRUE, FALSE))

bootmultimodel1B_L <- lmeresampler::bootstrap(multimodel1B_L, .f = fixef,
                                              type = "case", B = 1000, resample = c(TRUE, TRUE, FALSE))

# Visualizations
multimodel1Lrename <- confint(bootmultimodel1_L,type="norm") %>%
  dplyr::rename(conf.low = lower,
                conf.high = upper) %>%
  mutate(term = case_when(term == "`Incumbent Status`" ~ "Incumbent Status",
                          term == "Competition" ~ "`Competition`",
                          term == "`Number of Candidates`" ~ "`Number of Candidates",
                          term == "`Ranking Restrictions`" ~ "`Ranking Restrictions`",
                          term == "`Incumbent Presence`" ~ "`Incumbent Presence`",
                          term == "(Intercept)" ~ "(Intercept)",
                          term == "Female" ~ "Female")) # Rename to order plot (cumbersome)

# Model 1A (VADER)
multimodel1ALrename <- confint(bootmultimodel1A_L,type="norm") %>%
  dplyr::rename(conf.low = lower,
                conf.high = upper)

# Model 1B (VADER)
multimodel1BLrename <- confint(bootmultimodel1B_L,type="norm") %>%
  dplyr::rename(conf.low = lower,
                conf.high = upper)

# Model 2 (VADER)
multimodel2Lrename <- confint(bootmultimodel2_L,type="norm") %>%
  dplyr::rename(conf.low = lower,
                conf.high = upper) %>%
  mutate(term = case_when(term == "`Incumbent Status`" ~ "Incumbent Status",
                          term == "Competition" ~ "`Competition`",
                          term == "`Number of Candidates`" ~ "`Number of Candidates",
                          term == "`Ranking Restrictions`" ~ "`Ranking Restrictions`",
                          term == "`Incumbent Presence`" ~ "`Incumbent Presence`",
                          term == "(Intercept)" ~ "(Intercept)",
                          term == "Female" ~ "Female"))

# Model 3 (VADER)
multimodel3Lrename <- confint(bootmultimodel3_L,type="norm",level=0.95) %>%
  dplyr::rename(conf.low = lower,
                conf.high = upper) %>%
  mutate(term = case_when(term == "`Incumbent Status`" ~ "Incumbent Status",
                          term == "Competition" ~ "`Competition`",
                          term == "`Number of Candidates`" ~ "`Number of Candidates",
                          term == "`Ranking Restrictions`" ~ "`Ranking Restrictions`",
                          term == "`Incumbent Presence`" ~ "`Incumbent Presence`",
                          term == "(Intercept)" ~ "(Intercept)",
                          term == "Female" ~ "Female"))
# Output results to table!


# Plot with ggcoef
multimodel1_L_plot <- ggcoef(multimodel1Lrename, exclude_intercept = TRUE,  # Model 1
                             conf.int = TRUE, conf.level = 0.95) + 
  coord_flip() + labs(x="Estimate", y = "Variable") + 
  theme(plot.title = element_text(hjust=0.5)) + 
  theme_bw() + xlim(-0.2,0.2) + theme(text = element_text(size = 16))

multimodel2_L_plot <- ggcoef(multimodel2Lrename, exclude_intercept = TRUE, # Model 3
                             conf.int = TRUE, conf.level = 0.95) + 
  coord_flip() + labs(x="Estimate", y = "Variable") + 
  theme(plot.title = element_text(hjust=0.5)) + 
  theme_bw() + xlim(-0.2,0.2) + theme(text = element_text(size = 16))

multimodel3_L_plot <- ggcoef(multimodel3Lrename, exclude_intercept = TRUE, # Model 3
                             conf.int = TRUE, conf.level = 0.95) + 
  coord_flip() + labs(x="Estimate", y = "Variable") + 
  theme(plot.title = element_text(hjust=0.5)) + 
  theme_bw() + xlim(-0.2,0.2) + theme(text = element_text(size = 16))

multimodel1A_L_plot <- ggcoef(multimodel1ALrename, exclude_intercept = TRUE, # Model 1A
                              conf.int = TRUE, conf.level = 0.95) + 
  coord_flip() + labs(x="Estimate", y = "Variable") +
  theme(plot.title = element_text(hjust=0.5)) + 
  theme_bw() + xlim(-0.2,0.2) + theme(text = element_text(size = 16))

multimodel1B_L_plot <- ggcoef(multimodel1BLrename, exclude_intercept = TRUE, # Model 1B
                              conf.int = TRUE, conf.level = 0.95) + 
  coord_flip() + labs(x="Estimate", y = "Variable") +
  theme(plot.title = element_text(hjust=0.5)) +
  theme_bw() + xlim(-0.2,0.2) + theme(text = element_text(size = 16))

## Bivariate analyses (all use VADER)----
bivariate_cand <- results_cand %>%
  mutate(Female = case_when(Female == 1 ~ "Female",
                            Female == 0 ~ "Male")) %>%
  group_by(Female) %>%
  mutate(fe_sent = mean(`Compound Sentiment (VADER)`)) %>%
  ungroup() %>%
  mutate(`Incumbent Status` = case_when(`Incumbent Status` == 1 ~ "Incumbent",
                                        `Incumbent Status` == 0 ~ "Challenger")) %>%
  group_by(`Incumbent Status`) %>%
  mutate(inc_sent = mean(`Compound Sentiment (VADER)`)) %>%
  ungroup() %>%
  mutate(`Incumbent Presence` = case_when(`Incumbent Presence` == 1 ~ "Incumbent present",
                                          `Incumbent Presence` == 0 ~ "Open-seat")) %>%
  group_by(`Incumbent Presence`) %>%
  mutate(presence_sent = mean(`Compound Sentiment (VADER)`)) %>%
  ungroup() %>%
  mutate(`Ranking Restrictions` = case_when(`Ranking Restrictions` == 1 ~ "Restrictions present",
                                            `Ranking Restrictions` == 0 ~ "No restrictions")) %>%
  group_by(`Ranking Restrictions`) %>%
  mutate(restrict_sent = mean(`Compound Sentiment (VADER)`)) %>%
  ungroup() %>%
  mutate(logged_cand = log(`Number of Candidates`)) %>% # log number of candidates
  group_by(logged_cand) %>%
  mutate(number_sent = mean(`Compound Sentiment (VADER)`)) %>%
  ungroup()


# Female vs. male sentiment (Figure 2.2, Panel A)
female_sums <- bivariate_cand %>%
  group_by(Female) %>%
  summarise(mean_fe_sent = mean(fe_sent)) %>%
  na.omit()

female_bar <- ggplot(data = female_sums, 
                          aes (x = Female, y = mean_fe_sent)) +
  geom_bar(stat = "identity",fill = "steelblue") + labs(x = "Candidate Gender", y = "Average Statement Sentiment") +
  theme(plot.title = element_text(hjust = 0.5))+ theme_bw() + 
  theme(text = element_text(size = 24)) + ylim(0,0.42) # add more to these (color and design, axes, title, etc.)

# Incumbent vs. non-incumbent status (Figure 2.2, Panel B)
incumbent_sums <- bivariate_cand %>%
  group_by(`Incumbent Status`) %>%
  summarise(mean_inc_sent = mean(inc_sent)) %>%
  na.omit()

incumbent_bar <- ggplot(data = incumbent_sums, 
                             aes (x = `Incumbent Status`, y = mean_inc_sent)) +
  geom_bar(stat = "identity",fill= "steelblue") + labs(x = "Incumbent Status", y = "Average Statement Sentiment") + 
  theme(plot.title = element_text(hjust = 0.5)) + theme_bw() + 
  theme(text = element_text(size = 24)) + ylim(0,0.42)

# Ranking restrictions (Figure 2.4)
restrict_sums <- bivariate_cand %>%
  group_by(`Ranking Restrictions`) %>%
  summarise(mean_restrict_sent = mean(restrict_sent)) %>%
  na.omit()

restrict_bar <- ggplot(data = restrict_sums, 
                            aes (x = `Ranking Restrictions`, y = mean_restrict_sent)) +
  geom_bar(stat = "identity",fill = "steelblue") + labs(x = "Ranking Restrictions", y = "Average Statement Sentiment") + 
  theme(plot.title = element_text(hjust = 0.5)) + theme_bw() +
  theme(text = element_text(size = 24)) + ylim(0,0.42)

# Incumbent presence (Figure 2.3)
presence_sums <- bivariate_cand %>%
  group_by(`Incumbent Presence`) %>%
  summarise(mean_presence_sent = mean(presence_sent)) %>%
  na.omit()

presence_bar <- ggplot(data = presence_sums, 
                            aes (x = `Incumbent Presence`, y = mean_presence_sent)) +
  geom_bar(stat = "identity",fill = "steelblue") + labs(x = "Incumbent Presence", y = "Average Statement Sentiment") + 
  theme(plot.title = element_text(hjust = 0.5)) + theme_bw() +
  theme(text = element_text(size = 24)) + ylim(0,0.42)

# Number of candidates
# Logged first, to show trends (Figure 2.6)
logged_number_sums <- bivariate_cand %>%
  group_by(logged_cand) %>%
  summarise(mean_log_number_sent = mean(number_sent)) %>%
  na.omit

log_number_chart <- ggplot(data = logged_number_sums,
                           aes (x = logged_cand, y = mean_log_number_sent)) +
  geom_point(stat = "identity",fill = "steelblue", size = 3) + labs(x = "Logged number of candidates", y = "Average Statement Sentiment") + 
  theme(plot.title = element_text(hjust = 0.5)) + theme_bw() +
  theme(text = element_text(size = 24)) + xlim(1,3) + ylim(0,0.42) +
  geom_smooth(method=lm,se=FALSE)


number_sums <- bivariate_cand %>% # restricted to 20 or fewer candidates
  group_by(`Number of Candidates`) %>%
  summarise(mean_number_sent = mean(number_sent)) %>%
  na.omit

number_chart <- ggplot(data = number_sums,
                            aes (x = `Number of Candidates`, y = mean_number_sent)) +
  geom_point(stat = "identity",fill = "steelblue", size = 3) + labs(x = "Number of candidates", y = "Average Statement Sentiment") + 
  scale_x_continuous(n.breaks=10,limits = c(2,18)) +
  theme(plot.title = element_text(hjust = 0.5)) + theme_bw() +
  theme(text = element_text(size = 22)) + ylim(0,0.45)  +
  geom_smooth(method=lm,se=FALSE)

# Competition (Figure 2.8)
comp_sums <- full_bivariate_cand %>%
  group_by(Competition) %>%
  summarise(mean_number_sent = mean(number_sent)) %>%
  na.omit()

comp_chart <- ggplot(data = comp_sums,
                          aes (x = Competition, y = mean_number_sent)) +
  geom_point(stat = "identity",fill = "steelblue") + labs(x = "2-cycle percentage point margin of victory", y = "Average Statement Sentiment") + 
  theme(plot.title = element_text(hjust = 0.5)) + theme_bw() +
  theme(text = element_text(size = 24)) + ylim(0,0.42) +
  geom_smooth(method = lm, se=FALSE)


## end of file----