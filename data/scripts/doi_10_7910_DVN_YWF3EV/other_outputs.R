
#########################################################
#                                                       #
#Created by: Chinemelu Okafor and Shivram Viswanathan   #
#Date updated:                                          #
#Updated by:                                            #
#Notes:                                                 #
# This code replicates the relevant tables from the     #
# original paper, creates the histograms and density    #
# plots for the actual measures for the WVS, and those  #
# plots for Benin and Senegal from the Afrobarometer data #
#                                                       #
#########################################################

# read measures dataset 
measures <- readRDS("data/clean/wvs_measures_by_question.rds")


## Table 3
country_measures <- measures %>% 
  group_by(country) %>% 
  drop_na() %>% 
  summarize(ethnic_elf = mean(ethnic_elf),
            chisq = mean(chisq),
            fst = mean(fst),
            cultelf = mean(cultelf))


# put these averages into measures data
measures <- left_join(measures, country_measures, 
                      by = c("country"),
                      suffix = c("", "_country")) %>%
  
# Tables  -----------------------------------------------------------------


## replicate tables 


# this reports the summary statistics in Table 3 panel A
panela <- country_measures %>% 
  select(ethnic_elf, chisq, fst, cultelf) %>% 
  summarize_all(list(Mean=mean, SD=sd, Max=max, Min=min))

## TODO: systematize using gt


# this reports the summary statistics in Table 3 panel B of the original paper
cormat <- country_measures %>% 
  dplyr::select(ethnic_elf, chisq, fst, cultelf) %>% 
  drop_na() %>% 
  cor()
n_panelb <- country_measures %>% 
  dplyr::select(ethnic_elf, chisq, fst, cultelf) %>% 
  drop_na() %>% 
  nrow()

# add standard errors
cormat_se <- apply(cormat, c(1,2), function(x) ((1-x)^2)/((n_panelb-2)^(0.5)))

## TODO: systematize using gt



# Histograms (WVS) --------------------------------------------------------------


## histogram of countries!

# cultelf

country_measures %>% 
  ggplot(aes(x=cultelf)) +
  geom_histogram(fill = "gray13" , alpha = 1) +
  xlab("Cultural Fractionalization (Countries, Averaged Over Questions)")+
  ylab("Count") +
  theme_minimal() + 
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
ggsave(file.path(getwd(), "output/figures/cultelf.png"))


#fst
country_measures %>% 
  ggplot(aes(x=fst)) +
  geom_histogram(fill = "gray13", alpha = 1) +
  xlab("Fixation Index (Countries, Averaged Over Questions)")+
  ylab("Count") +
  theme_minimal() + 
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
ggsave(file.path(getwd(), "output/figures/fst.png"))

#chisq
country_measures %>% 
  ggplot(aes(x=chisq)) +
  geom_histogram(fill = "gray13", alpha = 1) +
  xlab("Chi Squared Index (Countries, Averaged Over Questions)") +
  ylab("Count") +
  theme_minimal() + 
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
ggsave(file.path(getwd(), "output/figures/chisq.png"))

#ethelf
country_measures %>% 
  ggplot(aes(x=ethnic_elf)) +
  geom_histogram(fill = "gray13", alpha = 1) +
  xlab("Ethnic Fractionalization (Countries)") +
  ylab("Count") +
  theme_minimal() + 
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
ggsave(file.path(getwd(), "output/figures/elf.png"))


# country by country histogram
#cultelf
measures %>% mutate(country = as.factor(country)) %>% 
  drop_na(cultelf) %>% 
  ggplot(aes(x = cultelf, fill = country)) + 
  geom_density(aes(y=..density..), alpha=0.4, position="identity", bins =60) +
  ylab("Density") + xlab("Cultural Fractionalization (By Country and Question)") +
  labs(caption = "Different colors represent different countries.") + 
  theme_minimal() + 
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = "none")
ggsave(file.path(getwd(), "output/figures/cultelf_hetero.png"))

#chisq
measures %>% mutate(country = as.factor(country)) %>% 
  drop_na(chisq) %>%  
  mutate(chisq_new = ifelse(chisq > 0.1, 0.1, chisq)) %>% 
  ggplot(aes(x = chisq_new, fill = country)) + 
  geom_density(aes(y=..density..), alpha=0.4, position="identity", bins =60) +
  ylab("Density") + xlab("Chi Squared Index (By Country and Question)") +
  labs(caption = "Different colors represent different countries. Values larger than 0.1 topcoded") +
  theme_minimal() + 
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = "none")
ggsave(file.path(getwd(), "output/figures/chisq_hetero.png"))


# fst
measures %>% mutate(country = as.factor(country)) %>% 
  drop_na(fst) %>% 
  mutate(fst_new = ifelse(fst > 0.03, 0.03, fst)) %>% 
  ggplot(aes(x = fst_new, fill = country)) + 
  geom_density(aes(y=..density..), alpha=0.4, position="identity", bins =60) +
  ylab("Density") + xlab("Fixation Index (By Country and Question)") +
  labs(caption = "Different colors represent different countries. Values larger than 0.03 topcoded.") +
  theme_minimal() + 
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = "none")
ggsave(file.path(getwd(), "output/figures/fst_hetero.png"))


# Afrobarometer PLOTS -----------------------------------------------------------

af_measures <- readRDS('data/clean/All_Country_Measures.rds')


af_country_measures <- af_measures %>% 
  group_by(country) %>% 
  drop_na() %>% 
  summarize(ethnic_elf = mean(ethnic_elf),
            chisq = mean(chisq),
            fst = mean(fst),
            cultelf = mean(cultelf))

## histogram plots
# histogram of countries!

# cultelf

af_country_measures %>% 
  ggplot(aes(x=cultelf)) +
  geom_histogram(fill = "steelblue", alpha = 0.5) +
  xlab("Cultural Fractionalization (Countries, Averaged Over Questions)")+
  ylab("Count") +
  theme_minimal() + 
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
ggsave(file.path(getwd(), "output/figures/af_cultelf.png"))


#fst
af_country_measures %>% 
  ggplot(aes(x=fst)) +
  geom_histogram(fill = "steelblue", alpha = 0.5) +
  xlab("Fixation Index (Countries, Averaged Over Questions)")+
  ylab("Count") +
  theme_minimal() + 
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
ggsave(file.path(getwd(), "output/figures/af_fst.png"))

#chisq
af_country_measures %>% 
  ggplot(aes(x=chisq)) +
  geom_histogram(fill = "steelblue", alpha = 0.5) +
  xlab("Chi Squared Index (Countries, Averaged Over Questions)") +
  ylab("Count") +
  theme_minimal() + 
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
ggsave(file.path(getwd(), "output/figures/af_chisq.png"))

#ethelf
af_country_measures %>% 
  ggplot(aes(x=ethnic_elf)) +
  geom_histogram(fill = "steelblue", alpha = 0.5) +
  xlab("Ethnic Fractionalization (Countries)") +
  ylab("Count") +
  theme_minimal() + 
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
ggsave(file.path(getwd(), "output/figures/af_elf.png"))


# country by country histogram
#cultelf
af_measures %>% mutate(country = as.factor(country)) %>% 
  drop_na(cultelf) %>% 
  ggplot(aes(x = cultelf, fill = country)) + 
  geom_density(alpha=0.4, position="identity", bins =60) +
  ylab("Count") + xlab("Cultural Fractionalization (By Country and Question)") +
  labs(caption = "Different colors represent different countries.") + 
  theme_minimal() + 
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = "none")
ggsave(file.path(getwd(), "output/figures/af_cultelf_hetero.png"))

#chisq
af_measures %>% mutate(country = as.factor(country)) %>% 
  drop_na(chisq) %>%  
  mutate(chisq_new = ifelse(chisq > 1, 1, chisq)) %>% 
  ggplot(aes(x = chisq_new, fill = country)) + 
  geom_density(alpha=0.2, position="identity", bins =30) +
  ylab("Count") + xlab("Chi Squared Index (By Country and Question)") +
  labs(caption = "Different colors represent different countries. Values larger than 1 topcoded") +
  theme_minimal() + 
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = "none")
ggsave(file.path(getwd(), "output/figures/af_chisq_hetero.png"))


# fst
af_measures %>% mutate(country = as.factor(country)) %>% 
  drop_na(fst) %>% 
  mutate(fst_new = ifelse(fst > 0.3, 0.3, fst)) %>% 
  ggplot(aes(x = fst_new, fill = country)) + 
  geom_density(alpha=0.2, position="identity", bins =30) +
  ylab("Count") + xlab("Fixation Index (By Country and Question)") +
  labs(caption = "Different colors represent different countries. Values larger than 0.3 topcoded.") +
  theme_minimal() + 
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = "none")
ggsave(file.path(getwd(), "output/figures/af_fst_hetero.png"))


# SIMULATION --------------------------------------------------------------

# just benin and senegal
ben_senegal <- readRDS('data/clean/Benin_Senegal_Measures.rds')


# question vector
finalqlist <- c("Q73A Q73B Q73C Q73D Q73E Q29A Q29B Q29C Q70A Q70B Q70C Q86A Q86B Q86C Q86D Q86E Q86F Q87 Q18 Q19 Q20 Q21 Q22 Q23 Q24 Q28 Q33 Q34 Q35 Q36 Q37 Q38 Q39 Q40 Q41 Q42 Q49 Q50 Q51 Q61A Q61B Q61C Q61D Q61E Q64 Q68A Q68B Q68C Q68D Q68E Q68F Q69A Q69B Q69C Q69D Q69E Q69F Q75C Q78 Q85A Q85B Q89A Q98B Q13A Q13B Q13C Q13D Q14 Q15 Q16 Q17A Q17B Q17C Q25A Q25B Q26A Q26B Q26C Q26D Q26E Q30A Q30B Q30C Q30D Q31A Q31B Q31C Q32 Q43 Q46A Q46B Q46C Q46D Q47A Q47B Q47C Q47D Q47E Q48A Q48B Q48C Q52A Q52B Q52C Q52D Q52E Q53 Q54 Q55 Q56A Q56B Q56C Q56D Q56E Q56F Q56G Q56H Q56I Q59A Q59B Q59C Q59D Q59E Q59F Q59G Q59H Q59I Q59J Q60A Q60B Q60C Q60D Q60E Q60F Q60G Q61F Q62A Q62B Q65A Q65B Q65C Q65D Q65E Q65F Q65G Q65H Q65I Q65J Q65K Q65L Q65M Q65N Q65O Q65P Q66A Q66B Q66C Q66D Q66E Q67A Q67B Q67C Q67D Q67E Q71A Q71B Q71C Q74 Q75A Q75B Q76A Q76B Q78A1 Q85C Q88A Q88B Q88C Q11 Q12 Q27 Q44 Q45 Q57 Q58 Q63PT1 Q63PT2 Q63PT3 Q72A Q72B Q72C Q77 Q98A")
finalqlist <- strsplit(finalqlist, " ")
finalqlist <- unlist(finalqlist)



# tibble
sims <- tibble(
  rep = numeric(),
  nquest = numeric(),
  country = character(),
  countryname = character(),
  countrycode = character(),
  ELF = numeric(),
  CF = numeric(),
  FST = numeric(),
  CHISQ = numeric()
)

reps <- 2000
set.seed(2001)
for ( nq in c(25, 50, 100, 150) ) {
  for (i in 1:reps) {
  
    # sample questions from vector
    sampq <- sample(finalqlist, nq, replace = FALSE)
    
    # subset data to just be questions
    temp <- ben_senegal %>% 
      filter(question %in% sampq) %>% 
      select(country, countryname, countrycode, ethnic_elf, cultelf, fst, chisq) %>% 
      group_by(country, countryname, countrycode) %>% 
      drop_na() %>% 
      summarize(ELF = mean(ethnic_elf),
                CHISQ = mean(chisq),
                FST = mean(fst),
                CF = mean(cultelf))
  
    # store values into tibble
    sims <- bind_rows(sims, temp) %>% 
      mutate(rep = ifelse(is.na(rep), i, rep))
    
  }
  sims <- sims %>% 
    mutate(nquest = ifelse(is.na(nquest), nq, nquest))
}


# compute means of each simulation
meanvals <- sims %>%
  group_by(nquest, country) %>% 
  select(country, nquest, CF, FST, CHISQ)  %>% 
  summarise(CF = mean(CF), 
            FST = mean(FST), 
            CHISQ = mean(CHISQ))

# get actual mean
bs <- ben_senegal %>% 
  group_by(country) %>% 
  select(country, CF, FST, chisq)  %>% 
  drop_na() %>% 
  summarise(CF = mean(CF), 
            FST = mean(FST), 
            CHISQ = mean(chisq)) %>% 
  mutate(nquest = 0)

meanvals <- bind_rows(meanvals, bs)

group.colors <- c('Benin' = "#00AFBB", 'Senegal' = "#FC4E07")


addSmallLegend <- function(myPlot, pointSize = 1, textSize = 10, spaceLegend = 0.5) {
  myPlot +
    guides(shape = guide_legend(override.aes = list(size = 1)),
           color = guide_legend(override.aes = list(size = 1))) +
    theme(legend.title = element_text(size = textSize), 
          legend.text  = element_text(size = textSize),
          legend.key.size = unit(spaceLegend, "lines"))
}



for ( nq in c(25, 50, 100, 150) ) {
  p <-  sims %>% 
    filter(nquest == nq) %>% 
    ggplot(aes(x = CF, fill = countryname)) + 
    geom_density(aes(y=..density..), alpha=0.4, position="identity", bins =60) +
    geom_vline(
      aes(xintercept = as.numeric(meanvals[meanvals$country == 1 & meanvals$nquest == 0, 'CF'])),
      linetype = "dashed",
      color = "#FC4E07") + # Benin true mean (all questions)
    geom_vline(
      aes(xintercept = as.numeric(meanvals[meanvals$country == 15 & meanvals$nquest == 0, 'CF'])),
      linetype = "dashed",
      color = "#00AFBB") + # Senegal true mean
    ylab("Density") + xlab("Cultural Fractionalization") +
    labs(fill = "",
         caption = paste0("Means for both countries using all questions shown with dashed line. N = ", nq)) + 
    theme_minimal() + 
    theme(axis.line = element_line(color = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position = "bottom") 
  addSmallLegend(p)
  ggsave(file.path(getwd(), paste0("output/figures/bensen_cf_", nq, ".png")))
  
  p <- sims %>% 
    filter(nquest == nq) %>% 
    ggplot(aes(x = FST, fill = countryname)) + 
    geom_density(aes(y=..scaled..), alpha=0.4, position="identity", bins =60)+
    geom_vline(
      aes(xintercept = as.numeric(meanvals[meanvals$country == 1 & meanvals$nquest == 0, 'FST'])),
      linetype = "dashed",
      color = "#FC4E07") + # Benin true mean (all questions)
    geom_vline(
      aes(xintercept = as.numeric(meanvals[meanvals$country == 15 & meanvals$nquest == 0, 'FST'])),
      linetype = "dashed",
      color = "#00AFBB") + # Senegal true mean
    ylab("Density") + xlab("FST Index") +
    labs(fill = "",
         caption = paste0("Means for both countries using all questions shown with dashed line. N = ", nq)) + 
    theme_minimal() + 
    theme(axis.line = element_line(color = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position = "bottom")
  addSmallLegend(p)
  ggsave(file.path(getwd(), paste0("output/figures/bensen_fst_", nq, ".png")))
  
  p <- sims %>% 
    filter(nquest == nq) %>% 
    ggplot(aes(x = CHISQ, fill = countryname)) + 
    geom_density(aes(y=..scaled..), alpha=0.4, position="identity", bins =60) +
    geom_vline(
      aes(xintercept = as.numeric(meanvals[meanvals$country == 1 & meanvals$nquest == 0, 'CHISQ'])),
      linetype = "dashed",
      color = "#FC4E07") + # Benin true mean (all questions)
    geom_vline(
      aes(xintercept = as.numeric(meanvals[meanvals$country == 15 & meanvals$nquest == 0, 'CHISQ'])),
      linetype = "dashed",
      color = "#00AFBB") + # Senegal true mean
    ylab("Density") + xlab("Chi Squared Index") +
    labs(fill = "",
         caption = paste0("Means for both countries using all questions shown with dashed line. N = ", nq))+ 
    theme_minimal() + 
    theme(axis.line = element_line(color = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position = "bottom")
  addSmallLegend(p)
  ggsave(file.path(getwd(), paste0("output/figures/bensen_chisq_", nq, ".png")))
}

## TODO: Komolgrov-Smirnov Test

# CF test of overlap
ks.test(
  as.numeric(unlist(sims[sims$nquest == 25 & sims$country == 1 & sims$rep < 100, 'CF'])),
  as.numeric(unlist(sims[sims$nquest == 25 & sims$country == 15 & sims$rep < 100, 'CF'])),
  alternative("two.sided"))

# FST test of overlap
ks.test(
  as.numeric(unlist(sims[sims$nquest == 100 & sims$country == 1, 'FST'])),
  as.numeric(unlist(sims[sims$nquest == 100 & sims$country == 15, 'FST']))
  )

  
                 