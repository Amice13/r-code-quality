#### clear work space ####
rm(list = ls())

#### Packages ####
packages <- c("dplyr", "ggplot2", "lme4",  "ggeffects", 
              "multiwayvcov", "lmtest", "ggstance",
              "stargazer", "forcats", "broom", "MASS",
              "stringi", "ggpubr", "cobalt", "ggalluvial")

sapply(packages, 
       FUN = function(x){
         if(x %in% rownames(installed.packages()) == FALSE){
           install.packages(x)
         }
         library(x, character.only = TRUE)
       })

#### Defining functions ####
meanOrMode <- function(x){
  mode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  x <- na.omit(x)
  out <- ifelse(length(unique(x)) == 2, mode(x), mean(x))
  return(out)
}


#### Data ####
load("EffectOfProtocol14.RData")

relevantVotes <- filter(df,
                        affectedByProtocol14 == 1,
                        adhocjudge == 0) %>%
  mutate(interactionTerm = nationalRegularJudge * afterRussianRatification,
         lastname = as.factor(lastname), 
         age = judgmentYear - biryear,
         ageIn2010 = 2010 - biryear, 
         academiaOrPrivatePractice = ifelse(`Main previous position` == "academic" |
                                              `Main previous position` == "private practice", 1,0),
         inLastTerm = ifelse(judgmentYear > lastAppointmentYearWithRenewableTerms, 1, 0))

### Number of votes and judgments:
nrow(relevantVotes)
length(unique(relevantVotes$filename))
min(relevantVotes$judgmentYear)
max(relevantVotes$judgmentYear)


### Descriptives on judges' careers (for Supplementary Materials): 
relevantVotes %>% 
  ungroup() %>% 
  dplyr::select( lastname,`Main previous position`) %>% 
  mutate(`Main previous position` = ifelse(`Main previous position` == "judge",
                                           "domestic judge", `Main previous position`)) %>% 
  distinct(lastname, .keep_all = TRUE) %>% 
  ggplot(aes(x = `Main previous position`))+
  geom_bar(stat = "count")+
  ylab("N")+
  xlab("")+
  theme_classic()
ggsave(filename = "preCourtCareers.pdf", width = 7, height = 7)

## alluvium plot of relationship between pre- and post-Court careers (for supplementary materials)
relevantVotes %>% 
  ungroup() %>% 
  dplyr::select( lastname, `Main previous position`, PostCourtMainPosition,ageIn2010) %>% 
  mutate(`Main previous position` = ifelse(`Main previous position` == "judge",
                                           "domestic judge", `Main previous position`), 
         PostCourtMainPosition = ifelse(PostCourtMainPosition == "judge",
                                        "domestic judge", PostCourtMainPosition),
         olderThan64in2010 = ifelse(ageIn2010 > 64,"65 years or\nolder in 2010", 
                                    "younger than 65\nyears in 2010"),
         `Main previous position` = factor(`Main previous position`, 
                                           levels = c("academic", "private practice", 
                                                      "domestic judge", 
                                                      "state agent", 
                                                      "politician")),
         PostCourtMainPosition = factor(PostCourtMainPosition, 
                                        levels = c("academic", "private practice", 
                                                   "NGO", "international organization", 
                                                   "international judge", 
                                                   "domestic judge", 
                                                   "state agent", 
                                                   "politician", 
                                                   "unclear"))) %>%
  distinct(lastname, .keep_all = TRUE) %>% 
  group_by(`Main previous position`, PostCourtMainPosition, olderThan64in2010) %>% 
  summarize(Freq = n()) %>% 
  ggplot( aes(y = Freq, 
              axis1 = `Main previous position`, 
              axis2 = PostCourtMainPosition)) +
  geom_alluvium(aes(fill = olderThan64in2010), width = 1/12) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", infer.label = TRUE)+
  scale_x_discrete(limits = c("Main previous position", "PostCourtMainPosition"),
                   labels = c("Pre-Court career", "Post-Court career"), 
                   expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1")+
  theme_classic()+
  ylab("N")+
  theme(legend.title = element_blank())
ggsave(filename = "careeralluvium.pdf", height = 10, width = 10)


#### Descriptive analysis of judges' voting patterns before and after the reform (for supplementary materials)
judgesDescriptivesAffected <-relevantVotes %>% 
  ungroup() %>% 
  mutate(nationalRegularJudge = ifelse(nationalRegularJudge == 1, "National\njudge\n", "Other\njudge"),
         afterRussianRatification = ifelse(afterRussianRatification == 1, "After Russian\nratification",
                                           "Before Russian\nratification"), 
         afterRussianRatification = factor(afterRussianRatification, levels = c("Before Russian\nratification", 
                                                                                "After Russian\nratification"))) %>% 
  group_by(afterRussianRatification, nationalRegularJudge) %>% 
  summarize(proViolationVote = mean(proViolationVote)) %>% 
  ggplot(aes(x = afterRussianRatification, 
             y = proViolationVote, 
             group = nationalRegularJudge,
             color = nationalRegularJudge, 
             shape = nationalRegularJudge, 
             linetype = nationalRegularJudge))+
  geom_point()+
  geom_line()+
  ylim(0.8,0.95)+
  theme_classic()+
  theme(legend.title = element_blank())+
  ylab("Share pro-violation votes")+
  xlab("")+
  ggtitle("Votes of affected judges,\n1998-2016")

judgesDescriptivesAffected2008to2012 <- relevantVotes %>% 
  filter(judgmentYear >= 2008,
         judgmentYear <= 2012) %>% 
  ungroup() %>% 
  mutate(nationalRegularJudge = ifelse(nationalRegularJudge == 1, "National\njudge\n", "Other\njudge"),
         afterRussianRatification = ifelse(afterRussianRatification == 1, "After Russian\nratification",
                                           "Before Russian\nratification"), 
         afterRussianRatification = factor(afterRussianRatification, levels = c("Before Russian\nratification", 
                                                                                "After Russian\nratification"))) %>% 
  group_by(afterRussianRatification, nationalRegularJudge) %>% 
  summarize(proViolationVote = mean(proViolationVote)) %>% 
  ggplot(aes(x = afterRussianRatification, 
             y = proViolationVote, 
             group = nationalRegularJudge,
             color = nationalRegularJudge, 
             shape = nationalRegularJudge, 
             linetype = nationalRegularJudge))+
  geom_point()+
  geom_line()+
  ylim(0.8,0.95)+
  theme_classic()+
  theme(legend.title = element_blank())+
  ylab("Share pro-violation votes")+
  xlab("")+
  ggtitle("Votes of affected judges,\n2008-2012")


judgesDescriptivesAffectedSplitVotes <- relevantVotes %>% 
  ungroup() %>% 
  filter(unanimousJudgement == 0) %>% 
  mutate(nationalRegularJudge = ifelse(nationalRegularJudge == 1, "National\njudge\n", "Other\njudge"),
         afterRussianRatification = ifelse(afterRussianRatification == 1, "After Russian\nratification",
                                           "Before Russian\nratification"), 
         afterRussianRatification = factor(afterRussianRatification, levels = c("Before Russian\nratification", 
                                                                                "After Russian\nratification"))) %>% 
  group_by(afterRussianRatification, nationalRegularJudge) %>% 
  summarize(proViolationVote = mean(proViolationVote)) %>% 
  ggplot(aes(x = afterRussianRatification, 
             y = proViolationVote, 
             group = nationalRegularJudge,
             color = nationalRegularJudge, 
             shape = nationalRegularJudge, 
             linetype = nationalRegularJudge))+
  geom_point() +
  geom_line() +
  ylim(0.2,0.7) +
  theme_classic() +
  theme(legend.title = element_blank())+
  ylab("Share pro-violation votes")+
  xlab("")+
  ggtitle("Votes of affected judges,\n1998-2016, split judgments")


judgesDescriptivesAffected2008to2012SplitVotes <- relevantVotes %>% 
  filter(unanimousJudgement == 0,
         judgmentYear >= 2008,
         judgmentYear <= 2012) %>% 
  ungroup() %>% 
  mutate(nationalRegularJudge = ifelse(nationalRegularJudge == 1, "National\njudge\n", "Other\njudge"),
         afterRussianRatification = ifelse(afterRussianRatification == 1, "After Russian\nratification",
                                           "Before Russian\nratification"), 
         afterRussianRatification = factor(afterRussianRatification, levels = c("Before Russian\nratification", 
                                                                                "After Russian\nratification"))) %>% 
  group_by(afterRussianRatification, nationalRegularJudge) %>% 
  summarize(proViolationVote = mean(proViolationVote)) %>% 
  ggplot(aes(x = afterRussianRatification, 
             y = proViolationVote, 
             group = nationalRegularJudge,
             color = nationalRegularJudge, 
             shape = nationalRegularJudge, 
             linetype = nationalRegularJudge))+
  geom_point()+
  geom_line()+
  ylim(0.2,0.7)+
  theme_classic()+
  theme(legend.title = element_blank())+
  ylab("Share pro-violation votes")+
  xlab("")+
  ggtitle("Votes of affected judges,\n2008-2012, split judgments")




ggarrange(judgesDescriptivesAffected, judgesDescriptivesAffectedSplitVotes, 
          judgesDescriptivesAffected2008to2012,judgesDescriptivesAffected2008to2012SplitVotes,
          nrow = 2, ncol = 2)
ggsave(filename = "judgesDescriptives.pdf", width = 7, height = 7)

#### Making table with summary statistics (for supplementary materials) ####
relevantVotes$age65orOlderin2010 <- ifelse(relevantVotes$ageIn2010 > 64,1,0)
descriptives <- as.data.frame(relevantVotes) %>%
  dplyr::select(proViolationVote, 
                nationalRegularJudge,
                afterRussianRatification,
                interactionTerm,
                age65orOlderin2010, 
                academiaOrPrivatePractice,
                GrandChamberJudgment,
                importance1, 
                importance2, 
                importance3, 
                article2case,
                article3case,
                article5case,
                article6case,
                article8case,
                article10case,
                article13case,
                article14case,
                protocol1_1case, 
                yearsSinceAppointment,
                age)

stargazer(descriptives, 
          title = "Summary Statistics", 
          summary = TRUE, 
          out = "SummaryStats.tex",
          covariate.labels = c("Pro-violation vote",
                               "National judge", 
                               "After Russian Ratification of Protocol 14",
                               "National judge *\n After Russian Ratification of Protocol 14",
                               "65 years or older in 2010", 
                               "Background in academia or private practice",
                               "Grand Chamber judgment", 
                               "Importance level 1", 
                               "Importance level 2", 
                               "Importance level 3", 
                               "Right to life case", 
                               "Prohibition of torture case", 
                               "Right to liberty case", 
                               "Right to fair trial case", 
                               "Right to respect for private and family life case", 
                               "Freedom of expression case", 
                               "Right to effective remedy case", 
                               "Right to discrimination case", 
                               "Right to private property case", 
                               "Years since appointment",
                               "Age"),
          font.size = "scriptsize", 
          label = "tab:summaryStats",
          style = "ajps")
#### Making table of affected Judges (for supplementary materials) #####
affectedJudges <- relevantVotes %>%
  mutate(lastname= as.character(lastname)) %>% 
  group_by(lastname) %>%
  mutate(numberOfVotes = n(), 
         numberOfNationalVotes = sum(nationalRegularJudge),
         numberOfVotesAfterRussianRatification = sum(afterRussianRatification),
         numberOfInteraction = sum(interactionTerm ),
         lastyear = ifelse(is.na(lastyear), "censored", as.character(lastyear))) %>%
  dplyr::select(lastname, judgecountry, appointmentyear, lastyear, 
                numberOfVotes, numberOfNationalVotes, numberOfVotesAfterRussianRatification, numberOfInteraction )%>%
  distinct() %>%
  arrange(appointmentyear)  

stri_sub(affectedJudges$lastname,1,1)  <- toupper(stri_sub(affectedJudges$lastname,1,1))
affectedJudges$lastname <- ifelse(affectedJudges$lastname == "SpielmannD", "Spielmann (Dean)", affectedJudges$lastname)
affectedJudges$lastname <- ifelse(affectedJudges$lastname == "Sandstrom", "Fura", affectedJudges$lastname)


stargazer(affectedJudges, 
          type = "latex", 
          covariate.labels = c("Name ", "Nominating state", "Appointed", "Last year",  "Votes",
                               "National votes", "Post-reform votes",
                               "Post-reform national votes"),
          title = "Judges serving on the ECtHR at the time of Russian Ratification of Protocol 14 ",
          out = "affectedJudges.tex",
          font.size = "tiny",
          rownames = FALSE,
          summary = FALSE,
          column.sep.width = "0pt",
          label = "tab:affectedJudges",
          style = "ajps")

##################################
#### Estimating logit models #####
##################################
### Model 1: 
affectedJudgesControlsFE <- glm(proViolationVote ~ 
                                  nationalRegularJudge + afterRussianRatification + interactionTerm +
                                  GrandChamberJudgment +
                                  importance1 +
                                  importance2 +
                                  importance3 +
                                  article2case +
                                  article3case +
                                  article5case +
                                  article6case +
                                  article8case +
                                  article10case +
                                  article13case +
                                  article14case +
                                  protocol1_1case + 
                                  yearsSinceAppointment +
                                  age +
                                  lastname, 
                                family = binomial(link = "logit"),
                                data = relevantVotes, 
                                x = TRUE)
affectedJudgesControlsFE$ses  <- coeftest(affectedJudgesControlsFE, 
                                          vcov. = cluster.vcov(affectedJudgesControlsFE, 
                                                               cluster = relevantVotes$filename))[,2]
affectedJudgesControlsFE$ps  <- coeftest(affectedJudgesControlsFE, 
                                         vcov. = cluster.vcov(affectedJudgesControlsFE, 
                                                              cluster = relevantVotes$filename))[,4]
### model 2:
affectedJudgesControlsSplitJudgmentsFE <- glm(proViolationVote ~ 
                                                nationalRegularJudge + afterRussianRatification + interactionTerm +
                                                GrandChamberJudgment +
                                                importance1 +
                                                importance2 +
                                                importance3 +
                                                article2case +
                                                article3case +
                                                article5case +
                                                article6case +
                                                article8case +
                                                article10case +
                                                article13case +
                                                article14case +
                                                protocol1_1case + 
                                                yearsSinceAppointment +
                                                age +
                                                lastname, 
                                              family = binomial(link = "logit"),
                                              data = filter(relevantVotes, 
                                                            unanimousJudgement == 0),
                                              x = TRUE)

affectedJudgesControlsSplitJudgmentsFE$ses <- coeftest(affectedJudgesControlsSplitJudgmentsFE , 
                                                       vcov. = cluster.vcov(affectedJudgesControlsSplitJudgmentsFE, 
                                                                            cluster = filter(relevantVotes, 
                                                                                             unanimousJudgement == 0)$filename))[,2]

affectedJudgesControlsSplitJudgmentsFE$ps <- coeftest(affectedJudgesControlsSplitJudgmentsFE , 
                                                      vcov. = cluster.vcov(affectedJudgesControlsSplitJudgmentsFE, 
                                                                           cluster = filter(relevantVotes, 
                                                                                            unanimousJudgement == 0)$filename))[,4]

##### Making predicted probability graph based on model 2:
set.seed(6789)
simb <- mvrnorm(n = 1000,
                mu = na.omit(coefficients(affectedJudgesControlsSplitJudgmentsFE)),
                Sigma = na.omit(cluster.vcov(affectedJudgesControlsSplitJudgmentsFE,
                                             cluster = filter(relevantVotes, unanimousJudgement == 0)$filename)))
set.x <- as.data.frame(affectedJudgesControlsSplitJudgmentsFE$x) %>% 
  summarize_all(.funs = meanOrMode)
set.x <- bind_rows(mutate(set.x, 
                          nationalRegularJudge = 1,
                          afterRussianRatification = 0,
                          interactionTerm = nationalRegularJudge * afterRussianRatification),
                   mutate(set.x, 
                          nationalRegularJudge = 1,
                          afterRussianRatification = 1,
                          interactionTerm = nationalRegularJudge * afterRussianRatification),
                   mutate(set.x, 
                          nationalRegularJudge = 0,
                          afterRussianRatification = 0,
                          interactionTerm = nationalRegularJudge * afterRussianRatification),
                   mutate(set.x, 
                          nationalRegularJudge = 0,
                          afterRussianRatification = 1,
                          interactionTerm = nationalRegularJudge * afterRussianRatification))

set.x <- as.matrix(set.x[colnames(set.x) %in%  names(na.omit(coefficients(affectedJudgesControlsSplitJudgmentsFE)))])

x.beta <- set.x %*% t(simb)
exp.y <- 1/(1+exp(-x.beta))

quantile.values <- as.data.frame(t(apply(X = exp.y, MARGIN = 1, FUN = quantile, probs = c(.025,.5,.975)))) %>% 
  mutate(nationalRegularJudge = c("National\njudge","National\njudge","Other\njudge","Other\njudge"), 
         afterRussianRatification = factor(c("Before Russian ratification",
                                             "After Russian ratification",
                                             "Before Russian ratification",
                                             "After Russian ratification"),
                                           levels = c("Before Russian ratification",
                                                      "After Russian ratification" )))
ggplot(quantile.values, 
       aes(x = nationalRegularJudge, 
           y = `50%`, 
           ymin = `2.5%`,
           ymax =  `97.5%`)) +
  facet_grid( cols= vars(afterRussianRatification)) +
  geom_point() +
  geom_errorbar(width = 0.2) +
  ylab("Pr(Violation vote = 1)")+
  xlab("") +
  theme_classic() +
  theme(text = element_text(size=20)) 
ggsave("fg2.pdf", height = 7, width = 7)
ggsave("fg2.tiff", width = 7, height = 7, dpi = 2000)
### Interaction with career background: 
### Model 3: 
affectedJudgesControlsFEacademiaOrPrivatePractice <- glm(proViolationVote ~ 
                                                           nationalRegularJudge + afterRussianRatification + interactionTerm +
                                                           GrandChamberJudgment +
                                                           importance1 +
                                                           importance2 +
                                                           importance3 +
                                                           article2case +
                                                           article3case +
                                                           article5case +
                                                           article6case +
                                                           article8case +
                                                           article10case +
                                                           article13case +
                                                           article14case +
                                                           protocol1_1case + 
                                                           yearsSinceAppointment +
                                                           age +
                                                           lastname, 
                                                         family = binomial(link = "logit"),
                                                         data = filter(relevantVotes, academiaOrPrivatePractice ==1))


affectedJudgesControlsFEacademiaOrPrivatePractice$ses  <- coeftest(affectedJudgesControlsFEacademiaOrPrivatePractice, 
                                                                   vcov. = cluster.vcov(affectedJudgesControlsFEacademiaOrPrivatePractice, 
                                                                                        cluster = filter(relevantVotes, academiaOrPrivatePractice ==1)$filename))[,2]
affectedJudgesControlsFEacademiaOrPrivatePractice$ps  <- coeftest(affectedJudgesControlsFEacademiaOrPrivatePractice, 
                                                                  vcov. = cluster.vcov(affectedJudgesControlsFEacademiaOrPrivatePractice, 
                                                                                       cluster = filter(relevantVotes, academiaOrPrivatePractice ==1)$filename))[,4]

### Model 4: 
affectedJudgesControlsFENotAcademiaOrPrivatePractice <- glm(proViolationVote ~ 
                                                              nationalRegularJudge + afterRussianRatification + interactionTerm +
                                                              GrandChamberJudgment +
                                                              importance1 +
                                                              importance2 +
                                                              importance3 +
                                                              article2case +
                                                              article3case +
                                                              article5case +
                                                              article6case +
                                                              article8case +
                                                              article10case +
                                                              article13case +
                                                              article14case +
                                                              protocol1_1case + 
                                                              yearsSinceAppointment +
                                                              age +
                                                              lastname, 
                                                            family = binomial(link = "logit"),
                                                            data = filter(relevantVotes, academiaOrPrivatePractice ==0))


affectedJudgesControlsFENotAcademiaOrPrivatePractice$ses  <- coeftest(affectedJudgesControlsFENotAcademiaOrPrivatePractice, 
                                                                      vcov. = cluster.vcov(affectedJudgesControlsFENotAcademiaOrPrivatePractice, 
                                                                                           cluster = filter(relevantVotes, academiaOrPrivatePractice ==0)$filename))[,2]
affectedJudgesControlsFENotAcademiaOrPrivatePractice$ps  <- coeftest(affectedJudgesControlsFENotAcademiaOrPrivatePractice, 
                                                                     vcov. = cluster.vcov(affectedJudgesControlsFENotAcademiaOrPrivatePractice, 
                                                                                          cluster = filter(relevantVotes, academiaOrPrivatePractice ==0)$filename))[,4]

### Three-way interaction for pre-Court career (for supplementary materials. )

affectedJudgesControlsAcademiaOrPrivatePracticeThreeway <- glm(proViolationVote ~ 
                                                                 nationalRegularJudge * afterRussianRatification * academiaOrPrivatePractice +
                                                                 GrandChamberJudgment +
                                                                 importance1 +
                                                                 importance2 +
                                                                 importance3 +
                                                                 article2case +
                                                                 article3case +
                                                                 article5case +
                                                                 article6case +
                                                                 article8case +
                                                                 article10case +
                                                                 article13case +
                                                                 article14case +
                                                                 protocol1_1case + 
                                                                 yearsSinceAppointment +
                                                                 age, 
                                                               family = binomial(link = "logit"),
                                                               data = relevantVotes)

affectedJudgesControlsAcademiaOrPrivatePracticeThreeway$ses  <- coeftest(affectedJudgesControlsAcademiaOrPrivatePracticeThreeway, 
                                                                         vcov. = cluster.vcov(affectedJudgesControlsAcademiaOrPrivatePracticeThreeway, 
                                                                                              cluster = relevantVotes$filename))[,2]

affectedJudgesControlsAcademiaOrPrivatePracticeThreeway$ps  <- coeftest(affectedJudgesControlsAcademiaOrPrivatePracticeThreeway, 
                                                                        vcov. = cluster.vcov(affectedJudgesControlsAcademiaOrPrivatePracticeThreeway, 
                                                                                             cluster = relevantVotes$filename))[,4]


affectedJudgesControlsAcademiaOrPrivatePracticeThreewayOpposite <- glm(proViolationVote ~ 
                                                                         nationalRegularJudge * afterRussianRatification * ifelse(academiaOrPrivatePractice==1,0,1) +
                                                                         GrandChamberJudgment +
                                                                         importance1 +
                                                                         importance2 +
                                                                         importance3 +
                                                                         article2case +
                                                                         article3case +
                                                                         article5case +
                                                                         article6case +
                                                                         article8case +
                                                                         article10case +
                                                                         article13case +
                                                                         article14case +
                                                                         protocol1_1case + 
                                                                         yearsSinceAppointment +
                                                                         age, 
                                                                       family = binomial(link = "logit"),
                                                                       data = relevantVotes)

affectedJudgesControlsAcademiaOrPrivatePracticeThreewayTerms <- bind_rows(tidy(coeftest(affectedJudgesControlsAcademiaOrPrivatePracticeThreewayOpposite, 
                                                                                        vcov. = cluster.vcov(affectedJudgesControlsAcademiaOrPrivatePracticeThreewayOpposite, 
                                                                                                             cluster = relevantVotes$filename))) %>% 
                                                                            filter(term == "nationalRegularJudge:afterRussianRatification"),
                                                                          tidy(coeftest(affectedJudgesControlsAcademiaOrPrivatePracticeThreeway, 
                                                                                        vcov. = cluster.vcov(affectedJudgesControlsAcademiaOrPrivatePracticeThreeway, 
                                                                                                             cluster = relevantVotes$filename))) %>% 
                                                                            filter(term == "nationalRegularJudge:afterRussianRatification"))%>% 
  mutate(JudgeBackground = c( "Judges with backgrounds from\nacademia or private practice", "Judges with other backgrounds"))


ggplot(affectedJudgesControlsAcademiaOrPrivatePracticeThreewayTerms, 
       aes(x = JudgeBackground, 
           y = estimate, 
           ymin = estimate - 1.96 * std.error, 
           ymax = estimate + 1.96 * std.error)) +
  geom_errorbar(width = 0.1) +
  geom_point() +
  geom_hline(aes(yintercept = 0), 
             linetype = "dashed", 
             color = "grey")+
  theme_classic() +
  xlab("") +
  ylab("Judge nominated by respondent state * after Russian ratification")
ggsave("affectedJudgesControlsAcademiaOrPrivatePracticeThreewayTerms.pdf", height = 7, width = 7)




### Model 5: 
affectedJudgesControlsFEYoung <- glm(proViolationVote ~ 
                                       nationalRegularJudge + afterRussianRatification + interactionTerm +
                                       GrandChamberJudgment +
                                       importance1 +
                                       importance2 +
                                       importance3 +
                                       article2case +
                                       article3case +
                                       article5case +
                                       article6case +
                                       article8case +
                                       article10case +
                                       article13case +
                                       article14case +
                                       protocol1_1case + 
                                       yearsSinceAppointment +
                                       age +
                                       lastname, 
                                     family = binomial(link = "logit"),
                                     data = filter(relevantVotes, ageIn2010 < 65))
affectedJudgesControlsFEYoung$ses  <- coeftest(affectedJudgesControlsFE, 
                                               vcov. = cluster.vcov(affectedJudgesControlsFEYoung, 
                                                                    cluster = filter(relevantVotes, ageIn2010 < 65)$filename))[,2]

affectedJudgesControlsFEYoung$ps  <- coeftest(affectedJudgesControlsFE, 
                                              vcov. = cluster.vcov(affectedJudgesControlsFEYoung, 
                                                                   cluster = filter(relevantVotes, ageIn2010 < 65)$filename))[,4]

### Model 6: 
affectedJudgesControlsFEOld <- glm(proViolationVote ~ 
                                     nationalRegularJudge + afterRussianRatification + interactionTerm +
                                     GrandChamberJudgment +
                                     importance1 +
                                     importance2 +
                                     importance3 +
                                     article2case +
                                     article3case +
                                     article5case +
                                     article6case +
                                     article8case +
                                     article10case +
                                     article13case +
                                     article14case +
                                     protocol1_1case + 
                                     yearsSinceAppointment +
                                     age +
                                     lastname, 
                                   family = binomial(link = "logit"),
                                   data = filter(relevantVotes, ageIn2010 > 64))
affectedJudgesControlsFEOld$ses  <- coeftest(affectedJudgesControlsFEOld, 
                                             vcov. = cluster.vcov(affectedJudgesControlsFEOld, 
                                                                  cluster = filter(relevantVotes, ageIn2010 > 64)$filename))[,2]


affectedJudgesControlsFEOld$ps  <- coeftest(affectedJudgesControlsFEOld, 
                                            vcov. = cluster.vcov(affectedJudgesControlsFEOld, 
                                                                 cluster = filter(relevantVotes, ageIn2010 > 64)$filename))[,4]

relevantVotes$olderThan65in2010 <- ifelse(relevantVotes$ageIn2010 > 64,1,0)
affectedJudgesControlsFEAgeThreeway <- glm(proViolationVote ~ 
                                             nationalRegularJudge * afterRussianRatification * olderThan65in2010 +
                                             GrandChamberJudgment +
                                             importance1 +
                                             importance2 +
                                             importance3 +
                                             article2case +
                                             article3case +
                                             article5case +
                                             article6case +
                                             article8case +
                                             article10case +
                                             article13case +
                                             article14case +
                                             protocol1_1case + 
                                             yearsSinceAppointment +
                                             age,
                                           family = binomial(link = "logit"),
                                           data = relevantVotes,
                                           x = TRUE)

affectedJudgesControlsFEAgeThreeway$ses  <- coeftest(affectedJudgesControlsFEAgeThreeway,
                                                     vcov. = cluster.vcov(affectedJudgesControlsFEAgeThreeway,
                                                                          cluster = relevantVotes$filename))[,2]


affectedJudgesControlsFEAgeThreeway$ps  <- coeftest(affectedJudgesControlsFEAgeThreeway,
                                                    vcov. = cluster.vcov(affectedJudgesControlsFEAgeThreeway,
                                                                         cluster = relevantVotes$filename))[,4]




affectedJudgesControlsFEAgeThreewayOpposite <- glm(proViolationVote ~ 
                                                     nationalRegularJudge * afterRussianRatification * I(olderThan65in2010 == 0) +
                                                     GrandChamberJudgment +
                                                     importance1 +
                                                     importance2 +
                                                     importance3 +
                                                     article2case +
                                                     article3case +
                                                     article5case +
                                                     article6case +
                                                     article8case +
                                                     article10case +
                                                     article13case +
                                                     article14case +
                                                     protocol1_1case + 
                                                     yearsSinceAppointment +
                                                     age,
                                                   # lastname, 
                                                   family = binomial(link = "logit"),
                                                   data = relevantVotes,
                                                   x = TRUE)

affectedJudgesControlsFEAgeThreewayTerms <- bind_rows(tidy(coeftest(affectedJudgesControlsFEAgeThreewayOpposite, 
                                                                    vcov. = cluster.vcov(affectedJudgesControlsFEAgeThreewayOpposite, 
                                                                                         cluster = relevantVotes$filename))) %>% 
                                                        filter(term == "nationalRegularJudge:afterRussianRatification"),
                                                      tidy(coeftest(affectedJudgesControlsFEAgeThreeway, 
                                                                    vcov. = cluster.vcov(affectedJudgesControlsFEAgeThreeway, 
                                                                                         cluster = relevantVotes$filename))) %>% 
                                                        filter(term == "nationalRegularJudge:afterRussianRatification")) %>% 
  mutate(JudgeAge = c("Judges 65 years\nor older in 2010", "Judges younger than\n65 years in 2010"))


ggplot(affectedJudgesControlsFEAgeThreewayTerms, 
       aes(x = JudgeAge, 
           y = estimate, 
           ymin = estimate - 1.96 * std.error, 
           ymax = estimate + 1.96 * std.error)) +
  geom_errorbar(width = 0.1) +
  geom_point() +
  geom_hline(aes(yintercept = 0), 
             linetype = "dashed", 
             color = "grey")+
  theme_classic() +
  xlab("") +
  ylab("Judge nominated by respondent state * after Russian ratification")
ggsave("affectedJudgesControlsFEAgeThreewayTerms.pdf", height = 7, width = 7)


### Interaction with Chamber/Grand Chamber:
# Model 7: 
affectedJudgesControlsFEGrandChamber <- glm(proViolationVote ~ 
                                              nationalRegularJudge + afterRussianRatification + interactionTerm +
                                              GrandChamberJudgment +
                                              importance1 +
                                              importance2 +
                                              importance3 +
                                              article2case +
                                              article3case +
                                              article5case +
                                              article6case +
                                              article8case +
                                              article10case +
                                              article13case +
                                              article14case +
                                              protocol1_1case + 
                                              yearsSinceAppointment +
                                              age +
                                              lastname, 
                                            family = binomial(link = "logit"),
                                            data = filter(relevantVotes, GrandChamberJudgment == 1))
affectedJudgesControlsFEGrandChamber$ses  <- coeftest(affectedJudgesControlsFEGrandChamber, 
                                                      vcov. = cluster.vcov(affectedJudgesControlsFEGrandChamber, 
                                                                           cluster = filter(relevantVotes, GrandChamberJudgment == 1)$filename))[,2]
affectedJudgesControlsFEGrandChamber$ps  <- coeftest(affectedJudgesControlsFEGrandChamber, 
                                                     vcov. = cluster.vcov(affectedJudgesControlsFEGrandChamber, 
                                                                          cluster = filter(relevantVotes, GrandChamberJudgment == 1)$filename))[,4]

affectedJudgesControlsFEChamber <- glm(proViolationVote ~ 
                                         nationalRegularJudge + afterRussianRatification + interactionTerm +
                                         GrandChamberJudgment +
                                         importance1 +
                                         importance2 +
                                         importance3 +
                                         article2case +
                                         article3case +
                                         article5case +
                                         article6case +
                                         article8case +
                                         article10case +
                                         article13case +
                                         article14case +
                                         protocol1_1case + 
                                         yearsSinceAppointment +
                                         age +
                                         lastname, 
                                       family = binomial(link = "logit"),
                                       data = filter(relevantVotes, GrandChamberJudgment == 0))
affectedJudgesControlsFEChamber$ses  <- coeftest(affectedJudgesControlsFEChamber, 
                                                 vcov. = cluster.vcov(affectedJudgesControlsFEChamber, 
                                                                      cluster = filter(relevantVotes, GrandChamberJudgment == 0)$filename))[,2]
affectedJudgesControlsFEChamber$ps  <- coeftest(affectedJudgesControlsFEChamber, 
                                                vcov. = cluster.vcov(affectedJudgesControlsFEChamber, 
                                                                     cluster = filter(relevantVotes, GrandChamberJudgment == 0)$filename))[,4]

affectedJudgesControlsFEGrandChamberThreeWays <- glm(proViolationVote ~ 
                                                       nationalRegularJudge * 
                                                       afterRussianRatification *
                                                       GrandChamberJudgment +
                                                       importance1 +
                                                       importance2 +
                                                       importance3 +
                                                       article2case +
                                                       article3case +
                                                       article5case +
                                                       article6case +
                                                       article8case +
                                                       article10case +
                                                       article13case +
                                                       article14case +
                                                       protocol1_1case +
                                                       yearsSinceAppointment +
                                                       age +
                                                       lastname, 
                                                     family = binomial(link = "logit"),
                                                     data = relevantVotes)
affectedJudgesControlsFEGrandChamberThreeWays$ses  <- coeftest(affectedJudgesControlsFEGrandChamberThreeWays, 
                                                               vcov. = cluster.vcov(affectedJudgesControlsFEGrandChamberThreeWays, 
                                                                                    cluster = relevantVotes$filename))[,2]

affectedJudgesControlsFEGrandChamberThreeWays$ps  <- coeftest(affectedJudgesControlsFEGrandChamberThreeWays, 
                                                              vcov. = cluster.vcov(affectedJudgesControlsFEGrandChamberThreeWays, 
                                                                                   cluster = relevantVotes$filename))[,4]


affectedJudgesControlsFEGrandChamberThreeWaysOpposite <- glm(proViolationVote ~ 
                                                               nationalRegularJudge * 
                                                               afterRussianRatification *
                                                               I(GrandChamberJudgment == 0) +
                                                               importance1 +
                                                               importance2 +
                                                               importance3 +
                                                               article2case +
                                                               article3case +
                                                               article5case +
                                                               article6case +
                                                               article8case +
                                                               article10case +
                                                               article13case +
                                                               article14case +
                                                               protocol1_1case + 
                                                               yearsSinceAppointment +
                                                               age +
                                                               lastname, 
                                                             family = binomial(link = "logit"),
                                                             data = relevantVotes)

affectedJudgesControlsFEGrandChamberThreeWaysTerms <- bind_rows(tidy(coeftest(affectedJudgesControlsFEGrandChamberThreeWaysOpposite, 
                                                                              vcov. = cluster.vcov(affectedJudgesControlsFEGrandChamberThreeWaysOpposite, 
                                                                                                   cluster = relevantVotes$filename))) %>% 
                                                                  filter(term == "nationalRegularJudge:afterRussianRatification"),
                                                                tidy(coeftest(affectedJudgesControlsFEGrandChamberThreeWays, 
                                                                              vcov. = cluster.vcov(affectedJudgesControlsFEGrandChamberThreeWays, 
                                                                                                   cluster = relevantVotes$filename))) %>% 
                                                                  filter(term == "nationalRegularJudge:afterRussianRatification")) %>% 
  mutate(Chamber = c("Grand Chamber", "Chamber"))

ggplot(affectedJudgesControlsFEGrandChamberThreeWaysTerms, 
       aes(x = Chamber, 
           y = estimate, 
           ymin = estimate - 1.96 * std.error, 
           ymax = estimate + 1.96 * std.error)) +
  geom_errorbar(width = 0.1) +
  geom_point() +
  geom_hline(aes(yintercept = 0), 
             linetype = "dashed", 
             color = "grey")+
  xlab("")+
  ylab("Judge nominated by respondent state * after Russian ratification")+
  theme_classic()
ggsave("affectedJudgesControlsFEGrandChamberThreeWaysTerms.pdf", height = 7, width = 7)


#### Main coef plot and regression table ####
#### Coef Plot reported in main manuscript"
coefs <- bind_rows(
  na.omit(tidy(affectedJudgesControlsFE)) %>% 
    mutate(std.error = affectedJudgesControlsFE$ses,
           model = "Fixed effects + controls"),
  na.omit(tidy(affectedJudgesControlsSplitJudgmentsFE)) %>% 
    mutate(std.error = affectedJudgesControlsSplitJudgmentsFE$ses,
           model = "Split votes, fixed effects + controls"),
  na.omit(tidy(affectedJudgesControlsFEacademiaOrPrivatePractice)) %>% 
    mutate(std.error = affectedJudgesControlsFEacademiaOrPrivatePractice$ses,
           model = "Judges with background in academia or private practice, fixed effects + controls"),
  na.omit(tidy(affectedJudgesControlsFENotAcademiaOrPrivatePractice)) %>% 
    mutate(std.error = affectedJudgesControlsFENotAcademiaOrPrivatePractice$ses,
           model = "Judges with background in judiciary, politics, or as government agents, fixed effects + controls"),
  na.omit(tidy(affectedJudgesControlsFEYoung)) %>% 
    mutate(std.error = affectedJudgesControlsFEYoung$ses,
           model = "Judges younger than 65 in 2010, fixed effects + controls"),
  na.omit(tidy(affectedJudgesControlsFEOld)) %>% 
    mutate(std.error = affectedJudgesControlsFEOld$ses,
           model = "Judges 65 years or older in 2010, fixed effects + controls"),
  na.omit(tidy(affectedJudgesControlsFEGrandChamber)) %>% 
    mutate(std.error = affectedJudgesControlsFEGrandChamber$ses,
           model = "Grand Chamber votes, fixed effects + controls"),
  na.omit(tidy(affectedJudgesControlsFEChamber)) %>% 
    mutate(std.error = affectedJudgesControlsFEChamber$ses,
           model = "Chamber votes, fixed effects + controls")) %>% 
  filter(term == "nationalRegularJudge" |
           term == "afterRussianRatification" |
           term == "interactionTerm") %>%
  mutate(upper = estimate + std.error * -qnorm((1-0.95)/2), 
         lower = estimate -  std.error * -qnorm((1-0.95)/2),
         Model = model) %>% 
  mutate(
    term = factor(term,
                  levels = c(
                    "age",
                    "yearsSinceAppointment",
                    "protocol1_1case",
                    "article14case",
                    "article13case",
                    "article10case",
                    "article8case",
                    "article6case",
                    "article5case",
                    "article3case",
                    "article2case",
                    "importance3",
                    "importance2",
                    "importance1",
                    "GrandChamberJudgment",
                    "nationalRegularJudge", 
                    "afterRussianRatification",
                    "interactionTerm"),
                  labels =c("Age",
                            "Years since appointment",
                            "Right to private property case",
                            "Prohbition of discrimination case",
                            "Right to effective remedy case",
                            "Freedom of expression case",
                            "Right to privacy and family life case",
                            "Right to fair trial case",
                            "Right to liberty case",
                            "Prohibition of torture case",
                            "Right to life case",
                            "Importance level 3 case",
                            "Importance level 2 case",
                            "Importance level 1 case",
                            "Grand Chamber judgment",
                            "National judge",
                            "After Russian Ratification",
                            "National judge *\nAfter Russian Ratification") ),
    Model = factor(Model,
                   levels = c(#"Fixed effects", 
                     "Fixed effects + controls",
                     "Split votes, fixed effects + controls", 
                     #"2009-2011, fixed effects + controls",
                     "Judges with background in academia or private practice, fixed effects + controls",
                     "Judges with background in judiciary, politics, or as government agents, fixed effects + controls",
                     "Judges younger than 65 in 2010, fixed effects + controls",
                     "Judges 65 years or older in 2010, fixed effects + controls",
                     "Grand Chamber votes, fixed effects + controls",
                     "Chamber votes, fixed effects + controls"),
                   labels = c(#"Model 1: Votes by affected judges\nFixed effects", 
                     "Model 1: Votes by affected judges\nFixed effects",
                     "Model 2: Votes by affected judges\nSplit votes",
                     "Model 3: Judges with backgrounds in academia\nor private practice",
                     "Model 4: Judges with backgrounds in the judiciary,\nas government agents, or in politics",
                     # "Model 4: Votes by affected judges\n2009-2011",
                     "Model 5: Votes by judges younger\nthan 65 in 2010",
                     "Model 6: Votes by judges 65 years\nor older in 2010",
                     "Model 7: Grand Chamber votes",
                     "Model 8: Chamber votes")))


ggplot(coefs, aes(color = term, y = Model, shape = term))+
  geom_pointrangeh(aes(x = estimate, xmin = lower, xmax = upper),
                   position = position_dodgev(height = 1/5)) +
  geom_vline(xintercept = 0, colour = gray(1/2), lty = 2) +
  ylab("") +
  xlab("Coefficient") +
  theme_classic() +
  theme(legend.title = element_blank())
ggsave("fg1.pdf", width = 7, height = 5)
ggsave("fg1.tiff", width = 7, height = 5, dpi = 2000)
#### Making the main regression table ####
stargazer(affectedJudgesControlsFE,
          affectedJudgesControlsSplitJudgmentsFE,
          affectedJudgesControlsFEacademiaOrPrivatePractice,
          affectedJudgesControlsFENotAcademiaOrPrivatePractice,
          affectedJudgesControlsFEYoung,
          affectedJudgesControlsFEOld,
          affectedJudgesControlsFEGrandChamber,
          affectedJudgesControlsFEChamber,
          se = list(affectedJudgesControlsFE$ses,
                    affectedJudgesControlsSplitJudgmentsFE$ses,
                    affectedJudgesControlsFEacademiaOrPrivatePractice$ses,
                    affectedJudgesControlsFENotAcademiaOrPrivatePractice$ses,
                    affectedJudgesControlsFEYoung$ses,
                    affectedJudgesControlsFEOld$ses,
                    affectedJudgesControlsFEGrandChamber$ses,
                    affectedJudgesControlsFEChamber$ses),
          p = list(affectedJudgesControlsFE$ps,
                   affectedJudgesControlsSplitJudgmentsFE$ps,
                   affectedJudgesControlsFEacademiaOrPrivatePractice$ps,
                   affectedJudgesControlsFENotAcademiaOrPrivatePractice$ps,
                   affectedJudgesControlsFEYoung$ps,
                   affectedJudgesControlsFEOld$ps,
                   affectedJudgesControlsFEGrandChamber$ps,
                   affectedJudgesControlsFEChamber$ps),
          p.auto = FALSE,
          dep.var.labels = "Dependent variable: Pro-violation vote",
          dep.var.labels.include = TRUE,
          model.names = FALSE,
          model.numbers = TRUE,
          type = "latex",
          out = "LogisticRegressions.tex",
          label = "tab:logisticRegressions",
          font.size = "tiny",
          perl = FALSE,
          title = "Logistic regression models with judge fixed effects",
          covariate.labels = c("National judge",
                               "After Russian Ratification",
                               "National *\nAfter Russian Ratification",
                               "Grand Chamber judgment",
                               "Importance level 1 case",
                               "Importance level 2 case",
                               "Importance level 3 case",
                               "Right to life case",
                               "Prohibition of torture case",
                               "Right to liberty case",
                               "Right to fair trial case",
                               "Right to privacy and family life case",
                               "Freedom of expression case",
                               "Right to effective remedy case",
                               "Prohbition of discrimination case",
                               "Right to private property case",
                               "Years since appointment",
                               "Age",
                               "Intercept"),
          omit = "lastname",
          omit.labels = "Judge fixed effects",
          add.lines = list(c("Sample", 
                             "Votes for ",
                             "Split Judgment votes", 
                             "Votes for affected judges,", 
                             "Votes for affected judges,", 
                             "Votes for affected judges,", 
                             "Votes for affected judges,", 
                             "Grand Chamber votes", 
                             "Chamber votes"),
                           c("", 
                             "affected judges",
                             "for affected judges", 
                             "academic/private ",
                             "other",
                             "younger than 65 in 2010",
                             "65 or older in 2010", 
                             "for affected judges",
                             "for affected judges"),
                           c("", 
                             "",
                             "", 
                             "practice backgrounds",
                             "backgrounds",
                             "",
                             "", 
                             "",
                             "")),
          omit.stat = "bic",
          style = "ajps",
          omit.yes.no = c("Yes", "No"),
          column.sep.width = "-1pt")


### Making three-way interaction table ####
stargazer(affectedJudgesControlsAcademiaOrPrivatePracticeThreeway, 
          affectedJudgesControlsFEAgeThreeway, 
          affectedJudgesControlsFEGrandChamberThreeWays, 
          se = list(affectedJudgesControlsAcademiaOrPrivatePracticeThreeway$ses,
                    affectedJudgesControlsFEAgeThreeway$ses, 
                    affectedJudgesControlsFEGrandChamberThreeWays$ses), 
          p = list(affectedJudgesControlsAcademiaOrPrivatePracticeThreeway$ps, 
                   affectedJudgesControlsFEAgeThreeway$ps, 
                   affectedJudgesControlsFEGrandChamberThreeWays$ps),
          p.auto = FALSE, 
          dep.var.labels = "Pro-violation vote",
          dep.var.labels.include = TRUE,
          model.names = FALSE,
          model.numbers = FALSE,
          order = c("^nationalRegularJudge$",
                    "^afterRussianRatification$",
                    "^nationalRegularJudge:afterRussianRatification$",
                    "academiaOrPrivatePractice", 
                    "nationalRegularJudge:academiaOrPrivatePractice", 
                    "afterRussianRatification:academiaOrPrivatePractice", 
                    "nationalRegularJudge:afterRussianRatification:academiaOrPrivatePractice",
                    "^olderThan65in2010$",
                    "^nationalRegularJudge:olderThan65in2010$",
                    "^afterRussianRatification:olderThan65in2010$",
                    "^nationalRegularJudge:afterRussianRatification:olderThan65in2010$",
                    "^GrandChamberJudgment$",
                    "^nationalRegularJudge:GrandChamberJudgment$",
                    "^afterRussianRatification:GrandChamberJudgment$",
                    "^nationalRegularJudge:afterRussianRatification:GrandChamberJudgment$",
                    "importance1",
                    "importance2",
                    "importance3",
                    "article2case",
                    "article3case",
                    "article5case",
                    "article6case",
                    "article8case",
                    "article10case",
                    "article13case",
                    "article14case",
                    "protocol1_1case",
                    "yearsSinceAppointment",
                    "age",
                    "Constant"),
          covariate.labels = c("Judge nominated by respondent state",
                               "After Russian Ratification",
                               "Judge nominated by respondent state * After Russian Ratification",
                               "Academia or private practice",
                               "Judge nominated by respondent state * Academia or private practice",
                               "After Russian Ratification* Academia or private practice",
                               "Judge nominated by respondent state * After Russian Ratification * Academia or private practice",
                               "Judge 65 years or older in 2010",
                               "Judge nominated by respondent state * Judge 65 years or older in 2010",
                               "After Russian Ratification* Judge 65 years or older in 2010",
                               "Judge nominated by respondent state * After Russian Ratification * Judge 65 years or older in 2010",
                               "Grand Chamber judgment",
                               "Judge nominated by respondent state * Grand Chamber judgment",
                               "After Russian Ratification * Grand Chamber judgment",
                               "Judge nominated by respondent state * After Russian Ratification * Grand Chamber judgment",
                               "Importance level 1 case",
                               "Importance level 2 case",
                               "Importance level 3 case",
                               "Right to life case",
                               "Prohibition of torture case",
                               "Right to liberty case",
                               "Right to fair trial case",
                               "Right to privacy and family life case",
                               "Freedom of expression case",
                               "Right to effective remedy case",
                               "Prohbition of discrimination case",
                               "Right to private property case",
                               "Years since appointment",
                               "Age",
                               "Intercept"),
          omit = "lastname",
          omit.stat = "bic",
          omit.labels = "Judge fixed effects",
          font.size = "tiny",
          title = "Three-way interaction models", 
          label = "tab:threeway",
          out = "ThreeWayInteractions.tex")



affectedJudgesControls2008to2012FE <- glm(proViolationVote ~ 
                                            nationalRegularJudge + afterRussianRatification + interactionTerm +
                                            GrandChamberJudgment +
                                            importance1 +
                                            importance2 +
                                            importance3 +
                                            article2case +
                                            article3case +
                                            article5case +
                                            article6case +
                                            article8case +
                                            article10case +
                                            article13case +
                                            article14case +
                                            protocol1_1case + 
                                            yearsSinceAppointment +
                                            age +
                                            lastname, 
                                          family = binomial(link = "logit"),
                                          data = filter(relevantVotes, 
                                                        judgmentYear > 2007, 
                                                        judgmentYear < 2013))

affectedJudgesControls2008to2012FE$ses <- coeftest(affectedJudgesControls2008to2012FE, 
                                                   vcov. = cluster.vcov(affectedJudgesControls2008to2012FE, 
                                                                        cluster = filter(relevantVotes, 
                                                                                         judgmentYear > 2007, 
                                                                                         judgmentYear < 2013)$filename))[,2]

affectedJudgesControls2008to2012FE$ps <- coeftest(affectedJudgesControls2008to2012FE, 
                                                  vcov. = cluster.vcov(affectedJudgesControls2008to2012FE, 
                                                                       cluster = filter(relevantVotes, 
                                                                                        judgmentYear > 2007, 
                                                                                        judgmentYear < 2013)$filename))[,4]




affectedJudgesControls2009to2011FE <- glm(proViolationVote ~ 
                                            nationalRegularJudge + afterRussianRatification + interactionTerm +
                                            GrandChamberJudgment +
                                            importance1 +
                                            importance2 +
                                            importance3 +
                                            article2case +
                                            article3case +
                                            article5case +
                                            article6case +
                                            article8case +
                                            article10case +
                                            article13case +
                                            article14case +
                                            protocol1_1case + 
                                            yearsSinceAppointment +
                                            age +
                                            lastname, 
                                          family = binomial(link = "logit"),
                                          data = filter(relevantVotes, 
                                                        judgmentYear > 2008, 
                                                        judgmentYear < 2012))

affectedJudgesControls2009to2011FE$ses <- coeftest(affectedJudgesControls2009to2011FE, 
                                                   vcov. = cluster.vcov(affectedJudgesControls2009to2011FE, 
                                                                        cluster = filter(relevantVotes, 
                                                                                         judgmentYear > 2008, 
                                                                                         judgmentYear < 2012)$filename))[,2]

affectedJudgesControls2009to2011FE$ps <- coeftest(affectedJudgesControls2009to2011FE, 
                                                  vcov. = cluster.vcov(affectedJudgesControls2009to2011FE, 
                                                                       cluster = filter(relevantVotes, 
                                                                                        judgmentYear > 2008, 
                                                                                        judgmentYear < 2012)$filename))[,4]



stargazer(affectedJudgesControls2008to2012FE,
          affectedJudgesControls2009to2011FE,
          se = list(affectedJudgesControls2008to2012FE$ses,
                    affectedJudgesControls2008to2012FE$ses),
          p = list(affectedJudgesControls2008to2012FE$ps,
                   affectedJudgesControls2009to2011FE$ps),
          p.auto = FALSE,
          dep.var.labels = "Dependent variable: Pro-violation vote",
          dep.var.labels.include = TRUE,
          model.names = FALSE,
          model.numbers = FALSE,
          type = "latex",
          out = "LogisticRegressionsProximity.tex",
          label = "tab:proximity",
          omit.stat = "bic",
          style = "ajps",
          omit = c("lastname"),
          omit.labels = c("Judge fixed effects"),
          covariate.labels = c("Judge nominated by respondent state",
                               "After Russian Ratification of Protocol 14",
                               "Judge nominated by respondent state * After Russian Ratification",
                               "Grand Chamber judgment",
                               "Importance level 1 case",
                               "Importance level 2 case",
                               "Importance level 3 case",
                               "Right to life case",
                               "Prohibition of torture case",
                               "Right to liberty case",
                               "Right to fair trial case",
                               "Right to privacy and family life case",
                               "Freedom of expression case",
                               "Right to effective remedy case",
                               "Prohbition of discrimination case",
                               "Right to private property case",
                               "Years since appointment",
                               "Age",
                               "Intercept"),
          add.lines = list(c("Sample", 
                             "Votes for relevant judges",
                             "Votes for relevant judges"),
                           c("", 
                             "2008-2012",
                             "2009-2011")),
          title = "Fixed effects logistic regression models: judgments rendered in close proximity to the Russian ratification of Protocol 14",
          font.size = "scriptsize",
          omit.yes.no = c("Yes", "No"))







##### Parallel lines tests ######
relevantVotes <- relevantVotes %>% 
  mutate(Placebo2009 = ifelse(as.Date(Judgment.Date, format = "%Y-%m-%d") > as.Date("2009-02-18", format = "%Y-%m-%d"),1,0),
         Placebo2008 = ifelse(as.Date(Judgment.Date, format = "%Y-%m-%d") > as.Date("2008-02-18", format = "%Y-%m-%d"),1,0),
         Placebo2007 = ifelse(as.Date(Judgment.Date, format = "%Y-%m-%d") > as.Date("2007-02-18", format = "%Y-%m-%d"),1,0),
         Placebo2006 = ifelse(as.Date(Judgment.Date, format = "%Y-%m-%d") > as.Date("2006-02-18", format = "%Y-%m-%d"),1,0),
         Placebo2005 = ifelse(as.Date(Judgment.Date, format = "%Y-%m-%d") > as.Date("2005-02-18", format = "%Y-%m-%d"),1,0),
         Placebo2004 = ifelse(as.Date(Judgment.Date, format = "%Y-%m-%d") > as.Date("2004-02-18", format = "%Y-%m-%d"),1,0),
         Placebo2003 = ifelse(as.Date(Judgment.Date, format = "%Y-%m-%d") > as.Date("2003-02-18", format = "%Y-%m-%d"),1,0),
         AfterAdoptionOfProtocol14 = ifelse(as.Date(Judgment.Date, format = "%Y-%m-%d") > as.Date("2004-05-13", format = "%Y-%m-%d"),1,0))

withLeads <- glm(proViolationVote ~ 
                   nationalRegularJudge + 
                   afterRussianRatification * nationalRegularJudge +
                   Placebo2009 * nationalRegularJudge +
                   Placebo2008 * nationalRegularJudge +
                   Placebo2007 * nationalRegularJudge +
                   Placebo2006 * nationalRegularJudge +
                   Placebo2005 * nationalRegularJudge +
                   Placebo2004 * nationalRegularJudge +
                   GrandChamberJudgment +
                   importance1 +
                   importance2 +
                   importance3 +
                   article2case +
                   article3case +
                   article5case +
                   article6case +
                   article8case +
                   article10case +
                   article13case +
                   article14case +
                   protocol1_1case + 
                   yearsSinceAppointment +
                   age +
                   lastname, 
                 family = binomial(link = "logit"),
                 data = relevantVotes, 
                 x = TRUE)
withLeads$SEs <-  coeftest(withLeads, 
                           vcov. = cluster.vcov(withLeads, 
                                                cluster = relevantVotes$filename))[,2]

withLeads$ps <-  coeftest(withLeads, 
                          vcov. = cluster.vcov(withLeads, 
                                               cluster = relevantVotes$filename))[,4]

na.omit(tidy(withLeads)) %>% 
  mutate(std.error = withLeads$SEs, 
         upper = estimate + 1.96 * std.error,
         lower = estimate - 1.96 * std.error) %>% 
  filter(grepl("nationalRegularJudge:", term))%>% 
  mutate(term = ifelse(term == "nationalRegularJudge:afterRussianRatification", 
                       "National judge *\nafter Russian\nratification", term),
         term = ifelse(term == "nationalRegularJudge:Placebo2009", 
                       "National judge *\none year lead of reform", term),
         term = ifelse(term == "nationalRegularJudge:Placebo2008", 
                       "National judge *\ntwo years lead of reform", term),
         term = ifelse(term == "nationalRegularJudge:Placebo2007", 
                       "National judge *\nthree years lead of reform", term),
         term = ifelse(term == "nationalRegularJudge:Placebo2006", 
                       "National judge *\nfour years lead of reform", term),
         term = ifelse(term == "nationalRegularJudge:Placebo2005", 
                       "National judge *\nfive years lead of reform", term),
         term = ifelse(term == "nationalRegularJudge:Placebo2004", 
                       "National judge *\nsix years lead of reform", term),
         term = factor(term, levels = c("National judge *\nsix years lead of reform",
                                        "National judge *\nfive years lead of reform",
                                        "National judge *\nfour years lead of reform",
                                        "National judge *\nthree years lead of reform",
                                        "National judge *\ntwo years lead of reform",
                                        "National judge *\none year lead of reform",
                                        "National judge *\nafter Russian\nratification")), 
         group = ifelse(term == "National judge *\nafter Russian\nratification", "treatment", "leads"))%>% 
  ggplot(aes(x = term, y = estimate, ymin = lower, ymax = upper, color = group)) +
  geom_errorbar(width = 0.1) +
  geom_hline(aes(yintercept = 0), linetype ="dashed") + 
  geom_point()+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45),
        axis.line.x = element_blank(), 
        legend.position = "none")+ 
  ylim(-0.8,0.6)+
  ylab("Coefficient")+
  xlab("")+
  theme(text = element_text(size=15)) 
ggsave("fg4.pdf", height = 7, width = 7, dpi = 1200)
ggsave("fg4.tiff", width = 7, height = 7, dpi = 2000)

effectOfAdoption <-  glm(proViolationVote ~ 
                           nationalRegularJudge + 
                           AfterAdoptionOfProtocol14 * nationalRegularJudge +
                           GrandChamberJudgment +
                           importance1 +
                           importance2 +
                           importance3 +
                           article2case +
                           article3case +
                           article5case +
                           article6case +
                           article8case +
                           article10case +
                           article13case +
                           article14case +
                           protocol1_1case + 
                           yearsSinceAppointment +
                           age +
                           lastname, 
                         family = binomial(link = "logit"),
                         data = relevantVotes, 
                         x = TRUE)

effectOfAdoption$SEs <- coeftest(effectOfAdoption, 
                                 vcov. = cluster.vcov(effectOfAdoption, 
                                                      cluster = relevantVotes$filename))[,2]

effectOfAdoption$ps <- coeftest(effectOfAdoption, 
                                vcov. = cluster.vcov(effectOfAdoption, 
                                                     cluster = relevantVotes$filename))[,2]

effectOfAdoptionAndRatification <-  glm(proViolationVote ~ 
                                          nationalRegularJudge + 
                                          afterRussianRatification * nationalRegularJudge +
                                          AfterAdoptionOfProtocol14 * nationalRegularJudge +
                                          GrandChamberJudgment +
                                          importance1 +
                                          importance2 +
                                          importance3 +
                                          article2case +
                                          article3case +
                                          article5case +
                                          article6case +
                                          article8case +
                                          article10case +
                                          article13case +
                                          article14case +
                                          protocol1_1case + 
                                          yearsSinceAppointment +
                                          age +
                                          lastname, 
                                        family = binomial(link = "logit"),
                                        data = relevantVotes, 
                                        x = TRUE)

effectOfAdoptionAndRatification$SEs <- coeftest(effectOfAdoptionAndRatification, 
                                                vcov. = cluster.vcov(effectOfAdoptionAndRatification, 
                                                                     cluster = relevantVotes$filename))[,2]


effectOfAdoptionAndRatification$ps <- coeftest(effectOfAdoptionAndRatification, 
                                               vcov. = cluster.vcov(effectOfAdoptionAndRatification, 
                                                                    cluster = relevantVotes$filename))[,4]


stargazer(withLeads,
          effectOfAdoption, 
          effectOfAdoptionAndRatification, 
          se = list(withLeads$SEs, 
                    effectOfAdoption$SEs, 
                    effectOfAdoptionAndRatification$ses), 
          p = list(withLeads$ps, 
                   effectOfAdoption$ps, 
                   effectOfAdoptionAndRatification$ps),
          p.auto = FALSE, 
          dep.var.labels = "Pro-violation vote",
          dep.var.labels.include = TRUE,
          model.names = FALSE,
          single.row = TRUE, 
          model.numbers = FALSE,
          order = c("^nationalRegularJudge$",
                    "^afterRussianRatification$",
                    "^nationalRegularJudge:afterRussianRatification$",
                    "Placebo2009", 
                    "Placebo2008",
                    "Placebo2007",
                    "Placebo2006",
                    "Placebo2005",
                    "Placebo2004", 
                    "nationalRegularJudge:Placebo2009", 
                    "nationalRegularJudge:Placebo2008", 
                    "nationalRegularJudge:Placebo2007", 
                    "nationalRegularJudge:Placebo2006", 
                    "nationalRegularJudge:Placebo2005", 
                    "nationalRegularJudge:Placebo2004", 
                    "AfterAdoptionOfProtocol14", 
                    "nationalRegularJudge:AfterAdoptionOfProtocol14",
                    "^GrandChamberJudgment$",
                    "importance1",
                    "importance2",
                    "importance3",
                    "article2case",
                    "article3case",
                    "article5case",
                    "article6case",
                    "article8case",
                    "article10case",
                    "article13case",
                    "article14case",
                    "protocol1_1case",
                    "yearsSinceAppointment",
                    "age",
                    "Constant"),
          covariate.labels = c("Judge nominated by respondent state",
                               "After Russian Ratification",
                               "Judge nominated by respondent state * After Russian Ratification",
                               "One year lead", 
                               "Two year lead",
                               "Three year lead",
                               "Four year lead",
                               "Five year lead",
                               "Six year lead", 
                               "Judge nominated by respondent state * One year lead", 
                               "Judge nominated by respondent state * Two year lead", 
                               "Judge nominated by respondent state * Three year lead", 
                               "Judge nominated by respondent state * Four year lead", 
                               "Judge nominated by respondent state * Five year lead", 
                               "Judge nominated by respondent state * Six year lead", 
                               "After Adoption of Protocol 14 (in 2004)", 
                               "Judge nominated by respondent state * After Adoption of Protocol 14 (in 2004)",
                               "Grand Chamber judgment",
                               "Importance level 1 case",
                               "Importance level 2 case",
                               "Importance level 3 case",
                               "Right to life case",
                               "Prohibition of torture case",
                               "Right to liberty case",
                               "Right to fair trial case",
                               "Right to privacy and family life case",
                               "Freedom of expression case",
                               "Right to effective remedy case",
                               "Prohbition of discrimination case",
                               "Right to private property case",
                               "Years since appointment",
                               "Age",
                               "Intercept"),
          omit = "lastname",
          omit.stat = "bic",
          omit.labels = "Judge fixed effects",
          font.size = "tiny",
          title = "Logistic regression models with judge fixed effects: Assessing anticipatory effects of Protocol 14", 
          label = "tab:anticipation",
          out = "anticipationModels.tex")

protocol14bisModel <- glm(proViolationVote ~ 
                            nationalRegularJudge * afterRussianRatification + 
                            nationalRegularJudge * Protocol14bisInForce + 
                            GrandChamberJudgment +
                            importance1 +
                            importance2 +
                            importance3 +
                            article2case +
                            article3case +
                            article5case +
                            article6case +
                            article8case +
                            article10case +
                            article13case +
                            article14case +
                            protocol1_1case + 
                            yearsSinceAppointment +
                            age +
                            lastname, 
                          family = binomial(link = "logit"),
                          data = relevantVotes, 
                          x = TRUE)
protocol14bisModel$SEs <-  coeftest(protocol14bisModel, vcov. = cluster.vcov(protocol14bisModel, 
                                                                             cluster = relevantVotes$filename))[,2]
protocol14bisModel$ps <-  coeftest(protocol14bisModel, vcov. = cluster.vcov(protocol14bisModel, 
                                                                            cluster = relevantVotes$filename))[,4]

na.omit(tidy(protocol14bisModel)) %>% 
  mutate(std.error = protocol14bisModel$SEs, 
         upper = estimate +  std.error* -qnorm((1-0.95)/2) , 
         lower = estimate - std.error * -qnorm((1-0.95)/2)) %>% 
  filter(term == "nationalRegularJudge" | 
           term == "afterRussianRatification" |
           term == "Protocol14bisInForce" |
           term == "nationalRegularJudge:afterRussianRatification" |
           term == "nationalRegularJudge:Protocol14bisInForce") %>% 
  mutate(term = ifelse(term == "nationalRegularJudge", 
                       "National judge", term), 
         term = ifelse(term == "afterRussianRatification", 
                       "After Russian ratification", term), 
         term = ifelse(term == "nationalRegularJudge:afterRussianRatification", 
                       "National judge *\nAfter Russian ratification", term), 
         term = ifelse(term == "Protocol14bisInForce", 
                       "After protocol 14bis entered into force", term),
         term = ifelse(term =="nationalRegularJudge:Protocol14bisInForce",
                       "National judge *\nAfter protocol 14bis entered into force", term),
         term = factor(term, levels = c("National judge *\nAfter Russian ratification",
                                        "After Russian ratification", 
                                        "National judge *\nAfter protocol 14bis entered into force", 
                                        "After protocol 14bis entered into force", 
                                        "National judge"))) %>% 
  ggplot(aes(x = estimate, y = term, xmin = lower, xmax = upper)) +
  geom_errorbarh(height = 0.1) +
  geom_point()+
  theme_classic()+
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "darkgrey") +
  ylab("") +
  xlab("Coefficient")+
  theme(text = element_text(size=20)) 
ggsave(file = "fg3.pdf", 
       height = 7, 
       width = 7)
ggsave("fg3.tiff", width = 7, height = 7, dpi = 2000)

stargazer(protocol14bisModel, 
          se = list(protocol14bisModel$SEs), 
          p = list(protocol14bisModel$ps),
          p.auto = FALSE, 
          dep.var.labels = "Pro-violation vote",
          dep.var.labels.include = TRUE,
          model.names = FALSE,
          model.numbers = FALSE,
           order = c("^nationalRegularJudge$",
          "Protocol14bisInForce", 
          "nationalRegularJudge:Protocol14bisInForce", 
                    "^afterRussianRatification$",
                    "^nationalRegularJudge:afterRussianRatification$",
                    "^GrandChamberJudgment$",
                    "^nationalRegularJudge:GrandChamberJudgment$",
                    "^afterRussianRatification:GrandChamberJudgment$",
                    "^nationalRegularJudge:afterRussianRatification:GrandChamberJudgment$",
                    "importance1",
                    "importance2",
                    "importance3",
                    "article2case",
                    "article3case",
                    "article5case",
                    "article6case",
                    "article8case",
                    "article10case",
                    "article13case",
                    "article14case",
                    "protocol1_1case",
                    "yearsSinceAppointment",
                    "age",
                    "Constant"),
          covariate.labels = c("Judge nominated by respondent state",
                               "Protocol 14bis operational", 
                               "Judge nominated by respondent state * Protocol 14bis operational", 
                               "After Russian Ratification",
                               "Judge nominated by respondent state * After Russian Ratification",
                               "Grand Chamber judgment",
                               "Importance level 1 case",
                               "Importance level 2 case",
                               "Importance level 3 case",
                               "Right to life case",
                               "Prohibition of torture case",
                               "Right to liberty case",
                               "Right to fair trial case",
                               "Right to privacy and family life case",
                               "Freedom of expression case",
                               "Right to effective remedy case",
                               "Prohbition of discrimination case",
                               "Right to private property case",
                               "Years since appointment",
                               "Age",
                               "Intercept"),
          omit = "lastname",
          omit.stat = "bic",
          omit.labels = "Judge fixed effects",
          font.size = "tiny",
          title = "Logistic regression model with judge fixed effects: Changes in tendency to favor nominating state associated with Protocol14bis becoming operational and Russian ratification of Protocol 14", 
          label = "tab:Protocol14bis",
          out = "Protocol14bis.tex")








