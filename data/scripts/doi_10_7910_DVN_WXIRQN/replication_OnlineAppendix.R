# only need to load these if you didn't start 
# with "replication_conditional_accept.R"
# will check automatically, and if not
# load the appropriate libraries/data
`%!in%` <- Negate(`%in%`)
if("openEnded" %!in% (.packages())){
  library(openEnded); library(xtable)
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
  # can load data from package with data("kaneData")
  # create jaccard and cosine similarity measures
  kaneData <- similarityMeasures(dataframe=kaneData,
                                 n_gram_measures_to_calculate=c("jaccard", "cosine", "jw", "dl"),
                                 prompt="textViewed", 
                                 response="openResponse", 
                                 ngrams=3)
  # indicate regression formula
  kane_formula <- SelectTrump ~ Conditions*ideology_factor
}

##########
# Fig SM.2
##########

# generate cosine similarity of word embeddings 
kaneData <- createWordEmbeddings(dataframe=kaneData, 
                                 responses="openResponse", 
                                 prompts="textViewed", 
                                 user_seed=5,
                                 prune_len=2,  
                                 language="en")

# plot correlation between similarity measures
pdf("../figures/FigSM2.pdf")
plotSimilarityCorr(dataframe=kaneData, 
                   measures=c("jaccardSimilarity", 
                              "cosineSimilarity", 
                              "embedding_score", 
                              "correct",
                              "jwSimilarity", 
                              "dlSimilarity"),
                   labels=c("Jaccard (n-gram)", 
                            "Cosine (n-gram)", 
                            "Cosine (Word Embeddings)", 
                            "'Correct' Answer", 
                            "Jaro (n-gram)",
                            "Damerau-Levenshtein (n-gram)"))
dev.off()

##########
# Fig SM.4
##########

complierATE(dataframe=kaneData,
            formula=kane_formula,
            plot_treatment="Conditions",
            plot_interact_x="ideology_factor",
            similarity_measures=c("cosineSimilarity", "jaccardSimilarity"),
            bounds=c(0, 0.1), 
            n=100, 
            user_seed=5, 
            model_type="logit", 
            k=3,
            display_plot=T,
            plot_path="../figures/FigSM4.pdf",
            stable_x=T, 
            k_plot=T)

############
# Table SM.1
############

kaneData <- averageSimilarity(kaneData, similarity_measures = c("jaccardSimilarity", "cosineSimilarity"), k=3)
tableA1 <- kaneData %>% group_by(Conditions, ideology_factor) %>% summarise_at(vars(avgSimilarity, SelectTrump), list(mean))
tableA1 <- cbind(as.data.frame(tableA1), aggregate(avgSimilarity ~ ideology_factor + Conditions, data = kaneData, FUN = length)[,3])
names(tableA1) <- c("Conditions", "Party", "Mean Average Similarity", "Mean Selection of Trump Story", "N")
print(xtable(tableA1, type = "latex"), file = "../tables/tabSM1.tex")

############
# Table SM.2
############

print(xtable(prop.table(table(kaneData$codeCorrect, kaneData$Conditions), 2), digits=3, type = "latex"), file = "../tables/tabSM2.tex")

############
# Table SM.3
############

print(texreg(list(glm(SelectTrump ~ Conditions*ideology_factor, data=kaneData, family = "binomial")), type = "latex"), file = "../tables/tabSM3.tex")

############
# Table SM.4
############

sink(file = "../tables/tabSM4.tex")
regressionComparison(dataframe=kaneData, 
                     formula=kane_formula, 
                     plot_treatment="Conditions",
                     plot_interact_x="ideology_factor",
                     similarity_measures=c("cosineSimilarity", "jaccardSimilarity"),
                     k=3, 
                     model_type="logit", 
                     up_down_weight="down",
                     user_seed=5,  
                     n_sims=10000, 
                     correct_vec="correct", 
                     display_plot=F,
                     plot_path=NULL, 
                     print_regs=T
)
sink()

##########
# Fig SM.6 
##########

compareK(dataframe=kaneData, 
         similarity_measures =c("jaccardSimilarity", "cosineSimilarity"), 
         correct_vec = "correct", 
         k_range=1:10,
         plot_path="../figures/FigSM6.pdf"
         )

###########
# Table SM.5
###########

sink(file = "../tables/tabSM5.tex")
predictAttention(dataframe = kaneData,
                 attention_formula = "age_factor + educ_factor + race_factor + income + gender_factor + ideology_factor*Conditions", 
                 similarity_measures=c("jaccardSimilarity", "cosineSimilarity"),
                 correct_vec="correct",
                 model_type="logit")
sink()

############
# Table SM.6
############

sink(file = "../tables/tabSM6.tex")
IVcomparison(dataframe = kaneData,
             formula = kane_formula, 
             similarity_measures = c("cosineSimilarity", "jaccardSimilarity"))
sink()

##############################
# SM.6 Additional Application 
##############################

# can load data from package with data("zieglerData")
# create jaccard and cosine similarity measures
zieglerData <- similarityMeasures(dataframe=zieglerData,
                               n_gram_measures_to_calculate=c("jaccard", 
                                                    "cosine", 
                                                    "jw", "dl"),
                               prompt="textViewed", 
                               response="validityCheck", 
                               ngrams=3)

############
# Fig SM.8
############

plotSimilarity(dataframe=zieglerData[which(zieglerData$Country=="Brazil"),],
               measure="jaccardSimilarity", 
               plot_path="../figures/FigSM8a.pdf"
) 
plotSimilarity(dataframe=zieglerData[which(zieglerData$Country=="Mexico"), ],
               measure="jaccardSimilarity", 
               plot_path="../figures/FigSM8b.pdf"
) 

############
# Fig SM.9
############

plotSimilarity(dataframe=zieglerData[which(zieglerData$Country=="Brazil"), ],
               measure="cosineSimilarity", 
               plot_path="../figures/FigSM9a.pdf"
) 
plotSimilarity(dataframe=zieglerData[which(zieglerData$Country=="Mexico"), ],
               measure="cosineSimilarity", 
               plot_path="../figures/FigSM9b.pdf"
) 

############
# Fig SM.10
############

# plot correlation between similarity measures
pdf("../figures/FigSM10.pdf", width=9, height=9)
plotSimilarityCorr(dataframe=zieglerData, 
                   measures=c("jaccardSimilarity", 
                              "cosineSimilarity", 
                              "jwSimilarity", 
                              "dlSimilarity"),
                   labels=c("Jaccard (n-gram)", 
                            "Cosine (n-gram)", 
                            "Jaro (n-gram)",
                            "Damerau-Levenshtein (n-gram)"))
dev.off()

############
# Table SM.8
############

# execute regressions to compare overall results
# the function in the package only does one regression at a time
# so we'll put all of our model formulas in a list first
trust_formula <- trustChurch_postTreat ~ Concordant*attendanceBin
model_formulas <- list(trust_formula,
                       update(trust_formula, responsiveness_postTreat ~ .),
                       update(trust_formula, futureVolunteer_postTreat ~ .),
                       update(trust_formula, futureAttendance_postTreat ~ .),
                       update(trust_formula, petitionInterest_postTreat ~ . ))

# and then we can run over our list and get the data output 
# for each regression model, which we'll store in "plotSM12_data"
plotSM11_data <- NULL
for(model_form in 1:length(model_formulas)){
  temp_data <- data.frame(outcome=as.character(model_formulas[[model_form]][2]), 
                          regressionComparison(dataframe=zieglerData, 
                                               formula=model_formulas[[model_form]], 
                                               plot_treatment="Concordant",
                                               plot_interact_x="attendanceBin",
                                               similarity_measures=c("cosineSimilarity", 
                                                                     "jaccardSimilarity"),
                                               k=3, 
                                               model_type="ols", 
                                               up_down_weight="down",
                                               user_seed=5,  
                                               n_sims=10000, 
                                               correct_vec=NULL, 
                                               display_plot=F,
                                               print_regs=F,
                                               return_data=T)
  )
  plotSM11_data <- rbind(plotSM11_data, temp_data)
}

# now that we have our regression results, we can create Table SM.8
print(texreg(list(baseModel_trustChurch_postTreat, 
            weightedModel_trustChurch_postTreat, 
            listwiseModel_trustChurch_postTreat,
            baseModel_responsiveness_postTreat,
            weightedModel_responsiveness_postTreat,
            listwiseModel_responsiveness_postTreat,
            baseModel_futureVolunteer_postTreat, 
            weightedModel_futureVolunteer_postTreat,
            listwiseModel_futureVolunteer_postTreat,
            baseModel_futureAttendance_postTreat,
            weightedModel_futureAttendance_postTreat,
            listwiseModel_futureAttendance_postTreat,
            baseModel_petitionInterest_postTreat,
            weightedModel_petitionInterest_postTreat,
            listwiseModel_petitionInterest_postTreat), 
            digits = 3, type = "latex"), file = "../tables/tabSM8.tex")

############
# Fig SM.11
############

# next, we'll create, which we have to do this way since 
# we have multiple regression models
plotSM11_data$outcome <- as.factor(plotSM11_data$outcome)
plotSM11_data$outcome <- factor(plotSM11_data$outcome, levels=rev(levels(plotSM11_data$outcome)))
levels(plotSM11_data$outcome) <- c("Trust", "Responsiveness", "Petition", "Volunteerism", "Attendance")
# if you don't want to save the figure, comment below
pdf("../figures/FigSM11.pdf", width=11)
ggplot(plotSM11_data) +
  theme_pubr() +
  geom_pointrange(aes(x=interact_x, y = first_diffs, 
                      ymin = lower_CI, ymax = upper_CI, 
                      colour=column_label, shape=column_label),
                  position = position_dodge(width =.75), size=1.5)+
  scale_shape_manual(values = c(17,18,19, 16))+
  geom_hline(aes(yintercept= 0), linetype="dashed", size=.5, colour="black") +
  facet_wrap(~outcome, ncol=3) + 
  scale_colour_grey(start = 0, end = 0.7)+
  theme(axis.title=element_text(size=20), axis.text = element_text(size=18), 
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.text=element_text(size=18), legend.background = element_blank(),
        strip.text = element_text(size=20), strip.background = element_rect(fill = NA, color = "black"),
        legend.position="bottom", legend.title = element_text(size=20),
        legend.box.background = element_rect(colour = "black"), 
        panel.border = element_blank(),
        panel.background = element_rect(fill = NA, color = "black"),
        panel.grid = element_blank()
  ) +
  geom_vline(xintercept = c(1.5,2.5)) +
  labs(y='\nMarginal Effect of Treatment\n', 
       x='\nTreatment\n', 
       colour="Sample\n(Model):", 
       shape="Sample\n(Model):")
dev.off()

############
# Fig SM.12
############

plotSM12_data <- NULL
for(model_form in 1:length(model_formulas)){
  temp_data <- data.frame(outcome=as.character(model_formulas[[model_form]][2]), 
                          complierATE(dataframe=zieglerData,
                                      formula=model_formulas[[model_form]],
                                      plot_treatment="Concordant",
                                      plot_interact_x="attendanceBin",
                                      similarity_measures=c("cosineSimilarity", "jaccardSimilarity"),
                                      bounds=c(0, 0.2), 
                                      n=100, 
                                      user_seed=5, 
                                      model_type="ols", 
                                      k=3,
                                      display_plot=F,
                                      stable_x=T, 
                                      k_plot=F,
                                      return_data=T)
  )
  plotSM12_data <- rbind(plotSM12_data, temp_data)
}

plotSM12_data$outcome <- as.factor(plotSM12_data$outcome)
plotSM12_data$outcome <- factor(plotSM12_data$outcome, levels=rev(levels(plotSM12_data$outcome)))
levels(plotSM12_data$outcome) <- c("Trust", "Responsiveness", "Petition", "Volunteerism", "Attendance")
# if you don't want to save the figure, comment below
pdf("../figures/FigSM12.pdf")
ggplot(plotSM12_data, aes(x=first_diffs, y=as.factor(treat_from_to), colour=subset, fill=subset)) +
    theme_pubr() +
    geom_vline(aes(xintercept=0), linetype="dashed", size=.5, colour="black") +
    geom_density_ridges(quantile_lines = F,  alpha=.75, scale=.9) +
    facet_wrap(~outcome + interact_x, ncol=3, scales="fixed") + 
    scale_colour_grey(start = 0.2, end = 0.7)+
    scale_fill_grey(start = 0.2, end = 0.7)+
    lims(x=c(min(plotSM12_data$first_diffs)-0.1, max(plotSM12_data$first_diffs)+0.1))+
    theme(axis.title=element_text(size=12), axis.text = element_text(size=10), legend.text=element_text(size=10),
          strip.text = element_text(size=8), strip.background = element_rect(fill = NA, color = "black"),
          legend.position="bottom", legend.title = element_text(size=15), legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black"), axis.text.x = element_text(angle = 45, hjust = 1),
          panel.border = element_blank(), 
          panel.background = element_rect(fill = NA, color = "black"),
          panel.grid = element_blank()
    ) + 
    geom_vline(xintercept = c(1.5,2.5)) +
    labs(x='\nMarginal Effect of Treatment\n', y='\nTreatment Condition\n', colour="Sample:", fill="Sample:"
    )
dev.off()
