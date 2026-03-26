rm(list = ls())                               #Clean the Global Environment
cat ("\014")                                  #Clean the R console
if (is.null(dev.list()) == FALSE) dev.off()   #Clean Plots

#STEP 1: Load libraries and functions------
#Load libraries (and install those which are missing):
if (!require("compute.es")) install.packages("compute.es"); library(compute.es)
if (!require("readxl")) install.packages("readxl"); library(readxl)
if (!require("descr")) install.packages("descr"); library(descr)
if (!require("grid")) install.packages("grid"); library(grid)
if (!require("meta")) install.packages("meta"); library(meta)
if (!require("metafor")) install.packages("metafor"); library(metafor)
if (!require("dplyr")) install.packages("dplyr"); library(dplyr)
if (!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)
if (!require("robumeta")) install.packages("robumeta"); library(robumeta)

GetMetaAnalysisResultsWithAggregatedDVs = function(dataFrame, aggregatedDVs, effectDuration, show_effects_with_na = T, hunter_schmidt_correction = F) 
{
  if (show_effects_with_na==F) {
    dataFrame = subset(dataFrame, !is.na(dataFrame$d_final))
  } else {}
  
  dataFrame = subset(dataFrame, to_code==1 & EffectDuration == effectDuration)
  subdata = filter(dataFrame, aggregated_dv %in% aggregatedDVs)
  subdata$M2 = rep(0,nrow(subdata))
  subdata$SD = rep(1,nrow(subdata))
  subdata    = subdata[order(-subdata$N),]
  
  if(hunter_schmidt_correction==T)
  {
    z = metacont(subdata$N/2, subdata$d_after_HunterSchmidt, subdata$SD, subdata$N/2, subdata$M2, subdata$SD, data= subdata, sm="SMD", studlab = study, comb.fixed=FALSE)
  }
  else
  {
    z = metacont(subdata$N/2, subdata$d_final, subdata$SD, subdata$N/2, subdata$M2, subdata$SD, data= subdata, sm="SMD", studlab = study, comb.fixed=FALSE)
  }
  return(z)
}

GetMetaAnalysisResultsWithGroupDVs = function(dataFrame, groupDVs, effectDuration, show_effects_with_na = T) 
{
  if (show_effects_with_na==F) {
    dataFrame = subset(dataFrame, !is.na(dataFrame$d_final))
  } else {}
  
  dataFrame = subset(dataFrame, to_code==1 & EffectDuration == effectDuration)
  subdata = filter(dataFrame, group_dv %in% groupDVs)
  subdata$M2 = rep(0,nrow(subdata))
  subdata$SD = rep(1,nrow(subdata))
  subdata    = subdata[order(-subdata$N),]
  z = metacont(subdata$N/2, subdata$d_final, subdata$SD, subdata$N/2, subdata$M2, subdata$SD, data= subdata, sm="SMD", studlab = study, comb.fixed=FALSE)
  return(z)
}

GetMetaAnalysisResultsWithInteractionForAggregatedDVs = function(dataFrame, aggregatedDVs, moderator, effectDuration, show_effects_with_na = T)
{
  if (show_effects_with_na==F) {
    dataFrame = subset(dataFrame, !is.na(dataFrame$d_final))
  } else {}
  dataFrame = subset(dataFrame, to_code==1 & EffectDuration == effectDuration)
  dataFrame = filter(dataFrame, aggregated_dv %in% aggregatedDVs)
  dataFrame$M2 = rep(0,nrow(dataFrame))
  dataFrame$SD = rep(1,nrow(dataFrame))
  #Meta analysis for Cohen's d with design as a categorical moderator (subgroup analysis)
  z = metacont(dataFrame$N/2, dataFrame$d_final, dataFrame$SD, dataFrame$N/2, dataFrame$M2, dataFrame$SD, data= dataFrame, sm="SMD", studlab = study, comb.fixed=FALSE, byvar=dataFrame[[moderator]])
  return(z)
}

GetErrorForPointEstimate = function(meta_analysis_results, from_interaction = F)
{
  error = (meta_analysis_results$upper.random - meta_analysis_results$lower.random)/2
  if(from_interaction==T)
  {
    error = mapply(function(x,y){sum(x,-y)/2},x = meta_analysis_results$upper.random.w, meta_analysis_results$lower.random.w)
  }
  
  return(error)
}

Write_p_curve_results_to_file = function(output_file_name, dataFrame, aggregatedDVs, effectDuration, show_effects_with_na = T)
{
  if (show_effects_with_na==F) {
    dataFrame = subset(dataFrame, !is.na(dataFrame$d_final))
  } else {}
  
  dataFrame = subset(dataFrame, to_code==1 & EffectDuration == effectDuration)
  subdata = filter(dataFrame, aggregated_dv %in% aggregatedDVs)
  
  r_results = des(subdata$d_final, subdata$N/2, subdata$N/2)
  subdata$r = r_results$r
  subdata$df_for_r = subdata$N-2
  subdata$r_and_df_for_pcurve = paste("r(", subdata$df_for_r,")=", subdata$r, sep = "")
  
  
  full_file_path = paste(getwd(), "/", output_file_name, sep="")
  fileConn=file(full_file_path)
  for (r in subdata$r_and_df_for_pcurve){
    print(r)
    writeLines(r, fileConn)
  }
  writeLines(subdata$r_and_df_for_pcurve, fileConn)
  close(fileConn)
}

Write_p_curve_results_to_file_from_group_dv = function(output_file_name, dataFrame, groupedDVs, effectDuration, show_effects_with_na = T)
{
  if (show_effects_with_na==F) {
    dataFrame = subset(dataFrame, !is.na(dataFrame$d_final))
  } else {}
  
  dataFrame = subset(dataFrame, to_code==1 & EffectDuration == effectDuration)
  subdata = filter(dataFrame, group_dv %in% groupedDVs)

  r_results = des(subdata$d_final, subdata$N/2, subdata$N/2)
  subdata$r = r_results$r
  subdata$df_for_r = subdata$N-2
  subdata$r_and_df_for_pcurve = paste("r(", subdata$df_for_r,")=", subdata$r, sep = "")
  
  full_file_path = paste(getwd(), "/", output_file_name, sep="")
  fileConn=file(full_file_path)
  for (r in subdata$r_and_df_for_pcurve){
    print(r)
    writeLines(r, fileConn)
  }
  writeLines(subdata$r_and_df_for_pcurve, fileConn)
  close(fileConn)
}

ExportCumulativeResultsToGraph = function(metaAnalysisResults, lowerRangeOfD, upperRangeOfD, fileName) 
{
  names(metaAnalysisResults)
  png(fileName, width = 2000, height = 7000, res=200)
  forest(metaAnalysisResults
         ,smlab = ""                           
         ,xlim= c(lowerRangeOfD, upperRangeOfD)
         ,col.square="aquamarine3"

  )                             
  dev.off()
}

GetHunterSchmidtCorrections = function(dataset_for_HS_correction, aggregatedDVs, effectDuration)
{
  meta_analysis_for_averaged_alpha = GetMetaAnalysisForAlpha(dataset_for_HS_correction, aggregatedDVs, 1, F)
  alpha_from_meta_analysis = meta_analysis_for_averaged_alpha$b
  
  dataset_for_HS_correction$alpha_recoded = ifelse(is.na(dataset_for_HS_correction$alpha_recoded), 
                                                   alpha_from_meta_analysis, 
                                                   dataset_for_HS_correction$alpha_recoded)
  
  r_results = invisible(des(dataset_for_HS_correction$d_final, dataset_for_HS_correction$N/2, dataset_for_HS_correction$N/2))
  dataset_for_HS_correction$r_updated = r_results$r
  
  dataset_for_HS_correction$artifact = sqrt(dataset_for_HS_correction$alpha_recoded)
  dataset_for_HS_correction$r.u = dataset_for_HS_correction$r_updated / dataset_for_HS_correction$artifact
  
  dataset_for_HS_correction$r.u = ifelse(dataset_for_HS_correction$r.u > 1, 0.99, dataset_for_HS_correction$r.u)
  
  dataset_for_HS_correction$d_after_HunterSchmidt = 2*dataset_for_HS_correction$r.u / (sqrt(1-dataset_for_HS_correction$r.u^2))
  results_to_return = GetMetaAnalysisResultsWithAggregatedDVs(dataset_for_HS_correction, aggregatedDVs, effectDuration, show_effects_with_na = F, hunter_schmidt_correction = T)
  return(results_to_return)
}

GetMetaAnalysisForAlpha = function(dataFrame, aggregatedDVs, effectDuration, show_effects_with_na = T) 
{
  origDataFrane = dataFrame
  if (show_effects_with_na==F) {
    dataFrame = subset(dataFrame, !is.na(dataFrame$alpha_recoded))
  } else {}
  
  
  dataFrame = subset(dataFrame, to_code==1 & EffectDuration == effectDuration)
  subdata = filter(dataFrame, aggregated_dv %in% aggregatedDVs)
  
  if(nrow(subdata)==0)
  {
    data_for_error_message = filter(origDataFrane, aggregated_dv %in% aggregatedDVs)
    errorMessage = paste("There was no alpha in any of the studies (k=", nrow(data_for_error_message),"). Therefore, there is no possibility to perform Hunter and Schmidt correction.", sep="")
    stop(errorMessage)
  }
  subdata$mi = 5
  results_alpha = rma(measure="ARAW", ai=alpha_recoded, mi=mi, ni=N, data=subdata)
  return(results_alpha)
}

OutlierTest = function(dataFrame, aggregatedDVs, effectDuration, show_effects_with_na = T, return_dataset_of_meta_analysis=F) 
{
  if (show_effects_with_na==F) {
    dataFrame = subset(dataFrame, !is.na(dataFrame$d_final))
  } else {}
  
  dataFrame = subset(dataFrame, to_code==1 & EffectDuration == effectDuration)
  subdata = filter(dataFrame, aggregated_dv %in% aggregatedDVs)
  
  r_results = des(subdata$d_final, subdata$N/2, subdata$N/2)
  subdata$r = r_results$r
  res_for_outlier = metafor::rma(ri=r, ni=N, measure="ZCOR", data = subdata)
  inf.freq = influence(res_for_outlier)
  if(return_dataset_of_meta_analysis == T)
  {
    return(subdata)
  }
  return(inf.freq)
}

OutlierTest_for_group_dv = function(dataFrame, groupedDVs, effectDuration, show_effects_with_na = T, return_dataset_of_meta_analysis=F)
{
  if (show_effects_with_na==F) {
    dataFrame = subset(dataFrame, !is.na(dataFrame$d_final))
  } else {}
  
  #effectDuration - the same numbers as our coding book
  dataFrame = subset(dataFrame, to_code==1 & EffectDuration == effectDuration)
  subdata = filter(dataFrame, group_dv %in% groupedDVs)
  
  r_results = des(subdata$d_final, subdata$N/2, subdata$N/2)
  subdata$r = r_results$r
  res_for_outlier = metafor::rma(ri=r, ni=N, measure="ZCOR", data = subdata)
  inf.freq = influence(res_for_outlier)
  if(return_dataset_of_meta_analysis == T)
  {
    return(subdata)
  }
  return(inf.freq)
}


#STEP 2: Load data----
setwd("")
#Option 1: Load all relevant datasets from separate .csv files:
pos_neg_gain_loss_abs = read.csv("pos_neg_gain_loss_abs.csv")
pos_neg_gain_loss_emotions = read.csv("pos_neg_gain_loss_emotions.csv")
attitudes_and_behavior_dataset = read.csv("attitudes_and_behavior_dataset.csv")
pos_control_absolute = read.csv("pos_control_absolute.csv")
neg_control_absolute = read.csv("neg_control_absolute.csv")
comp_control_absolute = read.csv("comp_control_absolute.csv")
comp_pos_neg_unique = read.csv("comp_pos_neg_unique.csv")
pos_neg_gain_loss_abs_behavior_only = read.csv("pos_neg_gain_loss_abs_behavior_only.csv")
#Option 2: Load one .RData file that includes all datasets:
#load("Real, but limited - BJPS.RData")



#STEP 3: Analysis of framing effects----
#Main effects (Table 1):
results_pos_neg_gain_loss_attitude = GetMetaAnalysisResultsWithAggregatedDVs(pos_neg_gain_loss_abs, "Attitude", 1, F); results_pos_neg_gain_loss_attitude
results_pos_neg_gain_loss_emotions = GetMetaAnalysisResultsWithAggregatedDVs(pos_neg_gain_loss_abs, "Emotions", 1, F); results_pos_neg_gain_loss_emotions
results_pos_neg_gain_loss_behavior = GetMetaAnalysisResultsWithAggregatedDVs(pos_neg_gain_loss_abs, "Behavior", 1, F); results_pos_neg_gain_loss_behavior

#Analyze positive and negative emotions separately (Table 1):
results_pos_neg_gain_loss_pos_emotions = GetMetaAnalysisResultsWithGroupDVs(pos_neg_gain_loss_emotions, "Emotions (positive)", 1, F); results_pos_neg_gain_loss_pos_emotions
results_pos_neg_gain_loss_neg_emotions = GetMetaAnalysisResultsWithGroupDVs(pos_neg_gain_loss_emotions, "Emotions (negative)", 1, F); results_pos_neg_gain_loss_neg_emotions

#The difference between framing effects on attitudes compared to behavior:
#Subgroup analysis (Table B1):
interaction_dv_type = GetMetaAnalysisResultsWithInteractionForAggregatedDVs(pos_neg_gain_loss_abs, c("Attitude", "Behavior", "Emotions"), "aggregated_dv", 1, F); interaction_dv_type
#Meta-regression with Attitudes as reference category (the default):
metareg(interaction_dv_type)
#Meta-regression with Behavior as reference category:
metareg(interaction_dv_type, relevel(factor(aggregated_dv), ref="2"))

#Perform a meta-analysis where we only take studies that tested BOTH attitudes and behavior on the same sample (i.e., with the same manipulation):
rve_results = robu(formula = d_final ~ aggregated_dv, data = attitudes_and_behavior_dataset,
                   studynum = study, var.eff.size = v_d_final,
                   rho = .8, small = TRUE); print(rve_results)

#Separate analysis for Positive vs. control, and for Negative vs. control (Table B4):
#Positive vs. control:
res_pos_control_attitude = GetMetaAnalysisResultsWithAggregatedDVs(pos_control_absolute, "Attitude", 1, F); res_pos_control_attitude
res_pos_control_emotion = GetMetaAnalysisResultsWithAggregatedDVs(pos_control_absolute, "Emotions", 1, F); res_pos_control_emotion
res_pos_control_behavior = GetMetaAnalysisResultsWithAggregatedDVs(pos_control_absolute, "Behavior", 1, F); res_pos_control_behavior
#Negative vs. control:
res_neg_control_attitude = GetMetaAnalysisResultsWithAggregatedDVs(neg_control_absolute, "Attitude", 1, F); res_neg_control_attitude
res_neg_control_behavior = GetMetaAnalysisResultsWithAggregatedDVs(neg_control_absolute, "Behavior", 1, F); res_neg_control_behavior


#Step 4: Analysis of competitive framing effects-----
#Main effect (Table 1):
res_comp_control_attitude = GetMetaAnalysisResultsWithAggregatedDVs(comp_control_absolute, "Attitude", 1, F); res_comp_control_attitude
#The Emotions and Behavior DVs are not meta-anayzed because they include only one study each.

#Test the difference between the framing and competitive framing effects (Table B3):
results_comp_pos_neg_unique = GetMetaAnalysisResultsWithInteractionForAggregatedDVs(comp_pos_neg_unique, "Attitude", "comparison_recoded", 1, F); results_comp_pos_neg_unique


#Step 5: Generate Figure 1-----
frame_types = c("Positive vs. Negative", "Positive vs. Negative", "Positive vs. Negative",
                "Competitive")
DV_types = c("Attitudes", "Emotions", "Behavior",
             "Attitudes")
all_effects = c(results_pos_neg_gain_loss_attitude$TE.random,
                results_pos_neg_gain_loss_emotions$TE.random, 
                results_pos_neg_gain_loss_behavior$TE.random,
                res_comp_control_attitude$TE.random
)
error = c(GetErrorForPointEstimate(results_pos_neg_gain_loss_attitude),
          GetErrorForPointEstimate(results_pos_neg_gain_loss_emotions),
          GetErrorForPointEstimate(results_pos_neg_gain_loss_behavior),
          GetErrorForPointEstimate(res_comp_control_attitude))

point_estimates_to_plot = data.frame(frame_types, DV_types, all_effects, error)
point_estimates_to_plot$DV_types_ordered = factor(DV_types, levels=c("Attitudes", "Emotions", "Behavior"))
point_estimates_to_plot$frame_types_ordered = factor(frame_types, levels=c("Positive vs. Negative", "Competitive"))
p = ggplot(point_estimates_to_plot, aes(x = DV_types_ordered, y = all_effects, color = frame_types_ordered))
p + geom_pointrange(aes(ymin = all_effects-error, ymax = all_effects+error),
                    position=position_dodge(width=0.3), 
                    size=1.15) + 
  ylab("Mean Effect Size")+
  coord_cartesian(ylim = c(0, .8)) + 
  xlab("")+
  geom_hline(yintercept=0, linetype="dashed")+
  scale_color_manual(values=c("black", "gray65"))+ 
  theme_bw()+
  theme(axis.text=element_text(size=18, color="black"),
        axis.title=element_text(size=18),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20)), 
        legend.text=element_text(size=18, margin = margin(r = 20)),
        legend.title=element_text(size=18),
        aspect.ratio=1,
        panel.grid.minor = element_line(size = 0.05), panel.grid.major = element_line(size = 0.05))+ 
  guides(color=guide_legend(title="Framing Type",keyheight=2))



#Step 6: Moderator analyses (Tables B1, B2)-----
#1. Moderators of positive vs. negative framing effects (Table B1):
#Emphasis vs. Equivalency:
equivalence_emphasis_interaction = GetMetaAnalysisResultsWithInteractionForAggregatedDVs(pos_neg_gain_loss_abs, "Attitude", "TypeFraming_recoded", 1, F); equivalence_emphasis_interaction

#Figure 2: Equivalency vs. Emphasis framing:
equi_emphasis_frame_types = c("Emphasis Framing", "Equivalency Framing")
all_effects_equi_emphasis = c(equivalence_emphasis_interaction$TE.random.w[1],
                              equivalence_emphasis_interaction$TE.random.w[2])

error_equi_emphasis = GetErrorForPointEstimate(equivalence_emphasis_interaction, from_interaction = T)

point_estimates_to_plot_equi_emphasis = data.frame(equi_emphasis_frame_types, all_effects_equi_emphasis, error_equi_emphasis)

p_equi_emphasis = ggplot(point_estimates_to_plot_equi_emphasis, aes(x = equi_emphasis_frame_types, y = all_effects_equi_emphasis))
p_equi_emphasis + geom_pointrange(aes(ymin = all_effects_equi_emphasis-error_equi_emphasis, ymax = all_effects_equi_emphasis+error_equi_emphasis),
                                  position=position_dodge(width=0.3), #distance between the lines of each frame type (side by side)
                                  size=1.15) + 
  ylab("Mean Effect Size")+
  coord_cartesian(ylim = c(0.2, .8)) + 
  xlab("")+
  geom_hline(yintercept=0, linetype="dashed")+
  theme_bw()+
  theme(axis.text=element_text(size=18, color="black"),
        axis.title=element_text(size=18),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20)), #increase distance of y title from axis (r) and from the edge of the screen (l)
        legend.text=element_text(size=18, margin = margin(r = 20)), #increase distance of legend text from the edge of the screen
        legend.title=element_text(size=18),
        aspect.ratio=1, #makes the plot square-shaped (rather than rectangle)
        panel.grid.minor = element_line(size = 0.05), panel.grid.major = element_line(size = 0.05))

#A formal comparison of the three DV types (attitudes, emotions, and behavior) is reported above in STEP 3

#Medium:
GetMetaAnalysisResultsWithInteractionForAggregatedDVs(filter(pos_neg_gain_loss_abs, !is.na(medium_recoded)), "Attitude", "medium_recoded", 1, F)
#Frame source:
GetMetaAnalysisResultsWithInteractionForAggregatedDVs(pos_neg_gain_loss_abs, "Attitude", "frameSource_recoded", 1, F)
#Country (US vs. non-US):
GetMetaAnalysisResultsWithInteractionForAggregatedDVs(pos_neg_gain_loss_abs, "Attitude", "country_binary", 1, F)
#Sample composition:
GetMetaAnalysisResultsWithInteractionForAggregatedDVs(filter(pos_neg_gain_loss_abs, !is.na(sample_recoded)), "Attitude", "sample_recoded", 1, F)
#Period:
GetMetaAnalysisResultsWithInteractionForAggregatedDVs(filter(pos_neg_gain_loss_abs, !is.na(Election)), "Attitude", "Election", 1, F)
#Actual behavior vs. behavioral intention:
#(exclude the study of Leeper, 2017 because it measures both actual and intended behavior on the same sample)
GetMetaAnalysisResultsWithInteractionForAggregatedDVs(filter(pos_neg_gain_loss_abs_behavior_only, study!="Leeper, 2017"), 
                                                      "Behavior", "behavior_intention", 1, F)


#2. Moderators for competitive framing effects (Table B2):
#Medium:
GetMetaAnalysisResultsWithInteractionForAggregatedDVs(comp_control_absolute, "Attitude", "medium_recoded", 1, F)
#Country (US vs. non-US):
GetMetaAnalysisResultsWithInteractionForAggregatedDVs(comp_control_absolute, "Attitude", "country_binary", 1, F)
#Sample composition:
GetMetaAnalysisResultsWithInteractionForAggregatedDVs(filter(comp_control_absolute, !is.na(sample_recoded)), "Attitude", "sample_recoded", 1, F)
#Period:
GetMetaAnalysisResultsWithInteractionForAggregatedDVs(filter(comp_control_absolute, !is.na(Election)), "Attitude", "Election", 1, F)



#Step 7: Sensitivity analyses for positive vs. negative frames-----

#Publication bias - Trim and Fill (Table 1):
trimfill(results_pos_neg_gain_loss_attitude, estimator = "L0", ma.fixed = "T")
trimfill(results_pos_neg_gain_loss_emotions, estimator = "L0", ma.fixed = "T")
trimfill(results_pos_neg_gain_loss_behavior, estimator = "L0", ma.fixed = "T")
trimfill(results_pos_neg_gain_loss_pos_emotions, estimator = "L0", ma.fixed = "T")
trimfill(results_pos_neg_gain_loss_neg_emotions, estimator = "L0", ma.fixed = "T")

#p-curve - another method for publication bias (insert the effect sizes in the text files created below into the online app at www.p-curve.com):
Write_p_curve_results_to_file("p-curve_pos_neg_attitudes.txt", pos_neg_gain_loss_abs, "Attitude", 1, F)
Write_p_curve_results_to_file("p-curve_pos_neg_emotions.txt", pos_neg_gain_loss_abs, "Emotions", 1, F)
Write_p_curve_results_to_file("p-curve_pos_neg_behavior.txt", pos_neg_gain_loss_abs, "Behavior", 1, F)
Write_p_curve_results_to_file_from_group_dv("p-curve_pos_neg_emotions_positive.txt", pos_neg_gain_loss_emotions, "Emotions (positive)", 1, F)
Write_p_curve_results_to_file_from_group_dv("p-curve_pos_neg_emotions_negative.txt", pos_neg_gain_loss_emotions, "Emotions (negative)", 1, F)


#Cumulative meta-analysis
#1. Sort by effect size (default):
cumulative_results_pos_neg_gain_loss_attitude = metacum(results_pos_neg_gain_loss_attitude, pooled = "random"); cumulative_results_pos_neg_gain_loss_attitude
ExportCumulativeResultsToGraph(cumulative_results_pos_neg_gain_loss_attitude, -.2, 1.3, "pos_neg_gain_loss_attitude_cumulative.png")
cumulative_results_pos_neg_gain_loss_emotions = metacum(results_pos_neg_gain_loss_emotions, pooled = "random"); cumulative_results_pos_neg_gain_loss_emotions
ExportCumulativeResultsToGraph(cumulative_results_pos_neg_gain_loss_emotions, -.2, .7, "pos_neg_gain_loss_emotions_cumulative.png")
cumulative_results_pos_neg_gain_loss_behavior = metacum(results_pos_neg_gain_loss_behavior, pooled = "random"); cumulative_results_pos_neg_gain_loss_behavior
ExportCumulativeResultsToGraph(cumulative_results_pos_neg_gain_loss_behavior, -.06, .20, "pos_neg_gain_loss_behavior_cumulative.png")
cumulative_results_pos_neg_gain_loss_pos_emotions = metacum(results_pos_neg_gain_loss_pos_emotions, pooled = "random"); cumulative_results_pos_neg_gain_loss_pos_emotions
ExportCumulativeResultsToGraph(cumulative_results_pos_neg_gain_loss_pos_emotions, -.07, 1.4, "pos_neg_gain_loss_emotions_positive_cumulative.png")
cumulative_results_pos_neg_gain_loss_neg_emotions = metacum(results_pos_neg_gain_loss_neg_emotions, pooled = "random"); cumulative_results_pos_neg_gain_loss_neg_emotions
ExportCumulativeResultsToGraph(cumulative_results_pos_neg_gain_loss_neg_emotions, -.60, 0.3, "pos_neg_gain_loss_emotions_negative_cumulative.png")
#2. Sort by year published:
year_cumulative_results_pos_neg_gain_loss_attitude = metacum(results_pos_neg_gain_loss_attitude, pooled = "random", sortvar=YearPublished); year_cumulative_results_pos_neg_gain_loss_attitude
ExportCumulativeResultsToGraph(year_cumulative_results_pos_neg_gain_loss_attitude, -.2, 1, "pos_neg_gain_loss_attitude_cumulative_year.png")
year_cumulative_results_pos_neg_gain_loss_emotions = metacum(results_pos_neg_gain_loss_emotions, pooled = "random", sortvar=YearPublished); year_cumulative_results_pos_neg_gain_loss_emotions
ExportCumulativeResultsToGraph(year_cumulative_results_pos_neg_gain_loss_emotions, -.2, .9, "pos_neg_gain_loss_emotions_cumulative_year.png")
year_cumulative_results_pos_neg_gain_loss_behavior = metacum(results_pos_neg_gain_loss_behavior, pooled = "random", sortvar=YearPublished); year_cumulative_results_pos_neg_gain_loss_behavior
ExportCumulativeResultsToGraph(year_cumulative_results_pos_neg_gain_loss_behavior, -.06, .90, "pos_neg_gain_loss_behavior_cumulative_year.png")
year_cumulative_results_pos_neg_gain_loss_pos_emotions = metacum(results_pos_neg_gain_loss_pos_emotions, pooled = "random", sortvar=YearPublished); year_cumulative_results_pos_neg_gain_loss_pos_emotions
ExportCumulativeResultsToGraph(year_cumulative_results_pos_neg_gain_loss_pos_emotions, -.50, 1.4, "pos_neg_gain_loss_emotions_positive_cumulative_year.png")
year_cumulative_results_pos_neg_gain_loss_neg_emotions = metacum(results_pos_neg_gain_loss_neg_emotions, pooled = "random", sortvar=YearPublished); year_cumulative_results_pos_neg_gain_loss_neg_emotions
ExportCumulativeResultsToGraph(year_cumulative_results_pos_neg_gain_loss_neg_emotions, -.90, 0.3, "pos_neg_gain_loss_emotions_negative_cumulative_year.png")


#Hunter & Schmidt correction for measurement error (Table 1):
results_pos_neg_gain_loss_attitude_hs = GetHunterSchmidtCorrections(pos_neg_gain_loss_abs, "Attitude",1); results_pos_neg_gain_loss_attitude_hs
results_pos_neg_gain_loss_emotions_hs = GetHunterSchmidtCorrections(pos_neg_gain_loss_abs, "Emotions",1); results_pos_neg_gain_loss_emotions_hs
results_pos_neg_gain_loss_behavior_hs = GetHunterSchmidtCorrections(pos_neg_gain_loss_abs, "Behavior",1); results_pos_neg_gain_loss_behavior_hs
results_pos_neg_gain_loss_pos_emotions_hs = GetHunterSchmidtCorrections(filter(pos_neg_gain_loss_emotions, group_dv=="Emotions (positive)"), "Emotions",1); results_pos_neg_gain_loss_pos_emotions_hs
results_pos_neg_gain_loss_neg_emotions_hs = GetHunterSchmidtCorrections(filter(pos_neg_gain_loss_emotions, group_dv=="Emotions (negative)"), "Emotions",1); results_pos_neg_gain_loss_neg_emotions_hs

#Outlier analysis
#Outlier analysis for attitudes:
results_pos_neg_gain_loss_attitude_outliers = OutlierTest(pos_neg_gain_loss_abs, "Attitude", 1, F);  png('results_pos_neg_gain_loss_attitude_outliers.png',  width = 4000, height = 4000, res = 500); plot(results_pos_neg_gain_loss_attitude_outliers); dev.off()
#See who's the outlier (study 58 in the attitudes dataset):
outlier_dataset_attitudes = OutlierTest(pos_neg_gain_loss_abs, "Attitude", 1, F, T)
outlier_dataset_attitudes$study[58]
#And run the meta-analysis without this outlier:
GetMetaAnalysisResultsWithAggregatedDVs(filter(pos_neg_gain_loss_abs, study!="Hilbig, 2012-Experiment 1"), "Attitude", 1, F)
#Outlier analysis for emotions (no outliers detected):
results_pos_neg_gain_loss_emotions_outliers = OutlierTest(pos_neg_gain_loss_abs, "Emotions", 1, F); png('results_pos_neg_gain_loss_emotions_outliers.png',  width = 4000, height = 4000, res = 500); plot(results_pos_neg_gain_loss_emotions_outliers); dev.off()
#Outlier analysis for positive emotions (no outliers detected):
results_pos_neg_gain_loss_positive_emotions_outliers = OutlierTest_for_group_dv(pos_neg_gain_loss_emotions, "Emotions (positive)", 1, F); png('results_pos_neg_gain_loss_positive_emotions_outliers.png',  width = 4000, height = 4000, res = 500); plot(results_pos_neg_gain_loss_positive_emotions_outliers); dev.off()
#Outlier analysis for negative emotions (no outliers detected):
results_pos_neg_gain_loss_negative_emotions_outliers = OutlierTest_for_group_dv(pos_neg_gain_loss_emotions, "Emotions (negative)", 1, F); png('results_pos_neg_gain_loss_negative_emotions_outliers.png',  width = 4000, height = 4000, res = 500); plot(results_pos_neg_gain_loss_negative_emotions_outliers); dev.off()
#Outlier analysis for behavior:
results_pos_neg_gain_loss_behavior_outliers = OutlierTest(pos_neg_gain_loss_abs, "Behavior", 1, F); png('results_pos_neg_gain_loss_behavior_outliers.png',  width = 4000, height = 4000, res = 500); plot(results_pos_neg_gain_loss_behavior_outliers); dev.off()
#See who's the outlier (study 11 in the behavior dataset):
outlier_dataset = OutlierTest(pos_neg_gain_loss_abs, "Behavior", 1, F, T)
outlier_dataset$study[11]
#And run the meta-analysis without this outlier:
GetMetaAnalysisResultsWithAggregatedDVs(filter(pos_neg_gain_loss_abs, study!="Gerber et al., 2018-Study 2"), "Behavior", 1, F)


#Publication bias for positive vs. control (Table B4):
trimfill(res_pos_control_attitude, estimator = "L0", ma.fixed = "T")
trimfill(res_pos_control_behavior, estimator = "L0", ma.fixed = "T")
#Hunter & Schmidt correction for positive vs. control (Table B4):
res_pos_control_attitude_hs = GetHunterSchmidtCorrections(pos_control_absolute, "Attitude",1); res_pos_control_attitude_hs
res_pos_control_emotion_hs = GetHunterSchmidtCorrections(pos_control_absolute, "Emotions",1); res_pos_control_emotion_hs
res_pos_control_behavior_hs = GetHunterSchmidtCorrections(pos_control_absolute, "Behavior",1); res_pos_control_behavior_hs

#Publication bias for negative vs. control (Table B4):
trimfill(res_neg_control_attitude, estimator = "L0", ma.fixed = "T")
trimfill(res_neg_control_behavior, estimator = "L0", ma.fixed = "T")
#Hunter & Schmidt correction for neg vs. control (Table B4):
res_neg_control_attitude_hs = GetHunterSchmidtCorrections(neg_control_absolute, "Attitude",1); res_neg_control_attitude_hs
res_neg_control_behavior_hs = GetHunterSchmidtCorrections(neg_control_absolute, "Behavior",1); res_neg_control_behavior_hs



#STEP 8: Sensitivity analyses for competitive framing effects----

#Publication bias - Trim and Fill (Table 1):
trimfill(res_comp_control_attitude, estimator = "L0", ma.fixed = "T")

#p-curve - another method for publication bias (insert the effect sizes in the text files created below into the online app at www.p-curve.com):
Write_p_curve_results_to_file("p-curve_competitive_attitudes.txt", comp_control_absolute, "Attitude", 1, F)

#Cumulative meta-analysis
#Sort by effect size:
cumulative_res_comp_control_attitude = metacum(res_comp_control_attitude, pooled = "random"); cumulative_res_comp_control_attitude
ExportCumulativeResultsToGraph(cumulative_res_comp_control_attitude, -.02, 0.27, "res_comp_control_attitude_cumulative.png")
#Sort by year:
year_cumulative_res_comp_control_attitude = metacum(res_comp_control_attitude, pooled = "random", sortvar=YearPublished); year_cumulative_res_comp_control_attitude
ExportCumulativeResultsToGraph(year_cumulative_res_comp_control_attitude, -.02, 0.4, "res_comp_control_attitude_cumulative_year.png")

#Hunter & Schmidt correction for measurement error (Table 1):
res_comp_control_attitude_hs = GetHunterSchmidtCorrections(comp_control_absolute, "Attitude",1); res_comp_control_attitude_hs

#Outlier analysis (no outliers detected):
res_comp_control_attitudes_outliers = OutlierTest(comp_control_absolute, "Attitude", 1, F);  png('res_comp_control_attitudes_outliers.png',  width = 4000, height = 4000, res = 500); plot(res_comp_control_attitudes_outliers); dev.off()
