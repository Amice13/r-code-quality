# Histograms for some outcomes (Figure 2 and Figure 3)

library(ggplot2)


raw=resp

set.seed(302)

source('helpers.R')

source("entropy_weighting.R")

raw$anytreat = raw$covid_any
raw$intended_behavior = raw$links_total
raw$safety_count_variable = 4-raw$safety_total_always_followup



outcome = "know_gap_count"
label = "Knowledge gap score"


controlgroup = raw[raw$anytreat==0,]
treatmentgroup = raw[raw$anytreat==1,]

hist_values = c()
lowerbound = c()
upperbound = c()

temp1 = controlgroup[!is.na(controlgroup[[outcome]]),]
temp2 = treatmentgroup[!is.na(treatmentgroup[[outcome]]),]

max_value = max(raw[[outcome]],na.rm=TRUE)

ks.test(temp1[[outcome]],temp2[[outcome]])


for (i in 0:5){
  temp1$outcome = as.numeric(temp1[[outcome]]==i)
  formula = as.formula(paste("outcome","  ~  ","1"))
  reg =  lm(formula = formula, data = temp1)
  coef_model = summary(reg)$coefficients
  se = coef_model[1,"Std. Error"]
  coef = coef_model[1,"Estimate"]
  
  lowerbound = c(lowerbound, round(coef - 1.96*se,3))
  upperbound = c(upperbound, round(coef + 1.96*se,3))
  
  temp2$outcome = as.numeric(temp2[[outcome]]==i)
  formula = as.formula(paste("outcome","  ~  ","1"))
  reg =  lm(formula = formula, data = temp2)
  coef_model = summary(reg)$coefficients
  se = coef_model[1,"Std. Error"]
  coef = coef_model[1,"Estimate"]
  
  lowerbound = c(lowerbound, round(coef - 1.96*se,3))
  upperbound = c(upperbound, round(coef + 1.96*se,3))
  

  hist_values = c(hist_values,round(sum(temp1[[outcome]]==i,na.rm=TRUE)/sum(!is.na(temp1[[outcome]])),3))
  hist_values = c(hist_values,round(sum(temp2[[outcome]]==i,na.rm=TRUE)/sum(!is.na(temp2[[outcome]])),3))
}

temp1$outcome = as.numeric(temp1[[outcome]]>5)
formula = as.formula(paste("outcome","  ~  ","1"))
reg =  lm(formula = formula, data = temp1)
coef_model = summary(reg)$coefficients
se = coef_model[1,"Std. Error"]
coef = coef_model[1,"Estimate"]

lowerbound = c(lowerbound, round(coef - 1.96*se,3))
upperbound = c(upperbound, round(coef + 1.96*se,3))

temp2$outcome = as.numeric(temp2[[outcome]]>5)
formula = as.formula(paste("outcome","  ~  ","1"))
reg =  lm(formula = formula, data = temp2)
coef_model = summary(reg)$coefficients
se = coef_model[1,"Std. Error"]
coef = coef_model[1,"Estimate"]

lowerbound = c(lowerbound, round(coef - 1.96*se,3))
upperbound = c(upperbound, round(coef + 1.96*se,3))

hist_values = c(hist_values,round(sum(temp1[[outcome]]>5,na.rm=TRUE)/sum(!is.na(temp1[[outcome]])),3))
hist_values = c(hist_values,round(sum(temp2[[outcome]]>5,na.rm=TRUE)/sum(!is.na(temp2[[outcome]])),3))

value = c()
for (i in 0:6){
  value = c(value,i,i)
}

df1 <- data.frame(

  val = c(1:14), #used to reorder factors
  Share = hist_values,
  lowerbound = lowerbound,
  upperbound = upperbound,
  Share_string = format(round(hist_values, digits=3), nsmall = 3),
  lowerbound_string = format(round(lowerbound, digits=3), nsmall = 3),
  upperbound_string = format(round(upperbound, digits=3), nsmall = 3),

  Group = rep(c("Control","Any_Intervention"),14
  ),
  Value = value)

df1$ci <- paste("(",df1$lowerbound_string,", ",df1$upperbound_string,")", sep="")

df1 <- df1 %>%
  arrange(val) %>%
  mutate(Outcome = factor(Value, levels = c(0:max_value))) %>%
  mutate(Group = factor(Group, levels = c("Control","Any_Intervention")))

levels(df1$Outcome)[levels(df1$Outcome) =="6"] <- "6 or more"

## WITH INTERVENTIONS AS FACETS

# clean base plot 
plot_base_clean <- ggplot(data = df1, mapping = aes(x = Outcome, y = Share, ymin = lowerbound, ymax = upperbound,fill = Group)) + 
  scale_x_discrete(label) +
  # apply basic black and white theme - this theme removes the background colour by default
  theme_bw() + 
  # remove gridlines. Panel.grid.major is for vertical lines, Panel.grid.minor is for horizontal lines
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        # remove borders
        panel.border = element_blank(),
        # removing borders also removes x and y axes. Add them back
        axis.line = element_line(),
        axis.title = element_text()
        
  )

Share_histogram <- plot_base_clean + 
 
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("#0072B2", "#D55E00"),labels=c("Control","Intervention")) +

  theme(# remove background colour from facet labels
    #strip.background  = element_blank(),
    # remove border from facet label
    panel.border = element_blank()) +
  geom_errorbar(position = position_dodge(width=0.9), width=.1) +
  geom_text(aes(label = Share_string), size = 3, vjust = -5,position = position_dodge(width=0.9)) +
  geom_text(aes(label = ci), size = 3, position = position_dodge(width=0.9), vjust = -3.5) 
 

last_plot()
ggsave(paste0("./Output/histogram_",outcome,".png"), width = 15, height = 9)



### Histograms for safety gaps


raw2=raw[!is.na(raw$weights_follow),]
outcome = "safety_count_variable"
label = "Safety gap score"


controlgroup = raw2[raw2$anytreat==0,]
treatmentgroup = raw2[raw2$anytreat==1,]

hist_values = c()
lowerbound = c()
upperbound = c()

temp1 = controlgroup[!is.na(controlgroup[[outcome]]),]
temp2 = treatmentgroup[!is.na(treatmentgroup[[outcome]]),]

max_value = max(raw2[[outcome]],na.rm=TRUE)

ks.test(temp1[[outcome]],temp2[[outcome]])


for (i in 0:max_value){
  temp1$outcome = as.numeric(temp1[[outcome]]==i)
  formula = as.formula(paste("outcome","  ~  ","1"))
  reg =  lm(formula = formula, data = temp1)
  coef_model = summary(reg)$coefficients
  se = coef_model[1,"Std. Error"]
  coef = coef_model[1,"Estimate"]
  
  lowerbound = c(lowerbound, round(coef - 1.96*se,3))
  upperbound = c(upperbound, round(coef + 1.96*se,3))
  
  temp2$outcome = as.numeric(temp2[[outcome]]==i)
  formula = as.formula(paste("outcome","  ~  ","1"))
  reg =  lm(formula = formula, data = temp2)
  coef_model = summary(reg)$coefficients
  se = coef_model[1,"Std. Error"]
  coef = coef_model[1,"Estimate"]
  
  lowerbound = c(lowerbound, round(coef - 1.96*se,3))
  upperbound = c(upperbound, round(coef + 1.96*se,3))
  
  
  
  
  hist_values = c(hist_values,round(sum(temp1[[outcome]]==i,na.rm=TRUE)/sum(!is.na(temp1[[outcome]])),3))
  hist_values = c(hist_values,round(sum(temp2[[outcome]]==i,na.rm=TRUE)/sum(!is.na(temp2[[outcome]])),3))
}

value = c()
for (i in 0:max_value){
  value = c(value,i,i)
}

df1 <- data.frame(
  val = c(1:(2*(max_value+1))), #used to reorder factors

  Share = hist_values,
  lowerbound = lowerbound,
  upperbound = upperbound,
  Share_string = format(round(hist_values, digits=3), nsmall = 3),
  lowerbound_string = format(round(lowerbound, digits=3), nsmall = 3),
  upperbound_string = format(round(upperbound, digits=3), nsmall = 3),
  Group = rep(c("Control","Any_Intervention"),(max_value+1)),
  Value = value)

df1$ci <- paste("(",df1$lowerbound_string,", ",df1$upperbound_string,")", sep="")

df1 <- df1 %>%
  arrange(val) %>%
  mutate(Outcome = factor(Value, levels = c(0:max_value))) %>%
  mutate(Group = factor(Group, levels = c("Control","Any_Intervention")))


## WITH INTERVENTIONS AS FACETS

# clean base plot 
plot_base_clean <- ggplot(data = df1, mapping = aes(x = Outcome, y = Share, ymin = lowerbound, ymax = upperbound,fill = Group)) + 
  scale_x_discrete(label) +
  # apply basic black and white theme - this theme removes the background colour by default
  theme_bw() + 
  # remove gridlines. Panel.grid.major is for vertical lines, Panel.grid.minor is for horizontal lines
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        # remove borders
        panel.border = element_blank(),
        # removing borders also removes x and y axes. Add them back
        axis.line = element_line(),
        axis.title = element_text()
        
  )

Share_histogram <- plot_base_clean + 
  
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("#0072B2", "#D55E00"),labels=c("Control","Intervention")) +
  
  theme(# remove background colour from facet labels
    #strip.background  = element_blank(),
    # remove border from facet label
    panel.border = element_blank()) +
  geom_errorbar(position = position_dodge(width=0.9), width=.1) +
  geom_text(aes(label = Share_string), size = 3, vjust = -5,position = position_dodge(width=0.9)) +
  geom_text(aes(label = ci), size = 3, position = position_dodge(width=0.9), vjust = -3.5) 


last_plot()
ggsave(paste0("./Output/histogram_",outcome,".png"), width = 15, height = 9)


